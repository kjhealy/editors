###
### Messing about with Brett Terpstra's iOS text/code editors comparison
### Data from http://brettterpstra.com/ios-text-editors/

library(cluster)
library(ggplot2)
library(scales)
library(reshape)
library(RColorBrewer)

### --------------------------------------------------
### Attribution footnote for graphs
### --------------------------------------------------
makeFootnote <- function(footnoteText=
                         format(Sys.time(), "%d %b %Y"),
                         size= .7, color= grey(.5))
{
   require(grid)
   pushViewport(viewport())
   grid.text(label= footnoteText ,
             x = unit(1,"npc") - unit(2, "mm"),
             y= unit(2, "mm"),
             just=c("right", "bottom"),
             gp=gpar(cex= size, col=color))
   popViewport()
}

credit <- function() {
  return(makeFootnote("\nhttp://kieranhealy.org/blog/2012/04/18/visualizing-ios-text-editors/"))
}

### --------------------------------------------------
### Data
### --------------------------------------------------
data <- read.csv("data/editors-data.csv", header=TRUE)

## Dissimilarity matrix
d <- daisy(data)

## Create a factor to aid clustering that mirror BT's own
## feature groupings.
Feature.Type <- factor(x=c("Price", rep("Device", 2), rep("Sync", 5),
                         rep("Export", 7), rep("Features", 15)))
data1 <- as.data.frame(t(data))
data1$Feature.Type <- Feature.Type
d1 <- daisy(data1)

## Clustering
out.by.name <- hclust(d, method="ward")
out.by.feature <- hclust(d1, method="ward")


### --------------------------------------------------
### Plot the Dendrograms
### --------------------------------------------------
pdf(file="figures/cluster-by-name.pdf", height=10,width=14, pointsize=11)
plot(out.by.name, hang=-1, main="Clustering Editors by Common Features",
     sub="", xlab="")
credit()
dev.off()

png(file="figures/cluster-by-name.png", height=700,width=1000, pointsize=12)
plot(out.by.name, hang=-1, main="Clustering Editors by Common Features",
     sub="", xlab="")
credit()
dev.off()

pdf(file="figures/cluster-by-feature.pdf", height=10,width=13, pointsize=11)
plot(out.by.feature, hang=-1, main="Clustering Features",
     sub="", xlab="")
credit()
dev.off()

png(file="figures/cluster-by-feature.png", height=700,width=1000, pointsize=12)
plot(out.by.feature, hang=-1, main="Clustering Features",
     sub="", xlab="")
credit()
dev.off()

###--------------------------------------------------
### Prep the tables
### --------------------------------------------------

## Clean the labels
feature.labels <- gsub("\\."," ", colnames(data))
feature.labels <- gsub("preview export", "preview/export", feature.labels)
feature.labels <- gsub("Open in ", "Open in…", feature.labels)
feature.labels <- gsub("Full text", "Full-text", feature.labels)
feature.labels <- gsub("handlers", "handler(s)", feature.labels)

## Put in the rownames as a variable
data.o <- data
r.names <- factor(rownames(data.o), levels=rev(rownames(data)), ordered=TRUE)

## All column names, ordered as in original data, including price
c.names <- factor(colnames(data.o), levels=colnames(data), ordered=TRUE)

## Column names ordered from cluster analysis, including Price
c.names.clus <- out.by.feature$labels[out.by.feature$order]
c.names.clus.f <- factor(c.names.clus, levels=out.by.feature$labels[out.by.feature$order], ordered=TRUE)

## Lookup table for labeling later (not strictly needed but for reference)
orig.labels <- colnames(data)
lab.lookup <- data.frame(orig.labels,feature.labels)
lab.lookup$cluster.order <- out.by.feature$order

## Price is not included as a label below (because we use it as a
## facet), so we will permute the table now, then drop it and its
## unused levels, then add a new numeric index.
lab.lookup <- lab.lookup[lab.lookup$cluster.order,]
lab.lookup <- lab.lookup[lab.lookup$orig.labels!="Price",]
lab.lookup$orig.labels <- droplevels(lab.lookup$orig.labels)
lab.lookup$feature.labels <- droplevels(lab.lookup$feature.labels)
lab.lookup$figure.order <- c(1:nrow(lab.lookup))

library(gdata)
lab.lookup$orig.labels <- reorder.factor(lab.lookup$orig.labels,
                                         new.order=lab.lookup$orig.labels[lab.lookup$figure.order])
lab.lookup$feature.labels <- reorder.factor(lab.lookup$feature.labels,
                                    new.order=lab.lookup$feature.labels[lab.lookup$figure.order])
detach(package:gdata)

### Get the data ready for plotting
data.m <- data.frame(r.names, data.o)
colnames(data.m)[1] <- "Name"
data.melt <- melt(data.m, id.vars=c("Name","Price"))
colnames(data.melt) <- c("Editor", "Price", "Feature", "Present")


### --------------------------------------------------
### Plot the table
### --------------------------------------------------

### --------------------------------------------------
### 1. A version of the Original
### --------------------------------------------------

library(gdata)
data.melt$Present <- reorder.factor(data.melt$Present,
                                     new.order=c("Yes", "No", "$$", "?"))
data.melt$Feature <- reorder.factor(data.melt$Feature,
                                    new.order=c.names)
data.melt$Price <- reorder.factor(data.melt$Price, new.order=c("Free",
                                                     "Free/$$",
                                                     "Free/2.99",
                                                     "$0.99", "$1.99",
                                                     "$2.99",
                                                     "$2/4.99",
                                                     "$3.99", "$4.99",
                                                     "$5.99",
                                                     "$5/8.99",
                                                     "$9.99", "$19.99"))
detach(package:gdata)

## Color palette
my.cols <- brewer.pal(9, "Pastel1")
my.cols <- my.cols[c(3,1,5,9)]

pdf(file="figures/spec-cluster-alphabetical.pdf", height=21, width=10, pointsize=12)
p <- ggplot(data.melt, aes(x=Feature, y=Editor, fill=Present,
                           color="black",
                           group=Price))
p + geom_tile() + scale_fill_manual(values=my.cols) + scale_x_discrete(labels=feature.labels) +
  opts(axis.text.x=theme_text(hjust=1, angle=90),
       axis.text.y=theme_text(hjust=1)) + scale_colour_discrete(guide="none") + opts(legend.position = "top")
credit()

dev.off()

png(file="figures/spec-cluster-alphabetical.png", height=1650, width=950, pointsize=18)
p <- ggplot(data.melt, aes(x=Feature, y=Editor, fill=Present,
                           color="black"))
p + geom_tile() + scale_fill_manual(values=my.cols) + scale_x_discrete(labels=feature.labels) +
  opts(axis.text.x=theme_text(hjust=1, angle=90),
       axis.text.y=theme_text(hjust=1)) + scale_colour_discrete(guide="none") + opts(legend.position = "top")
credit()

dev.off()


### --------------------------------------------------
### 2. Cluster on both
### --------------------------------------------------

library(gdata)
data.melt$Feature <- reorder.factor(data.melt$Feature,
                                    new.order=lab.lookup$orig.labels)
detach(package:gdata)



pdf(file="figures/spec-cluster-full.pdf", height=21, width=10, pointsize=12)
p <- ggplot(data.melt, aes(x=Feature, y=Editor, fill=Present,
                           color="black"))
p + geom_tile() + scale_fill_manual(values=my.cols) + scale_x_discrete(labels=lab.lookup$feature.labels) +
  opts(axis.text.x=theme_text(hjust=1, angle=90),
       axis.text.y=theme_text(hjust=1)) + scale_colour_discrete(guide="none") + opts(legend.position = "top")
credit()
dev.off()

png(file="figures/spec-cluster-full.png", height=1650, width=950, pointsize=18)
p <- ggplot(data.melt, aes(x=Feature, y=Editor, fill=Present,
                           color="black"))
p + geom_tile() + scale_fill_manual(values=my.cols) + scale_x_discrete(labels=lab.lookup$feature.labels) +
  opts(axis.text.x=theme_text(hjust=1, angle=90),
       axis.text.y=theme_text(hjust=1)) + scale_colour_discrete(guide="none") + opts(legend.position = "top")
credit()
dev.off()

### --------------------------------------------------
### 3. Break out by price
### --------------------------------------------------

pdf(file="figures/spec-cluster-by-price.pdf", height=21, width=10, pointsize=12)
p <- ggplot(data.melt, aes(x=Feature, y=Editor, fill=Present,
                           color="black",
                           group=Price))
p0 <- p + geom_tile() + scale_fill_manual(values=my.cols) + scale_x_discrete(labels=lab.lookup$feature.labels) +
  opts(axis.text.x=theme_text(hjust=1, angle=90),
       axis.text.y=theme_text(hjust=1)) # + coord_flip()
p0 + facet_grid(Price~., scales="free_y", space="free", as.table=TRUE,
                shrink=TRUE) + scale_colour_discrete(guide="none") +
  opts(legend.position = "top") +
  opts(strip.text.y=theme_text(size=9))
credit()
dev.off()

png(file="figures/spec-cluster-by-price.png", height=1650, width=950, pointsize=18)
p <- ggplot(data.melt, aes(x=Feature, y=Editor, fill=Present,
                           color="black",
                           group=Price))
p0 <- p + geom_tile() + scale_fill_manual(values=my.cols) + scale_x_discrete(labels=lab.lookup$feature.labels) +
  opts(axis.text.x=theme_text(hjust=1, angle=90),
       axis.text.y=theme_text(hjust=1)) # + coord_flip()
p0 + facet_grid(Price~., scales="free_y", space="free", as.table=TRUE,
                shrink=TRUE) + scale_colour_discrete(guide="none") +
  opts(legend.position = "top") +
  opts(strip.text.y=theme_text(size=9))
credit()
dev.off()
