## Martin Bontrager
# May 19, 2015
# Flavobacterium analysis from MOTHUR output

library(phyloseq)
library(ggplot2)
library(RColorBrewer)

setwd("D:/Users/Martin/Dropbox/Projects/flavobacterium/data/")

## Read in the .csv otu table and metadata
otuTable <- read.csv("2015-05-13_taxonomy_subtable.csv", header = TRUE, row.names = 1)
metaData <- read.csv("16S_metadata.csv", header = TRUE, row.names = 1)
ordered_samples <- as.vector((read.table("samples.txt"))[, 1])

## Create matrices for phyloseq
otumat <- as.matrix(subset(otuTable, select=c(8:length(colnames(otuTable)))))
otuTable[] <- lapply(otuTable, as.character)
taxmat <- as.matrix(subset(otuTable, select=c(1:7)))

## Declare phyloseq objects
OTU <- otu_table(otumat, taxa_are_rows = TRUE)
TAX <- tax_table(taxmat)
metaData <- sample_data(metaData)

physeq <- phyloseq(OTU, TAX, metaData)

## GGplot themeing
theme_set(theme_bw())
pal = "Set1"
scale_colour_discrete <- function(palname = pal, ...) {
        scale_colour_brewer(palette = palname, ...)
}
scale_fill_discrete <- function(palname = pal, ...) {
        scale_fill_brewer(palette = palname, ...)
}
getPalette <- colorRampPalette(brewer.pal(9, "Set1"))

## Prune taxa not present in any sample (if they exist)
## And remove unnecessary objects from memory
physeq_pruned <- prune_taxa(taxa_sums(physeq) > 0, physeq)
rm(otuTable, otumat, taxmat, physeq, TAX, OTU)
physeq_rabund <- transform_sample_counts(physeq_pruned, function(x) x / sum(x))

## Flavobacterium abundance plots
flavobact <- subset_taxa(physeq_rabund, Genus == "Flavobacterium")

theme_set(theme_bw() +
                  theme(axis.line = element_line(colour = "black"),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.border = element_blank(),
                        panel.background = element_blank()))

plot_bar(flavobact, fill = "Species", title = "Flavobacterium") + 
        scale_x_discrete(limits = ordered_samples) + 
        scale_fill_manual(values = getPalette(9))
        theme(axis.title.x = element_text(size = 12),
              axis.title.y = element_text(size = 12),
              plot.title = element_text(size = 24, face = "bold"))

