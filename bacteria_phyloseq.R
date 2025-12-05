# STREAM synoptic (Buonpane thesis)
# Re-analysis 
# bacteria/archaea/fungi

# setup environment----
library(ggplot2); library(tidyverse); library(phyloseq)

## Plot format----
pretty.theme <- function(){
  theme_bw() +
    theme(axis.text.x=element_text(size = 18, color="black"),
          axis.text.y=element_text(size = 18, color="black"),
          # panel.grid.major = element_blank(),
          # panel.grid.minor = element_blank(),
          text=element_text(size=18),
          panel.background = element_blank(),
          panel.border = element_rect(fill=NA),
          axis.line = element_line(colour = "black"))
}

## import data----
# change file names; need to reorder so metadata match otu table
mat = read.table("PHYLOSEQ-IMPORT/bacteria-ASV-table.txt", header = TRUE, sep = "\t", row.names = 1)
tax = read.table("PHYLOSEQ-IMPORT/bacteria-taxa-table.txt", header = TRUE, sep = "\t", row.names = 1)
meta = read.csv("PHYLOSEQ-IMPORT/bacteria_map.csv", header = TRUE)

dim(tax)
dim(mat)
dim(meta)

mat = as.matrix(mat)
tax = as.matrix(tax)

OTU <- otu_table(as.matrix(mat), taxa_are_rows = TRUE)
TAX = tax_table(tax)
META = sample_data(meta)

# If you have a SampleID column:
rownames(META) <- META$SampleID
META$SampleID <- NULL

phy = phyloseq(OTU,TAX,META)
phy

# Filtering----
phy %>%
  subset_taxa(Family != "Mitochondria",
              Order != "Chloroplast") -> phy.f
phy
phy.f 

per.f.final = transform_sample_counts(phy.f, function (x) x/sum(x)*100)

# Ordination----
# ordination----
BC_distance <- phyloseq::distance(per.f.final, "bray") 
bcOrd <- ordinate(per.f.final, "PCoA", BC_distance)
plot_scree(bcOrd)

# Are there other environmental variables you want to look for?
p1 <- plot_ordination(per.f.final, bcOrd, type = "samples") +
  geom_point(shape = 21, color = "black", stroke = 0.5, size = 5, alpha = 0.9) +
  scale_fill_viridis_d(option = "C") +
  pretty.theme() +
  labs(fill = "Treatment")
p1


# Extract OTU/ASV table as matrix
otu_mat <- as(otu_table(phy.f), "matrix")
if (taxa_are_rows(phy.f)) {
  otu_mat <- t(otu_mat)
}

# Rarefaction curves
rarecurve(otu_mat, step = 100, col = "blue", cex = 0.6)