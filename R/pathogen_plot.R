#copMean	watMean
#0.8114285714	0.0266666667
#5.3928571429	0.6077777778
#1.7692857143	0.0011111111

#medianCop	medianWat
#0.135	0.01
#2.56	0.06
#0.035	0
library(ggplot2)
library(reshape2)

# Plot title size, etc.


Genus <- c("Aeromonas", "Flavobacterium", "Chryseobacterium")
Copepod <- c(0.81, 5.39, 1.77)
Water <- c(0.03, 0.61, 0.001)
#Copepod <- c(0.135, 2.56, 0.035)
#Water <- c(0.01, 0.06, 0)

d <- as.data.frame(cbind(Genus, Copepod, Water))

theme_set(theme_bw(base_size = 30))
pal = "Set1"
dat.m <- melt(d,id.vars = "Genus")
dat.m$value <- as.numeric(dat.m$value)
png("pathogens.png", units="in", width=7, height=5, res=200)
ggplot(dat.m, aes(x = variable, y = value, fill=Genus)) +
    geom_bar(stat='identity') +
    ylab("Mean % Abundance") +
    theme(axis.title.x=element_blank(),
          axis.text=element_text(),
          axis.text.x=element_text(size=26),
          axis.title.y=element_text(face="bold"))+
    scale_fill_brewer(palette="Set1")
dev.off()
