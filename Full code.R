getwd()
setwd("D:/Downloads_Sep_2025/Aug 2025_aspaper1")  ##set your file path
setwd("/Users/divyaagrawal/Downloads") ###MAC
getwd()  ##to check your file path
###if not installed from install.packages (" ") then apply BiocManager:install(" ")
install.packages("BiocManager")
install.packages("FactoMineR")
install.packages("factoextra")
install.packages("readxl")
install.packages("dplyr")
install.packages("ggpubr")
install.packages("plotly")
BiocManager::install("dendextend")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("openxlsx")
install.packages("ggrepel")
install.packages("patchwork")
install.packages("extrafont")
install.packages("gridGraphics")


library(BiocManager)
library(FactoMineR)
library(factoextra)
library(readxl)
library(ggpubr)
library(dplyr)
library(plotly)
library(dendextend)
library(ggplot2)
library(reshape2)
library(openxlsx)
library(ggrepel)
library(patchwork)
library(extrafont)
library(gridGraphics)

###########################################################FTIR#####
df <- read_excel("~/Downloads/paper_spectrscopy_nov2025/FTIR_PCA_updated.xlsx") #in mac

# First column = group labels
groups <- df[[1]]
X <- as.matrix(df[, -1])

colnames(X)                     
# Truncate to 1 decimal place  ##use it for the FTIR and FLR dataset only
colnames(X) <- as.character(floor(as.numeric(colnames(X)) * 10) / 10)     ##use it for the FTIR dataset only
View(X)                     ##use it for the FTIR and FLR dataset only

# -----------------------------
# Perform PCA using FactoMineR
# -----------------------------
res.pca <- PCA(X, scale.unit = TRUE, ncp = 5, graph = TRUE)

eig.val <- get_eigenvalue(res.pca)
eig.val
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 100))

var <- get_pca_var(res.pca)
var

############sample group on side##################
##########################################################

############removed sample group on side##################
##########################################################
# Calculate variance explained
eig.val <- get_eigenvalue(res.pca)
pc1_var <- round(eig.val[1, 2], 1)  # % variance for PC1
pc2_var <- round(eig.val[2, 2], 1)  # % variance for PC2


# Define custom colors
colors <- c('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728',
            '#9467bd', '#8c564b', '#e377c2', '#7f7f7f',
            '#bcbd22', '#17becf')

# PCA 2D plot (base)
ind.p <- fviz_pca_ind(res.pca, axes = c(1, 2),  
                      pointsize = 3, 
                      pointshape = 21, 
                      fill = df$wavelength,        # fill by group
                      col.ind = df$wavelength,     # color points by group
                      geom.ind = "point", 
                      addEllipses = TRUE, 
                      ellipse.level = 0.95,
                      ellipse.type = "confidence",    ##confidence   ###t
                      palette = colors,            # custom palette
                      label = "none", 
                      mean.point = FALSE,
                      repel = TRUE, 
                      labelsize = 5)
# Beautify (with % variance in axis labels) + right legend
ind.p <- ggpubr::ggpar(
  ind.p,
  xlab = paste0("PC1 (", pc1_var, "%)"),
  ylab = paste0("PC2 (", pc2_var, "%)"),
  legend = "right",
  ggtheme = theme_classic()
)

# ---- Make axis, axis numbers, and labels bold + increase size ----
ind.p <- ind.p +
  theme(
    axis.title.x = element_text(face = "plain", size = 18, colour = "black"),
    axis.title.y = element_text(face = "plain", size = 18, colour = "black"),
    axis.text.x  = element_text(face = "plain", size = 18, colour = "black"),
    axis.text.y  = element_text(face = "plain", size = 18, colour = "black"),
    axis.line    = element_line(linewidth = 0.5, colour = "black"),
    
    legend.title = element_text(size = 14, face = "plain"),
    legend.text  = element_text(size = 12),
    legend.key.size = unit(0.6, "cm")
  ) +
  guides(
    colour = "none",                       # remove colour legend
    fill   = guide_legend(title = "Samples")  # rename legend title
  ) +
  coord_cartesian(ylim = c(-10, 10), xlim = c(-50, 100))


pA <- ind.p + theme(
  plot.title = element_blank(),
  plot.subtitle = element_blank()
)
pA

###Biplot
###PCA biplot for publications for FTIR for choosen variables#############
###########################################################################
chosen_vars <- c("1408.7",          ##FTIR choosen 10 variables ##previously it was 1726, and 1787.7 instead of that we have 1684 and 1652.7
                 "1443.4",
                 "1507.1",
                 "1539.4",
                 "1559.1",
                 "1586.1",
                 "1599.2",
                 "1699.9",
                 "1652.7",
                 "1684"
)

p <- fviz_pca_biplot(
  res.pca,
  geom.ind    = "point",
  fill.ind    = df$wavelength,
  col.ind     = "black",          # keep point border black
  pointshape  = 21,
  pointsize   = 2.5,
  palette     = colors,
  addEllipses = FALSE,
  col.var     = "black",
  repel       = TRUE,
  label       = "none",
  mean.point  = FALSE,
  select.var  = list(name = chosen_vars)
)

# ---- Apply same theme & legend settings as score plot ----
p <- ggpubr::ggpar(
  p,
  legend  = "right",
  ggtheme = theme_classic()
)

p <- p +
  theme(
    axis.title.x = element_text(face = "plain", size = 18, colour = "black"),
    axis.title.y = element_text(face = "plain", size = 18, colour = "black"),
    axis.text.x  = element_text(face = "plain", size = 18, colour = "black"),
    axis.text.y  = element_text(face = "plain", size = 18, colour = "black"),
    axis.line    = element_line(linewidth = 0.5, colour = "black"),
    
    legend.title = element_text(size = 14, face = "plain"),
    legend.text  = element_text(size = 12),
    legend.key.size = unit(0.6, "cm")
  ) +
  guides(
    colour = "none",                      # ❌ remove colour legend
    fill   = guide_legend(
      title = "Samples",
      override.aes = list(size = 5)       # bigger legend points
    )
  )


# Extract coordinates for chosen variables
var_coords <- as.data.frame(res.pca$var$coord[chosen_vars, 1:2]) * 5
var_coords$var <- rownames(var_coords)

p_final <- p +
  geom_label_repel(
    data = var_coords,
    aes(x = Dim.1, y = Dim.2, label = var),
    fill = "white",
    color = "black",
    size = 5,
    fontface = "plain",
    box.padding = 1.5,
    point.padding = 0.8,
    max.overlaps = Inf,
    nudge_x = 3,
    nudge_y = 3,
    segment.color = "black",
    segment.linetype = "dotted",
    segment.size = 0.6
  ) +
  labs(
    x = "PC1 (93.2%)",
    y = "PC2 (5.7%)"
  ) +
  coord_cartesian(xlim = c(-50, 100), ylim = c(-10, 10))



pB <- p_final + theme(
  plot.title = element_blank(),
  plot.subtitle = element_blank()
)
pB


##########################################################FAR UV CD##########
###################
df <- read_excel("~/Downloads/paper_spectrscopy_nov2025/CD_PCA_updated.xlsx") #in mac

# First column = group labels
groups <- df[[1]]
X <- as.matrix(df[, -1])

colnames(X)                     
# Truncate to 1 decimal place  ##use it for the FTIR and FLR dataset only
#colnames(X) <- as.character(floor(as.numeric(colnames(X)) * 10) / 10)     ##use it for the FTIR dataset only
#View(X)                     ##use it for the FTIR and FLR dataset only

# -----------------------------
# Perform PCA using FactoMineR
# -----------------------------
res.pca <- PCA(X, scale.unit = TRUE, ncp = 5, graph = TRUE)

eig.val <- get_eigenvalue(res.pca)
eig.val
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 100))

var <- get_pca_var(res.pca)
var

###########sample group on side##################
##########################################################
# Calculate variance explained
eig.val <- get_eigenvalue(res.pca)
pc1_var <- round(eig.val[1, 2], 1)  # % variance for PC1
pc2_var <- round(eig.val[2, 2], 1)  # % variance for PC2


# Define custom colors
colors <- c('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728',
            '#9467bd', '#8c564b', '#e377c2', '#7f7f7f',
            '#bcbd22', '#17becf')

# PCA 2D plot (base)
ind.p <- fviz_pca_ind(res.pca, axes = c(1, 2),  
                      pointsize = 3, 
                      pointshape = 21, 
                      fill = df$wavelength,        # fill by group
                      col.ind = df$wavelength,     # color points by group
                      geom.ind = "point", 
                      addEllipses = TRUE, 
                      ellipse.level = 0.95,
                      ellipse.type = "confidence",    ##confidence   ###t
                      palette = colors,            # custom palette
                      label = "none", 
                      mean.point = FALSE,
                      repel = TRUE, 
                      labelsize = 5)
# Beautify (with % variance in axis labels) + right legend
ind.p <- ggpubr::ggpar(
  ind.p,
  xlab = paste0("PC1 (", pc1_var, "%)"),
  ylab = paste0("PC2 (", pc2_var, "%)"),
  legend = "right",
  ggtheme = theme_classic()
)

# ---- Make axis, axis numbers, and labels bold + increase size ----
ind.p <- ind.p +
  theme(
    axis.title.x = element_text(face = "plain", size = 18, colour = "black"),
    axis.title.y = element_text(face = "plain", size = 18, colour = "black"),
    axis.text.x  = element_text(face = "plain", size = 18, colour = "black"),
    axis.text.y  = element_text(face = "plain", size = 18, colour = "black"),
    axis.line    = element_line(linewidth = 0.5, colour = "black"),
    
    legend.title = element_text(size = 14, face = "plain"),
    legend.text  = element_text(size = 12),
    legend.key.size = unit(0.6, "cm")
  ) +
  guides(
    colour = "none",                       # remove colour legend
    fill   = guide_legend(title = "Samples")  # rename legend title
  ) + coord_cartesian(ylim = c(-10, 10), xlim = c(-12, 10))

pC <- ind.p + theme(
  plot.title = element_blank(),
  plot.subtitle = element_blank()
)
pC

##biplot
#################PCA biplo final for publications ###for all spectrosocpy except FTIR

# Custom colors
colors <- c('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728',
            '#9467bd', '#8c564b', '#e377c2', '#7f7f7f',
            '#bcbd22', '#17becf')

# Get contributions of variables to PC1 and PC2
var_contrib <- get_pca_var(res.pca)$contrib[,1] + get_pca_var(res.pca)$contrib[,2]

#contrib <- as.data.frame(var_contrib)   ##modified 9th Sep
#write.xlsx(contrib, "contrib_FTIR_ex1.xlsx")

# Select top 10 variables by contribution
top_vars <- names(sort(var_contrib, decreasing = TRUE))[1:10]  ##modified 3rd sep 
top_vars

# Base biplot with only top 10 variable arrows

p <- fviz_pca_biplot(
  res.pca,
  geom.ind    = "point",
  fill.ind    = df$wavelength,
  col.ind     = "black",          # keep point border black
  pointshape  = 21,
  pointsize   = 2.5,
  palette     = colors,
  addEllipses = FALSE,
  col.var     = "black",
  repel       = TRUE,
  label       = "none",
  mean.point  = FALSE,
  select.var  = list(name = top_vars)
)

# ---- Apply same theme & legend settings as score plot ----
p <- ggpubr::ggpar(
  p,
  legend  = "right",
  ggtheme = theme_classic()
)

p <- p +
  theme(
    axis.title.x = element_text(face = "plain", size = 18, colour = "black"),
    axis.title.y = element_text(face = "plain", size = 18, colour = "black"),
    axis.text.x  = element_text(face = "plain", size = 18, colour = "black"),
    axis.text.y  = element_text(face = "plain", size = 18, colour = "black"),
    axis.line    = element_line(linewidth = 0.5, colour = "black"),
    
    legend.title = element_text(size = 14, face = "plain"),
    legend.text  = element_text(size = 12),
    legend.key.size = unit(0.6, "cm")
  ) +
  guides(
    colour = "none",                      # ❌ remove colour legend
    fill   = guide_legend(
      title = "Samples",
      override.aes = list(size = 5)       # bigger legend points
    )
  )

# Extract coordinates for those top 10 variables
# Extract coordinates for chosen variables
var_coords <- as.data.frame(res.pca$var$coord[top_vars, 1:2]) * 5
var_coords$var <- rownames(var_coords)

p_final <- p +
  geom_label_repel(
    data = var_coords,
    aes(x = Dim.1, y = Dim.2, label = var),
    fill = "white",
    color = "black",
    size = 5,
    fontface = "plain",
    box.padding = 1.5,
    point.padding = 0.8,
    max.overlaps = Inf,
    nudge_x = 3,
    nudge_y = 3,
    segment.color = "black",
    segment.linetype = "dotted",
    segment.size = 0.6
  ) +
  labs(
    x = "PC1 (37.2%)",
    y = "PC2 (7.8%)"
  ) +
  coord_cartesian(ylim = c(-10, 10), xlim = c(-12, 10))


##+coord_cartesian(xlim = c(-50, 100), ylim = c(-50, 100))
##top contributing variables
var_contrib <- get_pca_var(res.pca)$contrib[,1] + get_pca_var(res.pca)$contrib[,2]
top_combined <- sort(var_contrib, decreasing = TRUE)[1:10]
top_combined
#
pD <- p_final + theme(
  plot.title = element_blank(),
  plot.subtitle = element_blank()
)
pD

###Figure 2 (Panel A,B, C, D) for both FTIR and Far UV CD

#pA   # Figure A
#pB   # Figure B
#pC   # Figure C
#pD   # Figure D

#final_fig <- (pA | pB) /
#  (pC | pD) +
#  plot_annotation(
 #   tag_levels = "A",
 #   theme = theme(
 #     plot.tag = element_text(
  #      family = "Arial",
   #     face   = "bold",
    #    size   = 500   # ⬅ increase panel label size here
   #   )
  #  )
 # )
#

#ggsave(
  #filename = "Figure_3.pdf",
 # plot     = final_fig,
 # width    = 25,
  #height   = 25,
 # units    = "cm"
#)
#ggsave(
  #filename = "Figure_3.jpg",
  #plot     = final_fig,
  #device   = "jpeg",
  #width    = 25,
 # height   = 25,
  #units    = "cm",
  #dpi      = 300,
  #quality  = 100
#)


#font_import()   # once only
#loadfonts()

#####cowplot
library(cowplot)
combined <- plot_grid(
  pA, pB,
  pC, pD,
  ncol = 2,
  align = "hv"
)
final_fig <- ggdraw(combined) +
  draw_plot_label(
    label = c("A", "B", "C", "D"),
    x = c(0.02, 0.52, 0.02, 0.52),
    y = c(0.98, 0.98, 0.48, 0.48),
    hjust = 0,
    vjust = 1,
    fontfamily = "Arial",
    fontface   = "bold",
    size       = 24
  )
ggsave(
  "Figure_3.jpg",
  final_fig,
  width = 25,
  height = 25,
  units = "cm",
  dpi = 300,
  quality = 100
)
ggsave(
  "Figure_3.pdf",
  final_fig,
  width = 25,
  height = 25,
  units = "cm"
)
font_import()   # once only
loadfonts()

####################################################Intrinsic FLR################Figure 4
###################
df <- read_excel("~/Downloads/paper_spectrscopy_nov2025/FLR_280_PCA.xlsx") #in mac

# First column = group labels
groups <- df[[1]]
X <- as.matrix(df[, -1])

colnames(X)                     
# Truncate to 1 decimal place  ##use it for the FTIR and FLR dataset only
colnames(X) <- as.character(floor(as.numeric(colnames(X)) * 10) / 10)     ##use it for the FTIR dataset only
View(X)                     ##use it for the FTIR and FLR dataset only

# -----------------------------
# Perform PCA using FactoMineR
# -----------------------------
res.pca <- PCA(X, scale.unit = TRUE, ncp = 5, graph = TRUE)

eig.val <- get_eigenvalue(res.pca)
eig.val
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 100))

var <- get_pca_var(res.pca)
var

###########sample group on side##################
##########################################################
# Calculate variance explained
eig.val <- get_eigenvalue(res.pca)
pc1_var <- round(eig.val[1, 2], 1)  # % variance for PC1
pc2_var <- round(eig.val[2, 2], 1)  # % variance for PC2


# Define custom colors
colors <- c('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728',
            '#9467bd', '#8c564b', '#e377c2', '#7f7f7f',
            '#bcbd22', '#17becf')

# PCA 2D plot (base)
ind.p <- fviz_pca_ind(res.pca, axes = c(1, 2),  
                      pointsize = 3, 
                      pointshape = 21, 
                      fill = df$wavelength,        # fill by group
                      col.ind = df$wavelength,     # color points by group
                      geom.ind = "point", 
                      addEllipses = TRUE, 
                      ellipse.level = 0.95,
                      ellipse.type = "confidence",    ##confidence   ###t
                      palette = colors,            # custom palette
                      label = "none", 
                      mean.point = FALSE,
                      repel = TRUE, 
                      labelsize = 5)
# Beautify (with % variance in axis labels) + right legend
ind.p <- ggpubr::ggpar(
  ind.p,
  xlab = paste0("PC1 (", pc1_var, "%)"),
  ylab = paste0("PC2 (", pc2_var, "%)"),
  legend = "right",
  ggtheme = theme_classic()
)

# ---- Make axis, axis numbers, and labels bold + increase size ----
ind.p <- ind.p +
  theme(
    axis.title.x = element_text(face = "plain", size = 18, colour = "black"),
    axis.title.y = element_text(face = "plain", size = 18, colour = "black"),
    axis.text.x  = element_text(face = "plain", size = 18, colour = "black"),
    axis.text.y  = element_text(face = "plain", size = 18, colour = "black"),
    axis.line    = element_line(linewidth = 0.5, colour = "black"),
    
    legend.title = element_text(size = 14, face = "plain"),
    legend.text  = element_text(size = 12),
    legend.key.size = unit(0.6, "cm")
  ) +
  guides(
    colour = "none",                       # remove colour legend
    fill   = guide_legend(title = "Samples")  # rename legend title
  ) + coord_cartesian(ylim = c(-5, 12), xlim = c(-20, 20))

pA <- ind.p + theme(
  plot.title = element_blank(),
  plot.subtitle = element_blank()
)
pA

##biplot
#################PCA biplo final for publications ###for all spectrosocpy except FTIR

# Custom colors
colors <- c('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728',
            '#9467bd', '#8c564b', '#e377c2', '#7f7f7f',
            '#bcbd22', '#17becf')

# Get contributions of variables to PC1 and PC2
var_contrib <- get_pca_var(res.pca)$contrib[,1] + get_pca_var(res.pca)$contrib[,2]

#contrib <- as.data.frame(var_contrib)   ##modified 9th Sep
#write.xlsx(contrib, "contrib_FTIR_ex1.xlsx")

# Select top 10 variables by contribution
top_vars <- names(sort(var_contrib, decreasing = TRUE))[1:10]  ##modified 3rd sep 
top_vars

# Base biplot with only top 10 variable arrows

p <- fviz_pca_biplot(
  res.pca,
  geom.ind    = "point",
  fill.ind    = df$wavelength,
  col.ind     = "black",          # keep point border black
  pointshape  = 21,
  pointsize   = 2.5,
  palette     = colors,
  addEllipses = FALSE,
  col.var     = "black",
  repel       = TRUE,
  label       = "none",
  mean.point  = FALSE,
  select.var  = list(name = top_vars)
)

# ---- Apply same theme & legend settings as score plot ----
p <- ggpubr::ggpar(
  p,
  legend  = "right",
  ggtheme = theme_classic()
)

p <- p +
  theme(
    axis.title.x = element_text(face = "plain", size = 18, colour = "black"),
    axis.title.y = element_text(face = "plain", size = 18, colour = "black"),
    axis.text.x  = element_text(face = "plain", size = 18, colour = "black"),
    axis.text.y  = element_text(face = "plain", size = 18, colour = "black"),
    axis.line    = element_line(linewidth = 0.5, colour = "black"),
    
    legend.title = element_text(size = 14, face = "plain"),
    legend.text  = element_text(size = 12),
    legend.key.size = unit(0.6, "cm")
  ) +
  guides(
    colour = "none",                      # ❌ remove colour legend
    fill   = guide_legend(
      title = "Samples",
      override.aes = list(size = 5)       # bigger legend points
    )
  )

# Extract coordinates for those top 10 variables
# Extract coordinates for chosen variables
var_coords <- as.data.frame(res.pca$var$coord[top_vars, 1:2]) * 5
var_coords$var <- rownames(var_coords)

p_final <- p +
  geom_label_repel(
    data = var_coords,
    aes(x = Dim.1, y = Dim.2, label = var),
    fill = "white",
    color = "black",
    size = 5,
    fontface = "plain",
    box.padding = 1.5,
    point.padding = 0.8,
    max.overlaps = Inf,
    nudge_x = 3,
    nudge_y = 3,
    segment.color = "black",
    segment.linetype = "dotted",
    segment.size = 0.6
  ) +
  labs(
    x = "PC1 (85%)",
    y = "PC2 (4.5%)"
  ) +
  coord_cartesian(ylim = c(-5, 12), xlim = c(-20, 20))


##+coord_cartesian(xlim = c(-50, 100), ylim = c(-50, 100))
##top contributing variables
var_contrib <- get_pca_var(res.pca)$contrib[,1] + get_pca_var(res.pca)$contrib[,2]
top_combined <- sort(var_contrib, decreasing = TRUE)[1:10]
top_combined
#
pB <- p_final + theme(
  plot.title = element_blank(),
  plot.subtitle = element_blank()
)
pB

#####################################################eXTRINSIC FLR####
###################
df <- read_excel("~/Downloads/paper_spectrscopy_nov2025/FLR_ex_PCA.xlsx") #in mac

# First column = group labels
groups <- df[[1]]
X <- as.matrix(df[, -1])

colnames(X)                     
# Truncate to 1 decimal place  ##use it for the FTIR and FLR dataset only
colnames(X) <- as.character(floor(as.numeric(colnames(X)) * 10) / 10)     ##use it for the FTIR dataset only
View(X)                     ##use it for the FTIR and FLR dataset only

# -----------------------------
# Perform PCA using FactoMineR
# -----------------------------
res.pca <- PCA(X, scale.unit = TRUE, ncp = 5, graph = TRUE)

eig.val <- get_eigenvalue(res.pca)
eig.val
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 100))

var <- get_pca_var(res.pca)
var

###########sample group on side##################
##########################################################
# Calculate variance explained
eig.val <- get_eigenvalue(res.pca)
pc1_var <- round(eig.val[1, 2], 1)  # % variance for PC1
pc2_var <- round(eig.val[2, 2], 1)  # % variance for PC2


# Define custom colors
colors <- c('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728',
            '#9467bd', '#8c564b', '#e377c2', '#7f7f7f',
            '#bcbd22', '#17becf')

# PCA 2D plot (base)
ind.p <- fviz_pca_ind(res.pca, axes = c(1, 2),  
                      pointsize = 3, 
                      pointshape = 21, 
                      fill = df$wavelength,        # fill by group
                      col.ind = df$wavelength,     # color points by group
                      geom.ind = "point", 
                      addEllipses = TRUE, 
                      ellipse.level = 0.95,
                      ellipse.type = "confidence",    ##confidence   ###t
                      palette = colors,            # custom palette
                      label = "none", 
                      mean.point = FALSE,
                      repel = TRUE, 
                      labelsize = 5)
# Beautify (with % variance in axis labels) + right legend
ind.p <- ggpubr::ggpar(
  ind.p,
  xlab = paste0("PC1 (", pc1_var, "%)"),
  ylab = paste0("PC2 (", pc2_var, "%)"),
  legend = "right",
  ggtheme = theme_classic()
)

# ---- Make axis, axis numbers, and labels bold + increase size ----
ind.p <- ind.p +
  theme(
    axis.title.x = element_text(face = "plain", size = 18, colour = "black"),
    axis.title.y = element_text(face = "plain", size = 18, colour = "black"),
    axis.text.x  = element_text(face = "plain", size = 18, colour = "black"),
    axis.text.y  = element_text(face = "plain", size = 18, colour = "black"),
    axis.line    = element_line(linewidth = 0.5, colour = "black"),
    
    legend.title = element_text(size = 14, face = "plain"),
    legend.text  = element_text(size = 12),
    legend.key.size = unit(0.6, "cm")
  ) +
  guides(
    colour = "none",                       # remove colour legend
    fill   = guide_legend(title = "Samples")  # rename legend title
  ) + coord_cartesian(ylim = c(-10, 12), xlim = c(-20, 30))

pC <- ind.p + theme(
  plot.title = element_blank(),
  plot.subtitle = element_blank()
)
pC

##biplot
#################PCA biplo final for publications #

# Custom colors
colors <- c('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728',
            '#9467bd', '#8c564b', '#e377c2', '#7f7f7f',
            '#bcbd22', '#17becf')

# Get contributions of variables to PC1 and PC2
var_contrib <- get_pca_var(res.pca)$contrib[,1] + get_pca_var(res.pca)$contrib[,2]

#contrib <- as.data.frame(var_contrib)   ##modified 9th Sep
#write.xlsx(contrib, "contrib_FTIR_ex1.xlsx")

# Select top 10 variables by contribution
top_vars <- names(sort(var_contrib, decreasing = TRUE))[1:10]  ##modified 3rd sep 
top_vars

# Base biplot with only top 10 variable arrows

p <- fviz_pca_biplot(
  res.pca,
  geom.ind    = "point",
  fill.ind    = df$wavelength,
  col.ind     = "black",          # keep point border black
  pointshape  = 21,
  pointsize   = 2.5,
  palette     = colors,
  addEllipses = FALSE,
  col.var     = "black",
  repel       = TRUE,
  label       = "none",
  mean.point  = FALSE,
  select.var  = list(name = top_vars)
)

# ---- Apply same theme & legend settings as score plot ----
p <- ggpubr::ggpar(
  p,
  legend  = "right",
  ggtheme = theme_classic()
)

p <- p +
  theme(
    axis.title.x = element_text(face = "plain", size = 18, colour = "black"),
    axis.title.y = element_text(face = "plain", size = 18, colour = "black"),
    axis.text.x  = element_text(face = "plain", size = 18, colour = "black"),
    axis.text.y  = element_text(face = "plain", size = 18, colour = "black"),
    axis.line    = element_line(linewidth = 0.5, colour = "black"),
    
    legend.title = element_text(size = 14, face = "plain"),
    legend.text  = element_text(size = 12),
    legend.key.size = unit(0.6, "cm")
  ) +
  guides(
    colour = "none",                      # ❌ remove colour legend
    fill   = guide_legend(
      title = "Samples",
      override.aes = list(size = 5)       # bigger legend points
    )
  )

# Extract coordinates for those top 10 variables
# Extract coordinates for chosen variables
var_coords <- as.data.frame(res.pca$var$coord[top_vars, 1:2]) * 5
var_coords$var <- rownames(var_coords)

p_final <- p +
  geom_label_repel(
    data = var_coords,
    aes(x = Dim.1, y = Dim.2, label = var),
    fill = "white",
    color = "black",
    size = 5,
    fontface = "plain",
    box.padding = 1.5,
    point.padding = 0.8,
    max.overlaps = Inf,
    nudge_x = 3,
    nudge_y = 3,
    segment.color = "black",
    segment.linetype = "dotted",
    segment.size = 0.6
  ) +
  labs(
    x = "PC1 (81.6%)",
    y = "PC2 (4.6%)"
  ) +
  coord_cartesian(ylim = c(-10, 12), xlim = c(-20, 30))


##+coord_cartesian(xlim = c(-50, 100), ylim = c(-50, 100))
##top contributing variables
var_contrib <- get_pca_var(res.pca)$contrib[,1] + get_pca_var(res.pca)$contrib[,2]
top_combined <- sort(var_contrib, decreasing = TRUE)[1:10]
top_combined
#
pD <- p_final + theme(
  plot.title = element_blank(),
  plot.subtitle = element_blank()
)
pD

########################################Figure 4###
#####cowplot
library(cowplot)
combined <- plot_grid(
  pA, pB,
  pC, pD,
  ncol = 2,
  align = "hv"
)
final_fig <- ggdraw(combined) +
  draw_plot_label(
    label = c("A", "B", "C", "D"),
    x = c(0.02, 0.52, 0.02, 0.52),
    y = c(0.98, 0.98, 0.48, 0.48),
    hjust = 0,
    vjust = 1,
    fontfamily = "Arial",
    fontface   = "bold",
    size       = 24
  )
ggsave(
  "Figure_4.jpg",
  final_fig,
  width = 25,
  height = 25,
  units = "cm",
  dpi = 300,
  quality = 100
)
ggsave(
  "Figure_4.pdf",
  final_fig,
  width = 25,
  height = 25,
  units = "cm"
)
font_import()   # once only
loadfonts()

##################################UV Vis spec ###Supplementary Figure ####
df <- read_excel("~/Downloads/paper_spectrscopy_nov2025/UVspec_PCA.xlsx") #in mac

# First column = group labels
groups <- df[[1]]
X <- as.matrix(df[, -1])

colnames(X)                     
# Truncate to 1 decimal place  ##use it for the FTIR and FLR dataset only
#colnames(X) <- as.character(floor(as.numeric(colnames(X)) * 10) / 10)     ##use it for the FTIR dataset only
#View(X)                     ##use it for the FTIR and FLR dataset only

# -----------------------------
# Perform PCA using FactoMineR
# -----------------------------
res.pca <- PCA(X, scale.unit = TRUE, ncp = 5, graph = TRUE)

eig.val <- get_eigenvalue(res.pca)
eig.val
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 100))

var <- get_pca_var(res.pca)
var

###########sample group on side##################
##########################################################
# Calculate variance explained
eig.val <- get_eigenvalue(res.pca)
pc1_var <- round(eig.val[1, 2], 1)  # % variance for PC1
pc2_var <- round(eig.val[2, 2], 1)  # % variance for PC2


# Define custom colors
colors <- c('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728',
            '#9467bd', '#8c564b', '#e377c2', '#7f7f7f',
            '#bcbd22', '#17becf')

# PCA 2D plot (base)
ind.p <- fviz_pca_ind(res.pca, axes = c(1, 2),  
                      pointsize = 3, 
                      pointshape = 21, 
                      fill = df$wavelength,        # fill by group
                      col.ind = df$wavelength,     # color points by group
                      geom.ind = "point", 
                      addEllipses = TRUE, 
                      ellipse.level = 0.95,
                      ellipse.type = "confidence",    ##confidence   ###t
                      palette = colors,            # custom palette
                      label = "none", 
                      mean.point = FALSE,
                      repel = TRUE, 
                      labelsize = 5)
# Beautify (with % variance in axis labels) + right legend
ind.p <- ggpubr::ggpar(
  ind.p,
  xlab = paste0("PC1 (", pc1_var, "%)"),
  ylab = paste0("PC2 (", pc2_var, "%)"),
  legend = "right",
  ggtheme = theme_classic()
)

# ---- Make axis, axis numbers, and labels bold + increase size ----
ind.p <- ind.p +
  theme(
    axis.title.x = element_text(face = "plain", size = 18, colour = "black"),
    axis.title.y = element_text(face = "plain", size = 18, colour = "black"),
    axis.text.x  = element_text(face = "plain", size = 18, colour = "black"),
    axis.text.y  = element_text(face = "plain", size = 18, colour = "black"),
    axis.line    = element_line(linewidth = 0.5, colour = "black"),
    
    legend.title = element_text(size = 14, face = "plain"),
    legend.text  = element_text(size = 12),
    legend.key.size = unit(0.6, "cm")
  ) +
  guides(
    colour = "none",                       # remove colour legend
    fill   = guide_legend(title = "Samples")  # rename legend title
  ) + coord_cartesian(ylim = c(-15, 12), xlim = c(-20, 40))

pA <- ind.p + theme(
  plot.title = element_blank(),
  plot.subtitle = element_blank()
)
pA

##biplot
#################PCA biplo final for publications #

# Custom colors
colors <- c('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728',
            '#9467bd', '#8c564b', '#e377c2', '#7f7f7f',
            '#bcbd22', '#17becf')

# Get contributions of variables to PC1 and PC2
var_contrib <- get_pca_var(res.pca)$contrib[,1] + get_pca_var(res.pca)$contrib[,2]

#contrib <- as.data.frame(var_contrib)   ##modified 9th Sep
#write.xlsx(contrib, "contrib_FTIR_ex1.xlsx")

# Select top 10 variables by contribution
top_vars <- names(sort(var_contrib, decreasing = TRUE))[1:10]  ##modified 3rd sep 
top_vars

# Base biplot with only top 10 variable arrows

p <- fviz_pca_biplot(
  res.pca,
  geom.ind    = "point",
  fill.ind    = df$wavelength,
  col.ind     = "black",          # keep point border black
  pointshape  = 21,
  pointsize   = 2.5,
  palette     = colors,
  addEllipses = FALSE,
  col.var     = "black",
  repel       = TRUE,
  label       = "none",
  mean.point  = FALSE,
  select.var  = list(name = top_vars)
)

# ---- Apply same theme & legend settings as score plot ----
p <- ggpubr::ggpar(
  p,
  legend  = "right",
  ggtheme = theme_classic()
)

p <- p +
  theme(
    axis.title.x = element_text(face = "plain", size = 18, colour = "black"),
    axis.title.y = element_text(face = "plain", size = 18, colour = "black"),
    axis.text.x  = element_text(face = "plain", size = 18, colour = "black"),
    axis.text.y  = element_text(face = "plain", size = 18, colour = "black"),
    axis.line    = element_line(linewidth = 0.5, colour = "black"),
    
    legend.title = element_text(size = 14, face = "plain"),
    legend.text  = element_text(size = 12),
    legend.key.size = unit(0.6, "cm")
  ) +
  guides(
    colour = "none",                      # ❌ remove colour legend
    fill   = guide_legend(
      title = "Samples",
      override.aes = list(size = 5)       # bigger legend points
    )
  )

# Extract coordinates for those top 10 variables
# Extract coordinates for chosen variables
var_coords <- as.data.frame(res.pca$var$coord[top_vars, 1:2]) * 5
var_coords$var <- rownames(var_coords)

p_final <- p +
  geom_label_repel(
    data = var_coords,
    aes(x = Dim.1, y = Dim.2, label = var),
    fill = "white",
    color = "black",
    size = 5,
    fontface = "plain",
    box.padding = 1.5,
    point.padding = 0.8,
    max.overlaps = Inf,
    nudge_x = 3,
    nudge_y = 3,
    segment.color = "black",
    segment.linetype = "dotted",
    segment.size = 0.6
  ) +
  labs(
    x = "PC1 (76.8%)",
    y = "PC2 (21.9%)"
  ) +
  coord_cartesian(ylim = c(-15, 12), xlim = c(-20, 40))


##+coord_cartesian(xlim = c(-50, 100), ylim = c(-50, 100))
##top contributing variables
var_contrib <- get_pca_var(res.pca)$contrib[,1] + get_pca_var(res.pca)$contrib[,2]
top_combined <- sort(var_contrib, decreasing = TRUE)[1:10]
top_combined
#
pB <- p_final + theme(
  plot.title = element_blank(),
  plot.subtitle = element_blank()
)
pB

#######################################Supplementary Figure 
#####cowplot
library(cowplot)
combined <- plot_grid(
  pA, pB,
  ncol = 2,
  align = "hv"
)
final_fig <- ggdraw(combined) +
  draw_plot_label(
    label = c("A", "B"),
    x = c(0.02, 0.52),
    y = c(0.98, 0.98),
    hjust = 0,
    vjust = 1,
    fontfamily = "Arial",
    fontface   = "bold",
    size       = 24
  )
#x = c(0.02, 0.52, 0.02, 0.52),
#y = c(0.98, 0.98, 0.48, 0.48),
ggsave(
  "Supp Figure.jpg",
  final_fig,
  width = 25,
  height = 12.5,
  units = "cm",
  dpi = 300,
  quality = 100
)
ggsave(
  "Supp Figure.pdf",
  final_fig,
  width = 25,
  height = 12.5,
  units = "cm"
)
font_import()   # once only
loadfonts()

######### #####################################combination ########## Figure 5##

df <- read_excel("~/Downloads/paper_spectrscopy_nov2025/combination.xlsx") #in mac

# First column = group labels
groups <- df[[1]]
X <- as.matrix(df[, -1])

colnames(X)                     
# Truncate to 1 decimal place  ##use it for the FTIR and FLR dataset only
#colnames(X) <- as.character(floor(as.numeric(colnames(X)) * 10) / 10)     ##use it for the FTIR dataset only
#View(X)                     ##use it for the FTIR and FLR dataset only

# -----------------------------
# Perform PCA using FactoMineR
# -----------------------------
res.pca <- PCA(X, scale.unit = TRUE, ncp = 5, graph = TRUE)

eig.val <- get_eigenvalue(res.pca)
eig.val
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 100))

var <- get_pca_var(res.pca)
var

###########sample group on side##################
##########################################################
# Calculate variance explained
eig.val <- get_eigenvalue(res.pca)
pc1_var <- round(eig.val[1, 2], 1)  # % variance for PC1
pc2_var <- round(eig.val[2, 2], 1)  # % variance for PC2


# Define custom colors
colors <- c('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728',
            '#9467bd', '#8c564b', '#e377c2', '#7f7f7f',
            '#bcbd22', '#17becf')

# PCA 2D plot (base)
ind.p <- fviz_pca_ind(res.pca, axes = c(1, 2),  
                      pointsize = 5, 
                      pointshape = 21, 
                      fill = df$wavelength,        # fill by group
                      col.ind = df$wavelength,     # color points by group
                      geom.ind = "point", 
                      addEllipses = TRUE, 
                      ellipse.level = 0.95,
                      ellipse.type = "confidence",    ##confidence   ###t
                      palette = colors,            # custom palette
                      label = "none", 
                      mean.point = FALSE,
                      repel = TRUE, 
                      labelsize = 7)
# Beautify (with % variance in axis labels) + right legend
ind.p <- ggpubr::ggpar(
  ind.p,
  xlab = paste0("PC1 (", pc1_var, "%)"),
  ylab = paste0("PC2 (", pc2_var, "%)"),
  legend = "right",
  ggtheme = theme_classic()
)

# ---- Make axis, axis numbers, and labels bold + increase size ----
ind.p <- ind.p +
  theme(
    axis.title.x = element_text(face = "plain", size = 22, colour = "black"),
    axis.title.y = element_text(face = "plain", size = 22, colour = "black"),
    axis.text.x  = element_text(face = "plain", size = 22, colour = "black"),
    axis.text.y  = element_text(face = "plain", size = 22, colour = "black"),
    axis.line    = element_line(linewidth = 0.5, colour = "black"),
    
    legend.title = element_text(size = 18, face = "plain"),
    legend.text  = element_text(size = 16),
    legend.key.size = unit(1.0, "cm")
  ) +
  guides(
    colour = "none",                       # remove colour legend
    fill   = guide_legend(title = "Samples")  # rename legend title
  ) + coord_cartesian(ylim = c(-15, 15), xlim = c(-10, 12))

pA <- ind.p + theme(
  plot.title = element_blank(),
  plot.subtitle = element_blank()
)

pA <- pA +
  annotate("text", x =  8, y =  12, label = "Q1", size = 8) +  # upper right
  annotate("text", x = -8, y =  12, label = "Q2", size = 8) +  # upper left
  annotate("text", x = -8, y = -12, label = "Q3", size = 8) +  # lower left
  annotate("text", x =  8, y = -12, label = "Q4", size = 8)
pA
                             ###biplot
# Custom colors
colors <- c('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728',
            '#9467bd', '#8c564b', '#e377c2', '#7f7f7f',
            '#bcbd22', '#17becf')

# Get contributions of variables to PC1 and PC2
var_contrib <- get_pca_var(res.pca)$contrib[,1] + get_pca_var(res.pca)$contrib[,2]

#contrib <- as.data.frame(var_contrib)   ##modified 9th Sep
#write.xlsx(contrib, "contrib_FTIR_ex1.xlsx")

# Select top 10 variables by contribution
top_vars <- names(sort(var_contrib, decreasing = TRUE))[1:15]  ##modified 3rd sep 
top_vars

# Base biplot with only top 10 variable arrows
p <- fviz_pca_biplot(
  res.pca,
  geom.ind    = "point",
  fill.ind    = df$wavelength,
  col.ind     = "black",          # keep point border black
  pointshape  = 21,
  pointsize   = 3.5,
  palette     = colors,
  addEllipses = FALSE,
  col.var     = "black",
  repel       = TRUE,
  label       = "none",
  mean.point  = FALSE,
  select.var  = list(name = top_vars)
)

# ---- Apply same theme & legend settings as score plot ----
p <- ggpubr::ggpar(
  p,
  legend  = "right",
  ggtheme = theme_classic()
)

p <- p +
  theme(
    axis.title.x = element_text(face = "plain", size = 22, colour = "black"),
    axis.title.y = element_text(face = "plain", size = 22, colour = "black"),
    axis.text.x  = element_text(face = "plain", size = 22, colour = "black"),
    axis.text.y  = element_text(face = "plain", size = 22, colour = "black"),
    axis.line    = element_line(linewidth = 0.5, colour = "black"),
    
    legend.title = element_text(size = 18, face = "plain"),
    legend.text  = element_text(size = 16),
    legend.key.size = unit(1.0, "cm")
  ) +
  guides(
    colour = "none",                      #remove colour legend
    fill   = guide_legend(
      title = "Samples",
      override.aes = list(size = 6)       # bigger legend points
    )
  )

# Extract coordinates for those top 10 variables
# Extract coordinates for chosen variables
var_coords <- as.data.frame(res.pca$var$coord[top_vars, 1:2]) * 5
var_coords$var <- rownames(var_coords)

p_final <- p +
  geom_label_repel(
    data = var_coords,
    aes(x = Dim.1, y = Dim.2, label = var),
    fill = "white",
    color = "black",
    size = 6,
    fontface = "plain",
    box.padding = 1.5,
    point.padding = 0.8,
    max.overlaps = Inf,
    nudge_x = 3,
    nudge_y = 3,
    segment.color = "black",
    segment.linetype = "dotted",
    segment.size = 0.6
  ) +
  labs(
    x = "PC1 (42.4%)",
    y = "PC2 (15.8%)"
  ) +
  coord_cartesian(ylim = c(-15, 15), xlim = c(-10, 12))


##+coord_cartesian(xlim = c(-50, 100), ylim = c(-50, 100))
##top contributing variables
var_contrib <- get_pca_var(res.pca)$contrib[,1] + get_pca_var(res.pca)$contrib[,2]
top_combined <- sort(var_contrib, decreasing = TRUE)[1:10]
top_combined
#
pB <- p_final + theme(
  plot.title = element_blank(),
  plot.subtitle = element_blank()
)
pB

#############################################  Figure 5 C #################################################
df <- read_excel("~/Downloads/paper_spectrscopy_nov2025/combination_dend.xlsx") #in mac
# Remove the species column (since clustering is unsupervised)
data <- df[, -1]

# Scale the data (important so that variables with larger ranges don’t dominate)
scaled <- scale(data)
# Compute Euclidean distance between samples
dist_mat <- dist(scaled, method = "euclidean")
# Hierarchical clustering with complete linkage
hc <- hclust(dist_mat, method = "complete")       ###
# Basic dendrogram
plot(hc, main = "Cluster Dendrogram", xlab = "", sub = "", cex = 0.7)
# Cut dendrogram into 3 clusters
clusters <- cutree(hc, k = 3)

# Add rectangles to show clusters
rect.hclust(hc, k = 3, border = 2:4)
table(clusters, df$wavelength)


library(dendextend)
# Custom color palette (same as PCA)
colors <- c('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728',
            '#9467bd', '#8c564b', '#e377c2', '#7f7f7f',
            '#bcbd22', '#17becf')
# Custom color palette (same as PCA)
#colors <- c('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728',
#'#9467bd', '#8c564b', '#e377c2', '#7f7f7f',
#'#bcbd22', #'#17becf')
dend <- as.dendrogram(hc)
labels(dend) <- df$wavelength   # <-- add your row names here
dend_colored <- color_branches(dend, k = 9, col = colors[1:9])

# Make lines thick, labels black
dend_colored <- dend_colored %>%
  set("branches_lwd", 1.5) %>%   # thicker lines
  set("labels_col", "black") %>% 
  set("labels_cex", 1.4)       # slightly bigger

# labels + axis numbers
par(font = 1, cex.axis = 1.8, font.axis = 1, bg= NA, mar = c(5, 0, 2, 3))  # font=2 → bold, font=1 (plain)

# Plot horizontally
plot(dend_colored, horiz = TRUE)
labs <- labels(dend_colored)
# Indices of labels
i1 <- which(labs == "CT1")
i2 <- which(labs == "THD3")

rect(
  xleft   = 0,
  xright  = max(hc$height) * 1.02,
  ybottom = min(i1, i2) - 0.5,
  ytop    = max(i1, i2) + 0.5,
  border  = "darkgreen",
  lwd     = 2,
  lty     = 2          # dotted
)

# Center position for text
x_text1 <- max(hc$height) * 0.85
y_text1 <- mean(c(i1, i2)) - 6.5

text(
  x = x_text1,
  y = y_text1,
  labels = "CT1 – THD3",
  col = "darkgreen",
  cex = 1.7,
  font = 1  
)


j1 <- which(labs == "THE1")
j2 <- which(labs == "THH3")

rect(
  xleft   = 0,
  xright  = max(hc$height) * 1.02,
  ybottom = min(j1, j2) - 0.5,
  ytop    = max(j1, j2) + 0.5,
  border  = "maroon",
  lwd     = 2,
  lty     = 2
)
x_text2 <- max(hc$height) * 0.85
y_text2 <- mean(c(j1, j2))+ 5.0
text(
  x = x_text2,
  y = y_text2,
  labels = "THE1 – THH3",
  col = "maroon",
  cex = 1.7,
  font = 1
)

pC <- recordPlot()
replayPlot(pC)
#############################Figure 5 export#####
#####cowplot
library(cowplot)
#combined <- plot_grid(
 # pA, pB,
  #pC,
 # ncol = 2
  #align = "hv"
#)

top_row <- plot_grid(
  pA, pB,
  ncol = 2
)

bottom_row <- plot_grid(
  NULL, pC, NULL,
  ncol = 3,
  rel_widths = c(1, 2, 1)  # centers pC horizontally
)
combined <- plot_grid(
  top_row,
  bottom_row,
  ncol = 1
  #rel_heights = c(0.8, 1.2) # ⬅ increase pC row height
  )

final_fig <- ggdraw(combined) +
  draw_plot_label(
    label = c("A", "B", "C"),
    x = c(0.02, 0.52, 0.02),
    y = c(0.98, 0.98, 0.48),
    hjust = 0,
    vjust = 1,
    fontfamily = "Arial",
    fontface   = "bold",
    size       = 34
  )
#x = c(0.02, 0.52, 0.02, 0.52),
#y = c(0.98, 0.98, 0.48, 0.48),
ggsave(
  "Figure_6.jpg",
  final_fig,
  width = 33,
  height = 35,
  units = "cm",
  dpi = 300,
  quality = 100
)
ggsave(
  "Figure_5.pdf",
  final_fig,
  width = 33,
  height = 35,
  units = "cm"
)
font_import()   # once only
loadfonts()


###most contributing variables figure of supplmentary
##input the data of combination only

# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)

library(factoextra)
library(ggplot2)

p <- fviz_contrib(res.pca,
                  choice = "var",
                  axes = 1:2,
                  top = 50) +
  
  # Add x-axis label
  labs(title = NULL,
       x = "Most contributing independent variables") +
  
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 18, colour = "black", family = "Arial"),
    axis.title.y = element_text(size = 18, colour = "black", family = "Arial"),
    axis.text.x  = element_text(size = 14, colour = "black",
                                family = "Arial", angle = 45, hjust = 1),
    axis.text.y  = element_text(size = 15, colour = "black", family = "Arial"),
    axis.line = element_line(linewidth = 0.5),
    plot.title = element_blank()
  )

p
ggsave(
  filename = "PCA_variable_contribution1.jpg",
  plot = p,
  width = 25,
  height = 20,
  units = "cm",
  dpi = 300,
  quality = 100
)


