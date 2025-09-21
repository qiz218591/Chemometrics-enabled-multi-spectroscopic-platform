getwd()
setwd("D:/Downloads_Sep_2025/Aug 2025_aspaper1")  ##set your file path
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

# -----------------------------
# Load dataset
# -----------------------------
df <- read_excel("C:/Users/Lenovo/Downloads/Aug 2025_aspaper1/FTIR_PCA3D.xlsx")
df <- read_excel("C:/Users/Lenovo/Downloads/Aug 2025_aspaper1/CD_PCA.xlsx")
df <- read_excel("C:/Users/Lenovo/Downloads/Aug 2025_aspaper1/UVspec_PCA.xlsx")
df <- read_excel("C:/Users/Lenovo/Downloads/Aug 2025_aspaper1/FLR_260_PCA.xlsx")
df <- read_excel("C:/Users/Lenovo/Downloads/Aug 2025_aspaper1/FLR_275_PCA.xlsx")
df <- read_excel("C:/Users/Lenovo/Downloads/Aug 2025_aspaper1/FLR_280_PCA.xlsx")
df <- read_excel("C:/Users/Lenovo/Downloads/Aug 2025_aspaper1/FLR_ex_PCA.xlsx")
df <- read_excel("C:/Users/Lenovo/Downloads/Aug 2025_aspaper1/combination.xlsx")
df <- read_excel("C:/Users/Lenovo/Downloads/Aug 2025_aspaper1/In_FLR_260_275_280.xlsx")
df <- read_excel("C:/Users/Lenovo/Downloads/Aug 2025_aspaper1/FTIR_fullspectra.xlsx")

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


# Coordinates
head(var$coord)
# Cos2: quality on the factore map
head(var$cos2)
# Contributions to the principal components
head(var$contrib)
# Coordinates of variables
head(var$coord, 4)
#to plot variables 
fviz_pca_var(res.pca, col.var = "black")

head(var$cos2, 4)


library("corrplot")
corrplot(var$cos2, is.corr=FALSE)

# Total cos2 of variables on Dim.1 and Dim.2
fviz_cos2(res.pca, choice = "var", axes = 1:2)

# Color by cos2 values: quality on the factor map
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)

# Change the transparency by cos2 values
fviz_pca_var(res.pca, alpha.var = "cos2")

head(var$contrib, 4)


library("corrplot")
corrplot(var$contrib, is.corr=FALSE)    

# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)


fviz_contrib(res.pca, choice = "var", axes = 1:2, top = 10)


fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)
##2d plca plot for FTIR dataset
ind.p <- fviz_pca_ind(res.pca, axes = c(1, 2), pointsize = 3, pointshape = 21, fill = "lightblue",
                      labelsize = 5, repel = TRUE, geom.ind = "point", addEllipses = TRUE, ellipse.level = 0.95,
                      col.ind = df$wavelength, ellipse.type = "confidence", palette = "jco", label = "none", mean.point = FALSE) # hide individual labels   
ggpubr::ggpar(ind.p,
              title = "Principal Component Analysis",
              subtitle = "CD data set",
              caption = "Source: factoextra",
              xlab = "PC1", ylab = "PC2",
              legend.title = "Samples", legend.position = "top",
              ggtheme = theme_classic(), palette = "jco"
)

############made this 2d plot as in publishing format
# Calculate variance explained
eig.val <- get_eigenvalue(res.pca)
pc1_var <- round(eig.val[1, 2], 1)  # % variance for PC1
pc2_var <- round(eig.val[2, 2], 1)  # % variance for PC2

# Define custom colors
colors <- c('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728',
            '#9467bd', '#8c564b', '#e377c2', '#7f7f7f',
            '#bcbd22', '#17becf')

# PCA 2D plot with custom colors
ind.p <- fviz_pca_ind(res.pca, axes = c(1, 2),
                      pointsize = 3, 
                      pointshape = 21, 
                      fill = df$wavelength,        # fill by group
                      col.ind = df$wavelength,     # color points by group
                      geom.ind = "point", 
                      addEllipses = TRUE, 
                      ellipse.level = 0.95,
                      ellipse.type = "confidence",
                      palette = colors,            # custom palette
                      label = "none", 
                      mean.point = FALSE,
                      repel = TRUE, 
                      labelsize = 5)

# Beautify (with % variance in axis labels)
ggpubr::ggpar(ind.p,
              title = "Principal Component Analysis",
              subtitle = "UV Spec data set",
              caption = "Source: factoextra",
              xlab = paste0("PC1 (", pc1_var, "%)"),
              ylab = paste0("PC2 (", pc2_var, "%)"),
              legend.title = "Samples", 
              legend.position = "top",
              ggtheme = theme_classic())





##########

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
                      ellipse.type = "confidence",
                      palette = colors,            # custom palette
                      label = "none", 
                      mean.point = FALSE,
                      repel = TRUE, 
                      labelsize = 5)

# ---- Add group names inside ellipses ----
# Extract PCA coordinates of individuals
ind_coords <- as.data.frame(res.pca$ind$coord[, 1:2]) 
ind_coords$Group <- df$wavelength

# Compute group centroids
centroids <- ind_coords %>%
  group_by(Group) %>%
  summarise(PC1 = mean(Dim.1), PC2 = mean(Dim.2))

# Overlay labels at ellipse centers
ind.p <- ind.p +
  geom_text(data = centroids, 
            aes(x = PC1, y = PC2, label = Group),
            fontface = "bold", size = 5, color = "black")

# Beautify (with % variance in axis labels)
ggpubr::ggpar(ind.p,
              title = "Principal Component Analysis",
              subtitle = "CD data set",
              caption = "Source: factoextra",
              xlab = paste0("PC1 (", pc1_var, "%)"),
              ylab = paste0("PC2 (", pc2_var, "%)"),
              legend.title = "Samples", 
              legend.position = "top",
              ggtheme = theme_classic())

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

# ---- Add group names inside ellipses ----
ind_coords <- as.data.frame(res.pca$ind$coord[, 1:2]) 
ind_coords$Group <- df$wavelength

# Compute group centroids
centroids <- ind_coords %>%
  group_by(Group) %>%
  summarise(PC1 = mean(Dim.1), PC2 = mean(Dim.2))  

# Overlay labels at ellipse centers
ind.p <- ind.p +
  geom_text(data = centroids, 
            aes(x = PC1, y = PC2, label = Group),       
            fontface = "bold", size = 5, color = "black")

# Beautify (with % variance in axis labels) + REMOVE LEGEND
ind.p<- ggpubr::ggpar(ind.p,
              xlab = paste0("PC1 (", pc1_var, "%)"),
              ylab = paste0("PC2 (", pc2_var, "%)"),     
              legend = "none",   # <-- removes legend
              ggtheme = theme_classic())

# ---- Make axis, axis numbers, and labels bold + increase size ----
ind.p <- ind.p + theme(
  axis.title.x = element_text(face = "bold", size = 20, colour = "black"),
  axis.title.y = element_text(face = "bold", size = 20, colour = "black"),
  axis.text.x  = element_text(face = "bold", size = 20, colour = "black"),
  axis.text.y  = element_text(face = "bold", size = 20, colour = "black"),
  axis.line    = element_line(linewidth = 0.5, colour = "black")  # thicker, bold lines
)

ind.p

# Variables on dimensions 2 and 3
#fviz_pca_var(res.pca, axes = c(1,2), arrowsize = 1, labelsize = 5, 
#             repel = TRUE)


##combination of PC1-PC3
# Variance explained
pc1_var <- round(eig.val[1, 2], 1)
pc3_var <- round(eig.val[3, 2], 1)

# PCA Plot PC1 vs PC3
ind.p13 <- fviz_pca_ind(res.pca, axes = c(1, 3),
                        pointsize = 3, 
                        pointshape = 21, 
                        fill = df$wavelength,  
                        col.ind = df$wavelength, 
                        geom.ind = "point", 
                        addEllipses = TRUE, 
                        ellipse.level = 0.95,
                        ellipse.type = "confidence",
                        palette = colors,
                        label = "none", 
                        mean.point = FALSE,
                        repel = TRUE, 
                        labelsize = 5)

# Coordinates for centroids
ind_coords <- as.data.frame(res.pca$ind$coord[, c(1,3)])
ind_coords$Group <- df$wavelength

centroids <- ind_coords %>%
  group_by(Group) %>%
  summarise(PC1 = mean(Dim.1), PC3 = mean(Dim.3))

# Add group labels
ind.p13 <- ind.p13 +
  geom_text(data = centroids, aes(x = PC1, y = PC3, label = Group),
            fontface = "bold", size = 5, color = "black")

# Beautify
ind.p13 <- ggpubr::ggpar(ind.p13,
                         xlab = paste0("PC1 (", pc1_var, "%)"),
                         ylab = paste0("PC3 (", pc3_var, "%)"),
                         legend = "none", ggtheme = theme_classic())

ind.p13 <- ind.p13 + theme(
  axis.title.x = element_text(face = "bold", size = 20, colour = "black"),
  axis.title.y = element_text(face = "bold", size = 20, colour = "black"),
  axis.text.x  = element_text(face = "bold", size = 20, colour = "black"),
  axis.text.y  = element_text(face = "bold", size = 20, colour = "black"),
  axis.line    = element_line(linewidth = 0.5, colour = "black"))


ind.p13

######combination of PC2 and PC3

# Variance explained
pc2_var <- round(eig.val[2, 2], 1)
pc3_var <- round(eig.val[3, 2], 1)

# PCA Plot PC2 vs PC3
ind.p23 <- fviz_pca_ind(res.pca, axes = c(2, 3),
                        pointsize = 3, 
                        pointshape = 21, 
                        fill = df$wavelength,  
                        col.ind = df$wavelength, 
                        geom.ind = "point", 
                        addEllipses = TRUE, 
                        ellipse.level = 0.95,
                        ellipse.type = "confidence",
                        palette = colors,
                        label = "none", 
                        mean.point = FALSE,
                        repel = TRUE, 
                        labelsize = 5)

# Coordinates for centroids
ind_coords <- as.data.frame(res.pca$ind$coord[, c(2,3)])
ind_coords$Group <- df$wavelength

centroids <- ind_coords %>%
  group_by(Group) %>%
  summarise(PC2 = mean(Dim.2), PC3 = mean(Dim.3))

# Add group labels
ind.p23 <- ind.p23 +
  geom_text(data = centroids, aes(x = PC2, y = PC3, label = Group),
            fontface = "bold", size = 5, color = "black")

# Beautify
ind.p23 <- ggpubr::ggpar(ind.p23,
                         xlab = paste0("PC2 (", pc2_var, "%)"),
                         ylab = paste0("PC3 (", pc3_var, "%)"),
                         legend = "none", ggtheme = theme_classic())

ind.p23 <- ind.p23 + theme(
  axis.title.x = element_text(face = "bold", size = 20, colour = "black"),
  axis.title.y = element_text(face = "bold", size = 20, colour = "black"),
  axis.text.x  = element_text(face = "bold", size = 20, colour = "black"),
  axis.text.y  = element_text(face = "bold", size = 20, colour = "black"),
  axis.line    = element_line(linewidth = 0.5, colour = "black"))


ind.p23



##3D##plot

# Extract PCA scores (coordinates of the points in the new principal components space)
X_pca <- res.pca$ind$coord

# Explained variance and Eigenvalues
explained_var <- res.pca$eig[, 2]
eigenvalues <- res.pca$eig[, 1]

cat("\nExplained Variance Ratio (per PC):\n")
print(explained_var)

cat("\nEigenvalues (per PC):\n")
print(eigenvalues)

# -----------------------------
# 3D PCA Score Plot with plotly
# -----------------------------
# Define colors
unique_groups <- unique(groups)
colors <- c('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2', '#7f7f7f', '#bcbd22', '#17becf')

# Create scatter plot for each group using plotly
fig <- plot_ly()

for (i in 1:length(unique_groups)) {
  g <- unique_groups[i]
  idx <- which(groups == g)
  
  # 3D scatter plot with increased marker size
  fig <- fig %>% add_trace(
    type = 'scatter3d',
    mode = 'markers',
    x = X_pca[idx, 1],
    y = X_pca[idx, 2],
    z = X_pca[idx, 3],
    marker = list(size = 10, color = colors[i %% length(colors)], line = list(width = 0.5, color = 'black')),
    name = g
  )
}

# Set axis labels and title
fig <- fig %>% layout(
  scene = list(
    xaxis = list(title = paste("PC1 (", round(explained_var[1], 1), "%)", sep = "")),
    yaxis = list(title = paste("PC2 (", round(explained_var[2], 1), "%)", sep = "")),
    zaxis = list(title = paste("PC3 (", round(explained_var[3], 1), "%)", sep = ""))
  ),
  title = "3D PCA Scores",
  legend = list(title = list(text = 'Groups'))
)

fig

################################################
##PERFECT 3D PCA
###########################################################################################
#######################################################################################################
library(plotly)

# PCA scores
X_pca <- res.pca$ind$coord

# Explained variance
explained_var <- res.pca$eig[, 2]

# Unique groups
unique_groups <- unique(groups)
colors <- c('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728',
            '#9467bd', '#8c564b', '#e377c2', '#7f7f7f',
            '#bcbd22', '#17becf')

# Initialize plotly object
fig <- plot_ly()

for (i in 1:length(unique_groups)) {
  g <- unique_groups[i]
  idx <- which(groups == g)
  Xg <- X_pca[idx, 1:3]
  
  # Scatter points
  fig <- fig %>% add_trace(
    type = 'scatter3d',
    mode = 'markers',
    x = Xg[,1], y = Xg[,2], z = Xg[,3],
    marker = list(size = 8, color = colors[i], line = list(width = 0.5, color = 'black')),
    name = g
  )
  
  # --- Compute Ellipsoid ---
  cov_mat <- cov(Xg)
  mean_vec <- colMeans(Xg)
  eig <- eigen(cov_mat)
  
  # scale eigenvalues for 95% CI
  chi2_val <- sqrt(qchisq(0.95, df = 3))
  radii <- chi2_val * sqrt(eig$values)
  
  # parametric sphere
  u <- seq(0, 2*pi, length = 40)
  v <- seq(0, pi, length = 20)
  x <- outer(cos(u), sin(v))
  y <- outer(sin(u), sin(v))
  z <- outer(rep(1, length(u)), cos(v))
  
  # sphere → ellipsoid: scale by radii and rotate
  sphere <- rbind(as.vector(x), as.vector(y), as.vector(z))
  ellipsoid <- eig$vectors %*% diag(radii) %*% sphere
  
  # reshape back
  x_e <- matrix(ellipsoid[1,], nrow=length(u)) + mean_vec[1]
  y_e <- matrix(ellipsoid[2,], nrow=length(u)) + mean_vec[2]
  z_e <- matrix(ellipsoid[3,], nrow=length(u)) + mean_vec[3]
  
  # Add as transparent surface with same group color
  fig <- fig %>% add_surface(
    x = x_e, y = y_e, z = z_e,
    showscale = FALSE,
    opacity = 0.25,
    surfacecolor = matrix(rep(1, length(x_e)), nrow=nrow(x_e)), # dummy
    colorscale = list(c(0,1), c(colors[i], colors[i])),        # force group color
    name = paste(g, "ellipsoid"),
    showlegend = FALSE
  )
}

# Layout
fig <- fig %>% layout(
  scene = list(
    xaxis = list(title = paste0("PC1 (", round(explained_var[1],1), "%)")),
    yaxis = list(title = paste0("PC2 (", round(explained_var[2],1), "%)")),
    zaxis = list(title = paste0("PC3 (", round(explained_var[3],1), "%)"))
  ),
  title = "3D PCA Scores with 95% Ellipsoids",
  legend = list(title = list(text = 'Groups'))
)

fig


#############perfect 3d plot for publications##########
## PERFECT 3D PCA with bold axes, white labels, and no legend
###########################################################################################

# PCA scores
X_pca <- res.pca$ind$coord

# Explained variance
explained_var <- res.pca$eig[, 2]

# Unique groups
unique_groups <- unique(groups)
colors <- c('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728',
            '#9467bd', '#8c564b', '#e377c2', '#7f7f7f',
            '#bcbd22', '#17becf')

# Initialize plotly object
fig <- plot_ly(showlegend = FALSE)   # remove side legend

for (i in 1:length(unique_groups)) {
  g <- unique_groups[i]
  idx <- which(groups == g)
  Xg <- X_pca[idx, 1:3]
  
  # Scatter points
  fig <- fig %>% add_trace(
    type = 'scatter3d',
    mode = 'markers',
    x = Xg[,1], y = Xg[,2], z = Xg[,3],
    marker = list(size = 8, color = colors[i], 
                  line = list(width = 0.5, color = 'black'))
  )
  
  # --- Compute Ellipsoid ---
  cov_mat <- cov(Xg)
  mean_vec <- colMeans(Xg)
  eig <- eigen(cov_mat)
  
  chi2_val <- sqrt(qchisq(0.95, df = 3))
  radii <- pmax(chi2_val * sqrt(pmax(eig$values, 1e-6)), 0.5)
  
  u <- seq(0, 2*pi, length = 60)
  v <- seq(0, pi, length = 30)
  x <- outer(cos(u), sin(v))
  y <- outer(sin(u), sin(v))
  z <- outer(rep(1, length(u)), cos(v))
  
  sphere <- rbind(as.vector(x), as.vector(y), as.vector(z))
  ellipsoid <- eig$vectors %*% diag(radii) %*% sphere
  
  x_e <- matrix(ellipsoid[1,], nrow=length(u)) + mean_vec[1]
  y_e <- matrix(ellipsoid[2,], nrow=length(u)) + mean_vec[2]
  z_e <- matrix(ellipsoid[3,], nrow=length(u)) + mean_vec[3]
  
  # Add ellipsoid
  fig <- fig %>% add_surface(
    x = x_e, y = y_e, z = z_e,
    showscale = FALSE,
    opacity = 0.35,
    surfacecolor = matrix(rep(1, length(x_e)), nrow=nrow(x_e)),
    colorscale = list(c(0,1), c(colors[i], colors[i]))
  )
  
  # Add black group label
  fig <- fig %>% add_trace(
    type = "scatter3d",
    mode = "text",
    x = mean_vec[1],
    y = mean_vec[2],
    z = mean_vec[3],
    text = g,
    textfont = list(size = 18, color = "black", family="Arial Black"),
    showlegend = FALSE
  )
}

# Layout with bold axis lines + bold ticks
fig <- fig %>% layout(
  scene = list(
    xaxis = list(
      title = list(text = paste0("PC1 (", round(explained_var[1],1), "%)"),
                   font = list(size = 20, family = "Arial Black", color="black")),
      tickfont = list(size = 16, family = "Arial Black", color="black"),
      showline = TRUE, mirror = TRUE, linewidth = 4, linecolor="black", zeroline=FALSE
    ),
    yaxis = list(
      title = list(text = paste0("PC2 (", round(explained_var[2],1), "%)"),
                   font = list(size = 20, family = "Arial Black", color="black")),
      tickfont = list(size = 16, family = "Arial Black", color="black"),
      showline = TRUE, mirror = TRUE, linewidth = 4, linecolor="black", zeroline=FALSE
    ),
    zaxis = list(
      title = list(text = paste0("PC3 (", round(explained_var[3],1), "%)"),
                   font = list(size = 20, family = "Arial Black", color="black")),
      tickfont = list(size = 16, family = "Arial Black", color="black"),
      showline = TRUE, mirror = TRUE, linewidth = 4, linecolor="black", zeroline=FALSE
    )
  ),
  title = list(text="3D PCA Scores with 95% Ellipsoids", 
               font=list(size=24, family="Arial Black", color="black"))
)

fig


###Biplot#####################################################

# Base biplot
p <- fviz_pca_biplot(res.pca,
                     geom.ind = "point",
                     fill.ind = df$wavelength, col.ind = "black",
                     pointshape = 21, pointsize = 2,
                     palette = "jco",
                     addEllipses = TRUE,
                     col.var = "contrib",
                     gradient.cols = "RdYlBu",
                     legend.title = list(fill = "Species", color = "Contrib"),
                     repel = TRUE,
                     mean.point = FALSE,
                     label = "none"  # hide default labels
) + theme_classic()

# Extract variable coordinates
var_coords <- as.data.frame(res.pca$var$coord[,1:2]) * 5  # scaled for plotting
var_coords$var <- rownames(var_coords)


# Add labels in boxes
p + geom_label(data = var_coords, 
               aes(x = Dim.1, y = Dim.2, label = var), 
               fill = "white", color = "black", 
               size = 4, fontface = "bold")

#####################################################
###only first 6 vairbales then how to do it..############################
##################################################
library(factoextra)

# Get contributions of variables to PC1 and PC2
var_contrib <- get_pca_var(res.pca)$contrib[,1] + get_pca_var(res.pca)$contrib[,2]

# Select top 6 variables by contribution
top_vars <- names(sort(var_contrib, decreasing = TRUE))[1:10]  ##modified 3rd sep

# Base biplot with only top 6 variable arrows
p <- fviz_pca_biplot(res.pca,
                     geom.ind = "point",
                     fill.ind = df$wavelength, col.ind = "black",
                     pointshape = 21, pointsize = 2,
                     palette = "jco",
                     addEllipses = FALSE,
                     col.var = "contrib",
                     gradient.cols = "RdYlBu",
                     legend.title = list(fill = "Samples", color = "Contrib"),
                     repel = TRUE,
                     label = "none",
                     mean.point = FALSE,
                     select.var = list(name = top_vars)  # keep only top 6 variables
) + theme_classic()

# Extract coordinates for those top 6 variables
var_coords <- as.data.frame(res.pca$var$coord[top_vars, 1:2]) * 5
var_coords$var <- rownames(var_coords)

# Add labels in boxes
p + geom_label(data = var_coords, 
               aes(x = Dim.1, y = Dim.2, label = var), 
               fill = "white", color = "black", 
               size = 4, fontface = "bold")

#################PCA biplo final for publications ###for all spectrosocpy except FTIR

# Custom colors
colors <- c('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728',
            '#9467bd', '#8c564b', '#e377c2', '#7f7f7f',
            '#bcbd22', '#17becf')

# Get contributions of variables to PC1 and PC2
var_contrib <- get_pca_var(res.pca)$contrib[,1] + get_pca_var(res.pca)$contrib[,2]

#contrib <- as.data.frame(var_contrib)   ##modified 9th Sep
#write.xlsx(contrib, "contrib_FLR_ex.xlsx")

# Select top 10 variables by contribution
top_vars <- names(sort(var_contrib, decreasing = TRUE))[1:10]  ##modified 3rd sep 
top_vars

# Base biplot with only top 10 variable arrows
p <- fviz_pca_biplot(res.pca,
                     geom.ind = "point",
                     fill.ind = df$wavelength, col.ind = "black",
                     pointshape = 21, pointsize = 2.5,
                     palette = colors,              # <--- use custom palette here
                     addEllipses = FALSE,
                     col.var = "black",
                     legend.title = list(fill = "Samples"),
                     repel = TRUE,
                     label = "none",
                     mean.point = FALSE,
                     select.var = list(name = top_vars)  # keep only top 10 variables
) + theme_classic()+
  guides(
    fill = guide_legend(override.aes = list(size = 5))  # bigger legend points
  )

# Extract coordinates for those top 6 variables
var_coords <- as.data.frame(res.pca$var$coord[top_vars, 1:2]) * 5
var_coords$var <- rownames(var_coords)

# Add labels in boxes
p + geom_label(data = var_coords, 
               aes(x = Dim.1, y = Dim.2, label = var), 
               fill = "white", color = "black", 
               size = 5, fontface = "bold") +
  labs(
    x = "PC1 (75.9%)",   # rename x-axis   ##change it in case with each datasets of spectroscopy.
    y = "PC2 (10%)"    # rename y-axis ##change it in case with each datasets of spectroscopy
  ) +
  theme(
    axis.title.x = element_text(face = "bold", size = 20, color = "black"),  # axis labels bold, bigger
    axis.title.y = element_text(face = "bold", size = 20, color = "black"),
    axis.text.x  = element_text(face = "bold", size = 20, color = "black"),  # axis numbers bold
    axis.text.y  = element_text(face = "bold", size = 20, color = "black"),
    axis.line    = element_line(linewidth = 0.5, colour = "black")  # thicker, bold axes
  ) + 
  coord_cartesian(xlim = c(-10, 10))


##+coord_cartesian(xlim = c(-50, 100), ylim = c(-50, 100))
##top contributing variables
var_contrib <- get_pca_var(res.pca)$contrib[,1] + get_pca_var(res.pca)$contrib[,2]
top_combined <- sort(var_contrib, decreasing = TRUE)[1:10]
top_combined

###PCA biplot for publications for FTIR for choosen variables#############
###########################################################################
chosen_vars <- c("1408.7",          ##FTIR choosen 10 variables 
                 "1443.4",
                 "1507.1",
                 "1539.4",
                 "1559.1",
                 "1586.1",
                 "1599.2",
                 "1699.9",
                 "1726",
                 "1787.7"
)
p <- fviz_pca_biplot(res.pca,
                     geom.ind = "point",
                     fill.ind = df$wavelength, col.ind = "black",
                     pointshape = 21, pointsize = 2.5,
                     palette = colors,
                     addEllipses = FALSE,
                     col.var = "black",
                     legend.title = list(fill = "Samples"),
                     repel = TRUE,
                     label = "none",
                     mean.point = FALSE,
                     select.var = list(name = chosen_vars)  # <-- use your variables
) + theme_classic() +
  guides(
    fill = guide_legend(override.aes = list(size = 5))  # bigger legend points
  )

# Extract coordinates for chosen variables
var_coords <- as.data.frame(res.pca$var$coord[chosen_vars, 1:2]) * 5
var_coords$var <- rownames(var_coords)

# Add labels in boxes
p_final <- p + geom_label(data = var_coords, 
                          aes(x = Dim.1, y = Dim.2, label = var), 
                          fill = "white", color = "black", 
                          size = 5, fontface = "bold") +
  labs(
    x = "PC1 (93.3%)",   
    y = "PC2 (5.5%)"
  ) +
  theme(
    axis.title.x = element_text(face = "bold", size = 20, colour = "black"),
    axis.title.y = element_text(face = "bold", size = 20,colour = "black"),
    axis.text.x  = element_text(face = "bold", size = 20,colour = "black"),
    axis.text.y  = element_text(face = "bold", size = 20,colour = "black"),
    axis.line    = element_line(linewidth = 0.5, colour = "black")
  )+
  coord_cartesian(xlim = c(-50, 100))

p_final


##########combination
#################PCA biplo final for publications
# Custom colors
colors <- c('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728',
            '#9467bd', '#8c564b', '#e377c2', '#7f7f7f',
            '#bcbd22', '#17becf')

# Get contributions of variables to PC1 and PC2
var_contrib <- get_pca_var(res.pca)$contrib[,1] + get_pca_var(res.pca)$contrib[,2]

#contrib <- as.data.frame(var_contrib)   ##modified 9th Sep
#write.xlsx(contrib, "contrib_FLR_ex.xlsx")

# Select top 10 variables by contribution
top_vars <- names(sort(var_contrib, decreasing = TRUE))[1:20]  ##modified 8th sep 
top_vars

# Base biplot with only top 10 variable arrows
p <- fviz_pca_biplot(res.pca,
                     geom.ind = "point",
                     fill.ind = df$wavelength, col.ind = "black",
                     pointshape = 21, pointsize = 2.5,
                     palette = colors,              # <--- use custom palette here
                     addEllipses = FALSE,
                     col.var = "black",
                     legend.title = list(fill = "Samples"),
                     repel = TRUE,
                     label = "none",
                     mean.point = FALSE,
                     select.var = list(name = top_vars)  # keep only top 20 variables
) + theme_classic()+
  guides(
    fill = guide_legend(override.aes = list(size = 5))  # bigger legend points
  )

# Extract coordinates for those top 6 variables
var_coords <- as.data.frame(res.pca$var$coord[top_vars, 1:2]) * 5
var_coords$var <- rownames(var_coords)

# Add labels in boxes
p + geom_label(data = var_coords, 
               aes(x = Dim.1, y = Dim.2, label = var), 
               fill = "white", color = "black", 
               size = 5, fontface = "bold") +
  labs(
    x = "PC1 (38.5%)",   # rename x-axis   ##change it in case with each datasets of spectroscopy.
    y = "PC2 (12.8%)"    # rename y-axis ##change it in case with each datasets of spectroscopy
  ) +
  theme(
    axis.title.x = element_text(face = "bold", size = 20, color = "black"),  # axis labels bold, bigger
    axis.title.y = element_text(face = "bold", size = 20, color = "black"),
    axis.text.x  = element_text(face = "bold", size = 20, color = "black"),  # axis numbers bold
    axis.text.y  = element_text(face = "bold", size = 20, color = "black"),
    axis.line    = element_line(linewidth = 0.5, colour = "black")  # thicker, bold axes
  )+
  coord_cartesian(ylim = c(-12, 5), xlim = c(-10, 10))


##+coord_cartesian(xlim = c(-50, 100), ylim = c(-50, 100))
##top contributing variables
var_contrib <- get_pca_var(res.pca)$contrib[,1] + get_pca_var(res.pca)$contrib[,2]
top_combined <- sort(var_contrib, decreasing = TRUE)[1:10]
top_combined


############dendogram in r###for the combination of all ##spec

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
dend <- as.dendrogram(hc)
labels(dend) <- df$wavelength   # <-- add your row names here
dend_colored <- color_branches(dend, k = 9, col = colors[1:9])

# Make lines thick, labels black
dend_colored <- dend_colored %>%
  set("branches_lwd", 2) %>%   # thicker lines
  set("labels_col", "black") %>% 
  set("labels_cex", 1.2)       # slightly bigger

# Bold labels + axis numbers
par(font = 2, cex.axis = 2, font.axis = 2)  # font=2 → bold

# Plot horizontally
plot(dend_colored, horiz = TRUE)

# Reset to normal afterwards
par(font = 1)


###heatmap###################################################

# Get PCA variable results
var <- get_pca_var(res.pca)

# Contributions
contrib_df <- data.frame(
  Variable = rownames(var$contrib),
  PC1_Contribution = var$contrib[,1],
  PC2_Contribution = var$contrib[,2],
  Total_Contribution = var$contrib[,1] + var$contrib[,2]
)

# Loadings (coordinates)
loadings_df <- data.frame(
  Variable = rownames(var$coord),
  PC1_Loading = var$coord[,1],
  PC2_Loading = var$coord[,2]
)

# Combine contributions + loadings
df_heat <- merge(contrib_df, loadings_df, by = "Variable")

# Melt to long format
df_melt <- melt(df_heat, id.vars = "Variable")

# Add Type for faceting and coloring
df_melt$Type <- ifelse(grepl("Contribution", df_melt$variable), "Contribution", "Loading")

# Horizontal heatmap with facets
ggplot(df_melt, aes(x = Variable, y = variable, fill = value)) +
  geom_tile(color = "white") +
  facet_wrap(~Type, scales = "free_y") +
  # Set separate gradients for each Type using scale_fill_gradientn
  scale_fill_gradientn(
    colors = c("lightblue", "orange", "red"),
    na.value = "grey"
  ) +
  labs(x = "Variables", y = "Metric", fill = "Value",
       title = "PCA Variable Contributions & Loadings") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(face = "bold", size = 10, angle = 45, hjust = 1),
    axis.text.y = element_text(face = "bold", size = 12),
    strip.text = element_text(face = "bold", size = 14),   # facet labels
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
  )


######################heatmap2####contribution
library(factoextra)
library(ggplot2)
library(reshape2)

# Get PCA variable results
var <- get_pca_var(res.pca)

# Contributions
contrib_df <- data.frame(
  Variable = rownames(var$contrib),
  PC1 = var$contrib[,1],
  PC2 = var$contrib[,2],
  Total = var$contrib[,1] + var$contrib[,2]
)

# All variables horizontal
contrib_df$Variable <- factor(contrib_df$Variable, levels = contrib_df$Variable)

# Melt for heatmap
df_contrib <- melt(contrib_df, id.vars = "Variable")

# Plot Contribution Heatmap
ggplot(df_contrib, aes(x = Variable, y = variable, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(colors = c("lightblue", "orange", "red")) +
  labs(x = "Variables", y = "Contribution Metric", fill = "Contribution (%)",
       title = "PCA Variable Contributions (PC1, PC2, Total)") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(face = "bold", size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(face = "bold", size = 12),
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5))

##########################heatmap3#############loadings
# Loadings (coordinates)
loadings_df <- data.frame(
  Variable = rownames(var$coord),
  PC1 = var$coord[,1],
  PC2 = var$coord[,2]
)

# All variables horizontal
loadings_df$Variable <- factor(loadings_df$Variable, levels = loadings_df$Variable)

# Melt for heatmap
df_loadings <- melt(loadings_df, id.vars = "Variable")

# Plot Loadings Heatmap
ggplot(df_loadings, aes(x = Variable, y = variable, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(x = "Variables", y = "Loadings (Coordinates)", fill = "Value",
       title = "PCA Variable Loadings (PC1 & PC2)") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(face = "bold", size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(face = "bold", size = 12),
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5))



###heatmap of loadings ##Mixing pc1 and pc2 
# Loadings (coordinates)
loadings_df <- data.frame(
  Variable = rownames(var$coord),
  PC1 = var$coord[,1],
  PC2 = var$coord[,2]
)

# Calculate combined loadings (e.g., sum or average of PC1 and PC2)
loadings_df$PC1_PC2 <- rowSums(loadings_df[, c("PC1", "PC2")])  # You could also use rowMeans() if you prefer the average

# All variables horizontal
loadings_df$Variable <- factor(loadings_df$Variable, levels = loadings_df$Variable)

# Melt for heatmap, including the combined PC1 + PC2 column
df_loadings <- melt(loadings_df, id.vars = "Variable")

# Plot Loadings Heatmap for PC1, PC2, and PC1+PC2
ggplot(df_loadings, aes(x = Variable, y = variable, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(x = "Variables", y = "Principal Components", fill = "Value",
       title = "PCA Variable Loadings (PC1, PC2, PC1+PC2)") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(face = "bold", size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(face = "bold", size = 12),
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5)) +
  scale_y_discrete(labels = c("PC1", "PC2", "PC1+PC2"))  # Label the third row as PC1+PC2

