# FINAL SENSITIVITY ANALYSIS FOR THE CHAPTER 4 FUSED DATASET
# Input: combination.xlsx supplied for the fused PCA analysis.
# The dataset is used exactly as supplied: no sample or variable is added,
# excluded, filtered, imputed, or replaced.

# Install once if required:
# install.packages(c("readxl", "FactoMineR", "factoextra", "ggplot2", "openxlsx"))

library(readxl)
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(openxlsx)

getwd()
setwd("/Users/divyaagrawal/Downloads")

# 1. READ THE SAME COMBINED DATASET USED FOR THE REPORTED FUSED PCA
input_file <- "combination.xlsx"
df <- as.data.frame(read_excel(input_file))

sample_group <- factor(
  df[[1]],
  levels = c("CT", "THA", "THB", "THC", "THD",
             "THE", "THF", "THG", "THH"),
  ordered = TRUE
)

X <- as.data.frame(df[, -1], check.names = FALSE)
X[] <- lapply(X, as.numeric)

# Verify the attached dataset structure without changing it.
stopifnot(nrow(X) == 27)
stopifnot(ncol(X) == 50)
stopifnot(!anyNA(X))

# Actual column order in combination.xlsx:
#  1-10  = UV-visible
# 11-20  = FTIR
# 21-30  = intrinsic fluorescence
# 31-40  = extrinsic fluorescence
# 41-50  = far-UV CD
block_sizes <- c(10, 10, 10, 10, 10)
block_names <- c(
  "UV-visible", "FTIR", "Intrinsic fluorescence",
  "Extrinsic fluorescence", "Far-UV CD"
)

# Identifiers are used only for plotting/export and do not modify X.
replicate_number <- ave(seq_along(sample_group), sample_group, FUN = seq_along)
sample_id <- paste0(as.character(sample_group), "_R", replicate_number)
rownames(X) <- sample_id

time_hours <- c(
  CT = 0, THA = 1, THB = 3, THC = 6, THD = 9,
  THE = 15, THF = 18, THG = 21, THH = 24
)

metadata <- data.frame(
  Sample = sample_id,
  Group = sample_group,
  Hours = unname(time_hours[as.character(sample_group)]),
  Replicate = replicate_number
)

# 2. ORIGINAL CONCATENATED PCA - REPRODUCES THE REPORTED APPROACH
res_pca <- PCA(X, scale.unit = TRUE, ncp = 5, graph = FALSE)
pca_eigenvalues <- as.data.frame(res_pca$eig)
pca_scores <- as.data.frame(res_pca$ind$coord[, 1:2])
pca_scores$Sample <- sample_id
pca_scores$Group <- sample_group
pca_scores$Hours <- metadata$Hours

cat("\nOriginal fused PCA eigenvalues:\n")
print(pca_eigenvalues)

# 3. MULTIPLE FACTOR ANALYSIS - EXAMINER'S BLOCK-SCALED ANALYSIS
# type = "s" treats every block as scaled quantitative data. MFA then weights
# each block using its first partial eigenvalue so that an analytical block
# cannot dominate merely because of its internal variance/covariance structure.
res_mfa <- MFA(
  X,
  group = block_sizes,
  type = rep("s", length(block_sizes)),
  name.group = block_names,
  ncp = 5,
  graph = FALSE
)

mfa_eigenvalues <- as.data.frame(res_mfa$eig)
mfa_scores <- as.data.frame(res_mfa$ind$coord[, 1:2])
mfa_scores$Sample <- sample_id
mfa_scores$Group <- sample_group
mfa_scores$Hours <- metadata$Hours

cat("\nMFA eigenvalues:\n")
print(mfa_eigenvalues)

# 4. VARIANCE SUMMARY
variance_summary <- data.frame(
  Model = c("Original fused PCA", "Block-scaled MFA"),
  Dimension_1_percent = c(res_pca$eig[1, 2], res_mfa$eig[1, 2]),
  Dimension_2_percent = c(res_pca$eig[2, 2], res_mfa$eig[2, 2]),
  Cumulative_Dim1_Dim2_percent = c(res_pca$eig[2, 3], res_mfa$eig[2, 3])
)
print(variance_summary)

# These percentages describe differently weighted spaces and should not be
# interpreted as directly comparable performance measures.

# 5. SAMPLE SCORE PLOTS AND TEMPORAL TRAJECTORIES
colours <- c(
  CT = "#1f77b4", THA = "#ff7f0e", THB = "#2ca02c",
  THC = "#d62728", THD = "#9467bd", THE = "#8c564b",
  THF = "#e377c2", THG = "#7f7f7f", THH = "#bcbd22"
)

plot_pca <- ggplot(pca_scores, aes(Dim.1, Dim.2, fill = Group)) +
  geom_point(shape = 21, colour = "black", size = 4) +
  scale_fill_manual(values = colours, drop = FALSE) +
  theme_classic(base_size = 14) +
  labs(
    x = paste0("PC1 (", round(res_pca$eig[1, 2], 1), "%)"),
    y = paste0("PC2 (", round(res_pca$eig[2, 2], 1), "%)"),
    title = "Original fused PCA", fill = "Samples"
  )

plot_mfa <- ggplot(mfa_scores, aes(Dim.1, Dim.2, fill = Group)) +
  geom_point(shape = 21, colour = "black", size = 4) +
  scale_fill_manual(values = colours, drop = FALSE) +
  theme_classic(base_size = 14) +
  labs(
    x = paste0("MFA Dimension 1 (", round(res_mfa$eig[1, 2], 1), "%)"),
    y = paste0("MFA Dimension 2 (", round(res_mfa$eig[2, 2], 1), "%)"),
    title = "Block-scaled MFA", fill = "Samples"
  )

pca_centroids <- aggregate(cbind(Dim.1, Dim.2) ~ Group + Hours,
                           data = pca_scores, FUN = mean)
mfa_centroids <- aggregate(cbind(Dim.1, Dim.2) ~ Group + Hours,
                           data = mfa_scores, FUN = mean)

plot_pca_trajectory <- plot_pca +
  geom_path(data = pca_centroids, aes(Dim.1, Dim.2, group = 1),
            inherit.aes = FALSE, colour = "black", linewidth = 0.6,
            arrow = grid::arrow(length = grid::unit(0.12, "inches"))) +
  geom_text(data = pca_centroids, aes(Dim.1, Dim.2, label = Group),
            inherit.aes = FALSE, nudge_y = 0.5, size = 4)

plot_mfa_trajectory <- plot_mfa +
  geom_path(data = mfa_centroids, aes(Dim.1, Dim.2, group = 1),
            inherit.aes = FALSE, colour = "black", linewidth = 0.6,
            arrow = grid::arrow(length = grid::unit(0.12, "inches"))) +
  geom_text(data = mfa_centroids, aes(Dim.1, Dim.2, label = Group),
            inherit.aes = FALSE, nudge_y = 0.5, size = 4)

ggsave("Original_fused_PCA.pdf", plot_pca_trajectory, width = 8, height = 6)
ggsave("Block_scaled_MFA.pdf", plot_mfa_trajectory, width = 8, height = 6)

# 6. ANALYTICAL-BLOCK CONTRIBUTIONS IN MFA
block_contributions <- as.data.frame(res_mfa$group$contrib[, 1:2])
block_contributions$Analytical_block <- rownames(block_contributions)
block_contributions <- block_contributions[
  , c("Analytical_block", "Dim.1", "Dim.2")
]
print(block_contributions)

plot_mfa_blocks <- fviz_mfa_var(res_mfa, choice = "group", repel = TRUE) +
  theme_classic(base_size = 14) +
  labs(title = "Contribution of analytical blocks to the MFA model")
ggsave("MFA_block_contributions.pdf", plot_mfa_blocks, width = 8, height = 6)

# 7. CLUSTERING SENSITIVITY ANALYSIS
# Original clustering matches the GitHub approach: Euclidean distances after
# standardising all 50 concatenated variables.
original_distance <- dist(scale(X), method = "euclidean")
original_hclust <- hclust(original_distance, method = "complete")
original_cluster <- cutree(original_hclust, k = 2)

# MFA clustering uses the five-dimensional block-weighted global score space.
mfa_global_scores <- res_mfa$ind$coord[, 1:5, drop = FALSE]
mfa_distance <- dist(mfa_global_scores, method = "euclidean")
mfa_hclust <- hclust(mfa_distance, method = "complete")
mfa_cluster <- cutree(mfa_hclust, k = 2)

cluster_assignments <- data.frame(
  Sample = sample_id,
  Group = sample_group,
  Original_cluster = original_cluster,
  MFA_cluster = mfa_cluster
)
print(cluster_assignments)
print(table(Original = original_cluster, MFA = mfa_cluster))

adjusted_rand_index <- function(x, y) {
  tab <- table(x, y)
  choose_two <- function(z) z * (z - 1) / 2
  n <- sum(tab)
  observed <- sum(choose_two(tab))
  row_pairs <- sum(choose_two(rowSums(tab)))
  column_pairs <- sum(choose_two(colSums(tab)))
  expected <- row_pairs * column_pairs / choose_two(n)
  maximum <- (row_pairs + column_pairs) / 2
  if (maximum == expected) return(1)
  (observed - expected) / (maximum - expected)
}

ari <- adjusted_rand_index(original_cluster, mfa_cluster)
distance_correlation <- cor(as.vector(original_distance),
                            as.vector(mfa_distance), method = "spearman")

model_agreement <- data.frame(
  Comparison = c(
    "Adjusted Rand Index: original versus MFA clusters",
    "Spearman correlation: original versus MFA sample distances"
  ),
  Value = c(ari, distance_correlation)
)
print(model_agreement)

pdf("Original_clustering_dendrogram.pdf", width = 9, height = 7)
plot(original_hclust, labels = sample_id,
     main = "Original fused-data clustering", xlab = "", sub = "")
rect.hclust(original_hclust, k = 2, border = c("darkgreen", "maroon"))
dev.off()

pdf("MFA_clustering_dendrogram.pdf", width = 9, height = 7)
plot(mfa_hclust, labels = sample_id,
     main = "Block-scaled MFA clustering", xlab = "", sub = "")
rect.hclust(mfa_hclust, k = 2, border = c("darkgreen", "maroon"))
dev.off()

# 8. EXPORT ALL NUMERICAL RESULTS
write.xlsx(
  list(
    Variance_summary = variance_summary,
    PCA_eigenvalues = cbind(Dimension = rownames(pca_eigenvalues), pca_eigenvalues),
    MFA_eigenvalues = cbind(Dimension = rownames(mfa_eigenvalues), mfa_eigenvalues),
    PCA_scores = pca_scores,
    MFA_scores = mfa_scores,
    PCA_time_centroids = pca_centroids,
    MFA_time_centroids = mfa_centroids,
    MFA_block_contributions = block_contributions,
    Cluster_assignments = cluster_assignments,
    Model_agreement = model_agreement
  ),
  file = "PCA_MFA_sensitivity_results.xlsx",
  overwrite = TRUE
)

cat("\nSensitivity analysis completed successfully.\n")
cat("The same 27 rows and same 50 variables were used in both models.\n")
