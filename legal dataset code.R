install.packages(c("readr", "dplyr", "stringr", "tidytext", "textstem"))

library(readr)
library(dplyr)
library(stringr)
library(tidytext)
library(textstem)


df <- read_csv("legal_dataset_group_05_3k.csv", show_col_types = FALSE)


names(df)
head(df)


df <- df %>%
  mutate(doc_id = row_number())

df_clean <- df %>%
  mutate(
    Text = tolower(Text),
    Text = str_replace_all(Text, "<.*?>", " "),
    Text = str_replace_all(Text, "[0-9]+", " "),
    Text = str_replace_all(Text, "[[:punct:]]", " "),
    Text = str_replace_all(Text, "[^a-z\\s]", " "),
    Text = str_squish(Text)
  )

data("stop_words")
custom_stopwords <- data.frame(
  word = c(
    "shall", "may", "must", "also",
    "section", "article", "clause", "subsection",
    "hereby", "herein", "thereof", "therein",
    "whereas", "pursuant", "including",
    "said", "upon", "within"
  )
)
tokens_clean <- df_clean %>%
  select(doc_id, Category, Text) %>%
  unnest_tokens(word, Text) %>%
  anti_join(stop_words, by = "word") %>%
  anti_join(custom_stopwords, by = "word") %>%
  mutate(word = lemmatize_words(word)) %>%
  filter(nchar(word) > 2)

head(tokens_clean)
dim(tokens_clean)

write_csv(tokens_clean, "legal_tokens_clean.csv")




library(dplyr)
library(tidytext)
library(tm)
library(readr)


word_counts <- tokens_clean %>%
  count(doc_id, word, sort = TRUE)

tfidf_data <- word_counts %>%
  bind_tf_idf(word, doc_id, n)

tfidf_data <- tfidf_data %>%
  filter(tf_idf > 0)

dtm <- tfidf_data %>%
  cast_dtm(doc_id, word, tf_idf)

matrix_data <- as.matrix(dtm)

dim(matrix_data)
head(tfidf_data)


write_csv(tfidf_data,"legal_tfidf.csv")

install.packages("uwot")   
library(uwot)              
set.seed(123)


matrix_data <- as.matrix(dtm)

umap_result <- uwot::umap(
  matrix_data,
  n_neighbors = 15,
  min_dist = 0.1,
  n_components = 2,
  metric = "cosine"
)
umap_df <- data.frame(
  doc_id = as.integer(rownames(matrix_data)),
  UMAP1 = umap_result[, 1],
  UMAP2 = umap_result[, 2]
)

category_info <- tokens_clean %>%
  distinct(doc_id, Category)

umap_df <- umap_df %>%
  left_join(category_info, by = "doc_id")

head(umap_df)
dim(umap_df)


write_csv(umap_df,"legal_umap_result.csv")


install.packages(c("ggplot2", "uwot", "dbscan"))
library(ggplot2)
library(uwot)
library(dbscan)
install.packages("tidyverse")
library(tidyverse)

cluster_data <- umap_df[, c("UMAP1", "UMAP2")]

head(cluster_data)
dim(cluster_data)
set.seed(123)

wss <- numeric(10)
for (k in 1:10) {
  km <- kmeans(cluster_data, centers = k, nstart = 25)
  wss[k] <- km$tot.withinss
}
elbow_df <- data.frame(k = 1:10, wss = wss)

print(elbow_df)

plot(
  elbow_df$k,
  elbow_df$wss,
  type = "b",
  xlab = "Number of Clusters (K)",
  ylab = "Within-Cluster Sum of Squares",
  main = "Elbow Method for Optimal K"
)

cluster_data <- umap_df[, c("UMAP1", "UMAP2")]

set.seed(123)
kmeans_result <- kmeans(cluster_data, centers = 3, nstart = 25)
umap_df$kmeans_cluster <- kmeans_result$cluster



library(dbscan)

hdbscan_result <- hdbscan(cluster_data, minPts = 15)

umap_df$hdbscan_cluster <- hdbscan_result$cluster


dist_matrix <- dist(cluster_data, method = "euclidean")

hc_result <- hclust(dist_matrix, method = "ward.D2")

hierarchical_result <- cutree(hc_result, k = 3)

umap_df$hierarchical_cluster <- hierarchical_result


head(umap_df)

table(umap_df$kmeans_cluster)
table(umap_df$hdbscan_cluster)
table(umap_df$hierarchical_cluster)



write.csv(umap_df,"legal_clustering_results.csv",row.names = FALSE)

table(umap_df$Category, umap_df$kmeans_cluster)


library(ggplot2)

ggplot(umap_df, aes(x = UMAP1, y = UMAP2, color = Category)) +
  geom_point(alpha = 0.7) +
  labs(
    title = "Actual Document Categories",
    x = "UMAP1",
    y = "UMAP2",
    color = "Category"
  ) +
  theme_minimal()

ggplot(umap_df, aes(x = UMAP1, y = UMAP2, color = as.factor(kmeans_cluster))) +
  geom_point(alpha = 0.7) +
  labs(
    title = "K-Means Clustering (K=3)",
    x = "UMAP1",
    y = "UMAP2",
    color = "Cluster"
  ) +
  theme_minimal()

ggplot(umap_df, aes(x = UMAP1, y = UMAP2, color = as.factor(hdbscan_cluster))) +
  geom_point(alpha = 0.7) +
  labs(
    title = "HDBSCAN Clustering",
    x = "UMAP1",
    y = "UMAP2",
    color = "Cluster"
  ) +
  theme_minimal()

ggplot(umap_df, aes(x = UMAP1, y = UMAP2, color = as.factor(hierarchical_cluster))) +
  geom_point(alpha = 0.7) +
  labs(
    title = "Hierarchical Clustering",
    x = "UMAP1",
    y = "UMAP2",
    color = "Cluster"
  ) +
  theme_minimal()


plot(
  hc_result,
  labels = FALSE,
  main = "Hierarchical Clustering Dendrogram",
  xlab = "",
  sub = ""
)




kmeans_dist <- table(umap_df$kmeans_cluster)

print("K-Means Distribution:")
print(kmeans_dist)

print("K-Means Percentage:")
print(round(prop.table(kmeans_dist) * 100, 2))



hdbscan_dist <- table(umap_df$hdbscan_cluster)

print("HDBSCAN Distribution:")
print(hdbscan_dist)

print("HDBSCAN Percentage:")
print(round(prop.table(hdbscan_dist) * 100, 2))



hier_dist <- table(umap_df$hierarchical_cluster)

print("Hierarchical Distribution:")
print(hier_dist)

print("Hierarchical Percentage:")
print(round(prop.table(hier_dist) * 100, 2))



barplot(kmeans_dist,
        col = "skyblue",
        main = "K-Means Cluster Distribution",
        xlab = "Cluster",
        ylab = "Number of Documents")

# HDBSCAN Plot
barplot(hdbscan_dist,
        col = "orange",
        main = "HDBSCAN Cluster Distribution",
        xlab = "Cluster",
        ylab = "Number of Documents")

# Hierarchical Plot
barplot(hier_dist,
        col = "lightgreen",
        main = "Hierarchical Cluster Distribution",
        xlab = "Cluster",
        ylab = "Number of Documents")



install.packages("mclust")
library(cluster)
library(mclust)

cluster_data <- umap_df[, c("UMAP1", "UMAP2")]
dist_matrix <- dist(cluster_data)


silhouette_kmeans <- mean(silhouette(umap_df$kmeans_cluster, dist_matrix)[, 3])
silhouette_hier <- mean(silhouette(umap_df$hierarchical_cluster, dist_matrix)[, 3])

hdbscan_eval <- umap_df[umap_df$hdbscan_cluster != 0, ]
hdbscan_data <- hdbscan_eval[, c("UMAP1", "UMAP2")]
silhouette_hdbscan <- mean(
  silhouette(hdbscan_eval$hdbscan_cluster, dist(hdbscan_data))[, 3]
)


ari_kmeans <- adjustedRandIndex(umap_df$Category, umap_df$kmeans_cluster)
ari_hdbscan <- adjustedRandIndex(umap_df$Category, umap_df$hdbscan_cluster)
ari_hier <- adjustedRandIndex(umap_df$Category, umap_df$hierarchical_cluster)


calculate_nmi <- function(actual, predicted) {
  tab <- table(actual, predicted)
  n <- sum(tab)
  pxy <- tab / n
  px <- rowSums(tab) / n
  py <- colSums(tab) / n
  
  mi <- 0
  for (i in 1:nrow(pxy)) {
    for (j in 1:ncol(pxy)) {
      if (pxy[i, j] > 0) {
        mi <- mi + pxy[i, j] * log(pxy[i, j] / (px[i] * py[j]))
      }
    }
  }
  
  hx <- -sum(px * log(px))
  hy <- -sum(py * log(py))
  
  mi / sqrt(hx * hy)
}


nmi_kmeans <- calculate_nmi(umap_df$Category, umap_df$kmeans_cluster)
nmi_hdbscan <- calculate_nmi(umap_df$Category, umap_df$hdbscan_cluster)
nmi_hier <- calculate_nmi(umap_df$Category, umap_df$hierarchical_cluster)


evaluation_results <- data.frame(
  Algorithm = c("K-Means", "HDBSCAN", "Hierarchical"),
  Silhouette_Score = c(silhouette_kmeans, silhouette_hdbscan, silhouette_hier),
  NMI = c(nmi_kmeans, nmi_hdbscan, nmi_hier),
  ARI = c(ari_kmeans, ari_hdbscan, ari_hier)
)

evaluation_results




library(dplyr)


tokens_kmeans <- merge(
  tokens_clean,
  umap_df[, c("doc_id", "kmeans_cluster")],
  by = "doc_id"
)

top_kmeans <- tokens_kmeans %>%
  count(kmeans_cluster, word, sort = TRUE) %>%
  group_by(kmeans_cluster) %>%
  slice_max(n, n = 20) %>%
  arrange(kmeans_cluster, desc(n))



tokens_hdbscan <- merge(
  tokens_clean,
  umap_df[, c("doc_id", "hdbscan_cluster")],
  by = "doc_id"
)

tokens_hdbscan <- tokens_hdbscan[tokens_hdbscan$hdbscan_cluster != 0, ]

top_hdbscan <- tokens_hdbscan %>%
  count(hdbscan_cluster, word, sort = TRUE) %>%
  group_by(hdbscan_cluster) %>%
  slice_max(n, n = 20) %>%
  arrange(hdbscan_cluster, desc(n))



tokens_hier <- merge(
  tokens_clean,
  umap_df[, c("doc_id", "hierarchical_cluster")],
  by = "doc_id"
)

top_hier <- tokens_hier %>%
  count(hierarchical_cluster, word, sort = TRUE) %>%
  group_by(hierarchical_cluster) %>%
  slice_max(n, n = 20) %>%
  arrange(hierarchical_cluster, desc(n))




print("K-Means Top 20 Words:")
print(top_kmeans)

print("HDBSCAN Top 20 Words (Noise removed):")
print(top_hdbscan)

print("Hierarchical Top 20 Words:")
print(top_hier)


kmeans_table <- top_kmeans %>%
  group_by(kmeans_cluster) %>%
  summarise(
    top_words = paste(word, collapse = ", "),
    .groups = "drop"
  )

kmeans_table



kmeans_table$document_type <- NA

kmeans_table$document_type[kmeans_table$kmeans_cluster == 1] <- "Court Judgment"
kmeans_table$document_type[kmeans_table$kmeans_cluster == 2] <- "Contract"
kmeans_table$document_type[kmeans_table$kmeans_cluster == 3] <- "Policy"

kmeans_table


umap_df$kmeans_predicted_type <- kmeans_table$document_type[
  match(umap_df$kmeans_cluster, kmeans_table$kmeans_cluster)
]


table(
  Actual = umap_df$Category,
  Predicted = umap_df$kmeans_predicted_type
)



hier_table <- top_hier %>%
  group_by(hierarchical_cluster) %>%
  summarise(
    top_words = paste(word, collapse = ", "),
    .groups = "drop"
  )

hier_table


hier_table$document_type <- NA

hier_table$document_type[hier_table$hierarchical_cluster == 1] <- "Court Judgment"
hier_table$document_type[hier_table$hierarchical_cluster == 2] <- "Contract"
hier_table$document_type[hier_table$hierarchical_cluster == 3] <- "Policy"

hier_table
umap_df$hier_predicted_type <- hier_table$document_type[
  match(umap_df$hierarchical_cluster, hier_table$hierarchical_cluster)
]

table(
  Actual = umap_df$Category,
  Predicted = umap_df$hier_predicted_type
)



hdbscan_type_table <- aggregate(
  Category ~ hdbscan_cluster,
  data = umap_df,
  FUN = function(x) names(sort(table(x), decreasing = TRUE))[1]
)

colnames(hdbscan_type_table) <- c("hdbscan_cluster", "document_type")

hdbscan_type_table


umap_df$hdbscan_predicted_type <- hdbscan_type_table$document_type[
  match(umap_df$hdbscan_cluster, hdbscan_type_table$hdbscan_cluster)
]

table(
  Actual = umap_df$Category,
  Predicted = umap_df$hdbscan_predicted_type
)


library(ggplot2)

ggplot(umap_df, aes(x = UMAP1, y = UMAP2, color = kmeans_predicted_type)) +
  geom_point(alpha = 0.7) +
  labs(
    title = "Document Type Discovery using K-Means",
    x = "UMAP1",
    y = "UMAP2",
    color = "Document Type"
  ) +
  theme_minimal()

ggplot(umap_df, aes(x = UMAP1, y = UMAP2, color = hier_predicted_type)) +
  geom_point(alpha = 0.7) +
  labs(
    title = "Document Type Discovery using Hierarchical Clustering",
    x = "UMAP1",
    y = "UMAP2",
    color = "Document Type"
  ) +
  theme_minimal()

ggplot(umap_df, aes(x = UMAP1, y = UMAP2, color = hdbscan_predicted_type)) +
  geom_point(alpha = 0.7) +
  labs(
    title = "Document Type Discovery using HDBSCAN",
    x = "UMAP1",
    y = "UMAP2",
    color = "Document Type"
  ) +
  theme_minimal()
