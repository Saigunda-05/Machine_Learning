# Required Libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(cluster)
library(factoextra)
library(dbscan)
library(tidyr)
library(scales)
library(reshape2)

# Load dataset
data <- read_excel("Online Retail.xlsx")

# View structure
str(data)
summary(data)

# -----------------------------
# Data Cleaning & Preprocessing
# -----------------------------

# Remove missing CustomerID
data <- data[!is.na(data$CustomerID), ]

# Remove negative or zero quantities (returns/cancellations)
data <- data[data$Quantity > 0 & data$UnitPrice > 0, ]

# Create TotalPrice column
data$TotalPrice <- data$Quantity * data$UnitPrice

# -----------------------------
# RFM Feature Engineering (Recency, Frequency, Monetary)
# -----------------------------

# Convert InvoiceDate to Date type
data$InvoiceDate <- as.Date(data$InvoiceDate)

# Define analysis date as one day after the max date
analysis_date <- max(data$InvoiceDate) + 1

# Grouping by Customer
rfm_data <- data %>%
  group_by(CustomerID) %>%
  summarise(
    Recency = as.numeric(analysis_date - max(InvoiceDate)),
    Frequency = n_distinct(InvoiceNo),
    Monetary = sum(TotalPrice)
  )

# Remove any zero monetary value customers
rfm_data <- rfm_data %>% filter(Monetary > 0)

# Normalize features
rfm_scaled <- as.data.frame(scale(rfm_data[, 2:4]))
rownames(rfm_scaled) <- rfm_data$CustomerID

# -----------------------------
# Exploratory Data Analysis (EDA)
# -----------------------------



#Top 10 Selling Products
top_products <- data %>%
  group_by(Description) %>%
  summarise(TotalQty = sum(Quantity)) %>%
  arrange(desc(TotalQty)) %>%
  slice_head(n = 10)

ggplot(top_products, aes(x = reorder(Description, TotalQty), y = TotalQty)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Selling Products", x = "Product", y = "Total Quantity Sold") +
  theme_minimal()


#Monthly Sales Trend
data$InvoiceMonth <- format(data$InvoiceDate, "%Y-%m")

monthly_sales <- data %>%
  group_by(InvoiceMonth) %>%
  summarise(MonthlySales = sum(TotalPrice))

ggplot(monthly_sales, aes(x = InvoiceMonth, y = MonthlySales)) +
  geom_line(group = 1, color = "darkgreen") +
  geom_point(color = "darkred") +
  labs(title = "Monthly Sales Trend", x = "Month", y = "Sales (£)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Sales by Country


country_sales <- data %>%
  group_by(Country) %>%
  summarise(Sales = sum(TotalPrice)) %>%
  arrange(desc(Sales)) %>%
  slice_head(n = 10)

ggplot(country_sales, aes(x = reorder(Country, Sales), y = Sales)) +
  geom_bar(stat = "identity", fill = "coral") +
  coord_flip() +
  labs(title = "Top 10 Countries by Sales", x = "Country", y = "Total Sales") +
  theme_minimal()

#Distribution of Order Size (TotalPrice per Invoice)

invoice_totals <- data %>%
  group_by(InvoiceNo) %>%
  summarise(InvoiceTotal = sum(TotalPrice))

ggplot(invoice_totals, aes(x = InvoiceTotal)) +
  geom_histogram(bins = 40, fill = "skyblue", color = "black") +
  scale_x_log10(labels = scales::comma) +
  labs(title = "Invoice Total Distribution (Log Scale)", x = "Invoice Total (£)", y = "Count") +
  theme_minimal()

# Pair plot
pairs(rfm_scaled, main = "Pairwise Scatterplot of Scaled RFM Features")

# -----------------------------
# Clustering Algorithms
# -----------------------------

# 1. K-Means Clustering

# Elbow method to find optimal k
fviz_nbclust(rfm_scaled, kmeans, method = "wss") + ggtitle("Elbow Method")

# Apply K-means with 4 clusters
set.seed(123)
km <- kmeans(rfm_scaled, centers = 4, nstart = 25)

# Visualize K-means clusters
fviz_cluster(km, data = rfm_scaled, ellipse.type = "norm")

# Add cluster results
rfm_data$KMeansCluster <- as.factor(km$cluster)

# 2. Hierarchical Clustering
d <- dist(rfm_scaled)
hc <- hclust(d, method = "ward.D2")

# Plot dendrogram
plot(hc, labels = FALSE, hang = -1, main = "Dendrogram - Hierarchical Clustering")
rect.hclust(hc, k = 4, border = "red")

# Assign clusters
rfm_data$HCCluster <- as.factor(cutree(hc, k = 4))

# 3. DBSCAN Clustering
db <- dbscan(rfm_scaled, eps = 0.6, minPts = 5)

# Visualize DBSCAN
fviz_cluster(list(data = rfm_scaled, cluster = db$cluster), stand = FALSE, geom = "point", show.clust.cent = FALSE)

rfm_data$DBSCANCluster <- as.factor(db$cluster)

# -----------------------------
# PCA for 2D Visualisation
# -----------------------------
pca <- prcomp(rfm_scaled)
pca_data <- data.frame(pca$x[, 1:2], Cluster = rfm_data$KMeansCluster)

ggplot(pca_data, aes(PC1, PC2, color = Cluster)) +
  geom_point(alpha = 0.7) +
  ggtitle("PCA Plot of Clusters (KMeans)") +
  theme_minimal()

# -----------------------------
# Save Cleaned/Clustered Data
# -----------------------------
write.csv(rfm_data, "clustered_customers.csv", row.names = FALSE)

