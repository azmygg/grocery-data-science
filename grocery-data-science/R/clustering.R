# Clustering Module
# K-Means Clustering for Customer Segmentation

library(ggplot2)
library(plotly)
library(cluster)

# Custom color palette for clusters
cluster_colors <- c("#667eea", "#764ba2", "#f093fb", "#f5576c")

#' Perform K-Means Clustering on customer data
#' @param data Processed grocery data
#' @param num_clusters Number of clusters (2-4)
#' @return Data frame with cluster assignments
perform_kmeans <- function(data, num_clusters) {
  # Aggregate total spending per customer
  aggregated_data <- aggregate(
    cbind(total, age) ~ customer, 
    data = data, 
    FUN = function(x) if(is.numeric(x)) sum(x) else first(x)
  )
  
  # Ensure we have the age column properly
  customer_age <- aggregate(age ~ customer, data = data, FUN = function(x) first(x))
  customer_total <- aggregate(total ~ customer, data = data, FUN = sum)
  
  aggregated_data <- merge(customer_age, customer_total, by = "customer")
  
  # Select features for clustering (total spending and age)
  features <- aggregated_data[, c("total", "age")]
  
  # Normalize features for better clustering
  features_scaled <- scale(features)
  
  # Perform K-Means clustering
  set.seed(123)  # For reproducibility
  kmeans_result <- kmeans(features_scaled, centers = num_clusters, nstart = 25)
  
  # Add cluster assignments to the original data
  aggregated_data$cluster <- as.factor(kmeans_result$cluster)
  
  # Rename columns for clarity
  colnames(aggregated_data) <- c("Customer", "Age", "Total_Spending", "Cluster")
  
  return(aggregated_data)
}

#' Create interactive scatter plot for clusters
create_cluster_plot <- function(cluster_data) {
  # Get number of unique clusters
  n_clusters <- length(unique(cluster_data$Cluster))
  colors_to_use <- cluster_colors[1:n_clusters]
  
  p <- plot_ly(cluster_data,
               x = ~Age,
               y = ~Total_Spending,
               color = ~Cluster,
               colors = colors_to_use,
               type = 'scatter',
               mode = 'markers',
               marker = list(size = 12,
                             opacity = 0.8,
                             line = list(color = '#FFFFFF', width = 2)),
               hovertemplate = paste(
                 '<b>%{text}</b><br>',
                 'Age: %{x}<br>',
                 'Total Spending: $%{y:,.2f}<br>',
                 '<extra></extra>'
               ),
               text = ~paste("Customer:", Customer)) %>%
    layout(
      title = list(
        text = paste("K-Means Clustering: Customer Segmentation (K =", n_clusters, ")"),
        font = list(size = 18)
      ),
      xaxis = list(
        title = "Customer Age",
        tickfont = list(size = 12)
      ),
      yaxis = list(
        title = "Total Spending ($)",
        tickfont = list(size = 12)
      ),
      legend = list(
        title = list(text = "Cluster"),
        orientation = "h",
        y = -0.15
      ),
      paper_bgcolor = 'rgba(0,0,0,0)',
      plot_bgcolor = 'rgba(0,0,0,0)'
    )
  
  return(p)
}
