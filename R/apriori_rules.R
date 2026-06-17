# Association Rules Module
# Apriori Algorithm for Market Basket Analysis

library(arules)
library(plotly)
library(dplyr)

# Custom color palette
custom_colors <- c("#667eea", "#764ba2", "#f093fb", "#f5576c", "#4facfe", "#43e97b")

#' Run Apriori Algorithm to find association rules
#' @param data Processed grocery data with 'items' column
#' @param min_support Minimum support threshold (0.001 - 1)
#' @param min_confidence Minimum confidence threshold (0.001 - 1)
#' @return List containing rules data frame and summary
run_apriori <- function(data, min_support, min_confidence) {
  
  # Check if items column exists
  if (!"items" %in% colnames(data)) {
    # If no items column, return empty result with message
    return(list(
      rules_df = data.frame(
        rules = character(),
        support = numeric(),
        confidence = numeric(),
        coverage = numeric(),
        lift = numeric(),
        count = numeric(),
        stringsAsFactors = FALSE
      ),
      rules_summary = "No 'items' column found in dataset. Association rules require transaction data with items."
    ))
  }
  
  # Prepare transaction data
  # Split items by comma and convert to transactions
  items_list <- strsplit(as.character(data$items), ",")
  
  # Remove any empty items
  items_list <- lapply(items_list, function(x) trimws(x[x != ""]))
  
  # Convert to transactions object
  transactions <- as(items_list, "transactions")
  
  # Run Apriori algorithm
  rules <- apriori(
    transactions,
    parameter = list(
      supp = min_support,
      conf = min_confidence,
      minlen = 2,
      maxlen = 10
    ),
    control = list(verbose = FALSE)
  )
  
  if (length(rules) == 0) {
    return(list(
      rules_df = data.frame(
        rules = character(),
        support = numeric(),
        confidence = numeric(),
        coverage = numeric(),
        lift = numeric(),
        count = numeric(),
        stringsAsFactors = FALSE
      ),
      rules_summary = "No rules found with the given support and confidence thresholds."
    ))
  }
  
  # Convert rules to data frame
  rules_df <- as(rules, "data.frame")
  
  # Clean up the rules column name
  colnames(rules_df)[1] <- "rules"
  
  # Round numeric columns
  numeric_cols <- c("support", "confidence", "coverage", "lift", "count")
  for (col in numeric_cols) {
    if (col %in% colnames(rules_df)) {
      rules_df[[col]] <- round(rules_df[[col]], 4)
    }
  }
  
  # Generate summary
  summary_text <- capture.output(inspect(rules))
  
  return(list(
    rules_df = rules_df,
    rules_summary = paste(summary_text, collapse = "\n"),
    transactions = transactions
  ))
}

#' Create scatter plot of association rules (Support vs Confidence)
create_rules_scatter_plot <- function(rules_df) {
  if (nrow(rules_df) == 0) {
    return(plotly_empty(type = "scatter", mode = "markers") %>%
             layout(title = "No rules to display"))
  }
  
  p <- plot_ly(rules_df,
               x = ~support,
               y = ~confidence,
               color = ~lift,
               colors = colorRamp(custom_colors),
               type = 'scatter',
               mode = 'markers',
               marker = list(size = ~count / max(count) * 20 + 5,
                             opacity = 0.8,
                             line = list(color = '#FFFFFF', width = 1)),
               hovertemplate = paste(
                 '<b>Support:</b> %{x:.4f}<br>',
                 '<b>Confidence:</b> %{y:.4f}<br>',
                 '<b>Lift:</b> %{marker.color:.4f}<br>',
                 '<extra></extra>'
               )) %>%
    layout(
      title = list(
        text = "Association Rules: Support vs Confidence (colored by Lift)",
        font = list(size = 16)
      ),
      xaxis = list(
        title = "Support",
        tickfont = list(size = 12)
      ),
      yaxis = list(
        title = "Confidence",
        tickfont = list(size = 12)
      ),
      colorbar = list(title = "Lift"),
      paper_bgcolor = 'rgba(0,0,0,0)',
      plot_bgcolor = 'rgba(0,0,0,0)'
    )
  
  return(p)
}

#' Create item frequency plot
create_item_freq_plot <- function(data) {
  if (!"items" %in% colnames(data)) {
    return(plotly_empty(type = "scatter", mode = "markers") %>%
             layout(title = "No items column found"))
  }
  
  # Split items and count frequencies
  items_list <- unlist(strsplit(as.character(data$items), ","))
  items_list <- trimws(items_list)
  items_list <- items_list[items_list != ""]
  
  item_freq <- as.data.frame(table(items_list))
  colnames(item_freq) <- c("Item", "Frequency")
  item_freq <- item_freq[order(-item_freq$Frequency), ]
  item_freq <- head(item_freq, 15)  # Top 15 items
  
  n_items <- nrow(item_freq)
  bar_colors <- colorRampPalette(custom_colors)(n_items)
  
  p <- plot_ly(item_freq,
               x = ~reorder(Item, -Frequency),
               y = ~Frequency,
               type = 'bar',
               marker = list(color = bar_colors,
                             line = list(color = '#FFFFFF', width = 1)),
               hovertemplate = '<b>%{x}</b><br>Frequency: %{y}<extra></extra>') %>%
    layout(
      title = list(
        text = "Top 15 Most Frequent Items",
        font = list(size = 16)
      ),
      xaxis = list(
        title = "Item",
        tickfont = list(size = 10),
        tickangle = -45
      ),
      yaxis = list(
        title = "Frequency",
        tickfont = list(size = 12)
      ),
      paper_bgcolor = 'rgba(0,0,0,0)',
      plot_bgcolor = 'rgba(0,0,0,0)'
    )
  
  return(p)
}