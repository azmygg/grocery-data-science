# Visualization Module
# Creates interactive plots for the grocery data analysis

library(ggplot2)
library(plotly)
library(dplyr)
library(gridExtra)

# Color palette for consistent theming
custom_colors <- c("#667eea", "#764ba2", "#f093fb", "#f5576c", "#4facfe", "#43e97b", "#fa709a", "#fee140")

#' Create Pie Chart: Cash vs Credit Comparison
create_payment_plot <- function(data) {
  # Aggregate total spending by payment type
  totals <- aggregate(total ~ paymentType, data = data, sum)
  totals$percentage <- round(totals$total / sum(totals$total) * 100, 1)
  
  # Create color mapping
  color_map <- setNames(custom_colors[1:length(totals$paymentType)], totals$paymentType)
  
  p <- plot_ly(totals, 
               labels = ~paymentType, 
               values = ~total, 
               type = 'pie',
               hole = 0.4,
               marker = list(colors = custom_colors[1:nrow(totals)],
                             line = list(color = '#FFFFFF', width = 2)),
               textinfo = 'label+percent',
               textposition = 'outside',
               hovertemplate = '<b>%{label}</b><br>Total: $%{value:,.2f}<br>Percentage: %{percent}<extra></extra>') %>%
    layout(
      title = list(text = "Payment Type Distribution", font = list(size = 18)),
      showlegend = TRUE,
      legend = list(orientation = "h", y = -0.1),
      paper_bgcolor = 'rgba(0,0,0,0)',
      plot_bgcolor = 'rgba(0,0,0,0)'
    )
  
  return(p)
}

#' Create Line Chart: Age vs Total Spending
create_age_spending_plot <- function(data) {
  # Summarize total spending by age
  age_spending <- aggregate(total ~ age, data = data, sum)
  age_spending <- age_spending[order(age_spending$age), ]
  
  p <- plot_ly(age_spending, 
               x = ~age, 
               y = ~total,
               type = 'scatter',
               mode = 'lines+markers',
               line = list(color = '#667eea', width = 3),
               marker = list(color = '#764ba2', size = 8, line = list(color = '#FFFFFF', width = 2)),
               hovertemplate = '<b>Age: %{x}</b><br>Total Spending: $%{y:,.2f}<extra></extra>') %>%
    layout(
      title = list(text = "Total Spending by Age", font = list(size = 18)),
      xaxis = list(title = "Age", tickfont = list(size = 12)),
      yaxis = list(title = "Total Spending ($)", tickfont = list(size = 12)),
      paper_bgcolor = 'rgba(0,0,0,0)',
      plot_bgcolor = 'rgba(0,0,0,0)'
    )
  
  return(p)
}

#' Create Bar Chart: City Spending (Descending)
create_city_spending_plot <- function(data) {
  # Summarize total spending by city and sort descending
  city_spending <- aggregate(total ~ city, data = data, sum)
  city_spending <- city_spending[order(-city_spending$total), ]
  
  # Generate colors for each city
  n_cities <- nrow(city_spending)
  bar_colors <- colorRampPalette(custom_colors)(n_cities)
  
  p <- plot_ly(city_spending,
               x = ~reorder(city, -total),
               y = ~total,
               type = 'bar',
               marker = list(color = bar_colors,
                             line = list(color = '#FFFFFF', width = 1)),
               hovertemplate = '<b>%{x}</b><br>Total Spending: $%{y:,.2f}<extra></extra>') %>%
    layout(
      title = list(text = "Total Spending by City (Descending)", font = list(size = 18)),
      xaxis = list(title = "City", tickfont = list(size = 10), tickangle = -45),
      yaxis = list(title = "Total Spending ($)", tickfont = list(size = 12)),
      paper_bgcolor = 'rgba(0,0,0,0)',
      plot_bgcolor = 'rgba(0,0,0,0)'
    )
  
  return(p)
}

#' Create Histogram: Distribution of Total Spending
create_spending_dist_plot <- function(data) {
  p <- plot_ly(data, 
               x = ~total,
               type = 'histogram',
               nbinsx = 30,
               marker = list(color = 'rgba(102, 126, 234, 0.7)',
                             line = list(color = '#667eea', width = 1)),
               hovertemplate = '<b>Spending Range: %{x}</b><br>Count: %{y}<extra></extra>') %>%
    layout(
      title = list(text = "Distribution of Total Spending", font = list(size = 18)),
      xaxis = list(title = "Total Spending ($)", tickfont = list(size = 12)),
      yaxis = list(title = "Frequency", tickfont = list(size = 12)),
      paper_bgcolor = 'rgba(0,0,0,0)',
      plot_bgcolor = 'rgba(0,0,0,0)',
      bargap = 0.1
    )
  
  return(p)
}

#' Create Combined Dashboard (static ggplot for combined view)
create_combined_dashboard <- function(data) {
  # Plot 1: Cash vs Credit (Pie)
  totals <- aggregate(total ~ paymentType, data = data, sum)
  p1 <- ggplot(totals, aes(x = "", y = total, fill = paymentType)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y") +
    scale_fill_manual(values = custom_colors[1:2]) +
    labs(title = "1. Cash vs Credit", fill = "Payment Type") +
    theme_void() +
    theme(legend.position = "bottom")
  
  # Plot 2: Age vs Spending (Line)
  age_spending <- aggregate(total ~ age, data = data, sum)
  age_spending <- age_spending[order(age_spending$age), ]
  p2 <- ggplot(age_spending, aes(x = age, y = total)) +
    geom_line(color = custom_colors[1], linewidth = 1) +
    geom_point(color = custom_colors[2], size = 2) +
    labs(title = "2. Age vs Total Spending", x = "Age", y = "Total Spending") +
    theme_minimal()
  
  # Plot 3: City Spending (Bar)
  city_spending <- aggregate(total ~ city, data = data, sum)
  city_spending <- city_spending[order(-city_spending$total), ]
  p3 <- ggplot(city_spending, aes(x = reorder(city, -total), y = total)) +
    geom_bar(stat = "identity", fill = custom_colors[3]) +
    labs(title = "3. City Spending (Descending)", x = "City", y = "Total Spending") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Plot 4: Distribution (Histogram)
  p4 <- ggplot(data, aes(x = total)) +
    geom_histogram(bins = 30, fill = custom_colors[4], color = "white") +
    labs(title = "4. Distribution of Total Spending", x = "Total Spending", y = "Frequency") +
    theme_minimal()
  
  # Combine all plots
  combined <- grid.arrange(p1, p2, p3, p4, ncol = 2,
                           top = "Grocery Data Analytics Dashboard")
  
  return(combined)
}