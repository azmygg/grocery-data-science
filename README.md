# Grocery Store Data Analytics Platform

> **Introduction to Data Science - Fall 2024/2025**  
> **Alexandria University - Faculty of Computers and Science**

A comprehensive R Shiny web application for analyzing grocery store data. This project demonstrates practical applications of data science concepts including data preprocessing, interactive visualization, K-Means clustering, and Apriori association rule mining.

## Features

- **File Upload**: Easy CSV file upload with drag-and-drop support
- **Data Cleaning**: Automatic duplicate removal, NA handling, and outlier detection using IQR method
- **Interactive Dashboard**: 4 interactive Plotly visualizations:
  1. Cash vs Credit payment comparison (donut chart)
  2. Age vs Total Spending analysis (line chart)
  3. City Total Spending ranking (bar chart)
  4. Total Spending distribution (histogram)
- **Customer Clustering**: K-Means clustering with interactive scatter plot and downloadable results table
- **Association Rules**: Apriori algorithm with configurable support/confidence thresholds
- **Report Generation**: Downloadable PDF project report

## Technologies

- **R** - Statistical computing language
- **Shiny** - Web application framework for R
- **ggplot2 & plotly** - Data visualization
- **arules** - Association rules mining
- **DT** - Interactive data tables

## Project Structure

```
grocery-data-science/
├── app.R                 # Main Shiny application
├── README.md             # Project documentation
├── data/                 # Sample dataset folder
│   └── GRC.csv          # Sample grocery dataset
├── R/                    # R modules
│   ├── data_processing.R  # Data cleaning functions
│   ├── visualization.R    # Plotting functions
│   ├── clustering.R       # K-Means clustering
│   └── apriori_rules.R    # Apriori association rules
├── report/               # Report templates
│   └── Project_Report.Rmd # RMarkdown report template
└── www/                  # Static assets (CSS, images)
```

## Installation & Setup

### Prerequisites

- R (version 4.0 or higher)
- RStudio (recommended)

### Required R Packages

Install the following packages before running the application:

```r
install.packages(c(
  "shiny",
  "shinythemes",
  "shinycssloaders",
  "DT",
  "plotly",
  "dplyr",
  "ggplot2",
  "gridExtra",
  "arules",
  "cluster",
  "factoextra",
  "rmarkdown",
  "knitr"
))
```

### Running the Application

1. **Clone or download** this repository to your local machine

2. **Open RStudio** and navigate to the project folder

3. **Run the application** using one of these methods:
   - Click "Run App" button in RStudio (top-right of script)
   - Or run in R console:
     ```r
     shiny::runApp("/path/to/grocery-data-science")
     ```

4. The application will open in your default web browser

### Using the Application

1. **Upload Data** tab: Click "Browse" and select your GRC CSV file
2. **Visualization Dashboard**: View interactive charts after uploading data
3. **Customer Clustering**: Adjust the number of clusters (2-4) using the slider
4. **Association Rules**: Set support/confidence values and click "Run Apriori"
5. **About & Report**: Download the generated PDF project report

## Dataset Format

The CSV file should contain the following columns:

| Column | Description | Type |
|:-------|:------------|:-----|
| customer | Customer name/ID | String |
| age | Customer age | Integer |
| city | City name | String |
| total | Transaction total | Numeric |
| paymentType | Payment method | cash/credit |
| items | Purchased items (comma-separated) | String |
| count | Item count | Integer |

## User Inputs

| Parameter | Description | Range |
|:----------|:------------|:------|
| Number of Clusters | For K-Means clustering | 2 - 4 |
| Minimum Support | Apriori support threshold | 0.001 - 1 |
| Minimum Confidence | Apriori confidence threshold | 0.001 - 1 |

## Screenshots

[Add screenshots of your application here]

## Project Report

The project report (PDF) can be generated from the "About & Report" tab in the application. The report includes:
- Students information
- Problem description
- Dataset description
- Code explanation
- Results and insights
- Application screenshots

## Group Members

| Name | ID |
|:-----|:---|
| [Student 1] | [ID] |
| [Student 2] | [ID] |
| [Student 3] | [ID] |

## Acknowledgments

- Alexandria University, Faculty of Computers and Science
- Introduction to Data Science course, Fall 2024-2025

## License

This project is created for educational purposes as part of the Introduction to Data Science course requirements.
