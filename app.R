library(shiny)
library(shinythemes)
library(shinycssloaders)
library(DT)
library(plotly)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(arules)
library(cluster)
library(factoextra)

# Source module files
source("R/data_processing.R")
source("R/visualization.R")
source("R/clustering.R")
source("R/apriori_rules.R")

# UI Definition
ui <- tagList(
  # Include custom CSS
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  
  navbarPage(
    title = div(
      icon("shopping-basket"),
      "Grocery Data Science Analytics"
    ),
    theme = shinytheme("flatly"),
    windowTitle = "Grocery Analytics | Data Science Project",
    
    # Tab 1: Data Upload
    tabPanel(
      title = tags$span(icon("upload"), "Upload Data"),
      value = "upload",
      div(class = "container",
          div(class = "jumbotron text-center",
              style = "margin-top: 30px; padding: 50px; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; border-radius: 15px;",
              h1(tags$strong("Grocery Store Data Analytics")),
              p(class = "lead", "Introduction to Data Science - Fall 2024/2025"),
              hr(class = "my-4"),
              p("Upload your Grocery (GRC) dataset to begin analysis. The dataset should be in CSV format.")
          ),
          
          div(class = "row",
              div(class = "col-md-6 col-md-offset-3",
                  div(class = "panel panel-primary",
                      div(class = "panel-heading",
                          h3(class = "panel-title", tags$span(icon("file-csv"), "Dataset Upload"))
                      ),
                      div(class = "panel-body",
                          fileInput("file_upload", 
                                    label = tags$strong("Select your GRC dataset (CSV format):"),
                                    accept = c(".csv", "text/csv"),
                                    buttonLabel = tags$span(icon("folder-open"), " Browse..."),
                                    placeholder = "No file selected"
                          ),
                          helpText("Please upload the Grocery (GRC) dataset in CSV format."),
                          
                          conditionalPanel(
                            condition = "output.file_uploaded",
                            div(class = "alert alert-success",
                                icon("check-circle"),
                                tags$strong(" File uploaded successfully!"),
                                br(),
                                textOutput("file_info")
                            )
                          ),
                          
                          conditionalPanel(
                            condition = "!output.file_uploaded",
                            div(class = "alert alert-info",
                                icon("info-circle"), " Please upload a CSV file to proceed with the analysis."
                            )
                          )
                      )
                  )
              )
          ),
          
          # Data Preview Section
          conditionalPanel(
            condition = "output.file_uploaded",
            div(class = "row",
                div(class = "col-md-12",
                    div(class = "panel panel-default",
                        div(class = "panel-heading",
                            h3(class = "panel-title", tags$span(icon("table"), "Data Preview (First 10 Rows)"))
                        ),
                        div(class = "panel-body",
                            withSpinner(DTOutput("data_preview"), type = 8, color = "#667eea")
                        )
                    )
                )
            ),
            
            # Data Summary Cards
            div(class = "row",
                div(class = "col-md-3",
                    div(class = "panel panel-info text-center",
                        div(class = "panel-body",
                            h1(textOutput("total_rows"), style = "color: #667eea; font-size: 36px;"),
                            p("Total Rows", class = "text-muted")
                        )
                    )
                ),
                div(class = "col-md-3",
                    div(class = "panel panel-info text-center",
                        div(class = "panel-body",
                            h1(textOutput("total_customers"), style = "color: #764ba2; font-size: 36px;"),
                            p("Unique Customers", class = "text-muted")
                        )
                    )
                ),
                div(class = "col-md-3",
                    div(class = "panel panel-info text-center",
                        div(class = "panel-body",
                            h1(textOutput("total_cities"), style = "color: #f093fb; font-size: 36px;"),
                            p("Cities", class = "text-muted")
                        )
                    )
                ),
                div(class = "col-md-3",
                    div(class = "panel panel-info text-center",
                        div(class = "panel-body",
                            h1(textOutput("total_revenue"), style = "color: #4facfe; font-size: 36px;"),
                            p("Total Revenue", class = "text-muted")
                        )
                    )
                )
            ),
            
            # Data Cleaning Info
            div(class = "row",
                div(class = "col-md-12",
                    div(class = "panel panel-warning",
                        div(class = "panel-heading",
                            h3(class = "panel-title", tags$span(icon("broom"), "Data Cleaning Information"))
                        ),
                        div(class = "panel-body",
                            verbatimTextOutput("cleaning_info")
                        )
                    )
                )
            )
          )
      )
    ),
    
    # Tab 2: Data Visualization Dashboard
    tabPanel(
      title = tags$span(icon("chart-bar"), "Visualization Dashboard"),
      value = "visualization",
      conditionalPanel(
        condition = "!output.file_uploaded",
        div(class = "alert alert-warning text-center", style = "margin-top: 50px;",
            h3(icon("exclamation-triangle")), 
            p("Please upload a dataset first in the 'Upload Data' tab.")
        )
      ),
      conditionalPanel(
        condition = "output.file_uploaded",
        div(class = "container-fluid",
            div(class = "page-header",
                h2(tags$span(icon("chart-bar"), "Data Visualization Dashboard"))
            ),
            
            # Row 1: Payment Comparison & Age vs Spending
            div(class = "row",
                div(class = "col-md-6",
                    div(class = "panel panel-default",
                        div(class = "panel-heading",
                            h3(class = "panel-title", "1. Cash vs Credit Comparison")
                        ),
                        div(class = "panel-body",
                            withSpinner(plotlyOutput("payment_plot", height = "400px"), type = 8)
                        )
                    )
                ),
                div(class = "col-md-6",
                    div(class = "panel panel-default",
                        div(class = "panel-heading",
                            h3(class = "panel-title", "2. Age vs Total Spending")
                        ),
                        div(class = "panel-body",
                            withSpinner(plotlyOutput("age_spending_plot", height = "400px"), type = 8)
                        )
                    )
                )
            ),
            
            # Row 2: City Spending & Distribution
            div(class = "row",
                div(class = "col-md-6",
                    div(class = "panel panel-default",
                        div(class = "panel-heading",
                            h3(class = "panel-title", "3. City Total Spending (Descending)")
                        ),
                        div(class = "panel-body",
                            withSpinner(plotlyOutput("city_spending_plot", height = "400px"), type = 8)
                        )
                    )
                ),
                div(class = "col-md-6",
                    div(class = "panel panel-default",
                        div(class = "panel-heading",
                            h3(class = "panel-title", "4. Distribution of Total Spending")
                        ),
                        div(class = "panel-body",
                            withSpinner(plotlyOutput("spending_dist_plot", height = "400px"), type = 8)
                        )
                    )
                )
            ),
            
            # Combined Dashboard View
            div(class = "row",
                div(class = "col-md-12",
                    div(class = "panel panel-primary",
                        div(class = "panel-heading",
                            h3(class = "panel-title", tags$span(icon("tachometer-alt"), "Combined Dashboard View"))
                        ),
                        div(class = "panel-body",
                            withSpinner(plotOutput("combined_dashboard", height = "800px"), type = 8)
                        )
                    )
                )
            )
        )
      )
    ),
    
    # Tab 3: K-Means Clustering
    tabPanel(
      title = tags$span(icon("users"), "Customer Clustering"),
      value = "clustering",
      conditionalPanel(
        condition = "!output.file_uploaded",
        div(class = "alert alert-warning text-center", style = "margin-top: 50px;",
            h3(icon("exclamation-triangle")), 
            p("Please upload a dataset first in the 'Upload Data' tab.")
        )
      ),
      conditionalPanel(
        condition = "output.file_uploaded",
        div(class = "container-fluid",
            div(class = "page-header",
                h2(tags$span(icon("users"), "Customer Segmentation (K-Means Clustering)"))
            ),
            
            div(class = "row",
                # Controls
                div(class = "col-md-3",
                    div(class = "panel panel-primary",
                        div(class = "panel-heading",
                            h3(class = "panel-title", tags$span(icon("cogs"), "Configuration"))
                        ),
                        div(class = "panel-body",
                            sliderInput("num_clusters", 
                                        label = tags$strong("Number of Clusters:"),
                                        min = 2, max = 4, value = 3, step = 1),
                            
                            helpText("Select the number of customer segments (2-4)."),
                            
                            hr(),
                            
                            h4(tags$strong("Cluster Summary:")),
                            verbatimTextOutput("cluster_summary"),
                            
                            hr(),
                            
                            downloadButton("download_cluster", 
                                           label = tags$span(icon("download"), " Download Cluster Data"),
                                           class = "btn btn-success btn-block")
                        )
                    ),
                    
                    # Algorithm Info
                    div(class = "panel panel-info",
                        div(class = "panel-heading",
                            h3(class = "panel-title", tags$span(icon("info-circle"), "Algorithm"))
                        ),
                        div(class = "panel-body",
                            p(tags$strong("Method:"), " K-Means Clustering"),
                            p(tags$strong("Features Used:"), "Total Spending & Age"),
                            p(tags$strong("Description:"), "Customers are grouped based on their spending patterns and age demographics using the K-Means algorithm.")
                        )
                    )
                ),
                
                # Scatter Plot
                div(class = "col-md-9",
                    div(class = "panel panel-default",
                        div(class = "panel-heading",
                            h3(class = "panel-title", tags$span(icon("chart-scatter"), "Cluster Visualization"))
                        ),
                        div(class = "panel-body",
                            withSpinner(plotlyOutput("cluster_plot", height = "500px"), type = 8)
                        )
                    ),
                    
                    # Cluster Table
                    div(class = "panel panel-default",
                        div(class = "panel-heading",
                            h3(class = "panel-title", tags$span(icon("table"), "Cluster Results Table"))
                        ),
                        div(class = "panel-body",
                            withSpinner(DTOutput("cluster_table"), type = 8)
                        )
                    )
                )
            )
        )
      )
    ),
    
    # Tab 4: Association Rules (Apriori)
    tabPanel(
      title = tags$span(icon("project-diagram"), "Association Rules"),
      value = "association",
      conditionalPanel(
        condition = "!output.file_uploaded",
        div(class = "alert alert-warning text-center", style = "margin-top: 50px;",
            h3(icon("exclamation-triangle")), 
            p("Please upload a dataset first in the 'Upload Data' tab.")
        )
      ),
      conditionalPanel(
        condition = "output.file_uploaded",
        div(class = "container-fluid",
            div(class = "page-header",
                h2(tags$span(icon("project-diagram"), "Association Rules Mining (Apriori Algorithm)"))
            ),
            
            div(class = "row",
                # Controls
                div(class = "col-md-3",
                    div(class = "panel panel-primary",
                        div(class = "panel-heading",
                            h3(class = "panel-title", tags$span(icon="cogs", "Configuration"))
                        ),
                        div(class = "panel-body",
                            sliderInput("min_support", 
                                        label = tags$strong("Minimum Support:"),
                                        min = 0.001, max = 1, value = 0.01, step = 0.001),
                            
                            sliderInput("min_confidence", 
                                        label = tags$strong("Minimum Confidence:"),
                                        min = 0.001, max = 1, value = 0.5, step = 0.001),
                            
                            helpText("Adjust support and confidence thresholds to discover meaningful association rules."),
                            
                            actionButton("run_apriori", 
                                         label = tags$span(icon("play"), " Run Apriori"),
                                         class = "btn btn-primary btn-block",
                                         style = "margin-top: 15px;"),
                            
                            hr(),
                            
                            div(class = "panel panel-info",
                                div(class = "panel-heading",
                                    h3(class = "panel-title", tags$span(icon("info-circle"), "About Apriori"))
                                ),
                                div(class = "panel-body",
                                    p(tags$strong("Algorithm:"), " Apriori"),
                                    p(tags$strong("Support:"), "Frequency of itemset in all transactions."),
                                    p(tags$strong("Confidence:"), "Probability that a rule is correct."),
                                    p(tags$strong("Purpose:"), "Discover relationships between items purchased together.")
                                )
                            )
                        )
                    )
                ),
                
                # Results
                div(class = "col-md-9",
                    div(class = "panel panel-default",
                        div(class = "panel-heading",
                            h3(class = "panel-title", tags$span(icon("list"), "Association Rules"))
                        ),
                        div(class = "panel-body",
                            withSpinner(DTOutput("rules_table"), type = 8)
                        )
                    ),
                    
                    div(class = "panel panel-default",
                        div(class = "panel-heading",
                            h3(class = "panel-title", tags$span(icon("chart-scatter"), "Rules Scatter Plot (Support vs Confidence)"))
                        ),
                        div(class = "panel-body",
                            withSpinner(plotlyOutput("rules_scatter_plot", height = "400px"), type = 8)
                        )
                    ),
                    
                    div(class = "panel panel-default",
                        div(class = "panel-heading",
                            h3(class = "panel-title", tags$span(icon("chart-bar"), "Top Items Frequency"))
                        ),
                        div(class = "panel-body",
                            withSpinner(plotlyOutput("item_freq_plot", height = "400px"), type = 8)
                        )
                    )
                )
            )
        )
      )
    ),
    
    # Tab 5: About / Report
    tabPanel(
      title = tags$span(icon("info-circle"), "About & Report"),
      value = "about",
      div(class = "container",
          div(class = "row",
              div(class = "col-md-8 col-md-offset-2",
                  div(class = "panel panel-default",
                      style = "margin-top: 30px;",
                      div(class = "panel-heading",
                          h2(class = "panel-title text-center", 
                             tags$span(icon("graduation-cap"), " Project Information"))
                      ),
                      div(class = "panel-body",
                          h3(tags$strong("Introduction to Data Science")),
                          h4("Fall 2024 - 2025"),
                          h4("Alexandria University - Faculty of Computers and Science"),
                          
                          hr(),
                          
                          h4(tags$span(icon("file-alt"), " Project Report")),
                          p("Download the full project report (PDF):"),
                          downloadButton("download_report", 
                                         label = tags$span(icon("file-pdf"), " Download Project Report"),
                                         class = "btn btn-danger"),
                          
                          hr(),
                          
                          h4(tags$span(icon("code"), " Technologies Used:")),
                          tags$ul(
                            tags$li(tags$strong("R"), " - Programming language for statistical computing"),
                            tags$li(tags$strong("Shiny"), " - Web application framework for R"),
                            tags$li(tags$strong("ggplot2 & plotly"), " - Data visualization libraries"),
                            tags$li(tags$strong("arules"), " - Association rules mining (Apriori algorithm)"),
                            tags$li(tags$strong("K-Means"), " - Clustering algorithm for customer segmentation")
                          ),
                          
                          hr(),
                          
                          h4(tags$span(icon("tasks"), " Project Features:")),
                          tags$ul(
                            tags$li("CSV file upload with data validation"),
                            tags$li("Data cleaning and preprocessing (duplicates, NA, outliers)"),
                            tags$li("Interactive data visualization dashboard (4 charts)"),
                            tags$li("K-Means clustering with scatter plot and results table"),
                            tags$li("Apriori association rules mining with configurable thresholds"),
                            tags$li("Downloadable cluster data and project report")
                          )
                      )
                  )
              )
          )
      )
    )
  )
)

# Server Definition
server <- function(input, output, session) {
  
  # Reactive value to store processed data
  processed_data <- reactiveVal(NULL)
  cleaning_log <- reactiveVal("")
  file_uploaded <- reactiveVal(FALSE)
  
  # File upload handler
  observeEvent(input$file_upload, {
    req(input$file_upload)
    
    tryCatch({
      # Read the uploaded CSV file
      raw_data <- read.csv(input$file_upload$datapath, header = TRUE, stringsAsFactors = FALSE)
      
      # Process the data
      result <- process_data(raw_data)
      processed_data(result$data)
      cleaning_log(result$log)
      file_uploaded(TRUE)
      
      showNotification("Data uploaded and processed successfully!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
      file_uploaded(FALSE)
    })
  })
  
  # Output: file uploaded status
  output$file_uploaded <- reactive({
    file_uploaded()
  })
  outputOptions(output, "file_uploaded", suspendWhenHidden = FALSE)
  
  # Output: file info
  output$file_info <- renderText({
    req(input$file_upload)
    paste0("File: ", input$file_upload$name, 
           " | Size: ", round(input$file_upload$size / 1024, 2), " KB")
  })
  
  # Output: data preview table
  output$data_preview <- renderDT({
    req(processed_data())
    datatable(head(processed_data(), 10), 
              options = list(pageLength = 10, 
                             scrollX = TRUE,
                             dom = 't'),
              class = 'cell-border stripe hover')
  })
  
  # Output: summary statistics cards
  output$total_rows <- renderText({
    req(processed_data())
    nrow(processed_data())
  })
  
  output$total_customers <- renderText({
    req(processed_data())
    length(unique(processed_data()$customer))
  })
  
  output$total_cities <- renderText({
    req(processed_data())
    length(unique(processed_data()$city))
  })
  
  output$total_revenue <- renderText({
    req(processed_data())
    paste0("$", format(round(sum(processed_data()$total), 2), big.mark = ","))
  })
  
  # Output: cleaning info
  output$cleaning_info <- renderPrint({
    req(cleaning_log())
    cat(cleaning_log())
  })
  
  # ===== VISUALIZATION TAB =====
  
  # Plot 1: Cash vs Credit
  output$payment_plot <- renderPlotly({
    req(processed_data())
    create_payment_plot(processed_data())
  })
  
  # Plot 2: Age vs Spending
  output$age_spending_plot <- renderPlotly({
    req(processed_data())
    create_age_spending_plot(processed_data())
  })
  
  # Plot 3: City Spending
  output$city_spending_plot <- renderPlotly({
    req(processed_data())
    create_city_spending_plot(processed_data())
  })
  
  # Plot 4: Spending Distribution
  output$spending_dist_plot <- renderPlotly({
    req(processed_data())
    create_spending_dist_plot(processed_data())
  })
  
  # Combined Dashboard
  output$combined_dashboard <- renderPlot({
    req(processed_data())
    create_combined_dashboard(processed_data())
  })
  
  # ===== CLUSTERING TAB =====
  
  # Perform clustering
  cluster_results <- reactive({
    req(processed_data())
    req(input$num_clusters)
    perform_kmeans(processed_data(), input$num_clusters)
  })
  
  # Output: cluster plot
  output$cluster_plot <- renderPlotly({
    req(cluster_results())
    create_cluster_plot(cluster_results())
  })
  
  # Output: cluster table
  output$cluster_table <- renderDT({
    req(cluster_results())
    datatable(cluster_results(),
              options = list(pageLength = 15, scrollX = TRUE),
              class = 'cell-border stripe hover') %>%
      formatRound(columns = c("total"), digits = 2)
  })
  
  # Output: cluster summary
  output$cluster_summary <- renderPrint({
    req(cluster_results())
    req(input$num_clusters)
    
    clusters <- cluster_results()$cluster
    cat("Number of Clusters:", input$num_clusters, "\n")
    cat("Customers per Cluster:\n")
    print(table(clusters))
  })
  
  # Download: cluster data
  output$download_cluster <- downloadHandler(
    filename = function() {
      paste0("cluster_results_k", input$num_clusters, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(cluster_results(), file, row.names = FALSE)
    }
  )
  
  # ===== ASSOCIATION RULES TAB =====
  
  # Run Apriori
  apriori_results <- eventReactive(input$run_apriori, {
    req(processed_data())
    req(input$min_support)
    req(input$min_confidence)
    
    withProgress(message = 'Running Apriori Algorithm...', {
      result <- run_apriori(processed_data(), input$min_support, input$min_confidence)
      result
    })
  })
  
  # Output: rules table
  output$rules_table <- renderDT({
    req(apriori_results())
    
    rules_df <- apriori_results()$rules_df
    if (nrow(rules_df) == 0) {
      showNotification("No rules found. Try lowering support/confidence thresholds.", type = "warning")
    }
    
    datatable(rules_df,
              options = list(pageLength = 10, scrollX = TRUE),
              class = 'cell-border stripe hover') %>%
      formatRound(columns = c("support", "confidence", "coverage", "lift", "count"), digits = 4)
  })
  
  # Output: rules scatter plot
  output$rules_scatter_plot <- renderPlotly({
    req(apriori_results())
    create_rules_scatter_plot(apriori_results()$rules_df)
  })
  
  # Output: item frequency plot
  output$item_freq_plot <- renderPlotly({
    req(processed_data())
    create_item_freq_plot(processed_data())
  })
  
  # ===== ABOUT / REPORT TAB =====
  
  # Download: project report
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("Project_Report_Group_X_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      # Generate the report using RMarkdown
      temp_report <- file.path(tempdir(), "report.Rmd")
      file.copy("report/Project_Report.Rmd", temp_report, overwrite = TRUE)
      
      params <- list(data = processed_data())
      
      rmarkdown::render(temp_report, 
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)