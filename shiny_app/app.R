library(shiny)
library(DT)
library(shinyjs)
library(dplyr)
library(htmltools)

# Define the UI
ui <- fluidPage(
  useShinyjs(),  # Enables shinyjs functionalities
  titlePanel("MaxEnt with Hidden Structure in R"),
  sidebarLayout(
    sidebarPanel(
      # Input for uploading a file
      fileInput("fileInput", "Choose Input File", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      # Download link for sample input files
      tags$div(style = "margin-top: -35px;",  # Adjust the margin to control spacing
               downloadLink("downloadSampleFiles", "Sample Input Files")
      ),
      tags$br(),
      # Radio buttons for selecting input type
      radioButtons("frequencyType", "Select Input Type:", choices = c("Probabilities" = "prob", "Raw Frequencies" = "raw"), selected = "prob"),
      # Radio buttons for selecting normalization type
      radioButtons("normalizationType", "Select Normalization:", choices = c("Within Tableau" = "within", "Global Normalization" = "global"), selected = "within"),
      # Radio buttons for selecting prior type
      radioButtons("priorType", "Select Prior:", choices = c("L2" = "L2", "L1" = "L1"), selected = "L2"),
      # Button to generate grammar
      actionButton("generateBtn", "Generate Grammar"),
      # Button to download output
      downloadButton("downloadOutput", "Download Output", style = "display: none;"),
      # Placeholder for dynamic UI elements for updating weights
      uiOutput("updateWeightsUI"),
      # Button to load more constraints (initially hidden)
      actionButton("loadMoreBtn", "Load More Constraints", style = "display: none;"),
      # Button to update grammar (initially hidden)
      actionButton("updateGrammarBtn", "Update Grammar", style = "display: none;")
    ),
    mainPanel(
      # Output table to display the grammar
      DTOutput("grammarTable"),
      # Text output for additional information
      textOutput("infoText")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  # Reactive values to store various data
  grammarData <- reactiveVal(NULL)
  unroundedData <- reactiveVal(NULL)
  constraints <- reactiveVal(NULL)
  weights <- reactiveVal(NULL)
  stored_output_df <- reactiveVal(NULL)
  # Counter to keep track of the number of loaded constraints
  loadedConstraints <- reactiveVal(10)
  
  # Observe event when 'Generate Grammar' button is clicked
  observeEvent(input$generateBtn, {
    inFile <- input$fileInput
    if (is.null(inFile)) {
      showModal(modalDialog(
        title = "Error",
        "No file was uploaded. Please upload a file.",
        easyClose = TRUE,
        footer = NULL
      ))
      return(NULL)
    }
    
    fileExt <- tools::file_ext(inFile$name)
    if (!(fileExt %in% c("txt", "csv"))) {
      showModal(modalDialog(
        title = "Error",
        "The uploaded input file is not a txt or csv file. Please upload a txt or csv file.",
        easyClose = TRUE,
        footer = NULL
      ))
      return(NULL)
    }
    
    source("solver.R")  # Load external R script for solving
    tab <- read.table(inFile$datapath, header = TRUE, sep = "\t", check.names = FALSE)
    hidden_structure <- "hidden" %in% colnames(tab)  # Check if hidden structure is included
    
    # Offset to determine where constraints start in the data
    offset <- if (hidden_structure) 4 else 3
    w <- rep(0, ncol(tab) - offset)  # Initial weights for constraints
    tab[, -(1:offset)] <- -tab[, -(1:offset)]  # Negate constraints for MaxEnt calculation
    
    # Calculate observed probabilities based on user input
    if (input$frequencyType == "raw" && input$normalizationType == "global") {
      observed_probability <- tab[[offset]] / sum(tab[[offset]])
    } else if (input$frequencyType == "raw" && input$normalizationType == "within") {
      observed_probability <- tab %>%
        group_by(input) %>%
        mutate(observed_probability = probability / sum(probability)) %>%
        ungroup() %>%
        .$observed_probability
    } else {
      observed_probability <- tab[[offset]]
      if (any(observed_probability > 1)) {
        observed_probability <- observed_probability / 100
      }
    }
    
    # Create a new Tableau object
    o.tab <- new("Tableau", data = tab, hidden = hidden_structure, theory = "maxent")
    
    # Solve for optimal weights
    if (input$normalizationType == "global") {
      opt <- solve_global_norm(o.tab, w, method = "L-BFGS-B", reg = input$priorType, lower.bound = 0, upper.bound = Inf, observed_probs = observed_probability)
      harmony <- apply(tab[, (offset + 1):ncol(tab)], 1, function(x) sum(x * opt$par))
      expected_probability <- opt$final_probabilities
    } else {
      opt <- solve(o.tab, w, categorical = hidden_structure, method = "L-BFGS-B", reg = input$priorType, var = 1000000, mf = FALSE)
      harmony <- apply(tab[, (offset + 1):ncol(tab)], 1, function(x) sum(x * opt$par))
    }
    
    # Round the weights for display
    rounded_weights <- round(opt$par, 2)
    constraints(colnames(tab)[-(1:offset)])  # Store constraint names
    weights(rounded_weights)  # Store rounded weights
    
    # Prepare the output data frame
    output_df <- tab[, 1:offset]
    
    # Adjust the data frame based on frequency type
    if (input$frequencyType == "prob") {
      if (hidden_structure) {
        output_df <- output_df[, -4]
      } else {
        output_df <- output_df[, -3]
      }
    }
    
    output_df$observed_probability <- round(observed_probability, 5)
    output_df <- cbind(output_df, tab[, (offset + 1):ncol(tab)])
    
    output_df$harmony <- round(harmony, 5)
    if (input$normalizationType == "global") {
      output_df$expected_probability <- round(expected_probability, 5)
    } else {
      if (hidden_structure) {
        distribution_df <- hidden.distribution(o.tab, opt$par)
      } else {
        distribution_df <- distribution(o.tab, opt$par)
      }
      output_df$expected_probability <- round(distribution_df$probability, 5)
    }
    expected_probability <- output_df$expected_probability
    
    # Calculate error metrics
    output_df$AbsoluteError <- abs(observed_probability - expected_probability)
    output_df <- output_df %>%
      group_by(input) %>%
      mutate(MAE_per_tableau = mean(abs(observed_probability - expected_probability))) %>%
      ungroup()
    MAE_overall <- mean(abs(observed_probability - expected_probability))
    output_df$MAE_overall <- MAE_overall
    
    # Round error metrics for display
    output_df$AbsoluteError <- round(output_df$AbsoluteError, 5)
    output_df$MAE_per_tableau <- round(output_df$MAE_per_tableau, 5)
    output_df$MAE_overall <- round(output_df$MAE_overall, 5)
    
    # Store unrounded data for download
    unrounded_output_df <- output_df
    unrounded_output_df$observed_probability <- observed_probability
    unrounded_output_df$harmony <- harmony
    unrounded_output_df$expected_probability <- expected_probability
    unrounded_output_df$AbsoluteError <- abs(observed_probability - expected_probability)
    unrounded_output_df <- unrounded_output_df %>%
      group_by(input) %>%
      mutate(MAE_per_tableau = mean(abs(observed_probability - expected_probability))) %>%
      ungroup()
    unrounded_output_df$MAE_overall <- MAE_overall
    
    # Prepare weights row for display
    observed_prob_index <- which(names(output_df) == "observed_probability")
    if (input$frequencyType == "raw") {
      weight_row <- c(rep(NA, (offset+1)), rounded_weights)
      colnames(output_df)[observed_prob_index - 1] <- "raw_frequency"
      colnames(unrounded_output_df)[observed_prob_index - 1] <- "raw_frequency"
    } else {
      weight_row <- c(rep(NA, (offset)), rounded_weights)
    }
    weight_row <- c(weight_row, rep(NA, ncol(output_df) - length(weight_row)))
    weight_row <- as.data.frame(t(weight_row))
    colnames(weight_row) <- colnames(output_df)
    
    output_df <- rbind(weight_row, output_df)
    
    weight_row_unrounded <- c(rep(NA, (offset + 1)), opt$par)
    weight_row_unrounded <- c(weight_row_unrounded, rep(NA, ncol(unrounded_output_df) - length(weight_row_unrounded)))
    weight_row_unrounded <- as.data.frame(t(weight_row_unrounded))
    colnames(weight_row_unrounded) <- colnames(unrounded_output_df)
    
    unrounded_output_df <- rbind(weight_row_unrounded, unrounded_output_df)
    
    # Update reactive values
    grammarData(output_df)
    unroundedData(unrounded_output_df)
    stored_output_df(output_df)
    
    shinyjs::show("downloadOutput")
    
    # Initially load the first 10 constraints
    loadedConstraints(10)
    
    # Render dynamic UI for updating weights
    updateUI <- renderDynamicConstraints()
    output$updateWeightsUI <- renderUI(updateUI)
  })
  
  # Observe event when 'Load More Constraints' button is clicked
  observeEvent(input$loadMoreBtn, {
    currentCount <- loadedConstraints()
    loadedConstraints(currentCount + 10)
    
    updateUI <- renderDynamicConstraints()
    output$updateWeightsUI <- renderUI(updateUI)
  })
  
  # Reactive function to render dynamic constraints
  renderDynamicConstraints <- reactive({
    constraints_to_load <- head(constraints(), loadedConstraints())
    
    ui_elements <- lapply(seq_along(constraints_to_load), function(i) {
      constraint_name <- constraints_to_load[i]
      fluidRow(
        column(12, numericInput(paste0("weight_", i), constraint_name, value = weights()[i]))
      )
    })
    
    ui_elements <- c(
      list(h3("Update Weights")),  # Add the heading here
      ui_elements,
      list(actionButton("updateGrammarBtn", "Update Grammar"))
    )
    
    # Conditionally include the "Load More Constraints" button
    if (length(constraints()) > loadedConstraints()) {
      ui_elements <- c(ui_elements, list(actionButton("loadMoreBtn", "Load More Constraints")))
    }
    
    tagList(ui_elements)
  })
  
  # Observe event when 'Update Grammar' button is clicked
  observeEvent(input$updateGrammarBtn, {
    output_df <- stored_output_df()
    if (is.null(output_df)) {
      showModal(modalDialog(
        title = "Error",
        "No grammar has been generated. Please generate grammar first.",
        easyClose = TRUE,
        footer = NULL
      ))
      return(NULL)
    }
    
    hidden_structure <- "hidden" %in% colnames(output_df)
    offset <- if (hidden_structure) 5 else 4
    
    new_weights <- sapply(1:length(weights()), function(i) input[[paste0("weight_", i)]])
    
    # Update weights and recalculate harmony
    if (hidden_structure && input$frequencyType == "raw") {
      output_df[1, (offset + 1):(offset + length(new_weights))] <- round(new_weights, 2)
      tab <- output_df[2:nrow(output_df), c(1:(offset), (offset + 1):(offset + length(new_weights)))]
      harmony <- rowSums(tab[, (offset + 1):ncol(tab)] * matrix(new_weights, nrow = nrow(tab), ncol = length(new_weights), byrow = TRUE))
    } else if (!hidden_structure && input$frequencyType == "raw") {
      output_df[1, (offset + 1):(offset + length(new_weights))] <- round(new_weights, 2)
      tab <- output_df[2:nrow(output_df), 1:(offset + length(new_weights))]
      harmony <- rowSums(tab[, (offset + 1):ncol(tab)] * matrix(new_weights, nrow = nrow(tab), ncol = length(new_weights), byrow = TRUE))
    } else if (hidden_structure || input$frequencyType == "prob") {
      output_df[1, (offset):(offset + length(new_weights) - 1)] <- round(new_weights, 2)
      tab <- output_df[2:nrow(output_df), c(1:(offset - 1), (offset):(offset + length(new_weights) - 1))]
      harmony <- rowSums(tab[, (offset):(offset + length(new_weights) - 1)] * matrix(new_weights, nrow = nrow(tab), ncol = length(new_weights), byrow = TRUE))
    }
    
    # Calculate expected probabilities
    if (input$normalizationType == "global") {
      scores <- exp(c(harmony))
      total_score <- sum(scores)
      expected_probability <- scores / total_score
    } else if (input$normalizationType == "within") {
      expected_probability <- numeric(length = length(harmony))
      unique_inputs <- unique(output_df$input[2:nrow(output_df)])
      for (input_val in unique_inputs) {
        input_indices <- which(output_df$input[2:nrow(output_df)] == input_val) + 1
        scores <- exp(c(harmony[input_indices - 1]))
        total_score <- sum(scores)
        expected_probability[input_indices - 1] <- scores / total_score
      }
    }
    
    # Update the output data frame with new values
    output_df$harmony[2:nrow(output_df)] <- round(harmony, 5)
    output_df$expected_probability[2:nrow(output_df)] <- round(expected_probability, 5)
    observed_probability <- output_df$observed_probability[2:nrow(output_df)]
    new_AE <- abs(observed_probability - expected_probability)
    
    # Calculate MAE per tableau
    mae_per_tableau_df <- output_df[2:nrow(output_df),] %>%
      group_by(input) %>%
      summarise(MAE_per_tableau = mean(abs(observed_probability - expected_probability))) %>%
      ungroup()
    
    for (i in 2:nrow(output_df)) {
      output_df$MAE_per_tableau[i] <- mae_per_tableau_df$MAE_per_tableau[mae_per_tableau_df$input == output_df$input[i]]
    }
    
    new_MAE_overall <- mean(new_AE)
    
    for (i in 2:nrow(output_df)) {
      output_df$AbsoluteError[i] <- round(new_AE[i - 1], 5)
    }
    
    for (i in 2:nrow(output_df)) {
      output_df$MAE_overall[i] <- round(new_MAE_overall, 5)
    }
    
    # Update reactive values
    grammarData(output_df)
    stored_output_df(output_df)
    
    unrounded_output_df <- unroundedData()
    
    tryCatch({
      unrounded_output_df$harmony[2:nrow(unrounded_output_df)] <- harmony
      unrounded_output_df$expected_probability[2:nrow(unrounded_output_df)] <- expected_probability
      unrounded_output_df$AbsoluteError[2:nrow(unrounded_output_df)] <- new_AE
      
      for (i in 2:nrow(unrounded_output_df)) {
        unrounded_output_df$MAE_per_tableau[i] <- mae_per_tableau_df$MAE_per_tableau[mae_per_tableau_df$input == output_df$input[i]]
      }
      
      unrounded_output_df$MAE_overall[2:nrow(unrounded_output_df)] <- new_MAE_overall
      unroundedData(unrounded_output_df)
    }, error = function(e) {
      print(paste("Error occurred:", e$message))
    })
  })
  
  # Render the grammar table
  output$grammarTable <- renderDT({
    data <- grammarData()
    if (is.null(data) || nrow(data) == 0) {
      return(NULL)
    }
    datatable(data, editable = TRUE, options = list(pageLength = 25), escape = FALSE) %>%
      formatStyle(names(data), 'white-space' = 'nowrap') %>%
      formatStyle(columns = colnames(data), escape = FALSE, formatType = "html")
  }, server = TRUE)
  
  # Download handler for the output tableau
  output$downloadOutput <- downloadHandler(
    filename = function() {
      paste("output-tableau-", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      data <- stored_output_df()
      if (is.null(data)) return()
      write.table(data, file, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE, na = "NA", fileEncoding = "UTF-8")
    },
    contentType = "text/plain"
  )
  
  # Download handler for the sample input files
  output$downloadSampleFiles <- downloadHandler(
    filename = function() {
      "sample_input_files.zip"
    },
    content = function(file) {
      file.copy("sample_input_files.zip", file)
    },
    contentType = "application/zip"
  )
}

# Run the app
shinyApp(ui = ui, server = server)