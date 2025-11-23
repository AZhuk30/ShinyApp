# Australian Wine Sales Forecasting Shiny App
# Load required libraries
library(shiny)
library(tidyverse)
library(fpp3)
library(plotly)
library(DT)
library(bslib)

# Check for urca package (needed for ARIMA unit root tests)
if (!requireNamespace("urca", quietly = TRUE)) {
  message("Installing required package: urca")
  install.packages("urca")
}
library(urca)

# UI Definition
ui <- page_sidebar(
  title = "Australian Wine Sales Forecasting",
  theme = bs_theme(
    bootswatch = "cosmo",
    primary = "#7C3AED"
  ),
  
  sidebar = sidebar(
    width = 300,
    
    # Data Selection
    h4("Data Selection"),
    checkboxGroupInput(
      "varietals",
      "Select Wine Varietals:",
      choices = c("Fortified", "Red", "Rose", "sparkling", 
                  "Sweet white", "Dry white"),
      selected = c("Fortified", "Red")
    ),
    
    dateRangeInput(
      "date_range",
      "Date Range:",
      start = "1980-01-01",
      end = "1994-12-01",
      min = "1980-01-01",
      max = "1994-12-01",
      format = "yyyy-mm"
    ),
    
    hr(),
    
    # Model Configuration
    h4("Model Configuration"),
    sliderInput(
      "train_pct",
      "Training Data %:",
      min = 60,
      max = 95,
      value = 85,
      step = 5
    ),
    
    sliderInput(
      "forecast_horizon",
      "Forecast Horizon (months):",
      min = 6,
      max = 24,
      value = 12,
      step = 6
    ),
    
    checkboxGroupInput(
      "models",
      "Select Models:",
      choices = c("TSLM", "ETS", "ARIMA"),
      selected = c("TSLM", "ETS", "ARIMA")
    ),
    
    checkboxInput(
      "log_transform",
      "Apply Log Transformation",
      value = FALSE
    ),
    
    hr(),
    
    actionButton(
      "run_models",
      "Run Forecast Models",
      class = "btn-primary",
      width = "100%"
    )
  ),
  
  # Main Panel with Tabs
  navset_card_tab(
    nav_panel(
      "Overview",
      layout_columns(
        card(
          card_header("Time Series Data"),
          plotlyOutput("overview_plot", height = "400px")
        ),
        card(
          card_header("Seasonality Analysis"),
          plotlyOutput("seasonal_plot", height = "400px")
        )
      ),
      card(
        card_header("Data Summary Statistics"),
        DTOutput("data_summary")
      )
    ),
    
    nav_panel(
      "Model Results",
      card(
        card_header("Model Specifications"),
        DTOutput("model_specs")
      ),
      card(
        card_header("Accuracy Metrics"),
        DTOutput("accuracy_table")
      )
    ),
    
    nav_panel(
      "Forecasts",
      card(
        card_header("Comparative Forecasts"),
        plotlyOutput("forecast_plot", height = "500px")
      ),
      layout_columns(
        card(
          card_header("Forecast Values"),
          DTOutput("forecast_values")
        ),
        card(
          card_header("Best Model by RMSE"),
          uiOutput("best_model_summary")
        )
      )
    ),
    
    nav_panel(
      "Diagnostics",
      layout_columns(
        card(
          card_header("Residual Plots"),
          plotOutput("residual_plots", height = "400px")
        ),
        card(
          card_header("ACF/PACF"),
          plotOutput("acf_plots", height = "400px")
        )
      ),
      card(
        card_header("Ljung-Box Test Results"),
        DTOutput("ljung_box_table")
      )
    ),
    
    nav_panel(
      "About",
      card(
        card_header("About This Application"),
        markdown("
### Australian Wine Sales Forecasting Dashboard

**Purpose:** This interactive application enables comprehensive time series forecasting 
for Australian wine sales data (1980-1994) across multiple varietals.

**Features:**
- **Multi-Varietal Analysis:** Compare up to 6 wine types simultaneously
- **Flexible Model Selection:** TSLM, ETS, and ARIMA with auto-specification
- **Interactive Controls:** Adjustable training splits and forecast horizons
- **Data Transformation:** Optional log transformation for variance stabilization
- **Comprehensive Metrics:** Training and validation accuracy (RMSE, MAE, MAPE)
- **Model Specifications:** Detailed ETS components and ARIMA orders including seasonal components
- **Visual Diagnostics:** Residual analysis, ACF/PACF plots, and statistical tests
- **Comparative Forecasts:** Side-by-side model performance with prediction intervals
- **Professional Visualizations:** Interactive Plotly charts with zoom and hover capabilities

**Models:**
- **TSLM:** Time Series Linear Model with trend and seasonal components
- **ETS:** Exponential Smoothing State Space Models (auto-selected)
- **ARIMA:** AutoRegressive Integrated Moving Average (auto-selected, including SARIMA)

**Data Source:** Monthly sales data (thousands of liters) from 1980-1994

**Technical Details:**
- Built with R Shiny and fpp3 (tidyverts ecosystem)
- Implements holdout validation with configurable splits
- Provides 80% and 95% prediction intervals
- Requires: tidyverse, fpp3, plotly, DT, bslib, urca packages

**Author:** Alexander Zhuk  
**Course:** ADS-506 Time Series Analysis  
**Institution:** University of San Diego
        ")
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  
  # Load and prepare data
  wine_data <- reactive({
    req(input$varietals)
    
    # Read data
    aus_wine <- read_csv("AustralianWines.csv", 
                         na = "*",
                         col_types = cols(Rose = col_number()),
                         show_col_types = FALSE) %>%
      fill(Rose, .direction = "down") %>%
      mutate(Month = mdy(str_replace(Month, '-', '-01-')) %>% yearmonth())
    
    # Convert to long format and create tsibble
    wine_ts <- aus_wine %>%
      pivot_longer(cols = -Month, names_to = 'Varietal', values_to = 'Sales') %>%
      as_tsibble(index = Month, key = Varietal)
    
    # Filter based on selections
    wine_ts %>%
      filter(
        Varietal %in% input$varietals,
        Month >= yearmonth(input$date_range[1]),
        Month <= yearmonth(input$date_range[2])
      )
  })
  
  # Create train/test split
  split_data <- reactive({
    req(wine_data())
    
    data <- wine_data()
    
    # Calculate split point per varietal
    split_info <- data %>%
      as_tibble() %>%
      group_by(Varietal) %>%
      summarise(
        n_total = n(),
        n_train = floor(n_total * input$train_pct / 100),
        .groups = "drop"
      )
    
    # Create train and test sets by filtering based on row numbers
    train_list <- list()
    test_list <- list()
    
    for (v in unique(data$Varietal)) {
      v_data <- data %>% filter(Varietal == v)
      n_total <- nrow(v_data)
      n_train <- floor(n_total * input$train_pct / 100)
      
      # Split by row index
      train_list[[v]] <- v_data %>% slice(1:n_train)
      test_list[[v]] <- v_data %>% slice((n_train + 1):n_total)
    }
    
    # Combine back into tsibbles
    train <- bind_rows(train_list) %>%
      as_tsibble(index = Month, key = Varietal)
    
    test <- bind_rows(test_list) %>%
      as_tsibble(index = Month, key = Varietal)
    
    # Get split date
    split_date <- train %>%
      as_tibble() %>%
      group_by(Varietal) %>%
      slice_tail(n = 1) %>%
      pull(Month) %>%
      max()
    
    list(
      train = train,
      test = test,
      split_date = split_date
    )
  })
  
  # Fit models
  fitted_models <- eventReactive(input$run_models, {
    req(input$models, split_data())
    req(length(input$models) > 0)  # Ensure at least one model is selected
    
    withProgress(message = "Fitting models...", value = 0, {
      
      train_data <- split_data()$train
      
      # Apply transformation if selected
      if (input$log_transform) {
        train_data <- train_data %>%
          mutate(Sales_transformed = log(Sales))
      } else {
        train_data <- train_data %>%
          mutate(Sales_transformed = Sales)
      }
      
      # Build model list dynamically
      models_list <- list()
      n_models <- length(input$models)
      progress_step <- 1 / n_models
      
      if ("TSLM" %in% input$models) {
        incProgress(progress_step, detail = "Fitting TSLM...")
        models_list$TSLM <- TSLM(Sales_transformed ~ trend() + season())
      }
      
      if ("ETS" %in% input$models) {
        incProgress(progress_step, detail = "Fitting ETS...")
        models_list$ETS <- ETS(Sales_transformed)
      }
      
      if ("ARIMA" %in% input$models) {
        incProgress(progress_step, detail = "Fitting ARIMA...")
        models_list$ARIMA <- ARIMA(Sales_transformed)
      }
      
      # Validate that we have at least one model
      if (length(models_list) == 0) {
        showNotification("Please select at least one model", type = "error")
        return(NULL)
      }
      
      # Fit models
      result <- train_data %>%
        model(!!!models_list)
      
      result
    })
  })
  
  # Generate forecasts
  forecasts <- reactive({
    req(fitted_models())
    
    # Check if fitted_models is NULL or empty
    if (is.null(fitted_models())) {
      return(NULL)
    }
    
    fc <- fitted_models() %>%
      forecast(h = input$forecast_horizon)
    
    # Back-transform if log was applied
    if (input$log_transform) {
      fc <- fc %>%
        mutate(
          .mean = exp(.mean),
          Sales_transformed = exp(Sales_transformed)
        )
      
      # Also need to transform distribution
      fc <- fc %>%
        mutate(
          Sales_transformed = distributional::dist_transformed(
            Sales_transformed, 
            transform = exp, 
            inverse = log
          )
        )
    }
    
    fc
  })
  
  # Calculate accuracy metrics
  accuracy_metrics <- reactive({
    # Don't require - return empty if NULL
    if (is.null(fitted_models()) || is.null(split_data())) {
      return(tibble(
        Varietal = character(),
        .model = character(),
        Set = character(),
        RMSE = numeric(),
        MAE = numeric(),
        MAPE = numeric()
      ))
    }
    
    tryCatch({
      # Get test data with same transformation
      test_data <- split_data()$test
      if (input$log_transform) {
        test_data <- test_data %>%
          mutate(Sales_transformed = log(Sales))
      } else {
        test_data <- test_data %>%
          mutate(Sales_transformed = Sales)
      }
      
      # Training accuracy (on transformed scale)
      train_acc <- fitted_models() %>%
        accuracy() %>%
        mutate(Set = "Training")
      
      # Calculate max test length across varietals
      max_test_length <- test_data %>%
        as_tibble() %>%
        group_by(Varietal) %>%
        summarise(n = n(), .groups = "drop") %>%
        pull(n) %>%
        max()
      
      # Validation accuracy (on transformed scale)
      val_acc <- tryCatch({
        fitted_models() %>%
          forecast(h = max_test_length) %>%
          accuracy(test_data) %>%
          mutate(Set = "Validation")
      }, error = function(e) {
        message("Error in validation accuracy: ", e$message)
        tibble(
          Varietal = character(),
          .model = character(),
          Set = character(),
          RMSE = numeric(),
          MAE = numeric(),
          MAPE = numeric()
        )
      })
      
      # Combine results
      result <- bind_rows(train_acc, val_acc) %>%
        select(Varietal, .model, Set, RMSE, MAE, MAPE) %>%
        arrange(Varietal, .model, Set)
      
      result
    }, error = function(e) {
      message("Error in accuracy_metrics: ", e$message)
      tibble(
        Varietal = character(),
        .model = character(),
        Set = character(),
        RMSE = numeric(),
        MAE = numeric(),
        MAPE = numeric()
      )
    })
  })
  
  # Extract model specifications
  model_specs <- reactive({
    # Don't require - just return empty if NULL
    if (is.null(fitted_models())) {
      return(NULL)
    }
    
    tryCatch({
      # Get the fitted models
      models_mable <- fitted_models()
      
      # Get model names (columns except Varietal)
      model_names <- names(models_mable)[names(models_mable) != "Varietal"]
      
      # Extract specs for each varietal and model
      all_specs <- list()
      
      for (v in unique(models_mable$Varietal)) {
        for (m in model_names) {
          # Get the model for this varietal
          model_row <- models_mable %>%
            filter(Varietal == v)
          
          # Extract the model from the mable
          model_list <- model_row[[m]]
          
          if (length(model_list) > 0 && !is.null(model_list[[1]])) {
            mdl <- model_list[[1]]
            
            # Get all classes of the model
            mdl_classes <- class(mdl)
            
            spec <- tryCatch({
              # First, always try to extract from report output regardless of class
              mdl_report <- capture.output(report(mdl))
              mdl_str <- paste(mdl_report, collapse = " ")
              
              # Debug: print to console
              message("=== Model report for ", v, " - ", m, " ===")
              message(mdl_str)
              
              # Check for ARIMA in report (look for "Model:" line which has the spec)
              if (str_detect(mdl_str, "ARIMA")) {
                # Look for the Model: line specifically
                model_line <- mdl_report[str_detect(mdl_report, "Model:")]
                
                if (length(model_line) > 0) {
                  model_str <- paste(model_line, collapse = " ")
                  
                  # Try multiple ARIMA patterns
                  arima_match <- str_extract(
                    model_str,
                    "ARIMA\\([0-9]+,[0-9]+,[0-9]+\\)(\\([0-9]+,[0-9]+,[0-9]+\\)\\[\\d+\\])?(\\s+w/\\s+(drift|mean))?"
                  )
                  
                  if (!is.na(arima_match)) {
                    arima_match
                  } else {
                    # Maybe it's NULL model or something else
                    if (str_detect(model_str, "NULL")) {
                      "ARIMA: No suitable model found"
                    } else {
                      # Return what we found on Model: line
                      trimws(str_remove(model_str, "Model:"))
                    }
                  }
                } else {
                  "ARIMA: Model line not found"
                }
                
                # Check for ETS in report
              } else if (str_detect(mdl_str, "ETS")) {
                # Look for the Model: line
                model_line <- mdl_report[str_detect(mdl_report, "Model:")]
                if (length(model_line) > 0) {
                  model_str <- paste(model_line, collapse = " ")
                  ets_match <- str_extract(model_str, "ETS\\([A-Z,N]+\\)")
                } else {
                  ets_match <- str_extract(mdl_str, "ETS\\([A-Z,N]+\\)")
                }
                
                if (!is.na(ets_match)) {
                  ets_match
                } else {
                  "ETS (specification not extracted)"
                }
                
                # Check for TSLM
              } else if (str_detect(mdl_str, "TSLM") || str_detect(mdl_str, "linear model")) {
                "TSLM: Sales ~ trend() + season()"
                
              } else {
                # Unknown - show what we got from report
                if (length(mdl_report) > 1) {
                  # Show second line which usually has Model:
                  line2 <- trimws(mdl_report[2])
                  if (nchar(line2) > 0) {
                    substr(line2, 1, 60)
                  } else {
                    "Model specification not found"
                  }
                } else {
                  "Model type unknown"
                }
              }
            }, error = function(e) {
              paste0("Error: ", substr(e$message, 1, 30))
            })
            
            all_specs[[length(all_specs) + 1]] <- tibble(
              Varietal = v,
              Model = m,
              Specification = spec,
              Transformation = ifelse(input$log_transform, "Log", "None")
            )
          }
        }
      }
      
      # Combine all specs
      if (length(all_specs) > 0) {
        result <- bind_rows(all_specs)
        result
      } else {
        NULL
      }
      
    }, error = function(e) {
      message("Error in model_specs: ", e$message)
      NULL
    })
  })
  
  # Overview plot
  output$overview_plot <- renderPlotly({
    req(wine_data())
    
    p <- wine_data() %>%
      ggplot(aes(x = Month, y = Sales, color = Varietal)) +
      geom_line() +
      labs(
        title = "Wine Sales Over Time",
        x = "Month",
        y = "Sales (thousands of liters)"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p, height = 400)
  })
  
  # Seasonal plot
  output$seasonal_plot <- renderPlotly({
    req(wine_data())
    
    p <- wine_data() %>%
      mutate(Year = year(Month), Month_num = month(Month)) %>%
      ggplot(aes(x = Month_num, y = Sales, color = factor(Year), group = Year)) +
      geom_line(alpha = 0.6) +
      facet_wrap(~ Varietal, scales = "free_y") +
      scale_x_continuous(breaks = 1:12, labels = month.abb) +
      labs(
        title = "Seasonal Patterns by Year",
        x = "Month",
        y = "Sales",
        color = "Year"
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    ggplotly(p, height = 400)
  })
  
  # Data summary
  output$data_summary <- renderDT({
    req(wine_data())
    
    wine_data() %>%
      as_tibble() %>%
      group_by(Varietal) %>%
      summarise(
        Observations = n(),
        `Mean Sales` = round(mean(Sales, na.rm = TRUE), 1),
        `Std Dev` = round(sd(Sales, na.rm = TRUE), 1),
        Min = round(min(Sales, na.rm = TRUE), 1),
        Max = round(max(Sales, na.rm = TRUE), 1),
        `CV (%)` = round(100 * `Std Dev` / `Mean Sales`, 1),
        .groups = "drop"
      ) %>%
      datatable(
        options = list(pageLength = 10, dom = 't'),
        rownames = FALSE
      )
  })
  
  # Model specifications table
  output$model_specs <- renderDT({
    # Check if models have been fitted
    if (is.null(fitted_models())) {
      # Return empty table with message
      return(
        datatable(
          tibble(Message = "Click 'Run Forecast Models' to see specifications"),
          options = list(pageLength = 15, dom = 't'),
          rownames = FALSE
        )
      )
    }
    
    # Get specs
    specs <- model_specs()
    
    # Debug: check what we got
    if (is.null(specs)) {
      return(
        datatable(
          tibble(Message = "Extracting specifications..."),
          options = list(pageLength = 15, dom = 't'),
          rownames = FALSE
        )
      )
    }
    
    # Check if it's a proper data frame
    if (!is.data.frame(specs) || nrow(specs) == 0) {
      return(
        datatable(
          tibble(Message = "No specifications extracted. Please try again."),
          options = list(pageLength = 15, dom = 't'),
          rownames = FALSE
        )
      )
    }
    
    # Display the specifications table
    specs %>%
      datatable(
        options = list(
          pageLength = 20, 
          dom = 'tip',
          autoWidth = TRUE
        ),
        rownames = FALSE,
        caption = htmltools::tags$caption(
          style = 'caption-side: top; text-align: left; font-weight: bold; font-size: 14px;',
          'Model Specifications: ETS component forms (e.g., ETS(M,A,M)) and ARIMA orders (p,d,q)(P,D,Q)[m]'
        )
      ) %>%
      formatStyle(
        columns = 'Model',
        fontWeight = 'bold',
        backgroundColor = '#f0f0f0'
      ) %>%
      formatStyle(
        columns = 'Specification',
        fontFamily = 'monospace',
        fontSize = '13px'
      )
  })
  
  # Accuracy table
  output$accuracy_table <- renderDT({
    req(fitted_models())
    
    acc <- accuracy_metrics()
    
    if (is.null(acc) || nrow(acc) == 0) {
      # Return empty table with message
      tibble(
        Message = "Click 'Run Forecast Models' to see accuracy metrics"
      ) %>%
        datatable(
          options = list(pageLength = 15, dom = 't'),
          rownames = FALSE
        )
    } else {
      acc %>%
        mutate(across(where(is.numeric), ~round(.x, 2))) %>%
        datatable(
          options = list(pageLength = 15, dom = 'tip'),
          rownames = FALSE,
          filter = 'top'
        ) %>%
        formatStyle(
          'RMSE',
          background = styleColorBar(range(acc$RMSE, na.rm = TRUE), 'lightblue'),
          backgroundSize = '100% 90%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        )
    }
  })
  
  # Forecast plot
  output$forecast_plot <- renderPlotly({
    req(wine_data(), split_data())
    
    # Check if forecasts exist
    if (is.null(forecasts()) || nrow(as_tibble(forecasts())) == 0) {
      # Show empty plot with message
      plot_ly() %>%
        add_annotations(
          text = "Click 'Run Forecast Models' to generate forecasts",
          xref = "paper",
          yref = "paper",
          x = 0.5,
          y = 0.5,
          showarrow = FALSE,
          font = list(size = 16)
        ) %>%
        layout(
          xaxis = list(showgrid = FALSE, showticklabels = FALSE, title = ""),
          yaxis = list(showgrid = FALSE, showticklabels = FALSE, title = "")
        )
    } else {
      split_date <- split_data()$split_date
      
      # Convert split_date to numeric for geom_vline
      split_date_num <- as.numeric(as.Date(split_date))
      
      # Create base plot
      p <- wine_data() %>%
        ggplot(aes(x = as.Date(Month), y = Sales)) +
        geom_line(color = "black", size = 0.8) +
        geom_vline(xintercept = split_date_num, 
                   linetype = "dashed", color = "red", size = 1) +
        facet_wrap(~ Varietal, scales = "free_y", ncol = 2) +
        labs(
          title = "Forecasts by Model and Varietal",
          subtitle = "Red line = train/test split",
          x = "Date",
          y = "Sales (thousands of liters)"
        ) +
        theme_minimal()
      
      # Add forecasts
      fc_data <- forecasts() %>%
        as_tibble() %>%
        select(Varietal, .model, Month, .mean)
      
      if (nrow(fc_data) > 0) {
        p <- p + 
          geom_line(
            data = fc_data %>% mutate(Month = as.Date(Month)),
            aes(x = Month, y = .mean, color = .model),
            size = 1.2
          ) +
          scale_color_brewer(palette = "Set1", name = "Model")
      }
      
      ggplotly(p, height = 500) %>%
        layout(legend = list(orientation = "h", x = 0.2, y = -0.15))
    }
  })
  
  # Forecast values table
  output$forecast_values <- renderDT({
    if (is.null(forecasts()) || nrow(as_tibble(forecasts())) == 0) {
      tibble(
        Message = "Click 'Run Forecast Models' to generate forecasts"
      ) %>%
        datatable(
          options = list(pageLength = 12, dom = 't'),
          rownames = FALSE
        )
    } else {
      forecasts() %>%
        as_tibble() %>%
        mutate(Month = as.character(Month)) %>%  # Convert yearmonth to character
        select(Varietal, Model = .model, Month, Forecast = .mean) %>%
        mutate(Forecast = round(Forecast, 2)) %>%
        datatable(
          options = list(pageLength = 12, dom = 'tip'),
          rownames = FALSE,
          filter = 'top'
        )
    }
  })
  
  # Best model summary
  output$best_model_summary <- renderUI({
    acc <- accuracy_metrics()
    
    if (is.null(acc) || nrow(acc) == 0) {
      HTML("<p>Click 'Run Forecast Models' to see best performers</p>")
    } else {
      best_models <- acc %>%
        filter(Set == "Validation") %>%
        group_by(Varietal) %>%
        slice_min(RMSE, n = 1, with_ties = FALSE) %>%
        ungroup()
      
      if (nrow(best_models) == 0) {
        return(HTML("<p>No validation results available yet</p>"))
      }
      
      summary_text <- map_chr(1:nrow(best_models), function(i) {
        paste0(
          "<strong>", best_models$Varietal[i], ":</strong> ",
          best_models$.model[i], 
          " (RMSE: ", round(best_models$RMSE[i], 2), ")"
        )
      })
      
      HTML(paste(
        "<h5>Best Models by Varietal (Validation Set):</h5>",
        paste(summary_text, collapse = "<br/>"),
        sep = "<br/>"
      ))
    }
  })
  
  # Residual plots
  output$residual_plots <- renderPlot({
    if (is.null(fitted_models())) {
      plot.new()
      text(0.5, 0.5, "Click 'Run Forecast Models' to see diagnostics", cex = 1.5)
    } else {
      tryCatch({
        fitted_models() %>%
          augment() %>%
          filter(!is.na(.resid), !is.na(.fitted)) %>%  # Remove NA values
          ggplot(aes(x = .fitted, y = .resid, color = Varietal)) +
          geom_point(alpha = 0.6) +
          geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
          facet_wrap(~ .model, scales = "free") +
          labs(
            title = "Residual Plots by Model",
            x = "Fitted Values",
            y = "Residuals"
          ) +
          theme_minimal() +
          theme(legend.position = "bottom")
      }, error = function(e) {
        plot.new()
        text(0.5, 0.5, paste("Error generating residual plot:", e$message), cex = 1)
      })
    }
  })
  
  # ACF plots
  output$acf_plots <- renderPlot({
    if (is.null(fitted_models())) {
      plot.new()
      text(0.5, 0.5, "Click 'Run Forecast Models' to see diagnostics", cex = 1.5)
    } else {
      tryCatch({
        fitted_models() %>%
          augment() %>%
          filter(!is.na(.resid)) %>%  # Remove NA residuals
          ACF(.resid) %>%
          autoplot() +
          facet_wrap(~ Varietal + .model, scales = "free_y") +
          labs(title = "ACF of Residuals") +
          theme_minimal()
      }, error = function(e) {
        plot.new()
        text(0.5, 0.5, paste("Error generating ACF:", e$message), cex = 1)
      })
    }
  })
  
  # Ljung-Box test
  output$ljung_box_table <- renderDT({
    if (is.null(fitted_models())) {
      tibble(
        Message = "Click 'Run Forecast Models' to see test results"
      ) %>%
        datatable(
          options = list(pageLength = 15, dom = 't'),
          rownames = FALSE
        )
    } else {
      tryCatch({
        lb_results <- fitted_models() %>%
          augment() %>%
          filter(!is.na(.resid)) %>%  # Remove NA residuals
          features(.resid, ljung_box, lag = 24) %>%
          mutate(
            `Test Result` = if_else(
              lb_pvalue > 0.05,
              "Pass (No autocorrelation)",
              "Fail (Autocorrelation detected)"
            )
          ) %>%
          select(
            Varietal,
            Model = .model,
            `Test Statistic` = lb_stat,
            `P-value` = lb_pvalue,
            `Test Result`
          ) %>%
          mutate(across(where(is.numeric), ~round(.x, 4)))
        
        lb_results %>%
          datatable(
            options = list(pageLength = 15, dom = 't'),
            rownames = FALSE
          ) %>%
          formatStyle(
            'P-value',
            backgroundColor = styleInterval(0.05, c('lightcoral', 'lightgreen'))
          )
      }, error = function(e) {
        tibble(
          Message = paste("Error:", e$message)
        ) %>%
          datatable(
            options = list(pageLength = 15, dom = 't'),
            rownames = FALSE
          )
      })
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)