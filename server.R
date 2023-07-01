# Define server logic
server <- function(input, output) {
  
  # Plot histogram based on selected column
  output$histogram <- renderPlot({
    req(input$continuous_button_one)
    cont <- input$continuous_charts
    ggplot(auto, aes_string(x = cont)) +
      geom_histogram(fill = "blue", bins = 30) +
      labs(title = paste("Histogram of", cont)) 
  })
  
  # Plot scatter plot based on selected column
  output$scatterplot <- renderPlot({
    req(input$continuous_button_one)
    cont <- input$continuous_charts
    ggplot(auto, aes_string(x = cont, y=auto$price)) +
      geom_point(fill = "blue") +
      labs(title = paste("Relationship between", cont, ' and price'))+
      ylab('price')
  })
  
  #Plot bar chart based on selected column
  output$barChart <- renderPlot({
    req(input$categorical_button_one)
    cat <- input$categorical
    ggplot(auto, aes_string(x = cat)) +
      geom_bar(fill = "blue") +
      labs(title = paste("Bar Chart of", cat))
  })
  
  # Plot box plot based on selected column
  output$boxPlot <- renderPlot({
    req(input$categorical_button_one)
    cat <- input$categorical
    ggplot(auto, aes_string(x = cat, y=auto$price)) +
      geom_boxplot(fill = "blue") +
      labs(title = paste("Box Plot of", cat)) + xlab(cat) + ylab('price')
  })
  
  statistics <- reactive({
    req(input$continuous_button_two)
    continuous_stats <- input$continuous_stats
    list(
      "Mean" = mean(auto_cont[[continuous_stats]]),
      "Median" = median(auto_cont[[continuous_stats]]),
      "Standard Deviation" = sd(auto_cont[[continuous_stats]]),
      "Variance" = var(auto_cont[[continuous_stats]]),
      "Interquartile Range" = IQR(auto_cont[[continuous_stats]]),
      "Range" = max(auto_cont[[continuous_stats]]) - min(auto_cont[[continuous_stats]])
    )
  })
#display
  output$summary_one <- renderPrint({
    req(input$continuous_button_two)
    continuous_one <- input$continuous_stats
    measures <- statistics()
    cat(paste0("Selected column: ", continuous_one, "\n",
           "Mean: ", measures$Mean, "\n",
           "Median: ", measures$Median, "\n",
           "Standard Deviation: ", measures$`Standard Deviation`, "\n",
           "Variance: ", measures$Variance, "\n",
           "Interquartile Range: ", measures$`Interquartile Range`, "\n",
           "Range: ", measures$Range))
  })
  
  # Compute outliers of the selected columns
  boxplot_rule <- reactive({
    req(input$continuous_button_two)
    continuous_box <- input$continuous_stats
    
    for (i in auto_cont[[continuous_box]]) {
      if (i < mean(auto_cont[[continuous_box]]) - sd(auto_cont[[continuous_box]]) ||
          i > mean(auto_cont[[continuous_box]]) + sd(auto_cont[[continuous_box]])) {
        out <- c(out, i)
      }
    }
    
    list(
            out = out
    )
  })
  
  # Display outliers of the selected columns
  output$summary_two <- renderTable({
    req(input$continuous_button_two)
    continuous_two <- input$continuous_stats
    boxplot_rule <- boxplot_rule()
    
   summary_df <-  data.frame(Outlier = boxplot_rule$out)
   summary_df
  })
  
  # Compute outliers of selected columns
  chebyshev_rule <- reactive({
    req(input$continuous_button_two)
    continuous_chebyshev <- input$continuous_stats
    
    Q = quantile(auto_cont[[continuous_chebyshev]])
    Q1 = Q[2]; Q3 = Q[4]
    IQR = Q3-Q1
    
    for (i in auto_cont[[continuous_chebyshev]]){
      if (i < Q1-1.5*IQR | i > Q3+1.5*IQR){
        out <- c(out, i)
      }
    }
    
    list(
      out = out
    )
  })
  
  # Display outliers of selected columns
  output$summary_three <- renderTable({
    req(input$continuous_button_two)
    continuous_chebyshev <- input$continuous_stats
    chebyshev_rule <- chebyshev_rule()
    summary_df <-  data.frame(Outlier = chebyshev_rule$out)
    summary_df
  })
  
  # Discrete Probability Models
  # Compute and display results from Bernoulli experiment
  observeEvent(input$continuous_button_three, {
    output$summary_four <- renderTable({
      discrete_model <- auto_binary[[input$discrete]]
      
      # Specify the number of trials (s) and the probability of success (p)
      s <- input$number_of_trials
      p <- input$probability_of_success
      
      # Generate Bernoulli random variables using rbinom
      bernoulli_samples <- rbinom(s, 1, p)
      
      # Create a data frame with the summary
      summary_df <- data.frame(
        Variable = input$discrete,
        Number_of_Trials = s,
        Probability_of_Success = p,
        Bernoulli_Samples = bernoulli_samples
      )
      # Return the summary data frame
      summary_df
    })
  })
  
  # Compute and display results from Binomial experiment
  observeEvent(input$continuous_button_three, {
    output$summary_five <- renderTable({
      discrete_model <- auto_binary[[input$discrete]]
      
      # Specify the number of trials (s) and the probability of success (p)
      s <- input$number_of_trials
      p <- input$probability_of_success
      
      # Generate Binomial random variables using rbinom
      binomial_samples <- rbinom(s, 2, p)
      
      # Create a data frame with the summary
      summary_df <- data.frame(
        Variable = input$discrete,
        Number_of_Trials = s,
        Probability_of_Success = p,
        Binomial_Samples = binomial_samples
      )
      # Return the summary data frame
      summary_df
    })
  })
  
  # Continuous Probability Distribution
  # Compute Uniform Distribution
  uniform_model <- reactive({
    req(input$continuous_button_four)
    uniform <- input$model
    out_4 <- c()
    
    sam <- length(auto_cont[[uniform]])
    min_val <- min(auto_cont[[uniform]])
    max_val <- max(auto_cont[[uniform]])
    
    list(
      sam = sam,
      min = min_val,
      max = max_val
    )
  })
  
  # Display Uniform Samples experiment
  observeEvent(input$continuous_button_four, {
    output$summary_six <- renderTable({
      measures <- uniform_model()
      
      uniform_samples <- runif(measures$sam, measures$min, measures$max)
      
      summary_df <- data.frame(
        Variable = input$model,
        Number_of_Trials = measures$sam,
        Minimum = measures$min,
        Maximum = measures$max,
        Uniform_Samples = uniform_samples
      )
      summary_df
    })
  })
  
  # Normal Distribution
  # Compute Normal Distribution
  normal_model <- reactive({
    req(input$continuous_button_four)
    normal <- input$model
    out_5 <- c()
    
    sam <- length(auto_cont[[normal]])
    mean_val <- mean(auto_cont[[normal]])
    sd_val <- sd(auto_cont[[normal]])
    
    list(
      sam = sam,
      mean = mean_val,
      sd = sd_val
    )
  })
  
  # Display Normal Distribution experiment
  observeEvent(input$continuous_button_four, {
    output$summary_seven <- renderTable({
      measures <- normal_model()
      
      normal_samples <- rnorm(measures$sam, measures$mean, measures$sd)
      
      summary_df <- data.frame(
        Variable = input$model,
        Number_of_Trials = measures$sam,
        Mean = measures$mean,
        Standard_Deviation = measures$sd,
        Normal_Samples = normal_samples
      )
      summary_df
    })
  })

  # Calculate test of mean
  output$summary_eight <- renderPrint({
    req(input$test_of_mean_button)
    variable <- input$test_of_mean
    candidate_mean <- input$candidate_mean

    # two-sided test
    test_result <- t.test(auto_cont[[variable]], y=NULL, alternative = 'two.sided', mu = candidate_mean, conf.level = 1-0.05)

    # Hypotheses
    hypotheses <- c("H0: The true mean is equal to the proposed mean (μ = candidate_mean)\n",
                    "H1: The true mean is not equal to the proposed mean (μ ≠ candidate_mean)")

    # Decision and interpretation
    if (test_result$p.value <= 0.05) {
      decision <- "Reject H0"
      interpretation <- "There is evidence to suggest that the true mean is not equal to the proposed mean."
    }
    else {
      decision <- "Fail to reject H0"
      interpretation <- "There is not enough evidence to suggest that the true mean is not equal to the proposed mean."
    }

    # Display Parameters
    cat("Hypotheses:\n", hypotheses, "\n\n")
    cat("P-Value:", test_result$p.value, "\n")
    cat("Alpha:", 0.05, "\n\n")
    cat("Decision:", decision, "\n")
    cat("Interpretation:", interpretation, "\n")
  })
  
  # Calculate Goodness of Fit
  output$summary_nine <- renderPrint({
    req(input$goodness_of_fit_button)
    variable <- input$goodness_of_fit
    alpha <- 0.05
    
    k <- 3
    
    n = length(variable)
    
    p0 = rep(1/3, 3)
    
    E = n * p0
    
    test.value=sum(((F-E)^2)/E)
    
    c.value = qchisq(1-alpha,(k-1))
    
    hypotheses <- c("H0: The candidate choice is not fitted\n",
                    "H1: The candidate choice is well fitted ")
    
    # Decision and interpretation
    if (test.value>c.value) {
      decision <- "Reject H0"
      interpretation <- "There is evidence to suggest that the candiate choise is not well fitted."
    }
    else {
      decision <- "Fail to reject H0"
      interpretation <- "There is not enough evidence to suggest that the candidate choice is not well fitted."
    }
    
    # Display Results
    cat("Hypotheses:\n", hypotheses, "\n\n")
    cat("Test Value:", test.value, "\n")
    cat("Critical Value:", c.value, "\n\n")
    cat("Decision:", decision, "\n")
    cat("Interpretation:", interpretation, "\n")
  })
  
  #Calculate Independence Test
  output$summary_ten <- renderPrint({
    req(input$independence_test_button)
    var1 <- input$independence_test1
    var2 <- input$independence_test2
    alpha <- 0.01
    F=table(auto_cat[[var1]], auto_cat[[var2]])
    test_result <- chisq.test(F)
    statistic <- test_result$statistic
    critical_value <- qchisq(1 - alpha, df = test_result$parameter)
    
    # Hypotheses
    hypotheses <- c("H0: The variables are independent",
                    "H1: The variables are dependent")
    
    # Decision and interpretation
    if (test_result$p.value<=alpha) {
      decision <- "Reject H0"
      interpretation <- "There is evidence to suggest that the two variables are dependent."
    } else {
      decision <- "Fail to reject H0"
      interpretation <- "There is not enough evidence to suggest that the two variables are dependent."
    }
    
    # Display Results
    cat("Hypotheses:\n", hypotheses, "\n\n")
    cat("Statistic:", statistic, "\n")
    cat("Critical Value:", critical_value, "\n\n")
    cat("Decision:", decision, "\n")
    cat("Interpretation:", interpretation, "\n")
  })
}