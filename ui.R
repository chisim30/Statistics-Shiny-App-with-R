library(ggplot2)
library('shinythemes')
data(auto)
# run this portion of the code to change the column names of the data set.
colnames(auto)[2] <- 'normalized_losses'
colnames(auto)[4] <- 'fuel_type'
colnames(auto)[6] <- 'num_of_doors'
colnames(auto)[7] <- 'body_style'
colnames(auto)[8] <- 'drive_wheels'
colnames(auto)[9] <- 'engine_location'
colnames(auto)[10] <- 'wheel_base'
colnames(auto)[14] <- 'curb_weight'
colnames(auto)[15] <- 'engine_type'
colnames(auto)[16] <- 'num_of_cylinders'
colnames(auto)[17] <- 'engine_size'
colnames(auto)[18] <- 'fuel_system'
colnames(auto)[21] <- 'compression_ratio'
colnames(auto)[23] <- 'peak_rpm'
colnames(auto)[24] <- 'city_mpg'
colnames(auto)[25] <- 'highway_mpg'

# Divide the data set between it's continuous and categorical variables.
auto_cont <- auto[, c('normalized_losses','wheel_base', 'length', 
                      'width', 'height', 'curb_weight','engine_size',
                      'bore','stroke','compression_ratio', 'horsepower',
                      'peak_rpm', 'city_mpg','highway_mpg', 'price')]
auto_cat <- auto[, c('fuel_type', 'aspiration',
                     'num_of_doors', 'body_style', 'drive_wheels',
                     'engine_location', 'engine_type', 'num_of_cylinders',
                     'fuel_system', 'range')]

auto_binary <- auto[, c('fuel_type', 'aspiration', 'num_of_doors',
                        'engine_location')]
# Define UI
ui <- navbarPage(
  # App theme
  theme = shinytheme('slate'),
  #Title of the app
  title = "Cars",
  # Plot Tab Panel to explore the data set through graphs and plots
  tabPanel(
    title = 'Plot',
    sidebarLayout(
      sidebarPanel(
        selectInput("continuous_charts", "Select a column:", 
                    choices = colnames(auto_cont)),
        actionButton("continuous_button_one", "Numerical"),
        selectInput("categorical", "Select a column:", 
                    choices = colnames(auto_cat)),
        actionButton("categorical_button_one", "Categorical")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            title = 'Histogram',
            plotOutput("histogram")
          ),
          tabPanel(
            title = 'Scatter Plot',
            plotOutput("scatterplot")
          ),
          tabPanel(
            title = 'Bar Chart',
            plotOutput("barChart")
          ),
          tabPanel(
            title = 'Box Plot',
            plotOutput("boxPlot")
          )
        )
      )
    )
  ),
  # Statistics Tab Panel to show relevant statistics such as outliers, central and variation measures
  tabPanel(
    title = 'Statistics',
    sidebarLayout(
      sidebarPanel(selectInput("continuous_stats", "Select a column:", 
                               choices = colnames(auto_cont)),
                   actionButton("continuous_button_two", "Numerical")),
                   
      mainPanel(
        tabsetPanel(
          tabPanel(
            title = 'Central and Variation Measures',
            verbatimTextOutput('summary_one')
          ),
          tabPanel(
            title = 'Box Plot Rule',
            tableOutput('summary_two')
          ),
          tabPanel(
            title = 'Chebyshev Rule',
            tableOutput('summary_three')
          )
        )
      )
    )
  ),
  # Discrete Probability Model Tab Panel, perform experiments on selected columns; quantifying uncertainty with discrete probability models
  tabPanel(
    title = 'Discrete Probability Model',
    sidebarLayout(
      sidebarPanel(
        selectInput("discrete", "Select a column:",
                    choices = colnames(auto_binary)),
        numericInput("number_of_trials", "Number of Trials:", value = 201, min = 1),
        numericInput("probability_of_success", "Probability of Success:", value = 0.5,
                     min = 0, max = 1),
        actionButton("continuous_button_three", "Model")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            title = 'Bernoulli Model',
            tableOutput('summary_four')
          ),
          tabPanel(
            title = 'Binomial Model',
            tableOutput('summary_five')
          )
         )
        )
      )
    ),
  # Continuous Probability Model Tab Panel, perform experiments on selected columns; quantifying uncertainty with continuous probability models
  tabPanel(
    title = 'Continuous Probability Model',
    sidebarLayout(
      sidebarPanel(
        selectInput("model", "Select a column:",
                    choices = colnames(auto_cont)),
        actionButton("continuous_button_four", "Model")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            title = 'Uniform Model',
            tableOutput('summary_six')
          ),
          tabPanel(
            title = 'Normal Model',
            tableOutput('summary_seven')
          )
        )
      )
    )
  ),
  tabPanel(
    title = 'Test of Mean',
    sidebarLayout(
      sidebarPanel(
        selectInput("test_of_mean", "Select a variable:", choices = colnames(auto_cont)[c(1)]),
        numericInput("candidate_mean", "Enter the proposed mean:", value = 122),
        actionButton("test_of_mean_button", "Run Test of Mean")
      ),
      mainPanel(
        verbatimTextOutput("summary_eight")
      )
    )
  ),
  tabPanel(
    title = 'Goodness of Fit',
    sidebarLayout(
      sidebarPanel(
        selectInput("goodness_of_fit", "Select a variable:", choices = colnames(auto)[c(8)]),
        actionButton("goodness_of_fit_button", "Run Goodness of Fit Test")
      ),
      mainPanel(
        verbatimTextOutput("summary_nine")
      )
    )
  ),
  tabPanel(
    title = 'Independence Test',
    sidebarLayout(
      sidebarPanel(
        selectInput("independence_test1", "Select Variable 1:", choices = colnames(auto_cat[3])),
        selectInput("independence_test2", "Select Variable 2:", choices = colnames(auto_cat[4])),
        actionButton("independence_test_button", "Run Independence Test")
      ),
      mainPanel(
        verbatimTextOutput("summary_ten")
      )
    )
  )
)