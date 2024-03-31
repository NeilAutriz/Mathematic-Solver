#Mark Neil G. Autriz
#Source code for the User Interface using R Shiny.

#This R Shiny application serves as a numerical methods tool with a user-friendly interface for Polynomial Regression, Quadratic Spline 
#Interpolation (QSI), and a Diet Problem Solver. Users can upload data, perform polynomial regression with interactive plotting, estimate 
#values using quadratic spline interpolation, and optimize food item selection based on nutritional constraints. The application is structured 
#tabs for each method, offering an intuitive design for users to input data, visualize results, and obtain optimized solutions. The server logic 
#handles user interactions, data processing, and result updates. Overall, it provides a versatile platform for numerical analysis and optimization 
#tasks in a variety of domains.

# This contains the necessary libraries for the implementation of the R shiny that makes up the UI.
library(shiny) # The library that contains the r shiny definitions.
library(shinydashboard) # Allows the creation of dashboard for the R shiny.
library(shinyMatrix) # Module of R shiny for the presentation of the matrix.
library(shinyjs) # Allows the customization of the java script in R shiny.
library(shinythemes) # Permits the application of the themes in the program specifically Cerulean in this UI.
library(readxl) # Navigates to read the excel file that will be inputted.
library(DT) # Permits the interactive movement of the data tables. 
library(dplyr) # Guides data transformation and manipulation.

# This contains the source codes which are R programs that contains the functions and methods of the project.

source("QuadraticSplineInterpolation.R")
source("PolynomialRegression.R")
source("GaussJordanMethod.R")
source("SimplexImplementation.R")

# Allows us to read the information that is placed in the excel file that have the data regarding the food, nutrients, and constraints.
data <- read_excel("DPS_fooditems.xlsx")

# Create a vector of unique food items excluding "Minimum" from the "Food_Item" column
available_choices <- unique(data$Food_Item[data$Food_Item != "Minimum"])


# This contains the vital information regarding the user interface of the Numerical Applications in the program.
ui <- dashboardPage(
  # Header section which contains the icons along with the titles.
  dashboardHeader(
    title = tags$span(icon("calculator"), style = "font-weight:bold;", "Funtionalities"),  
    titleWidth = 230,
    # This contains the information icon which gives the user of the program regarding the usage of linear regression and other numerical methods in the program.
    tags$li(a(href = "https://www.sciencedirect.com/topics/engineering/numerical-method", target = "_blank", class = "fa fa-info-circle"), 
            class = "dropdown", title = "Information about Regression, Simplex, and QSI"), 
    tags$li(
      class = "dropdown",
      title = "App Purpose",
      style = "padding: 15px; text-align: center; background-color: #rgb(21,76,121); color: white; font-weight: bold;",
      "Polynomial Interpolation, Quadratic Spline Interpolation and Simplex Method"
    )
  ),

  # This section defines the content of the sidebar which guides the user of the program to navigate easier.
  dashboardSidebar(
    sidebarMenu(
      # This contains the elements in the menu tab for the polynomial regression.
      menuItem(
        "Polynomial Regression",
        tabName = "polynomialRegression",
        icon = icon("line-chart") 
      ),
      
      # This contains the elements in the menu tab for the quadratic spline interpolation.
      menuItem(
        "Quadratic Spline Interpolation",
        tabName = "qsi",
        icon = icon("area-chart") 
      ),

      # This contains the elements in the menu tab for the quadratic spline interpolation.
      menuItem(
        "Diet Problem Solver",
        tabName = "dietProblemSolver",
        icon = icon("cutlery") 
      )
    )
  ),  
  # This contains the body of the dash board that utilizes the shinyjs library for the accomplismetn of its implementation.
  dashboardBody(
    shinyjs::useShinyjs(),
    
    tabItems(
    # Contents of the user interface for the Polynoial Regression.
      tabItem(
        tabName = "polynomialRegression",
        fluidPage(
          # Title panel for Polynomial Regression
          titlePanel(tags$span(style = "font-weight:bold; text-align:center;", "Polynomial Regression Calculator")),

          # Description paragraphs for Polynomial Regression to guide the users of the program.
          p(style = "text-align: justify;", "Polynomial regression is a statistical method used to model the relationship between a dependent variable and one or more independent variables by fitting a polynomial equation to the observed data.", style = "color: black;"),
          p(style = "text-align: justify;", "The Polynomial Regression tab enables you to fit a polynomial to a set of data points.",
            "Upload a CSV file containing 'x' and 'y' columns, select the inputDegree of the polynomial, and calculate the regression.",
            "Visualize the results with an interactive plot and explore the coefficients of the fitted polynomial.", style = "color: black;"),
          
          # Layout with sidebar and main panels
          sidebarLayout(
            sidebarPanel(
              # File input for CSV file containing 'x' and 'y' columns
              helpText("Upload a CSV file containing 'x' and 'y' columns.", style = "color: #062a78;"),
              fileInput("fileInputPolyCSV", "Upload your CSV file for Polynomial Regression: "),
              
              # Numeric input for the degree of the polynomial
              numericInput("inputDegree", "Input the degree of polynomial: ", value = 2),
              
              # Help text and numeric input for x value prediction
              helpText("Select the degree of the polynomial and enter x value for prediction.", style = "color: #062a78;"),
              numericInput("estimatedValue", "Place the value that you want to predict: ", value = 0),

              # Action button to trigger polynomial calculation
              actionButton("polynomialFunction", "Perform Regression"),
              
            ),

            # Main panel which allows to display the results of the polynomial Regression.
            mainPanel(
              h3("Polynomial Regression Results"),
              verbatimTextOutput("regressionCoefficientsOutput"),
              verbatimTextOutput("regressionEstimatesOutput"),
              verbatimTextOutput("regressionFunctionOutput"),
              plotOutput("regressionPlotGraphOutput")
            )
          )
        )
      ),
      

      #Contents of the user interface definition for Quadratic Spline Interpolation (QSI) tab.
      tabItem(
        tabName = "qsi",
        fluidPage(
          # Title panel for Quadratic Spline Interpolation
          titlePanel(tags$span(style = "font-weight:bold; text-align:center;", "Quadratic Spline Interpolation")),

          # Description paragraphs for Quadratic Spline Interpolation
          p(style = "text-align: justify;", "Quadratic spline interpolation is a numerical technique for constructing a smooth curve that passes through a given set of data points using quadratic polynomials.", style = "color: black;"),
          p(style = "text-align: justify;", "The Quadratic Spline Interpolation tab allows you to estimate values between known data points.",
            "Upload a CSV file containing 'x' and 'y' columns, and the application will interpolate values using quadratic spline functions.",
            "Explore the piecewise quadratic functions and visualize the interpolated curve on an interactive plot.", style = "color: black;"),
          
          # Layout with sidebar and main panels
        sidebarLayout(
          sidebarPanel(
            # File input for CSV file containing 'x' and 'y' columns
            helpText("Upload a CSV file containing 'x' and 'y' columns.", style = "color: #062a78;"),
            fileInput("fileInputQSICSV", "Upload your CSV file for Quadratic Spline Interpolation: "),
            
            # Help text and numeric input for value estimation
            helpText("Enter the value to estimate between known data points using quadratic spline interpolation.", style = "color: #062a78;"),
            numericInput("estimate", "Value to be Estimated: ", 0),
            
            # Action button to trigger QSI calculation
            actionButton("solve", "Perform Interpolation")
          ),

        # Main panel with interactive elements
        mainPanel(
          # Fluid row for refresh button
          fluidRow(
            column(
              width = 12,
              align = "right",
              actionButton(
                "reset",
                "Refresh"
              )
            )
          ),

          # Columns for displaying QSI functions and estimated f(x)
          column(
            width = 12,
            offset = 1,
            fluidRow(
              br(),
              column(
                width = 5,
                h5(strong("Functions per Interval:")),
                htmlOutput("functions")
              )
            ),
            fluidRow(
              column(
                width = 3,
                h5(strong("Estimated f(x):")),
                verbatimTextOutput("finalValueQSI")
                )
              )
            )
          )
        )
      )
    ), 
      
      # Contains the definition of contents in the diet problem solver tab.
      tabItem(
        tabName = "dietProblemSolver",
         # Creating a fluid page for the Diet Problem Solver tab
        fluidPage(
          # Contains the introductory messages in order to orient the user of the program regarding the functionality of the diet problem solver.
          titlePanel(tags$span(style = "font-weight:bold; text-align:center;", "Diet Problem Solver")),
          p(style = "text-align: justify;", "Diet problem solver is a numerical method used to optimize the selection of food items to meet nutritional requirements while minimizing the cost, considering constraints on nutrient intake.", style = "color: black;"),
          p(style = "text-align: justify;", "The simplex method is applied to solve this optimization problem. It involves selecting a combination of food items that achieves the lowest cost while satisfying nutritional constraints and maximizing nutrient intake.", style = "color: black;"),
        ),

        # First fluid row for selecting food items
        fluidRow(
          box(
            title = "Select Food Choices",
            width = 12,
             # Input identifier for selected food items
            selectizeInput(
              "selectedFood",
              label = NULL,
              # Allows to present the foods choices that are available.
              choices = available_choices,
              # Permits the multiple selection of food in the drop down options.
              multiple = TRUE,
              options = list(
                # Allows to have the placer value, remove button plugins along with initialization in the input values.
                placeholder = 'Pick your Food Choices...',
                plugins = list('remove_button'),
                onInitialize = I('function() { this.setValue(""); }')
              )
            ),
            
            # Button that allows the performing the optimization of diet in the selected food.
            actionButton("optimizeDiet", "Optimize Diet from Selected Foods"),
            helpText(
              "Select multiple food items from the dropdown list.",
              "Use the 'Submit' button to view a summary of the nutritional information for the selected food items.",
              style = "color: #062a78;"
            )
          )
        ),

        # Second fluid row for the summary of selected food items
        fluidRow(
          box(
            title = tags$span(style = "text-align: center;", "Display of your Selected Food Items"),
            width = 12,
            
            #Allows to display the selected food in the program.
            DTOutput("selectedFoodOutput"),
            helpText(
              "Select multiple food items from the dropdown list.",
              "Use the 'Submit' button to view a summary of the nutritional information for the selected food items.",
              style = "color: #062a78;"
            )
          )
        ),

        # Third fluid row for displaying results and tableau/basic solution
        fluidRow(
          box(
            title = tags$span(style = "text-align: center;", "Resulting Optimized Diet"),
            width = 6,

            # Output for displaying optimization results
            DTOutput("matrixTable"),
            # Output for displaying optimal diet text
            textOutput("optimal_diet"),
            # Output for displaying error message during optimization
            textOutput("errorInOptimizingDiet"),
            helpText(
              "Explore the results of the optimization process.",
              "View the optimal diet, cost breakdown, and other relevant information.",
              style = "color: #062a78;"
            ),
                # Reset button
            actionButton("resetDiet", "Refresh")
          ),

          box(
            title = tags$span(style = "text-align: center;", "Tableau Set-up and Basic Solution"),
            width = 6,
            # Output for displaying tableau and basic solution UI
            uiOutput("tableForMatrices"),
            
            # Output for displaying basic solution table
            DTOutput("basicSolutionTable"),
            helpText(
              "Review the tableau and basic solution.",
              "These tables provide detailed information about the optimization process.",
              style = "color: #062a78;"
            )
          )
        )
      )      
    )
  )
)

# Function that converts a string into a numeric list used in quadratic spline interpolation to convert string input of the user into a numeric list
converter <- function(string) {
  # Split the string into a list based on ","
  listStorage <- strsplit(string, ",")[[1]]
  # Convert the elements of the list into numeric values
  numeric <- as.numeric(listStorage)
  return (numeric)
}

#This contains the server definition for the UI that encapsulates Polynomial Regression and Quadratic Spline Interpolation.
server <- function(input, output, session) {
  
  #Contains the definition for the server in Polynomial Regression.
  observeEvent(input$polynomialFunction, {   # Event handler for Polynomial Regression button
    # Check if CSV file is uploaded
    req(input$fileInputPolyCSV)

    # Read data from CSV file
    data <- read.csv(input$fileInputPolyCSV$datapath)
    x <- data$x
    y <- data$y
    inputDegree <- input$inputDegree
    estimatedValue <- input$estimatedValue
    
    # Check if the number of x and y values is the same
    if (length(x) != length(y)) {
      output$regressionCoefficientsOutput <- renderText("Number of x and y values must be the same!")
      output$regressionEstimatesOutput <- renderText("")
      output$regressionFunctionOutput <- renderText("")
      output$regressionPlotGraphOutput <- renderPlot({}) 
    } else {
      # Perform Polynomial Regression
      resultingPolynomial <- PolynomialRegression(x, y, inputDegree, estimatedValue)
      
      #Display the regression coefficients
      output$regressionCoefficientsOutput <- renderPrint({
        regressionCoefficientText <- paste("Coefficients:", paste(resultingPolynomial$coefficients, collapse = ", "))
        regressionCoefficientText
      })
      
      # Render Estimated Values
      output$regressionEstimatesOutput <- renderText({
        valuePredicted <- eval(parse(text = resultingPolynomial$functionString))(estimatedValue)
        paste("Estimated y for x =", estimatedValue, "is", valuePredicted)
      })
      
      # Exhibits the Polynomial Function
      output$regressionFunctionOutput <- renderPrint({
        eval(parse(text = resultingPolynomial$functionString))
      })
      
      # Render the plot
      output$regressionPlotGraphOutput <- renderPlot({
        fitted_function <- function(x_val) eval(parse(text = resultingPolynomial$functionString))(x_val)
        plot(x, y, col = "blue", pch = 19, xlab = "X", ylab = "Y", main = "Polynomial Regression")
        curve(fitted_function, add = TRUE, col = "#062a78", lwd = 2)
        legend("topright", legend = "Polynomial that is Fitted", col = "#062a78", lty = 1, lwd = 2)
      })
    }
  })
  
  #This contains the code in the server that encapsulates the Quadratic Spline Interpolation (QSI).
  observeEvent(input$solve, {
    # Check if CSV file for QSI is uploaded
    req(input$fileInputQSICSV)
    
    # Read data from CSV file
    data <- read.csv(input$fileInputQSICSV$datapath)
    
    # Extract x and y data
    QSIdataX <- data$x
    QSIdataY <- data$y
    
    # Check if there are at least 3 data points and lengths are equal
    if (length(QSIdataY) >= 3 && length(QSIdataX) >= 3 && length(QSIdataX) == length(QSIdataY)) {
      # Perform Quadratic Spline Interpolation
      poly <- quadraticSplineInterpolation(list(QSIdataX, QSIdataY), input$estimate)

      # Render the final estimated value
      output$finalValueQSI <- renderText(poly$y)

      # Showcase the piecewise quadratic functions
      output$functions <- renderUI({
        quadraticSplineTextFunction <- "<br/>"
        for (i in 1:length(poly$quadraticFunctions)) {
          quadraticSplineTextFunction <- paste(quadraticSplineTextFunction, "Interval ", i, ":", sep = "")
          quadraticSplineTextFunction <- paste(quadraticSplineTextFunction, "<br/>function(x) ", deparse(poly$quadraticFunctions[[i]])[2], sep = "")
          quadraticSplineTextFunction <- paste(quadraticSplineTextFunction, "", sep = "<br/><br/>")
        }
        HTML(paste(quadraticSplineTextFunction, sep = "<br/>"))
      })
    } 
    #Handles the cases where the contains are not met.
    else { 
      if (length(QSIdataX) < 3 || length(QSIdataY) < 3) {
        output$finalValueQSI <- renderText("Please fill at least 3 data points.")
      } else if (length(QSIdataX) != length(QSIdataY)) {
        output$finalValueQSI <- renderText("Please fill the same number of data points for x and y.")
      }
    }
  })
  
  # Event handler for reset button.
  observeEvent(
    input$reset, {
      refresh()
    }
  )
  
  
  #This contains the server definitions for the diet optimizer.  

  # Reactive expression for the selected data
  selected_data <- reactive({ 
    # Check if the optimization button is clicked and food items are selected
    req(input$optimizeDiet)
    req(input$selectedFood)
    
    #Indulges the "Bound" row which is placed in the selected data.
    selectedFoodSubset <- data[data$Food_Item %in% c(input$selectedFood, "Bound"), c("Food_Item", "Calories_Max", "Calories_Min", "Cholesterol_Max", "Cholesterol_Min", "Total_Fat_Max", "Total_Fat_Min", "Sodium_Max", "Sodium_Min",  "Carbohydrates_Max",  "Carbohydrates_Min", "Dietary_Fiber_Max", "Dietary_Fiber_Min", "Protein_Max", "Protein_Min", "Vit_A_Max", "Vit_A_Min", "Vit_C_Max", "Vit_C_Min",  "Calcium_Max",  "Calcium_Min",  "Iron_Max",  "Iron_Min", "Z", "Price")]

    # Set row names and remove the Food Item column
    rownames(selectedFoodSubset) <- selectedFoodSubset$Food_Item
    selectedFoodSubset <- selectedFoodSubset[, -1]
    
    return(selectedFoodSubset)
    
  })

  # Define a reactive expression for selected data which is for the initial table consisting of food items, nutrients, and price only.
  dataOfSelectedFood <- reactive({
    # Filter the data based on selected food items
    filteredFoodData <- data[data$Food_Item %in% input$selectedFood, c("Food_Item", "Calories_Min", "Cholesterol_Min", "Total_Fat_Min", "Sodium_Min", "Carbohydrates_Min", "Dietary_Fiber_Min","Protein_Min", "Vit_A_Min", "Vit_C_Min", "Calcium_Min",  "Iron_Min", "Price")]
    
    #Creating the righteous names for easier navigation.
    filteredFoodData <- filteredFoodData %>%
      mutate(`Food Item` = Food_Item,
             `Calories (mg)` = Calories_Min,
             `Cholesterol (g)` = Cholesterol_Min,
             `Total Fat (mg)` = Total_Fat_Min,
             `Sodium (mg)` = Sodium_Min,
             `Carbohydrates (g)` = Carbohydrates_Min,
             `Dietary Fiber (g)` = Dietary_Fiber_Min,
             `Protein (g)` = Protein_Min,
             `Vitamin A (IU)` = Vit_A_Min,
             `Vitamin C (IU)` = Vit_C_Min,
             `Calcium (mg)` = Calcium_Min,
             `Iron (mg)` = Iron_Min,
             `Price ($)` = Price) %>%
      select(`Food Item`, `Calories (mg)`, `Cholesterol (g)`, `Total Fat (mg)`, `Sodium (mg)`, `Carbohydrates (g)`, `Dietary Fiber (g)`, `Protein (g)`, `Vitamin A (IU)`, `Vitamin C (IU)`, `Calcium (mg)`, `Iron (mg)`, `Price ($)`)
    
    return(filteredFoodData)
  })
  
  #Allows us to showcase the selected data table in the user interface.
  output$selectedFoodOutput <- DT::renderDT({
    dataOfSelectedFood()
  }, rownames = FALSE, options = list(dom = 't', paging = FALSE, ordering = FALSE))
  
  observeEvent(input$optimizeDiet, {
    # Convert selected data to a matrix and set up the initial tableau
    givenMatrix <- as.matrix(selected_data())
    givenMatrix <- tableauSetup(givenMatrix)
    
    # Perform iterations until there is no negative in the last row of the tableau
    result <-tableauSolving(givenMatrix)
    
    # Allows us to obtain the formidable solutions, final solution along with the basic solution.
    solutions <- result
    
    finalSolution <- solutions[[length(solutions)-1]]
    finalBasicSolution <- solutions[[length(solutions)]]
    
    # Check if the finalSolution is a matrix, indicating a feasible solution
    if (is.matrix(finalSolution)) {
      
      # Extract the optimized cost value from the finalSolution matrix
      optimizedCostValue <- finalSolution[nrow(finalSolution), ncol(finalSolution)]
      
      # Render UI for displaying matrices (givenMatrix and basicSolution) in iterations
      output$tableForMatrices <- renderUI({
        
        # Allows us to calculate the number of iterations in the program.
        numberOfIterations <- length(solutions) / 2
        
        # Initializes the creation of the list that stores the values of data tables for the given matrix and the basic solution.
        matrixDataTable <- lapply(seq(1, length(solutions), by = 2), function(i) {
          givenMatrix <- solutions[[i]]
          basicSolution <- solutions[[i + 1]]
          
          # Urges the approximation of the given matrix and basic solution into 4 decimal places.
          approximatedGivenMatrix <- round(givenMatrix, 4)
          approximatedBasicSolution <- round(basicSolution, 4)

          # Permits the program to transform the givenMatrix and basic solution towards data frames.
          matrixGivenFrame <- as.data.frame(approximatedGivenMatrix)
          basicSolutionFrame <- as.data.frame(approximatedBasicSolution)

          # Validates if it is already considered as the last iteration.
          lastSolutionIterationDeterminer <- (i + 1) >= length(solutions)
          
          if (lastSolutionIterationDeterminer) {
            # Transfigures the data table which beholds the givenMatrix that has the limited decimal places.
            givenMatrixTable <- datatable(matrixGivenFrame, options = list(paging = FALSE, searching = FALSE),
                                           caption = "Final Tableau")
            
            # This also initializes the creation of the data table for the basic solution.
            tableForBasicSolution <- datatable(basicSolutionFrame, options = list(paging = FALSE, searching = FALSE),
                                              caption = "Final Basic Solution")

          # It returns the data table of the last iteration by passing it in the list.
            list(givenMatrixTable, tableForBasicSolution)
          } else {

            # Establish the data table which beholds the given matrix.
            givenMatrixTable <- datatable(matrixGivenFrame, options = list(pageLength = 5),
                                           caption = paste("Iteration", floor(i / 2), "- Tableau"))
            
            # Corroborates the table for the recent basic solution including the title and decimal places.
            tableForBasicSolution <- datatable(basicSolutionFrame, options = list(paging = FALSE, searching = FALSE),
                                              caption = paste("Iteration", floor(i / 2), "- Basic Solution"))

          #This returns the list of the defined data tables above which is responsible for the regular iterations in the code.
            list(givenMatrixTable, tableForBasicSolution)
          }
        })

        # We simply combine all of the lists in order to establish the entire iterations.
        tagList(matrixDataTable)
      })
      
      # Convert the selected food data to a matrix

      resultingAnswer <- as.matrix(dataOfSelectedFood())
      
      # Generate the final table with cost breakdown
      resultingTable <- creatingFinalSolution(resultingAnswer, finalBasicSolution, givenMatrix)
      
      # Print the resulting table to the console
      print(resultingTable)
      
      # Render text for displaying the cost of the optimal diet
      output$optimal_diet <- renderText({
        dietCost <- paste0("The cost of this optimal diet is $", round(optimizedCostValue, 4), " per day")
        dietCost
      })
      
      # Render the final table as a DataTable in the Shiny app with limited decimal places and a title
      output$matrixTable <- renderDT({
        datatable(resultingTable, options = list(paging = FALSE, searching = FALSE),
                  caption = "The Solution and Cost Breakdown by Food")
      })
      
    } else { #Allows to address the problem with infeasible cases.
      output$errorInOptimizingDiet <- renderText({
        "The Problem is Infeasible: It is not possible to meet the nutritional constraints with the foods that you have selected.  (Undefined Test Ratios)"
      })
    }
    
  })

  observeEvent(input$resetDiet, {
    # Creates the logic to reset any reactive values or outputs
    output$matrixTable <- renderDT({ NULL })
    output$optimal_diet <- renderText({ NULL })
    output$errorInOptimizingDiet <- renderText({ NULL })
    # Reset the selected food items
    updateSelectizeInput(session, "selectedFood", selected = character(0))    
  })
   
}
# Run the combined Shiny app
shinyApp(ui = ui, server = server)

