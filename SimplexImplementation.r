#Mark Neil G. Autriz
#Simplex Implementation


#The provided code implements a linear programming solver using the Simplex method for a nutritional optimization problem. It defines 
#functions for setting up the problem, solving it iteratively, and formatting the final results. The code initializes matrices for decision 
#variables and slack variables, solves the linear programming problem using the Gauss-Jordan method, and constructs a table with information 
#about the optimal solution, including food servings, nutrient values, and prices. The problem involves finding the optimal combination of food 
#servings to meet nutritional constraints while minimizing cost.

# This line sets the number of nutritional constraints in the program to 11.
nutrientConstraints <- 11

# This line calculates the total number of equations, considering both equality and inequality constraints.
totalNumberOfAEquations <- nutrientConstraints * 2

# This line defines the starting index for B constraints in the matrix, adding 1 to the total number of equations.
startIndexForBEquations <- totalNumberOfAEquations + 1

tableauSetup <- function(matrixGiven) {
  
  # Calculate the number of food items. The last row of matrix data is the objective function,
  # so the number of foods must be 1 less than the total number of rows.
  numberOfFood <- nrow(matrixGiven) - 1

  # Set up coefficients and variable names for the tableau, specifically for decision variables (x) and slack variables (s).
  # Initialize empty vectors for decision coefficients and slack coefficients.
  decisionCoefficients <- c()
  slackCoefficients <- c()

  # Initialize empty vectors for decision variable names (x variables).
  decisionVariableNames <- c()

  # Loop over the number of food items to create variable names (x1, x2, ..., xn).
  for (foodIndex in 1:numberOfFood) {
    decisionVariableNames <- append(decisionVariableNames, paste0("x", foodIndex))
  }

  # Initialize empty vectors for slack variable names (s variables).
  slackVariableNames <- c()
  # Calculate the offset, which is the starting index for slack variables.
  offset <- totalNumberOfAEquations + numberOfFood

  # Loop over the range of indices for slack variables and create their names (s1, s2, ..., sn).
  for (slackIndex in startIndexForBEquations:offset) {
    slackVariableNames <- append(slackVariableNames, paste0("s", slackIndex))
  }

  # Get the number of rows in the tableau, considering each food item and the objective function.
  tableauRows <- numberOfFood + 1
  
  # Initializing the contents of decisionCoefficients and slackCoefficients where s variables here correspond to B constraints where food_serving <= 10
  for (rowIndex in 1:tableauRows) {
    for (columnIndex in 1:numberOfFood) {
      # Check if the current row is the last row (objective function row).
      if (rowIndex == tableauRows) {
        #Allows us to set to decision coefficient to 0 and slack coefficient to 10.
        decisionCoefficients <- append(decisionCoefficients, 0)
        slackCoefficients <- append(slackCoefficients, 10)
      } else {
        # Check if the current column is not the same as the current row index.
        if (rowIndex != columnIndex) {
          #Allows to set the decision coefficient to 0 and slack coefficient to 0.
          decisionCoefficients <- append(decisionCoefficients, 0)
          slackCoefficients <- append(slackCoefficients, 0)
        } else {
          # If the current column is the same as the current row index, the program sets the decision coefficient to 1 and slack coefficient to -1.
          decisionCoefficients <- append(decisionCoefficients, 1)
          slackCoefficients <- append(slackCoefficients, -1)
        }
      }
    }
  }

  # Store the data as a matrix with column names == decisionVariableNames or slackVariableNames
  decisionVariablesMatrix <- matrix(data = decisionCoefficients, nrow = tableauRows, ncol = numberOfFood, byrow = TRUE, list(c(1:tableauRows), decisionVariableNames))
  slackVariablesMatrix <- matrix(data = slackCoefficients, nrow = tableauRows, ncol = numberOfFood, byrow = TRUE, list(c(1:tableauRows), slackVariableNames))

  # Calculate the starting column index for x variables after s variables for constraint B.
  startColumnX <- startIndexForBEquations + numberOfFood

  # Using cbind, combine all the submatrices to form a complete initial tableau.
  # Combine matrixGiven + slackVariablesMatrix + decisionVariablesMatrix.
  matrixGiven <- cbind(matrixGiven[, 1:(startIndexForBEquations - 1)], slackVariablesMatrix, matrixGiven[, startIndexForBEquations:ncol(matrixGiven)])
  matrixGiven <- cbind(matrixGiven[, 1:(startColumnX - 1)], decisionVariablesMatrix, matrixGiven[, startColumnX:ncol(matrixGiven)])

  # Return the updated matrixGiven
  return(matrixGiven)
}

tableauSolving <- function(matrixGiven) {
  # Initialize an empty list to store the calculated solution.
  calculatedSolution <- list()

  # Get the number of rows in the matrixGiven (tableau).
  tableauRows <- nrow(matrixGiven)

  # Get the number of columns in the matrixGiven (tableau).
  tableauColumns <- ncol(matrixGiven)

  
  # Continue iterating while there are negative values in the bottom row of the tableau.
  while (any(matrixGiven[tableauRows, ] < 0)) {
  # Apply the Gauss-Jordan method to update the tableau.
  matrixGiven <- GaussJordanMethod(matrixGiven)

  # Check if the result after the Gauss-Jordan method is not a matrix.
  ifelse(!is.matrix(matrixGiven), {
    # If not a matrix, handle the case where it's not possible to meet nutritional constraints.
    calculatedSolution <- append(calculatedSolution, list(matrixGiven = matrixGiven, basicSolution = basicSolution))
    print("It is not possible to meet the nutritional constraints with the foods that you have selected. (Undefined Test Ratios)")
    break
  }, {
    # If it is a matrix, proceed with updating and storing the solution.
    numberOfNutrients <- 11 * 2
    nameForSlack <- paste0("s", 1:numberOfNutrients)
    # Set column names for the matrixGiven.
    colnames(matrixGiven) <- c(nameForSlack, colnames(matrixGiven)[23:tableauColumns])

    # Calculate the end index for the tableau columns.
    tableauEndIndex <- tableauColumns - 1

    # Define the header for the basic solution.
    headerForBasicSolution <- c(nameForSlack, colnames(matrixGiven)[23:tableauEndIndex])

    # Extract elements for the basic solution from the last row of the tableau.
    elementsForBasicSolution <- as.numeric(matrixGiven[tableauRows, -(tableauColumns - 1)])

    # Create a numeric matrix with the elements for the basic solution.
    basicSolution <- matrix(data = elementsForBasicSolution, nrow = 1, ncol = length(elementsForBasicSolution),
                            byrow = TRUE, dimnames = list(c(1), headerForBasicSolution))

    # Store the current solution in the calculatedSolution list.
    calculatedSolution <- append(calculatedSolution, list(matrixGiven = matrixGiven, basicSolution = basicSolution))

    # Print the current tableau and basic solution.
    print(matrixGiven)
    print(basicSolution)
    })
  }

  # Return the list of calculated solutions.
  return(calculatedSolution)

}

# Define a function named creatingFinalSolution that takes three parameters: matrixFinal, basicSolution, and matrixGiven.
creatingFinalSolution <- function(matrixFinal, basicSolution, matrixGiven) {

  # Calculate the number of food items (excluding the objective function row).
  numberOfFood <- nrow(matrixGiven) - 1

  # Calculate the starting column index for x variables after s variables for constraint B.
  startColumnX <- startIndexForBEquations + numberOfFood

  # Calculate the last column index for x variables.
  lastColumnX <- (startColumnX + numberOfFood) - 1

  # Initialize an empty vector to store the final result data.
  finalResultData <- c()

  # Initialize the index for the current food item.
  currentFoodIndex <- 1

  # Loop over the columns corresponding to x variables in the basic solution.
  for (columnIndex in startColumnX:lastColumnX) {

    # Check if the basic solution for the current x variable is not zero.
    if (basicSolution[1, columnIndex] != 0) {

      # Append the food item name to the final result data.
      finalResultData <- append(finalResultData, matrixFinal[currentFoodIndex, 1])
      
      # Append the serving amount (rounded to 2 decimal places) to the final result data.
      finalResultData <- append(finalResultData, round(basicSolution[1, columnIndex], 2))
      
      # Calculate and append the price for the current x variable to the final result data (rounded to 2 decimal places).
      base_price <- as.numeric(matrixFinal[currentFoodIndex, 13])
      finalResultData <- append(finalResultData, round(basicSolution[1, columnIndex] * base_price, 2))
    }

    # Increment the current food index.
    currentFoodIndex <- currentFoodIndex + 1
  }

  # Print a message indicating a potential error.
  print("Error has been detected")

  # Calculate the final number of rows in the result matrix.
  finalRow <- length(finalResultData) / 3

  # Create a matrix with the final result data, organized in rows and columns.
  result <- matrix(data = finalResultData, nrow = finalRow, ncol = 3,
                   byrow = TRUE, dimnames = list(c(1:finalRow), c("Food", "Serving", "Price")))

  # Return the resulting matrix.
  return(result)
}
