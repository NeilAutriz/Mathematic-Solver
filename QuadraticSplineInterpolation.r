#Mark Neil G. Autriz
#CMSC 150 - Final Project

#The GaussJordanElimination function utilizes the Gauss-Jordan elimination method to solve linear systems. By transforming an augmented 
#coefficient matrix into reduced row-echelon form, it enables the determination of a unique solution, if available. The function takes the 
#matrix as input, performs row operations, and employs back-substitution to derive the solution vector. If a unique solution exists, the function 
#returns the vector; otherwise, it prints an error message and returns NA. This technique is crucial for efficiently solving linear systems, 
#notably in polynomial regression applications.
GaussJordanElimination <- function(matrix) {
  givenNumberOfRows <- nrow(matrix) #Obtain the number of rows in the matrix.
  givenNumberOfColumns <- ncol(matrix) #Obtains the number  of columns in the matrix.
  
  #This loop navigates the iterative implementation of Gauss Jordan.
  for (rowIndex in 1:givenNumberOfRows) {
    #This determines if the current row index is not the last row.
    if (rowIndex != givenNumberOfRows) {
      #Obtain the pivot element through getting the maximum absolute value within the rows.
      pivot <- max(abs(matrix[rowIndex:givenNumberOfRows, rowIndex]))
      #Acquire the row index of the pivot row.
      pivotrow <- which(abs(matrix[rowIndex:givenNumberOfRows, rowIndex]) == pivot) + rowIndex - 1
      #Determine if the chosen pivot element is present in multiple rows.
      if (length(pivotrow) > 1) {
        #Whenever there is the repetition, then we get the first instance of the pivot element.
        pivotrow <- pivotrow[1]
      }
      
      #This determines if the pivot element is already equal to 0 since it corroborates that there is no unique solution.
      if (matrix[pivotrow, rowIndex] == 0) {
        print("The unique solution does not exist.")
        return(NA)
      }
      
      #This permits the swap between the pivot row into the current row.
      temp <- matrix[rowIndex, ]
      matrix[rowIndex, ] <- matrix[pivotrow, ]
      matrix[pivotrow, ] <- temp
    }
    #Normalize the current row by dividing it by its pivot element
    matrix[rowIndex, ] <- matrix[rowIndex, ] / matrix[rowIndex, rowIndex]
    #Perform row operations to create zeros in the leading column of other rows
    for (columnIndex in 1:givenNumberOfRows) {
      if (columnIndex != rowIndex) {
        #Gets the multiplier in the program.
        multiplier <- matrix[columnIndex, rowIndex]
        #Allows the subtraction of the multiplier to the current row.
        matrix[columnIndex, ] <- matrix[columnIndex, ] - multiplier * matrix[rowIndex, ]
      }
    }
  }
  #This creates the storage of the solution set which is initialized to zero.
  solutionSet <- rep(0, givenNumberOfRows)
  #This continuously obtain the right hand side which gets all of the coefficients that will be supplied in the augmented coefficient matrix which is placed in the solution set storage that is created above.
  for (rowIndex in 1:givenNumberOfRows) {
    solutionSet[rowIndex] <- matrix[rowIndex, givenNumberOfColumns]
  }
  return(solutionSet) 
}


#The quadraticSplineInterpolation function in R performs quadratic spline interpolation on a given set of data points. It constructs quadratic givenEquations, 
#solves for their coefficients, and generates quadratic functions for each interval. The function predicts the value at a specified point 
#x and returns the resulting quadratic functions (quadraticFunctions) and the predicted value (y).
quadraticSplineInterpolation <- function(data, x) {
  
  #Finds the number of intervals or the data points minus 1.
  lengthOfData <- length(data[[1]])-1 
  
  #Creates the storage list that gets the given equations in the program.
  givenEquations <- list()
  
  #Create the string for the storage of the function declaration.
  function_string = "function ("

  # Generate the equation for the 1st endpoint through indulging the function declaration in the first end point.
  functionDeclaration <- paste(function_string, "a1, b1, c1) ", sep = "")
  equation <- paste(data[[1]][1]^2, " * a1 + ", data[[1]][1], " * b1 + 1 * c1 + -", data[[2]][1], sep = "")
  givenEquations <- append(givenEquations, eval(parse(text = paste(functionDeclaration, equation, sep = ""))))
  
  #Allows us to obtain the equation for the internal knots which entails the first condition.
  for (knotIndex in 2:lengthOfData) {
    functionDeclaration <- paste(function_string, "a", knotIndex-1, ", b", knotIndex-1, ", c", knotIndex-1, ") ", sep = "")
    equation <- paste(data[[1]][knotIndex]^2, " * a", knotIndex-1, " + ", data[[1]][knotIndex], " * b", knotIndex-1, " + 1 * c", knotIndex-1, " + -", data[[2]][knotIndex], sep = "")
    givenEquations <- append(givenEquations, eval(parse(text = paste(functionDeclaration, equation, sep = ""))))
    functionDeclaration <- paste(function_string, "a", knotIndex, ", b", knotIndex, ", c", knotIndex, ") ", sep = "")
    equation <- paste(data[[1]][knotIndex]^2, " * a", knotIndex, " + ", data[[1]][knotIndex], " * b", knotIndex, " + 1 * c", knotIndex, " + -", data[[2]][knotIndex], sep = "")
    givenEquations <- append(givenEquations, eval(parse(text = paste(functionDeclaration, equation, sep = ""))))
  }
  
  #This also allows us to generate the equation that is derived from the last end point which is under condition 2.
  functionDeclaration <- paste(function_string, "a", lengthOfData, ", b", lengthOfData, ", c", lengthOfData, ") ", sep = "")
  equation <- paste(data[[1]][lengthOfData+1]^2, " * a", lengthOfData, " + ", data[[1]][lengthOfData+1], " * b", lengthOfData, " + 1 * c", lengthOfData, " + -", data[[2]][lengthOfData+1], sep = "")
  givenEquations <- append(givenEquations, eval(parse(text = paste(functionDeclaration, equation, sep = ""))))
  
  #For the third condition, it permits us to get the equation through equating the first derivative that is discernible at the interior knots.
  for (equationIndex in 2:lengthOfData) {
  functionDeclaration <- paste(function_string, "a", equationIndex-1, ", a", equationIndex, ", b", equationIndex-1, ", b", equationIndex, ") ", sep = "")
  equation <- paste(data[[1]][equationIndex]*2, " * a", equationIndex-1, " + ", 1, " * b", equationIndex-1, " + -", data[[1]][equationIndex]*2, " * a", equationIndex, " + -1 * b", equationIndex, " + 0", sep = "")
  givenEquations <- append(givenEquations, eval(parse(text = paste(functionDeclaration, equation, sep = ""))))
}

#Creates the list which will be holding the variables.
variables = list() 
for (equationIndex in 1:length(givenEquations)) {
  variables <- append(variables, names(formals(givenEquations[[equationIndex]])))
}

#We utilize this keywordi n order to obtain the variables that are distinct or unique within the list of variables in the program.
variables <- unique(variables) 

#We assume that the value a1 will be eliminated granted that it is equal to zero which encapsulates the fourth condition.
for (variableIndex in 1:length(variables)) {
  if (variables[[variableIndex]] == "a1") {
    variables <- variables[-variableIndex]
    break
  }
}

  #This allows the addition of column at the end called the right hand side which will contain the values of the constants.
  nameOfColumn <- c(unlist(variables), "RHS")
  #Initializes the creation of the augmented coefficient matrix.
  augmentedCoefficientMatrix <- matrix(0, nrow = length(givenEquations), ncol = length(nameOfColumn), dimnames = list(1:length(givenEquations), nameOfColumn)) 
  for (equationIndex in 1:length(givenEquations)) { #Continuously iterates in the equation that are given.
    #We deparse the given equations in order to facilitate its splitting.
    vectorEquation <- unlist(strsplit(deparse(givenEquations[[equationIndex]])[2], "\\ \\+ ")) 
    for (termIndex in 1:length(vectorEquation)) { #Conversely, we also iterate in every term of the equation, which allows us to split them using the strsplit funtion.
        term <- unlist(strsplit(vectorEquation[termIndex], "\\ \\* "))
        if (length(term) == 1) { #Whenever we reached the condition that it is already equal to 1, then it means that it is singular element and will serve as the value which will be placed to the RHS.
            augmentedCoefficientMatrix[equationIndex, nameOfColumn == "RHS"] <- eval(parse(text = term[1])) * (-1)
        } else {
            if (!term[2] %in% nameOfColumn) { #We simply ignore the case wherein it is equal to a1 considering that it is assumed to be equal to zero only. 
                next
            } else { #Otherwise, then we need to add the coefficient that are obtained to the righteous places in the matrix.
                augmentedCoefficientMatrix[equationIndex, term[2]] <- eval(parse(text = term[1]))
            }
        }
    }
}

  #The calculation of the spline coefficients is obtain through the utilization of the Gauss Jordan Elimination in the created augmented coefficient matrix.
  splineCoefficients <- GaussJordanElimination(augmentedCoefficientMatrix)
  #We assume that the obtained second derivative is said to be equal to zero whenever it reaches the first point.
  splineCoefficients <- c(0, splineCoefficients) 
  #Allows the creation of the list that is capable of storing the quadratic functions in the chose nintervals.
  quadraticFunctions <- list() 
  coefficientIndex = 1
  for (intervalIndex in 1:lengthOfData) { #Continuously iterates in the index till it reaches throughout the entire data length.
    functionDeclaration <- paste(function_string, "x) ", sep = "") #We also perform the necessary parsing procedures to extract the correct data that is needed.
    equation <- paste(splineCoefficients[coefficientIndex], " * x^2 + ", splineCoefficients[coefficientIndex+1], " * x + ", splineCoefficients[coefficientIndex+2], sep = "")
    quadraticFunctions <- append(quadraticFunctions, eval(parse(text = paste(functionDeclaration, equation, sep = ""))))
    coefficientIndex <- coefficientIndex + 3
  }
  #This checks if the value of the x is encapsulated by the range of the data points
  if (x < data[[1]][1] || x > data[[1]][length(data[[1]])]) { 
  y = NA
  } else {
    intervalIndex = 1
    for (segmentIndex in 1:lengthOfData) {
      #This condition determines and checks for the interval which beholds the value of x along with evaluating the value which will be stored to y..
      if (x >= data[[1]][intervalIndex] && x <= data[[1]][intervalIndex + 1]) {
        y = eval(parse(text = paste("quadraticFunctions[[", segmentIndex, "]](x)", sep = "")))
      }
      intervalIndex = intervalIndex + 1
    }
  }
  necessaryValues = list( #We also create a labeled list which entails the obtained quadratic functon within the interval along with the formidable predicted value of x in the program. 
    quadraticFunctions = quadraticFunctions,
    y = y
  )
  return (necessaryValues)
}