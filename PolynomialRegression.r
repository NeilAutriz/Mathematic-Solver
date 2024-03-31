#Autriz, Mark Neil
#CMSC 150 - AB5L Final Project
#Source code for the Implementation of Polynomial Regression.

#The provided R code implements Polynomial Regression using Gauss-Jordan Elimination. The Gauss-Jordan Elimination function 
#solves a system of linear equations, and the Polynomial Regression function fits a polynomial of specified degree to input data 
#points. This program calculates coefficients, predicts output for a given value, and prints relevant information, with detailed 
#comments for clarity.


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


#The PolynomialRegression function fits a polynomial of a specified degree to input data, calculates coefficients through Gaussian elimination, 
#constructs and evaluates the resulting polynomial function, and estimates output for a given value. The function provides key information, 
#including coefficients, estimated output, function string, and the constructed function, for analysis and interpretation.
PolynomialRegression <- function(x, y, degree, estimatedValue) {
  #This initializes the matrix that stores the coefficients in the polynomial.
  matrix <- matrix(0, nrow = degree + 1, ncol = degree + 2)
  #Determines the length of the input data.
  givenLength <- length(x)
  #This creates the vector which stores the value of every power of x.
  values <- rep(0, degree * 2)

  #Calculate values for each power of x up to 2 times the specified degree
  for (indexPower in 1:(degree * 2)) {
  values[indexPower] <- sum(x ^ indexPower)
}

#This creates the vector which places the values of the right hand side in the matrix. 
RHS <- rep(0, ncol(matrix))

#This solves for the values of the right hand side through utilizing the powers that corresponds to x along with the certain values in y.
for (columnIndex in 1:ncol(matrix)) {
  RHS[columnIndex] <- sum(x ^ (columnIndex - 1) * y)
}

#Iteratively places the computed values along with the right hand side (RHS) in the coefficient matrix .
for (row_index in 1:nrow(matrix)) {
  for (columnIndex in 1:ncol(matrix)) {
    if (row_index == 1 && columnIndex == 1) {
      #The first element of the matrix is the sum of input data points
      matrix[row_index, columnIndex] <- givenLength
    } else if (columnIndex == ncol(matrix)) {
      #We place the coefficient in the RHS of the matrix.
      matrix[row_index, columnIndex] <- RHS[row_index]
    } else {
     # Intermediate elements are populated with values representing powers of x
      matrix[row_index, columnIndex] <- values[row_index + columnIndex - 2]
    }
  }
}
  
  #To get the coefficients we utilize the function of the Gauss Jordan Elimination in our matrix.
  obtainedCoefficients <- GaussJordanElimination(matrix)
  #This creates the string that stores the values of the function in the program.
  functionString <- "function (x) "
  for (coefficient_index in 1:length(obtainedCoefficients)) {
  if (coefficient_index < length(obtainedCoefficients)) {
    #Yields the polynomial string which contains the coefficient values along with their powers.
    functionString <- paste(functionString, obtainedCoefficients[coefficient_index], " * x^", coefficient_index - 1, " + ", sep = "")
  } else {
    functionString <- paste(functionString, obtainedCoefficients[coefficient_index], " * x^", coefficient_index - 1, sep = "")
  }
}
  #This also checks and evaluates for the function that is obtained.
  polynomialFunction <- eval(parse(text = functionString))
  #This also stores the value that is estimated in the program.
  estimatedValueY <- polynomialFunction(estimatedValue)
  
  #This printing statement allows to print the necessary values in the function namely the coefficient, estimated y for x, function string along with the constructed function.
  cat("Coefficients:", obtainedCoefficients, "\n")
  cat("Estimated y for x =", estimatedValue, "is", estimatedValueY, "\n")
  cat("Function String:", functionString, "\n")
  cat("Constructed Function:\n")
  print(polynomialFunction)
  
  return(list(coefficients = obtainedCoefficients, functionString = functionString))
}