#Mark Neil G. Autriz
#CMSC 150 - Final Project

#GaussJordanMethod is an R function tailored for the simplex method in linear programming. It employs Gauss-Jordan elimination on an 
#augmented coefficient matrix. Key steps include selecting a pivot column, calculating test ratios to find the pivot row, normalizing 
#the pivot row, and zeroing out other elements in the pivot column. The function returns the modified matrix, but if all test ratios 
#are infinite (indicating no valid pivot row), it returns NA. It includes commented-out code for extracting a solution vector and a 
#debugging print statement ("before return test"). Overall, this function is essential for the simplex method in solving linear programming 
#problems.

GaussJordanMethod <- function(augmentedCoefficientMatrix) { #The code defines a Gauss-Jordan elimination method in R, specifically crafted to support the simplex method in solving linear programming problems by transforming an augmented coefficient matrix.
  #Allows you to create the storages for the number of rows and columns in the given matrix.
  givenRows <- nrow(augmentedCoefficientMatrix)
  givenColumns <- ncol(augmentedCoefficientMatrix)
  
  
  #Identify the pivot column by selecting the one with the minimum value in the last row of the augmented matrix (excluding the last two columns).
  columnForPivotting <- which(augmentedCoefficientMatrix[givenRows, 1:(givenColumns - 2)] == min(augmentedCoefficientMatrix[givenRows, 1:(givenColumns - 2)]))
  
  #Finds the test ratios for each non-basic variable and determine the pivot row based on the minimum ratio.
  testRatio <- c()
  for (rowIndex in 1:(givenRows - 1)) {
    if (augmentedCoefficientMatrix[rowIndex, columnForPivotting] > 0) {
        testRatio <- c(testRatio, augmentedCoefficientMatrix[rowIndex, givenColumns] / augmentedCoefficientMatrix[rowIndex, columnForPivotting])
    } else {
        #Infinite ratio for non-positive pivot column entries.
        testRatio <- c(testRatio, Inf) 
    }
  }

  # Handle the case where all test ratios are infinite, indicating no valid pivot row.
  if (all(is.infinite(testRatio))) {
    augmentedCoefficientMatrix <- NA  # Set the augmented matrix to NA to signal no valid pivot row.
    return(augmentedCoefficientMatrix)
  }
  
  # Identify the pivot row based on the minimum test ratio.
  rowForPivotting <- which(testRatio == min(testRatio))
  
  #Normalize the pivot row by dividing all its elements by the pivot element.
  pivotElement <- augmentedCoefficientMatrix[rowForPivotting, columnForPivotting]
  augmentedCoefficientMatrix[rowForPivotting, ] <- augmentedCoefficientMatrix[rowForPivotting, ] / pivotElement
  #Eliminate other elements in the pivot column to make them zero in subsequent rows.
  for (rowIndex in 1:givenRows) {
    if (rowIndex != rowForPivotting) {
        factor <- augmentedCoefficientMatrix[rowIndex, columnForPivotting]
        augmentedCoefficientMatrix[rowIndex, ] <- augmentedCoefficientMatrix[rowIndex, ] - factor * augmentedCoefficientMatrix[rowForPivotting, ]
    }
  }
  
  #Extract the solution vector by populating the calculatedSolution array with values from the last column of the augmented matrix.
  calculatedSolution <- numeric(givenColumns - 1) 
  for (rowIndex in 1:givenRows) {
    calculatedSolution[rowIndex] <- augmentedCoefficientMatrix[rowIndex, givenColumns]
  }
  return(augmentedCoefficientMatrix)
}