## Matrix inversion is usually a costly computation and there may be
## some benefit to caching the inverse of a matrix rather than compute
## it repeatedly (there are also alternatives to matrix inversion that
## we will not discuss here). Your assignment is to write a pair of
## functions that cache the inverse of a matrix.

## Computing the inverse of a square matrix can be done with the solve
## function in R. For example, if X is a square invertible matrix, 
## solve(X) returns its inverse.

## Write the following functions:
##  
## makeCacheMatrix: This function creates a special "matrix" object that
## can cache its inverse.
## makeCacheMatrix: takes a matrix as input and return's the input matrix
##
## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then the cachesolve should
## retrieve the inverse from the cache.
## cacheSolve: takes a matrix as input and returns a list. 
## list[[1]] is the inverse of the input matix
## list[[2]] a logical indicating whether the inverse was retrieved
## from the cache or was solved
##
## clearCacheMatrix: clears the cache.
##
## prgAssgn2: is an object that encloses makeCacheMatrix(), cacheSolve() and
## clearCacheMatrix(). 
## prgAssgn2: returns a list of closures (i.e. makeCacheMatrix(),
## cacheSolve(), clearCacheMatrix())

prgAssgn2 <- function() {
  myMat <- matrix(nrow = 0, ncol = 0)
  myMatInv <- matrix(nrow = 0, ncol = 0)
  myZeroMat <- matrix(nrow = 0, ncol = 0)
  
  makeCacheMatrix <- function(x = getMatrix(3)) {
    
    if (identical(myMat, x) == FALSE) {
      myMat <<- x
      myMatInv <<- myZeroMat
    }
    
    return(myMat)
  }
  
  ## forceSolve logical indicating whether matrix inverse should be
  ## forcibly solved even though cache might have been valid
  ##
  cacheSolve <- function(x, forceSolve = FALSE) {
    
    ## flag indicating whether the input matrix's inverse was retrieved
    ## from the cache or was solved 
    cachedSolution <- FALSE  
    
    if (forceSolve == TRUE) {
      myMatInv <<- solve(myMat)
      return (list(myMatInv, cachedSolution))
    }
    
    if( (identical(myMat, x) == FALSE) || 
        (identical(myMatInv, myZeroMat) == TRUE) ) {
      myMatInv <<- solve(myMat)
    }
    else {
      cachedSolution = TRUE
    }
    
    return(list(myMatInv, cachedSolution))
  }
    
  clearCacheMatrix <-function() {
    myMatInv <<- myZeroMat
  }
  
  list(makeCacheMatrix = makeCacheMatrix, 
       cacheSolve = cacheSolve,
       clearCache = clearCacheMatrix)

}

## A wrapper function to generate a square matrix
##
set.seed(20151025)

getMatrix <- function(sqMatDim = 3) {
  myZeroMat <- matrix(nrow = 0, ncol = 0)
  
  if (sqMatDim == 0) {
    print("Invalid Matrix Dimension")
    return(myZeroMat)
  }
  
  myMat <- matrix(data = rnorm(sqMatDim*sqMatDim), nrow = sqMatDim, 
                   ncol = sqMatDim)
}

## test_prgAssgn2: test programme to test makeCacheMatrix() and cacheSolve()
## the test output is dumped to a file dumpdata.R
##
## It tests the following cases :
## (1) test for normal case -- cold cache (i.e cache invalid)
## (2) test for  warm cache (i.e getting the inverse from cache)
## (3) test for cleared cache (i.e getting the inverse from cache)
## (4) retest for cold cache
##
## During each test, the input matrix is multiplied with its inverse and the output of
## this multiplication should be a unit matrix (of the same order).
test_prgAssgn2 <-function() {
  myOutputMatInv <- matrix(nrow = 0, ncol = 0)
  myOutputMatCachedInv <- matrix(nrow = 0, ncol = 0)
  isMatInvCached  <- vector()
  myOutputList <- list()
  cacheStatus <- c("")
  
  test_prgAssgnList <- prgAssgn2()
  
  ##
  ## test for normal case -- cold cache (i.e cache invalid)
  ##
  testCase <- c("test for normal case -- cold cache")
  myInputMat     <- test_prgAssgnList$makeCacheMatrix()
  myOutputList   <- test_prgAssgnList$cacheSolve(myInputMat)
  myOutputMatInv <- myOutputList[[1]]
  myTestUnitMat  <- round(myInputMat %*% myOutputMatInv, 3)
  isMatInvCached <- myOutputList[[2]]
  
  if (isMatInvCached == FALSE) {
    cacheStatus <- c("Success: Cache Miss -- Matrix Inverse Solved")
  }
  else {
    cacheStatus <- c("Error: expected a Cache Miss, but was a Cache Hit")
  }
  
  listOfDumpObjs <- c("testCase", "myInputMat", "myOutputMatInv", 
                      "myTestUnitMat", "isMatInvCached", "cacheStatus")
  dump(list = listOfDumpObjs, append = FALSE)
  
  ##
  ## test for  warm cache (i.e getting the inverse from cache)
  ##
  testCase <- c("test for warm cache")
  myOutputList <- test_prgAssgnList$cacheSolve(myInputMat)
  myOutputMatCachedInv <- myOutputList[[1]]
  myTestUnitMat  <- round(myInputMat %*% myOutputMatInv, 3)
  isMatInvCached <- myOutputList[[2]]

  if ( (isMatInvCached == TRUE) && 
       (identical(myOutputMatCachedInv, myOutputMatInv) == TRUE) ) {
    cacheStatus <- c("Success: Cache Hit  -- Matrix retrieved from the Cache")
  }
  else {
    cacheStatus <- c("Error: expected a Cache Hit but, was a Cache Miss")
  }
  
  listOfDumpObjs <- c("testCase", "myOutputMatCachedInv", 
                      "myTestUnitMat", "isMatInvCached", "cacheStatus")
  dump(list = listOfDumpObjs, append = TRUE)
  
  
  ##
  ## test for cleared cache (i.e getting the inverse from cache)
  ##
  test_prgAssgnList$clearCache()
  testCase <- c("test for cleared cache")
  myOutputList <- test_prgAssgnList$cacheSolve(myInputMat)
  myOutputMatInv <- myOutputList[[1]]
  myTestUnitMat  <- round(myInputMat %*% myOutputMatInv, 3)
  isMatInvCached <- myOutputList[[2]]
  
  if (isMatInvCached == FALSE) {
    cacheStatus <- c("Success: Cache Miss -- Matrix Inverse Solved")
  }
  else {
    cacheStatus <- c("Error: expected a Cache Miss, but was a Cache Hit")
  }

  listOfDumpObjs <- c("testCase", "myOutputMatCachedInv", 
                      "myTestUnitMat", "isMatInvCached", "cacheStatus")
  dump(list = listOfDumpObjs, append = TRUE)

  ##
  ## Retest for cold cache
  ##
  testCase <- c("retest for cold cache")
  myInputMat     <- getMatrix(sqMatDim = 4)
  myInputMat     <- test_prgAssgnList$makeCacheMatrix(myInputMat)
  myOutputList   <- test_prgAssgnList$cacheSolve(myInputMat)
  myOutputMatInv <- myOutputList[[1]]
  myTestUnitMat  <- round(myInputMat %*% myOutputMatInv, 3)
  isMatInvCached <- myOutputList[[2]]
  
  if (isMatInvCached == FALSE) {
    cacheStatus <- c("Success: Cache Miss -- Matrix Inverse Solved")
  }
  else {
    cacheStatus <- c("Error: expected a Cache Miss, but was a Cache Hit")
  }
  
  listOfDumpObjs <- c("testCase", "myInputMat", "myOutputMatInv", 
                      "myTestUnitMat", "isMatInvCached", "cacheStatus")
  dump(list = listOfDumpObjs, append = TRUE)
  
}
