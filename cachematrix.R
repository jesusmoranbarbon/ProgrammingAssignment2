## Put comments here that give an overall description of what your
## functions do


# Steps to execute:
#     -Download this file in R working directory
#     -Open RStudio and execute
#         source("cachematrix.R")
#         test_functions() 




## Write a short comment describing this function
# Creates a cacheMatrix 
# The constructor receives a matrix x and creates:
#     -matrix = x
#     -inverse with null
# There are two modifiers:
#     -set: saves the matrix and deletes the inverse
#     -setInverse: saves the inverse
# There are two observers:
#     -get: returns the matrix
#     -getInverse: returns the inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
# Calculates the inverse of a cacheMatrix x
# If the inverse is saved in cacheMatrix, only returns this value
# But if the inverse is not saved in cacheMatrix:
#     -Calculates the inverse
#     -Saves the inverse in cacheMatrix
#     -Returns the inverse
cacheSolve <- function(x, ...) {
  i <- x$getInverse ()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse (i)
  i
}




# Function that test makeCacheMatrix and cacheSolve:
#     -Create a cacheMatrix
#     -Calculate the inverse without cache
#     -Calculate the inverse with cache
#     -Change the matrix
#     -Calculate the inverse
test_functions <- function(){
  m <- makeCacheMatrix(rbind(c(1,3,3), c(1,4,3), c(1,3,4)))
  cat("Create a cacheMatrix:\n")
  print(m$get())
  
  cat("\nInverse without cache:\n")
  print(cacheSolve(m))
  
  
  cat("\nInverse with cache:\n")
  print(cacheSolve(m))
  
  cat("\nChange the content of matrix:\n")
  m$set(rbind( c(1,1,0), c(1,0,1), c(0,1,0) ) )
  print(m$get())
  
  cat("\nInverse:\n")
  print(cacheSolve(m))
}


