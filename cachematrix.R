#The following two functions are a way of using the scoping rules in R to
#create a cacheable version of a Matrix's inverse.


#makeCaheMatrix takes a matrix argument, calculates its inverse, and caches
#the result.

makeCacheMatrix <- function(u = matrix()){
  minv <- NULL
  
  set <- function(y) {
    u <<- y
    minv <<- NULL
  }
  
  get <- function() u
  setinv <- function(solve) minv <<- solve
  getinv <- function() minv
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
  
}

#This functions solves for the inverse of the Matrix created by makeCacheMatrix.
#if no value is stored, the solve() function will run, otherwise it will return the value
#of minv

cacheSolve <- function(u, ...){
  minv <- u$getinv()
  
  if(!is.null(minv)) {
    message("retrieving cached matrix")
    return(minv)
  }
  
  data <- u$get()
  minv <- solve(data, ...)
  u$setinv(minv)
  minv
}
