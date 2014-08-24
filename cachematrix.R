## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(k = matrix()) {
  m <- NULL
  x <-NULL
  orig <- NULL
  
  ## this function stores the original matrix to 'x'
  set <- function(k) {
    x <<- k
    m <<- NULL
  }
  set(k)
  
  ## return the original matrix from 'x'
  get <- function() x
  
  ## this function stores the inverse matrix of k
  setsolved <- function(original, solved){ 
    m <<- solved
    orig <<- original
  }
  
  ## this function returns the inverse matrix of k stored earlier
  getsolved <- function() m
  
  ##this function stores the original matrix used for calculation
  getoriginal <- function () orig
  
  ## the list defines the "interface" functions to makeCacheMatrix
  list(set = set, get = get,
       setsolved = setsolved,
       getsolved = getsolved,getoriginal = getoriginal)
  
}

## Write a short comment describing this function
##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(s=matrix(), ...) {
  ## Return a matrix that is the inverse of 's'
  
  ## let's retrieve the original matrix from s for checking
  original <- s$getoriginal()
  solved <- s$getsolved()
  mat <- s$get()
  ## check do we have matrix stored and is it the same matrix for which we have cached the result
  if(!is.null(solved) & !is.null(original))
  {
    ##check that matrix has not changed since caching
    if(all.equal(original, mat))
    {
      message("fetching cached result")
      ## we'll return the result matrix instead of the original
      return(s$getsolved())
    }
  }
  ## cached result not available for the matrix s, let's retirive and do the potentially costly calc
  ##matrix <- s$get()
  
  ##calculate the inverse of the original matrix, we assume it to be inversible
  m <- solve(mat, ...)
  
  ##store the result to s environment for caching purposes
  s$setsolved(mat, m)
  
  ## return the inverse matrix of s
  m
}
