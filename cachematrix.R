## Put comments here that give an overall description of what your
## functions do
## The first function, makeCacheMatrix creates 
## a special "matrix"object that can cache its inverse. 
## The first function returns a list of functions namely
## setMatrix : to set the value of the matrix
## getMatrix: to get  the value of the matrix
## setCacheInverse : to set the Inverse of a matrix
## getCacheInverse : to get the Inverse of a matrix


## Creating a matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix())
  {
  ## Initialising the inverse property 
   CacheInverse <- NULL
  ## Function to set the matrix and flushing the Inverse if its in cache
   setMatrix <- function(newMatrixValue) 
    {
      x <<-newMatrixValue
      CacheInverse <<- NULL
    }
  ## Function to get the stored Matrix
    getMatrix <- function() 
    {
      ## Returning the matrix
      x
    }
  ## Function to set the Inverse of a matrix
    setCacheInverse <- function(solve) 
   	{
      CacheInverse<<- solve
    }
  ## Function to get the Inverse of a matrix 
    getCacheInverse <- function() 
    {
      ## Returns the inverse
      CacheInverse
    }
  ## Returns a list of Functons
    list(setMatrix = setMatrix, getMatrix=getMatrix,
         setCacheInverse = setCacheInverse ,
         getCacheInverse = getCacheInverse)
 }

## This function will compute the inverse of the special matrix 
## returned by "makeCacheMatrix"above. 
## If the inverse has already been calculated and matrix does not 
## change then the "cacheSolve" will retrieve the inverse from 
## the cache.

cacheSolve <- function(y, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  
  m<-y$getCacheInverse()
  
  ## Returning inverse if already set
  
  if(!is.null(m))
  {
    message("getting cached data")
    return(m)
  }
  ## Getting the matrix 
  matrixdata<-y$getMatrix()
  ## Calculate the inverse of the matrix
  m<-solve(matrixdata)
  ## Setting the inverse so that it is stroed in cache
  y$setCacheInverse(m)
  ## Returning the inverse
  m
}
