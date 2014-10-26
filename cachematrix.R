## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix function creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL 
        set <- function(y){ ## Set & cache the values of matrix and its inverse
            x <<-y
            inv <<- NULL
       }
    get <- function() x ## Gets the value of matrix
    setinverse <- function(solve) inv <<- solve ## Sets the inverse
    getinverse <- function() inv ## Gets the value of the inverse from inv
    list(set=set, get=get, 
         setinverse=setinverse, 
         getinverse=getinverse)
}

## cacheSolve function calculates the inverse of the matrix returned from the 
## makeCacheMatrix function. If the given matrix is the same as before, and its inverse 
## has already been calculated, then the cacheSolve function retrieves the inverse from 
## the cache 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          inv <- x$getinverse() ## Gets the inverse from cache
          if(!is.null(inv)){
             message("getting cached data")
             return(inv)
         }
    data <- x$get() ## Gets the matrix value 
    inv <- solve(data, ...) ## Computes the inverse of the matrix
    x$setinverse(inv) ## Sets & caches the inverse
    inv
}
