## R Programming course
## Course id: rprog-032 
## Programming assignment 2 - Lexical Scoping (Caching)
## This assignment deals about writing a pair of functions that cache the inverse of a matrix there by saving computing time. 

## Functions include:

## makeCacheMatrix: This function used to cache the inverse matrix. It makes use of setters and getters for caching matrix and matrix inverse.
## cacheSolve: Deals with inverse matrix computation, checking if already inverse of a matrix available in the cache and returning the inverse accordingly.

## Given: For this assignment, assume that the matrix supplied is always invertible.

## makeCacheMatrix function

makeCacheMatrix <- function(x = matrix()) {
		matrixInverse <- NULL
		set <- function(y) {
				x <<- y
				matrixInverse <<- NULL
		}
		get <- function() x
		setInverse <- function(inv) matrixInverse <<- inv
		getInverse <- function() matrixInverse
		list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		 matrixInverse <- x$getInverse()
		if(!is.null(matrixInverse)) {
				message("getting cached data")
				return(matrixInverse)
		}
		data <- x$get()
		matrixInverse <- solve(data)
		x$setInverse(matrixInverse)
		matrixInverse
}

## Usage:
## create a square matrix. 
## send the new matrix created to makeCacheMatrix. 
## you can view the contents of the matrix by calling get() sub function in makeCacheMatrix function. 
##call the cacheSolve function for getting the inverse of matrix input.

## example:
## mat <- matrix(1:4, nrow=2, ncol=2)
## mat.compute <- makeCacheMatrix(mat)
## mat.compute$get()
## cacheSolve(mat.compute)