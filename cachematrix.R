## Put comments here that give an overall description of what your
## functions do

## This function initialize the value of inv to null everytime it is called
## makeCacheMatrix contains all the functions that assign and get the value of
## the inverse Matrix

makeCacheMatrix <- function(x = matrix()) {
	inv<-NULL
	get<-function()x

	setInverse<-function(inverse) inv<<-inverse
	getInverse<-function() inv

	list(get=get,setInverse=setInverse,
		getInverse=getInverse)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  ## This function calculates the inverse of the matrix
	  ## It checks first if the inverse was already calculated (calling getInverse function)
	  ## If it wasn't already calculated, it calculates it and assigns its value calling get and set functions
{
   inv<-x$getInverse()
   if(!is.null(inv)) {
    message("getting cached data")
	return(inv)
    }

   data<-x$get()
    inv<-solve(data,...)
    x$setInverse(inv)
    inv
}
}
