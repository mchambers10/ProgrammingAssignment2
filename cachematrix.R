##function 1 creates a list with the methods that are set and get a matix withs its inverse
##fucntion 2 has a list passed from the first fucntion thta attempts to solve and find its inverse
##If it already has an iverse, the cached value is used

## Write a short comment describing this function
##makeCacheMatrix creates a matrix x and uses 3 methods to get x and its inverse


makeCacheMatrix <- function(x = matrix()) {

	cachedInverse <- NULL ## initializing inverse


	## set x in the parent environment with what is wanted, if the inverse is already set we get rid of it 
	set <- function(userValue = matrix() ){
		x <<- userValue
		cachedInverse <<- NULL
	}
	
	get <- function() x

	## Inverse variable is set in parent environment with what is required and is returned as a convenience
	setInverse <- function(inverseVal) {
		cachedInverse <<- inverseVal	
		return(cachedInverse)
	}

	getInverse <- function() cachedInverse
	list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
		
}


## Write a short comment describing this function
##From the list of variables given from the first fucntion I am checking to see if there is a cached inverse already there and return it
##If there isn't a cached inverse we will try and solve its iverse and set or return it



cacheSolve <- function(x=makeCacheMatrix(1:4, nrow=2,ncol=2), ...) {
        ## Return a matrix that is the inverse of 'x'

	##want to see if something is already there 
	calcInverse <- x$getinverse()


	##checking if there is a cached value in matrix form
	if(!is.null(calcInverse) && is.matrix(calcInverse)){
		message("Cached Data has been found")
		return(calcInverse)
	}

	matrixToCalc <- x$get()


	##trying to solve matrix while catching errors and warnings
	calcInverse <- tryCatch( {
		solve(matricToCalc)
	}, warning=function(warning) {
		message("This might not be the answer you were hoping for")
		message(warning)
	}, error = function(error){
		message("Something has gone wrong when solving your matrix")
		message(error)
		message("\n")

	})

	## whatver happne the value of inverse is set to 
	message("The value of the inverse is set to :")
	x$setInverse(calcInverse)
	
}
