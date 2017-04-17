## This function "makeCacheMatrix" creates a special “matrix” object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {     ## creates the specific matrix 
        inverse <- NULL                         ## creates an empty object called inverse 
        set <- function(y){                     
                x <<- y                         ## "<<-" operator assign a value to an object in an environment different from the current environment
                inverse <<- NULL
        }
        get <- function() x                     
        setInverse <- function(solve) inverse <<- solve  ## "solve" returns the inverse of the matrix
        getInverse <- function () inverse       
        list (set = set, get = get, setInverse = setInverse, getInverse = getInverse) ## list containing a function to set and get the value of the specific matrix and set ant get the value of its inverse
}        


## This function computes the inverse of the special “matrix” returned by makeCacheMatrix function above
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inverse <- x$getInverse()
        if(!is.null(inverse)){                  ## loop to check if the inverse has already been calculated
                message("Getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...) ## "solve" returns the inverse of the matrix
        x$setInverse(inverse)
        inverse
}
