## makeCacheMatrix(x): Creates the set/get functions to cache a Matrix x and its Reversed Matrix.  
## cacheSolve(x): Calculates the Reverse of a Matrix (x) using the cached values from makeCacheMatrix  

## makeCacheMatrix returns a list with 4 functions as components: 
## $set: Caches the Matrix, clears the cached reversed matrix
## $get: Returns the cached Matrix
## $setsolve: Calculates the cached reverse, by calling the Solve function without parameter b (see ?solve)
## $getsolve: Returns the cached reverse matrix of x, 

makeCacheMatrix <- function(x = matrix()) {

    s <- NULL
    
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)

}


## cachesolve: tries to get the Reverse Matrix from cached values. 
## If no cached data: calculates the reverse and caches the result.
## if cached data is used, a message is displayed.

cacheSolve <- function(x, ...) { 
        ## Return a matrix that is the inverse of 'x'

    s <- x$getsolve()
    
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
        }

    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    
    s

}
