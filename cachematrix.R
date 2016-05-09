## These two R functions, used in conjunction, create a special "matrix" object whos inverse may be cached. When a matrix object created 
## with makeCacheMatrix is passed to cacheSolve, cacheSolve will first check to see if the inverse has already been computed and cached.
## If so, it is returned. Otherwise the inverse in computed, cached, and returned. 

## makeCachMatrix has one formal parameter, x, which is a square matrix (precondition: the matrix is assumed to be invertible).
## it creates a special "matrix", which is represented in memory as a list containing 4 function
# - set(): sets the value of the matrix
# - get(): gets the value of the matrix
# - setinverse(): sets the inverse of the matrix
# - getinverse(): gets the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL ## set the cached inverse to NULL (as it has not been inverted, by default)
        
        ## Sets the matrix and inverse of the matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        get <- function() x ## a getter for the matrix passed in as argument
        setinverse <- function(inverse) i <<- inverse ## a setter for the inverse of matrix x
        getinverse <- function() i ## a getter for the inverse of matrix x
        ## a list containing named variables representing functions set, get, setinverse and getinverse
        ## this list serves as a representation in memory of a special "cacheable" matrix
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
        
}


## cacheSolve takes a special "matrix", x, (created by makeCacheMatrix) and returns its inverse (precondition: the matrix is assumed to 
## be invertible). The inverse that is computed is cached by storing it in the makeCacheMatrix object.
## When computing the inverse, cacheSolve first checks if the makeCacheMatrix object passed as argument has had its inverse computed
## and cached, if so, it simply returns this cached inverse. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse() ## call getinverse on x to retrive cached inverse (if it exists, otherwise NULL is returned)
        
        ## if the cached inverse exists for this matrix (i.e. i != null, simply return the cached inverse)
        if (!is.null(i)) {
                message("getting cached inverse")
                return(i)
        }
        
        # Otherwise, use solve to compute the inverse of the matrix, cache it in the makeCacheMatrix object, and return it
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
