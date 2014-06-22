# Functions:
#    makeCacheMatrix(): Creates a special 'matrix' that can cache its inverse.  
#    cacheSolve(): This function computes the inverse of the special "matrix" 
#                   returned by makeCacheMatrix above, not recomputing if inverse already found

#### function
# makeCacheMatrix ():
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL    #set matrix inverse to null
        get <- function() x   #returns the matrix
        setinverse <- function(solve) m <<- solve    #caches the inverse matrix
        getinverse <- function() m   #returns the inverse
        list(get = get,
             setinverse = setinverse,
             getinverse = getinverse) 
}

#### function
#  cacheSolve(): 
#  This function computes the inverse of the special "matrix" 
#  returned by makeCacheMatrix above. If the inverse has already been 
#  calculated (and the matrix has not changed), then the cachesolve should 
#  retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {    #the inverse has already been calculated, use it
                message("getting cached data")
                return(m)  
        }
        data <- x$get()   #it hasn't been cached
        m <- solve(data, ...)  #find inverse matrix
        x$setinverse(m)   #cache the inverse matrix
        m  #return the inverse matrix
}
