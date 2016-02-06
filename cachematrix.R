#Compute and cache the inverse of a matrix


makeCacheMatrix <- function(x) {
# creates a special matrix that will be inverted by cacheSolve
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse = function(inverse) i <<- inverse 
        getinverse = function() i
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        
        
        # if the inverse has already been calculated, retrieves from cache
        if (!is.null(i)){
                message("getting cached data")
                return(i)
        }
        
        #otherwise, inverts the matrix
        data <- x$get()
        i <-  solve(data, ...)
        
        x$setinverse(i)
        
        i
}