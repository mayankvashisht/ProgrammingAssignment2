##This function will give the special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x

##setinverse function will set the object m equal to the inverse of the input matrix.
        
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m

##list function will create a list of all function in makecachematrix function.
        
        list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)

}


## this function will calculate the inverse if its not in cache and if its already in cache memory then return that value.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        
##check whether the inverse is already calculated. if already in the cahe memory than give that value and return.
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
## if element is not in cache then following code will execute to find inverse of a matrix and save that value in cache
## by using setinverse functioni.
        
##solving for inverse of a matrix.
        
        m <- solve(data)
        x$setinverse(m)
        m
        m
        print(m)
## printing a matrix that is the inverse of 'x'
}
