## cacheMatrix calculates the inverse of a square matrix using cache, 
## so, if the inverse of the matrix was previuosly calculated, 
## return that value without repeating the calculation


## makeCacheMatrix creates a special matrix with a collection
## of funcionts that allow to manage the object as a cached matrix
##
## ex.: 
##  m <- makeCacheMatrix(matrix(c(4,-2,-1,3,0,-2,3,-5,2),nrow=3))

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inv <<- inv
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve calculates the inverse of a makeCacheMatrix matrix
## So only the first time is actually calculted. Then, the cached
## value will be returned, unless the matrix is changed.
##
## The matrix passed is supposed to be invertible
##
## ex. :
##  m <- makeCacheMatrix(matrix(c(4,-2,-1,3,0,-2,3,-5,2),nrow=3))
##  m$get()
##  cacheSolve(m)
##  cacheSolve(m)
##  m$set(matrix(c(2,5,1,3),nrow=2))
##  m$get()
##  cacheSolve(m)
##  cacheSolve(m)

cacheSolve <- function(x, ...) {
    inv <- x$getin()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
