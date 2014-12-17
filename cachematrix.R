## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(cachedMatrix = matrix()) {
    CachedInv <- NULL
    get <- function() cachedMatrix
    setInv <- function(Inv) CachedInv <<- Inv
    getInv <- function() CachedInv
    list(get = get,
         setInv = setInv,
         getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x) {
    x.Inv <- x$getInv()
    if(!is.null(x.Inv)) {
        message("getting cached data")
        return(x.Inv)
    }
    x.Inv <- solve(x$get())
    x$setInv(x.Inv)
    x.Inv
}
