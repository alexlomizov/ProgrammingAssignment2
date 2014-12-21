## Matrix inversion is usually a costly computation.
## These function implement caching of matrix inverse.
## A mtrix is set (and can be reset) with makeCacheMatrix function.
## An inverse is retrieved (computed or read from cache) with
## cacheSolve function.


## Function makeCacheMatrix creates a list which contains
## (1) a stored matrix to solve (cacheMatrix);
## (2) a cashed inverse and a function to retrieve it (CachedInv & getInv);
## (3) a function to set/store cached inverse (setInv).
## Cached inverse (CachedInv) is set to NULL each time function is called, 
## i.e. when stored matrix is set/re-set

makeCacheMatrix <- function(cachedMatrix = matrix()) {
    CachedInv <- NULL
    get <- function() cachedMatrix
    setInv <- function(Inv) CachedInv <<- Inv
    getInv <- function() CachedInv
    list(get = get,
         setInv = setInv,
         getInv = getInv)
}


## Function cacheSolve retirieves an inverse for a matrix set w/makeCacheMatrix.
## It reads what is stored as cached inverse with makeCacheMatrix as x.Inv.
## Then it checks if a cached inverse exists, i.e. if x.Inv is not NULL.
## If it exists (x.Inv is not NULL) --> it's returned.
## If not (x.Inv is NULL) --> it's calculated, set to cache and then returned.

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
