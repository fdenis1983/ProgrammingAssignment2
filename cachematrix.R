## makeCacheMatrix returns an object containing the matrix x and "methods"
## intended to deal with the matrix m and its inverse
makeCacheMatrix <- function(x = matrix())
{
    inv <- NULL
    set <- function(y)
    {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(invMatrix) inv <<- invMatrix
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Returns the inverse matrix of the matrix stored inside x.
## Uses the cached inverse matrix if possible.
cacheSolve <- function(x, ...)
{
    inv <- x$getinv()
    if(!is.null(inv))
    {
        message("getting cached data")
        return(inv)
    }
    mtrx <- x$get()
    inv <- solve(mtrx, diag(nrow(mtrx)), ...)
    x$setinv(inv)
    inv
}