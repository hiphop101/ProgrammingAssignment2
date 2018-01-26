## Put comments here that give an overall description of what your
## functions do

## This function has the getter setter methods to store and retrieve matrix and inverse matrix
makeCacheMatrix <- function(m = matrix()) {
    minv <- NULL
    get <- function() m
    getInv <- function() minv
    set <- function (y) {
        m <<- y
        minv <<- NULL
    }
    setInv <- function(y) {
        minv <<- y
    }
    list(set = set, get = get, getInv = getInv,setInv = setInv)
}


## This methods will try to retrieve inversed matrix. If it was previously cached,
# it will return the cached copy, else from the cache.
cacheSolve <- function(m, ...) {
    ## Return a matrix that is the inverse of 'x'
    if(!is.null(m$getInv())) {
        message("getting cached data")
        return(m$getInv())
    }
    m$set(m$get())
    
    minv <- solve(m$get())
    m$set(minv)
    minv
}

#TESTING BEGIN#
#mat <- matrix(c(5,3,4,6,7,8,1,1,2),nrow=3,ncol=3, byrow=TRUE)
#myCacheMat <- makeCacheMatrix(mat)

#myCacheMat$get()
#cacheSolve(myCacheMat)
#TESTING END#
