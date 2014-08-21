## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# 1. makeCacheMatrix() define a kind of matrix object offer 4 methods to get/set matrix data and it's inverse. 
# 2. When the function was called with inputing a matrix, it saves it to global enviroment as a cache. But it will not 
#    save it's inverse initially until setCache() method was utilized by other calls. Once a inverse cache formed, the 
#    next call of getting the matix inverse could use the getCache() method. The other functions get() and set() was 
#    design to get/set the data matrix itself from/to global enviroment.
# 3. Another internal function calculateInverse() was used to calcuate matrix inverse, when input data is squre matrix, 
#    it utilze solve() to get inverse, otherwise it use QR decomposition to get that.
makeCacheMatrix <- function(x = matrix()) {
    
    xInvCache <- NULL
    
    calculateInverse <- function(y, ...) {
        yDim <- dim(y)
        square.fg <- ifelse(yDim[1] == yDim[2], T, F) 
        if(square.fg) {
            mxI <- diag(yDim[1])
            yInverse <- solve(y, mxI, ...)
        } else {
            #if A=QR then (A-)=(R-)(Q-)=(R-)(Qt) and R- is easy to compute because R is triangular.
            QR <- qr(y, ...)    
            Q <- qr.Q(QR)
            R <- qr.R(QR)
            mxI <- diag(dim(R)[1])
            yInverse <- solve(R, mxI) %*% t(Q)
        }
        return(yInverse)
    }

    xInvCache <- NULL
    set <- function(y) {
            x <<- y
            xInvCache <<- NULL
    }
    get <- function() x
    setCache <- function(y) {
        xInvCache <<- list(x=y, inverse=calculateInverse(y))
    }
    getCache <- function() xInvCache

    list(set = set, get = get, setCache = setCache, getCache = getCache)
}


## Write a short comment describing this function
# 1. cacheSolve() offer chche mechanism to provide the inverse of input matrix object which is defined by makeCacheMatrix().
# 2. It will return the cache version of inverse if it is exists, else re-calcuate it and save to cache and return that.
# 3. The cache saving mechanism will save data itself and it's inverse simultaneous. It will check the data itself is 
#    the original one or not. And then return it's inverse if yes, or update the cache with new data and it's inverse 
#    which was get by setCache() method of this object.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    data <- x$get()
    xInvCache <- x$getCache()
    if(!is.null(xInvCache)) {
        message("getting cached data")
        if(all(xInvCache$x == data)) {
            return(xInvCache$inverse)
        }
    } else {
        message("Setting cached data")
        x$setCache(data)
        return(x$getCache()$inverse)
    }
}
