## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        mtrx <- NULL
        set <- function(y) { ## sets value for x and mtrx
                x <<- y
                mtrx <<- NULL
        }
        get <- function() x
        setInverse <- function(solved) mtrx <<-solved ## sets inverse matrix
        getInverse <- function() mtrx ## reports inverse matrix
        list(set = set, get=get, setInaverse=setInverse, getInverse = getInverse)
        
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above.   
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mtrx <- x$getInverse() ## gets value if Inverse already computed
        if(!is.null(mtrx)) { ## relays inverse matrix if already computed
                message("getting cached data")
                return(mtrx)
        }
        ## calculate inverse matrix
        data <- x$get()
        mtrx <- solve(data)
        x$setInverse(mtrx)  ## sets in cache so don't need to calculate again
        mtrx ## prints inverse matrix
}
