
# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

## The function to accomplish the above is:

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

## The function is:

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()           ## Return a matrix that is the inverse of 'x'
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
        
}

#########################################################
#################### Sample Run #########################
#########################################################
 
x=matrix(c(3, 2, 1, 4), ncol=2, nrow=2)
m = makeCacheMatrix(x)
m$get()

#[,1] [,2]
#[1,]    3    1
#[2,]    2    4



cacheSolve(m)

#[,1] [,2]
#[1,]  0.4 -0.1
#[2,] -0.2  0.3

#########################################################
############################# THE END ###################
#########################################################