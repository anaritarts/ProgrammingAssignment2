## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#The function below creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {  ##define argument "matrix";define inv as null
    inv <- NULL                                 
    set <- function(y) { #define set function to assign new value of matrix in parent environment
        x <<- y
        inv <<- NULL
    }
    get <- function() x   #function to get matrix x
    setinverse <- function(inverse) inv <<- inverse  #assigns value of inv in parent environment
    getinverse <- function() inv  #function to get inverse of the matrix
    list(set = set , get = get, 
         setinverse = setinverse,
         getinverse = getinverse) #refer to the function with the $operator
         
}


## Write a short comment describing this function
#The function below computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. If the inverse has
#already been calculated (and the matrix has not changed), then cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) { ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if (!isnull(inv)) {
        message("getting cache data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
