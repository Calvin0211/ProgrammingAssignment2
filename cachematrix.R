## The inverse of a matrix is computed. However a cache is also used to store the inverse of the matrix.
##  When the inverse is again required instead of recomputing it, it is accessed from the cache.

## This function creates a special Matrix object, which is of the class list and has 4 items within it. These 4 items
## are functions which do the following 1
## 1. Set the value of the matrxi
## 2. Get the value of the matrix
## 3. Set the inverse of the matrix
## 4. Get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
        library(matlib) 
inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inv1) inv <<- inv1
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}
## This function computes the inverse of the special Matrix Object created using the above function. It first checks if the inverse 
## is stored in the cache and if it is stored it simply outputs the stored inverse. If the inverse is not there in the cache, then 
## it will compute the inverse using the solve function and then store the inverse in the cache and also outputs the inverse.

cacheSolve <- function(x, ...) {
        
         inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <-solve(data)   ## present in matlib package
        x$setinv(inv)
        message("No cached data")
        inv
}
