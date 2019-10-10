## this funciton makes a cashe matrix that allows the cacheSolve function to check if the inverse of the matrix has already been computed 

makeCacheMatrix <- function(x = matrix()) { #making an empty matrix
    m <- NULL
    set <- function(y) { 
        x <<- y
        m <<- NULL # clears any value of m that had been cached by a prior execution of cacheSolve
    }
    get <- function() x # gets the value of x from the paternal enviroment of makeCacheMatrix
    setsolve <- function(solve) m <<- solve #defines the setter for the solve m
    getsolve <- function () m #defines geter for the solve m
    list( set = set, get = get, #assigns each of the created function as an element in a list and returns it to the parent enviroment
          setsolve = setsolve, 
          getsolve = getsolve)
}


## the cacheSolve funciton either populates and/or retrives the inversted matrix from an object of the type makeVector()
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    m <- x$getsolve() #retrive the inverse matrix from the object passed in the arugemnt 
    if(!is.null(m)) { #checks if the result is NULL, if it is not NULL it will return the cashed matrix to the parent enviroment
        message("getting cached data")
        return(m)
    }
    data <- x$get() #if the result of !is.null(m) is FALSE cacheSolve gets the matrix from the input object, calcualtes the inverse matrix and returns it to the parent enviroment 
    m <- solve(data, ...)
    x$setsolve(m)
    m
}