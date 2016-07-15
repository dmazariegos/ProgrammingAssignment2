## The funcions calculate the inverse of a special "Matrix"
## using the Scoping concepts

## In this function I define the special Matrix and his attributes

makeCacheMatrix <- function(x = matrix()) 
{
    m <- NULL                                 #Letter that represents the special Matrix
    set <- function(y) {                      #Function that sets a value in the special Matrix
        x <<- y
        m <<- NULL
    }
    get <- function() x                       #Function that return the value of a special Matrix
    setinv <- function(solve) m <<- solve     #Function that set the result of apply solve() function in Letter thar represents de special Matrix
    getinv <- function()                      #Function that return the inverse matrix result
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## In this function I calculate, if it's necessary, the inverse matrix

cacheSolve <- function(x, ...) {
    m <- x$getinv()                          #Letter that receibes a possible previous result of inverse matrix
    if(!is.null(m)) {                        #If inverse matrix is already calculated, then only return the result
        message("getting cached data")
        return(m)
    }
    data <- x$get()                         #Else calculate and set de inverse matrix in variable Letter m
    m <- solve(data)
    x$setinv(m)
    m
}
