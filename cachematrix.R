## These functions are used to create a special object that stores a matrix and 
## caches its inverse. The second function checks to see if the inverse of a given
## matrix has already been calculated and cached before calculating its inverse.


## This function creates a special "matrix", but is really a list containing a 
## function to set the value of the matrix, get the value of the matrix, set the 
## value of the inverse matrix and get the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) { #Note the input for x must be a matrix
        m <- NULL #set m to NULL so that it may be used later
        set <- function(y) {
                x <<- y # set global values for x and m in the parent environment
                m <<- NULL
        }
## This portion of the function sets and gets the inverse of the given matrix so
## that those portions of the list may be used with cacheSolve
        get <- function() x #create a function that uses the previously set value of x
        setinversematrix<- function(solve) m <<- solve #set the inverse of the matrix
        getinversematrix <- function() m #again, use the previously set value of m 
## create a list containing 4 values, each of which are named        
        list(set = set, get = get,
             setinversematrix = setinversematrix,
             getinversematrix = getinversematrix)
}


## This function calculates the inverse matrix of the special "matrix" created
## with the above function. Before it calculates the inverse matrix, however, it
## first checks to see if the inverse matrix has already been calculated. If so,
## it gets the inverse matrix from the cache and skips the computation. Otherwise,
## it calculates the inverse matrix and sets the inverse matrix in the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinversematrix() #use the global values from the list created in the
                                  #function above
        if(!is.null(m)) {         #if m already has a value, as set in the above function
                message("getting cached data") #display this message
                return(m)  #return the cached inverted matrix
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinversematrix(m) #otherwise calculate the inverse matrix
        m #return the value of the inverse matrix
}
