## This is a pair of functions that cache the inverse of a matrix. 

## This function creates a special "matrix" object that can cache its inverse. It is a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) { # x is the value of stored matrix
 		i <- NULL    # initialises the cache to NULL
        set <- function(y) {   # sets y as the new value of the matrix
                x <<- y
                i <<- NULL    # resets the cache to NULL
        }
        get <- function() x    # gets the value of the matrix
        setinverse <- function(inverse) i <<- inverse   # sets the inverse of the matrix
        getinverse <- function() i     # gets the inverse of the matrix
        
        # create a list of functions described above
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function calculates the inverse of the special matrix returned by the makeCacheMatrix function above. If the inverse has already been computed and the matrix has not changed, then this function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		i <- x$getinverse()   # gets the cache of the inverse of the matrix from x created by makeCacheMatrix function
        if(!is.null(i)) {     # the cache is returned here only if x stores already calculated inverse of the matrix
        	                  # it writes then the message "getting cached data"
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)  # calculates the inverse of the matrix 
        x$setinverse(i)        # and stores it in as the cache in x
        i                      # returns the inverse of the matrix
}
