# The examples makeVector and cachemean were rewritten to create makeCacheMatrix and cacheSolve.
# No big changes; just changed mean into solve, made x a matrix and changed the names.


# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix  <- function(x = matrix()) {
	# default x is an empty matrix
	# make sure the inverse m is empty
	m <- NULL		

	# define the set function
        set <- function(y) {
		# set x
            x <<- y
		#x make sure the inverse m is emptied when set function is called
            m <<- NULL
        }
        
        # define the get function; retrieve x
        get <- function() x
	
	# define the setsolve function; assign the inverse to m
        setsolve <- function(solve) m <<- solve
      
	# define the getsolve function; retrieve the inverse m
	getsolve <- function() m
       
	# make a list of these four functions
	list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated, then cacheSolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {

        # retrieve the cached inverse m
        m <- x$getsolve()

	# if inverse m is not NULL; the inverse has been cached before 
	if(!is.null(m)) {
                # in that case the cached m is returned, the function ends
                message("getting cached data")
                return(m)
        }

	# if inverse m is NULL, it either has not been calculated yet
	# or the "matrix" was changed by using the $set again
	  
	# retrieve the matrix x to be inverted with 
        data <- x$get()
	  
        # calculate the inverse of the matrix x
        m <- solve(data, ...)

	# put the inverted matrix in the cache
        
        x$setsolve(m)
	# show the inverted matrix as output
        m
}

# This was tested as follows with matrix M0 and M1
## M0 <- rbind(c(1, -1/4), c(-1/4,1))
## M1 <- rbind(c(2, -1/4), c(-1/4,2))
## M0
## M1
## mc0<-makeCacheMatrix()
## mc1<-makeCacheMatrix()
## mc0$set(M0)
## mc1$set(M1)
## cacheSolve(mc0)
## cacheSolve(mc0)
## cacheSolve(mc1)
## cacheSolve(mc1)
# The second time the answer was retrieved from cache
# Changing the special "matrix" requires using 'set', the cached inverse is removed in that case.
# Changing the original matrix M0 does not change the output of cacheSolve(mc0), 
# one needs to run mc0$set(M0) again in that case.
