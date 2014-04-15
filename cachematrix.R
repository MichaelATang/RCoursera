## Matrix inversion is usually a costly computation and their may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        ## Place holder matrix vector	
	  m <- NULL  

        ## Function that sets the values of vectors x and m
        ## Setter Method
        set <- function(y) {   
                x <<- y
                m <<- NULL
        }
        
        ## Function that gets the value of vector x
        ## Getter Method
        get <- function() x
        

        ## Function that sets the inverse of matrix
        ## Setter Method
        setinverse <- function(inverse) m <<- solve(m)%*%m

	  ## Function that gets the value of vector m
        ## Getter Method
        getinverse <- function() m
        
        ## list of available method names
        ## with corresponding functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
	
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        ## m has the value of x
	  m <- x$getinverse()

        ## If calculated value if found in m
        ## then retrieve the "cached" value    
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }

        ## else calculate the inverse 
        data <- x$get()

        ## calculate inverse and store in m
        m <- inverse(data, ...)

        ## update "cached" vector with new 
        ## calculated value
        x$setinverse(m)

        ## return calculated value
        m	
}
