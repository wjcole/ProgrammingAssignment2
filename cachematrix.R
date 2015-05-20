## Functions to store the inverse of a matrix in the cache and to recall it.

## This function is used by the function cachesolve to interact with the cache.
makecachematrix <- function(x = matrix()) {
        
        # Note, in this function, that x represents the function to be inverted and m its inverse
        
        # Initialise m as null
        m <- NULL
        
        # The set sub-function: in the cache, set x as the input matrix and m as null
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        
        # The get sub-function: return x from the cache
        get <- function() x
        
        # The setinv sub-function: in the cache, set m as the inverse of the input
        setinv <- function(inv) m <<- inv
        
        # The getinv sub-function: return m from the cache
        getinv <- function() m
        
        # The output format
        list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## This function returns a matrix that is the inverse of 'x' from the cache or, if none exists, 
## calculates it.
## The input should be the output of the makecachematrix function above.
cachesolve <- function(x) {
        
        # Recall the inverse of the matrix from the cache
        m <- x$getinv()
        
        # If there is an inverse stored in the cache, return it
        if(!is.null(m)) {
                message("Getting cached data")
                return(m)
        }
        
        # If there is not an inverse, recall the matrix that was to be inverted (formerly called x)
        data <- x$get()
        
        # Calculate the inverse
        m <- solve(data)
        
        # Store the inverse in the cache for future reference
        x$setinv(m)
        
        # Return the inverse
        m
}

# Example syntax (note that repeating the command cachesolve(mcm) will recall the previous answer
# from the cache):
# x <- matrix(c(1,6,3,7),2)
# mcm <- makecachematrix(x)
# cachesolve(mcm)
