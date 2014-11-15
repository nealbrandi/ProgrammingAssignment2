# The following 2 functions implement an optimized matrix inversion via caching. 

# makeCacheMatrix - Creates a special "matrix" object that can cache it's
#                   inverse

makeCacheMatrix <- function(x = matrix()) 
{
    # Requires: x to be a square invertible matrix
    # Returns : A cacheMatrix object capable of storing it's inverse
    
    inverse <- NULL
    
    set <- function(y) 
    {
        # Set object matrix and clear inverse matrix via "superassignment" 
        
        x <<- y
        
        inverse <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(i) inverse <<- i
    
    getInverse <- function() inverse
    
    list(set        = set, 
         get        = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


# cacheSolve - Computes the inverse of the special "matrix" 
#              returned by makeCacheMatrix above

cacheSolve <- function(x, ...) 
{
    # Requires: x to be an object of type cacheMatrix 
    # Returns: A matrix that is the inverse of 'x'
    
    inverse <- x$getInverse()
    
    if (!is.null(inverse)) 
    {
        # Return cached inverse
        
        return(inverse)
    }
    
    # Compute and caching inverse
    
    data <- x$get()
    
    inverse <- solve(data, ...)
    
    x$setInverse(inverse)
    
    inverse
}