## Function makeCacheMatrix creates a special matrix. 
## Function cacheSolve calculates the "inverse" of the matrix.
##
## If the "matrix inverse" is already calculated, it use
## the matrix already created (so called "cached inverse matrix")
 
makeCacheMatrix <- function(x = matrix()) 
	{
    		invX <- NULL
    		set <- function(y) 
    		{
        		x <<- y
        		invX <<- NULL
    		}
    		get <- function() x
    		setinverse<- function(inverse) invX <<-inverse
    		getinverse <- function() invX
    		list(	set = set, get = get,
         		setinverse = setinverse,
         		getinverse = getinverse
             	     )
	}
 
## Function cacheSolve returns the inverse of the matrix created with -
## the makeCacheMatrix function.
##
## If the "cached inverse matrix" is available, function "cacheSolve" returns it, 
## if the "cached inverse matrix" is NOT available it create, caches, and returns it.

cacheSolve <- function(x, ...) 
	{
    		## Return a matrix that is the inverse of 'x'
    		invX <- x$getinverse()
    		if (!is.null(invX)) 
    			{
        			message("getting cached inverse matrix")
        			return(invX)
    			} else {
    			    	invX <- solve(x$get())
    			    	x$setinverse(invX)
    			    	return(invX)
    			       }
	}