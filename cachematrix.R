## The following two functions can be used to reduce computation by 
## caching the inverse of a matrix and using the cached inverse instead
## of recomputing it.

## Creates a special matrix object which can be accessed by setter/getter 
## functions and caches the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) 
{
	m_inv <- NULL
	set <- function(y)
	{
		x <<- y
		m_inv <<- NULL
	}
	get <- function() {x}
	setInverse <- function(inverse) m_inv <<- inverse
	getInverse <- function() m_inv
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## Checks if the special matrix object has a cached inverse.
## Returns cached value if found, or else it computes the inverse
## by using the solve function.

cacheSolve <- function(x, ...) 
{
	m_inv <- x$getInverse()
	if (!is.null(m_inv)) 
	{
		message("Fetching from cache")
		return(m_inv)
	}
	m_inv <- solve(x$get(), ...)
	x$setInverse(m_inv)
	m_inv
}
