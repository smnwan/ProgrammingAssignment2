## The makeCacheMatrix function accepts a matrix and caches values.
## The cacheSolve function accepts a matrix

## Write a short comment describing this function; caches the actual matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                                   # first make m NULL
        set <- function(y) {                        # define the set function as
                x <<- y                             #     set x in the set function to the value of the matrix
                m <<- NULL                          #     set m in the set function to NULL
        }                                           # end definition
        get <- function() x                         # define get function, it returns the matrix supplied
        setinverse <- function(solve) m <<- solve   # define setinverse function, assigns to m solve from external environment 
        getinverse <- function() m		    # define getinverse; returns m
        list(set = set, get = get,                  # returns a list
             setinverse = setinverse,
             getinverse = getinverse)
}


## The below function is used to get the inverse of the matrix supplied from cache if cached else derive the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()				# assigns m as value of getinverse on x; retrieves cached inverse if any
        if(!is.null(m)) {				# check to see if m is not null
                message("getting cached data")		#		not null so tells user that it's getting results from cache
                return(m)				#		and then returns the inverse
        }						# endcheck
        data <- x$get()					# assigns to data the value of the get function on x (supplied matrix)
        m <- solve(data, ...)				# assigns to m the results of the solve; getting the inverse
        x$setinverse(m)					# caches the inverse using the setinverse function
        m						# returns m
}
