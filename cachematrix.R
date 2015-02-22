## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        # This finds the inverse of matrix m
        m_inv <- NULL 
        
        # set new matrix value
        set <- function(x,...) {
                m <<- matrix(x,...)
                m_inv <<- NULL
        }
        
        # gets the old matrix value        
        get <- function() m
        
        # sets the new inverse matrix value
        set.inv <- function(solve) m_inv <<- solve
        
        # gets the old inverse matrix value
        get.inv <- function() m_inv
        
        # creates a special matrix object
        list(set = set, get = get,
             set.inv = set.inv,
             get.inv = get.inv)
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        # gets the known inverse matrix value
        m_inv <- m$get.inv() 
        
        # this tries to return the known inverse matrix value - if possible
        if(!is.null(m_inv)) {
                message("Yippee! Getting cached data!")
                return(m_inv)
        }
        
        # calculate inverse matrix value - if you don't have one. 
        data <- m$get()
        m_inv <- solve(data, ...)
        m$set.inv(m_inv)
        
        # returns calculated inverse matrix value
        m_inv
        
        
}
