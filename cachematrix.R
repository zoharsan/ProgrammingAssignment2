## These functions allows to cache a matrix inverse operation
## Author: Zohar Nissare-Houssen - zoharsan@gmail@com


## This function returns a special matrix which is list of matrix functions which 
## allow to set a matrix, get a matrix, as well as compute its inverse,
## or simply get its existing inverse

makeCacheMatrix <- function(x = matrix()) {
             m <- NULL
             set <- function(y) 
                    {
                      x <<- y
                      m <<- NULL
                    }
              get <- function() x
              setsolve <- function(solve) m <<- solve
              getsolve <- function() m
              list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## This function uses as an input variable the list returned by the function
## makeCacheMatrix and checks if the inverse of the matrix passed within is already
## cached. If so, it will display the cached result. Otherwise, it will compute it. 

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'

        m <- x$getsolve()
        if(!is.null(m)) {
                          message("Getting cached data")
                          return(m)
                         }
        data <- x$get()
        m <-solve(data,...)
        x$setsolve(m)
        m
}
