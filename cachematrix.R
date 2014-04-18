##  This function will create a new matrix square matrix
##  where x is the size of the new square matrix

makeCacheMatrix <- function(x = matrix()) {
  
      #makes sure we have an empty variable
      i <- NULL
      ## Create a list to store values
      set <- function(y) {
               x <<- y    #Link to object
               i <<- Null #Holder for the inverse
      }
      get <- function() x
      setinvers <- function(solve) i <<- solve 
      getinvers <- function() i
      list(set = set, get = get,
           setinvers = setinvers,
           getinvers = getinvers)
  }




cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    ## Attempts to get the casched value if it exits return it
    i <- x$getinvers()
    if(!is.null(i)) {
       message("getting cached data")
        return(i)
    }
    ## If it does not exits set calculate the value and save to I and return
    data <- x$get()
     i <- solve(data )
    x$setinvers(i)
    i
   
}
