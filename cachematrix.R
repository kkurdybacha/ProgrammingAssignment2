## function makeCacheMatrix is creating special object 
## capable of holding matrix and its inverse
## object has inner properties to hold data
## and methods for getting/setting matrix and it's inverse
## matrix can be set while creating object or later,
## by using setting method

makeCacheMatrix <- function(mx = matrix()) {

    ## preparing empty place for matrix inverse
	  mxInv <- NULL
	
	  ## defining inner setting matrix function
	  ## the only way to change matrix will also
	  ## reset inverse in preparation for new value
	  set <- function(m) {
		    ## setting value for matrix in parent function
		    mx <<- m
		    ## claaning saved matrix inverse in parent function
		    mxInv <<- NULL
	  }
	
	  ## defining inner matrix getting function
	  get <- function() { mx }
	
	  ## function for setting inverse
	  setinv <- function(inv) { 
		    ## setting matrix inverse in in parent function
		    mxInv <<- inv 
	  }

	  ## defining inner inverse getting function
	  getinv <- function() { mxInv }
	
	  ## preparing return value
	  ## list with getting/setting function
	  ## method for manipulating object
	  list(set = set,
		    get = get,
		    setinv = setinv,
		    getinv = getinv
	  )
}


## function designed for operating on an object created by 
## makeCacheMatrix. 
## Function will use "object" methods for getting the value of matrix
## inverse. If there is not any in cache embedded in object, function
## will compute one, store it for future use, and return it as result.
## If there is inverse in object cache, function will get it and 
## return it as result without computing again

cacheSolve <- function(cmx, ...) {
  
	  ## getting matrix inverse
	  inv <- cmx$getinv()
	  
	  ## checkin if there is not null
	  if(!is.null(inv)) {
	    
	      ## writing message indicating using cached value
  		  message("getting cached data")
	    
	      ## returning cached value
	  	  return(inv)
	  }
	  ## getting origin matrix
	  mx <- cmx$get()
	
	  ## computing inverse with given parametrs
	  inv <- solve(mx, ...)
	
	  ## setting value for retain in cache
	  cmx$setinv(inv)
        
	  ## Return a matrix that is the inverse of matrix in object
	  inv
}
