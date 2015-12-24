# this function creates a list
# that contains 4 member functions: set, get, setInv
# and getInv. 

makeCacheMatrix <- function(x = matrix()) {
  
  xinv <- NULL # this is where the inverted matrix is stored
  
  # A "set" function is used to set a matrix to object created by makeCacheMatrix function
  # the <<- operator is used to assign a value to an object in an environment
  # that is different from the current environment
  set <- function(y) {
    x <<- y
    xinv <<- NULL 
  }
  get <- function() x # returns the input matrix
  setInv <- function(inv) xinv <<- inv # sets the inverted matrix
  getInv <- function() xinv # returns the inverted matrix
  
  # now return a list that contains the above functions
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


cacheSolve <- function(x, ...) {
  m <- x$getInv() # get the inverted matrix from object x
  
  # it will be null if uncalculated.if the inversion result has already    
  # been calculated, then return the calculated inverted matrix 
  if(!is.null(m)) {
    message("getting cached data")
    return(m) 
  }
  # if it is indeed NULL, we use x$get to get the matrix object, solve it 
  # and then set it to the object and finally return the solved result
  data <- x$get() 
  m <- solve(data) 
  x$setInv(m) 
  m 
}

# Test
# generate a random square, non-singular matrix
# testM <- matrix(runif(9,1,30),3,3)
# generate the makeCacheMatrix object with this matrix
# testCache <- makeCacheMatrix(testM)
# from now on calculate or retrieve calculated inversion using the cacheSolve function

# testInvM <- cacheSolve(testCache)
# testInvM <- cacheSolve(testCache)
# testInvM <- cacheSolve(testCache)


