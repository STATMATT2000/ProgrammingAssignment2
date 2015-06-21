## These functions makeCacheMatrix() and cacheSolve() work in concert with one another to cache a 
## computationally intensive inverse of a Matrix.  If it has been computed already it uses what is 
## in cache as the answer. 

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                                      # sets the matrix cache to null
  set <- function(y) {
    x <<- y                                      # creates a set function that reaches one level ouside of the current
    m <<- NULL                                   #   environment to assign y and m
  }
  get <- function() x                            # creates a get function that sets its value to the matrix
  setsolve <- function(solve) m <<- solve        # creates a setsolve function that assigns the solution to the cache
  getsolve <- function() m                       # creates a getsolve function that looks at what is inside the cache
  list(set = set, get = get,                     # aggregates the functions to be generated into a nice list of functions
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already 
##   been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
cacheSolve <- function(x, ...) {                # The functions list from makeCache matrix is fed into this function
                                                #   the ... will allow additional arguments into the solve function
  m <- x$getsolve()                             # getsolve function is obtained and checks to see if a solution exists
  if(!is.null(m)) {                             # if there is solution, submit a message and return the stored soln
    message("getting cached data")              # this solution is the one that is comming from the cashed value
    return(m)                                   # gets us out of the function cacheSolve with a displayed result
  }
  data <- x$get()                               # get function is obtained and fed the uncached matrix to be inverted
  m <- solve(data, ...)                         # the matrix is solved.  the ... argument allows addtional arguments to be
                                                #   inserted such as tolerance and arguments from other methods
  x$setsolve(m)                                 # runs the setsolve function which assigns the inverse to the cache in the
                                                #   environment for MakeCacheMatrix
  message("data saved to cache")                # outputs a message indicating the data is saved in the cache
  m                                             # outputs the inverse matrix
}


# Acknoledging John Barns 
# https://class.coursera.org/rprog-015/forum/thread?thread_id=442
M<-matrix(c(sample(6),sample(6),sample(6),sample(6),sample(6),sample(6)),6,6)
test.matrix <- M

# Acknoledging Chinmay Borwankar
# https://class.coursera.org/rprog-015/forum/thread?thread_id=414
test.fun <- makeCacheMatrix(test.matrix)
# First time it runs it will save the data to cache
cacheSolve(test.fun)
# Second time it runs it will read the date from cache
cacheSolve(test.fun)
