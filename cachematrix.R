## This 2 functions creates allow to keep a matrix (the inverse of a matrix we 
## input) in the memory so it can be called in the future without having to be
## calculating it every time as it is computationally costly
##
## makeCacheMatrix() takes as an argument the matrix that we want to generate a 
## the inverse so that it can be stored in the memory to be cached later
## 
makeCacheMatrix <- function(x = matrix()) {
      m<-NULL
## We define the setter and getter of the matrix (x).
## This are program modules that set and retrieve data from objects respectively.
##    
##    set() is a function that takes a new matrix as an argument in order to 
##    be able to change it and call cacheSolve() directly.
##    The new matrix (y) is assigned to x which is defined in the parent 
##    environment so we assign it with (<<-). 
##    m is set as null, since we are 
##    defining a new matrix so that it resets in case there was a value saved 
##    
##     get() is a function that retrieves x, since we don't put it as an 
##     argument it looks for x in the parent environment
      set <- function(y) {  
      x <<- y
      m <<- NULL
      }    
      get <- function() x


## We defined the getter and setter of the inverted matrix
## 
##    setinv() is a function that sets the  inverse matrix. Since m is in the
##    parent environment we access it after the function is completed. 
##    getinv() is a function that retrieves the inverse matrix, since we 
##    dont put it as an argument it looks for m in the parent environment. 
##    At the moment is set as NULL
##    This are internal functions for cacheSolve() to use
##    

      setinv <- function(invmat) m <<- invmat
      getinv <- function() m 


## The result of the function will be a list where all the functions are stored
## We name the functions so that we can call them later with the $ sign
## set() is called set, get() is called get etc.
      list(set = set, get = get,
     setinv = setinv,
     getinv = getinv)
}

## To sum up, makeCacheMatrix() creates a list which is defined in the global 
## environment so that we can call it from other functions. And it contains
## the setters and getters for x and m
##
##
## cacheSolve() is a function that computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above (stored in the list). 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## 
## First, it sets m to be the matrix defined previously in makeCacheMatrix() using
## the $ 


cacheSolve <- function(x, ...) {
      m <- x$getinv()
## Now if !is.null() is TRUE,then m is not null, this means that m is saved from       
## before, since every time a new matrix is used, m is set to NULL 
## So the m is cached from the memory
      if(!is.null(m)) {       
            message("getting cached data")
            return(m)
      } 
## When m is NULL,(rest of cases) it means that we have set a new matrix and so
## the inverse hasn't been calculated yet.
## In this case, the new matrix is cached with get and store in data
## Then the new inverse matrix is calculated and saved in  m, so that is kept in
## the memory for future use 
## Lastly setinv() is used to set the inverted matrix of the input object and 
## the inverted matrix is returned
      data <- x$get()
      m <- solve(data, ...)
      x$setinv(m)
      return(m)
}