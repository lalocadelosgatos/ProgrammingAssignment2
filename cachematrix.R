##This function creates a square matrix, calculate its inverse and caches this information

##This function creates a matrix with random numbers of 10 columns and 10 rows, 
#establishing the value, obtaining it and later establishing the inverse of the matrix and obtaining it


makeCacheMatrix <- function(x = matrix(rnorm(100), nrow = 10, ncol = 10)) {
  m<-NULL
  set<-function(y) {
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setsolve<-function(solve) m<<-solve
  getsolve<-function() m
  list(set=set,get=get,
       setsolve=setsolve,
       getsolve=getsolve)
  
}


##This function calculates the inverse of the matrix of the 
#previous function and saves it in the cache memory

cacheSolve <- function(x, ...) {
  m<-x$getsolve()
  if(!is.null(m)) {
    message("getting cache data")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$setsolve(m)
  ## Return a matrix that is the inverse of 'x'
}

