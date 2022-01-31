#creates augmented "matrix" structure which includes place to cache its inverse
makeCacheMatrix <- function(M = matrix()){
  invM <- NULL
  set <- function(N){
    M <<- N
    invM <<- NULL
  }
  get <- function() M
  setInv <- function(solve) invM <<- solve
  getInv <- function() invM
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


#checks if inverse of matrix has been calculated and, if not, calculates inverse
#and caches it in augmented matrix structure
cacheSolve <- function(M,...){
  invM <- M$getInv()
  
  if(!is.null(invM)){
    message("getting cached data")
    return(invM)
  }
  data <- M$get()
  invM <- solve(data,...)
  M$setInv(invM)
  invM
}
