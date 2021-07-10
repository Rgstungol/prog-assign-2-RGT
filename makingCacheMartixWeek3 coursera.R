makeCacheMatrix <- function(m= matrix()){
  i <- NULL
  set <- function(n){
    m <<- n
    i <<- NULL
  }
  get <- function() {m}
  setInv <- function(inv) (i <<- inv)
  getInv <- function() {i}
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


cachesolve <- function (m, ...){
  inv <- m$getInverse()
  if(!is.null(i)){
    message("getting inverse chached data")
    return(i)
  }
  mat <- m$get()
  inv <- solve(mat, ...)
  m$setInv(i)
  i
}

