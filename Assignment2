## makeCacheMatrix stores a matrix X in memory
makeCacheMatrix <- function(X = matrix()) {
  inverse <- NULL
  set <- function(Y){
    X <<- Y
    inverse <<- NULL
  }
  get <- function() X
  setinverse <- function(Inverse) inverse <<- Inverse
  getinverse <- function() inverse
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## find inverse using corpcor

cacheSolve <- function(X, ...) 
{
  if(require("corpcor")){
    print("corpcor is loaded")
  } else {
    print("attempting to install corpcor")
    install.packages("corpcor")
    if(require(corpcor)){
      print("corpcor installed")
    } else {
      stop("could not install corpcor")
    }
  }
  inverse <- X$getinverse()
  if(!is.null(inverse)){
    message("matrix is present in memory")
    return(inverse)
  }
  message("inverse is not present in memory; the inverse will be computed")
  data <- X$get()
  inverse <- pseudoinverse(data, ...)
  X$setinverse(inverse)
  inverse
}


#Use case 1
#square matrix
X <- matrix(rpois(25,3), nrow = 5)
cX <- makeCacheMatrix(X)
cX$get()
cacheSolve(cX)
cacheSolve(cX)
invX <- cacheSolve(cX)

#Use case 2
#rectangular matrix rows > cols
Y <- matrix(rpois(20,2), nrow = 5, ncol = 4)
cY <- makeCacheMatrix(Y)
cY$get()
cacheSolve(cY)
cacheSolve(cY)
invY <- cacheSolve(cY)

#Use case 3
#rectangular matrix rows < cols
Z <- matrix(rpois(20,1), nrow = 4, ncol = 5)
cZ <- makeCacheMatrix(Z)
cZ$get()
cacheSolve(cZ)
cacheSolve(cZ)
invZ <- cacheSolve(cZ)

#Use case 4
#multiplication must return identity or closer
invX %*% X 
X %*% invX
invY %*% Y 
Z %*% invZ
