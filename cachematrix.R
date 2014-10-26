## These 2 functions, used in collaboration,  
## are intended for optimizing matrix calculation involving repeated use 
## of the inverse matrix. 
## makeCacheMatrix encapsulates a matrix and an inverse matrix cache 
## the value of which is to be systematically retrieved by the 
## "cacheSolve" function that will only calculate the inverse matrix the 
## first time it is called and retrieve it from the cache thereafter. 
##
## I tested these functions and they worked.
## So if you are having any issues confirming this works I leave my email: cfserre@gmail.com 
##Perhaps also inform TA if you contact me, for full transparency.
##
## I used this example of a matrix:  matrix(c(3,-7,5,2), nrow=2, ncol=2)
## the inverse of which, obtained with solve() function is:
##           [,1]        [,2]
## [1,] 0.04878049 -0.12195122
## 2,] 0.17073171  0.07317073
##
##I recommend splitting the source into 2 R scripts with 
## same name as function and “.R” suffix. Then source them
## with the source() command and be sure that the working directory is correct.
## Use the explanations below for test verifications. Eg, in clear:
## xxx<-matrix(c(3,-7,5,2), nrow=2, ncol=2)
## xxx
## you should see:
##      [,1] [,2]
##[ 1,]    3    5
## [2,]   -7    2
##
## y<-makeCacheMatrix(xxx)
## yy<-y$get()
## yy
## you should see:
##      [,1] [,2]
##[ 1,]    3    5
## [2,]   -7    2
## 
##  y$getinversematrix()
## you should see NULL
## im1<-cacheSolve(y)
## im1
## you should get the inversed matrix above
## y$getinversematrix()
## you should get the inversed matrix above
##
## Thanks!   


## makeCacheMatrix called with an inversable square matrix
## as its argument serves as a constructor of a special matrix object. 
## eg: myInversableSquareMatrix<- matrix(1:4, nrow=2, ncol=2)
##     myCacheMatrix<-makeCacheMatrix(myInversableSquareMatrix)
## The returned object ("myCacheMatrix") is a list 
## of named functions that provide  
## "set" and "get" methods respectively setting a matrix passed in parameter 
## and returning the matrix, as well as  
## "setinversematrix" and "getinversematrix" methods to respectively
## set a cached inverse matrix and to retrieve it. 
## The functions/methods of the returned 
## object are named (get, set, getinversematrix, setinversematrix)  
## and can be invoked on the object with the $ operator 
## as in thefollowing example: 
## myInverseMatrix<-myCacheMatrix$getinversematrix() 
##
## For optimization the "getinversematrix" method should NOT be called 
## directly. Instead, to benefit from the inverse matrix cache, 
## call the cacheSolve function with your 
## CachedMatrix as a parameter, eg:
## myInvMatrix<-cacheSolve(myCacheMatrix) 

makeCacheMatrix <- function(x = matrix()) {
 
  inversematrix <- NULL
  set <- function(y) {
    x <<- y
    inversematrix <<- NULL
  }
  
  get <- function() x
  setinversematrix <- function(invmat) inversematrix <<- invmat
  getinversematrix <- function() inversematrix
  list(set = set, get = get,
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix
       )
}


## cacheSolve returns the cached inverse matrix from within 
## an object created with the makeCacheMatrix() function that is 
## passed to it as a parameter, if it is not NULL. Otherwise 
## the inverse matrix is calculated and set in the cache of the CachedMatrix 
## and its value is returned to the caller.  
##  
cacheSolve <- function(x, ...) {
  
  im <- x$getinversematrix()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  aMatrix <- x$get()
  im <- solve(aMatrix, ...)
  x$setinversematrix(im)
  im
  
}
