## Put comments here that give an overall description of what your
## functions do

## creating a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        invMatrix <- NULL
        
        #set the value of the Matrix
        setMatrix <- function(y) {
                x <<- y
                invMatrix <<- NULL
        }
        
        getMatrix <- function() x                              #get the value of the Matrix
        setInverse <- function(inverse) invMatrix <<- inverse  #set the value of the invertible matrix
        getInverse <- function() invMatrix                     #get the value of the invertible matrix
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse, getInverse = getInverse)
}

## computing the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        
        #get the value of the invertible matrix from the makeCacheMatrix function
        invMatrix <- x$getInverse()
        if(!is.null(invMatrix)) {                       #if inverse matrix is not NULL
                message("Getting Cached Invertible Matrix")   #Type message: Getting Cached Invertible Matrix 
                return(invMatrix)                             #return the invertible matrix
        }
        
        #if value of the invertible matrix is NULL then  
        MatrixData <- x$getMatrix()                     #get the original Matrix Data 
        invMatrix <- solve(MatrixData, ...)             #use solve function to inverse the matrix
        x$setInverse(invMatrix)                         #set the invertible matrix 
        return(invMatrix)                               #return the invertible matrix
        ## Return a matrix that is the inverse of 'x'
}

#####Testing#####

TestMatrix <- matrix(1:4,2,2)
TestMatrix
CacheMatrix <- makeCacheMatrix(TestMatrix)
CacheMatrix$getMatrix()
CacheMatrix$getInverse()
cacheSolve(CacheMatrix)

TestMatrix2 <- matrix(c(0,1,1,0,0,1,1,0,1),3,3)
TestMatrix2
CacheMatrix <- makeCacheMatrix(TestMatrix2)
CacheMatrix$getMatrix()
CacheMatrix$getInverse()
cacheSolve(CacheMatrix)

