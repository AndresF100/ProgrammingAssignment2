## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


#the makeCacheMatrix function returns a list composed of four functions, 
#these functions are:
#set: save the input matrix
#get: returns the saved matrix
#setinv: this function saves the inverse matrix
#getinv: returns the saved inverse matrix



makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function
#the cacheSolve function receives the list created by makeCacheMatrix
#use the getinv function to return the input matrix to reverse
#determines if the inverse matrix is null, if not null prints a message and 
#returns the inverse of the input matrix

#if the saved inverse matrix is null, use get to retrieve the input matrix and 
#store it in the data variable
#Calculate the inverse using the solve function and set this inverse matrix 
#using the setinv function,and finally returns the inverse matrix



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
        
}


#test:
#m1<-matrix(runif(25),5,5)
#m1
#cacheSolve(makeCacheMatrix(m1))
#m2<-solve(m1)
#identical(m2,cacheSolve(makeCacheMatrix(m1)))
#m1%*%m2

