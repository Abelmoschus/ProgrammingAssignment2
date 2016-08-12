## Put comments here that give an overall description of what your
## functions do

## First of all I decided to create the matrix in the object "x" so as to test my functions later

x<-matrix(as.integer(10*(rnorm(9,1))), 3, 3)

## The Cache object is the object that stores the information of the inverted matrix, by default, the value should be forced to NULL
## because there should be nothing. We will use then the environment of the other function. Because we only can invert matrix, 
## x=matrix() is needed.

Cache <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## As you can see in line 21, despite working with a matrix, a list is used. (say, you only get the original matrix
## by subsetting Cache(x)$get() )



## Once created the cache and its environment, the process of invert is to be created. An if...else loop is used to test
## "if there already exists a matrix in the cache, print that matrix and a message saying that the matrix was previously
## calculated and so returned; else, solve the matrix and save and print it on the screen". 

cacheinv <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
