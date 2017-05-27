## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## function to create a matrix able to catch its inverse input
 makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   set <- function(y) {
     x <<- y
     inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
	 }

## Write a short comment describing this function
## Now, the following function will compute the inverse of 
## the aforementioned function.  

 cacheSolve <- function(x, ...) {
            ## Return a matrix that is the inverse of 'x'
          inv <- x$getinv()
          if(!is.null(inv)) {
                message("getting cached result")
                return(inv)
              }
          data <- x$get()
          inv <- solve(data, ...)
          x$setinv(inv)
          inv
      }

#### Testing the program
##> mat1<- makeCacheMatrix(mat)
##> cacheSolve(mat1)
##[,1]       [,2]         [,3]       [,4]
##[1,] 1.73913390 -0.4103888 -1.416098184  0.6588861
##[2,] 0.05297559 -0.2349469 -0.322576989 -0.3452870
##[3,] 0.30241919  0.2478179 -0.003814663 -0.1765205
##[4,] 1.13207322  0.9797408 -1.435004928  0.4777470


