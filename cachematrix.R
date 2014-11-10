## The objective of this function is to reduce computation costs related to 
## matrix inversions by writing a pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse. 
#I constructed the function by using the example for the Assignment 2 and modifing the parameters

makeCacheMatrix <- function(x = matrix()) {
                m <- NULL
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
                get <- function() x
                setsolve <- function(solve) m <<- solve #Changed the mean function for solve that gives the inverted matrix 
                getsolve <- function() m    #also changed the name of the functions from XXXmean to XXXsolve
                list(set = set, get = get,
                     setsolve = setsolve,
                     getsolve = getsolve) #adapted the name of the functions
        
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache
##Similarly to the first function, I adapted the example and changed several parameters (mostly the name of the fuctions imsed makeCacheMatrix)

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
        ## Return a matrix that is the inverse of 'x'
}

###testing of the functions based on Gregory D. Horne' Discussion Forum post "Hints for Programming Assignment 2 - Lexical Scoping" (https://class.coursera.org/rprog-009/forum/thread?thread_id=164)
#amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
#amatrix$get()
#cacheSolve(amatrix)   # Computes, caches, and returns    matrix inverse
#amatrix$getsolve()  # Returns matrix inverse

#cacheSolve(amatrix)   # Returns cached matrix inverse using previously computed matrix inverse
#amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) # Modify existing matrix
#cacheSolve(amatrix)   # Computes, caches, and returns new matrix inverse
#amatrix$get()         # Returns matrix
#amatrix$getsolve()  # Returns matrix inverse