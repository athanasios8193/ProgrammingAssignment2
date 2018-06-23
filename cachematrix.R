## The purpose of the makeCacheMatrix and cacheSolve 
## functions are to receive a matrix from the user,
## calculate the inverse of that matrix, and store the
## result of the inverse operation to save computer memory.

## The "makeCacheMatrix" function receives a matrix (by default)
## object and stores that as the "makeCacheMatrix" environment
## as "x."
##
## It also defines the variable "m" as null for now. This
## variable later on will be used to store the value of the
## matrix inverse. It is itself a matrix because that is how
## linear algebra works.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {  ##This "set()" function allows
                              ## the user to set a new value of
                              ## "x" without rerunning the function
                x <<-y
                m <<- NULL    ## when "x" is reset so is "m"
        }
        get <- function() x   ## this defines the get function
                              ## by retrieving from the parent 
                              ## environment
        ## this function is used in the cacheSolve
        ## to compute the inverse of the matrix
        setinverse <- function (solve) m <<- solve
        ## this function is also used in cacheSolve
        ## to compute the inverse of the matrix
        getinverse <- function () m
        ## the following code creates a list which will
        ## allow the cacheSolve function to read the data
        ## and retrieve it to calculate the inverse of the
        ## given matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following code stores the inverse of the matrix provided
## in the makeCachematrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        ## the if loop checks to see if "m" is empty or not
        ## if "m" is not empty, it means that the matrix inverse
        ## had already been computed and will share the stored
        ## value
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## if "m" was empty, the following code runs
        ## the first line stores the matrix in the "data" variable
        data <- x$get()
        ## then the inverse is computed and stored in the "m" variable
        m <- solve(data, ...)
        ## the "setinverse" function from "makeCacheMatrix" is invoked
        x$setinverse(m)
        ## the inverse is printed to the console
        m
}

## now by passing a matrix to the "makeCacheMatrix" function
## and then passing the result of THAT to the "cacheSolve,"
## you can store the values of the inverses of matrices
## and potentially save a lot of memory