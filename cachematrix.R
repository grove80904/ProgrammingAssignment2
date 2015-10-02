## These functions will calculate a matrice's inverse and store it in cache memory

## Create 4 functions in 1
## Set the matrix to be whatever you want
## Return the chosen matrix
## Store the inverse of the chosen matrix
## Get the inverse of the chosen matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        seti <- function(inv) m <<- inv
        geti <- function() m
        list(set=set, get = get, seti = seti, geti = geti)
}
a <- makeCacheMatrix()

## Get the stored inverse.  
## If it exists, then display message and the inverse.  
## If it doesn't exist, then get the matrix, solve for the inverse and store it to memory

cacheSolve <- function(x, ...) {
        m <- a$geti()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- a$get()
        m <- solve(data, ...)
        a$seti(m)
        m
}