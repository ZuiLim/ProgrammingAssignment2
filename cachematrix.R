## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Creates a functions list that sets and gets the input matrix, as well as gets and gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(inverse_val) {
        inv <<- inverse_val
    }
    getInv <- function() inv
    list(set=set, get=get, setInv=setInv, getInv=getInv)
}


## Write a short comment describing this function
## using functions list created by above as input, this function first tries to retrieve the cached inverse matrix, failing which it will compute for one

cacheSolve <- function(l_mat, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- l_mat$getInv()
    if (!is.null(inv)) {
        message("retrieving cached matrix inverse!")
        return(inv)
    }
    mat <- l_mat$get()
    inv <- solve(mat)
    l_mat$setInv(inv)
    message("generated new matrix inverse!")
    inv
}
