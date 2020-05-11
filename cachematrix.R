## The following functions are used to create the inverse matrix from input matrix, 
## store the result in the 'cache' and return the data from the 'cache' when input matrix is not changed in order to skip unnecessary calculations

## The function 'makeCacheMatrix' is used to create the "cache" object 
## containing a list of fuctions, input matrix 'x' and inverse matrix 'Inv_Matrix' 
## which is set to empty matrix upon initialization

makeCacheMatrix <- function(x = matrix()) {
        Inv_Matrix <- matrix()
        set <- function (y) {               
                x <<- y
                Inv_Matrix <<- matrix()
        }
        get <- function() x
        setInv <- function(z) Inv_Matrix <<- z 
        getInv <- function() Inv_Matrix
        list(set = set,
             get = get,
             setInv = setInv,
             getInv = getInv)
}

## The function 'cacheSolve' calculates the inverse matrix when the cache is an empty matrix
## Otherwise it returns the data from the cache with the massage ("getting cached data")
## This function works only when the first argument is the object created by 'makeCacheMatrix'
## Ellipses are used to feed more aruments to solve() funciton

cacheSolve <- function(x, ...) {
        Inv_Matrix <- x$getInv()
        if(!is.na(Inv_Matrix[1,1])) {   ## The [1,1] element was specifically indicated to omit the warning message 
                                        ## "the condition has length > 1 and only the first element will be used"
                message("getting cached data")
                return(Inv_Matrix)
        }
        data <- x$get()
        Inv_Matrix <- solve(data, ...)
        x$setInv(Inv_Matrix)
        Inv_Matrix
}
