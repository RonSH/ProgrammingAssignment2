
## Function makeCacheMatrix will store a Matrix and its Inverse
## The object created is list containing additional functions.
## Values are not stored within the functions; rather, values are 
## stored in variables x and inv using the superassignment operator
## The variables x and inv therefore retain their values after function call ends

makeCacheMatrix <- function(x = matrix()) {
        ## ensure inverse is null when initial matrix is stored
        inv <- NULL
        
        ## store matrix to varible x
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## retrieve matrix from variable x
        get <- function() x
        
        ## store inverse matrix to inv
        setinv <- function(inverse) inv <<- inverse
        
        ## retrieve inverse matrix from varible inv
        getinv <- function() inv
        
        ## create the list object
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Function cacheSolve will return an inverse matrix by either retrieving cached 
## values or calculating the inverse.  This function works in conjunction 
## with objects defined using the makeCacheMatrix function.  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## Retrieve cached inverse matrix
        inv <- x$getinv()
        
        ## Determine whether cached inverse matrix exists
        ## If it exists, return values
        ## If not, calculate inverse matrix and then cache and return values
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv


}
