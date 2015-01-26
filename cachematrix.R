## creates a special matrix and caches its inverse to prevent repeated computation

## 
makeCacheMatrix <- function(x = matrix()) {
    I <- NULL # Set the inverse equal to NULL
    
    set <- function(y){
        x <<- y   # set arguement y to x
        I <<- NULL   # redfine inverse as NULL
    }
    get <- function() x   # get function returns the matrix
    
    setInverse <- function(solve) I <<- solve   # setInverse overrides the previous value of I and assigns the argument to Inverse    
    getInverse <- function() I   # getInverse returns the Inverse
    
    list(set = set, get = get, 
         setInverse = setInverse,
         getInverse = getInverse)   # creates a list of the functions   
}

## Computes of above defined matrix if it hasnt been computed already

cacheSolve <- function(x, ...) {
    I <- x$getInverse()   # Retrives the most recent value for the inverse
    
    if(!is.null(I)){
        message("getting cached data")
        return(I)   # If the value of Inverse is NOT null        
    }
    # Else calculate the inverse of the matrix
    message("newly calculating data")
    data <- x$get()
    I <- solve(data, ...)
    x$setInverse(I)
    I   #returnt the inverse
}
