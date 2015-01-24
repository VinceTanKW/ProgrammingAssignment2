## This is to cache the matrix inversion to save computational cost

## This function creates a special matrix object that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function() x
    setmatrix<-function(solve) m<<- solve
    getmatrix<-function() m
    list(set=set, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)
}


## This function computes the inverse of the special matrix 
## returned by makeCacheMatrix above.
## If the inverse has already been calculated, 
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m<-x$getmatrix()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    mat<-x$get()
    m<-solve(mat, ...)
    x$setmatrix(m)
    m
}
