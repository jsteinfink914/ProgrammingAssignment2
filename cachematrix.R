## The function creates a matrix that can cache its inverse
## The input x is set as a matrix
##solved value "s" is set as a null

makeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3)) {
    s<-NULL
    set<-function(y){
        x<<-y
        s<<- NULL
    }
    get<- function() x
    setmsolve<-function(solve) s<<-solve
    getsolve<-function() s
    list(set=set,get=get,
         setsolve=setsolve,
         getsolve=getsolve)
}


## This function returns the inverse of the matrix from makeCacheMatrix
## It checks the cache for the inverse, and if not there, does its own calculation

cacheSolve <- function(x, ...) {
        s<-x$getsolve()
        if(!is.null(s)){
                message("getting cached data")
                return(s)
        }
        data<-x$get()
        s<-solve(data,...)
        x$setseolve(s)
        s
}
