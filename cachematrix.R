## The functions below are used to create a special object
## that stores a matrix and caches its inverse

## is a list containing functions to 
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setinverse<-function(inverse) m<<-inverse
        getinverse<-function() m
        list(set=set,get=get,
             setinverse=setinverse,
             getinverse=getinverse)
}

## the function computes the inverse of the matrix
## but first it checks if the inverse has already been calculated
## if yes then it gets the value from the cache
## else it computes the inverse and sets the value in the cache
cacheSolve <- function(x, ...) {
        m<-x$getinverse()
        if(!is.null(m)){
                message("Getting cached data")
                return(m)
        }
        data<-x$get()
        m<-solve(data,...)
        x$setinverse(m)
        m
}
