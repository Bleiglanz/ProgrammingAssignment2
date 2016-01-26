## 
## makeCacheMatrix and cacheSolve together can be used to cache 
## the expensive operation of computing the inverse of a matrix
##

## creates an object (==list) of four functions that encapsulate
## the caches value in the global environment
makeCacheMatrix <- function(x = matrix()) {
    
    cachedInverse<-NULL
    
    #set overwrites the current value and invalidates the cache
    set <- function(y){
        x<<-y
        cachedInverse<<-NULL
    }
    
    #get just returns the value
    get <- function(){
        x
    }
    
    #only called from cacheSolve
    setInverse <- function(computedInverse) cachedInverse <<- computedInverse
    
    #only called from cacheSolve
    getInverse <- function() cachedInverse
    
    #note: this is stupid: could check for existing inverse right here!
    #      remove cacheSolve and setInverse and implement the caching in this 
    #      getInverse function
    #      but I will follow the given example
    
    #returns a list of four functions
    list(set=set, 
         get=get, 
         setInverse=setInverse,
         getInverse=getInverse)
    
}


## computes the inverse if necessary

cacheSolve <- function(x, ...) {
    cachedValue <- x$getInverse()
    if(!is.null(cachedValue)){
        message("getting cached data")
        return(cachedValue)
    }
    thematrix <- x$get()
    theinverse <- solve(thematrix)
    x$setInverse(theinverse)
    theinverse
}
