

## function makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        seti <- function(i) i <<- i
        geti <- function() i
        list(set=set, get=get, seti=seti, geti=geti)
}


## function cachesolve

cacheSolve <- function(x, ...) {
        i <- x$geti()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$seti(i)
        i
}

# testing
x = rbind(c(-5, 20), c(20, -5))
m = makeCacheMatrix(x)
m$get()

# more
cacheSolve(m)

#more
cacheSolve(m)