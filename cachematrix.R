#Our aim in this experiment is to write a pair of functions, namely
#"makeCacheMatrix" and "cacheSolve" that cache the inverse of a matrix


# I took the reference from the given example of mean 
# I optimized the code by changing few things like("m to i","getmean , setmean to getinv , setinv" )
# then I changed  reference to "mean" to "solve" in the cacheSolve function





# makeCacheMatrix is a function which creates a special "matrix" object that can
# cache its inverse for the input (which is an invertible square matrix)


makeCacheMatrix <- function(x = matrix()) {
         i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) i <<- inverse
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


# cacheSolve is a function which computes the inverse of the special "matrix"
# returned by makeCacheMatrix above. If the inverse has already been calculated
# (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         i<- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}

 #m <- matrix(rnorm(16),4,4)
 #m1 <- makeCacheMatrix(m)
 #cacheSolve(m1)



# ---------------OUTPUT------------------------
#m <- matrix(rnorm(16),4,4)
#print("original matrix  ")
#print(m)
#m1 <- makeCacheMatrix(m)
#print("Inverse of original matix ")
#cacheSolve(m1)

# "original matrix  "
#          [,1]       [,2]       [,3]       [,4]
#[1,]  0.93715806  0.1705566 -1.0458432 -1.0994978
#[2,] -0.45748035 -0.6488446 -0.8538465  0.5325657
#[3,]  0.09629711  2.6060923 -1.6608780 -0.7863880
#[4,]  0.04746619 -1.3892976  0.2098507 -2.1792362
# "Inverse of original matix "
#          [,1]       [,2]       [,3]        [,4]
#[1,]  0.9112555 -0.4470306 -0.3977540 -0.42547367
#[2,] -0.1813411 -0.3235057  0.2697724 -0.08491496
#[3,] -0.2829438 -0.5992509 -0.1112468  0.03645255
#[4,]  0.1082097  0.1387980 -0.1913603 -0.41049881