## The makeCacheMatrix function is supposed to store matrix of interest (A) and 
## its inverse matrix (B) if it has already been computed. This function is not
## computing the inverse matrix.

## It is required for the downstream inversion function that an input matrix is 
## a square matrix and that its determinant is not equal to 0

makeCacheMatrix <- function(A = matrix()) {
        B <- NULL #B is an inverse matrix
        setA <- function(C) {
                A <<- C
                B <<- NULL
        }
        getA <- function() A
        setB <- function(inverse) B <<- inverse
        getB <- function() B
        list(setA = setA, getA = getA,
             setB = setB,
             getB = getB)
}


## Function cacheSolve first tests if the inverse matrix has already been computed
## if that is not the case, it computes the inverse and assigns its value to be stored

cacheSolve <- function(x, ...) {
        B <- x$getB() #retrieve stored inverse matrix if already computed
        if(!is.null(B)) {
                message("getting cached data")
                return(B) #print retrived inverse matrix and ignore rest of the function
        }
        data <- x$getA() # this part is executed if inverse matrix has not beed computed
        B <- solve(data, ...) # calculate inverse matrix
        x$setB(B) # set the value of invese matrix to be stored
        B
}

## Example usage:
## example matrices to test:
m <- matrix(c(1:8,10), nrow = 3, ncol = 3)
l <- matrix(c(1:3,5), nrow = 2, ncol = 2)

L <- makeCacheMatrix(l)
cacheSolve(L)
#calling the same function once more should use cached value with a message "getting cached data" printed:
cacheSolve(L)

#the same for the other example matrix:
M <- makeCacheMatrix(m)
cacheSolve(M)
cacheSolve(M)
