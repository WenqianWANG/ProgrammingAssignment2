##goal of the work : use scoping operator <<- 
##it enables to assign the value to an object in a different environment

## get/set the matrix/inverse, with a "list" as return value
makeCacheMatrix <- function(x = matrix()) {
        #test if it is a square matrix  
        if(nrow(x)==ncol(x)){
                inverse <- NULL
                setmatrix <- function(y) {
                    x <<- y
                    inverse <<- NULL
                } 
                getmatrix <- function() {
                    x           
                }  
                setinverse <- function(inv) {
                    inverse <<- inv
                }
                getinverse <- function() {
                    inverse
                }
                #return
                list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse, getinverse = getinverse)        
        }
        else{
                #if not, stop and send a message
                stop("Not a square matrix!")    
        }
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        #search in the cache
        res <- x$getinverse() 
        if(!is.null(res)) {
                message("Getting the inverse of matrix in the cache!")
        } 
        else{
                #calculate yourself
                data <- x$getmatrix()
                #test if invertible (even though it is not demanded in the hw)
                if(det(data)!=0){
                        message("Calculating the inverse of your invertible matrix!")
                        res <- solve(data, ...)   
                        x$setinverse(res)  
                }
                else{
                        message("The matrix is not invertible because its determinant equals to zero!")
                }
        }
        res
}


#personal test
m <- matrix(c(4,7,5,9),2,2)
erreur <- try(test<- makeCacheMatrix(m))
print(class(erreur))
if(class(erreur)=="try-error"){
    stop("error occured")
}
test$getmatrix()
test$getinverse()
cacheSolve(test)
test$getinverse()
cacheSolve(test)
test$getinverse()
