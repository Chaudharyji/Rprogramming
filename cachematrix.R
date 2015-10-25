## using the following two functions, we can calculate and store the inverse of a matrix.
## storing the inverse of the matrix elliminates the need for repeated computation

## following function creates a special "matrix" object which can cache the inverse of the matrix .

makeCacheMatrix<-function(x = matrix()) {
        invurs<-NULL
        first<-function(y){
                x<<-y
                invurs<<-NULL
        }
        second<-function() x
        setInverse<-function(inverse) 
        invurs<<-inverse
        getInverse<-function() invurs
        list(first=first,
             second=second,
             setInverse=setInverse,
             getInverse=getInverse)
}


## following calculates the inverse the inverse of the matrix created by the above function.
## else, if the matrix is unchanged, the inverse is retrieved from cache 

cacheSolve<-function(x, ...){
        ## Return a matrix that is the inverse of 'x'
        invurs<-x$getInverse()
        if(!is.null(invurs)){
                return(invurs)
        }
        mat<-x$second()
        invurs<-solve(mat, ...)
        x$setInverse(invurs)
        invurs
}