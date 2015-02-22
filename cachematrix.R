## Assignment: Caching the Inverse of a Matrix
## Yevgeniy SAMYSHKIN



## The function  makeCacheMatrix() creates a special vector (list) that contains a function to 
## set and get the values of the matrix 
## set and get the inverted matrix
## The syntax is based on the Coursera assignment 


makeCacheMatrix <- function(x = matrix()) {      
	m <- NULL
      set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        	list(set = set, get = get,
	             setinverse = setinverse,
	             getinverse = getinverse)
}


## This function (cacheSolve) computes the inverse of the matrix created with the function makeCacheMatrix
## it initially checks the inverse of the matrix has been calculated and if so it obtains 
## the inverse from the cache 

## This can be tested by running the following sequence: 
## 1. define function makeCacheMatrix()
## 2. define function cacheSolve()
## 3. define data 
## 4. run xx<-makeCacheMatrix(matr)
## 5. run get_inverse <-cacheSolve(xx) - this perfoems matrix inversion and generates no message
## 6. ruN the above line again (or any subsequent number of times) get_inverse <-cacheSolve(xx)
## 7. this generates message 'getting cached data' which suggests that prior calculation was retrieved from cache 

## Input to this function is an arbitrary invertible matrix 10x10  shown in the Annex 

 
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

## The syntax is based on the Coursera assignment 


## data - an arbitrary invertible matrix I have created 

seq1<-c(1, 1, 2, 1, 1, 1, 1, 1, 1, 1)
seq2<-c(0, 1, 1, 1, 1, 1, 1, 1, 1, 1)
seq3<-c(0, 0, -1, 1, 1, 3, 1, 2, 1, 5)
seq4<-c(0, 0, 0, 1, 1, 1, 1, 1, 1, 1)
seq5<-c(0, 0, 0, 0, 1, -1, 1, 4, 1, 1)
seq6<-c(1, 0, 0, 0, 0, 1, 1, 1, 1, 1)
seq7<-c(0, 0, 0, 0, 0, 0, 1, 1, 1, 6)
seq8<-c(0, 0, 1, 0, 0, 0, 0, 1, 2, 1)
seq9<-c(0, 0, 0, 0, 0, 2, 0, 0, 1, 1)
seq10<-c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1)
matr <- rbind(seq1, seq2, seq3, seq4, seq5, seq6, seq7, seq8, seq9, seq10)

xx<-makeCacheMatrix(matr)
get_inverse <-cacheSolve(xx)


## ================ A N N E X - C A L C U L A T I O N   T E S T ============

## Below is the test for the task computations 

## Inverted matrix (based on matrix matr above) I have calculated outside R:
 
##[1]  -0.5	  0.5	 -0.25   0.25   0   1.5  -1.5   0.25  -0.5   8.75
##[2]  -1.5	  2.5	 -0.25  -0.75   0   1.5  -1.5   0.25  -0.5   8.75
##[3]   1.5	 -1.5	  0.25  -0.25   0  -1.5   1.5  -0.25   0.5  -8.75
##[4]   0.5	 -0.5	  1.75  -0.75  -1  -0.5   0.5   1.25  -2.5  -8.25
##[5]  -1	  1	 -2	   2	    1   1    -2    -1      2    17
##[6]   0.5	 -0.5	  0.25  -0.25   0  -0.5   0.5  -0.25   0.5  -3.75
##[7]   0.5	 -0.5	 -0.25   0.25   0  -0.5   1.5  -0.75   0.5  -7.25
##[8]   0.5	 -0.5	  0.75  -0.75   0  -0.5   0.5   0.25  -0.5  -5.25
##[9]  -1	  1	 -0.5	   0.5    0   1    -1     0.5	   0     6.5
##[10]  0	  0	  0	   0	    0   0     0     0      0     1



## R output - produced by the cacheSolve() funtion 

##      seq1 seq2  seq3  seq4 seq5 seq6 seq7  seq8 seq9 seq10
## [1,] -0.5  0.5 -0.25  0.25    0  1.5 -1.5  0.25 -0.5  8.75
## [2,] -1.5  2.5 -0.25 -0.75    0  1.5 -1.5  0.25 -0.5  8.75
## [3,]  1.5 -1.5  0.25 -0.25    0 -1.5  1.5 -0.25  0.5  -8.75
## [4,]  0.5 -0.5  1.75 -0.75   -1 -0.5  0.5  1.25 -2.5  -8.25
## [5,] -1.0  1.0 -2.00  2.00    1  1.0 -2.0 -1.00  2.0  17.00
## [6,]  0.5 -0.5  0.25 -0.25    0 -0.5  0.5 -0.25  0.5  -3.75
## [7,]  0.5 -0.5 -0.25  0.25    0 -0.5  1.5 -0.75  0.5  -7.25
## [8,]  0.5 -0.5  0.75 -0.75    0 -0.5  0.5  0.25 -0.5  -5.25
## [9,] -1.0  1.0 -0.50  0.50    0  1.0 -1.0  0.50  0.0   6.50
## [10,]  0.0  0.0  0.00  0.00    0  0.0  0.0  0.00  0.0  1.00 
