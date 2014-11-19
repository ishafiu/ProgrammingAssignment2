## Put comments here that give an overall description of what your
## functions do
#There are two functions and they are the makeCacheMatrix and cacheSolve
#The purpose of these functions are to cache the expensive process of 
#calculating the inverse of a given matrix.
#By caching the inverse, it can be called from the environment
#without recalculating


## Write a short comment describing this function

# The makeCacheMatrix function takes a matrix argument. Its starts by
# initialising the return value m
# The setinvmatrix sets the inverse of matrix x
# The get function returns the inverse matrix
# Finally the list returns these functions

makeCacheMatrix <- function(x = matrix()) {
m<-NULL
  set<-function(y){
  x<<-y
  m<<-NULL

}
get<-function() x
setinvmatrix<-function(solve) m<<- solve
getinvmatrix<-function() m
list(set=set, get=get,
   setinvmatrix=setmatrix,
   getinvmatrix=getmatrix)
}


## Write a short comment describing this function
#the cacheSolve checks if there is a inverse calcualtion for the given
#matrix by checking the m value for null
#if it is null the inverse is calculated, cached and returned
#if it is not, then the cached calculated value is returned

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
 m<-x$getmatrix()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m


}
