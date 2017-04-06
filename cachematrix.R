## These functions cache the inverse of a matrix
## This particular function creates a matrix and caches the inverse

makeCacheMatrix <- function(x = matrix()) {
	i<-NULL
	set<-function(y){
		x<<-y
		i<<-NULL
	}
	get<-function()x
	seti<-function(inverse)i<<-inverse
	geti<-function()i
	list(set=set,get=get,seti=seti,geti=geti)

}


## This function computes the inverse of the matrix

cacheSolve <- function(x, ...) {
	i<-x$geti()
	if(!is.null(i)){
		message("getting cached data")
		return(i)
	}
	data<-x$get()
	inv<-solve(data,...)
	x$seti(i)
	return(i)

        ## Return a matrix that is the inverse of 'x'
}
