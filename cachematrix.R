makeCacheMatrix <- function(mat = matrix){
	##Initializing the inverse prop
	inv<-NULL
	##method to set the matrix
	set <- function(matrix){
		mat<<-matrix
		inv<<-NULL
	}
	get <- function() {
		mat
	}
	setInv <- function(inverse){
		inv <<- inverse
	}
	getInv <- function() {
		inv
	}
	list(set=set, get=get, setInv = setInv,getInv = getInv)
}

cacheSolve <- function(x, ...) {
	mat <- x$getInv()
	if(!is.null(mat)){
		message("Getting the cached Data")
		return(mat)
	}
	##Get the matrix from the object defined
	data<-x$get()
	mat<-solve(data)%*% data
	x$setInv(mat)
	mat	
}
