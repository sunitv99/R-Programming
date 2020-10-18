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


##Testing conditions
##a1 <- c(3,2,5)
##a2 <- c(2,3,2)
##a3 <- c(5,2,4)
##A <- rbind(a1,a2,a3)
##mm <- makeCacheMatrix(A)
##mm$get()
##cacheSolve(mm)
##mm$getInv()
