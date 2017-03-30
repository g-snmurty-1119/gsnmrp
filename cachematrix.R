## The following functions will return the inverse of orginal matrix from cache and if cache is not 
## available then it builds the inverse matrix and make it cache and return


## The following function makeCacheMatrix returns a matrix object (list) with original matrix and its inverse
## along with get and set functions 

makeCacheMatrix <- function(x = matrix()) {
	xinv <- NULL
	setMatrix <- function(mat) { 
		x <<- mat 
		# Saving the xinv in different environment
	}
	getMatrix <- function() {x}
	
	setInvMatrix <- function(invmatrix) {
		xinv <<- invmatrix 
		# Saving the xinv in different environment
	}
	getInvMatrix <- function() {xinv}
	
	list(mat = x, invmat = xinv, setMatrix = setMatrix,
	getMatrix = getMatrix, setInvMatrix = setInvMatrix,
	getInvMatrix = getInvMatrix)
}


## The following function returns inverse matrix if it cached else 
## build the inverse matrix and return

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	invmat <- x$getInvMatrix()
	
	#checking whether the inverse matrix already exist
	if(is.null(invmat)) {
	# if not building the inverse matrix from main matrix
	
	#getting the original matrix
	mat <- x$getMatrix()
	mat
	nr <- nrow(mat)
	nc <- ncol(mat)

	#creating emampty invers matrix
	invmat <- matrix(numeric(), nr,nc,...)

	for( i in 1:nr ) {
		for(j in 1:nc) {
		invmat[j,i] <- mat[i,j]
		#storing the invers of the mat to invmat
		}
	}
	message("building the inverse matrix")
	x$setInvMatrix(invmat)
	invm <- x$getInvMatrix()
	#setting the inverse matrix to matrix object
	return(invm ) # return the invers matrix

	}

	#If above if fails mean, inverse matrix is already cached

	message("fetching the Cache inverse matrix")
	
	return(invmat)

}
