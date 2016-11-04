# Support of fast matrix inversion computation via caching. 

# Function: makeCacheMatrix  	- Creates a matrix object container. The data members
#						  of this container store a matrix object and the 
# 						  associated inverse matrix to that object.

makeCacheMatrix <- function( x = matrix() ) 
{

	# assign NULL to the inverse matrix data member
	INV_MAT 		<- NULL
	MAT 			<<- x
	# Function: set 		- assigns a matrix to data member MAT
	# 		        	  and the inverse matrix INV_MAT to NULL
	set 		<- function ( y = matrix() )
	{	
		# Set data members using the <<- assignment operator
		# to force search through parent env for existing definitions...
		MAT 		<<- y
		INV_MAT	<<- NULL
	}

	# Function: setInvMat 	- assigns a matrix to data member INV_MAT.
	# 		        	  There are no guarantees that I = MAT * INV_MAT
	setInvMat 	<- function ( invMat ) INV_MAT <<- invMat

	# Function: get 		- returns a copy of the data stored in MAT
	get 		<- function ( ) MAT

	# Function: getInvMat 	- returns a copy of the data stored in MAT
	getInvMat 	<- function ( ) INV_MAT


	# Output the function list
	list ( set = set , setInvMat = setInvMat , get = get , getInvMat = getInvMat ) 		
}


# Function: cacheSolve 		- Computes the inverse of a makeCacheMatrix object
# 					  and stores the result.

cacheSolve <- function ( x , ... ) 
{
    	# Check that we don't have any cached results on hand...
	invMat	<- x$getInvMat()
	
	if (!is.null(invMat))
	{
		message("getting cached inverse matrix result\n")
		return(invMat)
	}

	# Otherwise we proceed with our calculation...
	mat 		<- x$get()
	invMat	<- solve(mat)
	# Let's cache the result
	x$setInvMat(invMat)

	# Return the value of the inverse matrix
	invMat
}
