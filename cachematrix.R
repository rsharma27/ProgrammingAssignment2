## Put comments here that give an overall description of what your
## functions do

#==============================================================================
# Created two functions as per assignment. I have specifically
# created 2 global variables g_inv_matrix to store inverse of a matrix and
# g_st_matrix to store matrix for which inverse is cached. Flow for my code:
# 1. provide a square matrix input to cacheSolve() function
# 2. cacheSolve() compares if the input matrix is different than already stored
#    matrix. If different then compute inverse else return from cache
# 3. cacheSolve() does following:
#	a. Checks if g_st_matrix is NULL(empty) or not. If empty, call
#           makeCacheMatrix function to initialize global variables and 
#	   calculate inverse and return the result. 
#	b. If g_st_matrix is not empty, compare dimensions of input and 
#	   g_st_matrix. They should be same to compare two square matrices.
#	c. If dimensions match, compare all elements of g_st_matrix and input
#	   matrix. If the matrices are same, return the value from g_inv_matrix
#	   global variable.
#	d. If dimensions of the two matrices don't match, call makeCacheMatrix
#	   function to re-initialize global variables, calculate inverse and 
#	   return the result.
#	e. If dimensions are same but result of step c is not positive, then
#	   again call makeCacheMatrix to calculate inverse.
# 4. makeCacheMatrix() does following:
#	a. Four functions defined to get old matrix, set old matrix, get 
#	   inverse and set inverse of a matrix; get_old_matrix(), 
#          set_old_matrix(),get_Inv(),set_Inv(). 
#	b. get_old_matrix() - return value of global variable to store matrix
#	c. set_old_matrix() - store new matrix in global variable 
#       d. get_Inv() - returns value of global variable g_inv_matrix
#	e. set_Inv() - calls set_old_matrix() and calculates inverse of input 
#	   matrix and caches it into g_inv_matrix variable
#	f. returns result of all 4 functions defined 
#==============================================================================

#======================= MAIN CODE ============================
#Global variable for Matrix inverse
  g_inv_matrix<-NULL
  g_st_matrix<-NULL


## This function returns old matrix object for which inverse is already computed#  and new inverse matrix when invoked to calculate

makeCacheMatrix <- function(x = matrix()) {

# Get value of old matrix for which inverse is computed and cached
  get_old_matrix<-function() {
    return(g_st_matrix)
  }

# Set global variable for old matrix to be compared next time of funciton call  
  set_old_matrix<-function(y) {
    g_st_matrix<<-y
  }
  
# Return value of computed inverse of matrix
  get_Inv<-function() {
    return(g_inv_matrix)
  }

# Set global variable for old matrix, compute inverse and store in global var
  set_Inv<-function() {
    set_old_matrix(x)
    g_inv_matrix <<- solve(g_st_matrix)
  }
  
  list(set_old_matrix=set_old_matrix, 
       get_old_matrix=get_old_matrix, 
       get_Inv=get_Inv, 
       set_Inv=set_Inv)
}


## Write a short comment describing this function
# cacheSolve function checks if it already has a matrix cached for which inverse
# is computed. If not, it computes inverse directly else it compares stored 
# matrix and input matrix if they are different and return result from cache
# accordingly.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
# Handle to refer content of makeCacheMatrix function  
  mymat<-makeCacheMatrix(x)
  
  # Check if there is already a matrix stored
  if(is.null(mymat$get_old_matrix())) {  
              cat("No matrix is cached...calculating inverse now\n")
              mymat$set_Inv()
              return(mymat$get_Inv())
  } 

  # Compare input and old matrix
  # Compare matrices' dimensions first, they must match 

  else if(dim(mymat$get_old_matrix())[1]==dim(x)[1]) {

 	      # Compare two matrices' each element

	      if(all(x==mymat$get_old_matrix())) {
                    cat("Cached matrix and input matrix are same\n")
                    
                    # Check if inverse is cached

		    if(is.null(mymat$get_Inv())) {
                        cat("Matrix' inverse not cached...calculating it now\n")
                        mymat$set_Inv()
                        return(mymat$get_Inv())
                    } 
                    else {
                        cat("Getting inverse from cache...\n")
                        return(mymat$get_Inv())
                    }
              }
              else {
                    cat("Cached & input are different...calculating inverse\n")
                    mymat$set_Inv()
                    return(mymat$get_Inv())
              }
  }  
  else {
              cat("Cached & input are different...calculating inverse\n")
              mymat$set_Inv()
              return(mymat$get_Inv())
  }	
}
