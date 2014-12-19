## 
# This source document is composed by a couple of functions that will let you
# create a matrix and calculate its reverse.
# Beofore calculating the inverse matrix, this function will check first whether 
# for a given matrix its inverse one has been previouly calculated or not.
#   + If the inverse matrix has been calculated previously the function will 
#     retrieve it directly.
#   + In other case, the function will be force to calculate the inverse matrix
#     for the matrix given in the parameters.
#
# For this purpose, two functions had been created:
#
#   + makeCacheMatrix will assing to the R object, the initial matrix and a list
#     of associated function that will let you:
#       - set: set the initial matrix of the R object that will be use for
#              future calculations.
#       - get: retrieve the intial matrix of the object.
#       - setCacheMatrix: set the inverse matrix for the object
#       - getCacheMatrix: retrieve the inverse matrix of the object.
#
#   + cacheSolve will retrieve the inverse matrix for a given object.
##

## 
# makeCacheMatrix is an R function that can store a matrix and its associated
# inverse matrix. 
# The object contains 4 more functions to work with the matrixes.

makeCacheMatrix <- function(x = matrix()) {
    # a will contain the inverse matrix and starts NULL as it has not been
    # calculated yet.
    
    a <- NULL
    
    # set function, sets up the value of the initial matrix.
    # As the initial matrix is set up, the inverse one (a) has to be null 
    # to ensure that inverse matrix will be calculated based on this one
    # (otherwise it could store two matrix that wouldn't be related).
    # x and a are objects that belong to the parent environment
    # (makeCacheMatrix function).
    
    set <- function(y) {
      # Superassingmen operator is used, otherwise we would be loosing values
      # of x and a after the end of set function. This way, x and a values are
      # the ones that belong to makeCacheMatrix environment.
        
        x <<- y        
        a <<- NULL
    }
    
    # get function retrieves the initial matrix
    
    get <- function() x
    
    # setCacheMatrix function sets the solved inverse calculation of the initial
    # matrix.
    # Superassingmen operator is used, so the matrix will be stored in a variable
    # of makeCacheMatrix function (parent). Otherwise we would be loosing the
    # value of a variable after setCacheMatrix function ends.
    
    setCacheMatrix <- function(solve) a <<- solve
    
    # getCacheMatrix function retrieves the inverse calculated matrix 'a' 
    # (attribute) of its parent function 'makeCacheMatrix'.
    
    getCacheMatrix <- function() a
    
    # Finally, in order to make all these functions available, a named list
    # is created and returned so they can be used as part of the object in R
    
    list(set = set, get = get,
         setCacheMatrix = setCacheMatrix,
         getCacheMatrix = getCacheMatrix)   
    
}


## 
# cacheSolve is an R function that retrieves an inverse matrix from a given one.
# if this inverse matrix has been calculated before in the given object 
# the function will retriev the calcuted and stored matrix before.
#
# if caches matrix is null, the function will calculate the inverse matrix
# based on the initial given one.

cacheSolve <- function(x, ...) {
    
    a <- x$getCacheMatrix()
    
    # If 'a' is null means that this inverse matrix has not been calculated
    # before.
    # If not, there is no need to calculate anything, just need to retrieve it
    # from the variable of makeCacheMatrix function.
    
    if(!is.null(a)) {
      
      # This message will help us to know that the object is being retrieved
      # instead of being recalculated.
      
        message("getting cached data")
        
        # This will return the object and will finish the function.
        
        return(a)
    }
    
    # This part of code will be executed if the inverse matrix has not been
    # calculated before.
    
    data <- x$get() ## Retrieves de initial matrix.
    
    a <- solve(data, ...) ## Calcualtes the inverse matrix based on the inital 
                          ## one.
    
    x$setCacheMatrix(a) ## Stores the  inverse matrix
    
    a ## Returns the calculated inverse matrix
}
