makeCacheMatrix <- function(x = matrix()) {   #function that calculate the inverted matrice and store it into a list
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,            #set a list 1x4 where the matrix and the inverted matrix are stored
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){                  #Test if the inversion has already been done previously
    message("getting cached data")  #if not, a message will appear on the console
    return(m)                       #still, the inverted matrix is shown
  }
  matrix<-x$get()                   #otherwise it will do the operation we wanted
  m<-solve(matrix, ...)             #
  x$setmatrix(m)                    #store the inverted matrix to the list not to calculate it once again if needed
  m
}