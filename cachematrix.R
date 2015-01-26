## These functions demonstrate how chaching of calculations can be
## implemented. Although the assignment is scoped on caching only
## the inverse of (invertible) matrixes, the solution provided
## attempts to generically cache any number of calculation on any
## arbitary data. Provided data can be binded to a single variable
## and that it is the first argument to the calculating function.


## This function creates a unique sequence of characters from a 'call'
## to be use as the path definition for caching calculations
## with a recursive list

makePathFromCall <- function(aCall=call(`[`)){
  
  ## create an character vector by combinding
  ## the names of the call, if any, with the call data
  ## coerced to characters
  
  ## extract names from call
  ns<-names(aCall)
  
  ## replace blank names with its index text
  for (i in seq_along(ns)){
    ns[i]<- if (ns[i]=="") as.character(i) else ns[i]
  }
  
  ## combind and return names with call data corced to characters
  c(ns,as.character(aCall))
}

## Generic caching object for any number of calculation on any
## arbitary data. Provided data can be bind to a single variable
## and that it is the first argument to the calculating function.
## 'x'         initial vlaue of data to be calculated on
## 'cache'     a list that may already contain cached calculations
## 'fun'       a default function to do calculation
## '...'       arguments to pass on to function

makeCache <- function(x=NULL,cache=list(),...,fun=`[`){
  
  ## Cached calculations are strored recursively in a list.
  ## 'path'    is character vector containing the names of each named list
  ## 'cal'     is the calculated value to cache
  ## 'current' is the character vector representing the current working path
  ##           for calling setCache recursively
  setCache <- function(path,cal,current=character()){
    current=c(current,path[1])
    if(length(path)==1) cache[[current]]<<-cal
    else {
      if (!is.list(cache[[current]])) cache[[current]]<<-list()
      setCache(path[-1],cal,current)
    }
  }
  
  getCache <- function (path,c=cache){
    val <- c[[path[1]]]
    if (is.list(val) & length(path[-1])>0) getCache(path[-1],val)
    else val
  }
  
  set <- function(v){
    x <<- v
    cache <<- list()
  }
  
  get <- function() x
  
  calc <- function(...,f=fun){  
    ## translate function call to a path
    path <- makePathFromCall(substitute(f(...)))
    
    ## Look up a cache
    cal<-getCache(path)
    
    
    if (is.null(cal)){ ## If cal is null, calculate
      
      ## Simulate expensive calculation
      for(i in 1:5){
        Sys.sleep(1)
        message("Cache not found. Calculating, taking a long time..")
      }
      Sys.sleep(.5)
      message("Done.")
      Sys.sleep(.2)
      
      
      ## Apply function on x
      cal<-f(x,...)
      
      ## Store cache
      setCache(path,cal)
    } else {
      message("Found cache!")
    }
    return(cal)
  }
  list(set=set,get=get,calc=calc)
}

 makeCacheMatrix <- function(x = matrix()) {
   
  ## This is where the generic caching is implemented
  source("cache.R",local=TRUE)
  
  ## Setup specialized cache instance for matix and solve
  ## following example conventions
  cm<-makeCache(x,fun=solve)
  list(
    set=cm$set,
    get=cm$get,
    setSolve=function() { ## This is a stub function not needed
      message("Call getSolve to calcualte solve and set cache in the process.")
    },
    getSolve=function(...) { cm$calc(...) }
  )
 }


## Demonstrate use

cacheSolve <- function(x,...) {
  
  ## Return a matrix that is the inverse of 'x'
  ## 'x' is assumed a return object from 'makeCahceMatrix',
  ## i.e. has a $getSolve(...) function
  
  ## Get and return solved matrix. The underlying cache
  ## takes care of calculation if a cache does not exists.
  x$getSolve(...)
  
}

test <- function() {
  message("Create a matrix and make cache object.")
  m <- matrix(c(1:4),2,2)
  cm <- makeCacheMatrix(m)
  
  message("First solve.")
  firstCalinverse <- cacheSolve(cm)
  print(firstCalinverse)
  
  message("Second solve.")
  secondCalInverse <- cacheSolve(cm)
  print(secondCalInverse)
}
