#https://class.coursera.org/rprog-014/forum/thread?thread_id=576

# makeVector creates special "vector" i.e. list containing functions to
#1. set value of vector
#2. get the value of vector
#3. set mean
#4. get mean

# This function need to be called from outside. When called, it will just set x, set mean to null
# There is set function which can be used separatel to reset vector to another value. In that case, mean will be set to null
# setmean will calculate mean
# getmean will get mean
# get will return the list

makeVector <- function(x = numeric()) {
    m <- NULL
    
    # setter function - can be called outside
    set <- function(y) {
        x <<- y   # assigned parent x
        m <<- NULL  # assigns parent m
    }
    
    # getter function - will return list
    get <- function() {
        x
    }
    
    # setter for mean. Ideally should use variable name other than mean in fuction so that not to confuse with mean function
    setmean <- function(mean) {
        m <<- mean
    }
    
    # getter for mean
    getmean <- function() {
        m
    }
    
    # last statement will be return. This is nothing but list of functions with named variables
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}


# This function calculates mean if no value returned. After calculation, it sets value of mean for future usage
cachemean <- function(x, ...) {
    
    # if valid m value then do not calculate mean
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)  # exit program
    }
    
    # if no mean value. Then get list and calcualte mean and set
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m   # returns mean
}


##
# How to run
# myList <- makeVector(c(1:10))
# cachemean(myList)
# cachemean(myList)


