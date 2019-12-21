
R version 3.6.1 (2019-07-05) -- "Action of the Toes"
Copyright (C) 2019 The R Foundation for Statistical Computing
Platform: x86_64-w64-mingw32/x64 (64-bit)

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

  Natural language support but running in an English locale

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

[Previously saved workspace restored]

> ls()
[1] "%p%"             "boring_function" "evaluate"        "mad_libs"        "my_mean"         "remainder"       "telegram"       
> rm(list=ls())
> setwd(C:/Users/dhrit/Downloads/rprog_data_specdata)
Error: unexpected '/' in "setwd(C:/"
> setwd(C:\Users\dhrit\Downloads\rprog_data_specdata)
Error: unexpected input in "setwd(C:\"
> setwd("C:\Users\dhrit\Downloads\rprog_data_specdata")
Error: '\U' used without hex digits in character string starting ""C:\U"
> setwd("C:/Users/dhrit/Downloads/rprog_data_specdata")
> pollutantmean <- function(directory, pollutant, id = 1:332) {
+  means <- c()
+     
+     for(monitor in id){
+         path <- paste(getwd(), "/", directory, "/", sprintf("%03d", monitor), ".csv", sep = "")
+         monitor_data <- read.csv(path)
+         interested_data <- monitor_data[pollutant]
+         means <- c(means, interested_data[!is.na(interested_data)])
+     }
+     
+     mean(means)
+ }
> complete <- function(directory, id = 1:332){
+     ## 'director' is a character vector of length 1 indicating
+     ## the location of the CSV files
+     
+     ## 'id' is an integer vector indicating the monitor ID numbers
+     ## to be used
+     
+     ## Return a data frame of the from:
+     ## id nobs
+     ## 1  117
+     ## 2  1041
+     ## ...
+     ## where 'id' is the monitor ID number and 'nobs' is the
+     ## number of complete cases
+     results <- data.frame(id=numeric(0), nobs=numeric(0))
+     for(monitor in id){
+         path <- paste(getwd(), "/", directory, "/", sprintf("%03d", monitor), ".csv", sep = "")
+         monitor_data <- read.csv(path)
+         interested_data <- monitor_data[(!is.na(monitor_data$sulfate)), ]
+         interested_data <- interested_data[(!is.na(interested_data$nitrate)), ]
+         nobs <- nrow(interested_data)
+         results <- rbind(results, data.frame(id=monitor, nobs=nobs))
+     }
+     results
+ }
> corr <- function(directory, threshold = 0){
+     ## 'directory' is a character vector of length 1 indicating
+     ## the location of the CSV files
+     
+     ## 'threshold' is a numeric vector of length 1 indicating the 
+     ## number of completely observed observations (on all
+     ## variables) requi?red to compute the correlation between
+     ## nitrate and sulfate; the default is 0
+     
+     ## Return a numeric vector of correlations
+     ## NOTE: Do not round the result!
+     cor_results <- numeric(0)
+     
+     complete_cases <- complete(directory)
+     complete_cases <- complete_cases[complete_cases$nobs>=threshold, ]
+     #print(complete_cases["id"])
+     #print(unlist(complete_cases["id"]))
+     #print(complete_cases$id)
+     
+     if(nrow(complete_cases)>0){
+         for(monitor in complete_cases$id){
+             path <- paste(getwd(), "/", directory, "/", sprintf("%03d", monitor), ".csv", sep = "")
+             #print(path)
+             monitor_data <- read.csv(path)
+             #print(monitor_data)
+             interested_data <- monitor_data[(!is.na(monitor_data$sulfate)), ]
+             interested_data <- interested_data[(!is.na(interested_data$nitrate)), ]
+             sulfate_data <- interested_data["sulfate"]
+             nitrate_data <- interested_data["nitrate"]
+             cor_results <- c(cor_results, cor(sulfate_data, nitrate_data))
+         }
+     }
+     cor_results
+ }
> pollutantmean("specdata", "sulfate", 1:10)
[1] 4.064128
> pollutantmean("specdata", "nitrate", 70:72)
[1] 1.706047
> pollutantmean("specdata", "sulfate", 34)
[1] 1.477143
> pollutantmean("specdata", "nitrate")
[1] 1.702932
> cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
> print(cc$nobs)
[1] 228 148 124 165 104 460 232
> cc <- complete("specdata", 54)
> print(cc$nobs)
[1] 219
> RNGversion("3.5.1")  
Warning message:
In RNGkind("Mersenne-Twister", "Inversion", "Rounding") :
  non-uniform 'Rounding' sampler used
> set.seed(42)
> cc <- complete("specdata", 332:1)
> use <- sample(332, 10)
> print(cc[use, "nobs"])
 [1] 711 135  74 445 178  73  49   0 687 237
> cr <- corr("specdata")                
> cr <- sort(cr)   
> RNGversion("3.5.1")
Warning message:
In RNGkind("Mersenne-Twister", "Inversion", "Rounding") :
  non-uniform 'Rounding' sampler used
> set.seed(868)                
> out <- round(cr[sample(length(cr), 5)], 4)
> print(out)
[1]  0.2688  0.1127 -0.0085  0.4586  0.0447
> cr <- corr("specdata", 129)                
> cr <- sort(cr)                
> n <- length(cr)    
> RNGversion("3.5.1")
Warning message:
In RNGkind("Mersenne-Twister", "Inversion", "Rounding") :
  non-uniform 'Rounding' sampler used
> set.seed(197)                
> out <- c(n, round(cr[sample(n, 5)], 4))
> print(out)
[1] 243.0000   0.2540   0.0504  -0.1462  -0.1680   0.5969
> cr <- corr("specdata", 2000)                
> n <- length(cr)                
> cr <- corr("specdata", 1000)                
> cr <- sort(cr)
> print(c(n, round(cr, 4)))
[1]  0.0000 -0.0190  0.0419  0.1901
> q()
> ## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
