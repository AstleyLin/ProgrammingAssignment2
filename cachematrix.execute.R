setwd(wdir <- "D:\\Miscellaneous\\Google~1\\eLearn\\Johns Hopkins_Data Science\\R Programming\\Assignment")
source("cachematrix.R")

data.df <- as.data.frame(matrix(rnorm(500,0,1), ncol=5))
MM <- matrix(cor(data.df), nrow=5)
MN <- as.matrix(data.df[1:6,])

###### xMM ----------------------
### Create CacheMatrix object
cMM <- makeCacheMatrix(MM)
all(cMM$get() == MM)
cMM$getCache()

### solve inverse
cacheSolve(cMM)
cacheSolve(cMM)
cacheSolve(cMM)

### change data and solve again
MM2 <- as.matrix(data.df[11:16,])
cMM$set(MM2)
cMM$getCache()
cacheSolve(cMM)
cacheSolve(cMM)
cacheSolve(cMM)


###### xMN ----------------------
### Create CacheMatrix object
cMN <- makeCacheMatrix(MN)
all(cMN$get() == MN)
cMN$getCache()

### solve inverse
cacheSolve(cMN)
cacheSolve(cMN)
cacheSolve(cMN)

### change data and solve again
MN2 <- as.matrix(data.df[11:16,])
cMN$set(MN2)
cMN$getCache()
cacheSolve(cMN)
cacheSolve(cMN)
cacheSolve(cMN)
