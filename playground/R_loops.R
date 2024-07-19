################################################################
### Compare some methods for looping 
### R 3.4.0             2018-03-12
################################################################
### Parameters
mat1 <- NULL
nrows <- 20000
ncols <- 10
prob <- 0.5

## Method 1: while loop
start_time <- Sys.time()
i <- 0
while (i < nrows)
{
  mat1 <- rbind(mat1, (rbinom(ncols, 1, prob)))
  i <- i+1
}
time1 <- Sys.time() - start_time
message("Method 1 (while loop): ", round(time1, 3), " seconds.")



## Method 2: for loop
mat2 <- NULL
start_time <- Sys.time()
for (i in 1:nrows)
{
  mat2 <- rbind(mat2, (rbinom(ncols, 1, prob)))
}
time2 <- Sys.time() - start_time
message("Method 2 (for loop): ", round(time2, 4), " seconds.")

mat2 <- NULL
cat("Method 2 (for loop) [system.time]:\n")
system.time(
            for (i in 1:nrows)
            {
              mat2 <- rbind(mat2, (rbinom(ncols, 1, prob)))
            }         
            )


## Method 3: vectorized apply
cat("Method 3 (sapply) [system.time]:\n")
rownums <- 1:nrows
system.time( mat3 <- sapply(rownums, function (x) rbinom(ncols, 1, prob)) )
mat3 <- t(mat3)

## sapply joins vectors as columns in a matrix by default: there is probably a way to change this.
matx <- sapply(rownums, function (x) rep(x, ncols) )

## apply functions make more sense if you are actually using the values in the vector 
cat("Method 3, lapply of multiple matrices [system.time]:\n")
probs <- seq(0.1, 0.9, by=0.1)
system.time( mat3l <- lapply(probs, function (p) matrix(rbinom(ncols * nrows, 1, p), nrows, ncols, byrow = TRUE) ) )
## mat3l is now a list, with each item being a nrows x ncols matrix based on a given value of probs.
## Bind them all together in a giant matrix:
mat3.1 <- do.call(cbind, mat3l)

## Method 4: matrix() function (direct)
cat("Method 4 (matrix function) [system.time]:\n")
system.time( mat4 <- matrix(rbinom(ncols * nrows, 1, prob), nrows, ncols, byrow = TRUE) )
