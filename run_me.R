## script to run assignment 2
# this script should cache the inverse of a matrix 
# This is an example for an inverse:
mm = matrix(c(4,7,3,6),ncol =2)

# r_comp Helper function to check if the result is an inverse -> TRUE
# use zapsmall to deal with floating point arithmetic
r_comp = function(mat)
{
  ee1 = solve(mm)%*% mm
  ee2 = mm%*% solve(mm)
  str(all(zapsmall(ee1) == zapsmall(ee2)))
}
mm_r = r_comp(mm)


## Prepare the workspace 
# Remove vars from workspace
rm(list=ls())

# Use the directory of this file as reference
this_dir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this_dir)

source("cachematrix.R")

# Create a square matrix wit integer values [S_min, S_max]

M_rows = 2
M_cols = M_rows
M_elems = M_rows * M_cols

S_min = 1
S_max = 10

set.seed(10)
          
 M = matrix(sample(S_min:S_max,M_elems, replace = TRUE), ncol = M_cols, nrow = M_rows)
str(M)   

# use the R matrix
R_check = solve(M)%*% M
R_check = solve(M)%*% M

## -------------Testbed ----------------------------------------------------- ##

# Is it possible to create an enhanced matrix ?
M1 = makeCacheMatrix(M)
str(M1)

# Is the original matrix stored?
MM = M1$get()
str(all(M = MM))

# Invert the matrix
M1_inv = cacheSolve(M1)
str(M1_inv)

# Check if the caching works
M2_inv = cacheSolve(M1)
all(M1_inv == M2_inv)

# Validate if the result is really an inverse
str( all( zapsmall(MM %*% M2_inv) == zapsmall(M2_inv %*% MM)))

# Test what happens, if the matrix changes
M1$getInverse()
 M1$setInverse(NULL)
 M1$getInverse()
 
M11_inv = cacheSolve(M1) # should have no output
cacheSolve(M1)   # should show messages to be correct