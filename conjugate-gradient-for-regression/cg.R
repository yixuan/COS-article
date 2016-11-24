## Target: solve linear equation Ax = b. A is positive definite
## Ax      -- A function to calculate the matrix-vector product
##            `A * x` given a vector `x` as the first argument
## b       -- Vector of the right hand side of the equation
## x0      -- Initial guess of the solution
## eps     -- Precision parameter
## verbose -- Whether to print out iteration information
cg = function(Ax, b, x0 = rep(0, length(b)), eps = 1e-6,
              verbose = TRUE, ...)
{
    m = length(b)
    x = x0
    r = b - Ax(x0, ...)
    p = r
    r2 = sum(r^2)
    for(i in 1:m)
    {
        Ap = Ax(p, ...)
        alpha = r2 / sum(p * Ap)
        x = x + alpha * p
        r = r - alpha * Ap
        r2_new = sum(r^2)
        err = sqrt(r2_new)

        if(verbose)
            cat(sprintf("Iteration %d, err = %.8f\n", i, err))

        if(err < eps)
            break;
        beta = r2_new / r2
        p = r + beta * p
        r2 = r2_new
    }
    x
}

## Simulation example
set.seed(123)
n = 10000
p = 1000
x = matrix(rnorm(n * p), n)
b = rnorm(p)
y = x %*% b

t1 = Sys.time()
beta_direct = solve(crossprod(x), crossprod(x, y))
t2 = Sys.time()
cat("Direct method: ", t2 - t1, "s\n", sep = "")

mat_vec_mult = function(x, mat)
{
    as.numeric(crossprod(mat, mat %*% x))
}

t1 = Sys.time()
xy = as.numeric(crossprod(x, y))
beta_cg = cg(mat_vec_mult, xy, mat = x)
t2 = Sys.time()
cat("CG method: ", t2 - t1, "s\n", sep = "")

max(abs(beta_direct - beta_cg))


## Real data
## https://archive.ics.uci.edu/ml/datasets/Relative+location+of+CT+slices+on+axial+axis
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/00206/slice_localization_data.zip","slice_localization_data.zip")
unzip("slice_localization_data.zip")
dat = read.csv("slice_localization_data.csv")
n = nrow(dat)
p = ncol(dat)
y = dat$reference
x = as.matrix(dat[, -c(1, ncol(dat))]) / sqrt(n)
xy = as.numeric(crossprod(x, y))

# coeffs = cg(mat_vec_mult, xy, mat = x)

t1 = Sys.time()
ridge = function(x, mat, lambda = 0.01)
{
    as.numeric(crossprod(mat, mat %*% x)) + lambda * x
}
coeffs_ridge = cg(ridge, xy, eps = 1e-3, mat = x, lambda = 0.01)
t2 = Sys.time()
cat("CG ridge: ", t2 - t1, "s\n", sep = "")

library(Matrix)
xsp = as(x, "sparseMatrix")
t1 = Sys.time()
coeffs_sparse_ridge = cg(ridge, xy, eps = 1e-3, mat = xsp, lambda = 0.01)
t2 = Sys.time()
cat("CG sparse ridge: ", t2 - t1, "s\n", sep = "")
