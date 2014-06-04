getKcutoff <-
function (m, n, cutoffitern = 25) 
{
    cutoff = 0
    for (i in 1:cutoffitern) {
        A = matrix(rnorm(m * n), n, m)
        A = t(A) %*% A
        cutoff = cutoff + sqrt(svd(A, nu = 0, nv = 0)$d[1]/n)
    }
    return(cutoff/cutoffitern)
}
