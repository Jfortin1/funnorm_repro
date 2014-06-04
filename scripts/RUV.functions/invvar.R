invvar <-
function (Y, ctl, XZ = NULL, v = NULL, lambda = NULL) 
{
    Y = adjust_v(Y, v, ctl)
    m = nrow(Y)
    n = ncol(Y)
    if (is.null(XZ)) {
        pq = 0
    }
    else if (length(XZ) == 1) {
        if (XZ == 1) {
            XZ = matrix(1, m, 1)
            pq = 1
        }
    }
    else {
        pq = ncol(XZ)
    }
    k = m - pq
    if (pq > 0) 
        temp = residop(Y, XZ)
    else temp = Y
    temp = svd(temp[, ctl] %*% t(temp[, ctl]))
    Y0 = t(temp$u[, 1:k, drop = FALSE]) %*% Y
    Y0cd = sqrt(temp$d[1:k])
    if (is.null(lambda)) 
        lambda = 0
    Y0cd2 = Y0cd^2 + lambda
    if (k < 4) 
        stop("In function invvar: You are running the inverse algorithm on fewer than four degrees of freedom.  This is not currently supported, and very likely inadvisable as well.  A possible work-around, if you are sure you want to do this, is to set randomization=TRUE.")
    if (Y0cd2[k - 3]/Y0cd2[k] > 10) 
        stop("In function invvar: The fourth-smallest eigenvalue is more than 10 times larger than the smallest eigenvalue.  This is not currently supported, and very likely means something is wrong (perhaps a dimension was removed during preprocessing?).  If you are sure you know what you are doing you may wish to try setting randomization=TRUE.")
    Ed = momentintegral(1/Y0cd2^2)
    sigma2 = apply(Y0 * (as.vector(Ed) * Y0), 2, sum)/sum(Ed)
    df = sum(Ed)^2/sum(Ed^2)
    return(list(sigma2, df))
}
