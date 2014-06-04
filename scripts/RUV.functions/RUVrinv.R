RUVrinv <-
function (Y, X, ctl, Z = 1, v = NULL, fullW0 = NULL, lambda = NULL, 
    k = NULL, l = NULL, randomization = FALSE, iterN = 1e+05, 
    inputcheck = TRUE) 
{
    if (inputcheck) 
        inputcheck1(Y, X, Z, ctl)
    Y = adjust_v(Y, v, ctl)
    if (!is.null(lambda)) 
        return(RUVinv(Y, X, ctl, Z = Z, fullW0 = fullW0, lambda = lambda, 
            randomization = randomization, iterN = iterN))
    if (is.null(k)) {
        p = ncol(X)
        if (p > 1) {
            if (is.null(l)) {
                warning("Neither lambda nor k are specified, so a call to getK will be made.  But p > 1 and l is not specified.  Arbitrarily setting l = 1.")
                l = 1
            }
            tempX = X[, l, drop = FALSE]
            tempZ = cbind(X[, -l, drop = FALSE], Z)
            k = getK(Y, tempX, ctl, Z = tempZ, inputcheck = FALSE)$k
        }
        else k = getK(Y, X, ctl, Z = Z, fullW0 = fullW0, inputcheck = FALSE)$k
    }
    ruv4fit = RUV4(Y, X, ctl, k, Z = Z, fullW0 = fullW0)
    return(RUVinv(Y, X, ctl, Z = Z, fullW0 = fullW0, lambda = sum(ruv4fit$sigma2[ctl]), 
        randomization = randomization, iterN = iterN))
}
