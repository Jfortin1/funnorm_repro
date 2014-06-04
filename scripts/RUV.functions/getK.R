getK <-
function (Y, X, ctl, Z = 1, v = NULL, fullW0 = NULL, cutoff = NULL, 
    inputcheck = TRUE) 
{
    if (inputcheck) 
        inputcheck1(Y, X, Z, ctl)
    Y = adjust_v(Y, v, ctl)
    m = nrow(Y)
    n = ncol(Y)
    p = ncol(X)
    if (p > 1) {
        cat("Error: In function getK, X must have a single column.\n")
        return(0)
    }
    if (is.null(Z)) {
        q = 0
    }
    else if (length(Z) == 1) {
        if (Z == 1) {
            Z = matrix(1, m, 1)
            q = 1
        }
    }
    else {
        q = ncol(Z)
    }
    if (q > 0) {
        Y = residop(Y, Z)
        X = residop(X, Z)
    }
    X = X/sqrt(sum(X^2))
    Yc = Y[, ctl]
    Y0 = residop(Y, X)
    if (is.null(fullW0)) {
        fullW0 = svd(Y0 %*% t(Y0))$u[, 1:(m - p - q), drop = FALSE]
    }
    K1 = min(m - p - q - 1, sum(ctl))
    W0 = fullW0[, 1:K1, drop = FALSE]
    alpha = solve(t(W0) %*% W0) %*% t(W0) %*% Y0
    bycx = solve(t(X) %*% X) %*% t(X) %*% Yc
    alphac = alpha[, ctl, drop = FALSE]
    sizeratios = rep(0, K1)
    for (i in 1:K1) {
        alphaci = alphac[1:i, , drop = FALSE]
        bwx = bycx %*% t(alphaci) %*% solve(alphaci %*% t(alphaci))
        divisor = sqrt(1 + sum(bwx^2))
        betachat = bycx - bwx %*% alphaci
        scaledbetac = sqrt(sum(ctl)/(sum(ctl) - i)) * betachat/divisor
        sizeratios[i] = median(abs(as.vector(alphac[i, ])/as.vector(scaledbetac)))
    }
    if (is.null(cutoff)) 
        cutoff = getKcutoff(m, n)
    keep = sizeratios > cutoff
    K = sum(keep)
    return(list(k = K, cutoff = cutoff))
}
