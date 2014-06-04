RUV2 <-
function (Y, X, ctl, k, Z = 1, v = NULL, fullW = NULL, inputcheck = TRUE) 
{
    if (inputcheck) 
        inputcheck1(Y, X, Z, ctl)
    Y = adjust_v(Y, v, ctl)
    m = nrow(Y)
    n = ncol(Y)
    p = ncol(X)
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
    Yc = Y[, ctl]
    if (is.null(fullW)) {
        fullW = svd(Yc %*% t(Yc))$u[, 1:(m - p - q), drop = FALSE]
    }
    if (k > 0) {
        W = fullW[, 1:k, drop = FALSE]
        XZW = cbind(X, Z, W)
        W0 = residop(W, X)
        W0 = svd(W0)$u
        Y0 = residop(Y, X)
        alpha = solve(t(W0) %*% W0) %*% t(W0) %*% Y0
        byx = solve(t(X) %*% X) %*% t(X) %*% Y
        A = t(t(W0) %*% W)
        B = t(solve(t(X) %*% X) %*% t(X) %*% W)
        bwx = t(solve(t(A) %*% A) %*% t(A) %*% B)
    }
    else {
        XZW = cbind(X, Z)
        W = alpha = byx = bwx = NULL
    }
    A = solve(t(XZW) %*% XZW)
    betagammaalphahat = A %*% t(XZW) %*% Y
    resids = Y - XZW %*% betagammaalphahat
    betahat = betagammaalphahat[1:p, , drop = FALSE]
    multiplier = as.matrix(diag(A)[1:p])
    df = m - p - q - k
    sigma2 = apply(resids^2, 2, sum)/df
    sigma2 = as.vector(sigma2)
    se = sqrt(multiplier %*% t(sigma2))
    tvals = betahat/se
    pvals = tvals
    for (i in 1:nrow(pvals)) pvals[i, ] = 2 * pt(-abs(tvals[i, 
        ]), df)
    return(list(betahat = betahat, sigma2 = sigma2, t = tvals, 
        p = pvals, multiplier = multiplier, df = df, W = W, alpha = alpha, 
        byx = byx, bwx = bwx, X = X, k = k, ctl = ctl, Z = Z, 
        fullW = fullW))
}
