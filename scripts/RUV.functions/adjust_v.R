adjust_v <-
function (Y, v, ctl) 
{
    if (is.null(v)) 
        return(Y)
    if (length(v) == 1) 
        if (v == 1) 
            v = matrix(1, 1, ncol(Y))
    Yc = Y[, ctl, drop = FALSE]
    vc = v[, ctl, drop = FALSE]
    return(Y - Yc %*% t(vc) %*% solve(vc %*% t(vc)) %*% v)
}
