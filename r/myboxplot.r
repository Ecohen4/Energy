myboxplot=function (x, ..., range = 1.5, width = NULL, varwidth = FALSE, 
    notch = FALSE, outline = TRUE, names, boxwex = 0.8, plot = TRUE, 
    border = par("fg"), col = NULL, log = "", pars = NULL, horizontal = FALSE, 
    add = FALSE, at = NULL) 
{
    args <- list(x, ...)
    namedargs <- if (!is.null(attributes(args)$names)) 
        attributes(args)$names != ""
    else rep(FALSE, length.out = length(args))
    pars <- c(args[namedargs], pars)
    groups <- if (is.list(x)) 
        x
    else args[!namedargs]
    if (0 == (n <- length(groups))) 
        stop("invalid first argument")
    if (length(class(groups))) 
        groups <- unclass(groups)
    if (!missing(names)) 
        attr(groups, "names") <- names
    else {
        if (is.null(attr(groups, "names"))) 
            attr(groups, "names") <- 1:n
        names <- attr(groups, "names")
    }
    for (i in 1:n) groups[i] <- list(myboxplot.stats(groups[[i]], 
        range))
    stats <- matrix(0, nr = 5, nc = n)
    conf <- matrix(0, nr = 2, nc = n)
    ng <- out <- group <- numeric(0)
    ct <- 1
    for (i in groups) {
        stats[, ct] <- i$stats
        conf[, ct] <- i$conf
        ng <- c(ng, i$n)
        if ((lo <- length(i$out))) {
            out <- c(out, i$out)
            group <- c(group, rep.int(ct, lo))
        }
        ct <- ct + 1
    }
    z <- list(stats = stats, n = ng, conf = conf, out = out, 
        group = group, names = names)
    if (plot) {
        bxp(z, width, varwidth = varwidth, notch = notch, boxwex = boxwex, 
            border = border, col = col, log = log, pars = pars, 
            outline = outline, horizontal = horizontal, add = add, 
            at = at)
        invisible(z)
    }
    else z
}