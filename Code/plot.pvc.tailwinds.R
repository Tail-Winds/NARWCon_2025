plot.pvc.tailwinds <- function (x, scheme = c("shaded", "lines"), col.term = "darkred",
          lwd.term = 1.5, lty.se = 2, lwd.se = 1, col.se = "orange",
          col.shaded = "gray", factor.plots = FALSE, ...)
{
    scheme <- match.arg(scheme)
    is.Factor <- x$is.Factor
    if (!is.Factor) {
        beta <- x$beta.x
        var <- x$var
        upper <- beta + 2 * sqrt(var)
        lower <- beta - 2 * sqrt(var)
        ran <- range(upper, lower) * c(0.95, 1.05)
        plot(beta[order(x$x)] ~ x$x[order(x$x)], type = "l",
             ylim = ran, ylab = "b(x)", xlab = "x", main = "varying coef.",
             col = col.term, lwd = lwd.term)
        if (scheme == "lines") {
            lines(x$x[order(x$x)], upper[order(x$x)], lty = lty.se,
                  lwd = lwd.se, col = col.se)
            lines(x$x[order(x$x)], lower[order(x$x)], lty = lty.se,
                  lwd = lwd.se, col = col.se)
        }
        else {
            x1 <- x$x[order(x$x)]
            xx <- c(x1, rev(x1))
            yy <- c(lower[order(x$x)], upper[rev(order(x$x))])
            polygon(xx, yy, col = col.shaded, border = col.shaded)
            lines(x$x[order(x$x)], beta[order(x$x)], col = col.term,
                  lwd = lwd.term)
        }
    }
    if (is.Factor) {
        fv <- as.vector(x$fv)
        var <- x$var
        upper <- fv + 2 * sqrt(var)
        lower <- fv - 2 * sqrt(var)

        # browser()
        # Create a data frame with the data for plotting
        d <- data.frame(x = x$x, by = x$by, fv = fv, upper = upper, lower = lower)
        # Return the value for plotting outside of the function
        return(d)


        ran <- range(upper, lower) * c(0.95, 1.05)
        if (!factor.plots) {
            plot(fv[order(x$x)] ~ x$x[order(x$x)], type = "n",
                 ylim = ran, ylab = "f(x)", xlab = "x", main = "var. coef.",
                 col = col.term, lwd = lwd.term)
        }
        nlevs <- nlevels(x$by)
        for (i in levels(x$by)) {
            if (factor.plots) {
                plot(fv[order(x$x)] ~ x$x[order(x$x)], type = "n",
                     ylim = ran, ylab = "f(x)", xlab = "x", main = i,
                     col = col.term, lwd = lwd.term)
            }
            fvi <- fv[x$by == i]
            xi <- x$x[x$by == i]
            ox <- order(xi)
            upperi <- upper[x$by == i]
            loweri <- lower[x$by == i]
            if (scheme == "lines") {
                lines(xi[ox], upperi[ox], lty = lty.se, lwd = lwd.se,
                      col = col.se)
                lines(xi[ox], loweri[ox], lty = lty.se, lwd = lwd.se,
                      col = col.se)
                lines(xi[ox], fvi[ox], col = col.term, lwd = lwd.term)
            }
            else {
                x1 <- xi[ox]
                xx <- c(x1, rev(x1))
                yy <- c(loweri[ox], upperi[rev(ox)])
                polygon(xx, yy, col = col.shaded, border = col.shaded)
                lines(xi[ox], fvi[ox], col = col.term, lwd = lwd.term)
            }
        }
    }
}
