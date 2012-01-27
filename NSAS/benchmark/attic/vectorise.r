vectorise <- function (tab)
{
    n <- length(attributes(tab)[[1]])
    dims <- attributes(tab)[[1]]
    len <- prod(attributes(tab)[[1]])
    d2 <- c(dims, 0)
    df <- data.frame(as.vector(tab))
    names(df) <- "value"
    j <- 2
    for (i in 1:n) {
        ech <- max(1, prod(dims[0:(i - 1)]))
        reps <- max(1, prod(d2[(i + 1):n]))
        df[j] <- rep(dimnames(tab)[[i]], reps, each = ech)
        j <- j + 1
    }
    df
}
