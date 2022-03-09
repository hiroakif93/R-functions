#'Visualizing read quality indices per sample
#'
#' @importFrom vegan specnumber
#' @importFrom vegan rarefy
#' 
#' 
#'@reference https://dave-clark.github.io/post/speeding-up-rarefaction-curves-for-microbial-community-ecology/
#'@export
quickRareCurve <- function (x, step = 1, sample, xlab = "Sample Size",
                            ylab = "Species", label = TRUE, col, lty, max.cores = T, nCores = 1, ...)
{
    require(parallel)
    x <- as.matrix(x)
    if (!identical(all.equal(x, round(x)), TRUE))
        stop("function accepts only integers (counts)")
    if (missing(col))
        col <- par("col")
    if (missing(lty))
        lty <- par("lty")
    tot <- rowSums(x) # calculates library sizes
    S <- specnumber(x) # calculates n species for each sample
    if (any(S <= 0)) {
        message("empty rows removed")
        x <- x[S > 0, , drop = FALSE]
        tot <- tot[S > 0]
        S <- S[S > 0]
    } # removes any empty rows
    nr <- nrow(x) # number of samples
    col <- rep(col, length.out = nr)
    lty <- rep(lty, length.out = nr)
    # parallel mclapply
    # set number of cores
    mc <- getOption("mc.cores", ifelse(max.cores, detectCores(), nCores))
    message(paste("Using ", mc, " cores"))
    out <- mclapply(seq_len(nr), mc.cores = mc, function(i) {
        n <- seq(1, tot[i], by = step)
        if (n[length(n)] != tot[i])
            n <- c(n, tot[i])
        drop(rarefy(x[i, ], n))
    })
    Nmax <- sapply(out, function(x) max(attr(x, "Subsample")))
    Smax <- sapply(out, max)
    plot(c(1, max(Nmax)), c(1, max(Smax)), xlab = xlab, ylab = ylab,
         type = "n", ...)
    
    for (ln in seq_along(out)) {
        N <- attr(out[[ln]], "Subsample")
        lines(N, out[[ln]], col = col[ln], lty = lty[ln], ...)
    }
    if (label) {
        ordilabel(cbind(tot, S), labels = rownames(x), ...)
    }
    invisible(out)
}
