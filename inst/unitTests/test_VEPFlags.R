test_VEPParam_construction <- function()
{
    checkException(VEPFlags(version=89))
    checkException(VEPFlags(version=67))
    p <- VEPFlags(flags=list())
    checkIdentical(flags(p)$database, TRUE)
    checkIdentical(flags(p)$vcf, FALSE)
    checkException(VEPFlags(foo=list()), p, silent=TRUE)
}

test_VEPParam_replace <- function()
{
    ## single values
    p1 <- p2  <- VEPFlags()
    flags(p1) <- list(verbose=TRUE)
    flags(p2) <- c(verbose=TRUE)
    checkIdentical(p1, p2)

    ## multiple values
    p1 <- p2  <- VEPFlags()
    flags(p1) <- c(verbose=TRUE, config="myconfig.txt")
    flags(p2) <- list(verbose=TRUE, config="myconfig.txt")
    checkIdentical(p1, p2)
}
