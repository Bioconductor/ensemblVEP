test_VEPParam_construction <- function()
{
    p <- VEPParam()
    p <- VEPParam(basic=list())
    checkIdentical(VEPParam(foo=list()), p)
    checkException(VEPParam(basic=list(foo="")), silent=TRUE)
    checkException(VEPParam(basic=list(quiet="yes")), silent=TRUE)
    checkException(VEPParam(basic=c(verbose="foo")), silent=TRUE)
}

test_VEPParam_option_defaults <- function()
{
    p <- VEPParam()
    checkIdentical(input(p)$species, "homo_sapiens")
    checkIdentical(input(p)$force_overwrite, FALSE)
    checkIdentical(database(p)$host, "useastdb.ensembl.org")
    checkIdentical(database(p)$database, TRUE)
    checkIdentical(cache(p)$dir, "$HOME/.vep")
    checkIdentical(output(p)$terms, "so")
    checkIdentical(advanced(p)$buffer_size, 5000)
}

test_VEPParam_replace <- function()
{
    ## single values
    p1 <- p2  <- VEPParam()
    basic(p1) <- list(verbose=TRUE)
    basic(p2) <- c(verbose=TRUE)
    checkIdentical(p1, p2) 
    checkException(basic(p1)$verbose <- "foo", silent=TRUE)
    checkException(basic(p1)$config <- TRUE, silent=TRUE)

    ## multiple values
    p1 <- p2  <- VEPParam()
    basic(p1) <- c(verbose=TRUE, config="myconfig.txt")
    basic(p2) <- list(verbose=TRUE, config="myconfig.txt")
    checkIdentical(p1, p2) 
}
