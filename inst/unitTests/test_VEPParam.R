test_VEPParam_construction <- function()
{
    p77 <- VEPParam()
    checkIdentical(VEPParam(77), p77) 
    p75 <- VEPParam(75)
    p73 <- VEPParam(73)
    p67 <- VEPParam(67)
    checkTrue(all(slotNames(p67) %in% slotNames(p73)))
    checkTrue(all(slotNames(p67) %in% slotNames(p75)))
    checkTrue(all(slotNames(p73) %in% slotNames(p75)))
    checkTrue(all(slotNames(p75) %in% slotNames(p77)))

    p <- VEPParam()
    p <- VEPParam(basic=list())
    checkException(VEPParam(foo=list()), p, silent=TRUE)
    checkException(VEPParam(basic=list(foo="")), silent=TRUE)
    checkException(VEPParam(basic=list(quiet="yes")), silent=TRUE)
    checkException(VEPParam(basic=c(verbose="foo")), silent=TRUE)
}

test_VEPParam75_defaults <- function()
{
    p <- VEPParam(75)
    checkIdentical(input(p)$species, "homo_sapiens")
    checkIdentical(input(p)$force_overwrite, FALSE)
    checkIdentical(database(p)$host, "useastdb.ensembl.org")
    checkIdentical(database(p)$database, TRUE)
    checkIdentical(cache(p)$dir, "$HOME/.vep")
    checkIdentical(output(p)$terms, "so")
    checkIdentical(advanced(p)$buffer_size, 5000)
    checkIdentical(version(p), 75)
}

test_VEPParam73_defaults <- function()
{
    p <- VEPParam(73)
    checkIdentical(input(p)$species, "homo_sapiens")
    checkIdentical(input(p)$force_overwrite, FALSE)
    checkIdentical(database(p)$host, "ensembldb.ensembl.org")
    checkIdentical(database(p)$database, TRUE)
    checkIdentical(cache(p)$dir, "$HOME/.vep")
    checkIdentical(output(p)$terms, "so")
    checkIdentical(advanced(p)$buffer_size, 5000)
    checkIdentical(version(p), c(73, 74))
}

test_VEPParam67_defaults <- function()
{
    p <- VEPParam(67)
    checkIdentical(input(p)$species, "homo_sapiens")
    checkIdentical(input(p)$force_overwrite, FALSE)
    checkIdentical(database(p)$host, "ensembldb.ensembl.org")
    checkIdentical(cache(p)$dir, "$HOME/.vep")
    checkIdentical(output(p)$terms, "so")
    checkIdentical(advanced(p)$buffer_size, 5000)
    checkIdentical(version(p), 67)
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
