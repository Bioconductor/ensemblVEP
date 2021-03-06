test_VEPParam_construction <- function()
{
    checkException(VEPParam())
    p88 <- VEPParam(88)
    p78 <- VEPParam(78)
    p77 <- VEPParam(77)
    p75 <- VEPParam(75)
    p73 <- VEPParam(73)
    p67 <- VEPParam(67)
    p82 <- VEPParam(82)
    checkTrue(all(slotNames(p67) %in% slotNames(p73)))
    checkTrue(all(slotNames(p67) %in% slotNames(p75)))
    checkTrue(all(slotNames(p73) %in% slotNames(p75)))
    checkTrue(all(slotNames(p75) %in% slotNames(p77)))
    checkTrue(all(slotNames(p77) %in% slotNames(p78)))
    checkTrue(all(slotNames(p78) %in% slotNames(p82)))

    p <- VEPParam(88)
    p <- VEPParam(basic=list(), version=88)
    checkException(VEPParam(foo=list(), version=88), p, silent=TRUE)
    checkException(VEPParam(basic=list(foo=""), version=88), silent=TRUE)
    checkException(VEPParam(basic=list(quiet="yes"), version=88), silent=TRUE)
    checkException(VEPParam(basic=c(verbose="foo"), version=88), silent=TRUE)
}

test_VEPParam82_new_flags <- function()
{
    p <- VEPParam(82)
    checkIdentical(output(p)$gene_phenotype, logical(1))
}

test_VEPParam78_new_flags <- function()
{
    p <- VEPParam(78)
    checkIdentical(filterqc(p)$flag_pick, logical(1))
    checkIdentical(filterqc(p)$flag_pick_allele, logical(1))
    checkIdentical(filterqc(p)$pick_order, numeric())
    checkIdentical(identifier(p)$tsl, logical(1))
}

test_VEPParam77_new_flags <- function()
{
    p <- VEPParam(77)
    checkIdentical(filterqc(p)$pick, logical(1))
    checkIdentical(filterqc(p)$pick_allele, logical(1))
}

test_VEPParam75_defaults <- function()
{
    p <- VEPParam(75)
    checkIdentical(input(p)$species, "homo_sapiens")
    checkIdentical(input(p)$force_overwrite, FALSE)
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
    checkIdentical(cache(p)$dir, "$HOME/.vep")
    checkIdentical(output(p)$terms, "so")
    checkIdentical(advanced(p)$buffer_size, 5000)
    checkIdentical(version(p), 67)
}

test_VEPParam_replace <- function()
{
    ## single values
    p1 <- p2  <- VEPParam(88)
    basic(p1) <- list(verbose=TRUE)
    basic(p2) <- c(verbose=TRUE)
    checkIdentical(p1, p2)
    checkException(basic(p1)$verbose <- "foo", silent=TRUE)
    checkException(basic(p1)$config <- TRUE, silent=TRUE)

    ## multiple values
    p1 <- p2  <- VEPParam(88)
    basic(p1) <- c(verbose=TRUE, config="myconfig.txt")
    basic(p2) <- list(verbose=TRUE, config="myconfig.txt")
    checkIdentical(p1, p2)
}
