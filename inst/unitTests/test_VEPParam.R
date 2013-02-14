test_VEPParam_construction <- function()
{
    p <- VEPParam()
    p <- VEPParam(basic=list())
    checkIdentical(VEPParam(foo=list()), p)
    checkException(VEPParam(basic=list(foo="")), silent=TRUE)
    checkException(VEPParam(basic=c(verbose="foo")), silent=TRUE)
}

test_VEPParam_option_names <- function()
{
    p <- VEPParam()
    opt <- c("verbose", "quiet", "no_progress", "config",
             "everything", "fork")
    checkIdentical(opt, names(basic(p)))
    opt <- c("species", "format", "output_file", "force_overwrite")
    checkIdentical(opt, names(input(p)))
    opt <- c("host", "user", "password", "port", "genomes", "refseq",
             "db_version", "registry")
    checkIdentical(opt, names(database(p)))
    opt <- c("terms", "sift", "polyphen", "regulatory", "cell_type",
             "hgvs", "gene", "protein", "hgnc", "ccds", "canonical", 
             "xref_refseq", "numbers", "domains", "most_severe", 
             "summary", "per_gene", "convert", "fields", "vcf", "gvf", 
             "original")
    checkIdentical(opt, names(output(p)))
    opt <- c("check_ref", "coding_only", "check_existing", 
             "check_alleles", "check_svs", "gmaf", "individual", 
             "phased", "chr", "no_intergenic", "check_frequency", 
             "freq_pop", "freq_freq", "freq_gt_lt", "freq_filter", 
             "filter", "failed", "allow_non_variant")
    checkIdentical(opt, names(filterqc(p)))
}

test_VEPParam_option_defaults <- function()
{
    p <- VEPParam()
    checkIdentical(input(p)$species, "homo_sapiens")
    checkIdentical(input(p)$force_overwrite, FALSE)
    checkIdentical(database(p)$host, "useastdb.ensembl.org")
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
