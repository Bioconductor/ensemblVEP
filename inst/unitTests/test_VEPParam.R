
test_VEPParam_slot_options <- function()
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
             "xref_refseq", "numbers", "domains", "most_severe", "summary", 
             "per_gene", "convert", "fields", "vcf", "gvf", "original")
    checkIdentical(opt, names(output(p)))
    opt <- c("check_ref", "coding_only", "check_existing", "check_alleles",
             "check_svs", "gmaf", "individual", "phased", "chr",
             "no_intergenic", "check_frequency", "freq_pop", "freq_freq", 
             "freq_gt_lt", "freq_filter", "filter", "failed",
             "allow_non_variant")
    checkIdentical(opt, names(filterqc(p)))
}

test_VEPParam_subsetting <- function()
{

}

test_VEPParam_replacement <- function()
{

}

test_VEPParam_non_logicals <- function()
{

}
