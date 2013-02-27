### =========================================================================
### VEPParam class methods 
### =========================================================================

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

VEPParam  <- function(basic=list(), input=list(), database=list(), 
    output=list(), filterqc=list(), 
    ...) 
{
    bopts <- basicOpts()
    bopts[names(basic)] <- .asLogical(basic)

    iopts <- inputOpts()
    iopts[names(input)] <- .asLogical(input)

    dbopts <- databaseOpts()
    dbopts[names(database)] <- .asLogical(database)

    oopts <- outputOpts()
    oopts[names(output)] <- .asLogical(output)

    fopts <- filterqcOpts()
    fopts[names(filterqc)] <- .asLogical(filterqc) 

    new("VEPParam", basic=bopts, input=iopts, database=dbopts, 
        output=oopts, filterqc=fopts)
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity 
###

.valid.VEPParam.basic <- function(x)
{
    b <- basic(x)
    opt <- c("verbose", "quiet", "no_progress", "config",
             "everything", "fork")
    if (any(invalid <- !names(b) %in% opt))
        return(paste0("invalid options: ", 
               paste(names(b)[invalid], sep=",")))

    logic <- b[!names(b) %in% c("config", "fork")]
    if (!all(valid <- sapply(logic, is.logical)))
        return(paste0(paste(logic[!valid], sep=","), 
               " must be TRUE or FALSE"))

    if (!is.character(b$config))
        return("'config' must be character() or a file name")
    NULL 
} 

.valid.VEPParam.input <- function(x)
{
    i <- input(x)
    opt <- c("species", "format", "output_file", "force_overwrite")
    if (any(invalid <- !names(i) %in% opt))
        return(paste0("invalid options: ", 
               paste(names(i)[invalid], sep=",")))
    format <- i$format
    if (!identical(character(), format))
        if (!format %in% c("ensembl", "vcf", "pileup", "hgvs",
                           "id", "vep"))
            return(paste0("'format' must be one of 'ensembl', ",
                          "'vcf', 'pileup', 'hgvs', 'id' or 'vep'."))
    NULL 
} 

.valid.VEPParam.database <- function(x)
{
    d <- database(x)
    opt <- c("host", "user", "password", "port", "genomes", 
             "refseq", "db_version", "registry")
    if (any(invalid <- !names(d) %in% opt))
        return(paste0("invalid options: ", 
               paste(names(d)[invalid], sep=",")))
    if (!is.numeric(d$port))
        return("'port' must be numeric")
    if (!is.numeric(d$db_version))
        return("'db_version' must be numeric")
    NULL
}

.valid.VEPParam.output <- function(x)
{
    o <- output(x)
    opt <- c("terms", "sift", "polyphen", "regulatory", "cell_type",
             "hgvs", "gene", "protein", "hgnc", "ccds", "canonical", 
             "xref_refseq", "numbers", "domains", "most_severe", 
             "summary", "per_gene", "convert", "fields", "vcf", "gvf", 
             "original")
    if (any(invalid <- !names(o) %in% opt))
        return(paste0("invalid options: ", 
               paste(names(o)[invalid], sep=",")))
    logic <- o[c("regulatory", "hgvs", "gene", "protein", "hgnc", 
                 "ccds", "canonical", "xref_refseq", "numbers", 
                 "domains", "most_severe", "summary", "per_gene",
                 "gvf", "original")]
    if (any(nonlog <- !sapply(logic, is.logical)))
        return(paste0(paste(names(nonlog)[nonlog], sep=","), 
               " must be TRUE or FALSE"))
    if (!is.character(o$cell_type))
        return("'cell_type' must be a character'")
    if (is.logical(o$terms))
        if (o$terms)
            return("'terms' must be character() or 'all' or 'so'")
    if (is.logical(o$sift))
        if (o$sift)
            return("'sift' must be character() or 'p', 's' or 'b'")
    if (is.logical(o$polyphen))
        if (o$polyphen)
            return("'polyphen' must be characater() or 'p', 's' or 'b'")
    if (is.logical(o$convert))
        if (o$convert)
          return(paste0("'convert' must be character() or 'ensembl', ",
                 "'vcf' or 'pileup'"))
    NULL 
}
 
.valid.VEPParam.filterqc <- function(x)
{
    f <- filterqc(x)
    opt <- c("check_ref", "coding_only", "check_existing", 
             "check_alleles", "check_svs", "gmaf", "individual", 
             "phased", "chr", "no_intergenic", "check_frequency", 
             "freq_pop", "freq_freq", "freq_gt_lt", "freq_filter", 
             "filter", "failed", "allow_non_variant")
    if (any(invalid <- !names(f) %in% opt))
        return(paste0("invalid options: ", 
               paste(names(f)[invalid], sep=",")))
    logic <- f[c("check_ref", "coding_only",
        "check_existing", "check_alleles", "check_svs", "gmaf", 
        "phased", "no_intergenic", "check_frequency", "failed", 
        "allow_non_variant")]
    if (any(nonlog <- !sapply(logic, is.logical)))
        return(paste0(paste(names(nonlog)[nonlog], sep=","), 
               " must be TRUE or FALSE"))
    NULL 
}

.valid.VEPParam <- function(x)
{
    c(.valid.VEPParam.basic(x),
      .valid.VEPParam.input(x),
      .valid.VEPParam.database(x),
      .valid.VEPParam.output(x),
      .valid.VEPParam.filterqc(x))
}

setValidity2("VEPParam", .valid.VEPParam)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Getters and Setters
###

basic <- function(x) slot(x, "basic") 
input <- function(x) slot(x, "input") 
database <- function(x) slot(x, "database")
output <- function(x) slot(x, "output")
filterqc <- function(x) slot(x, "filterqc")

.asLogical <- function(x)
{
    if (is.list(x))
        return(x)
    x <- as.list(x)
    idx <- x %in% c("TRUE", "FALSE")
    x[idx] <- as.logical(x[idx]) 
    x
} 

"basic<-" <- function(x, value) 
{
    value <- .asLogical(value)
    slot(x, "basic")[names(value)] <- value
    msg <- .valid.VEPParam.basic(x)
    if (!is.null(msg))
        stop(msg) 
    x
}

"input<-" <- function(x, value) 
{
    value <- .asLogical(value)
    slot(x, "input")[names(value)] <- value
    msg <- .valid.VEPParam.input(x)
    if (!is.null(msg))
        stop(msg) 
    x 
}

"database<-" <- function(x, value) 
{
    value <- .asLogical(value)
    slot(x, "database")[names(value)] <- value
    msg <- .valid.VEPParam.database(x)
    if (!is.null(msg))
        stop(msg) 
    x 
}

"output<-" <- function(x, value) 
{
    value <- .asLogical(value)
    slot(x, "output")[names(value)] <- value
    msg <- .valid.VEPParam.output(x)
    if (!is.null(msg))
        stop(msg) 
    x 
}

"filterqc<-" <- function(x, value) 
{
    value <- .asLogical(value)
    slot(x, "filterqc")[names(value)] <- value
    msg <- .valid.VEPParam.filterqc(x)
    if (!is.null(msg))
        stop(msg) 
    x 
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### helpers / utils 
###

basicOpts <- function(verbose=logical(1), quiet=logical(1), 
                      no_progress=logical(1), config=character(), 
                      everything=logical(1), fork=integer(0))
{
    list(verbose=verbose, quiet=quiet, no_progress=no_progress,
         config=config, everything=everything, fork=fork)
} 


inputOpts <- function(species="homo_sapiens", format=character(), 
                      output_file=character(), 
                      force_overwrite=logical(1))
{
    list(species=species, format=format, output_file=output_file, 
         force_overwrite=force_overwrite)
} 

databaseOpts <- function(host="useastdb.ensembl.org", user=character(), 
                         password=character(), port=numeric(), 
                         genomes=logical(1), refseq=logical(1),
                         db_version=numeric(), registry=character())
{
    list(host=host, user=user, password=password, 
         port=port, genomes=genomes, refseq=refseq,
         db_version=db_version, registry=registry)
} 

outputOpts <- function(terms=character(), sift=character(), 
                       polyphen=character(), regulatory=logical(1), 
                       cell_type=character(), hgvs=logical(1), 
                       gene=logical(1), protein=logical(1), hgnc=logical(1), 
                       ccds=logical(1), canonical=logical(1), 
                       xref_refseq=logical(1), numbers=logical(1), 
                       domains=logical(1), most_severe=logical(1), 
                       summary=logical(1), per_gene=logical(1), 
                       convert=character(), fields=character(), 
                       vcf=logical(1), gvf=logical(1), original=logical(1))
{
    list(terms=terms, sift=sift, polyphen=polyphen, regulatory=regulatory,
         cell_type=cell_type, hgvs=hgvs, gene=gene, protein=protein, 
         hgnc=hgnc, ccds=ccds, canonical=canonical, xref_refseq=xref_refseq, 
         numbers=numbers, domains=domains, most_severe=most_severe, 
         summary=summary, per_gene=per_gene, convert=convert, fields=fields, 
         vcf=vcf, gvf=gvf, original=original)
}
 
filterqcOpts <- function(check_ref=logical(1), coding_only=logical(1),
                         check_existing=logical(1), check_alleles=logical(1), 
                         check_svs=logical(1), gmaf=logical(1),
                         individual=character(), phased=logical(1),
                         chr=character(), no_intergenic=logical(1),
                         check_frequency=logical(1), freq_pop=character(),
                         freq_freq=logical(1), freq_gt_lt=character(),
                         freq_filter=character(), filter=character(),
                         failed=logical(1), allow_non_variant=logical(1))
{
    list(check_ref=check_ref, coding_only=coding_only,
         check_existing=check_existing, check_alleles=check_alleles,
         check_svs=check_svs, gmaf=gmaf, individual=individual,
         phased=phased, chr=chr, no_intergenic=no_intergenic,
         check_frequency=check_frequency, freq_pop=freq_pop,
         freq_freq=freq_freq, freq_gt_lt=freq_gt_lt, 
         freq_filter=freq_filter, filter=filter, failed=failed,
         allow_non_variant=allow_non_variant) 
}

#cacheOpts <- function(no_whole_genome=logical(1), cache=logical(1),
#                      dir=logical(1), offline=logical(1),
#                      buffer_size=logical(1), write_cache=logical(1),
#                      build=logical(1), compress=logical(1), 
#                      skip_db_check=logical(1), cache_region_size=numeric(),
#                      fasta=logical(1))
#{
#    list(no_whole_genome=no_whole_genome, cache=cache, dir=dir, offline=offline,
#         buffer_size=buffer_size, write_cache=write_cache, build=build,
#         compress=compress, skip_db_check=skip_db_check, 
#         cache_region_size=cache_region_size, fasta=fasta)
#}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### show 
###

setMethod(show, "VEPParam",
    function(object)
    {
        scat <- function(fmt, vals=character(), exdent=2, ...)
        {
            vals <- ifelse(nzchar(vals), vals, "''")
            lbls <- paste(IRanges:::selectSome(vals), collapse=" ")
            txt <- sprintf(fmt, length(vals), lbls)
            cat(strwrap(txt, exdent=exdent, ...), sep="\n")
        }
        cat("class:", class(object), "\n")
        slots <- c("basic", "input", "database", "output",
                   "filterqc")
        for (i in slots) {
            elt <- slot(object, i)
            drop <- elt == FALSE | elementLengths(elt) == 0L
            drop[is.na(drop)] <- FALSE
            if (is.null(nms <- names(elt)[!drop]))
                nms <- character()
            scat(paste0(i, "(%d): %s\n"), nms)
        }
   }
)
