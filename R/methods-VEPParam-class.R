### =========================================================================
### VEPParam class methods 
### =========================================================================

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

## Force to list, reset logical data type
.formatList <- function(x)
{
    if (is.list(x))
        return(x)
    x <- as.list(x)
    idx <- x %in% c("TRUE", "FALSE")
    x[idx] <- as.logical(x[idx]) 
    x
} 

VEPParam  <- function(basic=list(), input=list(), cache=list(),
                      output=list(), filterqc=list(), 
                      database=list(), advanced=list(), ...) 
{
    basic_opts <- basicOpts()
    basic_opts[names(basic)] <- .formatList(basic)

    input_opts <- inputOpts()
    input_opts[names(input)] <- .formatList(input)

    cache_opts <- cacheOpts()
    cache_opts[names(cache)] <- .formatList(cache)

    output_opts <- outputOpts()
    output_opts[names(output)] <- .formatList(output)

    filterqc_opts <- filterqcOpts()
    filterqc_opts[names(filterqc)] <- .formatList(filterqc) 

    database_opts <- databaseOpts()
    database_opts[names(database)] <- .formatList(database)

    advanced_opts <- advancedOpts()
    advanced_opts[names(advanced)] <- .formatList(advanced)

    new("VEPParam", basic=basic_opts, input=input_opts, 
        cache=cache_opts, output=output_opts, 
        filterqc=filterqc_opts, database=database_opts, 
        advanced=advanced_opts) 
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity 
###

.valid.VEPParam.basic <- function(x)
{
    current <- basic(x)
    if (!is.numeric(current$fork))
        return("'fork' must be numeric")
    if (!is.character(current$config))
        return("'config' must be character() or a file name")

    target <- basicOpts()
    c(.checkNames(current, target), .checkLogicals(current, target))
} 

.valid.VEPParam.input <- function(x)
{
    current <- input(x)
    if (!identical(character(), current$format))
        if (!current$format %in% 
            c("ensembl", "vcf", "pileup", "hgvs", "id", "vep"))
            return(paste0("'format' must be one of 'ensembl', ",
                          "'vcf', 'pileup', 'hgvs', 'id' or 'vep'."))
    target <- inputOpts()
    c(.checkNames(current, target), .checkLogicals(current, target))
} 

.valid.VEPParam.cache <- function(x)
{
    current <- cache(x)
    target <- cacheOpts()
    c(.checkNames(current, target), .checkLogicals(current, target))
}

.valid.VEPParam.database <- function(x)
{
    current <- database(x)
    if (!is.numeric(current$port))
        return("'port' must be numeric")
    if (!is.numeric(current$db_version))
        return("'db_version' must be numeric")

    target <- databaseOpts()
    c(.checkNames(current, target), .checkLogicals(current, target))
}

.valid.VEPParam.output <- function(x)
{
    current <- output(x)
    if (!is.character(current$cell_type))
        return("'cell_type' must be a character'")
    if (!is.character(current$terms))
        return("'terms' must be character() or 'all' or 'so'")
    if (!is.character(current$sift))
        return("'sift' must be character() or 'p', 's' or 'b'")
    if (!is.character(current$polyphen))
        return("'polyphen' must be character() or 'p', 's' or 'b'")
    if (!is.character(current$convert))
        return(paste0("'convert' must be character() or 'ensembl', ",
               "'vcf' or 'pileup'"))
    if (!is.character(current$custom))
        return("'custom' must be a character'")
    if (!is.character(current$plugin))
        return("'plugin' must be a character'")

    target <- outputOpts()
    c(.checkNames(current, target), .checkLogicals(current, target))
}
 
.valid.VEPParam.filterqc <- function(x)
{
    current <- filterqc(x)
    target <- filterqcOpts()
    c(.checkNames(current, target), .checkLogicals(current, target))
}

.valid.VEPParam.advanced <- function(x)
{
    current <- advanced(x)
    target <- advancedOpts()
    c(.checkNames(current, target), .checkLogicals(current, target))
}

.checkNames <- function(current, target)
{
    invalid <- !names(current) %in% names(target)
    if (any(invalid))
        return(paste0("invalid runtime options '", 
               paste(names(current)[invalid], "'", sep=",")))
    NULL
}

.checkLogicals <- function(current, target)
{
    logic <- current[names(target)[target %in% c(TRUE, FALSE)]]
    invalid <- !logic %in% c(TRUE, FALSE)
    if (any(invalid))
        return(paste0("runtime options '", 
                      paste(names(logic)[invalid], sep=","), 
                      "' must be TRUE or FALSE"))
    NULL 
}

.valid.VEPParam <- function(x)
{
    c(.valid.VEPParam.basic(x),
      .valid.VEPParam.input(x),
      .valid.VEPParam.cache(x),
      .valid.VEPParam.output(x),
      .valid.VEPParam.filterqc(x),
      .valid.VEPParam.database(x),
      .valid.VEPParam.advanced(x))
}

setValidity2("VEPParam", .valid.VEPParam)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Getters and Setters
###

basic <- function(x) slot(x, "basic") 
input <- function(x) slot(x, "input") 
cache <- function(x) slot(x, "cache") 
output <- function(x) slot(x, "output")
filterqc <- function(x) slot(x, "filterqc")
database <- function(x) slot(x, "database")
advanced <- function(x) slot(x, "advanced")

"basic<-" <- function(x, value) 
{
    value <- .formatList(value)
    slot(x, "basic")[names(value)] <- value
    msg <- .valid.VEPParam.basic(x)
    if (!is.null(msg))
        stop(msg) 
    x
}

"input<-" <- function(x, value) 
{
    value <- .formatList(value)
    slot(x, "input")[names(value)] <- value
    msg <- .valid.VEPParam.input(x)
    if (!is.null(msg))
        stop(msg) 
    x 
}

"cache<-" <- function(x, value) 
{
    value <- .formatList(value)
    slot(x, "cache")[names(value)] <- value
    msg <- .valid.VEPParam.cache(x)
    if (!is.null(msg))
        stop(msg) 
    x 
}

"output<-" <- function(x, value) 
{
    value <- .formatList(value)
    slot(x, "output")[names(value)] <- value
    msg <- .valid.VEPParam.output(x)
    if (!is.null(msg))
        stop(msg) 
    x 
}

"filterqc<-" <- function(x, value) 
{
    value <- .formatList(value)
    slot(x, "filterqc")[names(value)] <- value
    msg <- .valid.VEPParam.filterqc(x)
    if (!is.null(msg))
        stop(msg) 
    x 
}

"database<-" <- function(x, value) 
{
    value <- .formatList(value)
    slot(x, "database")[names(value)] <- value
    msg <- .valid.VEPParam.database(x)
    if (!is.null(msg))
        stop(msg) 
    x 
}

"advanced<-" <- function(x, value) 
{
    value <- .formatList(value)
    slot(x, "advanced")[names(value)] <- value
    msg <- .valid.VEPParam.advanced(x)
    if (!is.null(msg))
        stop(msg) 
    x 
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### helpers / utils 
###

basicOpts <- function(verbose=logical(1), quiet=logical(1), 
                      no_progress=logical(1), config=character(), 
                      everything=logical(1), fork=numeric())
{
    list(verbose=verbose, quiet=quiet, no_progress=no_progress,
         config=config, everything=everything, fork=fork)
} 

## 'input_file' is omitted because it's an argument to ensemblVEP()
inputOpts <- function(species="homo_sapiens", format=character(), 
                      output_file=character(), force_overwrite=logical(1),
                      stats_file=character(), no_stats=logical(1),
                      html=logical(1))
{
    list(species=species, format=format, output_file=output_file, 
         force_overwrite=force_overwrite, stats_file=stats_file,
         no_stats=no_stats, html=html)
} 

cacheOpts <- function(cache=logical(1), dir="$HOME/.vep", offline=logical(1),
                      fasta=character())
{
    list(cache=cache, dir=dir, offline=offline, fasta=fasta)
}

outputOpts <- function(terms="so", sift=character(), 
                       polyphen=character(), regulatory=logical(1), 
                       cell_type=character(), hgvs=logical(1), 
                       gene=logical(1), protein=logical(1), hgnc=logical(1), 
                       ccds=logical(1), canonical=logical(1), 
                       xref_refseq=logical(1), numbers=logical(1), 
                       domains=logical(1), most_severe=logical(1), 
                       summary=logical(1), per_gene=logical(1), 
                       convert=character(), fields=character(), 
                       vcf=logical(1), gvf=logical(1), original=logical(1),
                       custom=character(), plugin=character())
{
    list(terms=terms, sift=sift, polyphen=polyphen, regulatory=regulatory,
         cell_type=cell_type, hgvs=hgvs, gene=gene, protein=protein, 
         hgnc=hgnc, ccds=ccds, canonical=canonical, xref_refseq=xref_refseq, 
         numbers=numbers, domains=domains, most_severe=most_severe, 
         summary=summary, per_gene=per_gene, convert=convert, fields=fields, 
         vcf=vcf, gvf=gvf, original=original, custom=custom, plugin=plugin)
}

filterqcOpts <- function(check_ref=logical(1), coding_only=logical(1),
                         check_existing=logical(1), check_alleles=logical(1), 
                         check_svs=logical(1), gmaf=logical(1),
                         maf_1kg=logical(1), individual=character(), 
                         phased=logical(1), chr=character(), 
                         no_intergenic=logical(1), filter_common=logical(1),
                         check_frequency=logical(1), freq_pop=character(),
                         freq_freq=logical(1), freq_gt_lt=character(),
                         freq_filter=character(), filter=character(),
                         failed=logical(1), allow_non_variant=logical(1))
{
    list(check_ref=check_ref, coding_only=coding_only,
         check_existing=check_existing, check_alleles=check_alleles,
         check_svs=check_svs, gmaf=gmaf, maf_1kg=maf_1kg, 
         individual=individual, phased=phased, chr=chr, 
         no_intergenic=no_intergenic, filter_common=filter_common,
         check_frequency=check_frequency, freq_pop=freq_pop,
         freq_freq=freq_freq, freq_gt_lt=freq_gt_lt, 
         freq_filter=freq_filter, filter=filter, failed=failed,
         allow_non_variant=allow_non_variant) 
}

databaseOpts <- function(database=TRUE, host="useastdb.ensembl.org", 
                         user=character(), password=character(), 
                         port=numeric(), genomes=logical(1), 
                         refseq=logical(1), db_version=numeric(), 
                         registry=character())
{
    list(database=database, host=host, user=user, password=password, 
         port=port, genomes=genomes, refseq=refseq,
         db_version=db_version, registry=registry)
} 

advancedOpts <- function(no_whole_genome=logical(1), buffer_size=5000,
                         write_cache=logical(1), build=character(),
                         compress=character(), skip_db_check=logical(1),
                         cache_region_size=numeric())
{
    list(no_whole_genome=no_whole_genome, buffer_size=buffer_size,
         write_cache=write_cache, build=build, compress=compress,
         skip_db_check=skip_db_check, cache_region_size=cache_region_size)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### show 
###

setMethod(show, "VEPParam",
    function(object)
    {
        scat <- function(fmt, vals=character(), exdent=2, ...)
        {
            vals <- ifelse(nzchar(vals), vals, "''")
            lbls <- paste(IRanges:::selectSome(vals), collapse=", ")
            txt <- sprintf(fmt, length(vals), lbls)
            cat(strwrap(txt, exdent=exdent, ...), sep="\n")
        }
        cat("class:", class(object), "\n")
        slots <- c("basic", "input", "cache", "output",
                   "filterqc", "database", "advanced")
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
