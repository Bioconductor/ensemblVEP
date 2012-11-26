### =========================================================================
### VEPParam class methods 
### =========================================================================

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

VEPParam  <- function(basic=basicOpts(), input=inputOpts(), 
    database=databaseOpts(), output=outputOpts(), 
    filterqc=filterqcOpts(), 
    ...) 
{
    new("VEPParam", basic=basic, input=input, database=database, 
       output=output, filterqc=filterqc)
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity 
###

.valid.VEPParam.basic <- function(x)
{
    lg <- basic(x)[!names(basic(x)) %in% "config"] 
    if (!all(valid <- sapply(lg, is.logical)))
        return(paste0(paste(lg[!valid], sep=","), 
               " must be TRUE or FALSE"))

    config <- basic(x)$config
    if (is.logical(config))
        if (config)
            return("'config' must be FALSE or a file name")
    NULL 
} 

.valid.VEPParam.input <- function(x)
{
    format <- x$format
    if (!format %in% c("ensembl", "vcf", "pileup", "hgvs",
                       "id", "vep"))
        return(paste0("'format' must be one of 'ensembl', 'vcf', ", 
               "'pileup', 'hgvs', 'id' or 'vep'."))
    if (!x$force_overwrite)
        return("'force_overwrite' must be TRUE when 'format=vcf'.")
    NULL 
} 

.valid.VEPParam.output <- function(x)
{
    output <- output(x)
    lg <- output[c("regulatory", "cell_type", "hgvs",
        "gene", "protein", "hgnc", "ccds", "canonical", "xref_refseq",
        "numbers", "domains", "most_severe", "summary", "per_gene",
        "gvf", "original")]  
    if (any(nonlog <- !sapply(lg, is.logical)))
        return(paste0(paste(names(nonlog)[nonlog], sep=","), 
               " must be TRUE or FALSE"))

    if (is.logical(output$terms))
        if (output$terms)
            return("'terms' must be FALSE or 'all' or 'so'")
    if (is.logical(output$sift))
        if (output$sift)
            return("'sift' must be FALSE or 'p', 's' or 'b'")
    if (is.logical(output$polyphen))
        if (output$polyphen)
            return("'polyphen' must be FALSE or 'p', 's' or 'b'")
    if (is.logical(output$convert))
        if (output$convert)
          return("'convert' must be FALSE or 'ensembl', 'vcf' or 'pileup'")
    NULL 
}
 
.valid.VEPParam.filterqc <- function(x)
{
    filt <- filterqc(x)
    lg <- filt[c("check_ref", "coding_only",
        "check_existing", "check_alleles", "check_svs", "gmaf", 
        "phased", "no_intergenic", "check_frequency", "failed", 
        "allow_non_variant")]
    if (any(nonlog <- !sapply(lg, is.logical)))
        return(paste0(paste(names(nonlog)[nonlog], sep=","), 
               " must be TRUE or FALSE"))
    NULL 
}

.valid.VEPParam <- function(x)
{
    c(.valid.VEPParam.basic(x),
      .valid.VEPParam.input(x),
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
#cache <- function(x) slot(x, "cache")

"basic<-" <- function(x, value) 
{
    slot(x, "basic")[names(value)] <- value
    validObject(x) 
    x
}

"input<-" <- function(x, value) 
{
    slot(x, "input")[names(value)] <- value
    validObject(x) 
    x 
}

"database<-" <- function(x, value) 
{
    slot(x, "database")[names(value)] <- value
    x 
}

"output<-" <- function(x, value) 
{
    slot(x, "output")[names(value)] <- value
    validObject(x) 
    x 
}

"filterqc<-" <- function(x, value) 
{
    slot(x, "filterqc")[names(value)] <- value
    validObject(x) 
    x 
}

#"cache<-" <- function(x, value) 
#{
#    slot(x, "cache")[names(value)] <- value
#    .valid.VEPParam.filterqc(x) 
#    x 
#}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### helpers / utils 
###

basicOpts <- function(verbose=logical(1), quiet=logical(1), 
                      no_progress=logical(1), config=logical(1), 
                      everything=logical(1), fork=logical(1))
{
    list(verbose=verbose, quiet=quiet, no_progress=no_progress,
         config=config, everything=everything, fork=fork)
} 


inputOpts <- function(species="homo_sapiens", format="vcf", 
                      output_file=paste0(tempdir(), "/temp.vcf"), 
                      force_overwrite=TRUE)
{
    list(species=species, format=format, output_file=output_file, 
         force_overwrite=force_overwrite)
} 

databaseOpts <- function(host="useastdb.ensembl.org", user=logical(1), 
                         password=logical(1), port=logical(1), 
                         genomes=logical(1), refseq=logical(1),
                         db_version=logical(1), registry=logical(1))
{
    list(host=host, user=user, password=password, 
         port=port, genomes=genomes, refseq=refseq,
         db_version=db_version, registry=registry)
} 

outputOpts <- function(terms=logical(1), sift="b", 
                       polyphen="b", regulatory=logical(1), 
                       cell_type=logical(1), hgvs=logical(1), gene=logical(1),
                       protein=logical(1), hgnc=logical(1), ccds=logical(1),
                       canonical=logical(1), xref_refseq=logical(1), 
                       numbers=logical(1), domains=logical(1), 
                       most_severe=logical(1), summary=logical(1), 
                       per_gene=logical(1), convert=logical(1),
                       fields=logical(1), vcf=logical(1), gvf=logical(1), 
                       original=logical(1))
{
    default <- c("Uploaded_variation", "Location", "Allele", "Gene", 
        "Feature", "Feature_type", "Consequence", "Existing_variation")
    if (is.character(fields))
        fields <- unique(c(default, fields))
    else
        fields <- default
    list(terms=terms, sift=sift, polyphen=polyphen, regulatory=regulatory,
         cell_type=cell_type, hgvs=hgvs, gene=gene, protein=protein, hgnc=hgnc,
         ccds=ccds, canonical=canonical, xref_refseq=xref_refseq, numbers=numbers,
         domains=domains, most_severe=most_severe, summary=summary,
         per_gene=per_gene, convert=convert, fields=paste0(fields, collapse=","),
         vcf=vcf, gvf=gvf, original=original)
}
 
filterqcOpts <- function(check_ref=logical(1), coding_only=logical(1),
                         check_existing=logical(1), check_alleles=logical(1), 
                         check_svs=logical(1), gmaf=logical(1),
                         individual=logical(1), phased=logical(1),
                         chr=logical(1), no_intergenic=logical(1),
                         check_frequency=logical(1), freq_pop=logical(1),
                         freq_freq=logical(1), freq_gt_lt=logical(1),
                         freq_filter=logical(1), filter=logical(1),
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
#    list(no_while_genome=no_whole_genome, cache=cache, dir=dir, offline=offline,
#         buffer_size=buffer_size, write_cache=write_cache, build=build,
#         compress=compress, skip_db_check=skip_db_check, 
#         cache_region_size=cache_region_size, fasta=fasta)
#}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### $ and $<- methods
###

setMethod("$", "VEPParam",
    function(x, name)
        c(basic(x), input(x), database(x), output(x),
          filterqc(x))[[name]]
)

setReplaceMethod("$", "VEPParam",
    function(x, name, value)
    { 
        if (name %in% names(basic(x)))
            basic(x)[[name]] <- value
        else if (name %in% names(input(x)))
            input(x)[[name]] <- value
        else if (name %in% names(database(x)))
            database(x)[[name]] <- value
        else if (name %in% names(output(x)))
            output(x)[[name]] <- value
        else if (name %in% names(filterqc(x)))
            filterqc(x)[[name]] <- value
        else
            stop(paste0("'", name, "'", "not a valid option"))
        x
    }
)

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
        cat("Runtime options:\n")
        slots <- c("basic", "input", "database", "output",
                   "filterqc")
        for (i in slots) { 
            idx <- sapply(slot(object, i), 
                       function(elt) {
                           if (is.logical(elt))
                               ifelse(elt, TRUE , FALSE)
                           else
                               ifelse(is.character(elt), TRUE, FALSE)
                       })
            nms <- names(slot(object, i))[idx]
            if (is.null(nms))
                nms <- character()
            scat(paste0(i, "(%d): %s\n"), nms)
        }
   }
)
