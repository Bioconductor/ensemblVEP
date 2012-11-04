### =========================================================================
### VEPParam class methods 
### =========================================================================

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

VEPParam  <- function(basic=basicOpts(), input=inputOpts(), 
    database=databaseOpts(), output=outputOpts(), 
    filterqc=filterqcOpts(), cache=cacheOpts(), 
    ...) 
{
    new("VEPParam", basic=basic, input=input, database=database, 
       output=output, filterqc=filterqc, cache=cache)
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Getters and Setters
###

basic <- function(x) slot(x, "basic") 
input <- function(x) slot(x, "input") 
database <- function(x) slot(x, "database")
output <- function(x) slot(x, "output")
filterqc <- function(x) slot(x, "filterqc")
cache <- function(x) slot(x, "cache")

"basic<-" <- function(x, value) 
{
    slot(x, "basic")[names(value)] <- value
    x 
}

"input<-" <- function(x, value) 
{
    slot(x, "input")[names(value)] <- value
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
    x 
}

"filterqc<-" <- function(x, value) 
{
    slot(x, "filterqc")[names(value)] <- value
    x 
}

"cache<-" <- function(x, value) 
{
    slot(x, "cache")[names(value)] <- value
    x 
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
            lbls <- paste(IRanges:::selectSome(vals), collapse=" ")
            txt <- sprintf(fmt, length(vals), lbls)
            cat(strwrap(txt, exdent=exdent, ...), sep="\n")
        }
        cat("class:", class(object), "\n")
        slots <- c("basic", "input", "database", "output",
                   "filterqc", "cache")
        for (i in slots) { 
            idx <- sapply(slot(object, i), function(elt) length(elt) > 0L)
            nms <- names(slot(object, i))[idx]
            if (is.null(nms))
                nms <- character()
            scat(paste0(i, "(%d): %s\n"), nms)
        }
    }
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### helpers / utils 
###

basicOpts <- function(verbose=logical(), quiet=logical(), no_progress=logical(),
                      config=logical(), everything=logical(), fork=logical())
{
    list(verbose=verbose, quiet=quiet, no_progress=no_progress,
         config=config, everything=everything, fork=fork)
} 

inputOpts <- function(species="homo_sapiens", input_file=character(), 
                      format="vcf", output_file=paste0(tempdir(), "/temp.vcf"), 
                      force_overwrite=TRUE)
{
    list(species=species, input_file=input_file, format=format, 
         output_file=output_file, force_overwrite=force_overwrite)
} 

databaseOpts <- function(host="useastdb.ensembl.org", user=character(), 
                         password=character(), port=character(), 
                         genomes=character(), refseq=character(),
                         db_version=character(), registry=character())
{
    list(host=host, user=user, password=password, 
         port=port, genomes=genomes, refseq=refseq,
         db_version=db_version, registry=registry)
} 

outputOpts <- function(terms=character(), sift="b", 
                       polyphen="b", regulatory=logical(), 
                       cell_type=list(), hgvs=logical(), gene=logical(),
                       protein=logical(), hgnc=logical(), ccds=logical(),
                       canonical=logical(), xref_refseq=logical(), 
                       numbers=logical(), domains=logical(), 
                       most_severe=logical(), summary=logical(), 
                       per_gene=logical(), convert=character(),
                       fields=paste0(c("Allele", "Gene", "Feature",
                           "Feature_type", "Consequence", "Protein_position",
                           "Amino_acids", "Codons", "Existing_variation"), 
                           collapse=","), 
                       vcf="CSQ", gvf=logical(), 
                       original=logical(), custom=character(),
                       plugin=character())
{
    list(terms=terms, sift=sift, polyphen=polyphen, regulatory=regulatory,
         cell_type=cell_type, hgvs=hgvs, gene=gene, protein=protein, hgnc=hgnc,
         ccds=ccds, canonical=canonical, xref_refseq=xref_refseq, numbers=numbers,
         domains=domains, most_severe=most_severe, summary=summary,
         per_gene=per_gene, convert=convert, fields=fields, vcf=vcf, gvf=gvf,
         original=original, custom=custom, plugin=plugin)
}
 
filterqcOpts <- function(check_ref=logical(), coding_only=logical(),
                         check_existing=logical(), check_alleles=logical(), 
                         check_svs=logical(), gmaf=logical(),
                         individual=list(), phased=logical(),
                         chr=list(), no_intergenic=logical(),
                         check_frequency=logical(), freq_pop=character(),
                         freq_freq=numeric(), freq_gt_lt=logical(),
                         freq_filter=logical(), filter=character(),
                         failed=logical(), allow_non_variant=logical())
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

cacheOpts <- function(no_whole_genome=logical(), cache=logical(),
                      dir=character(), offline=logical(),
                      buffer_size=numeric(), write_cache=logical(),
                      build=list(), compress=logical(), 
                      skip_db_check=logical(), cache_region_size=numeric(),
                      fasta=character())
{
    list(no_while_genome=no_whole_genome, cache=cache, dir=dir, offline=offline,
         buffer_size=buffer_size, write_cache=write_cache, build=build,
         compress=compress, skip_db_check=skip_db_check, 
         cache_region_size=cache_region_size, fasta=fasta)
}
 
