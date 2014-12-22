### =========================================================================
### VEPParam runtime options 
### =========================================================================

.version_error <- function(x)
{
    if (!any(x %in% unlist(supportedVEP())))
        stop(paste0("'version' must be one of ", 
             paste(unname(unlist(supportedVEP())), collapse=",")))
}

### basicOpts, advancedOpts are the same for all versions
basicOpts <- function(version, ..., verbose=logical(1), quiet=logical(1), 
                      no_progress=logical(1), config=character(), 
                      everything=logical(1), fork=numeric())
{
    list(verbose=verbose, quiet=quiet, no_progress=no_progress,
         config=config, everything=everything, fork=fork)
}

advancedOpts <- function(version, ..., no_whole_genome=logical(1), 
                         buffer_size=5000, write_cache=logical(1), 
                         build=character(), compress=character(), 
                         skip_db_check=logical(1), cache_region_size=numeric())
{
    list(no_whole_genome=no_whole_genome, buffer_size=buffer_size,
         write_cache=write_cache, build=build, compress=compress,
         skip_db_check=skip_db_check, cache_region_size=cache_region_size)
}

### inputOpts, cacheOpts, outputOpts, databaseOpts and filterqcOpts are
### different for versions 67, 73, 75
inputOpts <- function(version, ..., species="homo_sapiens", 
                      assembly=character(),
                      format=character(), output_file=character(), 
                      force_overwrite=logical(1), stats_file=character(), 
                      no_stats=logical(1), stats_text=logical(1), 
                      html=logical(1))
{
    if (any(version == 67)) {
        opts <- list(species=species, format=format, output_file=output_file,
                     force_overwrite=force_overwrite, stats_file=stats_file,
                     no_stats=no_stats, html=html)
    } else {
        opts <- list(species=species, format=format, output_file=output_file, 
                     force_overwrite=force_overwrite, stats_file=stats_file,
                     no_stats=no_stats, stats_text=stats_text, html=html)
    }
    if (any(version >= 77))
        opts$assembly <- assembly

    opts
} 

cacheOpts <- function(version, ..., cache=logical(1), dir="$HOME/.vep", 
                      dir_cache="$HOME/.vep",dir_plugins="$HOME/.vep", 
                      offline=logical(1), fasta=character(), 
                      cache_version=numeric())
{
    if (any(version == 67)) {
        list(cache=cache, dir=dir, offline=offline, fasta=fasta)
    } else if (all(version > 67 & version < 75)) {
        list(cache=cache, dir=dir, dir_cache=dir_cache, 
             dir_plugins=dir_plugins, offline=offline, fasta=fasta)
    } else {
        list(cache=cache, dir=dir, dir_cache=dir_cache, 
             dir_plugins=dir_plugins, offline=offline, fasta=fasta, 
             cache_version=cache_version)
    }
}

outputOpts <- function(version, ..., sift=character(), polyphen=character(), 
                       humdiv=logical(1), regulatory=logical(1), 
                       cell_type=character(), custom=character(), 
                       plugin=character(),
                       individual=character(), phased=logical(1),
                       allele_number=integer(), total_length=character(),
                       numbers=character(), domains=character(),
                       no_escape=logical(1), terms="so", hgvs=logical(1),
                       gene=logical(1), protein=logical(1), hgnc=logical(1),
                       ccds=logical(1), canonical=logical(1),
                       xref_refseq=logical(1),
                       most_severe=logical(1), summary=logical(1), 
                       per_gene=logical(1), convert=character(), 
                       fields=character(), vcf=logical(1), gvf=logical(1), 
                       original=logical(1))
{
    if (any(version == 67)) {
        list(terms=terms, sift=sift, polyphen=polyphen, regulatory=regulatory,
             cell_type=cell_type, hgvs=hgvs, gene=gene, protein=protein,
             hgnc=hgnc, ccds=ccds, canonical=canonical, 
             xref_refseq=xref_refseq,
             numbers=numbers, domains=domains, most_severe=most_severe,
             summary=summary, per_gene=per_gene, convert=convert, fields=fields,
             vcf=vcf, gvf=gvf, original=original, custom=custom, plugin=plugin)
    } else if (any(version >= 77)) {
        list(sift=sift, polyphen=polyphen, humdiv=humdiv, 
             regulatory=regulatory,
             cell_type=cell_type, custom=custom, plugin=plugin,
             individual=individual, phased=phased, allele_number=allele_number,
             total_length=total_length, numbers=numbers, domains=domains,
             no_escape=no_escape, terms=terms)
    } else {
        list(sift=sift, polyphen=polyphen, regulatory=regulatory,
             cell_type=cell_type, custom=custom, plugin=plugin,
             individual=individual, phased=phased, allele_number=allele_number,
             total_length=total_length, numbers=numbers, domains=domains,
             no_escape=no_escape, terms=terms)
    }
}

databaseOpts <- function(version, ..., database=TRUE, 
                         host="useastdb.ensembl.org", 
                         user=character(), password=character(), 
                         port=numeric(), genomes=logical(1), 
                         gencode_basic=logical(1), refseq=logical(1), 
                         merged=logical(1), all_refseq=logical(1),
                         lrg=logical(1), db_version=numeric(), 
                         registry=character())
{
    opts <-  list(host=host, user=user, password=password, 
             port=port, genomes=genomes, refseq=refseq,
             db_version=db_version, registry=registry)
    if (any(version < 75))
        opts$host <- "ensembldb.ensembl.org"
    if (any(version > 67))
        opts$database <- database
    if (any(version >= 77))
        opts <- as.list(c(gencode_basic=gencode_basic, merged=merged,
            all_refseq=all_refseq, lrg=lrg, opts))
    opts 
}

filterqcOpts <- function(version, ..., check_ref=logical(1), 
                         coding_only=logical(1),
                         chr=character(), no_intergenic=logical(1),
                         pick=logical(1), pick_allele=logical(1), 
                         flag_pick=logical(1), flag_pick_allele=logical(1),
                         per_gene=logical(1), pick_order=numeric(),
                         most_severe=logical(1), summary=logical(1), 
                         filter_common=logical(1),
                         check_frequency=logical(1), freq_pop=character(),
                         freq_freq=logical(1), freq_gt_lt=character(),
                         freq_filter=character(), filter=character(),
                         allow_non_variant=logical(1),
                         check_existing=logical(1), check_alleles=logical(1),
                         check_svs=logical(1), gmaf=logical(1),
                         maf_1kg=logical(1), individual=character(),
                         phased=logical(1), failed=logical(1))
{
    if (any(version == 67)) {
        opts <- list(check_ref=check_ref, coding_only=coding_only,
                     check_existing=check_existing, check_alleles=check_alleles,
                     check_svs=check_svs, gmaf=gmaf, maf_1kg=maf_1kg,
                     individual=individual, phased=phased, chr=chr,
                     no_intergenic=no_intergenic, filter_common=filter_common,
                     check_frequency=check_frequency, freq_pop=freq_pop,
                     freq_freq=freq_freq, freq_gt_lt=freq_gt_lt,
                     freq_filter=freq_filter, filter=filter, failed=failed,
                     allow_non_variant=allow_non_variant)
    } else {
        opts <- list(check_ref=check_ref, coding_only=coding_only, chr=chr,
                     no_intergenic=no_intergenic, most_severe=most_severe,
                     summary=summary, per_gene=per_gene, 
                     filter_common=filter_common,
                     check_frequency=check_frequency, freq_pop=freq_pop,
                     freq_freq=freq_freq, freq_gt_lt=freq_gt_lt, 
                     freq_filter=freq_filter, filter=filter, 
                     allow_non_variant=allow_non_variant) 
    }
    if (any(version >= 77)) { 
        opts$pick <- pick
        opts$pick_allele <- pick_allele
    }
    if (any(version == 78)) { 
        opts$flag_pick <- flag_pick
        opts$flag_pick_allele <- flag_pick_allele
        opts$pick_order <- pick_order
    }

    opts
}

### identifierOpts, colocatedVariantsOpts, and dataformatOpts not 
### supported for version 67
identifierOpts <- function(version, ..., hgvs=logical(1), 
                           protein=logical(1), symbol=logical(1), 
                           ccds=logical(1), uniprot=logical(1),
                           tsl=logical(1), canonical=logical(1), 
                           biotype=logical(1), xref_refseq=logical(1)) 
{
    if (any(version == 67))
        stop("'identifierOpts' not supported for VEP 67")
    else
        opts <- list(hgvs=hgvs, protein=protein, symbol=symbol, ccds=ccds, 
                     canonical=canonical, biotype=biotype, 
                     xref_refseq=xref_refseq)

    if (any(version >= 77))
        opts$uniprot <- uniprot
    if (any(version == 78))
        opts$tsl <- tsl 

    opts
}

colocatedVariantsOpts <- function(version, ..., check_existing=logical(1), 
                                  check_alleles=logical(1), 
                                  check_svs=logical(1), gmaf=logical(1),
                                  maf_1kg=logical(1), maf_esp=logical(1), 
                                  old_maf=logical(1), pubmed=logical(1), 
                                  failed=logical(1)) 
{
    if (any(version == 67))
        stop("'identifierOpts' not supported for VEP 67")
    else
        opts <- list(check_existing=check_existing, 
                     check_alleles=check_alleles,
                     check_svs=check_svs, gmaf=gmaf, maf_1kg=maf_1kg, 
                     maf_esp=maf_esp, pubmed=pubmed, failed=failed)

    if (any(version >= 77))
        opts$old_maf <- old_maf 

    opts
}

dataformatOpts <- function(version, ..., vcf=logical(1), json=logical(1),
                           gvf=logical(1), original=logical(1), 
                           fields=character(), convert=character())
{
    if (any(version == 67))
        stop("'identifierOpts' not supported for VEP 67")
    else
        opts <- list(vcf=vcf, gvf=gvf, original=original, fields=fields, 
                     convert=convert)

    if (any(version >= 77))
        opts$json <- json 

    opts
}
