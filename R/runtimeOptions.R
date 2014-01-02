### =========================================================================
### VEPParam runtime options 
### =========================================================================

.version_error <- function(x)
{
    if (!any(x %in% supportedVEP()))
        stop(paste0("'version' must be one of ", 
             paste(supportedVEP(), collapse=",")))
}

### basicOpts, databaseOpts and advancedOpts are the same for versions 67, 73
basicOpts <- function(version, ..., verbose=logical(1), quiet=logical(1), 
                      no_progress=logical(1), config=character(), 
                      everything=logical(1), fork=numeric())
{
    .version_error(version)
    list(verbose=verbose, quiet=quiet, no_progress=no_progress,
         config=config, everything=everything, fork=fork)
}

databaseOpts <- function(version, ..., database=TRUE, 
                         host="useastdb.ensembl.org", 
                         user=character(), password=character(), 
                         port=numeric(), genomes=logical(1), 
                         refseq=logical(1), db_version=numeric(), 
                         registry=character())
{
    .version_error(version)
    list(database=database, host=host, user=user, password=password, 
         port=port, genomes=genomes, refseq=refseq,
         db_version=db_version, registry=registry)
}

advancedOpts <- function(version, ..., no_whole_genome=logical(1), 
                         buffer_size=5000, write_cache=logical(1), 
                         build=character(), compress=character(), 
                         skip_db_check=logical(1), cache_region_size=numeric())
{
    .version_error(version)
    list(no_whole_genome=no_whole_genome, buffer_size=buffer_size,
         write_cache=write_cache, build=build, compress=compress,
         skip_db_check=skip_db_check, cache_region_size=cache_region_size)
}

### inputOpts, cacheOpts, outputOpts and filterqcOpts are
### different for versions 67, 73
inputOpts <- function(version, ..., species="homo_sapiens", format=character(), 
                      output_file=character(), force_overwrite=logical(1),
                      stats_file=character(), no_stats=logical(1),
                      stats_text=logical(1), html=logical(1))
{
    .version_error(version)
    if (version == 67) {
        list(species=species, format=format, output_file=output_file,
             force_overwrite=force_overwrite, stats_file=stats_file,
             no_stats=no_stats, html=html)
    } else {
        list(species=species, format=format, output_file=output_file, 
             force_overwrite=force_overwrite, stats_file=stats_file,
             no_stats=no_stats, stats_text=stats_text, html=html)
    }
} 

cacheOpts <- function(version, ..., cache=logical(1), dir="$HOME/.vep", 
                      dir_cache="$HOME/.vep",dir_plugins="$HOME/.vep", 
                      offline=logical(1), fasta=character())
{
    .version_error(version)
    if (version == 67) {
        list(cache=cache, dir=dir, offline=offline, fasta=fasta)
    } else {
        list(cache=cache, dir=dir, dir_cache=dir_cache, dir_plugins=dir_plugins,
             offline=offline, fasta=fasta)
    }
}

outputOpts <- function(version, ..., sift=character(), polyphen=character(), 
                       regulatory=logical(1), cell_type=character(), 
                       custom=character(), plugin=character(),
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
    .version_error(version)
    if (version == 67) {
        list(terms=terms, sift=sift, polyphen=polyphen, regulatory=regulatory,
             cell_type=cell_type, hgvs=hgvs, gene=gene, protein=protein,
             hgnc=hgnc, ccds=ccds, canonical=canonical, xref_refseq=xref_refseq,
             numbers=numbers, domains=domains, most_severe=most_severe,
             summary=summary, per_gene=per_gene, convert=convert, fields=fields,
             vcf=vcf, gvf=gvf, original=original, custom=custom, plugin=plugin)
    } else {
        list(sift=sift, polyphen=polyphen, regulatory=regulatory,
             cell_type=cell_type, custom=custom, plugin=plugin,
             individual=individual, phased=phased, allele_number=allele_number,
             total_length=total_length, numbers=numbers, domains=domains,
             no_escape=no_escape, terms=terms)
    }
}

filterqcOpts <- function(version, ..., check_ref=logical(1), 
                         coding_only=logical(1),
                         chr=character(), no_intergenic=logical(1),
                         most_severe=logical(1), summary=logical(1),
                         per_gene=logical(1), filter_common=logical(1),
                         check_frequency=logical(1), freq_pop=character(),
                         freq_freq=logical(1), freq_gt_lt=character(),
                         freq_filter=character(), filter=character(),
                         allow_non_variant=logical(1),
                         check_existing=logical(1), check_alleles=logical(1),
                         check_svs=logical(1), gmaf=logical(1),
                         maf_1kg=logical(1), individual=character(),
                         phased=logical(1), failed=logical(1))
{
    .version_error(version)
    if (version == 67) {
        list(check_ref=check_ref, coding_only=coding_only,
             check_existing=check_existing, check_alleles=check_alleles,
             check_svs=check_svs, gmaf=gmaf, maf_1kg=maf_1kg,
             individual=individual, phased=phased, chr=chr,
             no_intergenic=no_intergenic, filter_common=filter_common,
             check_frequency=check_frequency, freq_pop=freq_pop,
             freq_freq=freq_freq, freq_gt_lt=freq_gt_lt,
             freq_filter=freq_filter, filter=filter, failed=failed,
             allow_non_variant=allow_non_variant)
    } else {
        list(check_ref=check_ref, coding_only=coding_only, chr=chr,
             no_intergenic=no_intergenic, most_severe=most_severe,
             summary=summary, per_gene=per_gene, filter_common=filter_common,
             check_frequency=check_frequency, freq_pop=freq_pop,
             freq_freq=freq_freq, freq_gt_lt=freq_gt_lt, 
             freq_filter=freq_filter, filter=filter, 
             allow_non_variant=allow_non_variant) 
    }
}

### identifierOpts, colocatedVariantsOpts, and dataformatOpts are
### supported for version 73 only
identifierOpts <- function(version, ..., hgvs=logical(1), 
                           protein=logical(1), symbol=logical(1), 
                           ccds=logical(1), canonical=logical(1), 
                           biotype=logical(1), xref_refseq=logical(1)) 
{
    if (version >= currentVEP())
        list(hgvs=hgvs, protein=protein, symbol=symbol, ccds=ccds, 
             canonical=canonical, biotype=biotype, xref_refseq=xref_refseq)
    else
        stop(paste0("'identifierOpts' supported for VEP versions >= ",
             currentVEP()))
}

colocatedVariantsOpts <- function(version, ..., check_existing=logical(1), 
                                  check_alleles=logical(1), 
                                  check_svs=logical(1), gmaf=logical(1),
                                  maf_1kg=logical(1), maf_esp=logical(1), 
                                  pubmed=logical(1), failed=logical(1)) 
{
    if (version >= currentVEP())
        list(check_existing=check_existing, check_alleles=check_alleles,
             check_svs=check_svs, gmaf=gmaf, maf_1kg=maf_1kg, maf_esp=maf_esp, 
             pubmed=pubmed, failed=failed)
    else
        stop(paste0("'colocatedVariantsOpts' supported for VEP versions >= ",
             currentVEP()))
}

dataformatOpts <- function(version, ..., vcf=logical(1), gvf=logical(1), 
                           original=logical(1), fields=character(), 
                           convert=character())
{
    if (version >= currentVEP())
        list(vcf=vcf, gvf=gvf, original=original, fields=fields, convert=convert)
    else
        stop(paste0("'dataformatOpts' supported for VEP versions >= ",
             currentVEP()))
}
