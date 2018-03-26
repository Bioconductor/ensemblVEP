\name{oldRuntimeOptions}

\alias{oldRuntimeOptions}

\title{VEPParam runtime options}

\description{
  Runtime options for the most current API version of the Ensembl Variant
  Effect Predictor.
}

\details{
  \code{VEPParam} objects store the runtime options for querying the Ensembl
  Variant Effect Predictor (VEP). This page describes only the most current
  runtime options and is a condensed version of what is listed on the
  Ensembl web site:

  \url{http://uswest.ensembl.org/info/docs/tools/vep/script/vep_options.html}

  Runtime options for archived versions can be found on the corresponding
  archive page.

  \url{http://useast.ensembl.org/info/website/archives/index.html}
}

\section{Runtime options: }{
  Data in the \code{VEPParam} are organized into the following categories,
  \sQuote{basic}, \sQuote{input}, \sQuote{cache}, \sQuote{output},
  \sQuote{identifier}, \sQuote{colocatedVariants}, \sQuote{dataformat},
  \sQuote{filterqc}, \sQuote{database} and \sQuote{advanced}. Each category
  is a \code{list} of runtime options. \code{logical} options are turned
  on/off with TRUE/FALSE. \code{character} and \code{numeric} are
  \sQuote{on} when a character string is provided and \sQuote{off} when
  they contain an empty value (i.e., \code{character()} or \code{numeric()}.

  \sQuote{identifier}, \sQuote{colocatedVariants}, \sQuote{dataformat}
  are supported for VEPParam73 and later.
  \describe{
    \item{\code{basic}}{\code{list} of the following options:
      \itemize{
	\item{verbose:} \code{logical}, default FALSE; output status messages
	\item{quiet:} \code{logical}, default FALSE; suppress status/warnings
	\item{no_progress:} \code{logical}, default FALSE; don't show progress
	  bars
	\item{config:} \code{character}, default \code{character()}; name of
	  config file
	\item{everything:} \code{logical}, default FALSE; shortcut to switch
	  on 12 options (sift, polyphen, ccds, hgvs, hgnc, numbers, domains,
	  regulatory, cell_type, canonical, protein and gmaf).
	\item{fork:} \code{numeric}, default \code{numeric()}; enable forking
      }
    }
    \item{\code{input}}{\code{list} of the the following options:
      \itemize{
	\item{species:} \code{character}, default 'homo_sapiens';
	  species for the data
	\item{assembly:} \code{character}, default \code{character()};
	  select assembly version if more than one available
	\item{format:} \code{character}, default \code{character()};
	  one of the following input file formats, 'ensembl', 'vcf',
	  'pileup', 'hgvs', 'id' or 'vep'. By default the script
	  auto-detects the input file format.
	\item{output_file:} \code{character}, default writes to temp file;
	  path and file name of output file
	\item{force_overwrite:} \code{logical}, default FALSE; overwrite
	  the output file if it currently exists
	\item{stats_file:} \code{character}, default \code{character()};
	  summary stats file name
	\item{no_stats:} \code{logical}, default FALSE; do not generate
	  a stats file
	\item{stats_text:} \code{logical}, default FALSE; generate a plain
	  text stats file instead of html
	\item{html:} \code{logical}, default FALSE; generate html version
	  of the output file
      }
    }
    \item{\code{cache}}{\code{list} of the following options:
      \itemize{
	\item{cache:} \code{logical}, default FALSE; enable use of cache
	\item{dir:} \code{character}, default '$HOME/.vep/'; cache/plugin
	  to be used
	\item{dir_cache:} \code{character}, default '$HOME/.vep/'; cache
	  to be used
	\item{dir_plugins:} \code{character}, default '$HOME/.vep/'; plugin
	  to be used
	\item{offline:} \code{logical}, default FALSE; enable offline mode,
	  no database connections will be made
	\item{fasta:} \code{character}, default \code{character()}; FASTA
	  filename or directory to files to use for reference sequences
	\item{cache_version:} \code{character}, default \code{character()};
	  use a different cache version than the assumed default
	\item{show_cache_info:} \code{logical}, default FALSE;
	  show source version information for selected cache and quit
      }
    }
    \item{\code{output}}{\code{list} of the following options:
      \itemize{
	\item{variant_class:} \code{logical}, default FALSE;
	  output the sequence ontology variant class
	\item{sift:} \code{character}, default \code{character()};
	  output prediction, score
	  or both, valid strings are 'p', 's' or 'b'
	\item{polyphen:} \code{character}, default \code{character()};
	  output prediction,
	  score or both, valid strings are 'p', 's' or 'b'
	\item{humdiv:} \code{logical}, default FALSE;
	  retrieve the humDiv PolyPhen prediction instead of humVar
	\item{gene_phenotype:} \code{logical}, default FALSE;
	  indicates if overlapped gene is associated with a phenotype, disease
	  or trait
	\item{regulatory:} \code{logical}, default FALSE; identify overlaps
	  with regulatory regions
	\item{cell_type:} \code{character}, default \code{character()};
	  only report
	  regulatory regions found in the given cell type(s)
	\item{custom:} \code{character}, default \code{character()}; name of
	  custom annotation file to add to output. Currently only a single
	  annotation is supported.
	\item{plugin:} \code{character}, default \code{character()}; name of
	  plugin module. Currently only a single module is supported.
	\item{individual:} \code{character}, default \code{character()};
	  consider only alternate alleles present in the genotypes of
	  'all' or a character vector of specified individuals
	\item{phased:} \code{logical}, default FALSE; force VCF genotypes
	  to be interpreted as phased
	\item{allele_number:} \code{logical}, default FALSE; identify allele
	  number from VCF input (1=first ALT, 2=second ALT, etc.)
	\item{total_length:} \code{character}, default \code{character()};
	  cDNA, CDS and protein positions as position/length
	\item{numbers:} \code{logical}, default FALSE; output affectd exon and
	  intron numbering, format is Number/Total
	\item{domains:} \code{logical}, default FALSE; output names of
	  overlapping protein domains
	\item{no_escape:} \code{logical}, default FALSE;
	  don't URI escape HGVS string
	\item{keep_csq:} \code{logical}, default FALSE;
	  don't overwrite existing CSQ entry in VCF INFO field
	\item{vcf_info_field:} \code{character}, default CSQ;
	  change the name of the INFO key that VEP writes the consequences to
	  in the VCF output.
	\item{terms:} \code{character}, default 'so'; type
	  of consequence terms to output, valid strings are 'ensembl' or 'so'
      }
    }
    \item{\code{identifiers}}{\code{list} of the following options:
      \itemize{
	\item{hgvs:} \code{logical}, default FALSE; add hgvs ID's
	\item{shift_hgvs:} \code{[0/1]}, default 1 (shift);
	  enable or disable 3' shifting of HGVS notations
	\item{protein:} \code{logical}, default FALSE; add Ensembl protein ID's
	\item{symbol:} \code{logical}, default FALSE; add gene symbol
	  (e.g. HGNC) (where available) to the output
	\item{ccds:} \code{logical}, default FALSE; add CCDS transcript ID's
	\item{uniprot:} \code{logical}, default FALSE;
	  adds identifiers for translated protein products from three
	  UniProt-related databases
	\item{tsl:} \code{logical}, default FALSE;
	  adds the transcript support level for this transcript
	\item{canonical:} \code{logical}, default FALSE;
	  indicate if transcript is cononical transcript for the gene
	\item{biotype:} \code{logical}, default FALSE; add biotype of
	  transcript
	\item{xref_seq:} \code{logical}, default FALSE; output aligned refseq
	  mRNA ID
      }
    }
    \item{\code{colocatedVariants}}{\code{list} of the following options:
      \itemize{
	\item{check_existing:} \code{logical}, default FALSE; check for
	  co-located variants
	\item{check_alleles:} \code{logical}, default FALSE; when checking for
	  co-located variants only report them if none of the alleles
	  supplied are novel
	\item{check_svs:} \code{logical}, default FALSE; check for
	  structural variants that overlap the input variants
	\item{gmaf:} \code{logical}, default FALSE; add global minor allele
	  frequence (MAF) from 1000 Genomes Phase 1 data
	\item{maf_1kg:} \code{logical}, default FALSE; add MAF from
	  continental populations of 1000 Genomes Phase 1 data;
	  must be use with --cache
	\item{maf_esp:} \code{logical}, default FALSE; add MAF from
	  NHLBI-ESP populations; must be used with --cache
	\item{old_maf:} \code{logical}, default FALSE;
	  for maf_1kg and maf_esp report only the frequency (no allele) and
	  convert so it is always a minor frequency, i.e. < 0.5
	\item{pubmed:} \code{logical}, default FALSE;
	   report Pubmed IDs for publications that cite existing variant;
	   must be used with --cache
	\item{failed:} \code{logical}, default FALSE; when checking for
	  co-located variants include or exclude variants that have been
	  flagged as failed
      }
    }
    \item{\code{dataformat}}{\code{list} of the following options:
      \itemize{
	\item{vcf:} \code{logical}, default FALSE; write output in vcf format
	\item{json:} \code{logical}, default FALSE; write output in json format
	\item{gvf:} \code{logical}, default FALSE; write output in gcf format
	\item{fields:} \code{character}, default fields are
	  'Uploaded_variation', 'Location', 'Allele', 'Gene', 'Feature',
	  'Feature_type', 'Consequence', 'cDNA_position', 'CDS_position',
	  'Protein_position', 'Amino_acids', 'Codons' and 'Extra'. See
	  \url{http://www.ensembl.org/info/docs/variation/vep/vep_formats.html#sv}
	  for details.
	\item{convert:} \code{character}, default \code{character()};
	  converts input file to one of 'ensembl', 'vcf', or 'pileup'
	\item{minimal:} \code{logical}, default FALSE; convert alleles to
	  their most minimal representation before consequence calculation
      }
    }
    \item{\code{filterqc}}{\code{list} of the following options:
      \itemize{
	\item{check_ref:} \code{logical}, default FALSE; force check of
	  supplied reference allele against the sequence stored in Ensembl
	  Core database
	\item{coding_only:} \code{logical}, default FALSE; return
	  consequences in coding regions only
	\item{chr:} \code{character}, default \code{character()}; select
	  a subset of chromosomes to be analyzed
	\item{no_intergenic:} \code{logical}, default FALSE; do not
	  include intergenic consequences
	\item{pick:} \code{logical}, default FALSE;
	  pick once line of consequence data per variant
	\item{pick_allele:} \code{logical}, default FALSE;
	  pick once line of consequence data per variant allele
	\item{flag_pick:} \code{logical}, default FALSE;
	  as per --pick, but adds the PICK flag to the chosen block of
	  consequence data and retains others.
	\item{flag_pick_allele:} \code{logical}, default FALSE;
	  as per --pick_allele, but adds the PICK flag to the chosen block
	  of consequence data and retains others.
	\item{per_gene:} \code{logical}, default FALSE;
	  output only the most severe consequence per gene
	\item{pick_order:} \code{character}, See ensembl web page for
	  default order; customise the order of criteria applied when
	  choosing a block of annotation data with e.g. --pick.
	\item{most_severe:} \code{logical}, default FALSE; output only most
	  severe consequence per variation
	\item{summary:} \code{logical}, default FALSE; output a comma-separated
	  list of all observed consequences per variation, transcript-specific
	  columns will be left blank
	\item{filter_common:} \code{logical}, default FALSE; shortcut flag
	  to turn on filters, See web page for details.
	\item{check_frequency:} \code{logical}, default FALSE; turn on
	  frequency filtering, must also specify all of the
	  --freq\_* flags. See web page for details.
	\item{freq_pop:} \code{character}, default \code{character()};
	  population to use in frequency filter
	\item{freq_freq:} \code{numeric}, default \code{numeric()};
	  MAF to use in frequency filter
	\item{freq_gt_lt:} \code{character}, default \code{character()};
	  specify whether the frequency of the co-located variant must
	  be greater than or less than the value specified. Values
	  are 'gt' or 'lt'.
	  in the \code{freq_freq} option.
	\item{freq_filter:} \code{character}, default \code{character()};
	  specify whether to exclude or include variants that pass
	  the frequency filter. Values are 'exclude' or 'include'.
	\item{allow_non_variant:} \code{logical}, default FALSE; when using
	  VCF format as input and output, by default VEP will skip all
	  non-variant lines of input (i.e., where the ALT is NULL). When
	  this option is enabled, lines will be printed in the VCF output
	  with no consequence data added.
      }
    }
    \item{\code{database}}{\code{list} of the following options:
      \itemize{
	\item{database:} \code{logical}, default TRUE; enable the VEP to
	  use local or remote databases
	\item{host:} \code{character}, default  \code{character()};
	database host. This will use the default as defined by vep
	'ensembldb.ensembl.org'. Users in the US may find connection
	and transfer speeds quicker using the East coast mirror,
	'useastdb.ensembl.org'.
	\item{user:} \code{character} default \code{character()};
	  database user
	\item{password:} \code{character}, default \code{character()};
	  database password
	\item{port:} \code{numeric}, default \code{character()};
	  database port
	\item{genomes:} \code{logical}, default FALSE; override default
	  connection settings with those for the Ensembl Genomces public
	  MySQL server
	\item{gencode_basic:} \code{logical}, default FALSE;
	  limit analysis to transcripts in GENCODE basic set
	\item{refseq:} \code{logical}, default FALSE; use otherfeatures
	  database to retrieve transcripts
	\item{merged:} \code{logical}, default FALSE;
	  use the merged Ensembl and RefSeq cache
	\item{all_refseq:} \code{logical}, default FALSE;
	  include e.g. CCDS and Ensembl EST transcripts
	\item{lrg:} \code{logical}, default FALSE;
	  map input variants to LRG coordinates
	\item{db_version:} \code{numeric}, default \code{character()};
	  force connection to specific version
	\item{registry:} \code{character}, default \code{character()};
	  provide file to override default connection settings
      }
    }
    \item{\code{advanced}}{\code{list} of the following options:
      \itemize{
	\item{no_whole_genome:} \code{logical}, default FALSE; run in
	  non-whole genome mode, variants analyzed one at a time, no caching
	\item{buffer_size:} \code{numeric}, default 5000; internal buffer
	  size corresponding to number of variations read into memory
	  simultaneously
	\item{write_cache:} \code{logical}, default FALSE; enable writing
	  to the cache
	\item{build:} \code{character}, default \code{character()}; build
	  cache for the selected species from the database (See --chr flag)
	\item{compress:} \code{character}, default \code{character()};
	  specify utility to decompress cached files (zcat is default)
	\item{skip_db_check:} \code{logical}, default FALSE; force the script
	  to use a cache built from a different host than specified with
	  --host
	\item{cache_region_size:} \code{numeric}, default \code{numeric()};
	  size in base-pairs of the region covered by one file in the cache,
	  see full description of this flag on the web site for details
      }
    }
  }
}

\author{Valerie Obenchain}

\seealso{
  \itemize{
    \item The \code{ensemblVEP} function man page.
    \item The \code{VEPParam} class man page.
  }
}

\examples{
  ## See ?VEPParam for examples of constructing instances of a
  ## VEPParam object with different runtime options.
}

\keyword{utilities}
