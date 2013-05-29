\name{VEPParam-class}
\docType{class}

% Classes:
\alias{class:VEPParam}
\alias{VEPParam-class}

% Constructors:
\alias{VEPParam}

% Helpers:
\alias{basicOpts}
\alias{inputOpts}
\alias{cacheOpts}
\alias{outputOpts}
\alias{filterqcOpts}
\alias{databaseOpts}
\alias{advancedOpts}

% Accessors:
\alias{basic}
\alias{basic<-}
\alias{input}
\alias{input<-}
\alias{cache}
\alias{cache<-}
\alias{output}
\alias{output<-}
\alias{filterqc}
\alias{filterqc<-}
\alias{database}
\alias{database<-}
\alias{advanced}
\alias{advanced<-}

% "show" method:
\alias{show,VEPParam-method}

\title{VEPParam objects}

\description{
  A \code{VEPParam} object is a container for storing runtime options for the
  the Ensembl Variant Effect Predictor.
}

\details{
  The \code{VEPParam} class stores runtime options for the \code{ensemblVEP}
  function which queries the Ensembl Variant Effect Predictor (VEP). Brief
  descriptions of the runtime options are provided below. For complete
  details, see the Ensembl VEP web page.

  \url{http://www.ensembl.org/info/docs/variation/vep/vep_script.html#running}

  Data in the \code{VEPParam} are organized into five slots, \sQuote{basic}, 
  \sQuote{input}, \sQuote{cache}, \sQuote{output}, \sQuote{filterqc},
  \sQuote{database} and \sQuote{advanced}. Each slot holds a \code{list} of 
  runtime options. \code{logical} options are turned on/off with TRUE and 
  FALSE. \code{character} and \code{numeric} are \sQuote{on} when a character 
  string is provided and \sQuote{off} when they contain an empty value (i.e., 
  \code{character()} or \code{numeric()}.
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
        \item{html:} \code{logical}, default FALSE; generate html version
          of the output file 
      }
    }
    \item{\code{cache}}{\code{list} of the following options:
      \itemize{
        \item{cache:} \code{logical}, default FALSE; enable use of cache 
        \item{dir:} \code{character}, default '$HOME/.vep/'; base cache
          to be used 
        \item{offline:} \code{logical}, default FALSE; enable offline mode,
          no database connections will be made 
        \item{fasta:} \code{character}, default \code{character()}; FASTA
          filename or directory to files to use for reference sequences 
      }
    }
    \item{\code{output}}{\code{list} of the following options:
      \itemize{
        \item{terms:} \code{character}, default 'so'; type 
          of consequence terms to output, valid strings are 'ensembl' or 'so'
        \item{sift:} \code{character}, default \code{character()}; 
          output prediction, score
          or both, valid strings are 'p', 's' or 'b'
        \item{polyphen:} \code{character}, default \code{character()}; 
          output prediction, 
          score or both, valid strings are 'p', 's' or 'b'
        \item{regulatory:} \code{logical}, default FALSE; identify overlaps 
          with regulatory regions
        \item{cell_type:} \code{character}, default \code{character()}; 
          only report 
          regulatory regions found in the given cell type(s) 
        \item{hgvs:} \code{logical}, default FALSE; add hgvs ID's
        \item{gene:} \code{logical}, default FALSE; add Ensembl gene ID's
        \item{protein:} \code{logical}, default FALSE; add Ensembl protein ID's
        \item{ccds:} \code{logical}, default FALSE; add CCDS transcript ID's
        \item{canonical:} \code{logical}, default FALSE; indicate if transcript 
          is cononical transcript for the gene
        \item{xref_seq:} \code{logical}, default FALSE; output aligned refseq 
          mRNA ID
        \item{numbers:} \code{logical}, default FALSE; output affectd exon and 
          intron numbering, format is Number/Total
        \item{domains:} \code{logical}, default FALSE; output names of 
          overlapping protein domains
        \item{most_severe:} \code{logical}, default FALSE; output only most 
          severe consequence per variation
        \item{summary:} \code{logical}, default FALSE; output a comma-separated
          list of all observed consequences per variation, transcript-specific
          columns will be left blank
        \item{per_gene:} \code{logical}, default FALSE; output only the most
          severe consequence per gene
        \item{convert:} \code{character}, default \code{character()}; 
          converts input file to one of 'ensembl', 'vcf', or 'pileup'
        \item{fields:} \code{character}, default fields are
          'Uploaded_variation', 'Location', 'Allele', 'Gene', 'Feature', 
          'Feature_type', 'Consequence', 'cDNA_position', 'CDS_position', 
          'Protein_position', 'Amino_acids', 'Codons' and 'Extra'. See
          \url{http://www.ensembl.org/info/docs/variation/vep/vep_formats.html#sv}
          for details. 
        \item{vcf:} \code{logical}, default FALSE; write output in vcf format
        \item{gvf:} \code{logical}, default FALSE; write output in gcf format
        \item{original:} \code{logical}, default FALSE; writes output as 
          filtered set of input. Must be used with --filter.
        \item{custom:} \code{character}, default \code{character()}; name of
          custom annotation file to add to output. Currently only a single
          annotation is supported.
        \item{plugin:} \code{character}, default \code{character()}; name of
          plugin module. Currently only a single module is supported.
      }
    }
    \item{\code{filterqc}}{\code{list} of the following options:
      \itemize{
        \item{check_ref:} \code{logical}, default FALSE; force check of 
          supplied reference allele against the sequence stored in Ensembl 
          Core database
        \item{coding_only:} \code{logical}, default FALSE; return 
          consequences in coding regions only
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
          continental populations of 1000 Genomes Phase 1 data  
        \item{individual:} \code{character}, default \code{character()}; 
          consider only alternate alleles present in the genotypes of 
          'all' or a character vector of specified individuals
        \item{phased:} \code{logical}, default FALSE; force VCF genotypes 
          to be interpreted as phased
        \item{chr:} \code{character}, default \code{character()}; select 
          a subset of chromosomes to be analyzed
        \item{no_intergenic:} \code{logical}, default FALSE; do not 
          include intergenic consequences
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
        \item{filter:} \code{character}, default \code{character()}; 
          filter the output on consequence type. See web page for details.
        \item{failed:} \code{logical}, default FALSE; when checking for 
          co-located variants include or exclude variants that have been 
          flagged as failed
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
        \item{host:} \code{character}, default 'useast.ensembl.db.org'; 
          database host
        \item{user:} \code{character} default \code{character()}; 
          database user
        \item{password:} \code{character}, default \code{character()}; 
          database password
        \item{port:} \code{numeric}, default \code{character()}; 
          database port
        \item{genomes:} \code{logical}, default FALSE; override default 
          connection settings with those for the Ensembl Genomces public 
          MySQL server
        \item{refseq:} \code{logical}, default FALSE; use otherfeatures 
          database to retrieve transcripts
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

\section{Constructor}{
  \describe{
    \item{}{
      \code{VEPParam(basic=basicOpts(), input=inputOpts(),
                     cache=cacheOpts(), output=outputOpts(),
                     filterqc=filterqcOpts(), database=databaseOpts(), 
                     advanced=advancedOpts(), ...)}
      Creates a \code{VEPParam} object.
      \describe{
        \item{\code{basic}}{\code{list} of basic options}
        \item{\code{input}}{\code{list} of input options}
        \item{\code{cache}}{\code{list} of cache options}
        \item{\code{output}}{\code{list} of output options}
        \item{\code{filterqc}}{\code{list} of filterqc options}
        \item{\code{database}}{\code{list} of database options}
        \item{\code{advanced}}{\code{list} of advanced options}
      }
    }
  }
}

\section{Accessors}{
  In the following code, \code{x} is a \code{VEPParam} object
  and \code{value} is a named \code{list} or \code{character}
  vector. 
  \describe{
    \item{}{\code{basic(x)}, \code{basic(x) <- value}}
    \item{}{\code{input(x)}, \code{input(x) <- value}}
    \item{}{\code{cache(x)}, \code{cache(x) <- value}}
    \item{}{\code{output(x)}, \code{output(x) <- value}}
    \item{}{\code{filterqc(x)}, \code{filterqc(x) <- value}}
    \item{}{\code{database(x)}, \code{database(x) <- value}}
    \item{}{\code{advanced(x)}, \code{advanced(x) <- value}}
  }
}

\section{Helper functions}{
  These functions create a \code{list} of runtime options and are 
  used in the \code{VEPParam} constructor.
  \describe{
    \item{}{
      \code{basicOpts <- function(verbose=logical(1), quiet=logical(1),
                no_progress=logical(1), config=character(),
                everything=logical(1), fork=numeric())}
    }
    \item{}{
      \code{inputOpts <- function(species="homo_sapiens", 
                format=character(), output_file=character(), 
                force_overwrite=logical(1), stats_file=character(), 
                no_stats=logical(1), html=logical(1))} 
    }
    \item{}{
      \code{cacheOpts <- function(cache=logical(1), dir="$HOME/.vep",
                offline=logical(1), fasta=character())}
    }
    \item{}{
      \code{outputOpts <- function(terms="SO", sift=character(), 
                polyphen=character(), regulatory=logical(1), 
                cell_type=character(), hgvs=logical(1), 
                gene=logical(1), protein=logical(1), hgnc=logical(1), 
                ccds=logical(1), canonical=logical(1), 
                xref_refseq=logical(1), numbers=logical(1), 
                domains=logical(1), most_severe=logical(1), 
                summary=logical(1), per_gene=logical(1), 
                convert=character(), fields=character(), 
                vcf=logical(1), gvf=logical(1), original=logical(1),
                custom=character(), plugin=character())}
    }
    \item{}{
      \code{filterqcOpts <- function(check_ref=logical(1), 
                coding_only=logical(1), check_existing=logical(1), 
                check_alleles=logical(1), check_svs=logical(1), 
                gmaf=logical(1), maf_1kg=logical(1), individual=character(), 
                phased=logical(1), chr=character(), 
                no_intergenic=logical(1), filter_common=logical(1),
                check_frequency=logical(1), freq_pop=character(),
                freq_freq=logical(1), freq_gt_lt=character(),
                freq_filter=character(), filter=character(),
                failed=logical(1), allow_non_variant=logical(1))}
    }
    \item{}{
      \code{databaseOpts <- function(database=TRUE, 
                host="useastdb.ensembl.org", user=character(), 
                password=character(), port=numeric(), genomes=logical(1), 
                refseq=logical(1), db_version=numeric(), 
                registry=character())}
    }
    \item{}{
      \code{advancedOpts <- function(no_whole_genome=logical(1), 
                buffer_size=5000, write_cache=logical(1), build=character(),
                compress=character(), skip_db_check=logical(1),
                cache_region_size=numeric())}
    }
  }
}

\author{Valerie Obenchain <vobencha@fhcrc.org>}

\seealso{
  \link{ensemblVEP}
}

\examples{
  ## The VEPParam has five slots. By default, no options are 
  ## set for 'basic' or 'filterqc'.
  param <- VEPParam()
  param

  ## View the values in 'basic' and 'input'.
  basic(param)
  input(param)


  ## Change the value of the 'everything' to TRUE.
  basic(param)$everything
  basic(param)$everything <- TRUE
  basic(param)$everything

  ## Replace multiple values with a named list.
  basic(param) <- list(verbose=TRUE, config="myconfig.txt")
  basic(param)

  ## Write the output to myfile.vcf instead of returning a VCF object.
  ## Return the sift and polyphen predictions only (not scores).
  VEPParam(input=c(output_file="path/myfile.vcf"),
           output=c(sift="p", polyphen="p"))

  ## 'sift' and 'polyphen' are runtime options that require
  ## a character value, (i.e., 's', 'p', or 'b').
  output(param)$sift

  ## To turn off 'sift' or 'polyphen' set the value to an
  ## empty character (i.e., character()).
  output(param)$sift <- character() 
}

\keyword{methods}
\keyword{classes}
