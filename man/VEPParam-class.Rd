\name{VEPParam-class}
\docType{class}

% Classes:
\alias{class:VEPParam}
\alias{VEPParam-class}
\alias{class:VEPParam67}
\alias{VEPParam67-class}
\alias{class:VEPParam73}
\alias{VEPParam73-class}

% Constructors:
\alias{VEPParam}

% Helpers:
\alias{basicOpts}
\alias{inputOpts}
\alias{cacheOpts}
\alias{outputOpts}
\alias{identifierOpts}
\alias{colocatedVariantsOpts}
\alias{dataformatOpts}
\alias{filterqcOpts}
\alias{databaseOpts}
\alias{advancedOpts}

% Accessors:
\alias{basic}
\alias{basic<-}
\alias{basic,VEPParam-method}
\alias{basic<-,VEPParam-method}
\alias{input}
\alias{input<-}
\alias{input,VEPParam-method}
\alias{input<-,VEPParam-method}
\alias{cache}
\alias{cache<-}
\alias{cache,VEPParam-method}
\alias{cache<-,VEPParam-method}
\alias{output}
\alias{output<-}
\alias{output,VEPParam-method}
\alias{output<-,VEPParam-method}
\alias{filterqc}
\alias{filterqc<-}
\alias{filterqc,VEPParam-method}
\alias{filterqc<-,VEPParam-method}
\alias{database}
\alias{database<-}
\alias{database,VEPParam-method}
\alias{database<-,VEPParam-method}
\alias{advanced}
\alias{advanced<-}
\alias{advanced,VEPParam-method}
\alias{advanced<-,VEPParam-method}
\alias{version}
\alias{version<-}
\alias{version,VEPParam-method}
\alias{version<-,VEPParam-method}

\alias{identifier}
\alias{identifier<-}
\alias{identifier,VEPParam73-method}
\alias{identifier<-,VEPParam73-method}
\alias{dataformat}
\alias{dataformat<-}
\alias{dataformat,VEPParam73-method}
\alias{dataformat<-,VEPParam73-method}
\alias{colocatedVariants}
\alias{colocatedVariants<-}
\alias{colocatedVariants,VEPParam73-method}
\alias{colocatedVariants<-,VEPParam73-method}

% helpers:
\alias{currentVEP}
\alias{supportedVEP}

% "show" method:
\alias{show,VEPParam-method}

\title{VEPParam objects}

\description{
  \code{VEPParam} is a VIRTUAL class with concrete subclasses 
  \code{VEPParam67} and \code{VEPParam73}. These objects store 
  runtime options for the Ensembl Variant Effect Predictor.
}

\details{
  The \code{VEPParam} family of objects stores runtime options for the 
  \code{ensemblVEP} function which queries the Ensembl Variant Effect 
  Predictor (VEP). Brief descriptions of the runtime options are provided 
  below. For complete details, see the Ensembl VEP web page.

  \url{http://www.ensembl.org/info/docs/variation/vep/vep_script.html#running}

  The \code{VEPParam} class supports multiple versions of the Ensembl VEP
  API. The helper function \code{supportedVEP()} returns all versions
  supported and \code{currentVEP()} returns the most current. The
  most current version is used as the default in the \code{VEPParam}
  constructor and is the only version that performs a live query against
  Ensembl VEP web site. Previous versions use cached data.
 
  FIXME: Update with runtime options. 
}

\section{Runtime options}{
  Data in the \code{VEPParam} are organized into the following categories, 
  \sQuote{basic}, \sQuote{input}, \sQuote{cache}, \sQuote{output}, 
  \sQuote{identifier}, \sQuote{colocatedVariants}, \sQuote{dataformat},
  \sQuote{filterqc}, \sQuote{database} and \sQuote{advanced}. Each category
  is a \code{list} of runtime options. \code{logical} options are turned on/off 
  with TRUE and FALSE. \code{character} and \code{numeric} are \sQuote{on} when 
  a character string is provided and \sQuote{off} when they contain an empty value 
  (i.e., \code{character()} or \code{numeric()}.

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
      }
    }
    \item{\code{output}}{\code{list} of the following options:
      \itemize{
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
        \item{terms:} \code{character}, default 'so'; type 
          of consequence terms to output, valid strings are 'ensembl' or 'so'
      }
    }
    \item{\code{identifiers}}{\code{list} of the following options:
      \itemize{
        \item{hgvs:} \code{logical}, default FALSE; add hgvs ID's
        \item{protein:} \code{logical}, default FALSE; add Ensembl protein ID's
        \item{symbol:} \code{logical}, default FALSE; add gene symbol
          (e.g. HGNC) (where available) to the output
        \item{ccds:} \code{logical}, default FALSE; add CCDS transcript ID's
        \item{canonical:} \code{logical}, default FALSE; indicate if transcript 
          is cononical transcript for the gene
        \item{biotype:} \code{logical}, default FALSE; add biotype of transcript
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
        \item{gvf:} \code{logical}, default FALSE; write output in gcf format
        \item{original:} \code{logical}, default FALSE; writes output as 
          filtered set of input. Must be used with --filter.
        \item{fields:} \code{character}, default fields are
          'Uploaded_variation', 'Location', 'Allele', 'Gene', 'Feature', 
          'Feature_type', 'Consequence', 'cDNA_position', 'CDS_position', 
          'Protein_position', 'Amino_acids', 'Codons' and 'Extra'. See
          \url{http://www.ensembl.org/info/docs/variation/vep/vep_formats.html#sv}
          for details. 
        \item{convert:} \code{character}, default \code{character()}; 
          converts input file to one of 'ensembl', 'vcf', or 'pileup'
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
        \item{most_severe:} \code{logical}, default FALSE; output only most 
          severe consequence per variation
        \item{summary:} \code{logical}, default FALSE; output a comma-separated
          list of all observed consequences per variation, transcript-specific
          columns will be left blank
        \item{per_gene:} \code{logical}, default FALSE; output only the most
          severe consequence per gene
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
        \item{filter:} \code{character}, default \code{character()}; 
          filter the output on consequence type. See web page for details.
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
      \code{VEPParam(version=73, ..., basic=basicOpts(), input=inputOpts(),
                     cache=cacheOpts(), output=outputOpts(),
                     identifier=identifierOpts(), 
                     colocatedVariants=colocatedVariantsOpts(),
                     dataformat=dataformatOpts(), filterqc=filterqcOpts(), 
                     database=databaseOpts(), advanced=advancedOpts(), ...)}
      Creates a \code{VEPParam} object.
      \describe{
        \item{\code{version}}{
          Numeric specifying the version of the Ensembl API. Live web
          queries are available for the most current version only
          (\code{currentVEP()}). Running a previous version requires
          a local cache of the corresponding version of the data.}
        \item{\code{basic}}{
          \code{list} of basic options}
        \item{\code{input}}{
          \code{list} of input options}
        \item{\code{cache}}{
          \code{list} of cache options}
        \item{\code{output}}{
          \code{list} of output options}
        \item{\code{filterqc}}{
          \code{list} of filterqc options}
        \item{\code{database}}{
          \code{list} of database options}
        \item{\code{advanced}}{
          \code{list} of advanced options}
      }
      Supported for VEPParam73 and later:
      \describe{
        \item{\code{identifier}}{
          \code{list} of identifier options}
        \item{\code{colocatedVariants}}{
          \code{list} of colocatedVariants options}
        \item{\code{dataformat}}{
          \code{list} of dataformat options}
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
    \item{}{\code{version(x)}, \code{version(x) <- value}}
  }
  Supported for VEPParam73 and later:
  \describe{
    \item{}{\code{identifier(x)}, \code{identifier(x) <- value}}
    \item{}{\code{colocatedVariants(x)}, \code{colocatedVariants(x) <- value}}
    \item{}{\code{dataformat(x)}, \code{dataformat(x) <- value}}
  }
}

\section{Helper functions}{
  \describe{
    \item{}{
      \code{currentVEP()}:
      Invoked with no arguments.
      Return the most current version of the Ensembl VEP API
      supported by the \code{ensemblVEP} R package. This version
      is used as the default in the \code{VEPParam} constructor.
    }
    \item{}{
      \code{supportedVEP()}:
      Invoked with no arguments.
      Return all versions of the Ensembl VEP API supported
      by the \code{ensemblVEP} R package. 
    }
  }
  The following functions create a \code{list} of runtime options and are 
  used in the \code{VEPParam} constructor.
  \describe{
    \item{}{\code{basicOpts(version, ...)}}
    \item{}{\code{inputOpts(version, ...)}}
    \item{}{\code{cacheOpts(version, ...)}}
    \item{}{\code{outputOpts(version, ...)}}
    \item{}{\code{filterqcOpts(version, ...)}}
    \item{}{\code{databaseOpts(version, ...)}}
    \item{}{\code{advancedOpts(version, ...)}}
  }
  Supported for VEPParam73 and later:
  \describe{
    \item{}{\code{identifierOpts(version, ...)}}
    \item{}{\code{colocatedVariantsOpts(version, ...)}}
    \item{}{\code{dataformatOpts(version, ...)}}
  }
}

\author{Valerie Obenchain <vobencha@fhcrc.org>}

\seealso{
  \link{ensemblVEP}
}

\examples{
  ## The default constructor supports the most current version
  ## of the Ensembl API.
  param <- VEPParam()
  class(param)
  version(param)

  ## Display what API versions are currently supported.
  supportedVEP()

  ## Create a version '67' VEPParam.
  VEPParam(67) 

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
  param <- VEPParam(input=c(output_file="path/myfile.vcf"),
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
