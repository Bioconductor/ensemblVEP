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
\alias{databaseOpts}
\alias{outputOpts}
\alias{filterqcOpts}
\alias{cacheOpts}

% Accessors:
\alias{basic}
\alias{input}
\alias{database}
\alias{output}
\alias{filterqc}
\alias{cache}

\title{VEPParam objects}

\description{
  A \code{VEPParam} object is a container for storing runtime options for the
  the Ensembl Variant Effect Predictor.
}

\section{Constructor}{
  \describe{
    \item{}{
      \code{VEPParam(basic=basicOpts(), input=inputOpts(),
                     database=databaseOpts(), output=outputOpts(),
                     filterqc=filterqcOpts(), cache=cacheOpts(), ...)}
      Creates a \code{VEPParam} object.
    }
  }
}

\details{
  The \code{VEPParam} class stores the runtime options for the Ensembl Variant
  Effect Predictor (VEP) as described on their web page :

  \url{http://www.ensembl.org/info/docs/variation/vep/vep_script.html#running}

  Data are organized into six slots:
  \describe{
    \item{\code{basic}}{Contains the basic options as a \code{character} 
      vector. Fields include verbose, quiet, no_progress, config,
      everything and fork.
    }
    \item{\code{input}}{Contains the input options as a \code{character}
      vector. Fields include species, input_file, format, output_file and
      force_overwrite.
    }
    \item{\code{database}}{Contains the database options as a \code{list}. 
      Fields include host, user, password, port, genomes, refseq, db_version 
      and registry.
    }
    \item{\code{output}}{Contains the output options as a \code{character}
      vector. Fields include terms, sift, polyphen, regulatory, cell_type,
      hgvs, gene, protein, hgnc, ccds, canonical, xref_refseq, numbers,
      domains, most_severe, summary, per_gene, convert, fields, vcf,
      gvf, original, custom and plugin.
    }
    \item{\code{filterqc}}{Contains the filter and QC options as a 
      \code{character} vector. Fields include check_ref, coding_only,
      check_alleles, check_svn, gmaf, individual, phased, chr, no_intergenic,
      check_frequency, freq_pop, freq_freq, freq_gt_lt, freq_filter, filter,
      failed and allow_non_variant. 
    }
    \item{\code{cache}}{Contains the cache options as a \code{character}
      vector. Fields include no_whole_genome, cache, dir, offline,
      buffer_size, write_cache, build, compress, skip_db_check,
      cache_region_size and fasta. 
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
    \item{}{\code{database(x)}, \code{database(x) <- value}}
    \item{}{\code{output(x)}, \code{output(x) <- value}}
    \item{}{\code{filterqc(x)}, \code{filterqc(x) <- value}}
    \item{}{\code{cache(x)}, \code{cache(x) <- value}}
  }
}

\section{Helper functions}{
  These functions create the runtime options and are used in the 
  \code{VEPParam} constructor.
  \describe{
    \item{}{
      \code{basicOpts(verbose=logical(), quiet=logical(), 
                      no_progress=logical(), config=logical(), 
                      everything=logical(), fork=logical())}
    }
    \item{}{
      \code{inputOpts(species="homo_sapiens", input_file=character(), 
                      format="vcf", output_file=paste0(tempdir(), "/temp.vcf", 
                      force_overwrite=logical())}
    }
    \item{}{
      \code{databaseOpts(host="useastdb.ensembl.org", user=character(), 
                         password=character(), port=character(), 
                         genomes=character(), refseq=character(),
                         db_version=character(), registry=character())}
    }
    \item{}{
      \code{outputOpts(terms=character(), sift=logical(), 
                       polyphen=logical(), regulatory=logical(), 
                       cell_type=list(), hgvs=logical(), gene=logical(),
                       protein=logical(), hgnc=logical(), ccds=logical(),
                       canonical=logical(), xref_refseq=logical(), 
                       numbers=logical(), domains=logical(), 
                       most_severe=logical(), summary=logical(), 
                       per_gene=logical(), convert=character(),
                       fields=list(), vcf=logical(), gvf=logical(), 
                       original=logical(), custom=character(),
                       plugin=character())}
    }
  }
}

\author{Valerie Obenchain <vobencha@fhcrc.org>}

\seealso{
  \link{ensemblVEP}
}

\examples{
  ## Note that a default VEPParam has
  ## fields set in the 'input', 'database' and
  ## 'output' slots.
  param <- VEPParam()
  param

  ## Use the accessors to see all available fields and current values.
  basic(param)
  input(param)

  ## The 'everything' flag is not currently set in the basic options.
  basic(param)$everything

  ## Switch on the flag and confirm the value change.
  basic(param) <- c("everything"=TRUE)
  basic(param)$everything

  ## The change
  param

  ## Write the output to myfile.vcf instead of returning a VCF object.
  ## Return the sift and polyphen predictions and scores. 
  VEPParam(input=inputOpts(output_file="path/myfile.vcf"),
           output=outputOpts(sift="b", polyphen="b"))
}

\keyword{methods}
\keyword{classes}
