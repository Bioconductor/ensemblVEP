\name{ensemblVEP}

\alias{ensemblVEP}
\alias{ensemblVEP,character-method}

\title{
  Query Ensembl Variant Effect Predictor
}

\description{
  Retrieve variant annotation data from the Ensembl Variant Effect
  Predictor (VEP).
}

\usage{
\S4method{ensemblVEP}{character}(file, param=VEPFlags(), ..., verbose=FALSE)
}

\arguments{
  \item{file}{A  \code{character} specifying the full path to the
    file, including the file name.

    Valid input file types are described on the Ensembl VEP web page.
    \url{http://www.ensembl.org/info/docs/variation/vep/vep_script.html#running}
  }
  \item{param}{An instance of \code{VEPFlags} specifying runtime options.
  }
  \item{\dots}{Additional arguments passed to methods.
  }
  \item{verbose}{logical(1) should system call to vep be printed}
}

\details{
  \describe{
    \item{}{
      The Ensembl VEP tool is described in detail on the home page
      (link in `see also` section). The \code{ensemblVEP} function
      wraps the perl API and requires a local install of the Ensembl
      VEP available in the user's path. The \code{VEPFlags} class
      provides a way to specify runtime options. Results are returned
      from Ensembl VEP as \code{GRanges} (default) or \code{VCF} objects.
      Alternatively, results can be written directly to a file.
      Note: if using vep version < 90 the param is an instance of
      \code{VEPParam} instead of \code{VEPFlags}
    }
  }
}

\value{
  Default behavior returns a \code{GRanges} object. Options can be
  set to return a \link{VCF} object or write a file to disk.
}

\author{
  Valerie Obenchain and Lori Shepherd
}

\seealso{
  \link{VEPFlags-class}
}

\references{
  Ensembl VEP Home:
  \url{http://www.ensembl.org/info/docs/tools/vep/index.html}

  Human Genome Variation Society (hgvs):
  \url{http://www.hgvs.org/mutnomen/}
}

\examples{
  ## -----------------------------------------------------------------------
  ## Results returned as GRanges or VCF objects
  ## -----------------------------------------------------------------------
  ## The default behavior returns a GRanges with the consequence
  ## data as metadata columns.
  file <- system.file("extdata", "ex2.vcf", package="VariantAnnotation")
  \dontrun{
  gr <- ensemblVEP(file)
  gr[1:3]
  }
  ## When the 'vcf' option is TRUE, a VCF object is returned.
  myparam <- VEPFlags(flags=list(vcf=TRUE))
  vcf <- ensemblVEP(file, param=myparam)
  vcf

  ## The consequence data are returned as the 'CSQ' column in info.
  info(vcf)$CSQ

  ## To parse this column use parseCSQToGRanges().
  csq <- parseCSQToGRanges(vcf)
  head(csq, 4)

  ## The columns returned are controlled by the 'fields' option.
  ## By default all fields are returned. See ?VEPFlags for details.

  ## When comparing ensemblVEP() results to the data in the
  ## input vcf we see variant 20:1230237 was not returned.
  vcf_input <- readVcf(file, "hg19")
  rowRanges(vcf_input)
  rowRanges(vcf)

  ## This variant has no alternate allele and is called a
  ## monomorphic reference. The Ensembl VEP automatically
  ## drops these variants.
  rowRanges(vcf)[,c("REF", "ALT")]

  ## -----------------------------------------------------------------------
  ## Results written to disk
  ## -----------------------------------------------------------------------
  ## Write a file to disk by providing a path and file name as 'output_file'.
  ## Different output file formats are specified using the 'dataformat'
  ## runtime options.

  ## Write a vcf file to myfile.vcf:
  myparam <- VEPFlags(flags=list(vcf=TRUE,
		      output_file="/path/myfile.vcf"))
  ## Write a gvf file to myfile.gvf:
  myparam <- VEPFlags(flags=list(gvf=TRUE,
		      output_file="/path/myfile.gvf"))

  ## -----------------------------------------------------------------------
  ## Runtime options
  ## -----------------------------------------------------------------------
  ## All runtime options are controlled by specifying a VEPFlags.
  ## See ?VEPFlags for complete details.
  param <- VEPFlags()

  ## Logical options are turned on/off with TRUE/FALSE. By
  ## default, 'quiet' is FALSE.
  ## Setting 'quiet' to TRUE will suppress all status and warnings.
  flags(param)$quiet <- TRUE

  ## Characater options are turned on/off by specifying a character
  ## value or an empty character (i.e., character()). By default no
  ## 'sift' results are returned.
  ## Setting 'sift' to 'b' will return both predictions and scores.
  flags(param)$sift <- 'b'

}

\keyword{methods}
