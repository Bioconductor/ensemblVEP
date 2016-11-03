\name{parseCSQToGRanges}

\alias{parseCSQToGRanges}
\alias{parseCSQToGRanges,character-method}
\alias{parseCSQToGRanges,VCF-method}

\title{
  Parse the CSQ column of a VCF object into a GRanges object
}

\description{
  Parse the CSQ column in a VCF object returned from the Ensembl 
  Variant Effect Predictor (VEP).
}

\usage{
\S4method{parseCSQToGRanges}{character}(x, VCFRowID=character(), 
    ..., info.key = "CSQ")
\S4method{parseCSQToGRanges}{VCF}(x, VCFRowID=character(), 
    ..., info.key = "CSQ")
}

\arguments{
  \item{x}{
    The \code{character} name of a vcf file on disk or a \code{VCF} object
  }
  \item{VCFRowID}{
    A \code{character} vector of rownames from the original VCF. 
    When provided, the result includes a metadata column named 
    \sQuote{VCFRowID} which maps the result back to the row 
    (variant) in the original VCF.

    When \code{VCFRowID} is not provided no \sQuote{VCFRowID}
    column is included. 
  }
  \item{info.key}{
    The name of the INFO key that VEP writes the consequences to in the output
    (default is \code{CSQ}). This should only be used if something other that
    \code{CSQ} was passed in the --vcf_info_field flag in the output options.
  }
  \item{\dots}{
    Arguments passed to other methods. Currently not used.
  }
}

\details{
  \describe{
    \item{}{
      When \code{ensemblVEP} returns a \code{VCF} object, the
      consequence data are returned unparsed in the 'CSQ' INFO 
      column. \code{parseCSQToGRanges} parses these data into a 
      \code{GRanges} object that is expanded to match the dimension 
      of the 'CSQ' data. Because each variant can have multiple 
      matches, the ranges in the \code{GRanges} are repeated.

      If rownames from the original VCF are provided as 
      \code{VCFRowID} a metadata column is included in the result
      that maps back to the row (variant) in the original VCF.

      If no 'CSQ' column is found the function returns the data in
      \code{rowRanges()}.
    }
  }
}

\value{
  Returns a \code{GRanges} object with consequence data as the
  metadata columns. If no 'CSQ' column is found the \code{GRanges}
  from \code{rowRanges()} is returned.
}

\author{
  Valerie Obenchain 
}

\seealso{
  \link{ensemblVEP}
  \link{VEPParam-class}
}

\references{
  Ensembl VEP Home:
  \url{http://uswest.ensembl.org/info/docs/tools/vep/index.html}
}

\examples{
  file <- system.file("extdata", "ex2.vcf", package="VariantAnnotation") 
  vep <- ensemblVEP(file, param=VEPParam(dataformat=c(vcf=TRUE)))
 
  ## The returned 'CSQ' data are unparsed.
  info(vep)$CSQ
 
  ## Parse into a GRanges and include the 'VCFRowID' column.
  vcf <- readVcf(file, "hg19")
  csq <- parseCSQToGRanges(vep, VCFRowID=rownames(vcf))
  csq[1:4]
}

\keyword{methods}
