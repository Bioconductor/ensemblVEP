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
\S4method{parseCSQToGRanges}{character}(x, VCFRowID=NULL, ...)
\S4method{parseCSQToGRanges}{VCF}(x, VCFRowID=NULL, ...)
}

\arguments{
  \item{x}{
    The \code{character} name of a vcf file on disk or a \code{VCF} object
  }
  \item{VCFRowID}{
    Default \code{NULL} or a \code{character} vector of rownames from
    the original VCF. 

    When rownames are provided, a metadata column named \sQuote{VCFRowID}
    is included in the result which maps back to the row (variant) in the 
    original VCF.

    When \code{VCFRowID=NULL} or is not specified, no \sQuote{VCFRowID}
    column is included in the output. 
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
    }
  }
}

\value{
  Returns a \code{GRanges} object with consequence data as the
  metadata columns.
}

\author{
  Valerie Obenchain <vobencha@fhcrc.org> 
}

\seealso{
  \link{ensemblVEP}
  \link{VEPParam-class}
}

\references{
  Ensembl VEP Home:
  \url{http://www.ensembl.org/info/docs/variation/vep/index.html}
}

\examples{
file <- system.file("extdata", "ex2.vcf", package="VariantAnnotation") 
vep <- ensemblVEP(file, param=VEPParam(output=c(vcf=TRUE)))

## The returned 'CSQ' data are unparsed.
info(vep)$CSQ

## Parse into a GRanges and include the 'VCFRowID' column.
vcf <- readVcf(file, "hg19")
csq <- parseCSQToGRanges(vep, VCFRowID=rownames(vcf))
csq[1:4]
}

\keyword{methods}
