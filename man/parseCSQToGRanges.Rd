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
\S4method{parseCSQToGRanges}{character}(x, VCFRowID=TRUE, ...)
\S4method{parseCSQToGRanges}{VCF}(x, VCFRowID=TRUE, ...)
}

\arguments{
  \item{x}{
    The \code{character} name of a vcf file on disk or a \code{VCF} object
  }
  \item{VCFRowID}{
    \code{logical}. Include a metadata column that maps the rows of
    the expanded consequences back to the rows of the \code{VCF} object. 
  }
  \item{\dots}{
    Arguments passed to other methods.
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
vcf <- ensemblVEP(file, param=VEPParam(output=c(vcf=TRUE)))

## The returned 'CSQ' data are unparsed.
vcf$CSQ

## Parse the data into a GRanges.
csq <- parseCSQToGRanges(vcf)
csq[1:4]
}

\keyword{methods}
