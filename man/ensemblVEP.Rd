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
\S4method{ensemblVEP}{character}(file, param=VEPParam(), ...)
}

\arguments{
  \item{file}{A  \code{character} specifying the full path to the
    file, including the file name. 
  }
  \item{param}{An instance of \code{VEPParam} specifying runtime options.
  }
  \item{\dots}{Additional arguments passed to methods.
  }
}

\details{
  \describe{
    \item{}{
      The Ensembl VEP tool is described in detail on the home page.
      \url{http://www.ensembl.org/info/docs/variation/vep/index.html}

      The \code{ensemblVEP} function wraps the perl API and requires a 
      local install of the Ensembl VEP available in the users path. The 
      \code{VEPParam} class provides a way to specify runtime options. 
      By default, results are returned as a \code{GRanges} object. Options
      can be set to read the data into a \code{VCF} object or write a
      file to disk.
    }
  }
}

\value{
  Default behavior returns a \code{GRanges} object. Options can be
  set to return a \link{VCF} object or write a file to disk.
}

\author{
  Valerie Obenchain <vobencha@fhcrc.org> 
}

\seealso{
  \link{VEPParam-class}
}

\references{
  Ensembl VEP Home:
  \url{http://www.ensembl.org/info/docs/variation/vep/index.html}

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
gr <- ensemblVEP(file)
gr[1:3]

## When the 'vcf' option is TRUE, a VCF object is returned.
myparam <- VEPParam(output=c(vcf=TRUE))
vcf <- ensemblVEP(file, param=myparam)
vcf

## The consequence data are returned as the 'CSQ' column in info.
info(vcf)$CSQ

## To parse this column use parseCSQToGRanges().
csq <- parseCSQToGRanges(vcf)
head(csq, 4)

## The columns returned are controlled by the 'fields' option. 
## By default all fields are returned. See ?VEPParam for details.

## When comparing ensemblVEP() results to the data in the
## input vcf we see that variant 20:1230237 was not returned.
vcf_input <- readVcf(file, "hg19")
rowData(vcf_input)
rowData(vcf)

## This variant has no alternate allele and is called a
## monomorphic reference. The Ensembl VEP drops these
## variants. 
rowData(vcf)[,c("REF", "ALT")]

## -----------------------------------------------------------------------
## Results written to disk
## -----------------------------------------------------------------------
## By default 'output_file' is set to write to tempfile().
param <- VEPParam()
param
input(param)$output_file

## To write a file to disk, provide a path and file name
## as the 'output_file'.
## Write a vcf file to myfile.vcf:
myparam <- VEPParam(output=c(vcf=TRUE), 
                    input=c(output_file="/path/myfile.vcf"))
## Write a gvf file to myfile.gvf:
myparam <- VEPParam(output=c(gvf=TRUE), 
                    input=c(output_file="/path/myfile.gvf"))

## -----------------------------------------------------------------------
## Runtime options
## -----------------------------------------------------------------------
## All runtime options are controlled by specifying a VEPParam.
## See ?VEPParam for complete details.
param <- VEPParam()

## Logical options are turned on/off with TRUE/FALSE. By
## default, 'quiet' is FALSE.
basic(param)$quiet

## Setting 'quiet' to TRUE will suppress all status and warnings.
basic(param)$quiet <- TRUE

## Characater options are turned on/off by specifying a character 
## value or an empty character (i.e., character()). By default no 
## 'sift' results are returned.
output(param)$sift

## Setting 'sift' to 'b' will return both predictions and scores.
output(param)$sift <- 'b'

## Return 'sift' to the original state of no results returned.
output(param)$sift <- character() 
}

\keyword{methods}
