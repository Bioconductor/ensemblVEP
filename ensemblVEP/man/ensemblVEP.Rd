\name{ensemblVEP}

\alias{ensemblVEP}
\alias{ensemblVEP,character-method}

\title{
  Ensembl Variant Effect Predictor (VEP)
}

\description{
  Retrieve variant annotation data from the Ensembl Variant Effect 
  Predictor web service
}

\usage{
\S4method{EnsemblVEP}{character}(file, ..., param=VEPParam(), genome="GRCh37")
}

\arguments{
  \item{file}{A  \code{character} specifying the full path to the
    file, including the file name. 
  }
  \item{VEPParam}{An instance of \code{VEPParam} specifying the run options.
  }
  \item{genome}{A \code{character} specifying the genome to be passed to 
    \code{readVcf{}}.
  }
  \item{\dots}{Additional arguments passed to methods.
  }
}

\details{
  \describe{
    \item{}{
      The Ensembl VEP tool is described in detail on the home page.
      \url{http://www.ensembl.org/info/docs/variation/vep/index.html}

      The \code{ensemblVEP} function wraps the perl API and requires a local 
      install of the Ensembl VEP available in the users path. The 
      \code{VEPParam} class provides a way to specify runtime options. 
      Results are stored in a temporary file and read into a \code{VCF} 
      object with \code{readVcf}.
    }
  }
}

\value{A \link{VCF} object or vcf file on disk.}

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
## -------------------------------------
## ensemblVEP() results as VCF object 
## -------------------------------------
## The default behavior of ensemblVEP() takes a
## vcf file as inupt and returns the output as
## a VCF object.
file <- system.file("extdata", "ex2.vcf", package="VariantAnnotation") 
vep <- ensemblVEP(file)

## When comparing ensemblVEP() results to the data in the
## original vcf we see that variant 20:1230237 was not returned.
library(VariantAnnotation) 
vcf <- readVcf(file, "hg19")
rowData(vcf)
rowData(vep)

## This variant has no alternate allele and is called a
## monomorphic reference. The EnsemblVEP drops these
## variants. 
mcols(fixed(vcf))[c("REF", "ALT")]

## The consequence results from ensemblVEP() are 
## in an info column called 'CSQ'.
info(vep)$CSQ

## This column can be parsed with parseCSQ().
## The return value is a list the same length
## as the number of variants.
consequence <- parseCSQ(vep)
names(consequence)

## -------------------------------------
## Modifying the VEPParam 
## -------------------------------------
## When 'param' is not specified, ensemblVEP() uses
## the default VEPParam
param <- VEPParam()

## The VEPParam holds the runtime options in 6 slots.
## See ?VEPParam for information on class accessors.
param

## The results returned in the CSQ column were specified
## in the 'fields' field of the output slot.
output(param)$fields
colnames(consequence[[4]])

## Below are examples of how to modify the 'param' to
## control what data are returned.

## Silence the status bars and warning messages.
basic(param)$quiet <- TRUE

## Add HGNC gene identifiers and SO consequence terms
output(param)$hgnc <- TRUE

## Check for existing co-located variants, output only coding sequence consequences
filterqc(param) <- c(check_existing=TRUE, coding_only=TRUE)

## Write vcf to disk instead of a VCF object in R.
input(param)$output_file <- "ensemblvep.vcf"
}
