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
\S4method{ensemblVEP}{character}(file, ..., param=VEPParam(), genome="GRCh37")
}

\arguments{
  \item{file}{A  \code{character} specifying the full path to the
    file, including the file name. 
  }
  \item{param}{An instance of \code{VEPParam} specifying the run options.
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

      The \code{ensemblVEP} function wraps the perl API and requires a 
      local install of the Ensembl VEP available in the users path. The 
      \code{VEPParam} class provides a way to specify runtime options. 
      By default, results are returned as a \code{GRanges} object. Options
      can be set to read the data into a \code{VCF} object.
    }
  }
}

\value{
  Default behavior returns a \code{GRanges} object. Options can be
  set to return a \link{VCF} object or vcf file on disk.
}

\author{
  Valerie Obenchain <vobencha@fhcrc.org> 
}

\seealso{
  \link{VEPParam-class}
  \link{VEPToGRanges}
}

\references{
  Ensembl VEP Home:
  \url{http://www.ensembl.org/info/docs/variation/vep/index.html}

  Human Genome Variation Society (hgvs):
  \url{http://www.hgvs.org/mutnomen/}
}

\examples{
## -------------------------------------------------------------
## Results as GRanges object 
## -------------------------------------------------------------
## The default behavior takes a VCF file as input and returns 
## a GRanges.
file <- system.file("extdata", "ex2.vcf", package="VariantAnnotation") 
gr <- ensemblVEP(file)
gr

## -------------------------------------------------------------
## Results as VCF object 
## -------------------------------------------------------------
## To change the output to a VCF object, change the --vcf flag
## in the output options.
param <- VEPParam()
output(param)$vcf

output(param)$vcf <- "CSQ"
vep <- ensemblVEP(file, param=param)

## When comparing ensemblVEP() results to the data in the
## original vcf we see that variant 20:1230237 was not returned.
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

## The results returned in the CSQ column were specified
## in the 'fields' field of the output slot.
output(param)$fields

## The CSQ column can be parsed with parseCSQ().
consequence <- parseCSQ(vep)
names(consequence)

## -------------------------------------------------------------
## Results as a VCF file on disk.
## -------------------------------------------------------------
## Change the 'output _file' value to a local path and filename.
param$output_file <- "mypath/myfilename.vcf"
#ensemblVEP(file, param=param)

## -------------------------------------------------------------
## The VEPParam  object
## -------------------------------------------------------------
## When 'param' is not specified, ensemblVEP() uses
## the default VEPParam.
param <- VEPParam()

## The VEPParam holds the values of the runtime options.
## See ?VEPParam for class accessors.

## Below are examples of how to modify the 'param' to
## control what data are returned.

## Silence the status bars and warning messages.
basic(param)$quiet <- TRUE

## Add HGNC gene identifiers and SO consequence terms
output(param)$hgnc <- TRUE

## Check for existing co-located variants, output only coding 
## sequence consequences
filterqc(param) <- c(check_existing=TRUE, coding_only=TRUE)
}

\keyword{methods}
