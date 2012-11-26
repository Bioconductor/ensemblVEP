\name{VEPToGRanges}
\alias{VEPToGRanges}

\title{
  Convert Ensembl Variant Effect Predictor output to GRanges
}

\description{
  This function converts the tab-deliminited output from the Ensembl 
  Variant Effect Predictor (VEP) to a GRanges object. 
}

\usage{
  VEPToGRanges(x, ..., genome=NULL)
}

\arguments{
  \item{x}{
    An output tab-deliminted file produced by the Ensembl VEP 
    software when the input was a VCF file.
  }
  \item{\dots}{
    Additional arguments for other methods.
  }
  \item{genome}{
    When provided, the \code{genome} information is added to the 
    \code{seqinfo} slot of the \code{GRanges}.
  }
}

\details{
  By default, the \code{ensemblVEP} function will return results in
  a \code{GRanges} object. Chromosome and position make up the
  ranges and all other variables are included as metadata columns.
  The Uploaded_variation variable is used as the rownames.

  The following fields are always returned: Uploaded_variation,
  Location, Allele, Gene, Feature, Feature_type, Consequence, and
  Existing_variation. Additional fields can be requested in the 
  \code{fields} argument of the \code{output} options in the 
  \code{VEPParam}. For example,

  \code{output(<VEPParam>)$fields <- vector of additional options}
}

\value{
  A \code{GRanges} object containing the variants returned by the
  Ensembl VEP. Variables other than chromosome name and position
  are included as metadata columns accessed with \code{mcols}.

  At a minimum, the following columns will be returned,
  \itemize{
    \item Allele : variant allele
    \item Gene : gene ID given by VEP
    \item TXID : transcript ID taken from the 'Feature' column
    \item Feature_type : transcript, gene, exon, etc. 
    \item Consequence : Ensembl or SO consequence term 
    \item Existing_variation : rsID or cosmic ID if present
  }
}

\author{
  Jeremiah Degenhardt and Valerie Obenchain <vobencha@fhcrc.org>
}

\seealso{
  \link{ensemblVEP}
  \link{VEPParam}
}

\examples{
## File ex2.vcf.txt was created by running the variant_effect_predictor perl
## script on the ex2.vcf file in VariantAnnotation with default options.
fl <- system.file("extdata", "ex2.vcf.txt", package="ensemblVEP")
gr <- VEPToGRanges(fl, genome="hg19")
gr
}

\keyword{methods}
