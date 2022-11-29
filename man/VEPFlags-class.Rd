\name{VEPFlags-class}
\docType{class}

% Classes:
\alias{class:VEPFlags}
\alias{VEPFlags-class}

% Constructors:
\alias{VEPFlags}

% Helpers:
\alias{flagOpts}

% Accessors:
\alias{flags}
\alias{flags<-}
\alias{flags,VEPFlags-method}
\alias{flags<-,VEPFlags-method}

% helpers:
\alias{currentVEP}
\alias{supportedVEP}

% "show" method:
\alias{show,VEPFlags-method}

\title{VEPFlag objects}

\description{
  \code{VEPFlags} stores runtime options for the Ensembl Variant Effect
  Predictor.
}

\details{
  \itemize{
    \item{-}{
      The \code{VEPFlags} family of objects stores runtime options for querying
      the Ensembl Variant Effect Predictor (VEP). For complete details,
      see the Ensembl VEP web page.

      \url{http://uswest.ensembl.org/info/docs/tools/vep/script/vep_options.html}
    }
  }
}

\section{Runtime options}{
  For a description of runtime options for the most current version of
  the API see the ?\code{runtimeOptions} man page or the Ensembl web site:

  \url{http://www.ensembl.org/info/docs/tools/vep/script/vep_options.html}

  Runtime options for archived versions can be found on the corresponding
  archive page.
}

\section{Constructor}{
  \describe{
    \item{-}{
      \code{VEPFlags(version=max(unlist(currentVEP())),
	scriptPath=character(), flags=list(), ...)}
      Creates a \code{VEPFlags} object.
      \describe{
	\item{\code{version}}{
	  Numeric specifying the Ensembl API version(s) supported.}
	\item{\code{flags}}{
	  \code{list} of runtime options}
	\item{\code{scriptPath}}{
	  \code{character} path to variant_effect_predictor.pl script;
	  applicable when multiple versions of the script are installed locally}
      }
    }
  }
}

\section{Accessors}{
  In the following code, \code{x} is a \code{VEPFlags} object
  and \code{value} is a named \code{list} or \code{character}
  vector.
  \describe{
    \item{-}{\code{flags(x)}, \code{flags(x) <- value}}
 }
}

\section{Helper functions}{
  \describe{
    \item{-}{
      \code{currentVEP()}:
      Invoked with no arguments.
      Returns the most current \code{VEPFlags} class and supported
      Ensembl API versions.
    }
    \item{-}{
      \code{supportedVEP()}:
      Invoked with no arguments.
      Returns a list of \code{VEPParam} and \code{VEPFlags} subclasses
      and the Ensembl API versions they support.
    }
  }
  The following functions create a \code{list} of runtime options and are
  used in the \code{VEPFlags} constructor.
  \describe{
    \item{-}{\code{flagOpts(version, ...)}}
  }
}

\author{Lori Shepherd}

\seealso{
  \itemize{
    \item The \code{runtimeOptions} man page.
    \item The \code{ensemblVEP} function man page.
    \item The \code{VEPParam} class page for archieved API verisons.
  }
}

\examples{
  ## -----------------------------------------------------------------------
  ## Current API version
  ## -----------------------------------------------------------------------
  ## The default constructor supports the most current version
  ## of the Ensembl variant API.
  param <- VEPFlags()
  class(param)

  ## The 'version' slot lists all API versions supported by the class.
  version(param)

  ## The 'supportedVEP' helper returns a list of VEPParam and VEPFlags
  ## classes and the corresponding API versions it support.
  supportedVEP()

  ## -----------------------------------------------------------------------
  ## Archived API versions
  ## -----------------------------------------------------------------------
  ## Create a VEPParam for an archived version by supplying the
  ## version to the constructor.
  ## See ?VEPParam for more details

  ## Note on host: A host is not assigned by default and therefore will
  ## use the VEP host default of "ensembldb.ensembl.org". Users in the
  ## US may find connection and transfer speeds quicker using our East
  ## coast mirror, "useastdb.ensembl.org"
  param <- VEPFlags(flags=list(host="useastdb.ensembl.org"))

  ## By default the VEP script used is the one found in the PATH.
  ## To specify a script in a non-standard location use the 'scriptPath'
  ## setter. Include the full path and the name of the script with the
  ## .pl extension.
  \dontrun{
  scriptPath(param) <- "fullPathToScript/vep.pl"
  }

  ## -----------------------------------------------------------------------
  ## Manipulation
  ## -----------------------------------------------------------------------

  ## View the runtime values in 'flags'.
  flags(param)

  ## Change the value of the 'everything' to TRUE.
  flags(param)$everything
  flags(param)$everything <- TRUE
  flags(param)$everything

  ## Replace multiple values using a named list.
  flags(param) <- list(verbose=TRUE, config="myconfig.txt")
  flags(param)

  ## Write the output to myfile.vcf instead of returning a VCF object.
  ## Return the sift and polyphen predictions only (not scores).
  param <- VEPFlags(flags=list(output_file="path/myfile.vcf",
		    sift="p", polyphen="p"))

  ## 'sift' and 'polyphen' are runtime options that require
  ## a character value, (i.e., 's', 'p', or 'b').
  flags(param)$sift
}

\keyword{methods}
\keyword{classes}
