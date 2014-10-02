\name{VEPParam-class}
\docType{class}

% Classes:
\alias{class:VEPParam}
\alias{VEPParam-class}
\alias{class:VEPParam67}
\alias{VEPParam67-class}
\alias{class:VEPParam73}
\alias{VEPParam73-class}
\alias{class:VEPParam75}
\alias{VEPParam75-class}
\alias{class:VEPParam77}
\alias{VEPParam77-class}

% Constructors:
\alias{VEPParam}

% Helpers:
\alias{basicOpts}
\alias{inputOpts}
\alias{cacheOpts}
\alias{outputOpts}
\alias{identifierOpts}
\alias{colocatedVariantsOpts}
\alias{dataformatOpts}
\alias{filterqcOpts}
\alias{databaseOpts}
\alias{advancedOpts}

% Accessors:
\alias{basic}
\alias{basic<-}
\alias{basic,VEPParam-method}
\alias{basic<-,VEPParam-method}
\alias{input}
\alias{input<-}
\alias{input,VEPParam-method}
\alias{input<-,VEPParam-method}
\alias{cache}
\alias{cache<-}
\alias{cache,VEPParam-method}
\alias{cache<-,VEPParam-method}
\alias{output}
\alias{output<-}
\alias{output,VEPParam-method}
\alias{output<-,VEPParam-method}
\alias{filterqc}
\alias{filterqc<-}
\alias{filterqc,VEPParam-method}
\alias{filterqc<-,VEPParam-method}
\alias{database}
\alias{database<-}
\alias{database,VEPParam-method}
\alias{database<-,VEPParam-method}
\alias{advanced}
\alias{advanced<-}
\alias{advanced,VEPParam-method}
\alias{advanced<-,VEPParam-method}
\alias{version}
\alias{version<-}
\alias{scriptPath}
\alias{scriptPath<-}

\alias{identifier}
\alias{identifier<-}
\alias{identifier,VEPParam73-method}
\alias{identifier<-,VEPParam73-method}
\alias{identifier,VEPParam75-method}
\alias{identifier<-,VEPParam75-method}
\alias{identifier,VEPParam77-method}
\alias{identifier<-,VEPParam77-method}
\alias{dataformat}
\alias{dataformat<-}
\alias{dataformat,VEPParam73-method}
\alias{dataformat<-,VEPParam73-method}
\alias{dataformat,VEPParam75-method}
\alias{dataformat<-,VEPParam75-method}
\alias{dataformat,VEPParam77-method}
\alias{dataformat<-,VEPParam77-method}
\alias{colocatedVariants}
\alias{colocatedVariants<-}
\alias{colocatedVariants,VEPParam73-method}
\alias{colocatedVariants<-,VEPParam73-method}
\alias{colocatedVariants,VEPParam75-method}
\alias{colocatedVariants<-,VEPParam75-method}
\alias{colocatedVariants,VEPParam77-method}
\alias{colocatedVariants<-,VEPParam77-method}

% helpers:
\alias{currentVEP}
\alias{supportedVEP}

% "show" method:
\alias{show,VEPParam-method}

\title{VEPParam objects}

\description{
  \code{VEPParam} is a VIRTUAL class with concrete subclasses 
  \code{VEPParam67} and \code{VEPParam73}. These objects store 
  runtime options for the Ensembl Variant Effect Predictor.
}

\details{
  \itemize{
    \item{}{
      The \code{VEPParam} family of objects stores runtime options for querying 
      the Ensembl Variant Effect Predictor (VEP). Brief descriptions of the 
      options are provided below. For complete details, see the Ensembl VEP web page.

      \url{http://uswest.ensembl.org/info/docs/tools/vep/script/vep_options.html}
    }
    \item{VEPParam subclasses}:{
      As of \code{ensemblVEP} version >= 1.3.3 a new subclass of 
      \code{VEPParam} will be created when there is a substantial change 
      in the Ensembl variant database API. The subclass will support all 
      versions up until the next 'substantial' version change. The 
      \code{version} accessor on a \code{VEPParam} object lists all versions 
      that particular \code{VEPParam} class supports. By default the
      \code{VEPParam} constructor creates a subclass that supports the
      current version. To create a \code{VEPParam} for a specific version,
      supply the version as an argument.
    }
    \item{Archived versions}:{
      Past versions of the VEP script and corresponding data are available
      on the Ensembl archive page. 

      \url{http://uswest.ensembl.org/info/website/archives/index.html}

      Archived scripts can query cached data or perform a live query
      against the European mirror. (The US mirror, useastdb, hosts 
      the current release only.) The appropriate version of cached data 
      can be downloaded from the same page as the archived script. 
    }
    \item{Multiple versions}:{
      By default, the variant_effect_predictor.pl script found in the 
      PATH is the script used. If multiple versions of the script are 
      installed locally a few steps must be taken to ensure the
      proper version is used.

      \itemize{
        \item{scriptPath}{
          Provide the full path to the variant_effect_predictor.pl script
          (including the .pl extension) as the \code{scriptPath} argument 
          to the \code{VEPParam} constructor. 
        }
        \item{cache directories}{
          By default, the cache runtime options \code{dir}, \code{dir_cache} 
          and \code{dir_plugins} are set to $HOME/.vep. These locations
          need to be specified if the data are not in the default
          $HOME/.vep location. 
        }
      }
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
    \item{}{
      \code{VEPParam(version=max(unlist(currentVEP()), basic=basicOpts(), 
                     input=inputOpts(), cache=cacheOpts(), output=outputOpts(),
                     filterqc=filterqcOpts(), database=databaseOpts(), 
                     advanced=advancedOpts(), identifier=identifierOpts(), 
                     colocatedVariants=colocatedVariantsOpts(),
                     dataformat=dataformatOpts(), scriptPath=character(), 
                     ...)}
      Creates a \code{VEPParam} object.
      \describe{
        \item{\code{version}}{
          Numeric specifying the Ensembl API version(s) supported.}
        \item{\code{basic}}{
          \code{list} of basic options}
        \item{\code{input}}{
          \code{list} of input options}
        \item{\code{cache}}{
          \code{list} of cache options}
        \item{\code{output}}{
          \code{list} of output options}
        \item{\code{filterqc}}{
          \code{list} of filterqc options}
        \item{\code{database}}{
          \code{list} of database options}
        \item{\code{advanced}}{
          \code{list} of advanced options}
        \item{\code{scriptPath}}{
          \code{character} path to variant_effect_predictor.pl script;
          applicable when multiple versions of the script are installed locally}
      }
      Supported for VEPParam73 and later:
      \describe{
        \item{\code{identifier}}{
          \code{list} of identifier options}
        \item{\code{colocatedVariants}}{
          \code{list} of colocatedVariants options}
        \item{\code{dataformat}}{
          \code{list} of dataformat options}
      }
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
    \item{}{\code{cache(x)}, \code{cache(x) <- value}}
    \item{}{\code{output(x)}, \code{output(x) <- value}}
    \item{}{\code{filterqc(x)}, \code{filterqc(x) <- value}}
    \item{}{\code{database(x)}, \code{database(x) <- value}}
    \item{}{\code{advanced(x)}, \code{advanced(x) <- value}}
    \item{}{\code{version(x)}, \code{version(x) <- value}}
    \item{}{\code{scriptPath(x)}, \code{scriptPath(x) <- value}}
  }
  Supported for VEPParam73 and later:
  \describe{
    \item{}{\code{identifier(x)}, \code{identifier(x) <- value}}
    \item{}{\code{colocatedVariants(x)}, \code{colocatedVariants(x) <- value}}
    \item{}{\code{dataformat(x)}, \code{dataformat(x) <- value}}
  }
}

\section{Helper functions}{
  \describe{
    \item{}{
      \code{currentVEP()}:
      Invoked with no arguments.
      Returns the most current \code{VEPParam} class and supported
      Ensembl API versions.
    }
    \item{}{
      \code{supportedVEP()}:
      Invoked with no arguments.
      Returns a list of \code{VEPParam} subclasses and the the Ensembl 
      API versions they support.
    }
  }
  The following functions create a \code{list} of runtime options and are 
  used in the \code{VEPParam} constructor.
  \describe{
    \item{}{\code{basicOpts(version, ...)}}
    \item{}{\code{inputOpts(version, ...)}}
    \item{}{\code{cacheOpts(version, ...)}}
    \item{}{\code{outputOpts(version, ...)}}
    \item{}{\code{filterqcOpts(version, ...)}}
    \item{}{\code{databaseOpts(version, ...)}}
    \item{}{\code{advancedOpts(version, ...)}}
  }
  Supported for VEPParam73 and later:
  \describe{
    \item{}{\code{identifierOpts(version, ...)}}
    \item{}{\code{colocatedVariantsOpts(version, ...)}}
    \item{}{\code{dataformatOpts(version, ...)}}
  }
}

\author{Valerie Obenchain <vobencha@fhcrc.org>}

\seealso{
  \itemize{
    \item The \code{runtimeOptions} man page.
    \item The \code{ensemblVEP} function man page.
  }
}

\examples{
  ## -----------------------------------------------------------------------
  ## Current API version
  ## -----------------------------------------------------------------------
  ## The default constructor supports the most current version
  ## of the Ensembl variant API.
  param <- VEPParam()
  class(param)

  ## The 'version' slot lists all API versions supported by the class.
  version(param)

  ## The 'supportedVEP' helper returns a list of VEPParam classes and the 
  ## corresponding API versions they support. 
  supportedVEP()

  ## -----------------------------------------------------------------------
  ## Archived API versions
  ## -----------------------------------------------------------------------
  ## Create a VEPParam for an archived version by supplying the 
  ## version to the constructor.
  param67 <- VEPParam(67)
  param67

  ## Archive versions can query the cache or the European mirror. The
  ## default 'host' for live queries is set to 'ensembldb.ensembl.org'.
  database(param67)$host

  ## By default the VEP script used is the one found in the PATH.
  ## To specify a script in a non-standard location use the 'scriptPath' 
  ## setter. Include the full path and the name of the script with the
  ## .pl extension.
  \dontrun{ 
  scriptPath(param) <- "fullPathToScript/variant_effect_predictor.pl"
  }

  ## -----------------------------------------------------------------------
  ## Manipulation
  ## -----------------------------------------------------------------------

  ## View the values in 'basic' and 'input'.
  basic(param)
  input(param)

  ## Change the value of the 'everything' to TRUE.
  basic(param)$everything
  basic(param)$everything <- TRUE
  basic(param)$everything

  ## Replace multiple values using a named list.
  basic(param) <- list(verbose=TRUE, config="myconfig.txt")
  basic(param)

  ## Write the output to myfile.vcf instead of returning a VCF object.
  ## Return the sift and polyphen predictions only (not scores).
  param <- VEPParam(input=c(output_file="path/myfile.vcf"),
                    output=c(sift="p", polyphen="p"))

  ## 'sift' and 'polyphen' are runtime options that require
  ## a character value, (i.e., 's', 'p', or 'b').
  output(param)$sift

  ## To turn off 'sift' or 'polyphen' set the value to an
  ## empty character (i.e., character()).
  output(param)$sift <- character() 
}

\keyword{methods}
\keyword{classes}
