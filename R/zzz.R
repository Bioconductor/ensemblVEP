.onAttach <-
    function(libname, pkgname)
{
    msg <- paste0("vep script not found. ",
                  "Ensembl VEP is not installed in your path.")
    tryCatch(check <- system2(.getVepPath(),
        stdout=TRUE, stderr=TRUE),
        error=function(e) packageStartupMessage(msg))
}
