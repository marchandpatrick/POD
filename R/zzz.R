
.onAttach <- function(libname, pkgname) {
    VERSION <- utils::packageDescription(pkgname)$Version
    msg <- paste0( "Loading package '", pkgname, "' version ", VERSION, ".\n"
        , "Type help(package=", pkgname, ") for help.\n"
        , "Type citation(package='", pkgname,"') for citation.\n"
        , "For your suggestions and bug-reports please contact: <markus.boenn.sf@gmail.com>\n" )
    packageStartupMessage(msg)
}
