
.onAttach <- function(libname, pkgname) {
    VERSION <- utils::packageDescription(pkgname)$Version
    acro <- ""
    acro <- paste0(acro, "##  PPP    OO   DDD\n")
    acro <- paste0(acro, "##  P  P  O  O  D  D\n")
    acro <- paste0(acro, "##  PPP  O    O D   D\n")
    acro <- paste0(acro, "##  P     O  O  D  D\n")
    acro <- paste0(acro, "##  P      OO   DDD\n")

    msg <- paste0( acro, "##\n##  ", "Loading package '", pkgname, "' version ", VERSION, ".\n##  "
        , "Type help(package=", pkgname, ") for help.\n##  "
        , "Type citation(package='", pkgname,"') for citation.\n##  "
        , "For your suggestions and bug-reports please contact: <markus.boenn.sf@gmail.com>\n##  "
        , "Latest patches can also be found at: <https://github.com/markusboenn/POD>\n##  " 
        )
    packageStartupMessage(msg)
}
