## ----sources-------------------------------------------------------------
library(POD)

## ---- eval=FALSE---------------------------------------------------------
#  # where to store a copy of the macro
#  dest <- "~"
#  exportExcelMacro(dest)

## ----sessionInfo, echo=FALSE---------------------------------------------
#sessionInfo(package='POD')
aux <- sessionInfo()
aux$BLAS <- gsub(aux$BLAS, pattern=".*/lib/", replacement = "pathto/lib/")
aux$LAPACK <- gsub(aux$LAPACK, pattern=".*/lib/", replacement = "pathto/lib/")
print(aux)

