
---
title: "POD: How to use the Excel macro"
output: rmarkdown::html_vignette
author: Markus Bönn (State Office for Consumer Protection Saxony-Anhalt)
date: '06. Mai 2019'

bibliography: bibliography.bib
vignette: >
  %\VignetteIndexEntry{excelmacro}
  %\VignetteEngine{knitr::knitr}
  \usepackage[utf8]{inputenc}
---


# The POD package: A general introduction
The POD package is a re-implementation of a part of the functionality of the commercial tool "PROLab", provided by _QuoData_ (http://quodata.de/). The current version re-implements the evaluation of qualitative PCR experiments within a single laboratory, as described in [@uhlig2015validation].

<!--
An online tool has been setup to perform this kind of analysis ([http://quodata.de/content/validation-qualitative-pcr-methods-single-laboratory/](http://quodata.de/content/validation-qualitative-pcr-methods-single-laboratory/136-680d729fd3856d5e7c42b9a9ff6cfa48ea709ba7d8deda1a168b3fdba3f63d332f3c17806c8400759e05faa3fd0030107f32a1935e2e2e70e607d697f11aa7df)). The output of this online tool (as of February 2019) was used a s aguideline for the R implementation. It should be noted, however, that some results of the POD package and the original implementation in PROLab might disagree marginally.
-->

An online tool has been setup to perform this kind of analysis ([http://quodata.de/content/validation-qualitative-pcr-methods-single-laboratory/](http://quodata.de/content/validation-qualitative-pcr-methods-single-laboratory/)). The output of this online tool (as of February 2019) was used as a guideline for the R implementation. It should be noted, however, that some results of the POD package and the original implementation in PROLab might disagree marginally.

<!--
Two reasons are to blame for this: First, lack of details about the original implementation in "PROLab". Second, the PROLab implementation might have been subject to significant changes since the last revision of the R package.
-->

For instance, the graph provided by the online tool provides bars for each observation, the R implementation does not.

# The Excel macro
The POD package performs analyses in its natural environment, the statistical programming languange R. However, an Excel macro can be used to access the R package and run basic analyses without R experience.

This tutorial briefly guides you through the macro.

# Analysis
First of all, load the package:

```r
library(POD)
```

Get the excel macro:

```r
# where to store a copy of the macro
dest <- "~"
exportExcelMacro(dest)
```
The message confirms the destination, where now you will find a file called "pod.xlsm". Note that a logical is returned, which is FALSE if the a file with name "pod.xlsm" already exists in the destination directory.

## Settings
Open the macro. There are two sheets. In sheet 'Settings' you have to specify essential information about your system and details of your experiment.

1. First, specify the path to your R executable.
2. Define a directory for temporary files.
3. Define the quantile of the limit of detection (LOD). As opposed to the R code, in the Excel macro this has to be an integer. Defaults to 95.

![](sheetSettings.png)

## Input your data
Next, go ahead to sheet 'Input' to enter the data. __Do not change the column headers, the position of the table or the order of columns!__

The columns are as follows

1. nominal DNA concentration
2. number of successful PCR outcomes
3. total number of PCR experiments

Cells contain exemplary values by default. Default values can be used to test the macro. 

Delete default values and enter your own values into columns. Click the button 'compute POD'.

![](sheetInput.png)

## Diagram sheet
A new sheet appears. The graph shows the observed rate of success for each nominal DNA concentration and a fitted POD curve as well as its $95\%$ confidence interval. In addition, the limit of detection (LOD) at $95\%$ and its $95\%$ confidence interval are provided.

Numerical values of the $LOD_{95}$ and its confidence intervals also appear on sheet 'Input'.

If you choose another LOD quantile, $75$ for instance, the output in the graph and the sheet 'Input' is adapted accordingly.

# Appendix
## Output of `sessionInfo()`

```
## R version 3.4.1 (2017-06-30)
## Platform: x86_64-pc-linux-gnu (64-bit)
## Running under: Ubuntu 17.10
## 
## Matrix products: default
## BLAS: pathto/lib/libRblas.so
## LAPACK: pathto/lib/libRlapack.so
## 
## locale:
##  [1] LC_CTYPE=de_DE.UTF-8       LC_NUMERIC=C              
##  [3] LC_TIME=de_DE.UTF-8        LC_COLLATE=C              
##  [5] LC_MONETARY=de_DE.UTF-8    LC_MESSAGES=de_DE.UTF-8   
##  [7] LC_PAPER=de_DE.UTF-8       LC_NAME=C                 
##  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
## [11] LC_MEASUREMENT=de_DE.UTF-8 LC_IDENTIFICATION=C       
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] POD_1.1.2
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_0.12.19     roxygen2_6.1.1   withr_2.1.2      digest_0.6.18   
##  [5] crayon_1.3.4     rprojroot_1.3-2  assertthat_0.2.0 commonmark_1.7  
##  [9] R6_2.3.0         backports_1.1.2  magrittr_1.5     evaluate_0.12   
## [13] rlang_0.3.0.1    stringi_1.2.4    rstudioapi_0.8   testthat_2.0.1  
## [17] xml2_1.2.0       desc_1.2.0       tools_3.4.1      stringr_1.3.1   
## [21] purrr_0.2.5      pkgload_1.0.2    compiler_3.4.1   knitr_1.20
```

# References
