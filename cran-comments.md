## Test environments
* local OS X install, R 3.6.2
* local Windows install, R 3.6.2
* Fedora Linux, R-devel, clang, gfortran
* Debian Linux, R-release, GCC
* Windows Server 2008 R2 SP1, R-release, 32/64 bit
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:

* checking installed package size ... NOTE
    installed size is 22.3Mb
    sub-directories of 1Mb or more:
      R  22.0Mb

  The data contained in sysdata.rda is required to estimate model predictors, 
  which is essential to the main function of the package.
  
* This is my first submission.

## Downstream dependencies
There are currently no downstream dependencies for this package.
