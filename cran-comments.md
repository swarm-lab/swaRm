## Test environments

* local OS X 13.1, R 4.2.2 + devel
* local Windows 10, R 4.2.2 + devel
* local Ubuntu 22.04, R 4.2.2 + devel
* Github Actions "windows-latest (release)"
* Github Actions "macOS-latest (release)"
* Github Actions "ubuntu-22.04-latest (release)"
* Github Actions "ubuntu-22.04-latest (devel)"
* Github Actions "ubuntu-22.04-latest (oldrel-1)"
* Windows Server 2022, R-devel, 64 bit
* r-hub Ubuntu Linux 20.04.1 LTS, R-release, GCC
* r-hub Fedora Linux, R-devel, clang, gfortran
* win-builder.r-project.org

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs.

## Downstream dependencies

There are currently no downstream dependencies for this package.

## CRAN team comments

* The Title field should be in title case. Current version is:
    'A Package For Processing Collective Movement Data'
    In title case without redundancy that is:
    'Processing Collective Movement Data'
   
Fixed.

* Is there some reference about the method you can add in the Description
    field in the form Authors (year) <doi:10.....> or <arXiv:.....>?

Not really. Most of these are standard methods in the field, the rest are my own
creation. 
