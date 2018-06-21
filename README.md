# RIC
R Package for Relative Impact Characteristic (RIC) Curve

*RIC* is an R Package for the Relative Impact Characteristic (RIC) Curve: a novel graphical tool that visualizes and quantifies the population-level consequences of implementing diagnostic and prognostic biomarkers.

*RIC* is currently under development, and is available through [GitHub](https://github.com/aminadibi/RIC). 

To install RIC, follow these steps:

## Installation
### Windows 7 or Later
1. Download and Install the latest version of R from [https://cran.r-project.org/bin/windows/base/](https://cran.r-project.org/bin/windows/base/)
2. Download and Install R Studio from [https://www.rstudio.com/products/rstudio/download/](https://www.rstudio.com/products/rstudio/download/)
3. Using either an R session in Terminal or in R Studio, install the package `devtools`:
  `install.packages ('devtools')`
4. Install RIC from GitHub:
  `devtools::install_github('aminadibi/RIC')`


### Mac OS Sierra and Later
1. Download and Install the latest version of R from [https://cran.r-project.org/bin/macosx/](https://cran.r-project.org/bin/macosx/)
2. Download and Install R Studio from [https://www.rstudio.com/products/rstudio/download/](https://www.rstudio.com/products/rstudio/download/)
3. Using either an R session in Terminal or in R Studio, install the package `devtools`:

  `install.packages ('devtools')`

4. Install RIC from GitHub:

`devtools::install_github('aminadibi/epicR')`

### Ubuntu 16.04 and Later
1. Install R by executing the following commands in Terminal:

  `sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9`

  `sudo add-apt-repository 'deb [arch=amd64,i386] https://cran.rstudio.com/bin/linux/ubuntu xenial/'`

  `sudo apt-get update`

  `sudo apt-get install r-base`

If the installation is successful, you should be able to start R:

  `sudo -i R`

2. Download and Install R Studio from [https://www.rstudio.com/products/rstudio/download/](https://www.rstudio.com/products/rstudio/download/)
3. Install `libcurl` from Terminal: 

  `sudo apt-get install libcurl4-openssl-dev libssl-dev`

4. Using either an R session in Terminal or in R Studio, install the package `devtools`:

  `install.packages ('devtools')`
5. Install RIC from GitHub:

  `devtools::install_github('aminadibi/RIC')`

