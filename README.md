# HGSB: The Companion Package to *A Hitchhiker's Guide to Statistics in Biology*
### By Stephen R. Bowley, Edzard van Santen, Simon Riley & Ruth Hummel

To be published later this year by Wiley and the ASA-CSA-SSSA. Stay tuned for updates!

## Installation Instructions

### All Operating Systems

If you have never used R before, or if you are unsure of the difference between R and RStudio, or what an "R package" refers to, then you are advised to start by reading through the appendix of the (free) book [Hands-On Programming with R](https://rstudio-education.github.io/hopr/starting.html) by Garrett Grolemund, which provides a clear, concise introduction to R along with instructions for how to install it on your computer. 

### Windows and Mac

Before proceeding further, it is advisable to make sure you have a recent (>4.0.0) version of R installed. This can be checked by running the following code in R:

```
R.version$version.string
```

If need be, navigate to [https://cran.r-project.org/](https://cran.r-project.org/) to download and install the latest version of R.

The `devtools` package is required to install the `HGSB` package from github. To install `devtools`, run the following the code in R:

```
install.package("devtools")
```

For a **complete installation**, which includes all of the packages used in the text's example analyses, run the following code (note that this may take ten minutes or more, depending on how many of the dependencies are already installed on your machine):

```
devtools::install_github(repo = "SimonShamusRiley/HGSB", dependencies = TRUE, INSTALL_opts = c("--no-multiarch"))
```

If asked whether "you want to install from sources the packages which need compilation?", 
select "No" ^[Needing compilation means that the source contains C and/or fortran code. If you have compilers such as [RTools](https://cran.r-project.org/bin/windows/Rtools/) for Windows, you are free to select "Yes"]. If informed that "These packages have more recent versions available. 
It is recommended to update all of them. Which would you like to update?", 

For a **minimal installation**, which omits many packages used in the example analysis (and thus which will require installation later), simply omit the `dependencies = TRUE` argument (*do not* set dependencies to `FALSE`):

```
devtools::install_github(repo = "SimonShamusRiley/HGSB",                                                                           INSTALL_opts = c("--no-multiarch"))
```

## Ubuntu

First, the following code needs to be entered in the terminal in order to add the R 4.0 repository from CRAN, download and install the latest version of R, and to install some additional ubuntu packages needed for the compilation of various R packages later:

```
# Add CRAN Repository
sudo apt update -qq

sudo apt install --no-install-recommends software-properties-common dirmngr

wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | sudo tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc

sudo add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/"

# Install R
sudo apt install r-base r-base-dev

# Needed for devtools and other R packages
sudo apt-get install cmake libcurl4-gnutls-dev libxml2-dev libssl-dev
```

Next, the R package `devtools` should be installed by running the following in the R console:

```
install.package('devtools')
```

For a **complete installation**, which includes all of the packages used in the text's example analyses, run the following code (note that this may take a considerable amount of time, depending on how many of the dependencies are already installed on your machine):

```
devtools::install_github(repo = "SimonShamusRiley/HGSB", dependencies = TRUE)
```

For a **minimal installation**, which omits many packages used in the example analysis (and thus which will require installation later), simply omit the `dependencies = TRUE` argument (*do not* set dependencies to `FALSE`):

```
devtools::install_github(repo = "SimonShamusRiley/HGSB")
```

## Other Linux Distributions

Some guidance for those using Debian, Fedora, etc. is provided [here](https://cloud.r-project.org/bin/linux/). But keep in mind that:

1) You will need the development version of R (e.g. `r-base-dev`, `r-base-devel`) installed to compile packages

2) The R package `devtools` (itself required to install R packages from github) requires additional tools which may not be shipped with the development version of R

3) Like `devtools`, many of the other packages used in the text's example analyses will have additional dependencies. Thus, you are advised to start with the **minimal installation** of the `HGSB` package by running the following in R, and then installing (and troubleshooting) other required packages individually on an as-needed basis:

```
devtools::install_github('SimonShamusRiley/HGSB')
```













