# HGSB: The Companion Package to *A Hitchiker's Guide to Statistics in Biology*
### By Stephen R. Bowley, Edzard van Santen, Simon Riley & Ruth Hummel

To be published later this year by Wiley and the ASA-CSA-SSSA. Stay tuned for updates!

### Installation Instructions

If you don't know whether or not R is installed on your computer, the difference between R and RStudio, what is meant by the "R console", or what an "R package" refers to, then you are advised to start by reading through the appendix of the (free) book [Hands-On Programming with R](https://rstudio-education.github.io/hopr/starting.html) by Garrett Grolemund, which provides a clear, concise introduction to R along with instructions for how to install it on your computer.

If you are familiar with the above concepts, then just make sure you have a recent (>4.0.0) version of R installed by running the following code in the R console:

`R.version$version.string`

If need be, navigate to [https://cran.r-project.org/](https://cran.r-project.org/) to download and install the latest version of R. 

**Windows/Mac**

The following lines of code will install the latest version of the `HGSB` package, along with all of its dependencies, which includes all of the packages used in the book's example analyses (this may take 10 minutes or more to complete, depending on how many of those dependencies are already installed on your machine):

`install.package("devtools")`
`devtools::install_github(repo = "SimonShamusRiley/HGSB", INSTALL_opts=c("--no-multiarch"))`

If asked whether "you want to install from sources the packages which need compilation?", 
select "No". If informed "These packages have more recent versions available. 
It is recommended to update all of them. Which would you like to update?", type "2" in the
console and hit enter - this will update only those packages which have newer versions 
available on CRAN.

**Linux**

Testing (with Ubuntu desktop 18.04) is still underway, but it may be advisable to carefully follow the instructions provided by CRAN for installing on [Ubuntu](https://cran.r-project/bin/linux/ubuntu), [Debian](https://cran.r-project/bin/linux/debian), [Fedora/Redhat](https://cran.r-project/bin/linux/fedora)
