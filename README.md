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
`devtools::install_github(repo = "SimonShamusRiley/HGSB", dependencies = TRUE, INSTALL_opts=c("--no-multiarch"))`

If asked whether "you want to install from sources the packages which need compilation?", 
select "No". If informed "These packages have more recent versions available. 
It is recommended to update all of them. Which would you like to update?", type "2" in the
console and hit enter - this will update only those packages which have newer versions 
available on CRAN.

If you do not wish to download all the dependencies at once, either because it is causing errors or because you wish to do so on an as-needed basis, simply run the code above with `dependencies = FALSE`.

**Linux**

The code to install `devtools` and then `HGSB` in R is of course identical in all three operating systems. However, two steps are needed first when operating in linux: first, make sure to install the developer version of R, which comes bundled with most of the tools needed for R to compile and install packages. In the terminal run:

`sudo apt-get install r-base-dev`

Additionally, the following will also be needed to install the `devtools` package, among others:

`sudo apt-get install build-essential cmake libcurl4-gnutls-dev libxml2-dev libssl-dev`

If the package still fails to install due to failure to install dependencies, try setting `dependencies = FALSE` when installing the `HGSB` package (see the section on installing in Windows/Mac, above).


