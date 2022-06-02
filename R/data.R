#' @title Sudden Oak Death (*Phytophthora ramorum* Werres et al.) detection using polymerase
#' chain reaction (PCR) and tissue culture
#'
#' @description A confusion matrix showing detection/non-detection of 428 tests for
#' Sudden Oak Death using two diagnostic methods.
#'
#' @format A 2x2 array containing the results of 428 tests for Sudden Oak Death conducted
#' using both:
#' \describe{
#'   \item{**Gold Standard Test**:}{Agar culture of plant tissue}
#'   \item{**Alternate Test**:}{PCR testing for causal agent}
#' }
#' @source Vettraino, A.M., Sukno, S., Vannini, A., & Garbelotto, M. (2010). Diagnostic
#'   sensitivity and specificity of different methods used by two laboratories for the
#'   detection of Phytophthora ramorum on multiple natural hosts. Plant Pathology, 59,
#'   289-300. DOI: [10.1111/j.1365-3059.2009.02209.x](https://doi.org/10.1111/j.1365-3059.2009.02209.x)
"pcr"

#' @title Measurements of canola (*Brassica napus* L.) height
#'
#' @format A data frame containing 18 measurements:
#' \describe{
#'   \item{**height_cm**:}{Plant height, in cm}
#' }
#'
#' @source Cole, B. (1998). The use of morphological and RAPD markers for distinguishing
#'   canola (Brassica napus L.) varieties. \[Master’s thesis, University of Guelph\]. UG
#'   Atrium Repository. \url{https://hdl.handle.net/10214/20400}.
"canola"

#' @title Maize (*Zea mays* L.) performance under two methods of crop improvement
#'
#' @format A data frame containing 58 measurements:
#' \describe{
#'   \item{**method**:}{Breeding method, a factor with two levels "A" and "B"}
#'   \item{**hybrid**:}{Hybrid identifier, an integer}
#'   \item{**mpi**:}{Performance index, numeric value equal to 100 × (yield ÷ grain moisture)}
#' }
#'
#' @source Peña Ortega, G. (1999). Evaluation of three recurrent selection methods in two
#'   short-season maize (Zea mays L.) synthetics. \[Doctoral dissertation, University of
#'   Guelph\]. UG Atrium Repository.
#'
"maize"

#' @title Pea (*Pisum sativum* L.) color
#' @aliases pea_colour
#' @description An array containing the number of plants with white and purple flowers in a sample of 81 pea (*Pisum sativum* L.) plants.
#' @format A 1x2 array of integers
#'
#' @source Hellens, R. P., Moreau, C., Lin-Wang, K., Schwinn, K. E., Thomson, S. J.,
#'   Fiers, M. W. E. J., Frew, T. J., Murray, S. R., Hofer, J. M. I., Jacobs, J. M. E.,
#'   Davies, K. M., Allan, A. C., Bendahamane, A., Coyne, C. J., Timmerman-Vaughan, G. M.,
#'   & Ellis, T. H. N. (2010). Identification of Mendel’s white flower character. PLOS
#'   One, 5(10): e1320. DOI: [10.1371/journal.pone.0013230](https://www.doi.org/10.1371/journal.pone.0013230).
"pea_color"



