#' Calculate Prior Parameters for Surplus Production Models
#' 
#' Calculates prior parameters (r, mpar) for surplus production models from 
#' FLStock reference points and historical data.
#' 
#' @param x An FLStock object with benchmark and eqsim attributes
#' @param nmin Numeric vector indicating years to average at start (default: 0:2)
#' @param nmax Numeric vector indicating years to average at end (default: 0:2)
#' 
#' @return A named numeric vector containing:
#'   \item{r}{Intrinsic growth rate}
#'   \item{mpar}{Shape parameter m}
#'   \item{fmsy}{Fishing mortality at MSY}
#'   \item{bmsy}{Biomass at MSY}
#'   \item{b0}{Virgin biomass}
#'   \item{ssb.minyr}{Mean SSB in first years}
#'   \item{ssb.maxyr}{Mean SSB in last years}
#'   \item{f.minyr}{Mean F in first years}
#'   \item{f.maxyr}{Mean F in last years}
#'   \item{h.minyr}{Mean harvest rate in first years}
#'   \item{h.maxyr}{Mean harvest rate in last years}
#' 
#' Returns NULL if shape parameter or growth rate cannot be calculated.
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' data(icesdata)
#' priors = priorFn(icesdata[[1]])
#' }
priorFn <- function(x, nmin = 0:2, nmax = 0:2) {
  fmsy = unlist(c(attributes(x)$benchmark["Fmsy"]))
  bmsy = unlist(c(attributes(x)$eqsim["BMSY"]))
  b0 = unlist(c(attributes(x)$eqsim["B0"]))
  
  ssb.minyr = mean(ssb(x)[, 1 + nmin])
  f.minyr = mean(fbar(x)[, 1 + nmin])
  h.minyr = mean(hr(x)[, 1 + nmin])
  ssb.maxyr = mean(ssb(x)[, dim(ssb(x))[2] - nmax])
  f.maxyr = mean(fbar(x)[, dim(fbar(x))[2] - nmax])
  h.maxyr = mean(hr(x)[, dim(fbar(x))[2] - nmax])
  
  shape = bmsy / b0
  
  if (is.na(shape)) return(NULL)
  
  mi = seq(0.01, 2, 0.001)
  m = (mi^(-1/(mi - 1)) - shape)^2
  m = mi[m == min(m)]
  
  r = (1 - exp(-fmsy)) * (m - 1) / (1 - m^-1)
  
  rtn = c(r = r, mpar = m, fmsy = fmsy, bmsy = bmsy, b0 = b0,
          ssb.minyr = ssb.minyr, ssb.maxyr = ssb.maxyr,
          f.minyr = f.minyr, f.maxyr = f.maxyr,
          h.minyr = h.minyr, h.maxyr = h.maxyr)
  names(rtn) = c("r", "mpar", "fmsy", "bmsy", "b0",
                 "ssb.minyr", "ssb.maxyr",
                 "f.minyr", "f.maxyr",
                 "h.minyr", "h.maxyr")
  
  if (is.na(rtn["r"])) return(NULL)
  rtn
}
