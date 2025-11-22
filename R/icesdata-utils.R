#' @rdname ebiomass
#' @export
setMethod("ebiomass", signature(object="FLStock"),
          function(object) {
            sel   <- harvest(object)
            wt    <- catch.wt(object) %*% sel %/% fapex(sel)
            eb.wt <- qmax(wt, 0.000001)
            
            apply(eb.wt %*% stock.n(object), 2:6, sum)
          })

setMethod("ebiomass", signature(object="FLBRP"),
          function(object) {
            sel   <- harvest(object)
            wt    <- catch.wt(object) %*% sel %/% fapex(sel)
            eb.wt <- qmax(wt, 0.000001)
            
            apply(eb.wt %*% stock.n(object), 2:6, sum)
          })


#' Internal Function for Benchmark Extraction
#'
#' @param object An FLStock object
#' @return An FLPar object
#' @keywords internal
benchmarksFn <- function(object) {
  if ("logical"%in%is(attributes(object)$benchmark))
    return(FLCore::FLPar(fmsy=NA,flim=NA,fpa=NA,blim=NA,bpa=NA,btrigger=NA))
  
  if ("numeric"%in%is(attributes(object)$benchmark))
    attributes(object)$benchmark=FLCore::FLPar(attributes(object)$benchmark)
  
  names(attributes(object)$benchmark)=tolower(names(attributes(object)$benchmark))

  nms=names(attributes(object)$benchmark)[names(attributes(object)$benchmark)%in%
                                   c("fmsy","flim","fpa","blim","bpa","btrigger")]
                                 
  methods::as(attributes(object)$benchmark[nms],"FLPar")
}

#' @rdname benchmark
#' @export
setMethod("benchmark", signature(object="FLStock"), function(object) {
  if (!("benchmark" %in% names(attributes(object)))) {
    warning("No benchmark attribute found for this FLStock object.")
    return(NULL)}
  
  names(attributes(object)$benchmark)=tolower(names(attributes(object)$benchmark))

  benchmarksFn(object)
})

#' @rdname benchmark
#' @export
setMethod("benchmark", signature(object="FLStocks"), function(object) {
  plyr::ldply(plyr::llply(object, function(x) t(benchmark(x))), plyr::rbind.fill)
})

#' @rdname benchmark
#' @export
setMethod("benchmark", signature(object="FLBRP"), function(object) {
  # For FLBRP objects, extract reference points from refpts slot
  refs <- FLCore::refpts(object)
  
  # Create FLPar with benchmark reference points
  benchmark_pars <- FLCore::FLPar(
    fmsy = refs["msy", "harvest"],
    flim = refs["lim", "harvest"],
    fpa = refs["pa", "harvest"],
    blim = refs["lim", "ssb"],
    bpa = refs["pa", "ssb"],
    btrigger = refs["trigger", "ssb"]
  )
  
  # Remove any NA values
  benchmark_pars <- benchmark_pars[!is.na(benchmark_pars)]
  
  return(benchmark_pars)
})

#' Internal Function for FishLife Parameter Extraction
#'
#' @param object An FLStock object
#' @return An FLPar object
#' @keywords internal
fishlifesFn <- function(object) {
  if ("logical"%in%is(attributes(object)$fishlife))
    return(FLCore::FLPar(Fmsy=NA,Flim=NA,Fpa=NA,Blim=NA,Bpa=NA,Btrigger=NA))
  
  if ("numeric"%in%is(attributes(object)$fishlife))
    attributes(object)$fishlife=FLCore::FLPar(attributes(object)$fishlife)
  
  methods::as(attributes(object)$fishlife,"FLPar")
}

#' @rdname fishlife
#' @export
setMethod("fishlife", signature(object="FLStock"), function(object) {
  if (!("fishlife" %in% names(attributes(object)))) {
    warning("No fishlife attribute found for this FLStock object.")
    return(NULL)}
  
  fishlifesFn(object)
})

#' @rdname fishlife
#' @export
setMethod("fishlife", signature(object="FLStocks"), function(object) {
  plyr::ldply(plyr::llply(object, function(x) t(fishlife(x))), plyr::rbind.fill)
})


#' Internal Function for EqSim Reference Point Extraction
#'
#' @param object An FLStock object
#' @return An FLPar object
#' @keywords internal
eqsimFn <- function(object) {
  if ("logical"%in%is(attributes(object)$eqsim))
    return(FLCore::FLPar(catchequi=NA,bmsy=NA,b0=NA,fmsyMedianC=NA,fmsyMedianL=NA,f5percRiskBlim=NA,flimEqsim=NA,r0=NA)) 
  
  if ("numeric"%in%is(attributes(object)$eqsim))
    attributes(object)$eqsim=FLCore::FLPar(attributes(object)$eqsim)
  
  names(attributes(object)$eqsim)=stringr::str_c(tolower(stringr::str_sub(names(attributes(object)$eqsim), 1, 1)), 
                                           stringr::str_sub(names(attributes(object)$eqsim), 2))

  names(attributes(object)$eqsim)=stringr::str_replace_all(names(attributes(object)$eqsim), "MSY", "msy")
  
  nms=names(attributes(object)$eqsim)[names(attributes(object)$eqsim)%in%
    c("catchequi","bmsy","b0","fmsyMedianC","fmsyMedianL","f5percRiskBlim","flimEqsim","r0")]

  methods::as(attributes(object)$eqsim[nms],"FLPar")}

#' @rdname eqsim
#' @export
setMethod("eqsim", signature(object="FLStock"), function(object) {
  if (!("eqsim" %in% names(attributes(object)))) {
    warning("No eqsim attribute found for this FLStock object.")
    return(NULL)}
  
  eqsimFn(object)
})

#' @rdname eqsim
#' @export
setMethod("eqsim", signature(object="FLStocks"), function(object) {
  plyr::ldply(plyr::llply(object, function(x) t(eqsim(x))), plyr::rbind.fill)
})

#' Internal Function for FLife Parameter Extraction
#'
#' @param object An FLStock object
#' @return An FLPar object with life history parameters
#' @keywords internal
FLifeParFn <- function(object) {
  res=attributes(object)$fishlife
  
  if (!("fishlife"%in%names(attributes(object))))
    return(FLife::lhPar(FLCore::FLPar(c("linf"=NA,"k"=NA,"l50"=NA,"s"=NA))))
  
  if ("lm"%in%names(res))
    names(res)[seq_along(res)[(names(res)=="lm")]]="l50"
  
  res=FLCore::FLPar(res,units="NA")
  rtn=FLCore::FLPar("linf"     =NA,
            "k"        =NA,       
            "winf"     =NA,       
            "tmax"     =NA,       
            "tm"       =NA,       
            "m"        =NA,      
            "lm"       =NA,       
            "rho"      =NA,       
            "sigmaR"   =NA,     
            "s"        =NA,     
            "fmsy"     =NA,     
            "r"        =NA,    
            "g"         =NA,     
            "sd.logit.s"=NA)
  
  FLife::lhPar(res[c("linf","k","l50","s")])
}

#' @rdname FLifePar
#' @export
setMethod("FLifePar", signature(object="FLStock"), function(object) {
  if (!("fishlife" %in% names(attributes(object)))) {
    warning("No benchmark attribute found for this FLStock object.")
    return(NULL)}
  
  FLifeParFn(object)
})

#' @rdname FLifePar
#' @export
setMethod("FLifePar", signature(object="FLStocks"), function(object) {
  rtn=plyr::ldply(plyr::llply(object, function(x) t(FLifePar(x))), plyr::rbind.fill)
  rtn
})

#' Extracts Kobe Indicators  (SSB/BMSY and F/FMSY)
#' 
#' @description 
#' Calculates SSB and fishing mortality ratios (SSB/BMSY and F/FMSY)
#' for Kobe plot from an FLStock object.
#' 
#' @param path An object of class FLStock containing stock assessment results
#' @param method Not used, included for method consistency
#' 
#' @return An FLQuants object containing:
#' \itemize{
#'   \item stock: Time series of SSB/BMSY ratios
#'   \item harvest: Time series of F/FMSY ratios
#' }
#' 
#' @details 
#' The method requires that the FLStock object has:
#' \itemize{
#'   \item eqsim attribute containing BMSY reference point
#'   \item benchmark attribute containing FMSY reference point
#' }
#' 
#' @export
#' @rdname kobe
#' 
#' @examples
#' \dontrun{
#' data(ple4)
#' kobe_indicators <- kobe(ple4)
#' }
#' 
#' @seealso 
#' \code{\link[FLCore]{FLStock}}, \code{\link[FLCore]{FLQuants}}
setMethod( 'kobe',  signature(path='FLStock',method="missing"), 
           function(path, method, ...){ 
               names(attributes(path)$eqsim)    =tolower(names(attributes(path)$eqsim))
               names(attributes(path)$benchmark)=tolower(names(attributes(path)$benchmark))
               
               FLQuants(path, "stock"   =function(x) ssb(x)%/%eqsim(     x)["bmsy"],
                              "harvest" =function(x) fbar(x)%/%benchmark(x)["fmsy"],
                              "blim"    =function(x) ssb(x)%/%benchmark( x)["blim"],
                              "flim"    =function(x) ssb(x)%/%benchmark( x)["flim"])})
           
setMethod( 'kobe',  signature(path='FLBRP',method="missing"), 
           function(path, method, ...){ 
             
             FLQuants(path, 
                      "stock"  =function(x) ssb.obs( x)%/%refpts(x)["msy","ssb"],
                      "harvest"=function(x) fbar.obs(x)%/%refpts(x)["msy","harvest"],
                      "blim"   =function(x) ssb.obs( x)%/%blim(x)["blim","ssb"],
                      "flim"   =function(x) fbar.obs(x)%/%blim(x)["blim","harvest"])})

setMethod( 'kobe',signature(path='FLBRP',method="logical"), 
           function(path, method, ...){ 
             
    kb =kobe(path)
    rtn=FLQuants(green      =as.FLQuant(kb$stock>=1&kb$harvest<=1),
                 yellow     =as.FLQuant(kb$stock< 1&kb$harvest<=1),
                 orange     =as.FLQuant(kb$stock>=1&kb$harvest> 1),
                 red        =as.FLQuant(kb$stock< 1&kb$harvest> 1),
                 overfished =as.FLQuant(kb$stock< 1),
                 overfishing=as.FLQuant(kb$harvest<=1))
    rtn=model.frame(rtn)
    rtn=subset(reshape2::melt(rtn[,1:10],names(rtn)[1:6]),value==1)[,-8]
    
    return(rtn)})
#' @rdname calcLc
#' @export
setMethod("calcLc", signature(object = "FLQuant"),
          function(object, prob = 0.5) {
            dat = subset(as.data.frame(object), data > 0)
            if (nrow(dat) == 0) return(NA)
            
            dat = plyr::ddply(dat, .(len), with, cumsum(sum(data)) / sum(data))
            dat$V1 = cumsum(dat$V1) / sum(dat$V1)
            
            idx = which(abs(dat$V1 - prob) == min(abs(dat$V1 - prob)))
            if (length(idx) > 0) {
              dat[idx[1], "len"]
            } else {
              NA
            }
          })

#' @rdname checkVariation
#' @export
setMethod("checkVariation", signature(object = "FLStock"),
          function(object) {
            mData = m(object)
            wtData = stock.wt(object)
            matData = mat(object)
            
            data.frame(
              mAge = !all(mData[1, ] == apply(mData, 2, mean)),
              mYr = !all(apply(mData, 2, mean) == mean(mData[, 1])),
              massYr = !all(apply(wtData, 2, mean) == mean(wtData[, 1])),
              matYr = !all(apply(matData, 2, mean) == mean(matData[, 1]))
            )
          })

#' @rdname checkVariation
#' @export
setMethod("checkVariation", signature(object = "FLStocks"),
          function(object) {
            result = plyr::ldply(object, checkVariation)
            names(result)[1] = ".id"
            result
          })

    
