#' Extract Time Series Statistics from FLStock Objects
#'
#' @description
#' Creates a data frame of time series statistics from FLStock objects, including
#' catch, ebiomass, SSB, fishing mortality, harvest rate, and mean natural mortality.
#'
#' @param object An object of class FLStock or FLStocks
#' @param ... Additional arguments (not currently used)
#'
#' @return A data frame containing time series of:
#'   \itemize{
#'     \item catch - Catch values
#'     \item eb - Exploitable biomass
#'     \item ssb - Spawning stock biomass
#'     \item f - Fishing mortality (Fbar)
#'     \item h - Harvest rate (catch/ebiomass)
#'     \item m - Mean natural mortality
#'   }
#'
#' @examples
#' \dontrun{
#' # For single stock
#' data(ple4)
#' ts1 <- tseries(ple4)
#'
#' # For multiple stocks
#' stocks <- FLStocks(stock1=ple4, stock2=ple4)
#' ts2 <- tseries(stocks)
#' }
#'
#' @export
setGeneric("tseries", function(object, ...) standardGeneric("tseries"))

#' @rdname tseries
#' @export
setMethod("tseries", signature(object="FLStock"),
          function(object,flqs=list(catch=function(object) catch(object),
                                    eb   =function(object) ebiomass(object),
                                    ssb  =function(object) ssb(object),
                                    f    =function(object) fbar(object),
                                    h    =function(object) catch(object)/ebiomass(object),
                                    m    =function(object) FLCore::FLQuant(plyr::aaply(FLCore::m(object)[FLCore::ac(FLCore::range(object)["minfbar"]:FLCore::range(object)["maxfbar"])],2,mean),
                                                                   dimnames=dimnames(FLCore::fbar(object))))){
            model.frame(FLCore::metrics(object,flqs),drop=TRUE)})

#' @rdname tseries
#' @export
setMethod("tseries", signature(object="FLStocks"),
          function(object,flqs=list(catch=function(object) catch(object),
                                    eb   =function(object) ebiomass(object),
                                    ssb  =function(object) ssb(object),
                                    f    =function(object) fbar(object),
                                    h    =function(object) catch(object)/ebiomass(object),
                                    m    =function(object) FLCore::FLQuant(plyr::aaply(FLCore::m(object)[FLCore::ac(FLCore::range(object)["minfbar"]:FLCore::range(object)["maxfbar"])],2,mean),
                                                                   dimnames=dimnames(FLCore::fbar(object))))){
            result=lapply(object, tseries, flqs=flqs)
            rtn=do.call(rbind, result)
            rtn=cbind(.id=gsub("\\.([^.]+)$","",dimnames(rtn)[[1]]),rtn)
            rtn})

#' @title tseries
#' 
#' @description Calculates the surplus production and expected yield etc for the estinates of SSB and biomass
#'
#' @param object an \code{FLBRP} object 
#' @param seasons a numeric with seasons
#' 
#' @aliases
#' 
#' @return \code{FLQuants} object
#'
#' @seealso \code{\link{expand}}
#'
#' @export tseries
#' @docType methods
#' @rdname tseries
#'
#' 
#' @examples
#' \dontrun{
#' }

setMethod("tseries", signature(object="FLBRP"), function(object){
  ebiomass.obs<-function(x) attributes(x)$eb.obs
  
  nms=dimnames(refpts(object))
  nms$refpt=paste("ssb",dimnames(ssb.obs(object))$year,sep="")
  
  vrgn=refpts(object)["virgin","ssb"]
  
  discards.obs(object)[is.na(discards.obs(object))]=0
  
  landings.sel(object)=landings.sel(object)+discards.sel(object)
  discards.sel(object)[]=0
  
  rfs=FLCore::FLPar(array(NA,plyr::laply(nms,length),dimnames=nms))
  rfs[,"ssb",]=ssb.obs(object)
  refpts(object)=rfs
  rtn=FLCore::refptsEB(object)
  fbar(object)=FLCore::FLQuant(c(rtn[,"harvest"]))
  
  rtn=plyr::alply(rtn,2,FLCore::FLQuant,dimnames=dimnames(FLCore::ssb.obs(object)))
  names(rtn)=as.character(unlist(attributes(rtn)$split_labels))
  
  rtn$eb =ebiomass.obs(object)
  
  rtn$spSSB=ssb.obs(object)[,-1]-ssb.obs(object)[,-dim(ssb.obs(object))[2]]+catch.obs(object)[,-dim(ssb.obs(object))[2]]
  rtn$spEB =ebiomass.obs(object)[,-1]-ebiomass.obs(object)[,-dim(ebiomass.obs(object))[2]]+catch.obs(object)[,-dim(ebiomass.obs(object))[2]]
  
  rtn=FLCore::mcf(methods::as(rtn,"FLQuants"))
  
  ord=c("harvest","yield","rec","ssb","eb","revenue","cost","profit","spSSB","spEB")
  
  rtn=rtn[ord]
  rtn[["peSSB"]]=rtn$spSSB-rtn$yield
  rtn[["peSSB"]]=rtn[["peSSB"]]%/%rtn[["ssb"]]
  rtn[["peEB"]] =rtn$spEB -rtn$yield
  rtn[["peEB"]]=rtn[["peEB"]]%/%rtn[["eb"]]
  
  chk=ssb.obs(object)
  chk=chk%=%vrgn
  chk=chk<=ssb.obs(object)
  
  rtn[["ssb"]]=ssb.obs(object)
  
  if(any(chk)){
    for (i in names(rtn))
      rtn[[i]][chk]=NA}
  
  rtn})

setMethod("tseries", signature(object="FLBRPs"), function(object){
  plyr::ldply(object, function(x) model.frame(tseries(x)), .id=NULL)})

#'
#' @export
setGeneric("ebiomass", function(object) standardGeneric("ebiomass"))

#' @rdname ebiomass
#' @export
setMethod("ebiomass", signature(object="FLStock"),
          function(object) {
            sel   <- harvest(object)
            wt    <- catch.wt(object) %*% sel %/% fapex(sel)
            eb.wt <- qmax(wt, 0.000001)
            
            apply(eb.wt %*% stock.n(object), 2:6, sum)
          })


#' Standardize Values
#'
#' @description
#' Standardizes values by subtracting the mean and dividing by the standard deviation.
#'
#' @param x A numeric vector, matrix, array or FLQuant
#' @param na.rm Logical indicating whether to remove NA values when computing statistics
#'
#' @return An object of the same class as the input with standardized values
#'
#' @details
#' Standardization follows the formula: (x - mean(x))/sd(x)
#'
#' @examples
#' \dontrun{
#' # For numeric vector
#' x <- 1:10
#' stdz(object)
#'
#' # For FLQuant
#' data(ple4)
#' standardized_catch <- stdz(catch(ple4))
#' }
#'
#' @export
setGeneric("stdz", function(object, na.rm=TRUE, ...) standardGeneric("stdz"))

#' Calculate Exploitable Biomass
#'
#' @description
#' Calculates the exploitable biomass from an FLStock object using selectivity-weighted
#' catch weights and stock numbers.
#'
#' @param object An object of class FLStock
#'
#' @return An FLQuant object containing the exploitable biomass time series
#'
#' @details
#' Exploitable biomass is based on weighting catch weights by selectivity normalised 
#' by peak selectivity, then multiplying by stock numbers and summing across ages
#'
#' @examples
#' \dontrun{
#' data(ple4)
#' eb <- ebiomass(ple4)
#' }
#' @rdname stdz
#' @export
setMethod("stdz", signature(object="numeric"),
          function(object, na.rm=TRUE) {
            object=object-mean(  object, na.rm=na.rm)
            object/sqrt(var(object, na.rm=na.rm))
          })

#' @rdname stdz
#' @export
setMethod("stdz", signature(object="matrix"),
          function(object, na.rm=TRUE) {
            object=object-mean(object, na.rm=na.rm)
            object/sqrt(var(object, na.rm=na.rm))
          })

#' @rdname stdz
#' @export
setMethod("stdz", signature(object="array"),
          function(object, na.rm=TRUE) {
            object=object-mean(object, na.rm=na.rm)
            object/sqrt(var(object, na.rm=na.rm))
          })

#' @rdname stdz
#' @export
setMethod("stdz", signature(object="FLQuant"),
          function(object, na.rm=TRUE) {
            object=object-mean(object, na.rm=na.rm)
            object/sqrt(var(object, na.rm=na.rm))
          })




