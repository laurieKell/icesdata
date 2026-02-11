# Note: Generic for tseries is defined in generic.R
# This file contains only method definitions

#' @rdname tseries
#' @export
setMethod("tseries", signature(object="FLStock"),
          function(object,flqs=list(catch=function(object) catch(object),
                                    eb   =function(object) ebiomass(object),
                                    ssb  =function(object) ssb(object),
                                    f    =function(object) fbar(object),
                                    h    =function(object) catch(object)/ebiomass(object),
                                    m    =function(object) FLQuant(plyr::aaply(m(object)[FLCore::ac(range(object)["minfbar"]:range(object)["maxfbar"])],2,mean),
                                                                   dimnames=dimnames(fbar(object))))){
            model.frame(FLCore::metrics(object,flqs),drop=TRUE)})

#' @rdname tseries
#' @export
setMethod("tseries", signature(object="FLStocks"),
          function(object,flqs=list(catch=function(object) catch(object),
                                    eb   =function(object) ebiomass(object),
                                    ssb  =function(object) ssb(object),
                                    f    =function(object) fbar(object),
                                    h    =function(object) catch(object)/ebiomass(object),
                                    m    =function(object) FLCore::FLQuant(plyr::aaply(FLCore::m(object)[FLCore::ac(range(object)["minfbar"]:range(object)["maxfbar"])],2,mean),
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

# Note: Generics for ebiomass and stdz are defined in generic.R
# This file contains only method definitions for tseries

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




