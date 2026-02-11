library(FLCore)
library(ggplotFL)
library(icesdata)
library(plyr)
library(FLSkill)

library(statcomp)
library(patchwork)

data(icesdata)

load("C:/active/flrpapers/haf/risk/data/lsmps.RData")

set.seed(4567)

bbmsy=FLQuants(llply(icesdata, function(x) ssb( x)/benchmark(x)["btrigger"]))
ffmsy=FLQuants(llply(icesdata, function(x) fbar(x)/benchmark(x)["fmsy"]))

survey=FLQuants(llply(icesdata, function(x) rlnorm(1,log(tsb(x[-1])/mean(tsb(x[-1]))),0.2)))
hrate =FLQuants(llply(icesdata, function(x) rlnorm(1,log(catch(x)/ebiomass(x)),       0.2)))
lmn   =llply(lsmps, function(x) tryIt(lmean(x)))

id=35
plot(mcf(FLQuants(reponse=ffmsy[[id]],predictor=hrate[[id]],lmn=1/lmn[[id]])))

## state
dat=model.frame(FLQuants(response =ffmsy[[id]],
                         predictor=1/lmn[[id]]))
dat=transform(dat,response =response/median(response),
                  predictor=predictor/median(predictor))
skillScore(response=dat$response,predictor=dat$predictor)
skillSummary(response=dat$response,predictor=dat$predictor)
skillPlot(response=dat$response,predictor=dat$predictor)


best=median(dat$predictor)
p1=ggplot(dat)+
  geom_vline(aes(xintercept = 1),linetype=2,col="red")+
  geom_hline(aes(yintercept = 1),linetype=2,col="red")+
  geom_hline(aes(yintercept = best),linetype=2,col="red")+
  geom_point(aes(response,predictor))+
  geom_text(aes(label="State",x=-Inf,y=Inf),hjust=-0.1,vjust=1.0)

rcs=roc(ffmsy[[id]]<1,1/lmn[[id]])
auc=auc(rcs)
p2=ggplot(rcs)+
   geom_abline(aes(intercept=0,slope=1),col="red")+
   geom_path(aes(FPR,TPR))+
   geom_point(aes(FPR,TPR),data=rcs[which.min((rcs$ind-1)^2),],col="red",size=2)+
   geom_point(aes(FPR,TPR),data=rcs[which.max(rcs$TSS),],col="blue",size=2)+
   geom_text(aes(label=paste("AUC",signif(auc,2),sep="="),x=-Inf,y=Inf),hjust=-0.1,vjust=1.0)
p1+p2



## trend
dat=model.frame(FLQuants(response =FLQuant(c(diff(ffmsy[[id]]))),
                         predictor=FLQuant(c(diff(1/lmn[[id]])))))
dat=transform(dat,response =exp(response),
                  predictor=exp(predictor))
skillScore(response=dat$response,predictor=dat$predictor)
skillSummary(response=dat$response,predictor=dat$predictor)
skillPlot(response=dat$response,predictor=dat$predictor)


p3=ggplot(dat)+
  geom_vline(aes(xintercept = 1),linetype=2,col="red")+
  geom_hline(aes(yintercept = 1),linetype=2,col="red")+
  geom_hline(aes(yintercept = median(dat$predictor)),linetype=2,col="blue")+
  geom_point(aes(response,predictor))+
  geom_text(aes(label="Trend",x=-Inf,y=Inf),hjust=-0.1,vjust=1.0)

rcs=roc(exp(FLQuant(c(diff(ffmsy[[id]]))))>1,exp(FLQuant(c(diff(1/lmn[[id]])))))
auc2=auc(rcs)
p4=ggplot(rcs)+
  geom_abline(aes(intercept=0,slope=1),col="red")+
  geom_path(aes(FPR,TPR))+
  geom_point(aes(FPR,TPR),data=rcs[which.min((rcs$ind-1)^2),],col="red",size=2)+
  geom_point(aes(FPR,TPR),data=rcs[which.max(rcs$TSS),],col="blue",size=2)+
  geom_text(aes(label=paste("AUC",signif(auc2,2),sep="="),x=-Inf,y=Inf),hjust=-0.1,vjust=1.0)
p3+p4
  

ids=names(bbmsy)

## State
bstate=mdply(ids, function(id) 
  cbind(.id=id,model.frame(FLQuants(response=bbmsy[[id]],predictor=survey[[id]]),drop=TRUE)))[,-1]
bstate=ddply(bstate,.(.id), with, tryIt(skillSummary(response=response,predictor=predictor/mean(predictor))))

fstate=mdply(ids, function(id) 
  cbind(.id=id,model.frame(FLQuants(response=ffmsy[[id]],predictor=hrate[[id]]),drop=TRUE)))[,-1]
fstate=ddply(fstate,.(.id), with, tryIt(skillSummary(response=response,predictor=predictor/mean(predictor))))

lstate=mdply(names(lmn[laply(lmn, is.FLQuant)]), function(id) 
  cbind(.id=id,model.frame(mcf(FLQuants(response=ffmsy[[id]],predictor=1/lmn[[id]])),drop=TRUE)))[,-1]
lstate=ddply(lstate,.(.id), with, tryIt(skillSummary(response=response,predictor=predictor/mean(predictor))))

state=rbind.fill(cbind(Indicator="Survey",                 bstate),
                 cbind(Indicator="Relative\nHarvest Rate", fstate),
                 cbind(Indicator="Length-Based\nIndicator",lstate))

ggplot(state)+geom_histogram(aes(AUC))+facet_grid(Indicator~.)+ylab("")

## Trend
btrnd=mdply(ids, function(id) {
  tryIt(cbind(.id=id,model.frame(FLQuants(response =trend(bbmsy[[id]]/mean(bbmsy[[id]])),
                                          predictor=trend(survey[[id]]/mean(survey[[id]]))),drop=TRUE)))})[,-1]
btrnd=ddply(btrnd,.(.id), with, tryIt(skillSummary(response=exp(response),predictor=exp(predictor))))

ftrnd=mdply(ids, function(id) 
  tryIt(cbind(.id=id,model.frame(FLQuants(response=trend(ffmsy[[id]]/mean(ffmsy[[id]])),
                                          predictor=trend(hrate[[id]]/mean(hrate[[id]]))),drop=TRUE))))[,-1]
ftrnd=ddply(ftrnd,.(.id), with, tryIt(skillSummary(response=exp(response),predictor=exp(predictor))))

ltrnd=mdply(names(lmn[laply(lmn, is.FLQuant)]), function(id) 
  tryIt(cbind(.id=id,model.frame(mcf(FLQuants(response=trend(ffmsy[[id]]/mean(ffmsy[[id]])),
                                              predictor=trend(1/lmn[[id]]*mean(lmn[[id]])))),drop=TRUE))))[,-1]
ltrnd=ddply(ltrnd,.(.id), with, tryIt(skillSummary(response=exp(response),predictor=exp(predictor))))

trnd=rbind.fill(cbind(Indicator="Survey",                 btrnd),
                cbind(Indicator="Relative\nHarvest Rate", ftrnd),
                cbind(Indicator="Length-Based\nIndicator",ltrnd))

ggplot(trnd)+geom_histogram(aes(AUC))+facet_grid(Indicator~.)+ylab("")

ent=ldply(bbmsy, function(x) tryIt(data.frame(entropy=permutation_entropy(ordinal_pattern_distribution(x=x, ndemb=5)))))
