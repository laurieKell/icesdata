library(FLCore)
library(ggplotFL)
library(icesdata)
library(patchwork)

library(pROC)
data(aSAH)
head(aSAH)

rtn1=   pROC::roc(aSAH$outcome, aSAH$s100b, levels=c("Good", "Poor"))
rtn2=FLSkill::roc2(as.numeric(aSAH$outcome=="Good")+0.5, aSAH$s100b/median(aSAH$s100b))

rtn3= FLCore::roc(FLQuant(as.numeric(aSAH$outcome=="Good")), FLQuant(aSAH$s100b))
plot(rtn3$FPR,rtn3$TPR)

auc(rtn1)


plot(1-rtn1$specificities,rtn1$sensitivities)
lines(rtn2$TPR,rtn2$FPR)



set.seed(4567)
hrate=FLQuants(llply(icesdata, function(x) rlnorm(1,catch(x)/ebiomass(x),0.025)))
ffmsy=FLQuants(llply(icesdata, function(x) fbar(x)/benchmark(x)["fmsy"]))

## state
dat=model.frame(FLQuants(response =ffmsy[[1]],
                         predictor=hrate[[1]]))
dat=transform(dat,response =response/median(response),
                  predictor=predictor/median(predictor))
FLSkill::skillScore(obs=dat$response,pred=dat$predictor)
#FLSkill::skillSummary(obs=dat$response,pred=dat$predictor)
FLSkill::skillPlot(obs=dat$response,pred=dat$predictor)

line(rtn2$TPR,rtn2$FPR)
lines(rtn2$TPR,rtn2$FPR)



best=median(dat$predictor)
p1=ggplot(dat)+
  geom_vline(aes(xintercept = 1),linetype=2,col="red")+
  geom_hline(aes(yintercept = 1),linetype=2,col="red")+
  geom_hline(aes(yintercept = best),linetype=2,col="red")+
  geom_point(aes(response,predictor))+
  geom_text(aes(label="State",x=-Inf,y=Inf),hjust=-0.1,vjust=1.0)

rcs=roc(ffmsy[[1]]<1,hrate[[1]])
auc=auc(rcs)
p2=ggplot(rcs)+
   geom_abline(aes(intercept=0,slope=1),col="red")+
   geom_path(aes(FPR,TPR))+
   geom_point(aes(FPR,TPR),data=rcs[which.min((rcs$ind-1)^2),],col="red",size=2)+
   geom_point(aes(FPR,TPR),data=rcs[which.max(rcs$TSS),],col="blue",size=2)+
   geom_text(aes(label=paste("AUC",signif(auc,2),sep="="),x=-Inf,y=Inf),hjust=-0.1,vjust=1.0)
p1+p2

## trend
dat=model.frame(FLQuants(response =FLQuant(c(diff(ffmsy[[1]]))),
                         predictor=FLQuant(c(diff(hrate[[1]])))))
dat=transform(dat,response =exp(response),
                  predictor=exp(predictor))
FLSkill::skillScore(obs=dat$response,pred=dat$predictor)
#FLSkill::skillSummary(obs=dat$response,pred=dat$predictor)
FLSkill::skillPlot(obs=dat$response,pred=dat$predictor)


p3=ggplot(dat)+
  geom_vline(aes(xintercept = 1),linetype=2,col="red")+
  geom_hline(aes(yintercept = 1),linetype=2,col="red")+
  geom_hline(aes(yintercept = median(dat$predictor)),linetype=2,col="blue")+
  geom_point(aes(response,predictor))+
  geom_text(aes(label="Trend",x=-Inf,y=Inf),hjust=-0.1,vjust=1.0)

rcs=roc(exp(FLQuant(c(diff(ffmsy[[1]]))))<1,exp(FLQuant(c(diff(hrate[[1]])))))
auc2=auc(rcs)
p4=ggplot(rcs)+
  geom_abline(aes(intercept=0,slope=1),col="red")+
  geom_path(aes(FPR,TPR))+
  geom_point(aes(FPR,TPR),data=rcs[which.min((rcs$ind-1)^2),],col="red",size=2)+
  geom_point(aes(FPR,TPR),data=rcs[which.max(rcs$TSS),],col="blue",size=2)+
  geom_text(aes(label=paste("AUC",signif(auc2,2),sep="="),x=-Inf,y=Inf),hjust=-0.1,vjust=1.0)
p3+p4
  







