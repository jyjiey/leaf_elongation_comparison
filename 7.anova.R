require(MASS)
require(ggplot2)
require(lme4)
library(lmerTest)
library(MuMIn)
library("emmeans")

library(ggpubr)
##get distribution of data
theme_set(
  theme_bw()
)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
    library(plyr)

    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
    }

    # This does the summary. For each group's data frame, return a vector with
    # N, mean, and sd
    datac <- ddply(data, groupvars, .drop=.drop,
      .fun = function(xx, col) {
        c(N    = length2(xx[[col]], na.rm=na.rm),
          mean = mean   (xx[[col]], na.rm=na.rm),
          sd   = sd     (xx[[col]], na.rm=na.rm)
        )
      },
      measurevar
    )

    # Rename the "mean" column    
    datac <- rename(datac, c("mean" = measurevar))

    datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval: 
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult

    return(datac)
}

###distribution check
fitData <- function(data, fit="gamma", sample=0.5){
 distrib = list()
 numfit <- length(fit)
 results = matrix(0, ncol=5, nrow=numfit)

 for(i in 1:numfit){
if((fit[i] == "gamma") | 
     (fit[i] == "poisson") | 
     (fit[i] == "weibull") | 
     (fit[i] == "exponential") |
     (fit[i] == "logistic") |
     (fit[i] == "normal") | 
     (fit[i] == "geometric")
) 
  distrib[[i]] = fit[i]
else stop("Provide a valid distribution to fit data" )
 }

 # take a sample of dataset
 n = round(length(data)*sample)
 data = sample(data, size=n, replace=F)

 for(i in 1:numfit) {
  if(distrib[[i]] == "gamma") {
  gf_shape = "gamma"
  fd_g <- fitdistr(data, "gamma")
  est_shape = fd_g$estimate[[1]]
  est_rate = fd_g$estimate[[2]]

  ks = ks.test(data, "pgamma", shape=est_shape, rate=est_rate)

  # add to results
  results[i,] = c(gf_shape, est_shape, est_rate, ks$statistic, ks$p.value)
}

else if(distrib[[i]] == "poisson"){
  gf_shape = "poisson"
  fd_p <- fitdistr(data, "poisson")
  est_lambda = fd_p$estimate[[1]]

  ks = ks.test(data, "ppois", lambda=est_lambda)
  # add to results
  results[i,] = c(gf_shape, est_lambda, "NA", ks$statistic, ks$p.value)

}

else if(distrib[[i]] == "weibull"){
  gf_shape = "weibull"
  fd_w <- fitdistr(data,densfun=dweibull,start=list(scale=1,shape=2))
  est_shape = fd_w$estimate[[1]]
  est_scale = fd_w$estimate[[2]]

  ks = ks.test(data, "pweibull", shape=est_shape, scale=est_scale)
  # add to results
  results[i,] = c(gf_shape, est_shape, est_scale, ks$statistic, ks$p.value) 
}

else if(distrib[[i]] == "normal"){
  gf_shape = "normal"
  fd_n <- fitdistr(data, "normal")
  est_mean = fd_n$estimate[[1]]
  est_sd = fd_n$estimate[[2]]

  ks = ks.test(data, "pnorm", mean=est_mean, sd=est_sd)
  # add to results
  results[i,] = c(gf_shape, est_mean, est_sd, ks$statistic, ks$p.value)
}

else if(distrib[[i]] == "exponential"){
  gf_shape = "exponential"
  fd_e <- fitdistr(data, "exponential")
  est_rate = fd_e$estimate[[1]]
  ks = ks.test(data, "pexp", rate=est_rate)
  # add to results
  results[i,] = c(gf_shape, est_rate, "NA", ks$statistic, ks$p.value)
}

else if(distrib[[i]] == "logistic"){
  gf_shape = "logistic"
  fd_l <- fitdistr(data, "logistic")
  est_location = fd_l$estimate[[1]]
  est_scale = fd_l$estimate[[2]]
  ks = ks.test(data, "plogis", location=est_location, scale=est_scale)
  # add to results
  results[i,] = c(gf_shape, est_location, est_scale, ks$statistic,    ks$p.value) 
    }
  }
  results = rbind(c("distribution", "param1", "param2", "ks stat", "ks    pvalue"),   results)
  #print(results)
  return(results)
  }
   
bigtable<-NULL
 #microscopy<-read.csv("~/Downloads/microscopy_summary_6_28.csv", header=T)
#elongation <-read.csv("~/Downloads/elongation_summary_python1_out.csv", header=T)
#elongation <-read.csv("~/Downloads/elongations_test_out2.csv", header=T)
#info <-read.csv("~/Downloads/elongation_summary_python2_6_281.csv", header=T)
microscopy<-read.csv("~/Downloads/microscopy_summary_6_28.csv", header=T)
elongation <-read.csv("~/Downloads/elongation_summary_python1_out.csv", header=T)
info <-read.csv("~/Downloads/elongation_summary_python2_6_28.csv", header=T)

info<-as.data.frame(t(info))
colnames(info)<-info[1,]
info<-info[-1,]
head(elongation)
Netphotosynthesisrate3<-cbind(info[,which(colnames(info)%in%c("crop", "domestication", "accession","treatment","batch"))],elongation $lf)
Netphotosynthesisrate3$A<-Netphotosynthesisrate3[,6]
#3.Netphotosynthesisrate3$A<-c(-0.1*Drydown_brachy $hydraulic.potential.1)
#4.Netphotosynthesisrate3$A<-Drydown_brachy $A
#5.Netphotosynthesisrate3$A<-Drydown_brachy $gsw
#6.Netphotosynthesisrate3$A<-(Drydown_brachy $fresh.shoot+Drydown_brachy $fresh.root)
#7.Netphotosynthesisrate3$A<-(Drydown_brachy $dry.shoot+Drydown_brachy $dry.root)
#8.Netphotosynthesisrate3$A<-(Drydown_brachy $dry.root/Drydown_brachy $dry.shoot)
#9.Netphotosynthesisrate3$A<-Drydown_brachy $glucose.nmol.Glc.equivalents.mg
Netphotosynthesisrate3 <-na.omit(Netphotosynthesisrate3)
res = fitData(na.omit(Netphotosynthesisrate3$A), fit=c("gamma","logistic","normal","exponential","poisson","exponential"),
    sample=1)
res
###
Netphotosynthesisrate3 $Netphotosynthesisrate<-as.numeric(na.omit(Netphotosynthesisrate3 $A))
Netphotosynthesisrate3 $domestication<-as.factor(Netphotosynthesisrate3 $domestication)
Netphotosynthesisrate3 $crop <-as.factor(Netphotosynthesisrate3 $crop)
Netphotosynthesisrate3 $Treatment <-as.factor(Netphotosynthesisrate3 $treatment)
Netphotosynthesisrate3 $batch <-as.factor(Netphotosynthesisrate3 $batch)
#Netphotosynthesisrate3<-Netphotosynthesisrate3[-which(Netphotosynthesisrate3$accession%in%c("svevo","Vavilovii")),]
options(na.action = "na.fail")
a<-lmer(A ~ crop+Treatment+ domestication+(1|accession), data= Netphotosynthesisrate3)
b<-lmer(A ~ crop+Treatment* domestication+(1|accession), data= Netphotosynthesisrate3)
c<-lmer(A ~ crop*Treatment+ domestication+(1|accession), data= Netphotosynthesisrate3)
d<-lmer(A ~ crop* domestication+Treatment+(1|accession), data= Netphotosynthesisrate3)
e<-lmer(A ~ crop*Treatment+ domestication+(1|accession), data= Netphotosynthesisrate3)
f<-lmer(A ~ crop+(1|accession), data= Netphotosynthesisrate3)
g<-lmer(A ~ domestication+(1|accession), data= Netphotosynthesisrate3)
h<-lmer(A ~  Treatment +(1|accession), data= Netphotosynthesisrate3)
i<-lmer(A ~  crop+domestication+(1|accession), data= Netphotosynthesisrate3)
j<-lmer(A ~  crop+ Treatment +(1|accession), data= Netphotosynthesisrate3)
k<-lmer(A ~  Treatment +domestication+(1|accession), data= Netphotosynthesisrate3)
l<-lmer(A ~  crop*Treatment* domestication+(1|accession)+(1|batch), data= Netphotosynthesisrate3)
m<-lm(A ~  crop*Treatment* domestication, data= Netphotosynthesisrate3)

AIC(a,b,c,d,e,f,g,h,i,j,k,l,m)[order(AIC(a,b,c,d,e,f,g,h,i,j,k,l,m)[,2]),]


Netphotosynthesisrate3 $crop <- relevel(Netphotosynthesisrate3 $crop, ref = "barley")
Netphotosynthesisrate3 $crop <- factor(Netphotosynthesisrate3 $crop, levels = c("barley","bd21","oat","wheat"))

Netphotosynthesisrate3 $Treatment <- relevel(Netphotosynthesisrate3 $Treatment, ref = "c")
Netphotosynthesisrate3 $Treatment <- factor(Netphotosynthesisrate3 $Treatment, levels = c("c","d"))


Netphotosynthesisrate3 $domestication <- relevel(Netphotosynthesisrate3 $domestication, ref = "W")
Netphotosynthesisrate3 $domestication <- factor(Netphotosynthesisrate3 $domestication, levels = c("W","D"))

mean_A <- aggregate(A ~ crop+Treatment+domestication+ batch+ accession, data = Netphotosynthesisrate3, FUN = mean)
# View the result
print(mean_A)
#Netphotosynthesisrate3<-Netphotosynthesisrate3[-which(Netphotosynthesisrate3 $crop=="bd21"),]
l<-lmer(A ~  crop*Treatment* domestication+(1|batch)+(1|accession), data= Netphotosynthesisrate3)
a0<-as.matrix(car::Anova(l))[,3]

Netphotosynthesisrate31<-Netphotosynthesisrate3[-which(Netphotosynthesisrate3 $crop=="bd21"),]
l<-lmer(A ~  crop*Treatment* domestication+(1|batch)+(1|accession), data= Netphotosynthesisrate31)
car::Anova(l)
library(emmeans)
a1<-as.data.frame(emmeans(l, "Treatment"))[,2]
names(a1)<-as.data.frame(emmeans(l, "Treatment"))[,1]
a2<-as.data.frame(emmeans(l, "domestication"))[,2]
names(a2)<-as.data.frame(emmeans(l, "domestication"))[,1]
l<-lmer(A ~  crop+(1|batch)+(1|accession) +(1| domestication)+(1| Treatment), data= Netphotosynthesisrate3)
a3<-as.data.frame(emmeans(l, "crop"))[,2]
names(a3)<-as.data.frame(emmeans(l, "crop"))[,1]
bigtable<-cbind(bigtable,c(a0,a1,a2,a3))
rownames(bigtable)[8:9]<-c("ww","wd")
 #microscopy<-read.csv("~/Downloads/microscopy_summary_6_28.csv", header=T)
#elongation <-read.csv("~/Downloads/elongation_summary_python1_out.csv", header=T)
#elongation <-read.csv("~/Downloads/elongations_test_out2.csv", header=T)
#info <-read.csv("~/Downloads/elongation_summary_python2_6_281.csv", header=T)
microscopy<-read.csv("~/Downloads/microscopy_summary_6_28.csv", header=T)
elongation <-read.csv("~/Downloads/elongation_summary_python1_out.csv", header=T)
info <-read.csv("~/Downloads/elongation_summary_python2_6_28.csv", header=T)

info<-as.data.frame(t(info))
colnames(info)<-info[1,]
info<-info[-1,]
head(elongation)
Netphotosynthesisrate3<-cbind(info[,which(colnames(info)%in%c("crop", "domestication", "accession","treatment","batch"))],elongation $l10_90)
Netphotosynthesisrate3$A<-Netphotosynthesisrate3[,6]
#3.Netphotosynthesisrate3$A<-c(-0.1*Drydown_brachy $hydraulic.potential.1)
#4.Netphotosynthesisrate3$A<-Drydown_brachy $A
#5.Netphotosynthesisrate3$A<-Drydown_brachy $gsw
#6.Netphotosynthesisrate3$A<-(Drydown_brachy $fresh.shoot+Drydown_brachy $fresh.root)
#7.Netphotosynthesisrate3$A<-(Drydown_brachy $dry.shoot+Drydown_brachy $dry.root)
#8.Netphotosynthesisrate3$A<-(Drydown_brachy $dry.root/Drydown_brachy $dry.shoot)
#9.Netphotosynthesisrate3$A<-Drydown_brachy $glucose.nmol.Glc.equivalents.mg
Netphotosynthesisrate3 <-na.omit(Netphotosynthesisrate3)
res = fitData(na.omit(Netphotosynthesisrate3$A), fit=c("gamma","logistic","normal","exponential","poisson","exponential"),
    sample=1)
res
###
Netphotosynthesisrate3 $Netphotosynthesisrate<-as.numeric(na.omit(Netphotosynthesisrate3 $A))
Netphotosynthesisrate3 $domestication<-as.factor(Netphotosynthesisrate3 $domestication)
Netphotosynthesisrate3 $crop <-as.factor(Netphotosynthesisrate3 $crop)
Netphotosynthesisrate3 $Treatment <-as.factor(Netphotosynthesisrate3 $treatment)
Netphotosynthesisrate3 $batch <-as.factor(Netphotosynthesisrate3 $batch)
#Netphotosynthesisrate3<-Netphotosynthesisrate3[-which(Netphotosynthesisrate3$accession%in%c("svevo","Vavilovii")),]
options(na.action = "na.fail")
a<-lmer(A ~ crop+Treatment+ domestication+(1|accession), data= Netphotosynthesisrate3)
b<-lmer(A ~ crop+Treatment* domestication+(1|accession), data= Netphotosynthesisrate3)
c<-lmer(A ~ crop*Treatment+ domestication+(1|accession), data= Netphotosynthesisrate3)
d<-lmer(A ~ crop* domestication+Treatment+(1|accession), data= Netphotosynthesisrate3)
e<-lmer(A ~ crop*Treatment+ domestication+(1|accession), data= Netphotosynthesisrate3)
f<-lmer(A ~ crop+(1|accession), data= Netphotosynthesisrate3)
g<-lmer(A ~ domestication+(1|accession), data= Netphotosynthesisrate3)
h<-lmer(A ~  Treatment +(1|accession), data= Netphotosynthesisrate3)
i<-lmer(A ~  crop+domestication+(1|accession), data= Netphotosynthesisrate3)
j<-lmer(A ~  crop+ Treatment +(1|accession), data= Netphotosynthesisrate3)
k<-lmer(A ~  Treatment +domestication+(1|accession), data= Netphotosynthesisrate3)
l<-lmer(A ~  crop*Treatment* domestication+(1|accession)+(1|batch), data= Netphotosynthesisrate3)
m<-lm(A ~  crop*Treatment* domestication, data= Netphotosynthesisrate3)

AIC(a,b,c,d,e,f,g,h,i,j,k,l,m)[order(AIC(a,b,c,d,e,f,g,h,i,j,k,l,m)[,2]),]


Netphotosynthesisrate3 $crop <- relevel(Netphotosynthesisrate3 $crop, ref = "barley")
Netphotosynthesisrate3 $crop <- factor(Netphotosynthesisrate3 $crop, levels = c("barley","bd21","oat","wheat"))

Netphotosynthesisrate3 $Treatment <- relevel(Netphotosynthesisrate3 $Treatment, ref = "c")
Netphotosynthesisrate3 $Treatment <- factor(Netphotosynthesisrate3 $Treatment, levels = c("c","d"))


Netphotosynthesisrate3 $domestication <- relevel(Netphotosynthesisrate3 $domestication, ref = "W")
Netphotosynthesisrate3 $domestication <- factor(Netphotosynthesisrate3 $domestication, levels = c("W","D"))

mean_A <- aggregate(A ~ crop+Treatment+domestication+ batch+ accession, data = Netphotosynthesisrate3, FUN = mean)
# View the result
print(mean_A)
#Netphotosynthesisrate3<-Netphotosynthesisrate3[-which(Netphotosynthesisrate3 $crop=="bd21"),]
l<-lmer(A ~  crop*Treatment* domestication+(1|batch)+(1|accession), data= Netphotosynthesisrate3)
a0<-as.matrix(car::Anova(l))[,3]

Netphotosynthesisrate31<-Netphotosynthesisrate3[-which(Netphotosynthesisrate3 $crop=="bd21"),]
l<-lmer(A ~  crop*Treatment* domestication+(1|batch)+(1|accession), data= Netphotosynthesisrate31)
car::Anova(l)
library(emmeans)
a1<-as.data.frame(emmeans(l, "Treatment"))[,2]
names(a1)<-as.data.frame(emmeans(l, "Treatment"))[,1]
a2<-as.data.frame(emmeans(l, "domestication"))[,2]
names(a2)<-as.data.frame(emmeans(l, "domestication"))[,1]
l<-lmer(A ~  crop+(1|batch)+(1|accession) +(1| domestication)+(1| Treatment), data= Netphotosynthesisrate3)
a3<-as.data.frame(emmeans(l, "crop"))[,2]
names(a3)<-as.data.frame(emmeans(l, "crop"))[,1]
bigtable<-cbind(bigtable,c(a0,a1,a2,a3))



 #microscopy<-read.csv("~/Downloads/microscopy_summary_6_28.csv", header=T)
#elongation <-read.csv("~/Downloads/elongation_summary_python1_out.csv", header=T)
#elongation <-read.csv("~/Downloads/elongations_test_out2.csv", header=T)
#info <-read.csv("~/Downloads/elongation_summary_python2_6_281.csv", header=T)

microscopy<-read.csv("~/Downloads/microscopy_summary_6_28.csv", header=T)
elongation <-read.csv("~/Downloads/elongation_summary_python1_out.csv", header=T)
info <-read.csv("~/Downloads/elongation_summary_python2_6_28.csv", header=T)


info<-as.data.frame(t(info))
colnames(info)<-info[1,]
info<-info[-1,]
head(elongation)
Netphotosynthesisrate3<-cbind(info[,which(colnames(info)%in%c("crop", "domestication", "accession","treatment","batch"))],elongation $der)
Netphotosynthesisrate3$A<-Netphotosynthesisrate3[,6]
#3.Netphotosynthesisrate3$A<-c(-0.1*Drydown_brachy $hydraulic.potential.1)
#4.Netphotosynthesisrate3$A<-Drydown_brachy $A
#5.Netphotosynthesisrate3$A<-Drydown_brachy $gsw
#6.Netphotosynthesisrate3$A<-(Drydown_brachy $fresh.shoot+Drydown_brachy $fresh.root)
#7.Netphotosynthesisrate3$A<-(Drydown_brachy $dry.shoot+Drydown_brachy $dry.root)
#8.Netphotosynthesisrate3$A<-(Drydown_brachy $dry.root/Drydown_brachy $dry.shoot)
#9.Netphotosynthesisrate3$A<-Drydown_brachy $glucose.nmol.Glc.equivalents.mg
Netphotosynthesisrate3 <-na.omit(Netphotosynthesisrate3)
res = fitData(na.omit(Netphotosynthesisrate3$A), fit=c("gamma","logistic","normal","exponential","poisson","exponential"),
    sample=1)
res
###
Netphotosynthesisrate3 $Netphotosynthesisrate<-as.numeric(na.omit(Netphotosynthesisrate3 $A))
Netphotosynthesisrate3 $domestication<-as.factor(Netphotosynthesisrate3 $domestication)
Netphotosynthesisrate3 $crop <-as.factor(Netphotosynthesisrate3 $crop)
Netphotosynthesisrate3 $Treatment <-as.factor(Netphotosynthesisrate3 $treatment)
Netphotosynthesisrate3 $batch <-as.factor(Netphotosynthesisrate3 $batch)
#Netphotosynthesisrate3<-Netphotosynthesisrate3[-which(Netphotosynthesisrate3$accession%in%c("svevo","Vavilovii")),]
options(na.action = "na.fail")
a<-lmer(A ~ crop+Treatment+ domestication+(1|accession), data= Netphotosynthesisrate3)
b<-lmer(A ~ crop+Treatment* domestication+(1|accession), data= Netphotosynthesisrate3)
c<-lmer(A ~ crop*Treatment+ domestication+(1|accession), data= Netphotosynthesisrate3)
d<-lmer(A ~ crop* domestication+Treatment+(1|accession), data= Netphotosynthesisrate3)
e<-lmer(A ~ crop*Treatment+ domestication+(1|accession), data= Netphotosynthesisrate3)
f<-lmer(A ~ crop+(1|accession), data= Netphotosynthesisrate3)
g<-lmer(A ~ domestication+(1|accession), data= Netphotosynthesisrate3)
h<-lmer(A ~  Treatment +(1|accession), data= Netphotosynthesisrate3)
i<-lmer(A ~  crop+domestication+(1|accession), data= Netphotosynthesisrate3)
j<-lmer(A ~  crop+ Treatment +(1|accession), data= Netphotosynthesisrate3)
k<-lmer(A ~  Treatment +domestication+(1|accession), data= Netphotosynthesisrate3)
l<-lmer(A ~  crop*Treatment* domestication+(1|accession)+(1|batch), data= Netphotosynthesisrate3)
m<-lm(A ~  crop*Treatment* domestication, data= Netphotosynthesisrate3)

AIC(a,b,c,d,e,f,g,h,i,j,k,l,m)[order(AIC(a,b,c,d,e,f,g,h,i,j,k,l,m)[,2]),]


Netphotosynthesisrate3 $crop <- relevel(Netphotosynthesisrate3 $crop, ref = "barley")
Netphotosynthesisrate3 $crop <- factor(Netphotosynthesisrate3 $crop, levels = c("barley","bd21","oat","wheat"))

Netphotosynthesisrate3 $Treatment <- relevel(Netphotosynthesisrate3 $Treatment, ref = "c")
Netphotosynthesisrate3 $Treatment <- factor(Netphotosynthesisrate3 $Treatment, levels = c("c","d"))


Netphotosynthesisrate3 $domestication <- relevel(Netphotosynthesisrate3 $domestication, ref = "W")
Netphotosynthesisrate3 $domestication <- factor(Netphotosynthesisrate3 $domestication, levels = c("W","D"))

mean_A <- aggregate(A ~ crop+Treatment+domestication+ batch+ accession, data = Netphotosynthesisrate3, FUN = mean)
# View the result
print(mean_A)
#Netphotosynthesisrate3<-Netphotosynthesisrate3[-which(Netphotosynthesisrate3 $crop=="bd21"),]
l<-lmer(A ~  crop*Treatment* domestication+(1|batch)+(1|accession), data= Netphotosynthesisrate3)
a0<-as.matrix(car::Anova(l))[,3]

Netphotosynthesisrate31<-Netphotosynthesisrate3[-which(Netphotosynthesisrate3 $crop=="bd21"),]
l<-lmer(A ~  crop*Treatment* domestication+(1|batch)+(1|accession), data= Netphotosynthesisrate31)
car::Anova(l)
library(emmeans)
a1<-as.data.frame(emmeans(l, "Treatment"))[,2]
names(a1)<-as.data.frame(emmeans(l, "Treatment"))[,1]
a2<-as.data.frame(emmeans(l, "domestication"))[,2]
names(a2)<-as.data.frame(emmeans(l, "domestication"))[,1]
l<-lmer(A ~  crop+(1|batch)+(1|accession) +(1| domestication)+(1| Treatment), data= Netphotosynthesisrate3)
a3<-as.data.frame(emmeans(l, "crop"))[,2]
names(a3)<-as.data.frame(emmeans(l, "crop"))[,1]
bigtable<-cbind(bigtable,c(a0,a1,a2,a3))



Netphotosynthesisrate3<-NULL
 microscopy<-read.csv("~/Desktop/chapter3final/microscopy_summary.csv", header=T) 
 elongation <-read.csv("~/Desktop/chapter3final/elongation_summary_models.csv", header=T) 
 info <-read.csv("~/Desktop/chapter3final/elongation_summary_info.csv", header=T)
 
 info<-as.data.frame(t(info))
colnames(info)<-info[1,]
info<-info[-1,]
info<-info[,-5]
colnames(info)[5]<-"accession"
head(microscopy)
Netphotosynthesisrate3<-microscopy[,which(colnames(microscopy)%in%c("domestication", "accession", "crop","treatment", "divisionzonelist"))]
Netphotosynthesisrate3$A<-microscopy $lf
#3.Netphotosynthesisrate3$A<-c(-0.1*Drydown_brachy $hydraulic.potential.1)
#4.Netphotosynthesisrate3$A<-Drydown_brachy $A
#5.Netphotosynthesisrate3$A<-Drydown_brachy $gsw
#6.Netphotosynthesisrate3$A<-(Drydown_brachy $fresh.shoot+Drydown_brachy $fresh.root)
#7.Netphotosynthesisrate3$A<-(Drydown_brachy $dry.shoot+Drydown_brachy $dry.root)
#8.Netphotosynthesisrate3$A<-(Drydown_brachy $dry.root/Drydown_brachy $dry.shoot)
#9.Netphotosynthesisrate3$A<-Drydown_brachy $glucose.nmol.Glc.equivalents.mg
Netphotosynthesisrate3 <-na.omit(Netphotosynthesisrate3)
res = fitData(na.omit(Netphotosynthesisrate3$A), fit=c("gamma","logistic","normal","exponential","poisson","exponential"),
    sample=1)
res
###
Netphotosynthesisrate3 $Netphotosynthesisrate<-as.numeric(na.omit(Netphotosynthesisrate3 $A))
Netphotosynthesisrate3 $domestication<-as.factor(Netphotosynthesisrate3 $domestication)
Netphotosynthesisrate3 $crop <-as.factor(Netphotosynthesisrate3 $crop)
Netphotosynthesisrate3 $Treatment <-as.factor(Netphotosynthesisrate3 $treatment)
Netphotosynthesisrate3 $accession <-as.factor(Netphotosynthesisrate3 $accession)
options(na.action = "na.fail")
m<-lm(A ~  crop*Treatment* domestication, data= Netphotosynthesisrate3)

dredge(m, rank="AIC",extra = c("R^2", adjRsq=function(x) summary(x)$adj.r.squared))

a<-lmer(A ~ crop+Treatment+ domestication+(1|accession), data= Netphotosynthesisrate3)
b<-lmer(A ~ crop+Treatment* domestication+(1|accession), data= Netphotosynthesisrate3)
c<-lmer(A ~ crop*Treatment+ domestication+(1|accession), data= Netphotosynthesisrate3)
d<-lmer(A ~ crop* domestication+Treatment+(1|accession), data= Netphotosynthesisrate3)
e<-lmer(A ~ crop*Treatment+ domestication+(1|accession), data= Netphotosynthesisrate3)
f<-lmer(A ~ crop+(1|accession), data= Netphotosynthesisrate3)
g<-lmer(A ~ domestication+(1|accession), data= Netphotosynthesisrate3)
h<-lmer(A ~  Treatment +(1|accession), data= Netphotosynthesisrate3)
i<-lmer(A ~  crop+domestication+(1|accession), data= Netphotosynthesisrate3)
j<-lmer(A ~  crop+ Treatment +(1|accession), data= Netphotosynthesisrate3)
k<-lmer(A ~  Treatment +domestication+(1|accession), data= Netphotosynthesisrate3)
l<-lmer(A ~  crop*Treatment* domestication+(1|accession), data= Netphotosynthesisrate3)
m<-lm(A ~  crop*Treatment* domestication, data= Netphotosynthesisrate3)
m<-lm(A ~  crop*Treatment+domestication, data= Netphotosynthesisrate3)

AIC(a,b,c,d,e,f,g,h,i,j,k,l,m)[order(AIC(a,b,c,d,e,f,g,h,i,j,k,l,m)[,2]),]
car::Anova(l)
summary(l)
Netphotosynthesisrate3 $crop <- relevel(Netphotosynthesisrate3 $crop, ref = "barley")
Netphotosynthesisrate3 $crop <- factor(Netphotosynthesisrate3 $crop, levels = c("barley","bd21","oat","wheat"))

Netphotosynthesisrate3 $Treatment <- relevel(Netphotosynthesisrate3 $Treatment, ref = "c")
Netphotosynthesisrate3 $Treatment <- factor(Netphotosynthesisrate3 $Treatment, levels = c("c","d"))


Netphotosynthesisrate3 $domestication <- relevel(Netphotosynthesisrate3 $domestication, ref = "W")
Netphotosynthesisrate3 $domestication <- factor(Netphotosynthesisrate3 $domestication, levels = c("W","D"))

mean_A <- aggregate(A ~ crop+Treatment+domestication+ accession, data = Netphotosynthesisrate3, FUN = mean)
# View the result
print(mean_A)
#Netphotosynthesisrate3<-Netphotosynthesisrate3[-which(Netphotosynthesisrate3 $crop=="bd21"),]
l<-lmer(A ~  crop*Treatment* domestication+(1|accession), data= Netphotosynthesisrate3)
a0<-as.matrix(car::Anova(l))[,3]

Netphotosynthesisrate31<-Netphotosynthesisrate3[-which(Netphotosynthesisrate3 $crop=="bd21"),]
l<-lmer(A ~  crop*Treatment* domestication+(1|accession), data= Netphotosynthesisrate31)
car::Anova(l)
library(emmeans)
a1<-as.data.frame(emmeans(l, "Treatment"))[,2]
names(a1)<-as.data.frame(emmeans(l, "Treatment"))[,1]
a2<-as.data.frame(emmeans(l, "domestication"))[,2]
names(a2)<-as.data.frame(emmeans(l, "domestication"))[,1]
l<-lmer(A ~  crop+(1|accession) +(1| domestication)+(1| Treatment), data= Netphotosynthesisrate3)
a3<-as.data.frame(emmeans(l, "crop"))[,2]
names(a3)<-as.data.frame(emmeans(l, "crop"))[,1]
bigtable<-cbind(bigtable,c(a0,a1,a2,a3))
Netphotosynthesisrate3<-NULL
 microscopy<-read.csv("~/Desktop/chapter3final/microscopy_summary.csv", header=T) 
 elongation <-read.csv("~/Desktop/chapter3final/elongation_summary_models.csv", header=T) 
 info <-read.csv("~/Desktop/chapter3final/elongation_summary_info.csv", header=T)
info<-as.data.frame(t(info))
colnames(info)<-info[1,]
info<-info[-1,]
info<-info[,-5]
colnames(info)[5]<-"accession"
head(microscopy)

Netphotosynthesisrate3<-microscopy[,which(colnames(microscopy)%in%c("domestication", "accession", "crop","treatment", "divisionzonelist"))]
Netphotosynthesisrate3$A<-microscopy $divisionzonelist
#3.Netphotosynthesisrate3$A<-c(-0.1*Drydown_brachy $hydraulic.potential.1)
#4.Netphotosynthesisrate3$A<-Drydown_brachy $A
#5.Netphotosynthesisrate3$A<-Drydown_brachy $gsw
#6.Netphotosynthesisrate3$A<-(Drydown_brachy $fresh.shoot+Drydown_brachy $fresh.root)
#7.Netphotosynthesisrate3$A<-(Drydown_brachy $dry.shoot+Drydown_brachy $dry.root)
#8.Netphotosynthesisrate3$A<-(Drydown_brachy $dry.root/Drydown_brachy $dry.shoot)
#9.Netphotosynthesisrate3$A<-Drydown_brachy $glucose.nmol.Glc.equivalents.mg
Netphotosynthesisrate3 <-na.omit(Netphotosynthesisrate3)
res = fitData(na.omit(Netphotosynthesisrate3$A), fit=c("gamma","logistic","normal","exponential","poisson","exponential"),
    sample=1)
res
###
Netphotosynthesisrate3 $Netphotosynthesisrate<-as.numeric(na.omit(Netphotosynthesisrate3 $A))
Netphotosynthesisrate3 $domestication<-as.factor(Netphotosynthesisrate3 $domestication)
Netphotosynthesisrate3 $crop <-as.factor(Netphotosynthesisrate3 $crop)
Netphotosynthesisrate3 $Treatment <-as.factor(Netphotosynthesisrate3 $treatment)
Netphotosynthesisrate3 $accession <-as.factor(Netphotosynthesisrate3 $accession)
options(na.action = "na.fail")
m<-lm(A ~  crop*Treatment* domestication, data= Netphotosynthesisrate3)

dredge(m, rank="AIC",extra = c("R^2", adjRsq=function(x) summary(x)$adj.r.squared))

a<-lmer(A ~ crop+Treatment+ domestication+(1|accession), data= Netphotosynthesisrate3)
b<-lmer(A ~ crop+Treatment* domestication+(1|accession), data= Netphotosynthesisrate3)
c<-lmer(A ~ crop*Treatment+ domestication+(1|accession), data= Netphotosynthesisrate3)
d<-lmer(A ~ crop* domestication+Treatment+(1|accession), data= Netphotosynthesisrate3)
e<-lmer(A ~ crop*Treatment+ domestication+(1|accession), data= Netphotosynthesisrate3)
f<-lmer(A ~ crop+(1|accession), data= Netphotosynthesisrate3)
g<-lmer(A ~ domestication+(1|accession), data= Netphotosynthesisrate3)
h<-lmer(A ~  Treatment +(1|accession), data= Netphotosynthesisrate3)
i<-lmer(A ~  crop+domestication+(1|accession), data= Netphotosynthesisrate3)
j<-lmer(A ~  crop+ Treatment +(1|accession), data= Netphotosynthesisrate3)
k<-lmer(A ~  Treatment +domestication+(1|accession), data= Netphotosynthesisrate3)
l<-lmer(A ~  crop*Treatment* domestication+(1|accession), data= Netphotosynthesisrate3)
m<-lm(A ~  crop*Treatment* domestication, data= Netphotosynthesisrate3)
m<-lm(A ~  crop*Treatment+domestication, data= Netphotosynthesisrate3)

AIC(a,b,c,d,e,f,g,h,i,j,k,l,m)[order(AIC(a,b,c,d,e,f,g,h,i,j,k,l,m)[,2]),]
car::Anova(l)
summary(l)
Netphotosynthesisrate3 $crop <- relevel(Netphotosynthesisrate3 $crop, ref = "barley")
Netphotosynthesisrate3 $crop <- factor(Netphotosynthesisrate3 $crop, levels = c("barley","bd21","oat","wheat"))

Netphotosynthesisrate3 $Treatment <- relevel(Netphotosynthesisrate3 $Treatment, ref = "c")
Netphotosynthesisrate3 $Treatment <- factor(Netphotosynthesisrate3 $Treatment, levels = c("c","d"))


Netphotosynthesisrate3 $domestication <- relevel(Netphotosynthesisrate3 $domestication, ref = "W")
Netphotosynthesisrate3 $domestication <- factor(Netphotosynthesisrate3 $domestication, levels = c("W","D"))

mean_A <- aggregate(A ~ crop+Treatment+domestication+ accession, data = Netphotosynthesisrate3, FUN = mean)
# View the result
print(mean_A)
#Netphotosynthesisrate3<-Netphotosynthesisrate3[-which(Netphotosynthesisrate3 $crop=="bd21"),]
l<-lmer(A ~  crop*Treatment* domestication+(1|accession), data= Netphotosynthesisrate3)
a0<-as.matrix(car::Anova(l))[,3]

Netphotosynthesisrate31<-Netphotosynthesisrate3[-which(Netphotosynthesisrate3 $crop=="bd21"),]
l<-lmer(A ~  crop*Treatment* domestication+(1|accession), data= Netphotosynthesisrate31)
car::Anova(l)
library(emmeans)
a1<-as.data.frame(emmeans(l, "Treatment"))[,2]
names(a1)<-as.data.frame(emmeans(l, "Treatment"))[,1]
a2<-as.data.frame(emmeans(l, "domestication"))[,2]
names(a2)<-as.data.frame(emmeans(l, "domestication"))[,1]
l<-lmer(A ~  crop+(1|accession) +(1| domestication)+(1| Treatment), data= Netphotosynthesisrate3)
a3<-as.data.frame(emmeans(l, "crop"))[,2]
names(a3)<-as.data.frame(emmeans(l, "crop"))[,1]
bigtable<-cbind(bigtable,c(a0,a1,a2,a3))


Netphotosynthesisrate3<-NULL
 microscopy<-read.csv("~/Desktop/chapter3final/microscopy_summary.csv", header=T) 
 elongation <-read.csv("~/Desktop/chapter3final/elongation_summary_models.csv", header=T) 
 info <-read.csv("~/Desktop/chapter3final/elongation_summary_info.csv", header=T)
info<-as.data.frame(t(info))
colnames(info)<-info[1,]
info<-info[-1,]
info<-info[,-5]
colnames(info)[5]<-"accession"
head(microscopy)

Netphotosynthesisrate3<-microscopy[,which(colnames(microscopy)%in%c("domestication", "accession", "crop","treatment", "divisionzonelist"))]
Netphotosynthesisrate3$A<-microscopy $divisioncellsize
#3.Netphotosynthesisrate3$A<-c(-0.1*Drydown_brachy $hydraulic.potential.1)
#4.Netphotosynthesisrate3$A<-Drydown_brachy $A
#5.Netphotosynthesisrate3$A<-Drydown_brachy $gsw
#6.Netphotosynthesisrate3$A<-(Drydown_brachy $fresh.shoot+Drydown_brachy $fresh.root)
#7.Netphotosynthesisrate3$A<-(Drydown_brachy $dry.shoot+Drydown_brachy $dry.root)
#8.Netphotosynthesisrate3$A<-(Drydown_brachy $dry.root/Drydown_brachy $dry.shoot)
#9.Netphotosynthesisrate3$A<-Drydown_brachy $glucose.nmol.Glc.equivalents.mg
Netphotosynthesisrate3 <-na.omit(Netphotosynthesisrate3)
res = fitData(na.omit(Netphotosynthesisrate3$A), fit=c("gamma","logistic","normal","exponential","poisson","exponential"),
    sample=1)
res
###
Netphotosynthesisrate3 $Netphotosynthesisrate<-as.numeric(na.omit(Netphotosynthesisrate3 $A))
Netphotosynthesisrate3 $domestication<-as.factor(Netphotosynthesisrate3 $domestication)
Netphotosynthesisrate3 $crop <-as.factor(Netphotosynthesisrate3 $crop)
Netphotosynthesisrate3 $Treatment <-as.factor(Netphotosynthesisrate3 $treatment)
Netphotosynthesisrate3 $accession <-as.factor(Netphotosynthesisrate3 $accession)
options(na.action = "na.fail")
m<-lm(A ~  crop*Treatment* domestication, data= Netphotosynthesisrate3)

dredge(m, rank="AIC",extra = c("R^2", adjRsq=function(x) summary(x)$adj.r.squared))

a<-lmer(A ~ crop+Treatment+ domestication+(1|accession), data= Netphotosynthesisrate3)
b<-lmer(A ~ crop+Treatment* domestication+(1|accession), data= Netphotosynthesisrate3)
c<-lmer(A ~ crop*Treatment+ domestication+(1|accession), data= Netphotosynthesisrate3)
d<-lmer(A ~ crop* domestication+Treatment+(1|accession), data= Netphotosynthesisrate3)
e<-lmer(A ~ crop*Treatment+ domestication+(1|accession), data= Netphotosynthesisrate3)
f<-lmer(A ~ crop+(1|accession), data= Netphotosynthesisrate3)
g<-lmer(A ~ domestication+(1|accession), data= Netphotosynthesisrate3)
h<-lmer(A ~  Treatment +(1|accession), data= Netphotosynthesisrate3)
i<-lmer(A ~  crop+domestication+(1|accession), data= Netphotosynthesisrate3)
j<-lmer(A ~  crop+ Treatment +(1|accession), data= Netphotosynthesisrate3)
k<-lmer(A ~  Treatment +domestication+(1|accession), data= Netphotosynthesisrate3)
l<-lmer(A ~  crop*Treatment* domestication+(1|accession), data= Netphotosynthesisrate3)
m<-lm(A ~  crop*Treatment* domestication, data= Netphotosynthesisrate3)
m<-lm(A ~  crop*Treatment+domestication, data= Netphotosynthesisrate3)

AIC(a,b,c,d,e,f,g,h,i,j,k,l,m)[order(AIC(a,b,c,d,e,f,g,h,i,j,k,l,m)[,2]),]
car::Anova(l)
summary(l)
Netphotosynthesisrate3 $crop <- relevel(Netphotosynthesisrate3 $crop, ref = "barley")
Netphotosynthesisrate3 $crop <- factor(Netphotosynthesisrate3 $crop, levels = c("barley","bd21","oat","wheat"))

Netphotosynthesisrate3 $Treatment <- relevel(Netphotosynthesisrate3 $Treatment, ref = "c")
Netphotosynthesisrate3 $Treatment <- factor(Netphotosynthesisrate3 $Treatment, levels = c("c","d"))


Netphotosynthesisrate3 $domestication <- relevel(Netphotosynthesisrate3 $domestication, ref = "W")
Netphotosynthesisrate3 $domestication <- factor(Netphotosynthesisrate3 $domestication, levels = c("W","D"))

mean_A <- aggregate(A ~ crop+Treatment+domestication+ accession, data = Netphotosynthesisrate3, FUN = mean)
# View the result
print(mean_A)
#Netphotosynthesisrate3<-Netphotosynthesisrate3[-which(Netphotosynthesisrate3 $crop=="bd21"),]
l<-lmer(A ~  crop*Treatment* domestication+(1|accession), data= Netphotosynthesisrate3)
a0<-as.matrix(car::Anova(l))[,3]

Netphotosynthesisrate31<-Netphotosynthesisrate3[-which(Netphotosynthesisrate3 $crop=="bd21"),]
l<-lmer(A ~  crop*Treatment* domestication+(1|accession), data= Netphotosynthesisrate31)
car::Anova(l)
library(emmeans)
a1<-as.data.frame(emmeans(l, "Treatment"))[,2]
names(a1)<-as.data.frame(emmeans(l, "Treatment"))[,1]
a2<-as.data.frame(emmeans(l, "domestication"))[,2]
names(a2)<-as.data.frame(emmeans(l, "domestication"))[,1]
l<-lmer(A ~  crop+(1|accession) +(1| domestication)+(1| Treatment), data= Netphotosynthesisrate3)
a3<-as.data.frame(emmeans(l, "crop"))[,2]
names(a3)<-as.data.frame(emmeans(l, "crop"))[,1]
bigtable<-cbind(bigtable,c(a0,a1,a2,a3))

Netphotosynthesisrate3<-NULL
 microscopy<-read.csv("~/Desktop/chapter3final/microscopy_summary.csv", header=T) 
 elongation <-read.csv("~/Desktop/chapter3final/elongation_summary_models.csv", header=T) 
 info <-read.csv("~/Desktop/chapter3final/elongation_summary_info.csv", header=T)
info<-as.data.frame(t(info))
colnames(info)<-info[1,]
info<-info[-1,]
info<-info[,-5]
colnames(info)[5]<-"accession"
head(microscopy)

Netphotosynthesisrate3<-microscopy[,which(colnames(microscopy)%in%c("domestication", "accession", "crop","treatment", "divisionzonelist"))]
Netphotosynthesisrate3$A<-microscopy $divisionzonelist/microscopy$divisioncellsize
#3.Netphotosynthesisrate3$A<-c(-0.1*Drydown_brachy $hydraulic.potential.1)
#4.Netphotosynthesisrate3$A<-Drydown_brachy $A
#5.Netphotosynthesisrate3$A<-Drydown_brachy $gsw
#6.Netphotosynthesisrate3$A<-(Drydown_brachy $fresh.shoot+Drydown_brachy $fresh.root)
#7.Netphotosynthesisrate3$A<-(Drydown_brachy $dry.shoot+Drydown_brachy $dry.root)
#8.Netphotosynthesisrate3$A<-(Drydown_brachy $dry.root/Drydown_brachy $dry.shoot)
#9.Netphotosynthesisrate3$A<-Drydown_brachy $glucose.nmol.Glc.equivalents.mg
Netphotosynthesisrate3 <-na.omit(Netphotosynthesisrate3)
res = fitData(na.omit(Netphotosynthesisrate3$A), fit=c("gamma","logistic","normal","exponential","poisson","exponential"),
    sample=1)
res
###
Netphotosynthesisrate3 $Netphotosynthesisrate<-as.numeric(na.omit(Netphotosynthesisrate3 $A))
Netphotosynthesisrate3 $domestication<-as.factor(Netphotosynthesisrate3 $domestication)
Netphotosynthesisrate3 $crop <-as.factor(Netphotosynthesisrate3 $crop)
Netphotosynthesisrate3 $Treatment <-as.factor(Netphotosynthesisrate3 $treatment)
Netphotosynthesisrate3 $accession <-as.factor(Netphotosynthesisrate3 $accession)
options(na.action = "na.fail")
m<-lm(A ~  crop*Treatment* domestication, data= Netphotosynthesisrate3)

dredge(m, rank="AIC",extra = c("R^2", adjRsq=function(x) summary(x)$adj.r.squared))

a<-lmer(A ~ crop+Treatment+ domestication+(1|accession), data= Netphotosynthesisrate3)
b<-lmer(A ~ crop+Treatment* domestication+(1|accession), data= Netphotosynthesisrate3)
c<-lmer(A ~ crop*Treatment+ domestication+(1|accession), data= Netphotosynthesisrate3)
d<-lmer(A ~ crop* domestication+Treatment+(1|accession), data= Netphotosynthesisrate3)
e<-lmer(A ~ crop*Treatment+ domestication+(1|accession), data= Netphotosynthesisrate3)
f<-lmer(A ~ crop+(1|accession), data= Netphotosynthesisrate3)
g<-lmer(A ~ domestication+(1|accession), data= Netphotosynthesisrate3)
h<-lmer(A ~  Treatment +(1|accession), data= Netphotosynthesisrate3)
i<-lmer(A ~  crop+domestication+(1|accession), data= Netphotosynthesisrate3)
j<-lmer(A ~  crop+ Treatment +(1|accession), data= Netphotosynthesisrate3)
k<-lmer(A ~  Treatment +domestication+(1|accession), data= Netphotosynthesisrate3)
l<-lmer(A ~  crop*Treatment* domestication+(1|accession), data= Netphotosynthesisrate3)
m<-lm(A ~  crop*Treatment* domestication, data= Netphotosynthesisrate3)
m<-lm(A ~  crop*Treatment+domestication, data= Netphotosynthesisrate3)

AIC(a,b,c,d,e,f,g,h,i,j,k,l,m)[order(AIC(a,b,c,d,e,f,g,h,i,j,k,l,m)[,2]),]
car::Anova(l)
summary(l)
Netphotosynthesisrate3 $crop <- relevel(Netphotosynthesisrate3 $crop, ref = "barley")
Netphotosynthesisrate3 $crop <- factor(Netphotosynthesisrate3 $crop, levels = c("barley","bd21","oat","wheat"))

Netphotosynthesisrate3 $Treatment <- relevel(Netphotosynthesisrate3 $Treatment, ref = "c")
Netphotosynthesisrate3 $Treatment <- factor(Netphotosynthesisrate3 $Treatment, levels = c("c","d"))


Netphotosynthesisrate3 $domestication <- relevel(Netphotosynthesisrate3 $domestication, ref = "W")
Netphotosynthesisrate3 $domestication <- factor(Netphotosynthesisrate3 $domestication, levels = c("W","D"))

mean_A <- aggregate(A ~ crop+Treatment+domestication+ accession, data = Netphotosynthesisrate3, FUN = mean)
# View the result
print(mean_A)
#Netphotosynthesisrate3<-Netphotosynthesisrate3[-which(Netphotosynthesisrate3 $crop=="bd21"),]
l<-lmer(A ~  crop*Treatment* domestication+(1|accession), data= Netphotosynthesisrate3)
a0<-as.matrix(car::Anova(l))[,3]

Netphotosynthesisrate31<-Netphotosynthesisrate3[-which(Netphotosynthesisrate3 $crop=="bd21"),]
l<-lmer(A ~  crop*Treatment* domestication+(1|accession), data= Netphotosynthesisrate31)
car::Anova(l)
library(emmeans)
a1<-as.data.frame(emmeans(l, "Treatment"))[,2]
names(a1)<-as.data.frame(emmeans(l, "Treatment"))[,1]
a2<-as.data.frame(emmeans(l, "domestication"))[,2]
names(a2)<-as.data.frame(emmeans(l, "domestication"))[,1]
l<-lmer(A ~  crop+(1|accession) +(1| domestication)+(1| Treatment), data= Netphotosynthesisrate3)
a3<-as.data.frame(emmeans(l, "crop"))[,2]
names(a3)<-as.data.frame(emmeans(l, "crop"))[,1]
bigtable<-cbind(bigtable,c(a0,a1,a2,a3))

Netphotosynthesisrate3<-NULL
 microscopy<-read.csv("~/Desktop/chapter3final/microscopy_summary.csv", header=T) 
 elongation <-read.csv("~/Desktop/chapter3final/elongation_summary_models.csv", header=T) 
 info <-read.csv("~/Desktop/chapter3final/elongation_summary_info.csv", header=T)
info<-as.data.frame(t(info))
colnames(info)<-info[1,]
info<-info[-1,]
info<-info[,-5]
colnames(info)[5]<-"accession"
head(microscopy)

Netphotosynthesisrate3<-microscopy[,which(colnames(microscopy)%in%c("domestication", "accession", "crop","treatment", "divisionzonelist"))]
Netphotosynthesisrate3$A<-microscopy $Lge
#3.Netphotosynthesisrate3$A<-c(-0.1*Drydown_brachy $hydraulic.potential.1)
#4.Netphotosynthesisrate3$A<-Drydown_brachy $A
#5.Netphotosynthesisrate3$A<-Drydown_brachy $gsw
#6.Netphotosynthesisrate3$A<-(Drydown_brachy $fresh.shoot+Drydown_brachy $fresh.root)
#7.Netphotosynthesisrate3$A<-(Drydown_brachy $dry.shoot+Drydown_brachy $dry.root)
#8.Netphotosynthesisrate3$A<-(Drydown_brachy $dry.root/Drydown_brachy $dry.shoot)
#9.Netphotosynthesisrate3$A<-Drydown_brachy $glucose.nmol.Glc.equivalents.mg
Netphotosynthesisrate3 <-na.omit(Netphotosynthesisrate3)
res = fitData(na.omit(Netphotosynthesisrate3$A), fit=c("gamma","logistic","normal","exponential","poisson","exponential"),
    sample=1)
res
###
Netphotosynthesisrate3 $Netphotosynthesisrate<-as.numeric(na.omit(Netphotosynthesisrate3 $A))
Netphotosynthesisrate3 $domestication<-as.factor(Netphotosynthesisrate3 $domestication)
Netphotosynthesisrate3 $crop <-as.factor(Netphotosynthesisrate3 $crop)
Netphotosynthesisrate3 $Treatment <-as.factor(Netphotosynthesisrate3 $treatment)
Netphotosynthesisrate3 $accession <-as.factor(Netphotosynthesisrate3 $accession)
options(na.action = "na.fail")
m<-lm(A ~  crop*Treatment* domestication, data= Netphotosynthesisrate3)

dredge(m, rank="AIC",extra = c("R^2", adjRsq=function(x) summary(x)$adj.r.squared))

a<-lmer(A ~ crop+Treatment+ domestication+(1|accession), data= Netphotosynthesisrate3)
b<-lmer(A ~ crop+Treatment* domestication+(1|accession), data= Netphotosynthesisrate3)
c<-lmer(A ~ crop*Treatment+ domestication+(1|accession), data= Netphotosynthesisrate3)
d<-lmer(A ~ crop* domestication+Treatment+(1|accession), data= Netphotosynthesisrate3)
e<-lmer(A ~ crop*Treatment+ domestication+(1|accession), data= Netphotosynthesisrate3)
f<-lmer(A ~ crop+(1|accession), data= Netphotosynthesisrate3)
g<-lmer(A ~ domestication+(1|accession), data= Netphotosynthesisrate3)
h<-lmer(A ~  Treatment +(1|accession), data= Netphotosynthesisrate3)
i<-lmer(A ~  crop+domestication+(1|accession), data= Netphotosynthesisrate3)
j<-lmer(A ~  crop+ Treatment +(1|accession), data= Netphotosynthesisrate3)
k<-lmer(A ~  Treatment +domestication+(1|accession), data= Netphotosynthesisrate3)
l<-lmer(A ~  crop*Treatment* domestication+(1|accession), data= Netphotosynthesisrate3)
m<-lm(A ~  crop*Treatment* domestication, data= Netphotosynthesisrate3)
m<-lm(A ~  crop*Treatment+domestication, data= Netphotosynthesisrate3)

AIC(a,b,c,d,e,f,g,h,i,j,k,l,m)[order(AIC(a,b,c,d,e,f,g,h,i,j,k,l,m)[,2]),]
car::Anova(l)
summary(l)
Netphotosynthesisrate3 $crop <- relevel(Netphotosynthesisrate3 $crop, ref = "barley")
Netphotosynthesisrate3 $crop <- factor(Netphotosynthesisrate3 $crop, levels = c("barley","bd21","oat","wheat"))

Netphotosynthesisrate3 $Treatment <- relevel(Netphotosynthesisrate3 $Treatment, ref = "c")
Netphotosynthesisrate3 $Treatment <- factor(Netphotosynthesisrate3 $Treatment, levels = c("c","d"))


Netphotosynthesisrate3 $domestication <- relevel(Netphotosynthesisrate3 $domestication, ref = "W")
Netphotosynthesisrate3 $domestication <- factor(Netphotosynthesisrate3 $domestication, levels = c("W","D"))

mean_A <- aggregate(A ~ crop+Treatment+domestication+ accession, data = Netphotosynthesisrate3, FUN = mean)
# View the result
print(mean_A)
#Netphotosynthesisrate3<-Netphotosynthesisrate3[-which(Netphotosynthesisrate3 $crop=="bd21"),]
l<-lmer(A ~  crop*Treatment* domestication+(1|accession), data= Netphotosynthesisrate3)
a0<-as.matrix(car::Anova(l))[,3]

Netphotosynthesisrate31<-Netphotosynthesisrate3[-which(Netphotosynthesisrate3 $crop=="bd21"),]
l<-lmer(A ~  crop*Treatment* domestication+(1|accession), data= Netphotosynthesisrate31)
car::Anova(l)
library(emmeans)
a1<-as.data.frame(emmeans(l, "Treatment"))[,2]
names(a1)<-as.data.frame(emmeans(l, "Treatment"))[,1]
a2<-as.data.frame(emmeans(l, "domestication"))[,2]
names(a2)<-as.data.frame(emmeans(l, "domestication"))[,1]
l<-lmer(A ~  crop+(1|accession) +(1| domestication)+(1| Treatment), data= Netphotosynthesisrate3)
a3<-as.data.frame(emmeans(l, "crop"))[,2]
names(a3)<-as.data.frame(emmeans(l, "crop"))[,1]
bigtable<-cbind(bigtable,c(a0,a1,a2,a3))

Netphotosynthesisrate3<-NULL
 microscopy<-read.csv("~/Desktop/chapter3final/microscopy_summary.csv", header=T) 
 elongation <-read.csv("~/Desktop/chapter3final/elongation_summary_models.csv", header=T) 
 info <-read.csv("~/Desktop/chapter3final/elongation_summary_info.csv", header=T)
info<-as.data.frame(t(info))
colnames(info)<-info[1,]
info<-info[-1,]
info<-info[,-5]
colnames(info)[5]<-"accession"
head(microscopy)

Netphotosynthesisrate3<-microscopy[,which(colnames(microscopy)%in%c("domestication", "accession", "crop","treatment", "divisionzonelist"))]
Netphotosynthesisrate3$A<-microscopy $N
#3.Netphotosynthesisrate3$A<-c(-0.1*Drydown_brachy $hydraulic.potential.1)
#4.Netphotosynthesisrate3$A<-Drydown_brachy $A
#5.Netphotosynthesisrate3$A<-Drydown_brachy $gsw
#6.Netphotosynthesisrate3$A<-(Drydown_brachy $fresh.shoot+Drydown_brachy $fresh.root)
#7.Netphotosynthesisrate3$A<-(Drydown_brachy $dry.shoot+Drydown_brachy $dry.root)
#8.Netphotosynthesisrate3$A<-(Drydown_brachy $dry.root/Drydown_brachy $dry.shoot)
#9.Netphotosynthesisrate3$A<-Drydown_brachy $glucose.nmol.Glc.equivalents.mg
Netphotosynthesisrate3 <-na.omit(Netphotosynthesisrate3)
res = fitData(na.omit(Netphotosynthesisrate3$A), fit=c("gamma","logistic","normal","exponential","poisson","exponential"),
    sample=1)
res
###
Netphotosynthesisrate3 $Netphotosynthesisrate<-as.numeric(na.omit(Netphotosynthesisrate3 $A))
Netphotosynthesisrate3 $domestication<-as.factor(Netphotosynthesisrate3 $domestication)
Netphotosynthesisrate3 $crop <-as.factor(Netphotosynthesisrate3 $crop)
Netphotosynthesisrate3 $Treatment <-as.factor(Netphotosynthesisrate3 $treatment)
Netphotosynthesisrate3 $accession <-as.factor(Netphotosynthesisrate3 $accession)
options(na.action = "na.fail")
m<-lm(A ~  crop*Treatment* domestication, data= Netphotosynthesisrate3)

dredge(m, rank="AIC",extra = c("R^2", adjRsq=function(x) summary(x)$adj.r.squared))

a<-lmer(A ~ crop+Treatment+ domestication+(1|accession), data= Netphotosynthesisrate3)
b<-lmer(A ~ crop+Treatment* domestication+(1|accession), data= Netphotosynthesisrate3)
c<-lmer(A ~ crop*Treatment+ domestication+(1|accession), data= Netphotosynthesisrate3)
d<-lmer(A ~ crop* domestication+Treatment+(1|accession), data= Netphotosynthesisrate3)
e<-lmer(A ~ crop*Treatment+ domestication+(1|accession), data= Netphotosynthesisrate3)
f<-lmer(A ~ crop+(1|accession), data= Netphotosynthesisrate3)
g<-lmer(A ~ domestication+(1|accession), data= Netphotosynthesisrate3)
h<-lmer(A ~  Treatment +(1|accession), data= Netphotosynthesisrate3)
i<-lmer(A ~  crop+domestication+(1|accession), data= Netphotosynthesisrate3)
j<-lmer(A ~  crop+ Treatment +(1|accession), data= Netphotosynthesisrate3)
k<-lmer(A ~  Treatment +domestication+(1|accession), data= Netphotosynthesisrate3)
l<-lmer(A ~  crop*Treatment* domestication+(1|accession), data= Netphotosynthesisrate3)
m<-lm(A ~  crop*Treatment* domestication, data= Netphotosynthesisrate3)
m<-lm(A ~  crop*Treatment+domestication, data= Netphotosynthesisrate3)

AIC(a,b,c,d,e,f,g,h,i,j,k,l,m)[order(AIC(a,b,c,d,e,f,g,h,i,j,k,l,m)[,2]),]
car::Anova(l)
summary(l)
Netphotosynthesisrate3 $crop <- relevel(Netphotosynthesisrate3 $crop, ref = "barley")
Netphotosynthesisrate3 $crop <- factor(Netphotosynthesisrate3 $crop, levels = c("barley","bd21","oat","wheat"))

Netphotosynthesisrate3 $Treatment <- relevel(Netphotosynthesisrate3 $Treatment, ref = "c")
Netphotosynthesisrate3 $Treatment <- factor(Netphotosynthesisrate3 $Treatment, levels = c("c","d"))


Netphotosynthesisrate3 $domestication <- relevel(Netphotosynthesisrate3 $domestication, ref = "W")
Netphotosynthesisrate3 $domestication <- factor(Netphotosynthesisrate3 $domestication, levels = c("W","D"))

mean_A <- aggregate(A ~ crop+Treatment+domestication+ accession, data = Netphotosynthesisrate3, FUN = mean)
# View the result
print(mean_A)
#Netphotosynthesisrate3<-Netphotosynthesisrate3[-which(Netphotosynthesisrate3 $crop=="bd21"),]
l<-lmer(A ~  crop*Treatment* domestication+(1|accession), data= Netphotosynthesisrate3)
a0<-as.matrix(car::Anova(l))[,3]

Netphotosynthesisrate31<-Netphotosynthesisrate3[-which(Netphotosynthesisrate3 $crop=="bd21"),]
l<-lmer(A ~  crop*Treatment* domestication+(1|accession), data= Netphotosynthesisrate31)
car::Anova(l)
library(emmeans)
a1<-as.data.frame(emmeans(l, "Treatment"))[,2]
names(a1)<-as.data.frame(emmeans(l, "Treatment"))[,1]
a2<-as.data.frame(emmeans(l, "domestication"))[,2]
names(a2)<-as.data.frame(emmeans(l, "domestication"))[,1]
l<-lmer(A ~  crop+(1|accession) +(1| domestication)+(1| Treatment), data= Netphotosynthesisrate3)
a3<-as.data.frame(emmeans(l, "crop"))[,2]
names(a3)<-as.data.frame(emmeans(l, "crop"))[,1]
bigtable<-cbind(bigtable,c(a0,a1,a2,a3))
write.csv(as.matrix(bigtable),"~/Downloads/tmp.csv")


                               require(MASS)
require(ggplot2)
require(lme4)
library(lmerTest)
library(MuMIn)
library("emmeans")

library(ggpubr)
##get distribution of data
theme_set(
  theme_bw()
)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
    library(plyr)

    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
    }

    # This does the summary. For each group's data frame, return a vector with
    # N, mean, and sd
    datac <- ddply(data, groupvars, .drop=.drop,
      .fun = function(xx, col) {
        c(N    = length2(xx[[col]], na.rm=na.rm),
          mean = mean   (xx[[col]], na.rm=na.rm),
          sd   = sd     (xx[[col]], na.rm=na.rm)
        )
      },
      measurevar
    )

    # Rename the "mean" column    
    datac <- rename(datac, c("mean" = measurevar))

    datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval: 
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult

    return(datac)
}

###distribution check
fitData <- function(data, fit="gamma", sample=0.5){
 distrib = list()
 numfit <- length(fit)
 results = matrix(0, ncol=5, nrow=numfit)

 for(i in 1:numfit){
if((fit[i] == "gamma") | 
     (fit[i] == "poisson") | 
     (fit[i] == "weibull") | 
     (fit[i] == "exponential") |
     (fit[i] == "logistic") |
     (fit[i] == "normal") | 
     (fit[i] == "geometric")
) 
  distrib[[i]] = fit[i]
else stop("Provide a valid distribution to fit data" )
 }

 # take a sample of dataset
 n = round(length(data)*sample)
 data = sample(data, size=n, replace=F)

 for(i in 1:numfit) {
  if(distrib[[i]] == "gamma") {
  gf_shape = "gamma"
  fd_g <- fitdistr(data, "gamma")
  est_shape = fd_g$estimate[[1]]
  est_rate = fd_g$estimate[[2]]

  ks = ks.test(data, "pgamma", shape=est_shape, rate=est_rate)

  # add to results
  results[i,] = c(gf_shape, est_shape, est_rate, ks$statistic, ks$p.value)
}

else if(distrib[[i]] == "poisson"){
  gf_shape = "poisson"
  fd_p <- fitdistr(data, "poisson")
  est_lambda = fd_p$estimate[[1]]

  ks = ks.test(data, "ppois", lambda=est_lambda)
  # add to results
  results[i,] = c(gf_shape, est_lambda, "NA", ks$statistic, ks$p.value)

}

else if(distrib[[i]] == "weibull"){
  gf_shape = "weibull"
  fd_w <- fitdistr(data,densfun=dweibull,start=list(scale=1,shape=2))
  est_shape = fd_w$estimate[[1]]
  est_scale = fd_w$estimate[[2]]

  ks = ks.test(data, "pweibull", shape=est_shape, scale=est_scale)
  # add to results
  results[i,] = c(gf_shape, est_shape, est_scale, ks$statistic, ks$p.value) 
}

else if(distrib[[i]] == "normal"){
  gf_shape = "normal"
  fd_n <- fitdistr(data, "normal")
  est_mean = fd_n$estimate[[1]]
  est_sd = fd_n$estimate[[2]]

  ks = ks.test(data, "pnorm", mean=est_mean, sd=est_sd)
  # add to results
  results[i,] = c(gf_shape, est_mean, est_sd, ks$statistic, ks$p.value)
}

else if(distrib[[i]] == "exponential"){
  gf_shape = "exponential"
  fd_e <- fitdistr(data, "exponential")
  est_rate = fd_e$estimate[[1]]
  ks = ks.test(data, "pexp", rate=est_rate)
  # add to results
  results[i,] = c(gf_shape, est_rate, "NA", ks$statistic, ks$p.value)
}

else if(distrib[[i]] == "logistic"){
  gf_shape = "logistic"
  fd_l <- fitdistr(data, "logistic")
  est_location = fd_l$estimate[[1]]
  est_scale = fd_l$estimate[[2]]
  ks = ks.test(data, "plogis", location=est_location, scale=est_scale)
  # add to results
  results[i,] = c(gf_shape, est_location, est_scale, ks$statistic,    ks$p.value) 
    }
  }
  results = rbind(c("distribution", "param1", "param2", "ks stat", "ks    pvalue"),   results)
  #print(results)
  return(results)
  }
   
   Netphotosynthesisrate3<-NULL
   microscopy<-read.csv("~/Downloads/microscopy_summary_6_28.csv", header=T)
elongation <-read.csv("~/Downloads/elongation_summary_python1_out.csv", header=T)
info <-read.csv("~/Downloads/elongation_summary_python2_6_28.csv", header=T)
info<-as.data.frame(t(info))
colnames(info)<-info[1,]
info<-info[-1,]
head(microscopy)
Netphotosynthesisrate3<-microscopy[,which(colnames(microscopy)%in%c("domestication", "accession", "crop","treatment", "divisionzonelist"))]
Netphotosynthesisrate3$A<-microscopy $divisionzonelist
#3.Netphotosynthesisrate3$A<-c(-0.1*Drydown_brachy $hydraulic.potential.1)
#4.Netphotosynthesisrate3$A<-Drydown_brachy $A
#5.Netphotosynthesisrate3$A<-Drydown_brachy $gsw
#6.Netphotosynthesisrate3$A<-(Drydown_brachy $fresh.shoot+Drydown_brachy $fresh.root)
#7.Netphotosynthesisrate3$A<-(Drydown_brachy $dry.shoot+Drydown_brachy $dry.root)
#8.Netphotosynthesisrate3$A<-(Drydown_brachy $dry.root/Drydown_brachy $dry.shoot)
#9.Netphotosynthesisrate3$A<-Drydown_brachy $glucose.nmol.Glc.equivalents.mg
Netphotosynthesisrate3 <-na.omit(Netphotosynthesisrate3)
res = fitData(na.omit(Netphotosynthesisrate3$A), fit=c("gamma","logistic","normal","exponential","poisson","exponential"),
    sample=1)
res
###
Netphotosynthesisrate3 $Netphotosynthesisrate<-as.numeric(na.omit(Netphotosynthesisrate3 $A))
Netphotosynthesisrate3 $domestication<-as.factor(Netphotosynthesisrate3 $domestication)
Netphotosynthesisrate3 $crop <-as.factor(Netphotosynthesisrate3 $crop)
Netphotosynthesisrate3 $Treatment <-as.factor(Netphotosynthesisrate3 $treatment)
Netphotosynthesisrate3 $accession <-as.factor(Netphotosynthesisrate3 $accession)
options(na.action = "na.fail")
m<-lm(A ~  crop*Treatment* domestication, data= Netphotosynthesisrate3)

dredge(m, rank="AIC",extra = c("R^2", adjRsq=function(x) summary(x)$adj.r.squared))
m<-lm(A ~  crop*Treatment+domestication, data= Netphotosynthesisrate3)

AIC(a,b,c,d,e,f,g,h,i,j,k,l,m)[order(AIC(a,b,c,d,e,f,g,h,i,j,k,l,m)[,2]),]
car::Anova(l)
summary(l)
gm_mc <- emmeans(m, ~ Treatment | crop* domestication, ddf="kenward-roger")
Hf_Aarea_lsmeans <-pairs(gm_mc)
Hf_Aarea_lsmeans 
letter<-Hf_Aarea_lsmeans
letter<-as.data.frame(letter)
letter <- symnum(letter[,dim(letter)[2]], corr = FALSE, na = FALSE, cutpoints = c(0, 
    0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", " "))        
theme_set(
  theme_bw()
)
letterA<-cbind(c(rep("D",4),rep("W",4)),rep(levels(as.factor(Netphotosynthesisrate3 $crop)),2), letter)
letter<-letterA[match(paste0(Netphotosynthesisrate3 $domestication, Netphotosynthesisrate3 $crop), paste0(letterA[,1],letterA[,2])),3]
species<-paste0(Netphotosynthesisrate3 $domestication,"-", Netphotosynthesisrate3 $crop )
elongation_zone_size<-Netphotosynthesisrate3 $A
treatment<-Netphotosynthesisrate3 $treatment
data=data.frame(Netphotosynthesisrate3 $domestication, Netphotosynthesisrate3 $accession ,  Netphotosynthesisrate3 $crop,  treatment , elongation_zone_size, letter, species)
 pd <- position_dodge(0.8) #
# grouped boxplot
data$treatment[which(data$treatment=="d")]<-"Drought"
data$treatment[which(data$treatment=="c")]<-"Control"
data$species[which(data$species =="D-barley")]<-"Barley-Domesticated"
data$species[which(data$species =="W-barley")]<-"Barley-Wild"
data$species[which(data$species =="W-bd21")]<-"Bd21"
data$species[which(data$species =="D-wheat")]<-"Wheat-Domesticated"
data$species[which(data$species =="W-wheat")]<-"Wheat-Wild"
data$species[which(data$species =="D-oat")]<-"Oat-Domesticated"
data$species[which(data$species =="W-oat")]<-"Oat-Wild"
ggplot(data, aes(x= factor(species, level=c("Barley-Domesticated","Barley-Wild","Wheat-Wild","Wheat-Domesticated","Oat-Domesticated","Oat-Wild","Bd21")) + 
    geom_boxplot()+geom_point(position=pd)+ geom_text(
    aes(label = letter, y= 5000, group= species),color="black", position =  position_dodge(width=0), size=6)+xlab("Species")+ylab(expression(paste("Division zone size (", mu, "m)")))+ scale_color_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+scale_fill_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+geom_rect(aes(xmin=0.5,xmax=1.5,ymin=-Inf, ymax=Inf),fill="grey",alpha=0.01)+geom_rect(aes(xmin=2.5,xmax=3.5,ymin=-Inf, ymax=Inf),fill="grey",alpha=0.01)+geom_rect(aes(xmin=4.5,xmax=5.5,ymin=-Inf, ymax=Inf),fill="grey",alpha=0.01)+ theme( axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(data, aes(x= factor(species, level=c("Barley-Domesticated","Barley-Wild","Wheat-Domesticated","Wheat-Wild","Oat-Domesticated","Oat-Wild","Bd21")), y= elongation_zone_size, fill= treatment)) + 
    geom_boxplot()+geom_point(position=pd)+ geom_text(
    aes(label = letter, y= 5000, group= species),color="black", position =  position_dodge(width=0), size=6)+xlab("Species")+ylab(expression(paste("Division zone size (", mu, "m)")))+ scale_color_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+scale_fill_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+geom_rect(aes(xmin=0.5,xmax=1.5,ymin=-Inf, ymax=Inf),fill="grey",alpha=0.01)+geom_rect(aes(xmin=2.5,xmax=3.5,ymin=-Inf, ymax=Inf),fill="grey",alpha=0.01)+geom_rect(aes(xmin=4.5,xmax=5.5,ymin=-Inf, ymax=Inf),fill="grey",alpha=0.01)+ theme( axis.text.x = element_text(angle = 45, hjust = 1))








Netphotosynthesisrate3<-NULL
   microscopy<-read.csv("~/Downloads/microscopy_summary_6_28.csv", header=T)
elongation <-read.csv("~/Downloads/elongation_summary_python1_out.csv", header=T)
info <-read.csv("~/Downloads/elongation_summary_python2_6_28.csv", header=T)
info<-as.data.frame(t(info))
colnames(info)<-info[1,]
info<-info[-1,]
head(microscopy)
Netphotosynthesisrate3<-microscopy[,which(colnames(microscopy)%in%c("domestication", "accession", "crop","treatment", "Lge"))]
Netphotosynthesisrate3$A<-microscopy $Lge
#3.Netphotosynthesisrate3$A<-c(-0.1*Drydown_brachy $hydraulic.potential.1)
#4.Netphotosynthesisrate3$A<-Drydown_brachy $A
#5.Netphotosynthesisrate3$A<-Drydown_brachy $gsw
#6.Netphotosynthesisrate3$A<-(Drydown_brachy $fresh.shoot+Drydown_brachy $fresh.root)
#7.Netphotosynthesisrate3$A<-(Drydown_brachy $dry.shoot+Drydown_brachy $dry.root)
#8.Netphotosynthesisrate3$A<-(Drydown_brachy $dry.root/Drydown_brachy $dry.shoot)
#9.Netphotosynthesisrate3$A<-Drydown_brachy $glucose.nmol.Glc.equivalents.mg
Netphotosynthesisrate3 <-na.omit(Netphotosynthesisrate3)
res = fitData(na.omit(Netphotosynthesisrate3$A), fit=c("gamma","logistic","normal","exponential","poisson","exponential"),
    sample=1)
res
###
Netphotosynthesisrate3 $Netphotosynthesisrate<-as.numeric(na.omit(Netphotosynthesisrate3 $A))
Netphotosynthesisrate3 $domestication<-as.factor(Netphotosynthesisrate3 $domestication)
Netphotosynthesisrate3 $crop <-as.factor(Netphotosynthesisrate3 $crop)
Netphotosynthesisrate3 $Treatment <-as.factor(Netphotosynthesisrate3 $treatment)
Netphotosynthesisrate3 $accession <-as.factor(Netphotosynthesisrate3 $accession)
options(na.action = "na.fail")
m<-lm(A ~  crop*Treatment* domestication, data= Netphotosynthesisrate3)

dredge(m, rank="AIC",extra = c("R^2", adjRsq=function(x) summary(x)$adj.r.squared))
a<-lmer(A ~ crop+Treatment+ domestication+(1|accession), data= Netphotosynthesisrate3)
b<-lmer(A ~ crop+Treatment* domestication+(1|accession), data= Netphotosynthesisrate3)
c<-lmer(A ~ crop*Treatment+ domestication+(1|accession), data= Netphotosynthesisrate3)
d<-lmer(A ~ crop* domestication+Treatment+(1|accession), data= Netphotosynthesisrate3)
e<-lmer(A ~ crop*Treatment+ domestication+(1|accession), data= Netphotosynthesisrate3)
f<-lmer(A ~ crop+(1|accession), data= Netphotosynthesisrate3)
g<-lmer(A ~ domestication+(1|accession), data= Netphotosynthesisrate3)
h<-lmer(A ~  Treatment +(1|accession), data= Netphotosynthesisrate3)
i<-lmer(A ~  crop+domestication+(1|accession), data= Netphotosynthesisrate3)
j<-lmer(A ~  crop+ Treatment +(1|accession), data= Netphotosynthesisrate3)
k<-lmer(A ~  Treatment +domestication+(1|accession), data= Netphotosynthesisrate3)
l<-lmer(A ~  crop*Treatment* domestication+(1|accession), data= Netphotosynthesisrate3)

AIC(a,b,c,d,e,f,g,h,i,j,k,l,m)[order(AIC(a,b,c,d,e,f,g,h,i,j,k,l,m)[,2]),]
car::Anova(l)
summary(l)
gm_mc <- emmeans(l, ~ Treatment | crop* domestication, ddf="kenward-roger")
Hf_Aarea_lsmeans <-pairs(gm_mc)
Hf_Aarea_lsmeans 
letter<-Hf_Aarea_lsmeans
letter<-as.data.frame(letter)
letter <- symnum(letter[,dim(letter)[2]], corr = FALSE, na = FALSE, cutpoints = c(0, 
    0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", " "))        
theme_set(
  theme_bw()
)
letterA<-cbind(c(rep("D",4),rep("W",4)),rep(levels(as.factor(Netphotosynthesisrate3 $crop)),2), letter)
letter<-letterA[match(paste0(Netphotosynthesisrate3 $domestication, Netphotosynthesisrate3 $crop), paste0(letterA[,1],letterA[,2])),3]
species<-paste0(Netphotosynthesisrate3 $domestication,"-", Netphotosynthesisrate3 $crop )
elongation_zone_size<-Netphotosynthesisrate3 $A
treatment<-Netphotosynthesisrate3 $treatment
data=data.frame(Netphotosynthesisrate3 $domestication, Netphotosynthesisrate3 $accession ,  Netphotosynthesisrate3 $crop,  treatment , elongation_zone_size, letter, species)
 pd <- position_dodge(0.8) #
# grouped boxplot
data$treatment[which(data$treatment=="d")]<-"Drought"
data$treatment[which(data$treatment=="c")]<-"Control"
data$species[which(data$species =="D-barley")]<-"Barley-Domesticated"
data$species[which(data$species =="W-barley")]<-"Barley-Wild"
data$species[which(data$species =="W-bd21")]<-"Bd21"
data$species[which(data$species =="D-wheat")]<-"Wheat-Domesticated"
data$species[which(data$species =="W-wheat")]<-"Wheat-Wild"
data$species[which(data$species =="D-oat")]<-"Oat-Domesticated"
data$species[which(data$species =="W-oat")]<-"Oat-Wild"
ggplot(data, aes(x= factor(species, level=c("Barley-Domesticated","Barley-Wild","Wheat-Domesticated","Wheat-Wild","Oat-Domesticated","Oat-Wild","Bd21")), y= elongation_zone_size, fill= treatment)) + 
    geom_boxplot()+geom_point(position=pd)+ geom_text(
    aes(label = letter, y= 20000, group= species),color="black", position =  position_dodge(width=0), size=6)+xlab("Species")+ylab(expression(paste("Elongation zone size (", mu, "m)")))+ scale_color_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+scale_fill_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+geom_rect(aes(xmin=0.5,xmax=1.5,ymin=-Inf, ymax=Inf),fill="grey",alpha=0.01)+geom_rect(aes(xmin=2.5,xmax=3.5,ymin=-Inf, ymax=Inf),fill="grey",alpha=0.01)+geom_rect(aes(xmin=4.5,xmax=5.5,ymin=-Inf, ymax=Inf),fill="grey",alpha=0.01)+ theme( axis.text.x = element_text(angle = 45, hjust = 1))







Netphotosynthesisrate3<-NULL
   microscopy<-read.csv("~/Downloads/microscopy_summary_6_28.csv", header=T)
elongation <-read.csv("~/Downloads/elongation_summary_python1_out.csv", header=T)
info <-read.csv("~/Downloads/elongation_summary_python2_6_28.csv", header=T)
info<-as.data.frame(t(info))
colnames(info)<-info[1,]
info<-info[-1,]
head(microscopy)
Netphotosynthesisrate3<-microscopy[,which(colnames(microscopy)%in%c("domestication", "accession", "crop","treatment", "Lge"))]
Netphotosynthesisrate3$A<-microscopy $divisionzonelist
#3.Netphotosynthesisrate3$A<-c(-0.1*Drydown_brachy $hydraulic.potential.1)
#4.Netphotosynthesisrate3$A<-Drydown_brachy $A
#5.Netphotosynthesisrate3$A<-Drydown_brachy $gsw
#6.Netphotosynthesisrate3$A<-(Drydown_brachy $fresh.shoot+Drydown_brachy $fresh.root)
#7.Netphotosynthesisrate3$A<-(Drydown_brachy $dry.shoot+Drydown_brachy $dry.root)
#8.Netphotosynthesisrate3$A<-(Drydown_brachy $dry.root/Drydown_brachy $dry.shoot)
#9.Netphotosynthesisrate3$A<-Drydown_brachy $glucose.nmol.Glc.equivalents.mg

Netphotosynthesisrate3 <-na.omit(Netphotosynthesisrate3)
res = fitData(na.omit(Netphotosynthesisrate3$A), fit=c("gamma","logistic","normal","exponential","poisson","exponential"),
    sample=1)
res
###
Netphotosynthesisrate3 $Netphotosynthesisrate<-as.numeric(na.omit(Netphotosynthesisrate3 $A))
Netphotosynthesisrate3 $domestication<-as.factor(Netphotosynthesisrate3 $domestication)
Netphotosynthesisrate3 $crop <-as.factor(Netphotosynthesisrate3 $crop)
Netphotosynthesisrate3 $Treatment <-as.factor(Netphotosynthesisrate3 $treatment)
Netphotosynthesisrate3 $accession <-as.factor(Netphotosynthesisrate3 $accession)
Netphotosynthesisrate34<-Netphotosynthesisrate3
Netphotosynthesisrate3<-Netphotosynthesisrate34[which((Netphotosynthesisrate34 $treatment=="c")&(Netphotosynthesisrate34 $crop!="bd21")),]
options(na.action = "na.fail")
f<-lm(A ~ crop* domestication, data= Netphotosynthesisrate3)

dredge(f, rank="AIC",extra = c("R^2", adjRsq=function(x) summary(x)$adj.r.squared))

a<-lmer(A ~ crop+ domestication+(1|accession), data= Netphotosynthesisrate3)
b<-lmer(A ~ crop* domestication+(1|accession), data= Netphotosynthesisrate3)
c<-lmer(A ~ crop+(1|accession), data= Netphotosynthesisrate3)
d<-lmer(A ~ domestication+(1|accession), data= Netphotosynthesisrate3)
e<-lm(A ~ crop+ domestication, data= Netphotosynthesisrate3)
f<-lm(A ~ crop* domestication, data= Netphotosynthesisrate3)
g<-lm(A ~ crop, data= Netphotosynthesisrate3)
h<-lm(A ~ domestication, data= Netphotosynthesisrate3)

AIC(a,b,c,d,e,f,g,h)[order(AIC(a,b,c,d,e,f,g,h)[,2]),]

gm_mc <- emmeans(e, ~ domestication | crop, ddf="kenward-roger")
Hf_Aarea_lsmeans <-pairs(gm_mc)
Hf_Aarea_lsmeans 
letter<-Hf_Aarea_lsmeans
letter<-as.data.frame(letter)
letter <- symnum(letter[,dim(letter)[2]], corr = FALSE, na = FALSE, cutpoints = c(0, 
    0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", " "))        
theme_set(
  theme_bw()
)
letterA<-cbind(c("barley","oat","wheat"), letter)
letter<-letterA[match(paste0(Netphotosynthesisrate3 $crop), paste0(letterA[,1])),2]
species<-paste0(Netphotosynthesisrate3 $domestication,"-", Netphotosynthesisrate3 $crop )
division_zone_size<-Netphotosynthesisrate3 $A
treatment<-Netphotosynthesisrate3 $treatment
crop<-Netphotosynthesisrate3 $crop
domestication<-Netphotosynthesisrate3 $domestication
data=data.frame(domestication, Netphotosynthesisrate3 $accession ,  crop,  treatment , division_zone_size, letter, species)
 pd <- position_dodge(0.8) #
 data<-as.matrix(data )
 data[which(data[,1]=="W"),1]<-"Wild"
data[which(data[,1]=="D"),1]<-"Domesticated"
data[which(data[,3] =="barley"),3]<-"Barley"
data[which(data[,3] =="oat"),3]<-"Oat"
data[which(data[,3] =="wheat"),3]<-"Wheat"
data<-as.data.frame(data)
write.csv(data,"~/Downloads/tmp.csv")
data<-read.csv("~/Downloads/tmp.csv", header=T,row.names=1)

# grouped boxplot
ggplot(data, aes(x= factor(data$crop, level=c("Barley","Oat","Wheat")), y= division_zone_size, fill= domestication)) + 
    geom_boxplot()+geom_point(position=pd)+ geom_text(
    aes(label = letter, y= 5000, group= crop),color="black", position =  position_dodge(width=0), size=6)+xlab("crop")+ylab(expression(paste("Division zone size (", mu, "m)")))+ scale_color_manual(values = c("Wild" = "blue", "Domesticated" = "red"))+scale_fill_manual(values = c("Wild" = "blue", "Domesticated" = "red"))
# Plot the boxplot comparing domesticated vs wild for each crop
t.test(Netphotosynthesisrate3$A[which((Netphotosynthesisrate3 $crop=="oat")&(Netphotosynthesisrate3 $domestication =="Wild")&(Netphotosynthesisrate3 $Treatment =="c"))],Netphotosynthesisrate3$A[which((Netphotosynthesisrate3 $crop=="oat")&(Netphotosynthesisrate3 $domestication =="D")&(Netphotosynthesisrate3 $Treatment =="c"))])






