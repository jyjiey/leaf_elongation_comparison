

nnn=2000

listall<-listall2<-NULL
mm=3
for(mm in c(1,3)){

a<-list<-list2<-NULL
bd21_m<-bd21_m2<-bd21_e<-NULL
microscopy<-read.csv("~/Desktop/chapter3final/microscopy_summary_2.csv", header=T)
elongation <-read.csv("~/Desktop/chapter3final/elongation_summary_models.csv", header=T)
info <-read.csv("~/Desktop/chapter3final/elongation_summary_info.csv", header=T)
info<-as.data.frame(t(info))
colnames(info)<-info[1,]
info<-info[-1,]
name2<-levels(as.factor(substr(microscopy $species,1,4)))[mm]
domestication <-as.factor(info $domestication)[which((substr(info$accession,1,4) == name2)&(info$treatment=="c"))]

bd21_e<-cbind(domestication,elongation[which((substr(info$accession,1,4) == name2)&(info$treatment=="c")),c(5,10,14)])
name1<-levels(as.factor(substr(microscopy $species,1,4)))[mm]

bd21_m<-microscopy[which((substr(microscopy $species,1,4) == name1)&(microscopy$treatment=="c")),c(1,11,5,6,13,14,15)]
colnames(bd21_m)[1]<-"domestication"
bd21_m <-na.omit(bd21_m)
bd21_e$der
bd21_e$treatment
##elongation rate
a<-summary(lm(der~ domestication,data= bd21_e))$coefficient[2,c(1,4)]
model<-lm(der~ domestication,data= bd21_e)
# Calculate Cook's Distance
cooks_distance <- cooks.distance(model)
# Set a threshold for influential points (often 4/n, where n is the number of data points)
threshold <- 4 / nrow(bd21_e)
influential_points <- cooks_distance > threshold
# Filter out influential points
bd21_e_filtered <- bd21_e[!influential_points, ]
modelfilter<-lm(der~ domestication,data= bd21_e_filtered)
a<-summary(modelfilter)$coefficient[2,c(1,4)]
list<-c(list,a[2])
intercept <- coef(modelfilter)[1]  # beta_0 (mean for control)
treatment_coef <- coef(modelfilter)[2]  # beta_1 (effect of drought)
proportion_change <- treatment_coef / intercept
list2<-c(list2, -proportion_change)
##elongation duration
a<-summary(lm(l10_90~ domestication,data= bd21_e))$coefficient[2,c(1,4)]
model<-lm(l10_90 ~ domestication,data= bd21_e)
# Calculate Cook's Distance
cooks_distance <- cooks.distance(model)
# Set a threshold for influential points (often 4/n, where n is the number of data points)
threshold <- 4 / nrow(bd21_e)
influential_points <- cooks_distance > threshold
# Filter out influential points
bd21_e_filtered <- bd21_e[!influential_points, ]
modelfilter<-lm(l10_90 ~ domestication,data= bd21_e_filtered)
a<-summary(modelfilter)$coefficient[2,c(1,4)]
list<-c(list,a[2])
intercept <- coef(modelfilter)[1]  # beta_0 (mean for control)
treatment_coef <- coef(modelfilter)[2]  # beta_1 (effect of drought)
proportion_change <- treatment_coef / intercept
list2<-c(list2, -proportion_change)
a<-summary(lm(lf~ domestication,data= bd21_e))$coefficient[2,c(1,4)]
model<-lm(lf ~ domestication,data= bd21_e)
# Calculate Cook's Distance
cooks_distance <- cooks.distance(model)
# Set a threshold for influential points (often 4/n, where n is the number of data points)
threshold <- 4 / nrow(bd21_e)
influential_points <- cooks_distance > threshold
# Filter out influential points
bd21_e_filtered <- bd21_e[!influential_points, ]
modelfilter<-lm(lf ~ domestication,data= bd21_e_filtered)
a<-summary(modelfilter)$coefficient[2,c(1,4)]
list<-c(list,a[2])
intercept <- coef(modelfilter)[1]  # beta_0 (mean for control)
treatment_coef <- coef(modelfilter)[2]  # beta_1 (effect of drought)
proportion_change <- treatment_coef / intercept
list2<-c(list2, -proportion_change)

bd21_e_summary<-cbind(aggregate(bd21_e $l10_90,list(bd21_e$domestication),mean),aggregate(bd21_e$der,list(bd21_e$domestication),mean)[,2],aggregate(bd21_e$lf,list(bd21_e$domestication),mean)[,2])

colnames(bd21_e_summary)<-c("","duration","rate","lm")
print(bd21_e_summary)
print(name1)
bd21_m<-cbind(bd21_m,bd21_e_summary[match(bd21_m$domestication,bd21_e_summary[,1]),])


a<-NaN
bd21_m$lf <-as.numeric(bd21_m$lf)
##lf
if(0%in%bd21_m$lf){
bd21_m_no0<-bd21_m[-which(bd21_m$lf==0),]
}else{
	bd21_m_no0<-bd21_m
	}
a<-summary(lm(lf ~ domestication,data= as.data.frame(na.omit(bd21_m_no0))))$coefficient[2,c(1,4)]
model<-lm(lf ~ domestication,data= as.data.frame(na.omit(bd21_m_no0)))
# Calculate Cook's Distance
cooks_distance <- cooks.distance(model)
# Set a threshold for influential points (often 4/n, where n is the number of data points)
threshold <- 4 / nrow(as.data.frame(na.omit(bd21_m_no0)))
influential_points <- cooks_distance > threshold
# Filter out influential points
bd21_m_no0_filtered <- as.data.frame(na.omit(bd21_m_no0))[!influential_points, ]
modelfilter<-lm(lf ~ domestication,data= bd21_m_no0_filtered)
a<-summary(modelfilter)$coefficient[2,c(1,4)]
list<-c(list,a[2])
intercept <- coef(modelfilter)[1]  # beta_0 (mean for control)
treatment_coef <- coef(modelfilter)[2]  # beta_1 (effect of drought)
proportion_change <- treatment_coef / intercept
list2<-c(list2, -proportion_change)


##n_all****

#bd21_m_no0 $all<-bd21_m_no0 $lm/bd21_m_no0 $lf


permutation<-cbind(c(rep("W",nnn),rep("D",nnn)),c(sample(bd21_e $lf[which(bd21_e $domestication%in%c("W","w"))],nnn,replace = TRUE)/sample(bd21_m_no0 $lf[which(bd21_m_no0 $domestication %in%c("w","W"))],nnn,replace = TRUE), sample(bd21_e $lf[which(bd21_e $domestication%in%c("d","D"))],nnn,replace = TRUE)/sample(bd21_m_no0 $lf[which(bd21_m_no0 $domestication%in%c("d","D"))],nnn,replace = TRUE)))

colnames(permutation)<-c("domestication","lf")
permutation<-as.data.frame(permutation)
permutation $lf <-as.numeric(permutation $lf)
a<-summary(lm(lf ~ domestication,data= permutation))$coefficient[2,c(1,4)]
model<-lm(lf ~ domestication,data= permutation)
# Calculate Cook's Distance
cooks_distance <- cooks.distance(model)
# Set a threshold for influential points (often 4/n, where n is the number of data points)
threshold <- 4 / nrow(permutation)
influential_points <- cooks_distance > threshold
# Filter out influential points
permutation_filtered <- permutation[!influential_points, ]
modelfilter<-lm(lf ~ domestication,data= permutation_filtered)
a<-summary(modelfilter)$coefficient[2,c(1,4)]
list<-c(list,a[2])
intercept <- coef(modelfilter)[1]  # beta_0 (mean for control)
treatment_coef <- coef(modelfilter)[2]  # beta_1 (effect of drought)
proportion_change <- treatment_coef / intercept
list2<-c(list2, -proportion_change)


permutation<-cbind(c(rep("W",nnn),rep("D",nnn)),c(sample(bd21_e $der[which(bd21_e $domestication%in%c("W","w"))],nnn,replace = TRUE)/sample(bd21_m_no0 $lf[which(bd21_m_no0 $domestication %in%c("w","W"))],nnn,replace = TRUE), sample(bd21_e $der[which(bd21_e $domestication%in%c("d","D"))],nnn,replace = TRUE)/sample(bd21_m_no0 $lf[which(bd21_m_no0 $domestication%in%c("d","D"))],nnn,replace = TRUE)))

colnames(permutation)<-c("domestication","lf")
permutation<-as.data.frame(permutation)
permutation $lf <-as.numeric(permutation $lf)
a<-summary(lm(lf ~ domestication,data= permutation))$coefficient[2,c(1,4)]
model<-lm(lf ~ domestication,data= permutation)
# Calculate Cook's Distance
cooks_distance <- cooks.distance(model)
# Set a threshold for influential points (often 4/n, where n is the number of data points)
threshold <- 4 / nrow(permutation)
influential_points <- cooks_distance > threshold
# Filter out influential points
permutation_filtered <- permutation[!influential_points, ]
modelfilter<-lm(lf ~ domestication,data= permutation_filtered)
a<-summary(modelfilter)$coefficient[2,c(1,4)]
list<-c(list,a[2])
intercept <- coef(modelfilter)[1]  # beta_0 (mean for control)
treatment_coef <- coef(modelfilter)[2]  # beta_1 (effect of drought)
proportion_change <- treatment_coef / intercept
list2<-c(list2, -proportion_change)


##division
a<-summary(lm(divisionzonelist ~ domestication,data= as.data.frame(na.omit(bd21_m))))$coefficient[2,c(1,4)]
model<-lm(divisionzonelist ~ domestication,data= as.data.frame(na.omit(bd21_m)))

# Calculate Cook's Distance
cooks_distance <- cooks.distance(model)
# Set a threshold for influential points (often 4/n, where n is the number of data points)
threshold <- 4 / nrow(as.data.frame(na.omit(bd21_m)))
influential_points <- cooks_distance > threshold
# Filter out influential points
bd21_m_filtered <- as.data.frame(na.omit(bd21_m))[!influential_points, ]
modelfilter<-lm(divisionzonelist ~ domestication,data= bd21_m_filtered)
a<-summary(modelfilter)$coefficient[2,c(1,4)]
list<-c(list,a[2])
intercept <- coef(modelfilter)[1]  # beta_0 (mean for control)
treatment_coef <- coef(modelfilter)[2]  # beta_1 (effect of drought)
proportion_change <- treatment_coef / intercept
list2<-c(list2, -proportion_change)




bd21_m_no0 $divisioncellsize <-as.numeric(bd21_m_no0 $divisioncellsize)
a<-NaN
##e
a<-summary(lm(divisioncellsize ~ domestication,data= as.data.frame(na.omit(bd21_m_no0))))$coefficient[2,c(1,4)]
model<-lm(divisioncellsize ~ domestication,data= as.data.frame(na.omit(bd21_m_no0)))

# Calculate Cook's Distance
cooks_distance <- cooks.distance(model)
# Set a threshold for influential points (often 4/n, where n is the number of data points)
threshold <- 4 / nrow(as.data.frame(na.omit(bd21_m_no0)))
influential_points <- cooks_distance > threshold
# Filter out influential points
bd21_m_no0_filtered <- as.data.frame(na.omit(bd21_m_no0))[!influential_points, ]
modelfilter<-lm(divisioncellsize ~ domestication,data= bd21_m_no0_filtered)
a<-summary(modelfilter)$coefficient[2,c(1,4)]
list<-c(list,a[2])
intercept <- coef(modelfilter)[1]  # beta_0 (mean for control)
treatment_coef <- coef(modelfilter)[2]  # beta_1 (effect of drought)
proportion_change <- treatment_coef / intercept
list2<-c(list2, -proportion_change)

##Llge


##Ndiv
bd21_m_no0 $Ndiv <-bd21_m_no0 $divisionzonelist/bd21_m_no0 $divisioncellsize
a<-summary(lm(Ndiv ~ domestication,data= as.data.frame(na.omit(bd21_m_no0))))$coefficient[2,c(1,4)]
model<-lm(Ndiv ~ domestication,data= as.data.frame(na.omit(bd21_m_no0)))
# Calculate Cook's Distance
cooks_distance <- cooks.distance(model)
# Set a threshold for influential points (often 4/n, where n is the number of data points)
threshold <- 4 / nrow(as.data.frame(na.omit(bd21_m_no0)))
influential_points <- cooks_distance > threshold
# Filter out influential points
bd21_m_no0_filtered <- as.data.frame(na.omit(bd21_m_no0))[!influential_points, ]
modelfilter<-lm(Ndiv ~ domestication,data= bd21_m_no0_filtered)
a<-summary(modelfilter)$coefficient[2,c(1,4)]
list<-c(list,a[2])
intercept <- coef(modelfilter)[1]  # beta_0 (mean for control)
treatment_coef <- coef(modelfilter)[2]  # beta_1 (effect of drought)
proportion_change <- treatment_coef / intercept
list2<-c(list2, -proportion_change)




##Ddiv*****
#bd21_m_no0 $Ddiv <-bd21_m_no0 $rate/(bd21_m_no0 $lf*bd21_m_no0 $Ndiv)




permutation<-cbind(c(rep("W",nnn),rep("D",nnn)),c(sample(bd21_e $der[which(bd21_e $domestication%in%c("W","w"))],nnn,replace = TRUE)/sample((bd21_m_no0 $lf*bd21_m_no0 $Ndiv)[which(bd21_m_no0 $domestication %in%c("w","W"))],nnn,replace = TRUE), sample(bd21_e $der[which(bd21_e $domestication%in%c("d","D"))],nnn,replace = TRUE)/sample((bd21_m_no0 $lf*bd21_m_no0 $Ndiv)[which(bd21_m_no0 $domestication%in%c("d","D"))],nnn,replace = TRUE)))

colnames(permutation)<-c("domestication","lf")
permutation<-as.data.frame(permutation)
permutation $lf <-as.numeric(permutation $lf)
a<-summary(lm(lf ~ domestication,data= permutation))$coefficient[2,c(1,4)]
model<-lm(lf ~ domestication,data= permutation)
# Calculate Cook's Distance
cooks_distance <- cooks.distance(model)
# Set a threshold for influential points (often 4/n, where n is the number of data points)
threshold <- 4 / nrow(permutation)
influential_points <- cooks_distance > threshold
# Filter out influential points
permutation_filtered <- permutation[!influential_points, ]
modelfilter<-lm(lf ~ domestication,data= permutation_filtered)
a<-summary(modelfilter)$coefficient[2,c(1,4)]
list<-c(list,a[2])
intercept <- coef(modelfilter)[1]  # beta_0 (mean for control)
treatment_coef <- coef(modelfilter)[2]  # beta_1 (effect of drought)
proportion_change <- treatment_coef / intercept
list2<-c(list2, -proportion_change)





###
bd21_m_no0 $Lge <-as.numeric(bd21_m_no0 $Lge)
a<-summary(lm(Lge ~ domestication,data= as.data.frame(na.omit(bd21_m_no0))))$coefficient[2,c(1,4)]
model<-lm(Lge ~ domestication,data= as.data.frame(na.omit(bd21_m_no0)))
# Calculate Cook's Distance
cooks_distance <- cooks.distance(model)
# Set a threshold for influential points (often 4/n, where n is the number of data points)
threshold <- 4 / nrow(as.data.frame(na.omit(bd21_m_no0)))
influential_points <- cooks_distance > threshold
# Filter out influential points
bd21_m_no0_filtered <- as.data.frame(na.omit(bd21_m_no0))[!influential_points, ]
modelfilter<-lm(Lge ~ domestication,data= bd21_m_no0_filtered)
a<-summary(modelfilter)$coefficient[2,c(1,4)]
list<-c(list,a[2])
intercept <- coef(modelfilter)[1]  # beta_0 (mean for control)
treatment_coef <- coef(modelfilter)[2]  # beta_1 (effect of drought)
proportion_change <- treatment_coef / intercept
list2<-c(list2, -proportion_change)


##ne
bd21_m_no0 $N <-as.numeric(bd21_m_no0 $N)
a<-summary(lm(N ~ domestication,data= as.data.frame(na.omit(bd21_m_no0))))$coefficient[2,c(1,4)]
model<-lm(N ~ domestication,data= as.data.frame(na.omit(bd21_m_no0)))
# Calculate Cook's Distance
cooks_distance <- cooks.distance(model)
# Set a threshold for influential points (often 4/n, where n is the number of data points)
threshold <- 4 / nrow(as.data.frame(na.omit(bd21_m_no0)))
influential_points <- cooks_distance > threshold
# Filter out influential points
bd21_m_no0_filtered <- as.data.frame(na.omit(bd21_m_no0))[!influential_points, ]
modelfilter<-lm(N ~ domestication,data= bd21_m_no0_filtered)
a<-summary(modelfilter)$coefficient[2,c(1,4)]
list<-c(list,a[2])
intercept <- coef(modelfilter)[1]  # beta_0 (mean for control)
treatment_coef <- coef(modelfilter)[2]  # beta_1 (effect of drought)
proportion_change <- treatment_coef / intercept
list2<-c(list2, -proportion_change)##E_rate****
#bd21_m_no0 $E_rate <-((bd21_m_no0 $lf-bd21_m_no0 $divisioncellsize)/bd21_m_no0 $lf/bd21_m_no0 $Lge)*bd21_m_no0 $rate


permutation<-cbind(c(rep("W",nnn),rep("D",nnn)),c(sample(((bd21_m_no0 $lf-bd21_m_no0 $divisioncellsize)/bd21_m_no0 $lf/bd21_m_no0 $Lge)[which(bd21_m_no0 $domestication%in%c("W","w"))],nnn,replace = TRUE)*sample(bd21_e $der[which(bd21_e $domestication %in%c("w","W"))],nnn,replace = TRUE), sample(((bd21_m_no0 $lf-bd21_m_no0 $divisioncellsize)/bd21_m_no0 $lf/bd21_m_no0 $Lge)[which(bd21_m_no0 $domestication%in%c("d","D"))],nnn,replace = TRUE)*sample(bd21_e $der[which(bd21_e $domestication %in%c("D","d"))],nnn,replace = TRUE)))

colnames(permutation)<-c("domestication","lf")
permutation<-as.data.frame(permutation)
permutation $lf <-as.numeric(permutation $lf)
a<-summary(lm(lf ~ domestication,data= permutation))$coefficient[2,c(1,4)]
model<-lm(lf ~ domestication,data= permutation)
# Calculate Cook's Distance
cooks_distance <- cooks.distance(model)
# Set a threshold for influential points (often 4/n, where n is the number of data points)
threshold <- 4 / nrow(permutation)
influential_points <- cooks_distance > threshold
# Filter out influential points
permutation_filtered <- permutation[!influential_points, ]
modelfilter<-lm(lf ~ domestication,data= permutation_filtered)
a<-summary(modelfilter)$coefficient[2,c(1,4)]
list<-c(list,a[2])
intercept <- coef(modelfilter)[1]  # beta_0 (mean for control)
treatment_coef <- coef(modelfilter)[2]  # beta_1 (effect of drought)
proportion_change <- treatment_coef / intercept
list2<-c(list2, -proportion_change)

permutation<-cbind(c(rep("W",nnn),rep("D",nnn)),c(sample((bd21_m_no0 $N *bd21_m_no0 $lf)[which(bd21_m_no0 $domestication%in%c("W","w"))],nnn,replace = TRUE)/sample((bd21_e $der)[which(bd21_e $domestication %in%c("w","W"))],nnn,replace = TRUE), sample((bd21_m_no0 $N *bd21_m_no0 $lf)[which(bd21_m_no0 $domestication%in%c("d","D"))],nnn,replace = TRUE)/sample((bd21_e $der)[which(bd21_e $domestication %in%c("D","d"))],nnn,replace = TRUE)))

colnames(permutation)<-c("domestication","lf")
permutation<-as.data.frame(permutation)
permutation $lf <-as.numeric(permutation $lf)
a<-summary(lm(lf ~ domestication,data= permutation))$coefficient[2,c(1,4)]
model<-lm(lf ~ domestication,data= permutation)
# Calculate Cook's Distance
cooks_distance <- cooks.distance(model)
# Set a threshold for influential points (often 4/n, where n is the number of data points)
threshold <- 4 / nrow(permutation)
influential_points <- cooks_distance > threshold
# Filter out influential points
permutation_filtered <- permutation[!influential_points, ]
modelfilter<-lm(lf ~ domestication,data= permutation_filtered)
a<-summary(modelfilter)$coefficient[2,c(1,4)]
list<-c(list,a[2])
intercept <- coef(modelfilter)[1]  # beta_0 (mean for control)
treatment_coef <- coef(modelfilter)[2]  # beta_1 (effect of drought)
proportion_change <- treatment_coef / intercept
list2<-c(list2, -proportion_change)




listall<-cbind(listall,list)
listall2<-cbind(listall2,list2)




}
mm=4
a<-list<-list2<-NULL
bd21_m<-bd21_m2<-bd21_e<-NULL
microscopy<-read.csv("~/Desktop/chapter3final/microscopy_summary_2.csv", header=T)
elongation <-read.csv("~/Desktop/chapter3final/elongation_summary_models.csv", header=T)
info <-read.csv("~/Desktop/chapter3final/elongation_summary_info.csv", header=T)
info<-as.data.frame(t(info))
colnames(info)<-info[1,]
info<-info[-1,]
name2<-c("Wheat-L-", "Wheat-Z-")
domestication <-as.factor(info $domestication)[which((substr(info$accession,1,8) %in%name2)&(info$treatment=="c"))]

bd21_e<-cbind(domestication,elongation[which((substr(info$accession,1,8) %in%name2)&(info$treatment=="c")),c(5,10,14)])
name1<-levels(as.factor(substr(microscopy $species,1,8)))[mm]
name1<-c("Wheat-L-", "Wheat-Z-")
bd21_m<-microscopy[which((substr(microscopy $species,1,8)%in% name1)&(microscopy$treatment=="c")),c(1,11,5,6,13,14,15)]
colnames(bd21_m)[1]<-"domestication"
bd21_m <-na.omit(bd21_m)
bd21_e$der
bd21_e$treatment
##elongation rate
a<-summary(lm(der~ domestication,data= bd21_e))$coefficient[2,c(1,4)]
model<-lm(der~ domestication,data= bd21_e)
# Calculate Cook's Distance
cooks_distance <- cooks.distance(model)
# Set a threshold for influential points (often 4/n, where n is the number of data points)
threshold <- 4 / nrow(bd21_e)
influential_points <- cooks_distance > threshold
# Filter out influential points
bd21_e_filtered <- bd21_e[!influential_points, ]
modelfilter<-lm(der~ domestication,data= bd21_e_filtered)
a<-summary(modelfilter)$coefficient[2,c(1,4)]
list<-c(list,a[2])
intercept <- coef(modelfilter)[1]  # beta_0 (mean for control)
treatment_coef <- coef(modelfilter)[2]  # beta_1 (effect of drought)
proportion_change <- treatment_coef / intercept
list2<-c(list2, -proportion_change)
##elongation duration
a<-summary(lm(l10_90~ domestication,data= bd21_e))$coefficient[2,c(1,4)]
model<-lm(l10_90 ~ domestication,data= bd21_e)
# Calculate Cook's Distance
cooks_distance <- cooks.distance(model)
# Set a threshold for influential points (often 4/n, where n is the number of data points)
threshold <- 4 / nrow(bd21_e)
influential_points <- cooks_distance > threshold
# Filter out influential points
bd21_e_filtered <- bd21_e[!influential_points, ]
modelfilter<-lm(l10_90 ~ domestication,data= bd21_e_filtered)
a<-summary(modelfilter)$coefficient[2,c(1,4)]
list<-c(list,a[2])
intercept <- coef(modelfilter)[1]  # beta_0 (mean for control)
treatment_coef <- coef(modelfilter)[2]  # beta_1 (effect of drought)
proportion_change <- treatment_coef / intercept
list2<-c(list2, -proportion_change)
a<-summary(lm(lf~ domestication,data= bd21_e))$coefficient[2,c(1,4)]
model<-lm(lf ~ domestication,data= bd21_e)
# Calculate Cook's Distance
cooks_distance <- cooks.distance(model)
# Set a threshold for influential points (often 4/n, where n is the number of data points)
threshold <- 4 / nrow(bd21_e)
influential_points <- cooks_distance > threshold
# Filter out influential points
bd21_e_filtered <- bd21_e[!influential_points, ]
modelfilter<-lm(lf ~ domestication,data= bd21_e_filtered)
a<-summary(modelfilter)$coefficient[2,c(1,4)]
list<-c(list,a[2])
intercept <- coef(modelfilter)[1]  # beta_0 (mean for control)
treatment_coef <- coef(modelfilter)[2]  # beta_1 (effect of drought)
proportion_change <- treatment_coef / intercept
list2<-c(list2, -proportion_change)

bd21_e_summary<-cbind(aggregate(bd21_e $l10_90,list(bd21_e$domestication),mean),aggregate(bd21_e$der,list(bd21_e$domestication),mean)[,2],aggregate(bd21_e$lf,list(bd21_e$domestication),mean)[,2])

colnames(bd21_e_summary)<-c("","duration","rate","lm")
print(bd21_e_summary)
print(name1)
bd21_m<-cbind(bd21_m,bd21_e_summary[match(bd21_m$domestication,bd21_e_summary[,1]),])


a<-NaN
bd21_m$lf <-as.numeric(bd21_m$lf)
##lf
if(0%in%bd21_m$lf){
bd21_m_no0<-bd21_m[-which(bd21_m$lf==0),]
}else{
	bd21_m_no0<-bd21_m
	}
a<-summary(lm(lf ~ domestication,data= as.data.frame(na.omit(bd21_m_no0))))$coefficient[2,c(1,4)]
model<-lm(lf ~ domestication,data= as.data.frame(na.omit(bd21_m_no0)))
# Calculate Cook's Distance
cooks_distance <- cooks.distance(model)
# Set a threshold for influential points (often 4/n, where n is the number of data points)
threshold <- 4 / nrow(as.data.frame(na.omit(bd21_m_no0)))
influential_points <- cooks_distance > threshold
# Filter out influential points
bd21_m_no0_filtered <- as.data.frame(na.omit(bd21_m_no0))[!influential_points, ]
modelfilter<-lm(lf ~ domestication,data= bd21_m_no0_filtered)
a<-summary(modelfilter)$coefficient[2,c(1,4)]
list<-c(list,a[2])
intercept <- coef(modelfilter)[1]  # beta_0 (mean for control)
treatment_coef <- coef(modelfilter)[2]  # beta_1 (effect of drought)
proportion_change <- treatment_coef / intercept
list2<-c(list2, -proportion_change)


##n_all****

#bd21_m_no0 $all<-bd21_m_no0 $lm/bd21_m_no0 $lf


permutation<-cbind(c(rep("W",nnn),rep("D",nnn)),c(sample(bd21_e $lf[which(bd21_e $domestication%in%c("W","w"))],nnn,replace = TRUE)/sample(bd21_m_no0 $lf[which(bd21_m_no0 $domestication %in%c("w","W"))],nnn,replace = TRUE), sample(bd21_e $lf[which(bd21_e $domestication%in%c("d","D"))],nnn,replace = TRUE)/sample(bd21_m_no0 $lf[which(bd21_m_no0 $domestication%in%c("d","D"))],nnn,replace = TRUE)))

colnames(permutation)<-c("domestication","lf")
permutation<-as.data.frame(permutation)
permutation $lf <-as.numeric(permutation $lf)
a<-summary(lm(lf ~ domestication,data= permutation))$coefficient[2,c(1,4)]
model<-lm(lf ~ domestication,data= permutation)
# Calculate Cook's Distance
cooks_distance <- cooks.distance(model)
# Set a threshold for influential points (often 4/n, where n is the number of data points)
threshold <- 4 / nrow(permutation)
influential_points <- cooks_distance > threshold
# Filter out influential points
permutation_filtered <- permutation[!influential_points, ]
modelfilter<-lm(lf ~ domestication,data= permutation_filtered)
a<-summary(modelfilter)$coefficient[2,c(1,4)]
list<-c(list,a[2])
intercept <- coef(modelfilter)[1]  # beta_0 (mean for control)
treatment_coef <- coef(modelfilter)[2]  # beta_1 (effect of drought)
proportion_change <- treatment_coef / intercept
list2<-c(list2, -proportion_change)


permutation<-cbind(c(rep("W",nnn),rep("D",nnn)),c(sample(bd21_e $der[which(bd21_e $domestication%in%c("W","w"))],nnn,replace = TRUE)/sample(bd21_m_no0 $lf[which(bd21_m_no0 $domestication %in%c("w","W"))],nnn,replace = TRUE), sample(bd21_e $der[which(bd21_e $domestication%in%c("d","D"))],nnn,replace = TRUE)/sample(bd21_m_no0 $lf[which(bd21_m_no0 $domestication%in%c("d","D"))],nnn,replace = TRUE)))

colnames(permutation)<-c("domestication","lf")
permutation<-as.data.frame(permutation)
permutation $lf <-as.numeric(permutation $lf)
a<-summary(lm(lf ~ domestication,data= permutation))$coefficient[2,c(1,4)]
model<-lm(lf ~ domestication,data= permutation)
# Calculate Cook's Distance
cooks_distance <- cooks.distance(model)
# Set a threshold for influential points (often 4/n, where n is the number of data points)
threshold <- 4 / nrow(permutation)
influential_points <- cooks_distance > threshold
# Filter out influential points
permutation_filtered <- permutation[!influential_points, ]
modelfilter<-lm(lf ~ domestication,data= permutation_filtered)
a<-summary(modelfilter)$coefficient[2,c(1,4)]
list<-c(list,a[2])
intercept <- coef(modelfilter)[1]  # beta_0 (mean for control)
treatment_coef <- coef(modelfilter)[2]  # beta_1 (effect of drought)
proportion_change <- treatment_coef / intercept
list2<-c(list2, -proportion_change)


##division
a<-summary(lm(divisionzonelist ~ domestication,data= as.data.frame(na.omit(bd21_m))))$coefficient[2,c(1,4)]
model<-lm(divisionzonelist ~ domestication,data= as.data.frame(na.omit(bd21_m)))

# Calculate Cook's Distance
cooks_distance <- cooks.distance(model)
# Set a threshold for influential points (often 4/n, where n is the number of data points)
threshold <- 4 / nrow(as.data.frame(na.omit(bd21_m)))
influential_points <- cooks_distance > threshold
# Filter out influential points
bd21_m_filtered <- as.data.frame(na.omit(bd21_m))[!influential_points, ]
modelfilter<-lm(divisionzonelist ~ domestication,data= bd21_m_filtered)
a<-summary(modelfilter)$coefficient[2,c(1,4)]
list<-c(list,a[2])
intercept <- coef(modelfilter)[1]  # beta_0 (mean for control)
treatment_coef <- coef(modelfilter)[2]  # beta_1 (effect of drought)
proportion_change <- treatment_coef / intercept
list2<-c(list2, -proportion_change)




bd21_m_no0 $divisioncellsize <-as.numeric(bd21_m_no0 $divisioncellsize)
a<-NaN
##e
a<-summary(lm(divisioncellsize ~ domestication,data= as.data.frame(na.omit(bd21_m_no0))))$coefficient[2,c(1,4)]
model<-lm(divisioncellsize ~ domestication,data= as.data.frame(na.omit(bd21_m_no0)))

# Calculate Cook's Distance
cooks_distance <- cooks.distance(model)
# Set a threshold for influential points (often 4/n, where n is the number of data points)
threshold <- 4 / nrow(as.data.frame(na.omit(bd21_m_no0)))
influential_points <- cooks_distance > threshold
# Filter out influential points
bd21_m_no0_filtered <- as.data.frame(na.omit(bd21_m_no0))[!influential_points, ]
modelfilter<-lm(divisioncellsize ~ domestication,data= bd21_m_no0_filtered)
a<-summary(modelfilter)$coefficient[2,c(1,4)]
list<-c(list,a[2])
intercept <- coef(modelfilter)[1]  # beta_0 (mean for control)
treatment_coef <- coef(modelfilter)[2]  # beta_1 (effect of drought)
proportion_change <- treatment_coef / intercept
list2<-c(list2, -proportion_change)

##Llge


##Ndiv
bd21_m_no0 $Ndiv <-bd21_m_no0 $divisionzonelist/bd21_m_no0 $divisioncellsize
a<-summary(lm(Ndiv ~ domestication,data= as.data.frame(na.omit(bd21_m_no0))))$coefficient[2,c(1,4)]
model<-lm(Ndiv ~ domestication,data= as.data.frame(na.omit(bd21_m_no0)))
# Calculate Cook's Distance
cooks_distance <- cooks.distance(model)
# Set a threshold for influential points (often 4/n, where n is the number of data points)
threshold <- 4 / nrow(as.data.frame(na.omit(bd21_m_no0)))
influential_points <- cooks_distance > threshold
# Filter out influential points
bd21_m_no0_filtered <- as.data.frame(na.omit(bd21_m_no0))[!influential_points, ]
modelfilter<-lm(Ndiv ~ domestication,data= bd21_m_no0_filtered)
a<-summary(modelfilter)$coefficient[2,c(1,4)]
list<-c(list,a[2])
intercept <- coef(modelfilter)[1]  # beta_0 (mean for control)
treatment_coef <- coef(modelfilter)[2]  # beta_1 (effect of drought)
proportion_change <- treatment_coef / intercept
list2<-c(list2, -proportion_change)




##Ddiv*****
#bd21_m_no0 $Ddiv <-bd21_m_no0 $rate/(bd21_m_no0 $lf*bd21_m_no0 $Ndiv)




permutation<-cbind(c(rep("W",nnn),rep("D",nnn)),c(sample(bd21_e $der[which(bd21_e $domestication%in%c("W","w"))],nnn,replace = TRUE)/sample((bd21_m_no0 $lf*bd21_m_no0 $Ndiv)[which(bd21_m_no0 $domestication %in%c("w","W"))],nnn,replace = TRUE), sample(bd21_e $der[which(bd21_e $domestication%in%c("d","D"))],nnn,replace = TRUE)/sample((bd21_m_no0 $lf*bd21_m_no0 $Ndiv)[which(bd21_m_no0 $domestication%in%c("d","D"))],nnn,replace = TRUE)))

colnames(permutation)<-c("domestication","lf")
permutation<-as.data.frame(permutation)
permutation $lf <-as.numeric(permutation $lf)
a<-summary(lm(lf ~ domestication,data= permutation))$coefficient[2,c(1,4)]
model<-lm(lf ~ domestication,data= permutation)
# Calculate Cook's Distance
cooks_distance <- cooks.distance(model)
# Set a threshold for influential points (often 4/n, where n is the number of data points)
threshold <- 4 / nrow(permutation)
influential_points <- cooks_distance > threshold
# Filter out influential points
permutation_filtered <- permutation[!influential_points, ]
modelfilter<-lm(lf ~ domestication,data= permutation_filtered)
a<-summary(modelfilter)$coefficient[2,c(1,4)]
list<-c(list,a[2])
intercept <- coef(modelfilter)[1]  # beta_0 (mean for control)
treatment_coef <- coef(modelfilter)[2]  # beta_1 (effect of drought)
proportion_change <- treatment_coef / intercept
list2<-c(list2, -proportion_change)





###
bd21_m_no0 $Lge <-as.numeric(bd21_m_no0 $Lge)
a<-summary(lm(Lge ~ domestication,data= as.data.frame(na.omit(bd21_m_no0))))$coefficient[2,c(1,4)]
model<-lm(Lge ~ domestication,data= as.data.frame(na.omit(bd21_m_no0)))
# Calculate Cook's Distance
cooks_distance <- cooks.distance(model)
# Set a threshold for influential points (often 4/n, where n is the number of data points)
threshold <- 4 / nrow(as.data.frame(na.omit(bd21_m_no0)))
influential_points <- cooks_distance > threshold
# Filter out influential points
bd21_m_no0_filtered <- as.data.frame(na.omit(bd21_m_no0))[!influential_points, ]
modelfilter<-lm(Lge ~ domestication,data= bd21_m_no0_filtered)
a<-summary(modelfilter)$coefficient[2,c(1,4)]
list<-c(list,a[2])
intercept <- coef(modelfilter)[1]  # beta_0 (mean for control)
treatment_coef <- coef(modelfilter)[2]  # beta_1 (effect of drought)
proportion_change <- treatment_coef / intercept
list2<-c(list2, -proportion_change)


##ne
bd21_m_no0 $N <-as.numeric(bd21_m_no0 $N)
a<-summary(lm(N ~ domestication,data= as.data.frame(na.omit(bd21_m_no0))))$coefficient[2,c(1,4)]
model<-lm(N ~ domestication,data= as.data.frame(na.omit(bd21_m_no0)))
# Calculate Cook's Distance
cooks_distance <- cooks.distance(model)
# Set a threshold for influential points (often 4/n, where n is the number of data points)
threshold <- 4 / nrow(as.data.frame(na.omit(bd21_m_no0)))
influential_points <- cooks_distance > threshold
# Filter out influential points
bd21_m_no0_filtered <- as.data.frame(na.omit(bd21_m_no0))[!influential_points, ]
modelfilter<-lm(N ~ domestication,data= bd21_m_no0_filtered)
a<-summary(modelfilter)$coefficient[2,c(1,4)]
list<-c(list,a[2])
intercept <- coef(modelfilter)[1]  # beta_0 (mean for control)
treatment_coef <- coef(modelfilter)[2]  # beta_1 (effect of drought)
proportion_change <- treatment_coef / intercept
list2<-c(list2, -proportion_change)##E_rate****
#bd21_m_no0 $E_rate <-((bd21_m_no0 $lf-bd21_m_no0 $divisioncellsize)/bd21_m_no0 $lf/bd21_m_no0 $Lge)*bd21_m_no0 $rate


permutation<-cbind(c(rep("W",nnn),rep("D",nnn)),c(sample(((bd21_m_no0 $lf-bd21_m_no0 $divisioncellsize)/bd21_m_no0 $lf/bd21_m_no0 $Lge)[which(bd21_m_no0 $domestication%in%c("W","w"))],nnn,replace = TRUE)*sample(bd21_e $der[which(bd21_e $domestication %in%c("w","W"))],nnn,replace = TRUE), sample(((bd21_m_no0 $lf-bd21_m_no0 $divisioncellsize)/bd21_m_no0 $lf/bd21_m_no0 $Lge)[which(bd21_m_no0 $domestication%in%c("d","D"))],nnn,replace = TRUE)*sample(bd21_e $der[which(bd21_e $domestication %in%c("D","d"))],nnn,replace = TRUE)))

colnames(permutation)<-c("domestication","lf")
permutation<-as.data.frame(permutation)
permutation $lf <-as.numeric(permutation $lf)
a<-summary(lm(lf ~ domestication,data= permutation))$coefficient[2,c(1,4)]
model<-lm(lf ~ domestication,data= permutation)
# Calculate Cook's Distance
cooks_distance <- cooks.distance(model)
# Set a threshold for influential points (often 4/n, where n is the number of data points)
threshold <- 4 / nrow(permutation)
influential_points <- cooks_distance > threshold
# Filter out influential points
permutation_filtered <- permutation[!influential_points, ]
modelfilter<-lm(lf ~ domestication,data= permutation_filtered)
a<-summary(modelfilter)$coefficient[2,c(1,4)]
list<-c(list,a[2])
intercept <- coef(modelfilter)[1]  # beta_0 (mean for control)
treatment_coef <- coef(modelfilter)[2]  # beta_1 (effect of drought)
proportion_change <- treatment_coef / intercept
list2<-c(list2, -proportion_change)

permutation<-cbind(c(rep("W",nnn),rep("D",nnn)),c(sample((bd21_m_no0 $N *bd21_m_no0 $lf)[which(bd21_m_no0 $domestication%in%c("W","w"))],nnn,replace = TRUE)/sample((bd21_e $der)[which(bd21_e $domestication %in%c("w","W"))],nnn,replace = TRUE), sample((bd21_m_no0 $N *bd21_m_no0 $lf)[which(bd21_m_no0 $domestication%in%c("d","D"))],nnn,replace = TRUE)/sample((bd21_e $der)[which(bd21_e $domestication %in%c("D","d"))],nnn,replace = TRUE)))

colnames(permutation)<-c("domestication","lf")
permutation<-as.data.frame(permutation)
permutation $lf <-as.numeric(permutation $lf)
a<-summary(lm(lf ~ domestication,data= permutation))$coefficient[2,c(1,4)]
model<-lm(lf ~ domestication,data= permutation)
# Calculate Cook's Distance
cooks_distance <- cooks.distance(model)
# Set a threshold for influential points (often 4/n, where n is the number of data points)
threshold <- 4 / nrow(permutation)
influential_points <- cooks_distance > threshold
# Filter out influential points
permutation_filtered <- permutation[!influential_points, ]
modelfilter<-lm(lf ~ domestication,data= permutation_filtered)
a<-summary(modelfilter)$coefficient[2,c(1,4)]
list<-c(list,a[2])
intercept <- coef(modelfilter)[1]  # beta_0 (mean for control)
treatment_coef <- coef(modelfilter)[2]  # beta_1 (effect of drought)
proportion_change <- treatment_coef / intercept
list2<-c(list2, -proportion_change)




listall<-cbind(listall,list)
listall2<-cbind(listall2,list2)





mm=5
a<-list<-list2<-NULL
bd21_m<-bd21_m2<-bd21_e<-NULL
microscopy<-read.csv("~/Desktop/chapter3final/microscopy_summary_2.csv", header=T)
elongation <-read.csv("~/Desktop/chapter3final/elongation_summary_models.csv", header=T)
info <-read.csv("~/Desktop/chapter3final/elongation_summary_info.csv", header=T)
info<-as.data.frame(t(info))
colnames(info)<-info[1,]
info<-info[-1,]
name2<-c("Wheat-CS", "Wheat-Z-")
domestication <-as.factor(info $domestication)[which((substr(info$accession,1,8) %in%name2)&(info$treatment=="c"))]

bd21_e<-cbind(domestication,elongation[which((substr(info$accession,1,8) %in%name2)&(info$treatment=="c")),c(5,10,14)])
name1<-levels(as.factor(substr(microscopy $species,1,8)))[mm]
name1<-c("Wheat-CS", "Wheat-Z-")
bd21_m<-microscopy[which((substr(microscopy $species,1,8)%in% name1)&(microscopy$treatment=="c")),c(1,11,5,6,13,14,15)]
colnames(bd21_m)[1]<-"domestication"
bd21_m <-na.omit(bd21_m)
bd21_e$der
bd21_e$treatment
##elongation rate


a<-summary(lm(der~ domestication,data= bd21_e))$coefficient[2,c(1,4)]
model<-lm(der~ domestication,data= bd21_e)
# Calculate Cook's Distance
cooks_distance <- cooks.distance(model)
# Set a threshold for influential points (often 4/n, where n is the number of data points)
threshold <- 4 / nrow(bd21_e)
influential_points <- cooks_distance > threshold
# Filter out influential points
bd21_e_filtered <- bd21_e[!influential_points, ]
modelfilter<-lm(der~ domestication,data= bd21_e_filtered)
a<-summary(modelfilter)$coefficient[2,c(1,4)]
list<-c(list,a[2])
intercept <- coef(modelfilter)[1]  # beta_0 (mean for control)
treatment_coef <- coef(modelfilter)[2]  # beta_1 (effect of drought)
proportion_change <- treatment_coef / intercept
list2<-c(list2, -proportion_change)
##elongation duration
a<-summary(lm(l10_90~ domestication,data= bd21_e))$coefficient[2,c(1,4)]
model<-lm(l10_90 ~ domestication,data= bd21_e)
# Calculate Cook's Distance
cooks_distance <- cooks.distance(model)
# Set a threshold for influential points (often 4/n, where n is the number of data points)
threshold <- 4 / nrow(bd21_e)
influential_points <- cooks_distance > threshold
# Filter out influential points
bd21_e_filtered <- bd21_e[!influential_points, ]
modelfilter<-lm(l10_90 ~ domestication,data= bd21_e_filtered)
a<-summary(modelfilter)$coefficient[2,c(1,4)]
list<-c(list,a[2])
intercept <- coef(modelfilter)[1]  # beta_0 (mean for control)
treatment_coef <- coef(modelfilter)[2]  # beta_1 (effect of drought)
proportion_change <- treatment_coef / intercept
list2<-c(list2, -proportion_change)
a<-summary(lm(lf~ domestication,data= bd21_e))$coefficient[2,c(1,4)]
model<-lm(lf ~ domestication,data= bd21_e)
# Calculate Cook's Distance
cooks_distance <- cooks.distance(model)
# Set a threshold for influential points (often 4/n, where n is the number of data points)
threshold <- 4 / nrow(bd21_e)
influential_points <- cooks_distance > threshold
# Filter out influential points
bd21_e_filtered <- bd21_e[!influential_points, ]
modelfilter<-lm(lf ~ domestication,data= bd21_e_filtered)
a<-summary(modelfilter)$coefficient[2,c(1,4)]
list<-c(list,a[2])
intercept <- coef(modelfilter)[1]  # beta_0 (mean for control)
treatment_coef <- coef(modelfilter)[2]  # beta_1 (effect of drought)
proportion_change <- treatment_coef / intercept
list2<-c(list2, -proportion_change)

bd21_e_summary<-cbind(aggregate(bd21_e $l10_90,list(bd21_e$domestication),mean),aggregate(bd21_e$der,list(bd21_e$domestication),mean)[,2],aggregate(bd21_e$lf,list(bd21_e$domestication),mean)[,2])

colnames(bd21_e_summary)<-c("","duration","rate","lm")
print(bd21_e_summary)
print(name1)
bd21_m<-cbind(bd21_m,bd21_e_summary[match(bd21_m$domestication,bd21_e_summary[,1]),])


a<-NaN
bd21_m$lf <-as.numeric(bd21_m$lf)
##lf
if(0%in%bd21_m$lf){
bd21_m_no0<-bd21_m[-which(bd21_m$lf==0),]
}else{
	bd21_m_no0<-bd21_m
	}
a<-summary(lm(lf ~ domestication,data= as.data.frame(na.omit(bd21_m_no0))))$coefficient[2,c(1,4)]
model<-lm(lf ~ domestication,data= as.data.frame(na.omit(bd21_m_no0)))
# Calculate Cook's Distance
cooks_distance <- cooks.distance(model)
# Set a threshold for influential points (often 4/n, where n is the number of data points)
threshold <- 4 / nrow(as.data.frame(na.omit(bd21_m_no0)))
influential_points <- cooks_distance > threshold
# Filter out influential points
bd21_m_no0_filtered <- as.data.frame(na.omit(bd21_m_no0))[!influential_points, ]
modelfilter<-lm(lf ~ domestication,data= bd21_m_no0_filtered)
a<-summary(modelfilter)$coefficient[2,c(1,4)]
list<-c(list,a[2])
intercept <- coef(modelfilter)[1]  # beta_0 (mean for control)
treatment_coef <- coef(modelfilter)[2]  # beta_1 (effect of drought)
proportion_change <- treatment_coef / intercept
list2<-c(list2, -proportion_change)


##n_all****

#bd21_m_no0 $all<-bd21_m_no0 $lm/bd21_m_no0 $lf


permutation<-cbind(c(rep("W",nnn),rep("D",nnn)),c(sample(bd21_e $lf[which(bd21_e $domestication%in%c("W","w"))],nnn,replace = TRUE)/sample(bd21_m_no0 $lf[which(bd21_m_no0 $domestication %in%c("w","W"))],nnn,replace = TRUE), sample(bd21_e $lf[which(bd21_e $domestication%in%c("d","D"))],nnn,replace = TRUE)/sample(bd21_m_no0 $lf[which(bd21_m_no0 $domestication%in%c("d","D"))],nnn,replace = TRUE)))

colnames(permutation)<-c("domestication","lf")
permutation<-as.data.frame(permutation)
permutation $lf <-as.numeric(permutation $lf)
a<-summary(lm(lf ~ domestication,data= permutation))$coefficient[2,c(1,4)]
model<-lm(lf ~ domestication,data= permutation)
# Calculate Cook's Distance
cooks_distance <- cooks.distance(model)
# Set a threshold for influential points (often 4/n, where n is the number of data points)
threshold <- 4 / nrow(permutation)
influential_points <- cooks_distance > threshold
# Filter out influential points
permutation_filtered <- permutation[!influential_points, ]
modelfilter<-lm(lf ~ domestication,data= permutation_filtered)
a<-summary(modelfilter)$coefficient[2,c(1,4)]
list<-c(list,a[2])
intercept <- coef(modelfilter)[1]  # beta_0 (mean for control)
treatment_coef <- coef(modelfilter)[2]  # beta_1 (effect of drought)
proportion_change <- treatment_coef / intercept
list2<-c(list2, -proportion_change)


permutation<-cbind(c(rep("W",nnn),rep("D",nnn)),c(sample(bd21_e $der[which(bd21_e $domestication%in%c("W","w"))],nnn,replace = TRUE)/sample(bd21_m_no0 $lf[which(bd21_m_no0 $domestication %in%c("w","W"))],nnn,replace = TRUE), sample(bd21_e $der[which(bd21_e $domestication%in%c("d","D"))],nnn,replace = TRUE)/sample(bd21_m_no0 $lf[which(bd21_m_no0 $domestication%in%c("d","D"))],nnn,replace = TRUE)))

colnames(permutation)<-c("domestication","lf")
permutation<-as.data.frame(permutation)
permutation $lf <-as.numeric(permutation $lf)
a<-summary(lm(lf ~ domestication,data= permutation))$coefficient[2,c(1,4)]
model<-lm(lf ~ domestication,data= permutation)
# Calculate Cook's Distance
cooks_distance <- cooks.distance(model)
# Set a threshold for influential points (often 4/n, where n is the number of data points)
threshold <- 4 / nrow(permutation)
influential_points <- cooks_distance > threshold
# Filter out influential points
permutation_filtered <- permutation[!influential_points, ]
modelfilter<-lm(lf ~ domestication,data= permutation_filtered)
a<-summary(modelfilter)$coefficient[2,c(1,4)]
list<-c(list,a[2])
intercept <- coef(modelfilter)[1]  # beta_0 (mean for control)
treatment_coef <- coef(modelfilter)[2]  # beta_1 (effect of drought)
proportion_change <- treatment_coef / intercept
list2<-c(list2, -proportion_change)


##division
a<-summary(lm(divisionzonelist ~ domestication,data= as.data.frame(na.omit(bd21_m))))$coefficient[2,c(1,4)]
model<-lm(divisionzonelist ~ domestication,data= as.data.frame(na.omit(bd21_m)))

# Calculate Cook's Distance
cooks_distance <- cooks.distance(model)
# Set a threshold for influential points (often 4/n, where n is the number of data points)
threshold <- 4 / nrow(as.data.frame(na.omit(bd21_m)))
influential_points <- cooks_distance > threshold
# Filter out influential points
bd21_m_filtered <- as.data.frame(na.omit(bd21_m))[!influential_points, ]
modelfilter<-lm(divisionzonelist ~ domestication,data= bd21_m_filtered)
a<-summary(modelfilter)$coefficient[2,c(1,4)]
list<-c(list,a[2])
intercept <- coef(modelfilter)[1]  # beta_0 (mean for control)
treatment_coef <- coef(modelfilter)[2]  # beta_1 (effect of drought)
proportion_change <- treatment_coef / intercept
list2<-c(list2, -proportion_change)




bd21_m_no0 $divisioncellsize <-as.numeric(bd21_m_no0 $divisioncellsize)
a<-NaN
##e
a<-summary(lm(divisioncellsize ~ domestication,data= as.data.frame(na.omit(bd21_m_no0))))$coefficient[2,c(1,4)]
model<-lm(divisioncellsize ~ domestication,data= as.data.frame(na.omit(bd21_m_no0)))

# Calculate Cook's Distance
cooks_distance <- cooks.distance(model)
# Set a threshold for influential points (often 4/n, where n is the number of data points)
threshold <- 4 / nrow(as.data.frame(na.omit(bd21_m_no0)))
influential_points <- cooks_distance > threshold
# Filter out influential points
bd21_m_no0_filtered <- as.data.frame(na.omit(bd21_m_no0))[!influential_points, ]
modelfilter<-lm(divisioncellsize ~ domestication,data= bd21_m_no0_filtered)
a<-summary(modelfilter)$coefficient[2,c(1,4)]
list<-c(list,a[2])
intercept <- coef(modelfilter)[1]  # beta_0 (mean for control)
treatment_coef <- coef(modelfilter)[2]  # beta_1 (effect of drought)
proportion_change <- treatment_coef / intercept
list2<-c(list2, -proportion_change)

##Llge


##Ndiv
bd21_m_no0 $Ndiv <-bd21_m_no0 $divisionzonelist/bd21_m_no0 $divisioncellsize
a<-summary(lm(Ndiv ~ domestication,data= as.data.frame(na.omit(bd21_m_no0))))$coefficient[2,c(1,4)]
model<-lm(Ndiv ~ domestication,data= as.data.frame(na.omit(bd21_m_no0)))
# Calculate Cook's Distance
cooks_distance <- cooks.distance(model)
# Set a threshold for influential points (often 4/n, where n is the number of data points)
threshold <- 4 / nrow(as.data.frame(na.omit(bd21_m_no0)))
influential_points <- cooks_distance > threshold
# Filter out influential points
bd21_m_no0_filtered <- as.data.frame(na.omit(bd21_m_no0))[!influential_points, ]
modelfilter<-lm(Ndiv ~ domestication,data= bd21_m_no0_filtered)
a<-summary(modelfilter)$coefficient[2,c(1,4)]
list<-c(list,a[2])
intercept <- coef(modelfilter)[1]  # beta_0 (mean for control)
treatment_coef <- coef(modelfilter)[2]  # beta_1 (effect of drought)
proportion_change <- treatment_coef / intercept
list2<-c(list2, -proportion_change)




##Ddiv*****
#bd21_m_no0 $Ddiv <-bd21_m_no0 $rate/(bd21_m_no0 $lf*bd21_m_no0 $Ndiv)




permutation<-cbind(c(rep("W",nnn),rep("D",nnn)),c(sample(bd21_e $der[which(bd21_e $domestication%in%c("W","w"))],nnn,replace = TRUE)/sample((bd21_m_no0 $lf*bd21_m_no0 $Ndiv)[which(bd21_m_no0 $domestication %in%c("w","W"))],nnn,replace = TRUE), sample(bd21_e $der[which(bd21_e $domestication%in%c("d","D"))],nnn,replace = TRUE)/sample((bd21_m_no0 $lf*bd21_m_no0 $Ndiv)[which(bd21_m_no0 $domestication%in%c("d","D"))],nnn,replace = TRUE)))

colnames(permutation)<-c("domestication","lf")
permutation<-as.data.frame(permutation)
permutation $lf <-as.numeric(permutation $lf)
a<-summary(lm(lf ~ domestication,data= permutation))$coefficient[2,c(1,4)]
model<-lm(lf ~ domestication,data= permutation)
# Calculate Cook's Distance
cooks_distance <- cooks.distance(model)
# Set a threshold for influential points (often 4/n, where n is the number of data points)
threshold <- 4 / nrow(permutation)
influential_points <- cooks_distance > threshold
# Filter out influential points
permutation_filtered <- permutation[!influential_points, ]
modelfilter<-lm(lf ~ domestication,data= permutation_filtered)
a<-summary(modelfilter)$coefficient[2,c(1,4)]
list<-c(list,a[2])
intercept <- coef(modelfilter)[1]  # beta_0 (mean for control)
treatment_coef <- coef(modelfilter)[2]  # beta_1 (effect of drought)
proportion_change <- treatment_coef / intercept
list2<-c(list2, -proportion_change)





###
bd21_m_no0 $Lge <-as.numeric(bd21_m_no0 $Lge)
a<-summary(lm(Lge ~ domestication,data= as.data.frame(na.omit(bd21_m_no0))))$coefficient[2,c(1,4)]
model<-lm(Lge ~ domestication,data= as.data.frame(na.omit(bd21_m_no0)))
# Calculate Cook's Distance
cooks_distance <- cooks.distance(model)
# Set a threshold for influential points (often 4/n, where n is the number of data points)
threshold <- 4 / nrow(as.data.frame(na.omit(bd21_m_no0)))
influential_points <- cooks_distance > threshold
# Filter out influential points
bd21_m_no0_filtered <- as.data.frame(na.omit(bd21_m_no0))[!influential_points, ]
modelfilter<-lm(Lge ~ domestication,data= bd21_m_no0_filtered)
a<-summary(modelfilter)$coefficient[2,c(1,4)]
list<-c(list,a[2])
intercept <- coef(modelfilter)[1]  # beta_0 (mean for control)
treatment_coef <- coef(modelfilter)[2]  # beta_1 (effect of drought)
proportion_change <- treatment_coef / intercept
list2<-c(list2, -proportion_change)


##ne
bd21_m_no0 $N <-as.numeric(bd21_m_no0 $N)
a<-summary(lm(N ~ domestication,data= as.data.frame(na.omit(bd21_m_no0))))$coefficient[2,c(1,4)]
model<-lm(N ~ domestication,data= as.data.frame(na.omit(bd21_m_no0)))
# Calculate Cook's Distance
cooks_distance <- cooks.distance(model)
# Set a threshold for influential points (often 4/n, where n is the number of data points)
threshold <- 4 / nrow(as.data.frame(na.omit(bd21_m_no0)))
influential_points <- cooks_distance > threshold
# Filter out influential points
bd21_m_no0_filtered <- as.data.frame(na.omit(bd21_m_no0))[!influential_points, ]
modelfilter<-lm(N ~ domestication,data= bd21_m_no0_filtered)
a<-summary(modelfilter)$coefficient[2,c(1,4)]
list<-c(list,a[2])
intercept <- coef(modelfilter)[1]  # beta_0 (mean for control)
treatment_coef <- coef(modelfilter)[2]  # beta_1 (effect of drought)
proportion_change <- treatment_coef / intercept
list2<-c(list2, -proportion_change)##E_rate****
#bd21_m_no0 $E_rate <-((bd21_m_no0 $lf-bd21_m_no0 $divisioncellsize)/bd21_m_no0 $lf/bd21_m_no0 $Lge)*bd21_m_no0 $rate


permutation<-cbind(c(rep("W",nnn),rep("D",nnn)),c(sample(((bd21_m_no0 $lf-bd21_m_no0 $divisioncellsize)/bd21_m_no0 $lf/bd21_m_no0 $Lge)[which(bd21_m_no0 $domestication%in%c("W","w"))],nnn,replace = TRUE)*sample(bd21_e $der[which(bd21_e $domestication %in%c("w","W"))],nnn,replace = TRUE), sample(((bd21_m_no0 $lf-bd21_m_no0 $divisioncellsize)/bd21_m_no0 $lf/bd21_m_no0 $Lge)[which(bd21_m_no0 $domestication%in%c("d","D"))],nnn,replace = TRUE)*sample(bd21_e $der[which(bd21_e $domestication %in%c("D","d"))],nnn,replace = TRUE)))

colnames(permutation)<-c("domestication","lf")
permutation<-as.data.frame(permutation)
permutation $lf <-as.numeric(permutation $lf)
a<-summary(lm(lf ~ domestication,data= permutation))$coefficient[2,c(1,4)]
model<-lm(lf ~ domestication,data= permutation)
# Calculate Cook's Distance
cooks_distance <- cooks.distance(model)
# Set a threshold for influential points (often 4/n, where n is the number of data points)
threshold <- 4 / nrow(permutation)
influential_points <- cooks_distance > threshold
# Filter out influential points
permutation_filtered <- permutation[!influential_points, ]
modelfilter<-lm(lf ~ domestication,data= permutation_filtered)
a<-summary(modelfilter)$coefficient[2,c(1,4)]
list<-c(list,a[2])
intercept <- coef(modelfilter)[1]  # beta_0 (mean for control)
treatment_coef <- coef(modelfilter)[2]  # beta_1 (effect of drought)
proportion_change <- treatment_coef / intercept
list2<-c(list2, -proportion_change)

permutation<-cbind(c(rep("W",nnn),rep("D",nnn)),c(sample((bd21_m_no0 $N *bd21_m_no0 $lf)[which(bd21_m_no0 $domestication%in%c("W","w"))],nnn,replace = TRUE)/sample((bd21_e $der)[which(bd21_e $domestication %in%c("w","W"))],nnn,replace = TRUE), sample((bd21_m_no0 $N *bd21_m_no0 $lf)[which(bd21_m_no0 $domestication%in%c("d","D"))],nnn,replace = TRUE)/sample((bd21_e $der)[which(bd21_e $domestication %in%c("D","d"))],nnn,replace = TRUE)))

colnames(permutation)<-c("domestication","lf")
permutation<-as.data.frame(permutation)
permutation $lf <-as.numeric(permutation $lf)
a<-summary(lm(lf ~ domestication,data= permutation))$coefficient[2,c(1,4)]
model<-lm(lf ~ domestication,data= permutation)
# Calculate Cook's Distance
cooks_distance <- cooks.distance(model)
# Set a threshold for influential points (often 4/n, where n is the number of data points)
threshold <- 4 / nrow(permutation)
influential_points <- cooks_distance > threshold
# Filter out influential points
permutation_filtered <- permutation[!influential_points, ]
modelfilter<-lm(lf ~ domestication,data= permutation_filtered)
a<-summary(modelfilter)$coefficient[2,c(1,4)]
list<-c(list,a[2])
intercept <- coef(modelfilter)[1]  # beta_0 (mean for control)
treatment_coef <- coef(modelfilter)[2]  # beta_1 (effect of drought)
proportion_change <- treatment_coef / intercept
list2<-c(list2, -proportion_change)




listall<-cbind(listall,list)
listall2<-cbind(listall2,list2)




  #d<-(-log(listall,10))*abs(listall2)/listall2
  d<-listall2
      d[which(d>1)]<-1
    d[which(d<c(-1))]<-c(-1)
  colnames(d)<-c("Barley", "Oat","Wheat-L vs Z","Wheat-CS vs Z")
  rownames(d)<-c("leaf elongation rate","leaf elongation duration","final leaf length","mature cell size","cell number^","cell flux^","division zone size","dividing cell size","division zone cell number","division rate^","elongation zone size","elongation cells number","relative cell elongation rate^","cell elongation duration^")
  list

library(gplots)
library(RColorBrewer)


labs<-matrix(symnum(c(listall), corr = FALSE, na = FALSE, cutpoints = c(0, 
    0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", " ")),dim(d)[1],dim(d)[2])

 # Save the output to a PDF
pdf(paste0("~/Downloads/enhanced_heatmap.pdf"), width=6, height=6)

# Improved color palette: More distinguishable colors
custom_palette <- colorRampPalette(c("magenta", "white", "cyan"))
# Plot the heatmap
heatmap.2(d[1:12,], 
          Colv = FALSE, 
          Rowv = FALSE, 
          trace = "none", 
          cellnote = labs,  # Add labels
          col = custom_palette(100),  # Improved color scheme
          cexRow = 1.2,  # Slightly larger row labels for readability
          cexCol = 1.2,  # Larger column labels
          offsetRow = 1,  # Fine-tuning row offset
          srtCol = 45,  # Rotate column labels for readability
          notecex = 1.5,  # Adjust size of cell text
          notecol = "black",  # Text color for cell notes
          key = TRUE,  # Display color key
          key.title = "Proportion",  # Title for the key
          key.xlab = "Value",  # Label for the key
          key.ylab = "Count",  # Histogram label
          margins = c(6, 16),  # Adjust margins for labels
          density.info = "density",  # Add histogram and density plot
          denscol = "black",  # Histogram color
          lwid = c(1.5, 4),  # Adjust width of side color bars
          lhei = c(1.5, 5)   # Adjust height for better aspect ratio
)
dev.off()
pdf(paste0("~/Downloads/enhanced_heatmap2.pdf"), width=6, height=3)

heatmap.2(d[13:14,], 
          Colv = FALSE, 
          Rowv = FALSE, 
          trace = "none", 
          cellnote = labs,  # Add labels
          col = custom_palette(100),  # Improved color scheme
          cexRow = 1.2,  # Slightly larger row labels for readability
          cexCol = 1.2,  # Larger column labels
          offsetRow = 1,  # Fine-tuning row offset
          srtCol = 45,  # Rotate column labels for readability
          notecex = 1.5,  # Adjust size of cell text
          notecol = "black",  # Text color for cell notes
          key = TRUE,  # Display color key
          key.title = "Proportion",  # Title for the key
          key.xlab = "Value",  # Label for the key
          key.ylab = "Count",  # Histogram label
          margins = c(6, 16),  # Adjust margins for labels
          density.info = "density",  # Add histogram and density plot
          denscol = "black",  # Histogram color
          lwid = c(1.5, 4),  # Adjust width of side color bars
          lhei = c(1.5, 5)   # Adjust height for better aspect ratio
)

# Close the PDF
dev.off()





