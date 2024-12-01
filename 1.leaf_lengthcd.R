library(ggpubr)

parameters <-read.csv(paste0("/Users/jiey/Desktop/chapter3final/elongation_summary_models.csv"), header=T)
l10_90<-parameters[,12]-parameters[,11]
parameters<-cbind(parameters,l10_90)

dotdata <-name <-read.csv(paste0("/Users/jiey/Downloads/elongation_summary_info.csv"), header=T)
data<-name[1:5,]
name<-name[-c(1:5),]
rownames(name)<-name[,1]
name<-name[,-1]
  data<-data[,-1]
  rownames(dotdata)<-dotdata[,1]
dotdata<-dotdata[-c(1:5),-1]
    Find_Max_CCF<- function(a,b)

{
	listmin<-NULL
for(iiii in 1:(length(a)*2/3)){
listmin<-rbind(listmin ,c(iiii,	cor(c(rep(a[1],iiii),a[1:(length(b)-iiii+1)]),c(b[iiii:length(b)],rep(b[length(b)],iiii)))))	
}
for(iiiii in 1:(length(a)*2/3)){
listmin<-rbind(listmin ,c(-iiiii,	cor(c(rep(b[1],iiiii),b[1:(length(b)-iiiii+1)]),c(a[iiiii:length(b)],rep(a[length(a)],iiiii)))))	
}
res_max <-listmin[which(listmin[,2]==max(listmin[,2]))[1],1]
 return(-res_max)
} 


a<-"zavic_2"
aa<-"zavid_2"
bb<-7
ss=0
max<-200
xmax<-1000
xmatrix1<-xmatrix2<-ymatrix1<-ymatrix2<-NULL
sslist<-NULL
for(i in which(substr(colnames(name),1,bb)==a)){
L=parameters[which(substr(colnames(name),1,bb)==a)[1],1]
   x0=parameters[which(substr(colnames(name),1,bb)==a)[1],2]
   k=parameters[which(substr(colnames(name),1,bb)==a)[1],3]
   b=parameters[which(substr(colnames(name),1,bb)==a)[1],4]
      x=c(-500:5:500)
    y0 = L / (1 + exp(-k*(x-x0))) + b

  L=parameters[i,1]
   x0=parameters[i,2]
   k=parameters[i,3]
   b=parameters[i,4]
  y1 = L / (1 + exp(-k*(x-x0))) + b
x1=Find_Max_CCF(y0,y1)+x
print(Find_Max_CCF(y0,y1))
  	plot( x1, y1, type="l", col="#66CC66",ylim=c(0,max),xlim=c(-500,xmax) )
par(new=TRUE)	
ss=ss+1
ymatrix1<-c(ymatrix1,y1)
xmatrix1<-c(xmatrix1, x1)
sslist<-c(sslist,rep(ss,length(y1)))
}
for(i in which(substr(colnames(name),1,bb)==aa)){
L=parameters[which(substr(colnames(name),1,bb)==aa)[1],1]
   x0=parameters[which(substr(colnames(name),1,bb)==aa)[1],2]
   k=parameters[which(substr(colnames(name),1,bb)==aa)[1],3]
   b=parameters[which(substr(colnames(name),1,bb)==aa)[1],4]
      x=c(-500:5:500)
    y0 = L / (1 + exp(-k*(x-x0))) + b

  L=parameters[i,1]
   x0=parameters[i,2]
   k=parameters[i,3]
   b=parameters[i,4]
  y1 = L / (1 + exp(-k*(x-x0))) + b
x1=Find_Max_CCF(y0,y1)+x
print(Find_Max_CCF(y0,y1))

  	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,max),xlim=c(-500,xmax) )
par(new=TRUE)	
ss=ss+1
ymatrix2<-c(ymatrix2,y1)
xmatrix2<-c(xmatrix2, x1)
sslist<-c(sslist,rep(ss,length(y1)))
}
for(i in 1:length(which(substr(colnames(dotdata),1,bb)%in%c(a,aa))) ){
plot(as.numeric(rownames(dotdata)),as.numeric(dotdata[,which(substr(colnames(dotdata),1,bb)%in%c(a,aa))[i]]),ylim=c(0,max),xlim=c(-500,xmax) )
par(new=TRUE)	
}
	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,max),xlim=c(-500,xmax) )

all_data_bd21_plot<-cbind(c(xmatrix1, xmatrix2),c(ymatrix1, ymatrix2),sslist,c(rep("Control",length(ymatrix1)),rep("Drought",length(ymatrix2))))

all_data_bd21_plot<-all_data_bd21_plot[which(all_data_bd21_plot[,1]>0),]


all_data_bd21_plot<-all_data_bd21_plot[which(all_data_bd21_plot[,1]<plyr::count(c(xmatrix1, xmatrix2))[max(which(plyr::count(c(xmatrix1, xmatrix2))[,2]==max(plyr::count(c(xmatrix1, xmatrix2))[,2]))),1]),]

colnames(all_data_bd21_plot)<-c("location","size","sample","treatment")
write.csv(as.matrix(all_data_bd21_plot),"~/Downloads/tmp.csv")
all_data_bd21_plot <-read.csv(paste0("~/Downloads/tmp.csv"), header=T,row.names=1)
library(ggplot2)
library(reshape2)

all_data_bd21_plot$treatment<-as.factor(all_data_bd21_plot$treatment)


  aaaa2 <-ggplot(all_data_bd21_plot,aes(location,size))+
  theme_classic()+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = treatment))+
  labs(x = "Thermal time (°Cd)",y = expression(paste("Leaf length (mm)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",size = 1,aes(fill = treatment),alpha = 0.3)+
  guides(fill = "none")+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = treatment))+
  labs(x = "Thermal time (°Cd)",y = expression(paste("Leaf length (mm)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  geom_line(aes(group = interaction(sample, treatment),colour = treatment,alpha = 0.2))+
  guides(alpha= "none") + scale_color_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+scale_fill_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+ ggtitle(
"Wheat-Z-Wild")+ scale_color_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+scale_fill_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))




a<-"wheat_wc"
aa<-"wheat_wd"
bb<-8
ss=0
max<-max(name[,which(substr(colnames(name),1,bb)%in%c(a,aa))], na.rm=T)
xmax<-max(parameters[which(substr(colnames(name),1,bb)%in%c(a,aa)),2], na.rm=T)*3
xmatrix1<-xmatrix2<-ymatrix1<-ymatrix2<-NULL
sslist<-NULL
for(i in which(substr(colnames(name),1,bb)==a)){
L=parameters[which(substr(colnames(name),1,bb)==a)[1],1]
   x0=parameters[which(substr(colnames(name),1,bb)==a)[1],2]
   k=parameters[which(substr(colnames(name),1,bb)==a)[1],3]
   b=parameters[which(substr(colnames(name),1,bb)==a)[1],4]
      x=c(-500:5:500)
    y0 = L / (1 + exp(-k*(x-x0))) + b

  L=parameters[i,1]
   x0=parameters[i,2]
   k=parameters[i,3]
   b=parameters[i,4]
  y1 = L / (1 + exp(-k*(x-x0))) + b
x1=Find_Max_CCF(y0,y1)+x
print(Find_Max_CCF(y0,y1))
  	plot( x1, y1, type="l", col="#66CC66",ylim=c(0,max),xlim=c(-500,xmax) )
par(new=TRUE)	
ss=ss+1
ymatrix1<-c(ymatrix1,y1)
xmatrix1<-c(xmatrix1, x1)
sslist<-c(sslist,rep(ss,length(y1)))
}
for(i in which(substr(colnames(name),1,bb)==aa)){
L=parameters[which(substr(colnames(name),1,bb)==aa)[1],1]
   x0=parameters[which(substr(colnames(name),1,bb)==aa)[1],2]
   k=parameters[which(substr(colnames(name),1,bb)==aa)[1],3]
   b=parameters[which(substr(colnames(name),1,bb)==aa)[1],4]
      x=c(-500:5:500)
    y0 = L / (1 + exp(-k*(x-x0))) + b

  L=parameters[i,1]
   x0=parameters[i,2]
   k=parameters[i,3]
   b=parameters[i,4]
  y1 = L / (1 + exp(-k*(x-x0))) + b
x1=Find_Max_CCF(y0,y1)+x
print(Find_Max_CCF(y0,y1))

  	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,max),xlim=c(-500,xmax) )
par(new=TRUE)	
ss=ss+1
ymatrix2<-c(ymatrix2,y1)
xmatrix2<-c(xmatrix2, x1)
sslist<-c(sslist,rep(ss,length(y1)))
}
 	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,max),xlim=c(-500,xmax) )

all_data_bd21_plot<-cbind(c(xmatrix1, xmatrix2),c(ymatrix1, ymatrix2),sslist,c(rep("Control",length(ymatrix1)),rep("Drought",length(ymatrix2))))

all_data_bd21_plot<-all_data_bd21_plot[which(all_data_bd21_plot[,1]>0),]

all_data_bd21_plot<-all_data_bd21_plot[which(all_data_bd21_plot[,1]<plyr::count(c(xmatrix1, xmatrix2))[max(which(plyr::count(c(xmatrix1, xmatrix2))[,2]==max(plyr::count(c(xmatrix1, xmatrix2))[,2]))),1]),]



colnames(all_data_bd21_plot)<-c("location","size","sample","treatment")
write.csv(as.matrix(all_data_bd21_plot),"~/Downloads/tmp.csv")
all_data_bd21_plot <-read.csv(paste0("~/Downloads/tmp.csv"), header=T,row.names=1)
library(ggplot2)
library(reshape2)

all_data_bd21_plot$treatment<-as.factor(all_data_bd21_plot$treatment)

aaaa1 <-ggplot(all_data_bd21_plot,aes(location,size))+
  theme_classic()+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = treatment))+
  labs(x = "Thermal time (°Cd)",y = expression(paste("Leaf length (mm)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",size = 1,aes(fill = treatment),alpha = 0.3)+
  guides(fill = "none")+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = treatment))+
  labs(x = "Thermal time (°Cd)",y = expression(paste("Leaf length (mm)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  geom_line(aes(group = interaction(sample, treatment),colour = treatment,alpha = 0.2))+
  guides(alpha= "none") + scale_color_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+scale_fill_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+ ggtitle(
"Wheat-Z-Wild 1st batch")



a<-"csc_1"
aa<-"csd_1"
bb<-5
ss=0
max<-max(name[,which(substr(colnames(name),1,bb)%in%c(a,aa))], na.rm=T)
xmax<-max(parameters[which(substr(colnames(name),1,bb)%in%c(a,aa)),2], na.rm=T)*3
xmatrix1<-xmatrix2<-ymatrix1<-ymatrix2<-NULL
sslist<-NULL
for(i in which(substr(colnames(name),1,bb)==a)){
L=parameters[which(substr(colnames(name),1,bb)==a)[1],1]
   x0=parameters[which(substr(colnames(name),1,bb)==a)[1],2]
   k=parameters[which(substr(colnames(name),1,bb)==a)[1],3]
   b=parameters[which(substr(colnames(name),1,bb)==a)[1],4]
      x=c(-500:5:500)
    y0 = L / (1 + exp(-k*(x-x0))) + b

  L=parameters[i,1]
   x0=parameters[i,2]
   k=parameters[i,3]
   b=parameters[i,4]
  y1 = L / (1 + exp(-k*(x-x0))) + b
x1=Find_Max_CCF(y0,y1)+x
print(Find_Max_CCF(y0,y1))
  	plot( x1, y1, type="l", col="#66CC66",ylim=c(0,max),xlim=c(-500,xmax) )
par(new=TRUE)	
ss=ss+1
ymatrix1<-c(ymatrix1,y1)
xmatrix1<-c(xmatrix1, x1)
sslist<-c(sslist,rep(ss,length(y1)))
}
for(i in which(substr(colnames(name),1,bb)==aa)){
L=parameters[which(substr(colnames(name),1,bb)==aa)[1],1]
   x0=parameters[which(substr(colnames(name),1,bb)==aa)[1],2]
   k=parameters[which(substr(colnames(name),1,bb)==aa)[1],3]
   b=parameters[which(substr(colnames(name),1,bb)==aa)[1],4]
      x=c(-500:5:500)
    y0 = L / (1 + exp(-k*(x-x0))) + b

  L=parameters[i,1]
   x0=parameters[i,2]
   k=parameters[i,3]
   b=parameters[i,4]
  y1 = L / (1 + exp(-k*(x-x0))) + b
x1=Find_Max_CCF(y0,y1)+x
print(Find_Max_CCF(y0,y1))

  	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,max),xlim=c(-500,xmax) )
par(new=TRUE)	
ss=ss+1
ymatrix2<-c(ymatrix2,y1)
xmatrix2<-c(xmatrix2, x1)
sslist<-c(sslist,rep(ss,length(y1)))
}
 	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,max),xlim=c(-500,xmax) )

all_data_bd21_plot<-cbind(c(xmatrix1, xmatrix2),c(ymatrix1, ymatrix2),sslist,c(rep("Control",length(ymatrix1)),rep("Drought",length(ymatrix2))))

all_data_bd21_plot<-all_data_bd21_plot[which(all_data_bd21_plot[,1]>0),]

all_data_bd21_plot<-all_data_bd21_plot[which(all_data_bd21_plot[,1]<plyr::count(c(xmatrix1, xmatrix2))[max(which(plyr::count(c(xmatrix1, xmatrix2))[,2]==max(plyr::count(c(xmatrix1, xmatrix2))[,2]))),1]),]



colnames(all_data_bd21_plot)<-c("location","size","sample","treatment")
write.csv(as.matrix(all_data_bd21_plot),"~/Downloads/tmp.csv")
all_data_bd21_plot <-read.csv(paste0("~/Downloads/tmp.csv"), header=T,row.names=1)
library(ggplot2)
library(reshape2)

all_data_bd21_plot$treatment<-as.factor(all_data_bd21_plot$treatment)

bbb1<-ggplot(all_data_bd21_plot,aes(location,size))+
  theme_classic()+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = treatment))+
  labs(x = "Thermal time (°Cd)",y = expression(paste("Leaf length (mm)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",size = 1,aes(fill = treatment),alpha = 0.3)+
  guides(fill = "none")+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = treatment))+
  labs(x = "Thermal time (°Cd)",y = expression(paste("Leaf length (mm)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  geom_line(aes(group = interaction(sample, treatment),colour = treatment,alpha = 0.2))+
  guides(alpha= "none") + scale_color_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+scale_fill_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+ ggtitle(
"Wheat-CS-Domesticated 1st batch")





a<-"csc_2"
aa<-"csd_2"
bb<-5
ss=0
max<-max(name[,which(substr(colnames(name),1,bb)%in%c(a,aa))], na.rm=T)
xmax<-max(parameters[which(substr(colnames(name),1,bb)%in%c(a,aa)),2], na.rm=T)*3
xmatrix1<-xmatrix2<-ymatrix1<-ymatrix2<-NULL
sslist<-NULL
for(i in which(substr(colnames(name),1,bb)==a)){
L=parameters[which(substr(colnames(name),1,bb)==a)[1],1]
   x0=parameters[which(substr(colnames(name),1,bb)==a)[1],2]
   k=parameters[which(substr(colnames(name),1,bb)==a)[1],3]
   b=parameters[which(substr(colnames(name),1,bb)==a)[1],4]
      x=c(-500:5:500)
    y0 = L / (1 + exp(-k*(x-x0))) + b

  L=parameters[i,1]
   x0=parameters[i,2]
   k=parameters[i,3]
   b=parameters[i,4]
  y1 = L / (1 + exp(-k*(x-x0))) + b
x1=Find_Max_CCF(y0,y1)+x
print(Find_Max_CCF(y0,y1))
  	plot( x1, y1, type="l", col="#66CC66",ylim=c(0,max),xlim=c(-500,xmax) )
par(new=TRUE)	
ss=ss+1
ymatrix1<-c(ymatrix1,y1)
xmatrix1<-c(xmatrix1, x1)
sslist<-c(sslist,rep(ss,length(y1)))
}
for(i in which(substr(colnames(name),1,bb)==aa)){
L=parameters[which(substr(colnames(name),1,bb)==aa)[1],1]
   x0=parameters[which(substr(colnames(name),1,bb)==aa)[1],2]
   k=parameters[which(substr(colnames(name),1,bb)==aa)[1],3]
   b=parameters[which(substr(colnames(name),1,bb)==aa)[1],4]
      x=c(-500:5:500)
    y0 = L / (1 + exp(-k*(x-x0))) + b

  L=parameters[i,1]
   x0=parameters[i,2]
   k=parameters[i,3]
   b=parameters[i,4]
  y1 = L / (1 + exp(-k*(x-x0))) + b
x1=Find_Max_CCF(y0,y1)+x
print(Find_Max_CCF(y0,y1))

  	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,max),xlim=c(-500,xmax) )
par(new=TRUE)	
ss=ss+1
ymatrix2<-c(ymatrix2,y1)
xmatrix2<-c(xmatrix2, x1)
sslist<-c(sslist,rep(ss,length(y1)))
}
 	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,max),xlim=c(-500,xmax) )

all_data_bd21_plot<-cbind(c(xmatrix1, xmatrix2),c(ymatrix1, ymatrix2),sslist,c(rep("Control",length(ymatrix1)),rep("Drought",length(ymatrix2))))

all_data_bd21_plot<-all_data_bd21_plot[which(all_data_bd21_plot[,1]>0),]

all_data_bd21_plot<-all_data_bd21_plot[which(all_data_bd21_plot[,1]<plyr::count(c(xmatrix1, xmatrix2))[max(which(plyr::count(c(xmatrix1, xmatrix2))[,2]==max(plyr::count(c(xmatrix1, xmatrix2))[,2]))),1]),]



colnames(all_data_bd21_plot)<-c("location","size","sample","treatment")
write.csv(as.matrix(all_data_bd21_plot),"~/Downloads/tmp.csv")
all_data_bd21_plot <-read.csv(paste0("~/Downloads/tmp.csv"), header=T,row.names=1)
library(ggplot2)
library(reshape2)

all_data_bd21_plot$treatment<-as.factor(all_data_bd21_plot$treatment)

bbb2<-ggplot(all_data_bd21_plot,aes(location,size))+
  theme_classic()+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = treatment))+
  labs(x = "Thermal time (°Cd)",y = expression(paste("Leaf length (mm)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",size = 1,aes(fill = treatment),alpha = 0.3)+
  guides(fill = "none")+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = treatment))+
  labs(x = "Thermal time (°Cd)",y = expression(paste("Leaf length (mm)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  geom_line(aes(group = interaction(sample, treatment),colour = treatment,alpha = 0.2))+
  guides(alpha= "none") + scale_color_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+scale_fill_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+ ggtitle(
"Wheat-CS-Domesticated")


a<-"vavic_2"
aa<-"vavid_2"
bb<-7
ss=0
max<-max(name[,which(substr(colnames(name),1,bb)%in%c(a,aa))], na.rm=T)
xmax<-max(parameters[which(substr(colnames(name),1,bb)%in%c(a,aa)),2], na.rm=T)*3
xmatrix1<-xmatrix2<-ymatrix1<-ymatrix2<-NULL
sslist<-NULL
for(i in which(substr(colnames(name),1,bb)==a)){
L=parameters[which(substr(colnames(name),1,bb)==a)[1],1]
   x0=parameters[which(substr(colnames(name),1,bb)==a)[1],2]
   k=parameters[which(substr(colnames(name),1,bb)==a)[1],3]
   b=parameters[which(substr(colnames(name),1,bb)==a)[1],4]
      x=c(-500:5:500)
    y0 = L / (1 + exp(-k*(x-x0))) + b

  L=parameters[i,1]
   x0=parameters[i,2]
   k=parameters[i,3]
   b=parameters[i,4]
  y1 = L / (1 + exp(-k*(x-x0))) + b
x1=Find_Max_CCF(y0,y1)+x
print(Find_Max_CCF(y0,y1))
  	plot( x1, y1, type="l", col="#66CC66",ylim=c(0,max),xlim=c(-500,xmax) )
par(new=TRUE)	
ss=ss+1
ymatrix1<-c(ymatrix1,y1)
xmatrix1<-c(xmatrix1, x1)
sslist<-c(sslist,rep(ss,length(y1)))
}
for(i in which(substr(colnames(name),1,bb)==aa)){
L=parameters[which(substr(colnames(name),1,bb)==aa)[1],1]
   x0=parameters[which(substr(colnames(name),1,bb)==aa)[1],2]
   k=parameters[which(substr(colnames(name),1,bb)==aa)[1],3]
   b=parameters[which(substr(colnames(name),1,bb)==aa)[1],4]
      x=c(-500:5:500)
    y0 = L / (1 + exp(-k*(x-x0))) + b

  L=parameters[i,1]
   x0=parameters[i,2]
   k=parameters[i,3]
   b=parameters[i,4]
  y1 = L / (1 + exp(-k*(x-x0))) + b
x1=Find_Max_CCF(y0,y1)+x
print(Find_Max_CCF(y0,y1))

  	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,max),xlim=c(-500,xmax) )
par(new=TRUE)	
ss=ss+1
ymatrix2<-c(ymatrix2,y1)
xmatrix2<-c(xmatrix2, x1)
sslist<-c(sslist,rep(ss,length(y1)))
}
 	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,max),xlim=c(-500,xmax) )

all_data_bd21_plot<-cbind(c(xmatrix1, xmatrix2),c(ymatrix1, ymatrix2),sslist,c(rep("Control",length(ymatrix1)),rep("Drought",length(ymatrix2))))

all_data_bd21_plot<-all_data_bd21_plot[which(all_data_bd21_plot[,1]>0),]

all_data_bd21_plot<-all_data_bd21_plot[which(all_data_bd21_plot[,1]<plyr::count(c(xmatrix1, xmatrix2))[max(which(plyr::count(c(xmatrix1, xmatrix2))[,2]==max(plyr::count(c(xmatrix1, xmatrix2))[,2]))),1]),]



colnames(all_data_bd21_plot)<-c("location","size","sample","treatment")
write.csv(as.matrix(all_data_bd21_plot),"~/Downloads/tmp.csv")
all_data_bd21_plot <-read.csv(paste0("~/Downloads/tmp.csv"), header=T,row.names=1)
library(ggplot2)
library(reshape2)

all_data_bd21_plot$treatment<-as.factor(all_data_bd21_plot$treatment)

c <-ggplot(all_data_bd21_plot,aes(location,size))+
  theme_classic()+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = treatment))+
  labs(x = "Thermal time (°Cd)",y = expression(paste("Leaf length (mm)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",size = 1,aes(fill = treatment),alpha = 0.3)+
  guides(fill = "none")+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = treatment))+
  labs(x = "Thermal time (°Cd)",y = expression(paste("Leaf length (mm)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  geom_line(aes(group = interaction(sample, treatment),colour = treatment,alpha = 0.2))+
  guides(alpha= "none") + scale_color_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+scale_fill_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+ ggtitle(
"Wheat-V-Wild")
  
  
  
  a<-"svevoc_2"
aa<-"svevod_2"
bb<-8
ss=0
max<-max(name[,which(substr(colnames(name),1,bb)%in%c(a,aa))], na.rm=T)
xmax<-max(parameters[which(substr(colnames(name),1,bb)%in%c(a,aa)),2], na.rm=T)*3
xmatrix1<-xmatrix2<-ymatrix1<-ymatrix2<-NULL
sslist<-NULL
for(i in which(substr(colnames(name),1,bb)==a)){
L=parameters[which(substr(colnames(name),1,bb)==a)[1],1]
   x0=parameters[which(substr(colnames(name),1,bb)==a)[1],2]
   k=parameters[which(substr(colnames(name),1,bb)==a)[1],3]
   b=parameters[which(substr(colnames(name),1,bb)==a)[1],4]
      x=c(-500:5:500)
    y0 = L / (1 + exp(-k*(x-x0))) + b

  L=parameters[i,1]
   x0=parameters[i,2]
   k=parameters[i,3]
   b=parameters[i,4]
  y1 = L / (1 + exp(-k*(x-x0))) + b
x1=Find_Max_CCF(y0,y1)+x
print(Find_Max_CCF(y0,y1))
  	plot( x1, y1, type="l", col="#66CC66",ylim=c(0,max),xlim=c(-500,xmax) )
par(new=TRUE)	
ss=ss+1
ymatrix1<-c(ymatrix1,y1)
xmatrix1<-c(xmatrix1, x1)
sslist<-c(sslist,rep(ss,length(y1)))
}
for(i in which(substr(colnames(name),1,bb)==aa)){
L=parameters[which(substr(colnames(name),1,bb)==aa)[1],1]
   x0=parameters[which(substr(colnames(name),1,bb)==aa)[1],2]
   k=parameters[which(substr(colnames(name),1,bb)==aa)[1],3]
   b=parameters[which(substr(colnames(name),1,bb)==aa)[1],4]
      x=c(-500:5:500)
    y0 = L / (1 + exp(-k*(x-x0))) + b

  L=parameters[i,1]
   x0=parameters[i,2]
   k=parameters[i,3]
   b=parameters[i,4]
  y1 = L / (1 + exp(-k*(x-x0))) + b
x1=Find_Max_CCF(y0,y1)+x
print(Find_Max_CCF(y0,y1))

  	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,max),xlim=c(-500,xmax) )
par(new=TRUE)	
ss=ss+1
ymatrix2<-c(ymatrix2,y1)
xmatrix2<-c(xmatrix2, x1)
sslist<-c(sslist,rep(ss,length(y1)))
}
 	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,max),xlim=c(-500,xmax) )

all_data_bd21_plot<-cbind(c(xmatrix1, xmatrix2),c(ymatrix1, ymatrix2),sslist,c(rep("Control",length(ymatrix1)),rep("Drought",length(ymatrix2))))

all_data_bd21_plot<-all_data_bd21_plot[which(all_data_bd21_plot[,1]>0),]

all_data_bd21_plot<-all_data_bd21_plot[which(all_data_bd21_plot[,1]<plyr::count(c(xmatrix1, xmatrix2))[max(which(plyr::count(c(xmatrix1, xmatrix2))[,2]==max(plyr::count(c(xmatrix1, xmatrix2))[,2]))),1]),]



colnames(all_data_bd21_plot)<-c("location","size","sample","treatment")
write.csv(as.matrix(all_data_bd21_plot),"~/Downloads/tmp.csv")
all_data_bd21_plot <-read.csv(paste0("~/Downloads/tmp.csv"), header=T,row.names=1)
library(ggplot2)
library(reshape2)

all_data_bd21_plot$treatment<-as.factor(all_data_bd21_plot$treatment)

d <-ggplot(all_data_bd21_plot,aes(location,size))+
  theme_classic()+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = treatment))+
  labs(x = "Thermal time (°Cd)",y = expression(paste("Leaf length (mm)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",size = 1,aes(fill = treatment),alpha = 0.3)+
  guides(fill = "none")+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = treatment))+
  labs(x = "Thermal time (°Cd)",y = expression(paste("Leaf length (mm)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  geom_line(aes(group = interaction(sample, treatment),colour = treatment,alpha = 0.2))+
  guides(alpha= "none") + scale_color_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+scale_fill_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+ ggtitle(
"Wheat-S-Domesticated")
  
   a<-"longdonc"
aa<-"longdond"
bb<-8
ss=0
max<-max(name[,which(substr(colnames(name),1,bb)%in%c(a,aa))], na.rm=T)
xmax<-max(parameters[which(substr(colnames(name),1,bb)%in%c(a,aa)),2], na.rm=T)*3
xmatrix1<-xmatrix2<-ymatrix1<-ymatrix2<-NULL
sslist<-NULL
for(i in which(substr(colnames(name),1,bb)==a)){
L=parameters[which(substr(colnames(name),1,bb)==a)[1],1]
   x0=parameters[which(substr(colnames(name),1,bb)==a)[1],2]
   k=parameters[which(substr(colnames(name),1,bb)==a)[1],3]
   b=parameters[which(substr(colnames(name),1,bb)==a)[1],4]
      x=c(-500:5:500)
    y0 = L / (1 + exp(-k*(x-x0))) + b

  L=parameters[i,1]
   x0=parameters[i,2]
   k=parameters[i,3]
   b=parameters[i,4]
  y1 = L / (1 + exp(-k*(x-x0))) + b
x1=Find_Max_CCF(y0,y1)+x
print(Find_Max_CCF(y0,y1))
  	plot( x1, y1, type="l", col="#66CC66",ylim=c(0,max),xlim=c(-500,xmax) )
par(new=TRUE)	
ss=ss+1
ymatrix1<-c(ymatrix1,y1)
xmatrix1<-c(xmatrix1, x1)
sslist<-c(sslist,rep(ss,length(y1)))
}
for(i in which(substr(colnames(name),1,bb)==aa)){
L=parameters[which(substr(colnames(name),1,bb)==aa)[1],1]
   x0=parameters[which(substr(colnames(name),1,bb)==aa)[1],2]
   k=parameters[which(substr(colnames(name),1,bb)==aa)[1],3]
   b=parameters[which(substr(colnames(name),1,bb)==aa)[1],4]
      x=c(-500:5:500)
    y0 = L / (1 + exp(-k*(x-x0))) + b

  L=parameters[i,1]
   x0=parameters[i,2]
   k=parameters[i,3]
   b=parameters[i,4]
  y1 = L / (1 + exp(-k*(x-x0))) + b
x1=Find_Max_CCF(y0,y1)+x
print(Find_Max_CCF(y0,y1))

  	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,max),xlim=c(-500,xmax) )
par(new=TRUE)	
ss=ss+1
ymatrix2<-c(ymatrix2,y1)
xmatrix2<-c(xmatrix2, x1)
sslist<-c(sslist,rep(ss,length(y1)))
}
 	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,max),xlim=c(-500,xmax) )

all_data_bd21_plot<-cbind(c(xmatrix1, xmatrix2),c(ymatrix1, ymatrix2),sslist,c(rep("Control",length(ymatrix1)),rep("Drought",length(ymatrix2))))

all_data_bd21_plot<-all_data_bd21_plot[which(all_data_bd21_plot[,1]>0),]

all_data_bd21_plot<-all_data_bd21_plot[which(all_data_bd21_plot[,1]<plyr::count(c(xmatrix1, xmatrix2))[max(which(plyr::count(c(xmatrix1, xmatrix2))[,2]==max(plyr::count(c(xmatrix1, xmatrix2))[,2]))),1]),]



colnames(all_data_bd21_plot)<-c("location","size","sample","treatment")
write.csv(as.matrix(all_data_bd21_plot),"~/Downloads/tmp.csv")
all_data_bd21_plot <-read.csv(paste0("~/Downloads/tmp.csv"), header=T,row.names=1)
library(ggplot2)
library(reshape2)

all_data_bd21_plot$treatment<-as.factor(all_data_bd21_plot$treatment)

e1 <-ggplot(all_data_bd21_plot,aes(location,size))+
  theme_classic()+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = treatment))+
  labs(x = "Thermal time (°Cd)",y = expression(paste("Leaf length (mm)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",size = 1,aes(fill = treatment),alpha = 0.3)+
  guides(fill = "none")+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = treatment))+
  labs(x = "Thermal time (°Cd)",y = expression(paste("Leaf length (mm)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  geom_line(aes(group = interaction(sample, treatment),colour = treatment,alpha = 0.2))+
  guides(alpha= "none") + scale_color_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+scale_fill_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+ ggtitle(
"Wheat-L-Domesticated 1st batch")
  

  
   a<-"longc_2"
aa<-"longd_2"
bb<-7
ss=0
max<-max(name[,which(substr(colnames(name),1,bb)%in%c(a,aa))], na.rm=T)
xmax<-max(parameters[which(substr(colnames(name),1,bb)%in%c(a,aa)),2], na.rm=T)*3
xmatrix1<-xmatrix2<-ymatrix1<-ymatrix2<-NULL
sslist<-NULL
for(i in which(substr(colnames(name),1,bb)==a)){
L=parameters[which(substr(colnames(name),1,bb)==a)[1],1]
   x0=parameters[which(substr(colnames(name),1,bb)==a)[1],2]
   k=parameters[which(substr(colnames(name),1,bb)==a)[1],3]
   b=parameters[which(substr(colnames(name),1,bb)==a)[1],4]
      x=c(-500:5:500)
    y0 = L / (1 + exp(-k*(x-x0))) + b

  L=parameters[i,1]
   x0=parameters[i,2]
   k=parameters[i,3]
   b=parameters[i,4]
  y1 = L / (1 + exp(-k*(x-x0))) + b
x1=Find_Max_CCF(y0,y1)+x
print(Find_Max_CCF(y0,y1))
  	plot( x1, y1, type="l", col="#66CC66",ylim=c(0,max),xlim=c(-500,xmax) )
par(new=TRUE)	
ss=ss+1
ymatrix1<-c(ymatrix1,y1)
xmatrix1<-c(xmatrix1, x1)
sslist<-c(sslist,rep(ss,length(y1)))
}
for(i in which(substr(colnames(name),1,bb)==aa)){
L=parameters[which(substr(colnames(name),1,bb)==aa)[1],1]
   x0=parameters[which(substr(colnames(name),1,bb)==aa)[1],2]
   k=parameters[which(substr(colnames(name),1,bb)==aa)[1],3]
   b=parameters[which(substr(colnames(name),1,bb)==aa)[1],4]
      x=c(-500:5:500)
    y0 = L / (1 + exp(-k*(x-x0))) + b

  L=parameters[i,1]
   x0=parameters[i,2]
   k=parameters[i,3]
   b=parameters[i,4]
  y1 = L / (1 + exp(-k*(x-x0))) + b
x1=Find_Max_CCF(y0,y1)+x
print(Find_Max_CCF(y0,y1))

  	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,max),xlim=c(-500,xmax) )
par(new=TRUE)	
ss=ss+1
ymatrix2<-c(ymatrix2,y1)
xmatrix2<-c(xmatrix2, x1)
sslist<-c(sslist,rep(ss,length(y1)))
}
 	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,max),xlim=c(-500,xmax) )

all_data_bd21_plot<-cbind(c(xmatrix1, xmatrix2),c(ymatrix1, ymatrix2),sslist,c(rep("Control",length(ymatrix1)),rep("Drought",length(ymatrix2))))

all_data_bd21_plot<-all_data_bd21_plot[which(all_data_bd21_plot[,1]>0),]

all_data_bd21_plot<-all_data_bd21_plot[which(all_data_bd21_plot[,1]<plyr::count(c(xmatrix1, xmatrix2))[max(which(plyr::count(c(xmatrix1, xmatrix2))[,2]==max(plyr::count(c(xmatrix1, xmatrix2))[,2]))),1]),]



colnames(all_data_bd21_plot)<-c("location","size","sample","treatment")
write.csv(as.matrix(all_data_bd21_plot),"~/Downloads/tmp.csv")
all_data_bd21_plot <-read.csv(paste0("~/Downloads/tmp.csv"), header=T,row.names=1)
library(ggplot2)
library(reshape2)

all_data_bd21_plot$treatment<-as.factor(all_data_bd21_plot$treatment)

e2 <-ggplot(all_data_bd21_plot,aes(location,size))+
  theme_classic()+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = treatment))+
  labs(x = "Thermal time (°Cd)",y = expression(paste("Leaf length (mm)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",size = 1,aes(fill = treatment),alpha = 0.3)+
  guides(fill = "none")+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = treatment))+
  labs(x = "Thermal time (°Cd)",y = expression(paste("Leaf length (mm)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  geom_line(aes(group = interaction(sample, treatment),colour = treatment,alpha = 0.2))+
  guides(alpha= "none") + scale_color_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+scale_fill_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+ ggtitle(
"Wheat-L-Domesticated")
  
   
   a<-"oatdc"
aa<-"oatdd"
bb<-5
ss=0
max<-max(name[,which(substr(colnames(name),1,bb)%in%c(a,aa))], na.rm=T)
xmax<-max(parameters[which(substr(colnames(name),1,bb)%in%c(a,aa)),2], na.rm=T)*3
xmatrix1<-xmatrix2<-ymatrix1<-ymatrix2<-NULL
sslist<-NULL
for(i in which(substr(colnames(name),1,bb)==a)){
L=parameters[which(substr(colnames(name),1,bb)==a)[1],1]
   x0=parameters[which(substr(colnames(name),1,bb)==a)[1],2]
   k=parameters[which(substr(colnames(name),1,bb)==a)[1],3]
   b=parameters[which(substr(colnames(name),1,bb)==a)[1],4]
      x=c(-500:5:500)
    y0 = L / (1 + exp(-k*(x-x0))) + b

  L=parameters[i,1]
   x0=parameters[i,2]
   k=parameters[i,3]
   b=parameters[i,4]
  y1 = L / (1 + exp(-k*(x-x0))) + b
x1=Find_Max_CCF(y0,y1)+x
print(Find_Max_CCF(y0,y1))
  	plot( x1, y1, type="l", col="#66CC66",ylim=c(0,max),xlim=c(-500,xmax) )
par(new=TRUE)	
ss=ss+1
ymatrix1<-c(ymatrix1,y1)
xmatrix1<-c(xmatrix1, x1)
sslist<-c(sslist,rep(ss,length(y1)))
}
for(i in which(substr(colnames(name),1,bb)==aa)){
L=parameters[which(substr(colnames(name),1,bb)==aa)[1],1]
   x0=parameters[which(substr(colnames(name),1,bb)==aa)[1],2]
   k=parameters[which(substr(colnames(name),1,bb)==aa)[1],3]
   b=parameters[which(substr(colnames(name),1,bb)==aa)[1],4]
      x=c(-500:5:500)
    y0 = L / (1 + exp(-k*(x-x0))) + b

  L=parameters[i,1]
   x0=parameters[i,2]
   k=parameters[i,3]
   b=parameters[i,4]
  y1 = L / (1 + exp(-k*(x-x0))) + b
x1=Find_Max_CCF(y0,y1)+x
print(Find_Max_CCF(y0,y1))

  	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,max),xlim=c(-500,xmax) )
par(new=TRUE)	
ss=ss+1
ymatrix2<-c(ymatrix2,y1)
xmatrix2<-c(xmatrix2, x1)
sslist<-c(sslist,rep(ss,length(y1)))
}
 	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,max),xlim=c(-500,xmax) )

all_data_bd21_plot<-cbind(c(xmatrix1, xmatrix2),c(ymatrix1, ymatrix2),sslist,c(rep("Control",length(ymatrix1)),rep("Drought",length(ymatrix2))))

all_data_bd21_plot<-all_data_bd21_plot[which(all_data_bd21_plot[,1]>0),]

all_data_bd21_plot<-all_data_bd21_plot[which(all_data_bd21_plot[,1]<plyr::count(c(xmatrix1, xmatrix2))[max(which(plyr::count(c(xmatrix1, xmatrix2))[,2]==max(plyr::count(c(xmatrix1, xmatrix2))[,2]))),1]),]



colnames(all_data_bd21_plot)<-c("location","size","sample","treatment")
write.csv(as.matrix(all_data_bd21_plot),"~/Downloads/tmp.csv")
all_data_bd21_plot <-read.csv(paste0("~/Downloads/tmp.csv"), header=T,row.names=1)
library(ggplot2)
library(reshape2)

all_data_bd21_plot$treatment<-as.factor(all_data_bd21_plot$treatment)

f <-ggplot(all_data_bd21_plot,aes(location,size))+
  theme_classic()+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = treatment))+
  labs(x = "Thermal time (°Cd)",y = expression(paste("Leaf length (mm)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",size = 1,aes(fill = treatment),alpha = 0.3)+
  guides(fill = "none")+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = treatment))+
  labs(x = "Thermal time (°Cd)",y = expression(paste("Leaf length (mm)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  geom_line(aes(group = interaction(sample, treatment),colour = treatment,alpha = 0.2))+
  guides(alpha= "none") + scale_color_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+scale_fill_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+ ggtitle(
"Oat-Domesticated")
  
     a<-"oatwc"
aa<-"oatwd"
bb<-5
ss=0
max<-max(name[,which(substr(colnames(name),1,bb)%in%c(a,aa))], na.rm=T)
xmax<-max(parameters[which(substr(colnames(name),1,bb)%in%c(a,aa)),2], na.rm=T)*3
xmatrix1<-xmatrix2<-ymatrix1<-ymatrix2<-NULL
sslist<-NULL
for(i in which(substr(colnames(name),1,bb)==a)){
L=parameters[which(substr(colnames(name),1,bb)==a)[1],1]
   x0=parameters[which(substr(colnames(name),1,bb)==a)[1],2]
   k=parameters[which(substr(colnames(name),1,bb)==a)[1],3]
   b=parameters[which(substr(colnames(name),1,bb)==a)[1],4]
      x=c(-500:5:500)
    y0 = L / (1 + exp(-k*(x-x0))) + b

  L=parameters[i,1]
   x0=parameters[i,2]
   k=parameters[i,3]
   b=parameters[i,4]
  y1 = L / (1 + exp(-k*(x-x0))) + b
x1=Find_Max_CCF(y0,y1)+x
print(Find_Max_CCF(y0,y1))
  	plot( x1, y1, type="l", col="#66CC66",ylim=c(0,max),xlim=c(-500,xmax) )
par(new=TRUE)	
ss=ss+1
ymatrix1<-c(ymatrix1,y1)
xmatrix1<-c(xmatrix1, x1)
sslist<-c(sslist,rep(ss,length(y1)))
}
for(i in which(substr(colnames(name),1,bb)==aa)){
L=parameters[which(substr(colnames(name),1,bb)==aa)[1],1]
   x0=parameters[which(substr(colnames(name),1,bb)==aa)[1],2]
   k=parameters[which(substr(colnames(name),1,bb)==aa)[1],3]
   b=parameters[which(substr(colnames(name),1,bb)==aa)[1],4]
      x=c(-500:5:500)
    y0 = L / (1 + exp(-k*(x-x0))) + b

  L=parameters[i,1]
   x0=parameters[i,2]
   k=parameters[i,3]
   b=parameters[i,4]
  y1 = L / (1 + exp(-k*(x-x0))) + b
x1=Find_Max_CCF(y0,y1)+x
print(Find_Max_CCF(y0,y1))

  	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,max),xlim=c(-500,xmax) )
par(new=TRUE)	
ss=ss+1
ymatrix2<-c(ymatrix2,y1)
xmatrix2<-c(xmatrix2, x1)
sslist<-c(sslist,rep(ss,length(y1)))
}
 	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,max),xlim=c(-500,xmax) )

all_data_bd21_plot<-cbind(c(xmatrix1, xmatrix2),c(ymatrix1, ymatrix2),sslist,c(rep("Control",length(ymatrix1)),rep("Drought",length(ymatrix2))))

all_data_bd21_plot<-all_data_bd21_plot[which(all_data_bd21_plot[,1]>0),]

all_data_bd21_plot<-all_data_bd21_plot[which(all_data_bd21_plot[,1]<plyr::count(c(xmatrix1, xmatrix2))[max(which(plyr::count(c(xmatrix1, xmatrix2))[,2]==max(plyr::count(c(xmatrix1, xmatrix2))[,2]))),1]),]



colnames(all_data_bd21_plot)<-c("location","size","sample","treatment")
write.csv(as.matrix(all_data_bd21_plot),"~/Downloads/tmp.csv")
all_data_bd21_plot <-read.csv(paste0("~/Downloads/tmp.csv"), header=T,row.names=1)
library(ggplot2)
library(reshape2)

all_data_bd21_plot$treatment<-as.factor(all_data_bd21_plot$treatment)

g <-ggplot(all_data_bd21_plot,aes(location,size))+
  theme_classic()+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = treatment))+
  labs(x = "Thermal time (°Cd)",y = expression(paste("Leaf length (mm)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",size = 1,aes(fill = treatment),alpha = 0.3)+
  guides(fill = "none")+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = treatment))+
  labs(x = "Thermal time (°Cd)",y = expression(paste("Leaf length (mm)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  geom_line(aes(group = interaction(sample, treatment),colour = treatment,alpha = 0.2))+
  guides(alpha= "none") + scale_color_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+scale_fill_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+ ggtitle(
"Oat-Wild")
  
  
  
     a<-"barleydc"
aa<-"barleydd"
bb<-8
ss=0
max<-max(name[,which(substr(colnames(name),1,bb)%in%c(a,aa))], na.rm=T)
xmax<-max(parameters[which(substr(colnames(name),1,bb)%in%c(a,aa)),2], na.rm=T)*3
xmatrix1<-xmatrix2<-ymatrix1<-ymatrix2<-NULL
sslist<-NULL
for(i in which(substr(colnames(name),1,bb)==a)){
L=parameters[which(substr(colnames(name),1,bb)==a)[1],1]
   x0=parameters[which(substr(colnames(name),1,bb)==a)[1],2]
   k=parameters[which(substr(colnames(name),1,bb)==a)[1],3]
   b=parameters[which(substr(colnames(name),1,bb)==a)[1],4]
      x=c(-500:5:500)
    y0 = L / (1 + exp(-k*(x-x0))) + b

  L=parameters[i,1]
   x0=parameters[i,2]
   k=parameters[i,3]
   b=parameters[i,4]
  y1 = L / (1 + exp(-k*(x-x0))) + b
x1=Find_Max_CCF(y0,y1)+x
print(Find_Max_CCF(y0,y1))
  	plot( x1, y1, type="l", col="#66CC66",ylim=c(0,max),xlim=c(-500,xmax) )
par(new=TRUE)	
ss=ss+1
ymatrix1<-c(ymatrix1,y1)
xmatrix1<-c(xmatrix1, x1)
sslist<-c(sslist,rep(ss,length(y1)))
}
for(i in which(substr(colnames(name),1,bb)==aa)){
L=parameters[which(substr(colnames(name),1,bb)==aa)[1],1]
   x0=parameters[which(substr(colnames(name),1,bb)==aa)[1],2]
   k=parameters[which(substr(colnames(name),1,bb)==aa)[1],3]
   b=parameters[which(substr(colnames(name),1,bb)==aa)[1],4]
      x=c(-500:5:500)
    y0 = L / (1 + exp(-k*(x-x0))) + b

  L=parameters[i,1]
   x0=parameters[i,2]
   k=parameters[i,3]
   b=parameters[i,4]
  y1 = L / (1 + exp(-k*(x-x0))) + b
x1=Find_Max_CCF(y0,y1)+x
print(Find_Max_CCF(y0,y1))

  	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,max),xlim=c(-500,xmax) )
par(new=TRUE)	
ss=ss+1
ymatrix2<-c(ymatrix2,y1)
xmatrix2<-c(xmatrix2, x1)
sslist<-c(sslist,rep(ss,length(y1)))
}
 	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,max),xlim=c(-500,xmax) )

all_data_bd21_plot<-cbind(c(xmatrix1, xmatrix2),c(ymatrix1, ymatrix2),sslist,c(rep("Control",length(ymatrix1)),rep("Drought",length(ymatrix2))))

all_data_bd21_plot<-all_data_bd21_plot[which(all_data_bd21_plot[,1]>0),]

all_data_bd21_plot<-all_data_bd21_plot[which(all_data_bd21_plot[,1]<plyr::count(c(xmatrix1, xmatrix2))[max(which(plyr::count(c(xmatrix1, xmatrix2))[,2]==max(plyr::count(c(xmatrix1, xmatrix2))[,2]))),1]),]



colnames(all_data_bd21_plot)<-c("location","size","sample","treatment")
write.csv(as.matrix(all_data_bd21_plot),"~/Downloads/tmp.csv")
all_data_bd21_plot <-read.csv(paste0("~/Downloads/tmp.csv"), header=T,row.names=1)
library(ggplot2)
library(reshape2)

all_data_bd21_plot$treatment<-as.factor(all_data_bd21_plot$treatment)

h <-ggplot(all_data_bd21_plot,aes(location,size))+
  theme_classic()+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = treatment))+
  labs(x = "Thermal time (°Cd)",y = expression(paste("Leaf length (mm)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",size = 1,aes(fill = treatment),alpha = 0.3)+
  guides(fill = "none")+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = treatment))+
  labs(x = "Thermal time (°Cd)",y = expression(paste("Leaf length (mm)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  geom_line(aes(group = interaction(sample, treatment),colour = treatment,alpha = 0.2))+
  guides(alpha= "none") + scale_color_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+scale_fill_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+ ggtitle(
"Barley-Domesticated")
  
     a<-"barleywc"
aa<-"barleywd"
bb<-8
ss=0
max<-max(name[,which(substr(colnames(name),1,bb)%in%c(a,aa))], na.rm=T)
xmax<-max(parameters[which(substr(colnames(name),1,bb)%in%c(a,aa)),2], na.rm=T)*3
xmatrix1<-xmatrix2<-ymatrix1<-ymatrix2<-NULL
sslist<-NULL
for(i in which(substr(colnames(name),1,bb)==a)){
L=parameters[which(substr(colnames(name),1,bb)==a)[1],1]
   x0=parameters[which(substr(colnames(name),1,bb)==a)[1],2]
   k=parameters[which(substr(colnames(name),1,bb)==a)[1],3]
   b=parameters[which(substr(colnames(name),1,bb)==a)[1],4]
      x=c(-500:5:500)
    y0 = L / (1 + exp(-k*(x-x0))) + b

  L=parameters[i,1]
   x0=parameters[i,2]
   k=parameters[i,3]
   b=parameters[i,4]
  y1 = L / (1 + exp(-k*(x-x0))) + b
x1=Find_Max_CCF(y0,y1)+x
print(Find_Max_CCF(y0,y1))
  	plot( x1, y1, type="l", col="#66CC66",ylim=c(0,max),xlim=c(-500,xmax) )
par(new=TRUE)	
ss=ss+1
ymatrix1<-c(ymatrix1,y1)
xmatrix1<-c(xmatrix1, x1)
sslist<-c(sslist,rep(ss,length(y1)))
}
for(i in which(substr(colnames(name),1,bb)==aa)){
L=parameters[which(substr(colnames(name),1,bb)==aa)[1],1]
   x0=parameters[which(substr(colnames(name),1,bb)==aa)[1],2]
   k=parameters[which(substr(colnames(name),1,bb)==aa)[1],3]
   b=parameters[which(substr(colnames(name),1,bb)==aa)[1],4]
      x=c(-500:5:500)
    y0 = L / (1 + exp(-k*(x-x0))) + b

  L=parameters[i,1]
   x0=parameters[i,2]
   k=parameters[i,3]
   b=parameters[i,4]
  y1 = L / (1 + exp(-k*(x-x0))) + b
x1=Find_Max_CCF(y0,y1)+x
print(Find_Max_CCF(y0,y1))

  	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,max),xlim=c(-500,xmax) )
par(new=TRUE)	
ss=ss+1
ymatrix2<-c(ymatrix2,y1)
xmatrix2<-c(xmatrix2, x1)
sslist<-c(sslist,rep(ss,length(y1)))
}
 	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,max),xlim=c(-500,xmax) )

all_data_bd21_plot<-cbind(c(xmatrix1, xmatrix2),c(ymatrix1, ymatrix2),sslist,c(rep("Control",length(ymatrix1)),rep("Drought",length(ymatrix2))))

all_data_bd21_plot<-all_data_bd21_plot[which(all_data_bd21_plot[,1]>0),]

all_data_bd21_plot<-all_data_bd21_plot[which(all_data_bd21_plot[,1]<plyr::count(c(xmatrix1, xmatrix2))[max(which(plyr::count(c(xmatrix1, xmatrix2))[,2]==max(plyr::count(c(xmatrix1, xmatrix2))[,2]))),1]),]



colnames(all_data_bd21_plot)<-c("location","size","sample","treatment")
write.csv(as.matrix(all_data_bd21_plot),"~/Downloads/tmp.csv")
all_data_bd21_plot <-read.csv(paste0("~/Downloads/tmp.csv"), header=T,row.names=1)
library(ggplot2)
library(reshape2)

all_data_bd21_plot$treatment<-as.factor(all_data_bd21_plot$treatment)

iii <-ggplot(all_data_bd21_plot,aes(location,size))+
  theme_classic()+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = treatment))+
  labs(x = "Thermal time (°Cd)",y = expression(paste("Leaf length (mm)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",size = 1,aes(fill = treatment),alpha = 0.3)+
  guides(fill = "none")+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = treatment))+
  labs(x = "Thermal time (°Cd)",y = expression(paste("Leaf length (mm)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  geom_line(aes(group = interaction(sample, treatment),colour = treatment,alpha = 0.2))+
  guides(alpha= "none") + scale_color_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+scale_fill_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+ ggtitle(
"Barley-Wild")
  
  
       a<-"bd21c"
aa<-"bd21d"
bb<-5
ss=0
max<-max(name[,which(substr(colnames(name),1,bb)%in%c(a,aa))], na.rm=T)
xmax<-max(parameters[which(substr(colnames(name),1,bb)%in%c(a,aa)),2], na.rm=T)*3
xmatrix1<-xmatrix2<-ymatrix1<-ymatrix2<-NULL
sslist<-NULL
for(i in which(substr(colnames(name),1,bb)==a)){
L=parameters[which(substr(colnames(name),1,bb)==a)[1],1]
   x0=parameters[which(substr(colnames(name),1,bb)==a)[1],2]
   k=parameters[which(substr(colnames(name),1,bb)==a)[1],3]
   b=parameters[which(substr(colnames(name),1,bb)==a)[1],4]
      x=c(-500:5:500)
    y0 = L / (1 + exp(-k*(x-x0))) + b

  L=parameters[i,1]
   x0=parameters[i,2]
   k=parameters[i,3]
   b=parameters[i,4]
  y1 = L / (1 + exp(-k*(x-x0))) + b
x1=Find_Max_CCF(y0,y1)+x
print(Find_Max_CCF(y0,y1))
  	plot( x1, y1, type="l", col="#66CC66",ylim=c(0,max),xlim=c(-500,xmax) )
par(new=TRUE)	
ss=ss+1
ymatrix1<-c(ymatrix1,y1)
xmatrix1<-c(xmatrix1, x1)
sslist<-c(sslist,rep(ss,length(y1)))
}
for(i in which(substr(colnames(name),1,bb)==aa)){
L=parameters[which(substr(colnames(name),1,bb)==aa)[1],1]
   x0=parameters[which(substr(colnames(name),1,bb)==aa)[1],2]
   k=parameters[which(substr(colnames(name),1,bb)==aa)[1],3]
   b=parameters[which(substr(colnames(name),1,bb)==aa)[1],4]
      x=c(-500:5:500)
    y0 = L / (1 + exp(-k*(x-x0))) + b

  L=parameters[i,1]
   x0=parameters[i,2]
   k=parameters[i,3]
   b=parameters[i,4]
  y1 = L / (1 + exp(-k*(x-x0))) + b
x1=Find_Max_CCF(y0,y1)+x
print(Find_Max_CCF(y0,y1))

  	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,max),xlim=c(-500,xmax) )
par(new=TRUE)	
ss=ss+1
ymatrix2<-c(ymatrix2,y1)
xmatrix2<-c(xmatrix2, x1)
sslist<-c(sslist,rep(ss,length(y1)))
}
 	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,max),xlim=c(-500,xmax) )

all_data_bd21_plot<-cbind(c(xmatrix1, xmatrix2),c(ymatrix1, ymatrix2),sslist,c(rep("Control",length(ymatrix1)),rep("Drought",length(ymatrix2))))

all_data_bd21_plot<-all_data_bd21_plot[which(all_data_bd21_plot[,1]>0),]

all_data_bd21_plot<-all_data_bd21_plot[which(all_data_bd21_plot[,1]<plyr::count(c(xmatrix1, xmatrix2))[max(which(plyr::count(c(xmatrix1, xmatrix2))[,2]==max(plyr::count(c(xmatrix1, xmatrix2))[,2]))),1]),]



colnames(all_data_bd21_plot)<-c("location","size","sample","treatment")
write.csv(as.matrix(all_data_bd21_plot),"~/Downloads/tmp.csv")
all_data_bd21_plot <-read.csv(paste0("~/Downloads/tmp.csv"), header=T,row.names=1)
library(ggplot2)
library(reshape2)

all_data_bd21_plot$treatment<-as.factor(all_data_bd21_plot$treatment)

j <-ggplot(all_data_bd21_plot,aes(location,size))+
  theme_classic()+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = treatment))+
  labs(x = "Thermal time (°Cd)",y = expression(paste("Leaf length (mm)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",size = 1,aes(fill = treatment),alpha = 0.3)+
  guides(fill = "none")+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = treatment))+
  labs(x = "Thermal time (°Cd)",y = expression(paste("Leaf length (mm)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  geom_line(aes(group = interaction(sample, treatment),colour = treatment,alpha = 0.2))+
  guides(alpha= "none") + scale_color_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+scale_fill_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+ ggtitle(
"Bd21")
  
  
  pdf(paste0("~/Downloads/elongation_plot_all.pdf"), width=16, height = 8)

ggarrange(j,aaaa2,  bbb2,e2,c,d,f,g,h,iii,ncol=5,nrow=2, common.legend=TRUE, legend="bottom")
dev.off()























  pdf(paste0("~/Downloads/allfit.pdf"), width=8, height = 12)
par(mfrow = c(6,4))
dataall <-read.csv(paste0("~/Desktop/chapter3final/elongation_summary_data.csv"), header=T,row.names=1)
dataall<-as.matrix(dataall)
dataall<-dataall[,-which(substr(colnames(dataall),1,7)=="wheat_w")]
parameters <-read.csv(paste0("~/Desktop/chapter3final/elongation_summary_models.csv"), header=T)
l10_90<-parameters[,12]-parameters[,11]
parameters<-cbind(parameters,l10_90)
for(i in 1:dim(parameters)[1]){
plot(as.numeric(rownames(dataall)),as.numeric(dataall[,i]),xlim=c(0,700),ylim=c(0,max(na.omit(as.numeric(dataall[,i])))),xlab="thermal time (°Cd)",ylab=expression(paste("leaf length (", mu, "m)")),main=colnames(dataall)[i])
par(new=TRUE)	
L=parameters[i,1]
   x0=parameters[i,2]
   k=parameters[i,3]
   b=parameters[i,4]
      x=c(-500:5:700)
    y0 = L / (1 + exp(-k*(x-x0))) + b
plot( x,y0, type="l", col="#66CC66",lDrought=2, xlim=c(0,700),ylim=c(0,max(na.omit(as.numeric(dataall[,i])))),xlab="",ylab="")
text(x = 200, y = 20, labels = paste("R² =", round(parameters$r_squared[i], 3)), cex = 1.5, col = "#FF6600")

}
dev.off()
