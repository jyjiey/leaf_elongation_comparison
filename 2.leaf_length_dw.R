library(ggpubr)
max=300
parameters <-read.csv(paste0("/Users/jiey/Desktop/chapter3final/elongation_summary_models.csv"), header=T)
l10_90<-parameters[,12]-parameters[,11]
parameters<-cbind(parameters,l10_90)

name <-read.csv(paste0("/Users/jiey/Desktop/chapter3final/elongation_summary_info.csv"), header=T)
data<-name[1:5,]
name<-name[-c(1:5),]
rownames(name)<-name[,1]
name<-name[,-1]
  data<-data[,-1]
~/Desktop/chapter3final/elongation_summary_data.csv

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



all_data_bd21_plot<-NULL
a<-aa<-"zavic_2"
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
  	plot( x1, y1, type="l", col="blue",ylim=c(0,max),xlim=c(-500,xmax) )
par(new=TRUE)	
ss=ss+1
ymatrix1<-c(ymatrix1,y1)
xmatrix1<-c(xmatrix1, x1)
sslist<-c(sslist,rep(ss,length(y1)))
}
all_data_bd21_plot<-rbind(all_data_bd21_plot ,cbind(c(xmatrix1),c(ymatrix1),sslist,c(rep(a,length(ymatrix1)))))


a<-aa<-"csc_2"
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
  	plot( x1, y1, type="l", col="blue",ylim=c(0,max),xlim=c(-500,xmax) )
par(new=TRUE)	
ss=ss+1
ymatrix1<-c(ymatrix1,y1)
xmatrix1<-c(xmatrix1, x1)
sslist<-c(sslist,rep(ss,length(y1)))
}
all_data_bd21_plot<-rbind(all_data_bd21_plot ,cbind(c(xmatrix1),c(ymatrix1),sslist,c(rep(a,length(ymatrix1)))))

a<-aa<-"vavic_2"
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
  	plot( x1, y1, type="l", col="blue",ylim=c(0,max),xlim=c(-500,xmax) )
par(new=TRUE)	
ss=ss+1
ymatrix1<-c(ymatrix1,y1)
xmatrix1<-c(xmatrix1, x1)
sslist<-c(sslist,rep(ss,length(y1)))
}
all_data_bd21_plot<-rbind(all_data_bd21_plot ,cbind(c(xmatrix1),c(ymatrix1),sslist,c(rep(a,length(ymatrix1)))))

 a<-aa<-"svevoc_2"

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
  	plot( x1, y1, type="l", col="blue",ylim=c(0,max),xlim=c(-500,xmax) )
par(new=TRUE)	
ss=ss+1
ymatrix1<-c(ymatrix1,y1)
xmatrix1<-c(xmatrix1, x1)
sslist<-c(sslist,rep(ss,length(y1)))
}
all_data_bd21_plot<-rbind(all_data_bd21_plot ,cbind(c(xmatrix1),c(ymatrix1),sslist,c(rep(a,length(ymatrix1)))))


  a<-aa<-"longc_2"
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
  	plot( x1, y1, type="l", col="blue",ylim=c(0,max),xlim=c(-500,xmax) )
par(new=TRUE)	
ss=ss+1
ymatrix1<-c(ymatrix1,y1)
xmatrix1<-c(xmatrix1, x1)
sslist<-c(sslist,rep(ss,length(y1)))
}

all_data_bd21_plot<-rbind(all_data_bd21_plot ,cbind(c(xmatrix1),c(ymatrix1),sslist,c(rep(a,length(ymatrix1)))))

all_data_bd21_plot<-all_data_bd21_plot[which(all_data_bd21_plot[,1]>0),]


all_data_bd21_plot<-all_data_bd21_plot[which(all_data_bd21_plot[,1]<400),]

colnames(all_data_bd21_plot)<-c("location","size","sample","species")
write.csv(as.matrix(all_data_bd21_plot),"~/Downloads/tmp.csv")
all_data_bd21_plot <-read.csv(paste0("~/Downloads/tmp.csv"), header=T,row.names=1)
all_data_bd21_plot$species[which(all_data_bd21_plot$species=="svevoc_2")]<-"Wheat-S-Domesticated"
all_data_bd21_plot$species[which(all_data_bd21_plot$species=="csc_2")]<-"Wheat-CS-Domesticated"
all_data_bd21_plot$species[which(all_data_bd21_plot$species=="longc_2")]<-"Wheat-L-Domesticated"
all_data_bd21_plot$species[which(all_data_bd21_plot$species=="zavic_2")]<-"Wheat-Z-Wild"
all_data_bd21_plot$species[which(all_data_bd21_plot$species=="vavic_2")]<-"Wheat-V-Wild"

library(ggplot2)
library(reshape2)

all_data_bd21_plot$species <-as.factor(all_data_bd21_plot$species)


aaaaaaaa<-ggplot(all_data_bd21_plot,aes(location,size))+
  theme_classic()+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = species))+
  labs(x = "Thermal time (°Cd)",y = expression(paste("Leaf length (mm)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",size = 1,aes(fill = species),alpha = 0.3)+
  guides(fill = "none")+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = species))+
  labs(x = "Thermal time (°Cd)",y = expression(paste("Leaf length (mm)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  geom_line(aes(group = interaction(sample, species),colour = species,alpha = 0.2))+ guides(alpha= "none",colour = guide_legend(nrow = 2)) + ggtitle("Wheat")+ scale_color_manual(values = c("Wheat-CS-Domesticated" = "red", "Wheat-L-Domesticated" = "orange","Wheat-S-Domesticated"="pink","Wheat-V-Wild"="purple","Wheat-Z-Wild"="blue"))+scale_fill_manual(values = c("Wheat-CS-Domesticated" = "red", "Wheat-L-Domesticated" = "orange","Wheat-S-Domesticated"="pink","Wheat-V-Wild"="purple","Wheat-Z-Wild"="blue"))+ guides(color = guide_legend(ncol = 1))

all_data_bd21_plot<-NULL
 a<-aa<-"oatdc"
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
  	plot( x1, y1, type="l", col="blue",ylim=c(0,max),xlim=c(-500,xmax) )
par(new=TRUE)	
ss=ss+1
ymatrix1<-c(ymatrix1,y1)
xmatrix1<-c(xmatrix1, x1)
sslist<-c(sslist,rep(ss,length(y1)))
}
all_data_bd21_plot<-rbind(all_data_bd21_plot ,cbind(c(xmatrix1),c(ymatrix1),sslist,c(rep(a,length(ymatrix1)))))

  a<-aa<-"oatwc"
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
  	plot( x1, y1, type="l", col="blue",ylim=c(0,max),xlim=c(-500,xmax) )
par(new=TRUE)	
ss=ss+1
ymatrix1<-c(ymatrix1,y1)
xmatrix1<-c(xmatrix1, x1)
sslist<-c(sslist,rep(ss,length(y1)))
}

all_data_bd21_plot<-rbind(all_data_bd21_plot ,cbind(c(xmatrix1),c(ymatrix1),sslist,c(rep(a,length(ymatrix1)))))

all_data_bd21_plot<-all_data_bd21_plot[which(all_data_bd21_plot[,1]>0),]


all_data_bd21_plot<-all_data_bd21_plot[which(all_data_bd21_plot[,1]<400),]

colnames(all_data_bd21_plot)<-c("location","size","sample","species")
write.csv(as.matrix(all_data_bd21_plot),"~/Downloads/tmp.csv")
all_data_bd21_plot <-read.csv(paste0("~/Downloads/tmp.csv"), header=T,row.names=1)
all_data_bd21_plot$species[which(all_data_bd21_plot$species=="oatdc")]<-"Oat-Domesticated"
all_data_bd21_plot$species[which(all_data_bd21_plot$species=="oatwc")]<-"Oat-Wild"
library(ggplot2)
library(reshape2)

all_data_bd21_plot$species <-as.factor(all_data_bd21_plot$species)


bbbbbbbb<-ggplot(all_data_bd21_plot,aes(location,size))+
  theme_classic()+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = species))+
  labs(x = "Thermal time (°Cd)",y = expression(paste("Leaf length (mm)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",size = 1,aes(fill = species),alpha = 0.3)+
  guides(fill = "none")+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = species))+
  labs(x = "Thermal time (°Cd)",y = expression(paste("Leaf length (mm)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  geom_line(aes(group = interaction(sample, species),colour = species,alpha = 0.2))+
  guides(alpha= "none") + ggtitle("Oat")+ scale_color_manual(values = c("Oat-Domesticated" = "red", "Oat-Wild"="blue"))+scale_fill_manual(values = c("Oat-Domesticated" = "red","Oat-Wild"="blue"))

all_data_bd21_plot<-NULL
 a<-aa<-"barleydc"
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
  	plot( x1, y1, type="l", col="blue",ylim=c(0,max),xlim=c(-500,xmax) )
par(new=TRUE)	
ss=ss+1
ymatrix1<-c(ymatrix1,y1)
xmatrix1<-c(xmatrix1, x1)
sslist<-c(sslist,rep(ss,length(y1)))
}
all_data_bd21_plot<-rbind(all_data_bd21_plot ,cbind(c(xmatrix1),c(ymatrix1),sslist,c(rep(a,length(ymatrix1)))))

  a<-aa<-"barleywc"
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
  	plot( x1, y1, type="l", col="blue",ylim=c(0,max),xlim=c(-500,xmax) )
par(new=TRUE)	
ss=ss+1
ymatrix1<-c(ymatrix1,y1)
xmatrix1<-c(xmatrix1, x1)
sslist<-c(sslist,rep(ss,length(y1)))
}

all_data_bd21_plot<-rbind(all_data_bd21_plot ,cbind(c(xmatrix1),c(ymatrix1),sslist,c(rep(a,length(ymatrix1)))))

all_data_bd21_plot<-all_data_bd21_plot[which(all_data_bd21_plot[,1]>0),]


all_data_bd21_plot<-all_data_bd21_plot[which(all_data_bd21_plot[,1]<400),]

colnames(all_data_bd21_plot)<-c("location","size","sample","species")
write.csv(as.matrix(all_data_bd21_plot),"~/Downloads/tmp.csv")
all_data_bd21_plot <-read.csv(paste0("~/Downloads/tmp.csv"), header=T,row.names=1)
all_data_bd21_plot$species[which(all_data_bd21_plot$species=="barleydc")]<-"Barley-Domesticated"
all_data_bd21_plot$species[which(all_data_bd21_plot$species=="barleywc")]<-"Barley-Wild"
library(ggplot2)
library(reshape2)

all_data_bd21_plot$species <-as.factor(all_data_bd21_plot$species)
cccccccc<-ggplot(all_data_bd21_plot,aes(location,size))+
  theme_classic()+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = species))+
  labs(x = "Thermal time (°Cd)",y = expression(paste("Leaf length (mm)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",size = 1,aes(fill = species),alpha = 0.3)+
  guides(fill = "none")+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = species))+
  labs(x = "Thermal time (°Cd)",y = expression(paste("Leaf length (mm)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  geom_line(aes(group = interaction(sample, species),colour = species,alpha = 0.2))+
  guides(alpha= "none") + ggtitle("Barley")+scale_fill_manual(values = c("Barley-Domesticated" = "red","Barley-Wild"="blue"))+scale_color_manual(values = c("Barley-Domesticated" = "red","Barley-Wild"="blue"))

  pdf(paste0("~/Downloads/elongation_plot_control.pdf"), width=16, height = 16)

ggarrange(aaaaaaaa,bbbbbbbb,cccccccc,ncol=3,nrow=4, common.legend=FALSE)
dev.off()


