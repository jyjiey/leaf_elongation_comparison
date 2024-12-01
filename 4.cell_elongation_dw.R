library(matrixStats)
library(dplyr)

list1<- c("Oat D 1 control",  "Oat D 1 drought" , "Oat D 2 control"  ,"Oat D 2 drought" ,"Oat D 3 drought"  ,  "Oat D 4 control" , "Oat D 4 drought" , "Oat D 5 control" , "Oat D 5 drought"  , "Oat D extra 3 (control)","Oat W 1 drought" , "Oat W 2 control", "Oat W 2 drought" , "Oat W 3 control","Oat W 3 drought","Oat W 4 control" ,"Oat W 4 drought" , "Oat W 5 control", "Oat W 5 drought" )[-10]
list2<-c("Oat-D-1-control",  "Oat-D-1-drought" , "Oat-D-2-control"  ,"Oat-D-2-drought" ,"Oat-D-3-drought"  ,  "Oat-D-4-control" , "Oat-D-4-drought" , "Oat-D-5-control" , "Oat-D-5-drought" ,"Oat-D-extra-3-(control)"  ,"Oat-W-1-drought" , "Oat-W-2-control", "Oat-W-2-drought" , "Oat-W-3-control","Oat-W-3-drought","Oat-W-4-control" ,"Oat-W-4-drought" , "Oat-W-5-control", "Oat-W-5-drought")[-10]
empty <-c(0,0,0,420,470,2000,195,367,0,663,440,916,1585,536,780,1087,941,944,0)[-10]
namelength <-rep(43,length(list1))
list4<-namelength
orientation<-c("left","left","left","right","right","left","left","left","left","left","right","left","left","right","right","left","right","left","right")[-10]
matrix1<-matrix3<-matrix2<-matrix4<-matrix(0,231, length(list1))
matrix1<-matrix(0,301, length(list1))
divisionzonelist<-divisioncellsize <-NULL
list5<-c( "Oat-D-2-control"  ,"Oat-D-2-drought" ,"Oat-D-3-drought"  ,  "Oat-D-4-control" ,"Oat-D-5-drought","Oat-D-4-drought","Oat-W-1-drought" , "Oat-W-2-control", "Oat-W-2-drought" , "Oat-W-3-control","Oat-W-3-drought","Oat-W-4-control" ,"Oat-W-4-drought" ,"Oat-W-5-control", "Oat-W-5-drought", "Oat-D-1-control",  "Oat-D-1-drought" ,"Oat-D-5-control"  )

list2<-c("Oat-D-1-control",  "Oat-D-1-drought" , "Oat-D-2-control"  ,"Oat-D-2-drought" ,"Oat-D-3-drought"  ,  "Oat-D-4-control" , "Oat-D-4-drought" , "Oat-D-5-control" , "Oat-D-5-drought" ,"Oat-De3-control"  ,"Oat-W-1-drought" , "Oat-W-2-control", "Oat-W-2-drought" , "Oat-W-3-control","Oat-W-3-drought","Oat-W-4-control" ,"Oat-W-4-drought" , "Oat-W-5-control", "Oat-W-5-drought")[-10]
parameters <-read.csv(paste0("~/Downloads/matrixoat_2_out.csv"), header=T)
name <-read.csv(paste0("~/Downloads/matrixoat_2.csv"), header=T)

  colnames(name)<-c("location",list2)
  Find_Max_CCF<- function(a,b)
{
 d <- ccf(a, b, plot = FALSE)
 cor = d$acf[,,1]
 lag = d$lag[,,1]
 res = data.frame(cor,lag)
 res_max = res[which.max(res$cor),]
 return(res_max)
} 
all_data_bd21_plot<-NULL

a="control"
aa="drought"
bb=5
listname<-c("Oat-W","Oat-W")
list4<-substr(list2,bb+4,bb+10)

ss<-0
xmatrix1 <-xmatrix2<-ymatrix1<-ymatrix2<-NULL
sslist<-NULL
for(i in which((substr(colnames(name),bb+4,bb+10)==a)&(substr(colnames(name),1,bb)%in% listname))){

	i=i-1
L=parameters[which(substr(colnames(name),bb+4,bb+10)==a)[1],1]
   x0=parameters[which(substr(colnames(name),bb+4,bb+10)==a)[1],2]
   k=parameters[which(substr(colnames(name),bb+4,bb+10)==a)[1],3]
   b=parameters[which(substr(colnames(name),bb+4,bb+10)==a)[1],4]
   x=seq(-1000,30000,200)
    y0 = L / (1 + exp(-k*(x-x0))) + b

  L=parameters[i,1]
   x0=parameters[i,2]
   k=parameters[i,3]
   b=parameters[i,4]
  y1 = L / (1 + exp(-k*(x-x0))) + b
x1=x

  	plot( x1, y1, type="l", col="blue",ylim=c(0,400),xlim=c(0,xmax) ,xlab="Distance(um)", ylab="cell length (μm)" )
par(new=TRUE)	
ymatrix1<-c(ymatrix1,y1)
xmatrix1<-c(xmatrix1,x1)
ss=ss+1
sslist<-c(sslist,rep(ss,length(y1)))
x
}
all_data_bd21_plot<-rbind(all_data_bd21_plot ,cbind(c(xmatrix1),c(ymatrix1),sslist,c(rep(listname[1],length(ymatrix1)))))
a="control"
aa="drought"
bb=5
listname<-c("Oat-D","Oat-D")
list4<-substr(list2,bb+4,bb+10)

ss<-0

sslist<-NULL
for(i in which((substr(colnames(name),bb+4,bb+10)==a)&(substr(colnames(name),1,bb)%in% listname))){

	i=i-1
L=parameters[which(substr(colnames(name),bb+4,bb+10)==a)[1],1]
   x0=parameters[which(substr(colnames(name),bb+4,bb+10)==a)[1],2]
   k=parameters[which(substr(colnames(name),bb+4,bb+10)==a)[1],3]
   b=parameters[which(substr(colnames(name),bb+4,bb+10)==a)[1],4]
   x=seq(-1000,30000,200)
    y0 = L / (1 + exp(-k*(x-x0))) + b

  L=parameters[i,1]
   x0=parameters[i,2]
   k=parameters[i,3]
   b=parameters[i,4]
  y1 = L / (1 + exp(-k*(x-x0))) + b
x1=x

  	plot( x1, y1, type="l", col="blue",ylim=c(0,400),xlim=c(0,xmax) ,xlab="Distance(um)", ylab="cell length (μm)" )
par(new=TRUE)	
ymatrix2<-c(ymatrix2,y1)
xmatrix2<-c(xmatrix2,x1)
ss=ss+1
sslist<-c(sslist,rep(ss,length(y1)))
x
}
all_data_bd21_plot<-rbind(all_data_bd21_plot ,cbind(c(xmatrix2),c(ymatrix2),sslist,c(rep(listname[1],length(ymatrix2)))))
all_data_bd21_plot<-all_data_bd21_plot[which(all_data_bd21_plot[,1]>0),]


colnames(all_data_bd21_plot)<-c("location","size","sample","species")
all_data_bd21_plot[which(all_data_bd21_plot[,4]=="Oat-D"),4]<-"Oat-Domesticated"
all_data_bd21_plot[which(all_data_bd21_plot[,4]=="Oat-W"),4]<-"Oat-Wild"
write.csv(as.matrix(all_data_bd21_plot),"~/Downloads/tmp.csv")
all_data_bd21_plot <-read.csv(paste0("~/Downloads/tmp.csv"), header=T,row.names=1)
library(ggplot2)
library(reshape2)

all_data_bd21_plot$species <-as.factor(all_data_bd21_plot$species)
aaaaaaaaaa<-ggplot(all_data_bd21_plot,aes(location,size))+  theme_classic()+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = species))+
  labs(x = expression(paste("Distance (", mu, "m)")),y = expression(paste("Cell length (", mu, "m)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",size = 1,aes(fill = species),alpha = 0.3)+
  guides(fill = "none")+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = species))+
  labs(x = expression(paste("Distance (", mu, "m)")),y = expression(paste("Cell length (", mu, "m)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  geom_line(aes(group = interaction(sample, species),colour = species,alpha = 0.2))+
  guides(alpha= "none") + ggtitle("Oat")+ scale_color_manual(values = c("Oat-Domesticated" = "red", "Oat-Wild" = "blue"))+scale_fill_manual(values = c("Oat-Domesticated" = "red", "Oat-Wild" = "blue"))



all_data_bd21_plot<-NULL
elongationrate_c<-0.8303862
elongationrate_d<-0.3960259
a="control"
aa="drought"
bb=5
listname<-c("Oat-W","Oat-W")
list4<-substr(list2,bb+4,bb+10)


max<-max(name[,which(substr(colnames(name),1,bb)%in% listname)], na.rm=T)
xmax<-max(parameters[,2], na.rm=T)*7
ymatrix1<-ymatrix2<-NULL

ss<-0
xmatrix1<-xmatrix2<-ymatrix1<-ymatrix2<-NULL
sslist<-NULL
for(i in which((substr(colnames(name),bb+4,bb+10)==a)&(substr(colnames(name),1,bb)%in% listname))){

	i=i-1
L=parameters[which(substr(colnames(name),bb+4,bb+10)==a)[1],1]
   x0=parameters[which(substr(colnames(name),bb+4,bb+10)==a)[1],2]
   k=parameters[which(substr(colnames(name),bb+4,bb+10)==a)[1],3]
   b=parameters[which(substr(colnames(name),bb+4,bb+10)==a)[1],4]
   x=seq(-1000,50000,200)
    y0 = L / (1 + exp(-k*(x-x0))) + b

  L=parameters[i,1]
   x0=parameters[i,2]
   k=parameters[i,3]
   b=parameters[i,4]

x1=x


rootlist<-NULL
for (ismall in 1:length(x1)){
g <- Vectorize(function(x) parameters[i,5]/elongationrate_c*(integrate(function(t) 1/( L / (1 + exp(-k*(t-x0))) + b), lower = 0, upper = x)$value)-x1[ismall])
root <- uniroot(g,c(-10000,200000))$root
rootlist<-c(rootlist,root)
}

  y1 = L / (1 + exp(-k*(rootlist-x0))) + b
xmatrix1<-c(xmatrix1,x1)

print(Find_Max_CCF(y0,y1)$lag)
  	plot( x1, y1, type="l", col="blue",ylim=c(0,400),xlim=c(0,xmax) ,xlab="Distance(um)", ylab="cell length (μm)" )
par(new=TRUE)	
ymatrix1<-c(ymatrix1,y1)
ss=ss+1
sslist<-c(sslist,rep(ss,length(y1)))

}
all_data_bd21_plot<-rbind(all_data_bd21_plot ,cbind(c(xmatrix1),c(ymatrix1),sslist,c(rep(listname[1],length(ymatrix1)))))

a="control"
aa="drought"
bb=5
listname<-c("Oat-D","Oat-D")
list4<-substr(list2,bb+4,bb+10)

elongationrate_c<-1.2427181
elongationrate_d<-0.7445045


sslist<-NULL
for(i in which((substr(colnames(name),bb+4,bb+10)==a)&(substr(colnames(name),1,bb)%in% listname))){

	i=i-1
L=parameters[which(substr(colnames(name),bb+4,bb+10)==a)[1],1]
   x0=parameters[which(substr(colnames(name),bb+4,bb+10)==a)[1],2]
   k=parameters[which(substr(colnames(name),bb+4,bb+10)==a)[1],3]
   b=parameters[which(substr(colnames(name),bb+4,bb+10)==a)[1],4]
   x=seq(-1000,50000,200)
    y0 = L / (1 + exp(-k*(x-x0))) + b

  L=parameters[i,1]
   x0=parameters[i,2]
   k=parameters[i,3]
   b=parameters[i,4]
 x1=x


rootlist<-NULL
for (ismall in 1:length(x1)){
g <- Vectorize(function(x) parameters[i,5]/elongationrate_c*(integrate(function(t) 1/( L / (1 + exp(-k*(t-x0))) + b), lower = 0, upper = x)$value)-x1[ismall])
root <- uniroot(g,c(-10000,200000))$root
rootlist<-c(rootlist,root)
}

  y1 = L / (1 + exp(-k*(rootlist-x0))) + b

xmatrix2<-c(xmatrix2,x1)
print(Find_Max_CCF(y0,y1)$lag)
  	plot( x1, y1, type="l", col="blue",ylim=c(0,400),xlim=c(0,xmax) ,xlab="Distance(um)", ylab="cell length (μm)" )
par(new=TRUE)	
ymatrix2<-c(ymatrix2,y1)
ss=ss+1
sslist<-c(sslist,rep(ss,length(y1)))

}

all_data_bd21_plot<-rbind(all_data_bd21_plot ,cbind(c(xmatrix2),c(ymatrix2),sslist,c(rep(listname[1],length(ymatrix2)))))
all_data_bd21_plot<-all_data_bd21_plot[which(all_data_bd21_plot[,1]>0),]


colnames(all_data_bd21_plot)<-c("location","size","sample","species")
all_data_bd21_plot[which(all_data_bd21_plot[,4]=="Oat-D"),4]<-"Oat-Domesticated"
all_data_bd21_plot[which(all_data_bd21_plot[,4]=="Oat-W"),4]<-"Oat-Wild"

write.csv(as.matrix(all_data_bd21_plot),"~/Downloads/tmp.csv")
all_data_bd21_plot <-read.csv(paste0("~/Downloads/tmp.csv"), header=T,row.names=1)
library(ggplot2)
library(reshape2)
all_data_bd21_plot$location<-all_data_bd21_plot$location/1000
all_data_bd21_plot$species <-as.factor(all_data_bd21_plot$species)
aaaaaaaaaa2<-ggplot(all_data_bd21_plot,aes(location,size))+  theme_classic()+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = species))+
  labs(x = "Thermal time (°Cd)",y = expression(paste("Cell length (", mu, "m)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",size = 1,aes(fill = species),alpha = 0.3)+
  guides(fill = "none")+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = species))+
  labs(x = "Thermal time (°Cd)",y = expression(paste("Cell length (", mu, "m)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  geom_line(aes(group = interaction(sample, species),colour = species,alpha = 0.2))+
  guides(alpha= "none") + ggtitle("Oat")+ scale_color_manual(values = c("Oat-Domesticated" = "red", "Oat-Wild" = "blue"))+scale_fill_manual(values = c("Oat-Domesticated" = "red", "Oat-Wild" = "blue"))






library(matrixStats)
library(dplyr)

list1<- c("Barley D 1 control",
"Barley D 2 control",
"Barley D 2 drought",
"Barley D 3 control",
"Barley D 3 drought",
"Barley D 4 control",
"Barley D 4 drought",
"Barley D 4 drought redo",
"Barley D 5 control",
"Barley D 5 drought","Barley W 1 control",
"Barley W 1 drought redo",
"Barley W 2 control",
"Barley W 2 drought",
"Barley W 3 control",
"Barley W 3 drought",
"Barley W 4 control",
"Barley W 4 drought",
"Barley W 5 control redo"
)
list2<-c("Barley-D-1-control",
"Barley-D-2-control",
"Barley-D-2-drought",
"Barley-D-3-control",
"Barley-D-3-drought",
"Barley-D-4-control",
"Barley-D-4-drought",
"Barley-D-4-drought-redo",
"Barley-D-5-control",
"Barley-D-5-drought","Barley-W-1-control",
"Barley-W-1-drought-redo",
"Barley-W-2-control",
"Barley-W-2-drought",
"Barley-W-3-control",
"Barley-W-3-drought",
"Barley-W-4-control",
"Barley-W-4-drought",
"Barley-W-5-control-redo"
)
empty<-c(674,894,879,897,104,472,1220,770,1449,0,634,2000, 771,578,713,877,1145,0,0,730)
orientation<-c("left","right","left","left","left","left","left","left","left","left","left","right","right","right","left","right","right","right","right","right")
divisioncellsize<-divisionzonelist <-NULL
matrix1<-matrix3<-matrix2<-matrix5<-matrix4<-matrix8 <-matrix(0,331, length(list1))
matrix1<-matrix(0,401, length(list1))
list4<-namelength <-rep(43,length(list1))
list5<-c("Barley D 2 drought","Barley W 1 drought redo")


list2<-c("Barley-D-1-control",
"Barley-D-2-control",
"Barley-D-2-drought",
"Barley-D-3-control",
"Barley-D-3-drought",
"Barley-D-4-control",
"Barley-D-4-drought",
"Barley-D-4-drought-redo",
"Barley-D-5-control",
"Barley-D-5-drought","Barley-W-1-control",
"Barley-W-1-drought-redo",
"Barley-W-2-control",
"Barley-W-2-drought",
"Barley-W-3-control",
"Barley-W-3-drought",
"Barley-W-4-control",
"Barley-W-4-drought",
"Barley-W-5-control-redo"
)
all_data_bd21_plot<-NULL
parameters <-read.csv(paste0("~/Downloads/matrixbarley_out.csv"), header=T)
name <-read.csv(paste0("~/Downloads/matrixbarley.csv"), header=T)
  colnames(name)<-c("location",list2)
a="control"
aa="drought"
bb=8
listname<-c("Barley-W","Barley-W")
list4<-substr(list2,bb+4,bb+10)

ss<-0
xmatrix1 <-xmatrix2<-ymatrix1<-ymatrix2<-NULL
sslist<-NULL
for(i in which((substr(colnames(name),bb+4,bb+10)==a)&(substr(colnames(name),1,bb)%in% listname))){

	i=i-1
L=parameters[which(substr(colnames(name),bb+4,bb+10)==a)[1],1]
   x0=parameters[which(substr(colnames(name),bb+4,bb+10)==a)[1],2]
   k=parameters[which(substr(colnames(name),bb+4,bb+10)==a)[1],3]
   b=parameters[which(substr(colnames(name),bb+4,bb+10)==a)[1],4]
   x=seq(-1000,30000,200)
    y0 = L / (1 + exp(-k*(x-x0))) + b

  L=parameters[i,1]
   x0=parameters[i,2]
   k=parameters[i,3]
   b=parameters[i,4]
  y1 = L / (1 + exp(-k*(x-x0))) + b
x1=x

  	plot( x1, y1, type="l", col="blue",ylim=c(0,400),xlim=c(0,xmax) ,xlab="Distance(um)", ylab="cell length (μm)" )
par(new=TRUE)	
ymatrix1<-c(ymatrix1,y1)
xmatrix1<-c(xmatrix1,x1)
ss=ss+1
sslist<-c(sslist,rep(ss,length(y1)))
x
}
all_data_bd21_plot<-rbind(all_data_bd21_plot ,cbind(c(xmatrix1),c(ymatrix1),sslist,c(rep(listname[1],length(ymatrix1)))))



 
a="control"
aa="drought"
bb=8
listname<-c("Barley-D","Barley-D")
list4<-substr(list2,bb+4,bb+10)





ss<-0

sslist<-NULL
for(i in which((substr(colnames(name),bb+4,bb+10)==a)&(substr(colnames(name),1,bb)%in% listname))){

	i=i-1
L=parameters[which(substr(colnames(name),bb+4,bb+10)==a)[1],1]
   x0=parameters[which(substr(colnames(name),bb+4,bb+10)==a)[1],2]
   k=parameters[which(substr(colnames(name),bb+4,bb+10)==a)[1],3]
   b=parameters[which(substr(colnames(name),bb+4,bb+10)==a)[1],4]
   x=seq(-1000,30000,200)
    y0 = L / (1 + exp(-k*(x-x0))) + b

  L=parameters[i,1]
   x0=parameters[i,2]
   k=parameters[i,3]
   b=parameters[i,4]
  y1 = L / (1 + exp(-k*(x-x0))) + b
x1=x

  	plot( x1, y1, type="l", col="blue",ylim=c(0,400),xlim=c(0,xmax) ,xlab="Distance(um)", ylab="cell length (μm)" )
par(new=TRUE)	
ymatrix2<-c(ymatrix2,y1)
xmatrix2<-c(xmatrix2,x1)
ss=ss+1
sslist<-c(sslist,rep(ss,length(y1)))
x
}
all_data_bd21_plot<-rbind(all_data_bd21_plot ,cbind(c(xmatrix2),c(ymatrix2),sslist,c(rep(listname[1],length(ymatrix2)))))
all_data_bd21_plot<-all_data_bd21_plot[which(all_data_bd21_plot[,1]>0),]


colnames(all_data_bd21_plot)<-c("location","size","sample","species")
all_data_bd21_plot[which(all_data_bd21_plot[,4]=="Barley-D"),4]<-"Barley-Domesticated"
all_data_bd21_plot[which(all_data_bd21_plot[,4]=="Barley-W"),4]<-"Barley-Wild"

write.csv(as.matrix(all_data_bd21_plot),"~/Downloads/tmp.csv")
all_data_bd21_plot <-read.csv(paste0("~/Downloads/tmp.csv"), header=T,row.names=1)
library(ggplot2)
library(reshape2)

all_data_bd21_plot$species <-as.factor(all_data_bd21_plot$species)
bbbbbbbbbb<-ggplot(all_data_bd21_plot,aes(location,size))+  theme_classic()+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = species))+
  labs(x = expression(paste("Distance (", mu, "m)")),y = expression(paste("Cell length (", mu, "m)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",size = 1,aes(fill = species),alpha = 0.3)+
  guides(fill = "none")+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = species))+
  labs(x = expression(paste("Distance (", mu, "m)")),y = expression(paste("Cell length (", mu, "m)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  geom_line(aes(group = interaction(sample, species),colour = species,alpha = 0.2))+
  guides(alpha= "none")+ ggtitle("Barley")+ scale_color_manual(values = c("Barley-Domesticated" = "red", "Barley-Wild" = "blue"))+scale_fill_manual(values = c("Barley-Domesticated" = "red", "Barley-Wild" = "blue"))



all_data_bd21_plot<-NULL

elongationrate_c<-1.1870709
elongationrate_d<-0.4500016

a="control"
aa="drought"
bb=8
listname<-c("Barley-W","Barley-W")
list4<-substr(list2,bb+4,bb+10)


max<-max(name[,which(substr(colnames(name),1,bb)%in% listname)], na.rm=T)
xmax<-max(parameters[,2], na.rm=T)*7
ymatrix1<-ymatrix2<-NULL

ss<-0
xmatrix1<-xmatrix2<-ymatrix1<-ymatrix2<-NULL
sslist<-NULL
for(i in which((substr(colnames(name),bb+4,bb+10)==a)&(substr(colnames(name),1,bb)%in% listname))){

	i=i-1
L=parameters[which(substr(colnames(name),bb+4,bb+10)==a)[1],1]
   x0=parameters[which(substr(colnames(name),bb+4,bb+10)==a)[1],2]
   k=parameters[which(substr(colnames(name),bb+4,bb+10)==a)[1],3]
   b=parameters[which(substr(colnames(name),bb+4,bb+10)==a)[1],4]
   x=seq(-1000,50000,200)
    y0 = L / (1 + exp(-k*(x-x0))) + b

  L=parameters[i,1]
   x0=parameters[i,2]
   k=parameters[i,3]
   b=parameters[i,4]

x1=x


rootlist<-NULL
for (ismall in 1:length(x1)){
g <- Vectorize(function(x) parameters[i,5]/elongationrate_c*(integrate(function(t) 1/( L / (1 + exp(-k*(t-x0))) + b), lower = 0, upper = x)$value)-x1[ismall])
root <- uniroot(g,c(-10000,200000))$root
rootlist<-c(rootlist,root)
}

  y1 = L / (1 + exp(-k*(rootlist-x0))) + b
xmatrix1<-c(xmatrix1,x1)

print(Find_Max_CCF(y0,y1)$lag)
  	plot( x1, y1, type="l", col="blue",ylim=c(0,400),xlim=c(0,xmax) ,xlab="Distance(um)", ylab="cell length (μm)" )
par(new=TRUE)	
ymatrix1<-c(ymatrix1,y1)
ss=ss+1
sslist<-c(sslist,rep(ss,length(y1)))

}
all_data_bd21_plot<-rbind(all_data_bd21_plot ,cbind(c(xmatrix1),c(ymatrix1),sslist,c(rep(listname[1],length(ymatrix1)))))

a="control"
aa="drought"
bb=8
listname<-c("Barley-D","Barley-D")
list4<-substr(list2,bb+4,bb+10)

elongationrate_c<-1.3098889
elongationrate_d<-0.6172375





sslist<-NULL
for(i in which((substr(colnames(name),bb+4,bb+10)==a)&(substr(colnames(name),1,bb)%in% listname))){

	i=i-1
L=parameters[which(substr(colnames(name),bb+4,bb+10)==a)[1],1]
   x0=parameters[which(substr(colnames(name),bb+4,bb+10)==a)[1],2]
   k=parameters[which(substr(colnames(name),bb+4,bb+10)==a)[1],3]
   b=parameters[which(substr(colnames(name),bb+4,bb+10)==a)[1],4]
   x=seq(-1000,50000,200)
    y0 = L / (1 + exp(-k*(x-x0))) + b

  L=parameters[i,1]
   x0=parameters[i,2]
   k=parameters[i,3]
   b=parameters[i,4]
 x1=x


rootlist<-NULL
for (ismall in 1:length(x1)){
g <- Vectorize(function(x) parameters[i,5]/elongationrate_c*(integrate(function(t) 1/( L / (1 + exp(-k*(t-x0))) + b), lower = 0, upper = x)$value)-x1[ismall])
root <- uniroot(g,c(-10000,200000))$root
rootlist<-c(rootlist,root)
}

  y1 = L / (1 + exp(-k*(rootlist-x0))) + b

xmatrix2<-c(xmatrix2,x1)
print(Find_Max_CCF(y0,y1)$lag)
  	plot( x1, y1, type="l", col="blue",ylim=c(0,400),xlim=c(0,xmax) ,xlab="Distance(um)", ylab="cell length (μm)" )
par(new=TRUE)	
ymatrix2<-c(ymatrix2,y1)
ss=ss+1
sslist<-c(sslist,rep(ss,length(y1)))

}

all_data_bd21_plot<-rbind(all_data_bd21_plot ,cbind(c(xmatrix2),c(ymatrix2),sslist,c(rep(listname[1],length(ymatrix2)))))
all_data_bd21_plot<-all_data_bd21_plot[which(all_data_bd21_plot[,1]>0),]


colnames(all_data_bd21_plot)<-c("location","size","sample","species")
all_data_bd21_plot[which(all_data_bd21_plot[,4]=="Barley-D"),4]<-"Barley-Domesticated"
all_data_bd21_plot[which(all_data_bd21_plot[,4]=="Barley-W"),4]<-"Barley-Wild"
write.csv(as.matrix(all_data_bd21_plot),"~/Downloads/tmp.csv")
all_data_bd21_plot <-read.csv(paste0("~/Downloads/tmp.csv"), header=T,row.names=1)
library(ggplot2)
library(reshape2)
all_data_bd21_plot$location<-all_data_bd21_plot$location/1000
all_data_bd21_plot$species <-as.factor(all_data_bd21_plot$species)
bbbbbbbbbb2<-ggplot(all_data_bd21_plot,aes(location,size))+  theme_classic()+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = species))+
  labs(x = "Thermal time (°Cd)",y = expression(paste("Cell length (", mu, "m)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",size = 1,aes(fill = species),alpha = 0.3)+
  guides(fill = "none")+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = species))+
  labs(x = "Thermal time (°Cd)",y = expression(paste("Cell length (", mu, "m)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  geom_line(aes(group = interaction(sample, species),colour = species,alpha = 0.2))+
  guides(alpha= "none")+ ggtitle("Barley")+ scale_color_manual(values = c("Barley-Domesticated" = "red", "Barley-Wild" = "blue"))+scale_fill_manual(values = c("Barley-Domesticated" = "red", "Barley-Wild" = "blue"))




list1<-c("Wheat Z W 1 control",
"Wheat Z W 2 control",
 "Wheat Z W 3 control" ,
"Wheat Z W 4 control" ,
"zavi_ce2_normal" ,
"zavi_cm1","zavi_de_x7",
"zavi_de2","zavi_dex_redo1"	,"zavi_dm1",	"zavi_dm3")[-c(5,6,9)]

list2<-c("Wheat-Z-W-1-control",
"Wheat-Z-W-2-control",
 "Wheat-Z-W-3-control" ,
"Wheat-Z-W-4-control" ,
"zavi_ce2_normal" ,
"zavi_cm1","zavi_de_x7",
"zavi_de2","zavi_dex_redo1"	,"zavi_dm1",	"zavi_dm3")[-c(5,6,9)]
empty<-c(0,1183,860,0,750,1838,523,1137,400,1395,1591)[-c(5,6,9)]
orientation<-c("right","right","left","right","left","left","left","left","right","left","left")[-c(5,6,9)]
list3<-c(		
"2021-02-19-tris_minimal-0307_2023-05-25", "2021-02-19-tris_minimal-0307_2023-05-25", "2021-02-19-tris_minimal-0307_2023-05-25", "2021-02-19-tris_minimal-0307_2023-05-25" ,
			"zavi_ce2_normal",
		#	"zavi_ce2_whitebase"
				"zavi_cm1","zavi_dex",
		"zavi_de2", 
"zavi_dex",
#"Zavice2(1sttry)"	
#"Zavice2(redone)"	,
	"zavi_dm1",
		"zavi_dm3"
)[-c(5,6,9)]
namelength <-list4<-c(nchar(list3)+4)
list5<-c("zavi_dm1")


list2<-c("Wheat-Z-W-1-control",
"Wheat-Z-W-2-control",
 "Wheat-Z-W-3-control" ,
"Wheat-Z-W-4-control" ,
"zavi_ce2_normal" ,
"zavi_cm1","Wheat-Z-W7x-drought",
"Wheat-Z-We2-drought","zavi_dex_redo1"	,"Wheat-Z-Wm1-drought",	"Wheat-Z-Wm3-drought")[-c(5,6,9)]







all_data_bd21_plot<-NULL

parameters <-read.csv(paste0("~/Downloads/matrixwheatzavitan_out.csv"), header=T)
name <-read.csv(paste0("~/Downloads/matrixwheatzavitan.csv"), header=T)
  colnames(name)<-c("location",list2)
  a="control"
aa="drought"
bb=9
listname<-c("Wheat-Z-W","Wheat-Z-W")
list4<-substr(list2,bb+4,bb+10)

ss<-0
xmatrix1 <-xmatrix2<-ymatrix1<-ymatrix2<-xmatrix3<-ymatrix3<-NULL
sslist<-NULL
for(i in which((substr(colnames(name),bb+4,bb+10)==a)&(substr(colnames(name),1,bb)%in% listname))){

	i=i-1
L=parameters[which(substr(colnames(name),bb+4,bb+10)==a)[1],1]
   x0=parameters[which(substr(colnames(name),bb+4,bb+10)==a)[1],2]
   k=parameters[which(substr(colnames(name),bb+4,bb+10)==a)[1],3]
   b=parameters[which(substr(colnames(name),bb+4,bb+10)==a)[1],4]
   x=seq(-1000,30000,200)
    y0 = L / (1 + exp(-k*(x-x0))) + b

  L=parameters[i,1]
   x0=parameters[i,2]
   k=parameters[i,3]
   b=parameters[i,4]
  y1 = L / (1 + exp(-k*(x-x0))) + b
x1=x

  	plot( x1, y1, type="l", col="blue",ylim=c(0,400),xlim=c(0,xmax) ,xlab="Distance(um)", ylab="cell length (μm)" )
par(new=TRUE)	
ymatrix1<-c(ymatrix1,y1)
xmatrix1<-c(xmatrix1,x1)
ss=ss+1
sslist<-c(sslist,rep(ss,length(y1)))
x
}
all_data_bd21_plot<-rbind(all_data_bd21_plot ,cbind(c(xmatrix1),c(ymatrix1),sslist,c(rep(listname[1],length(ymatrix1)))))



 list1<- c("Wheat L D 1 control" ,"Wheat L D 1 drought", "Wheat L D 2 control", "Wheat L D 2 drought" ,"Wheat L D 3 control",  "Wheat L D 3 drought", "Wheat L D 4 control" , "Wheat L D 4 drought" )
 list2<-c("Wheat-L-D-1-control" ,"Wheat-L-D-1-drought", "Wheat-L-D-2-control", "Wheat-L-D-2-drought" ,"Wheat-L-D-3-control",  "Wheat-L-D-3-drought", "Wheat-L-D-4-control" , "Wheat-L-D-4-drought" )
orientation<-c("right","right","right","right","right","right","right",   "right")
empty<-c(650,578,1135,447,1011,0,904,583)
namelength <-list4<-rep(43,length(list1))
list5=c("Wheat L D 1 control" ,"Wheat L D 1 drought",  "Wheat L D 3 control" , "Wheat L D 3 drought", "Wheat L D 4 control" , "Wheat L D 4 drought" )

 list2<-c("Wheat-L-D-1-control" ,"Wheat-L-D-1-drought", "Wheat-L-D-2-control", "Wheat-L-D-2-drought" ,"Wheat-L-D-3-control",  "Wheat-L-D-3-drought", "Wheat-L-D-4-control" , "Wheat-L-D-4-drought" )
 
parameters <-read.csv(paste0("~/Downloads/matrixwheatL_out.csv"), header=T)
name <-read.csv(paste0("~/Downloads/matrixwheatL.csv"), header=T)
  colnames(name)<-c("location",list2)
a="control"
aa="drought"
bb=9
listname<-c("Wheat-L-D","Wheat-L-D")
list4<-substr(list2,bb+4,bb+10)





ss<-0

sslist<-NULL
for(i in which((substr(colnames(name),bb+4,bb+10)==a)&(substr(colnames(name),1,bb)%in% listname))){

	i=i-1
L=parameters[which(substr(colnames(name),bb+4,bb+10)==a)[1],1]
   x0=parameters[which(substr(colnames(name),bb+4,bb+10)==a)[1],2]
   k=parameters[which(substr(colnames(name),bb+4,bb+10)==a)[1],3]
   b=parameters[which(substr(colnames(name),bb+4,bb+10)==a)[1],4]
   x=seq(-1000,30000,200)
    y0 = L / (1 + exp(-k*(x-x0))) + b

  L=parameters[i,1]
   x0=parameters[i,2]
   k=parameters[i,3]
   b=parameters[i,4]
  y1 = L / (1 + exp(-k*(x-x0))) + b
x1=x

  	plot( x1, y1, type="l", col="blue",ylim=c(0,400),xlim=c(0,xmax) ,xlab="Distance(um)", ylab="cell length (μm)" )
par(new=TRUE)	
ymatrix2<-c(ymatrix2,y1)
xmatrix2<-c(xmatrix2,x1)
ss=ss+1
sslist<-c(sslist,rep(ss,length(y1)))
x
}
all_data_bd21_plot<-rbind(all_data_bd21_plot ,cbind(c(xmatrix2),c(ymatrix2),sslist,c(rep(listname[1],length(ymatrix2)))))



list1<-c("Wheat CS D 1 control" ,"Wheat CS D 1 drought" ,"Wheat CS D 2 control" ,"Wheat CS D 2 control extra",  "Wheat CS D 2 drought", "Wheat CS D 3 control", "Wheat CS D 3 drought"  ,"Wheat CS D 4 control" , "Wheat CS D 4 drought")
list2<-c("Wheat-CS-D-1-control" ,"Wheat-CS-D-1-drought" ,"Wheat-CS-D-2-control" ,"Wheat-CS-D-2-control-extra",  "Wheat-CS-D-2-drought", "Wheat-CS-D-3-control", "Wheat-CS-D-3-drought"  ,"Wheat-CS-D-4-control" , "Wheat-CS-D-4-drought")
list5=c( "Wheat CS D 2 drought", "Wheat CS D 3 control",  "Wheat CS D 3 drought" , "Wheat CS D 4 drought" , "Wheat CS D 1 control" , "Wheat CS D 1 drought")
empty<-c(1300,679,925,550,580,787,737,0,626)
orientation<-c("left","left","left","right","left","left","right","left","left")
namelength <-rep(43,length(list1))
list4<-namelength


list2<-c("Wheat-CS-D-1-control" ,"Wheat-CS-D-1-drought" ,"Wheat-CS-D-2-control" ,"Wheat-CS-D-2-control-extra",  "Wheat-CS-D-2-drought", "Wheat-CS-D-3-control", "Wheat-CS-D-3-drought"  ,"Wheat-CS-D-4-control" , "Wheat-CS-D-4-drought")

 parameters <-read.csv(paste0("~/Downloads/matrixwheatcs_out.csv"), header=T)
name <-read.csv(paste0("~/Downloads/matrixwheatcs.csv"), header=T)
  colnames(name)<-c("location",list2)
a="control"
aa="drought"
bb=10
listname<-c("Wheat-CS-D","Wheat-CS-D")
list4<-substr(list2,bb+4,bb+10)





ss<-0

sslist<-NULL
for(i in which((substr(colnames(name),bb+4,bb+10)==a)&(substr(colnames(name),1,bb)%in% listname))){

	i=i-1
L=parameters[which(substr(colnames(name),bb+4,bb+10)==a)[1],1]
   x0=parameters[which(substr(colnames(name),bb+4,bb+10)==a)[1],2]
   k=parameters[which(substr(colnames(name),bb+4,bb+10)==a)[1],3]
   b=parameters[which(substr(colnames(name),bb+4,bb+10)==a)[1],4]
   x=seq(-1000,30000,200)
    y0 = L / (1 + exp(-k*(x-x0))) + b

  L=parameters[i,1]
   x0=parameters[i,2]
   k=parameters[i,3]
   b=parameters[i,4]
  y1 = L / (1 + exp(-k*(x-x0))) + b
x1=x

  	plot( x1, y1, type="l", col="blue",ylim=c(0,400),xlim=c(0,xmax) ,xlab="Distance(um)", ylab="cell length (μm)" )
par(new=TRUE)	
ymatrix3<-c(ymatrix3,y1)
xmatrix3<-c(xmatrix3,x1)
ss=ss+1
sslist<-c(sslist,rep(ss,length(y1)))
x
}
all_data_bd21_plot<-rbind(all_data_bd21_plot ,cbind(c(xmatrix3),c(ymatrix3),sslist,c(rep(listname[1],length(ymatrix2)))))
all_data_bd21_plot<-all_data_bd21_plot[which(all_data_bd21_plot[,1]>0),]


colnames(all_data_bd21_plot)<-c("location","size","sample","species")
all_data_bd21_plot[which(all_data_bd21_plot[,4]=="Wheat-Z-W"),4]<-"Wheat-Z-Wild"
all_data_bd21_plot[which(all_data_bd21_plot[,4]=="Wheat-CS-D"),4]<-"Wheat-CS-Domesticated"
all_data_bd21_plot[which(all_data_bd21_plot[,4]=="Wheat-L-D"),4]<-"Wheat-L-Domesticated"

write.csv(as.matrix(all_data_bd21_plot),"~/Downloads/tmp.csv")
all_data_bd21_plot <-read.csv(paste0("~/Downloads/tmp.csv"), header=T,row.names=1)
library(ggplot2)
library(reshape2)

all_data_bd21_plot$species <-as.factor(all_data_bd21_plot$species)
cccccccccc<-ggplot(all_data_bd21_plot,aes(location,size))+
  theme_classic()+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = species))+
  labs(x = expression(paste("Distance (", mu, "m)")),y = expression(paste("Cell length (", mu, "m)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",size = 1,aes(fill = species),alpha = 0.3)+
  guides(fill = "none")+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = species))+
  labs(x = expression(paste("Distance (", mu, "m)")),y = expression(paste("Cell length (", mu, "m)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  geom_line(aes(group = interaction(sample, species),colour = species,alpha = 0.2))+
  guides(alpha= "none")+ ggtitle("Wheat")+ scale_color_manual(values = c("Wheat-CS-Domesticated" = "red", "Wheat-L-Domesticated" = "orange","Wheat-Z-Wild"="blue"))+scale_fill_manual(values = c("Wheat-CS-Domesticated" = "red", "Wheat-L-Domesticated" = "orange","Wheat-Z-Wild"="blue"))



list1<-c("Wheat Z W 1 control",
"Wheat Z W 2 control",
 "Wheat Z W 3 control" ,
"Wheat Z W 4 control" ,
"zavi_ce2_normal" ,
"zavi_cm1","zavi_de_x7",
"zavi_de2","zavi_dex_redo1"	,"zavi_dm1",	"zavi_dm3")[-c(5,6,9)]

list2<-c("Wheat-Z-W-1-control",
"Wheat-Z-W-2-control",
 "Wheat-Z-W-3-control" ,
"Wheat-Z-W-4-control" ,
"zavi_ce2_normal" ,
"zavi_cm1","zavi_de_x7",
"zavi_de2","zavi_dex_redo1"	,"zavi_dm1",	"zavi_dm3")[-c(5,6,9)]
empty<-c(0,1183,860,0,750,1838,523,1137,400,1395,1591)[-c(5,6,9)]
orientation<-c("right","right","left","right","left","left","left","left","right","left","left")[-c(5,6,9)]
list3<-c(		
"2021-02-19-tris_minimal-0307_2023-05-25", "2021-02-19-tris_minimal-0307_2023-05-25", "2021-02-19-tris_minimal-0307_2023-05-25", "2021-02-19-tris_minimal-0307_2023-05-25" ,
			"zavi_ce2_normal",
		#	"zavi_ce2_whitebase"
				"zavi_cm1","zavi_dex",
		"zavi_de2", 
"zavi_dex",
#"Zavice2(1sttry)"	
#"Zavice2(redone)"	,
	"zavi_dm1",
		"zavi_dm3"
)[-c(5,6,9)]
namelength <-list4<-c(nchar(list3)+4)
list5<-c("zavi_dm1")


list2<-c("Wheat-Z-W-1-control",
"Wheat-Z-W-2-control",
 "Wheat-Z-W-3-control" ,
"Wheat-Z-W-4-control" ,
"zavi_ce2_normal" ,
"zavi_cm1","Wheat-Z-W7x-drought",
"Wheat-Z-We2-drought","zavi_dex_redo1"	,"Wheat-Z-Wm1-drought",	"Wheat-Z-Wm3-drought")[-c(5,6,9)]







all_data_bd21_plot<-NULL

parameters <-read.csv(paste0("~/Downloads/matrixwheatzavitan_out.csv"), header=T)
name <-read.csv(paste0("~/Downloads/matrixwheatzavitan.csv"), header=T)
 colnames(name)<-c("location",list2)

a="control"
aa="drought"
bb=9
listname<-c("Wheat-Z-W","Wheat-Z-W")
list4<-substr(list2,bb+4,bb+10)
all_data_bd21_plot<-NULL

elongationrate_c<-0.7984710
elongationrate_d<-0.4778396



max<-max(name[,which(substr(colnames(name),1,bb)%in% listname)], na.rm=T)
xmax<-max(parameters[,2], na.rm=T)*7
ymatrix1<-ymatrix2<-NULL

ss<-0
xmatrix1<-xmatrix2<-ymatrix1<-ymatrix2<-xmatrix3<-ymatrix3<-NULL
sslist<-NULL
for(i in which((substr(colnames(name),bb+4,bb+10)==a)&(substr(colnames(name),1,bb)%in% listname))){

	i=i-1
L=parameters[which(substr(colnames(name),bb+4,bb+10)==a)[1],1]
   x0=parameters[which(substr(colnames(name),bb+4,bb+10)==a)[1],2]
   k=parameters[which(substr(colnames(name),bb+4,bb+10)==a)[1],3]
   b=parameters[which(substr(colnames(name),bb+4,bb+10)==a)[1],4]
   x=seq(-1000,50000,200)
    y0 = L / (1 + exp(-k*(x-x0))) + b

  L=parameters[i,1]
   x0=parameters[i,2]
   k=parameters[i,3]
   b=parameters[i,4]

x1=x


rootlist<-NULL
for (ismall in 1:length(x1)){
g <- Vectorize(function(x) parameters[i,5]/elongationrate_c*(integrate(function(t) 1/( L / (1 + exp(-k*(t-x0))) + b), lower = 0, upper = x)$value)-x1[ismall])
root <- uniroot(g,c(-10000,200000))$root
rootlist<-c(rootlist,root)
}

  y1 = L / (1 + exp(-k*(rootlist-x0))) + b
xmatrix1<-c(xmatrix1,x1)

print(Find_Max_CCF(y0,y1)$lag)
  	plot( x1, y1, type="l", col="blue",ylim=c(0,400),xlim=c(0,xmax) ,xlab="Distance(um)", ylab="cell length (μm)" )
par(new=TRUE)	
ymatrix1<-c(ymatrix1,y1)
ss=ss+1
sslist<-c(sslist,rep(ss,length(y1)))

}
all_data_bd21_plot<-rbind(all_data_bd21_plot ,cbind(c(xmatrix1),c(ymatrix1),sslist,c(rep(listname[1],length(ymatrix1)))))



 list1<- c("Wheat L D 1 control" ,"Wheat L D 1 drought", "Wheat L D 2 control", "Wheat L D 2 drought" ,"Wheat L D 3 control",  "Wheat L D 3 drought", "Wheat L D 4 control" , "Wheat L D 4 drought" )
 list2<-c("Wheat-L-D-1-control" ,"Wheat-L-D-1-drought", "Wheat-L-D-2-control", "Wheat-L-D-2-drought" ,"Wheat-L-D-3-control",  "Wheat-L-D-3-drought", "Wheat-L-D-4-control" , "Wheat-L-D-4-drought" )
orientation<-c("right","right","right","right","right","right","right",   "right")
empty<-c(650,578,1135,447,1011,0,904,583)
namelength <-list4<-rep(43,length(list1))
list5=c("Wheat L D 1 control" ,"Wheat L D 1 drought",  "Wheat L D 3 control" , "Wheat L D 3 drought", "Wheat L D 4 control" , "Wheat L D 4 drought" )

 list2<-c("Wheat-L-D-1-control" ,"Wheat-L-D-1-drought", "Wheat-L-D-2-control", "Wheat-L-D-2-drought" ,"Wheat-L-D-3-control",  "Wheat-L-D-3-drought", "Wheat-L-D-4-control" , "Wheat-L-D-4-drought" )
 parameters <-read.csv(paste0("~/Downloads/matrixwheatL_out.csv"), header=T)
name <-read.csv(paste0("~/Downloads/matrixwheatL.csv"), header=T)
  colnames(name)<-c("location",list2)
 a="control"
aa="drought"
bb=9
listname<-c("Wheat-L-D","Wheat-L-D")
list4<-substr(list2,bb+4,bb+10)


elongationrate_c<-1.2042611
elongationrate_d<-0.5052139





sslist<-NULL
for(i in which((substr(colnames(name),bb+4,bb+10)==a)&(substr(colnames(name),1,bb)%in% listname))){

	i=i-1
L=parameters[which(substr(colnames(name),bb+4,bb+10)==a)[1],1]
   x0=parameters[which(substr(colnames(name),bb+4,bb+10)==a)[1],2]
   k=parameters[which(substr(colnames(name),bb+4,bb+10)==a)[1],3]
   b=parameters[which(substr(colnames(name),bb+4,bb+10)==a)[1],4]
   x=seq(-1000,50000,200)
    y0 = L / (1 + exp(-k*(x-x0))) + b

  L=parameters[i,1]
   x0=parameters[i,2]
   k=parameters[i,3]
   b=parameters[i,4]
 x1=x


rootlist<-NULL
for (ismall in 1:length(x1)){
g <- Vectorize(function(x) parameters[i,5]/elongationrate_c*(integrate(function(t) 1/( L / (1 + exp(-k*(t-x0))) + b), lower = 0, upper = x)$value)-x1[ismall])
root <- uniroot(g,c(-10000,200000))$root
rootlist<-c(rootlist,root)
}

  y1 = L / (1 + exp(-k*(rootlist-x0))) + b

xmatrix2<-c(xmatrix2,x1)
print(Find_Max_CCF(y0,y1)$lag)
  	plot( x1, y1, type="l", col="blue",ylim=c(0,400),xlim=c(0,xmax) ,xlab="Distance(um)", ylab="cell length (μm)" )
par(new=TRUE)	
ymatrix2<-c(ymatrix2,y1)
ss=ss+1
sslist<-c(sslist,rep(ss,length(y1)))

}

all_data_bd21_plot<-rbind(all_data_bd21_plot ,cbind(c(xmatrix2),c(ymatrix2),sslist,c(rep(listname[1],length(ymatrix2)))))




list1<-c("Wheat CS D 1 control" ,"Wheat CS D 1 drought" ,"Wheat CS D 2 control" ,"Wheat CS D 2 control extra",  "Wheat CS D 2 drought", "Wheat CS D 3 control", "Wheat CS D 3 drought"  ,"Wheat CS D 4 control" , "Wheat CS D 4 drought")
list2<-c("Wheat-CS-D-1-control" ,"Wheat-CS-D-1-drought" ,"Wheat-CS-D-2-control" ,"Wheat-CS-D-2-control-extra",  "Wheat-CS-D-2-drought", "Wheat-CS-D-3-control", "Wheat-CS-D-3-drought"  ,"Wheat-CS-D-4-control" , "Wheat-CS-D-4-drought")
list5=c( "Wheat CS D 2 drought", "Wheat CS D 3 control",  "Wheat CS D 3 drought" , "Wheat CS D 4 drought" , "Wheat CS D 1 control" , "Wheat CS D 1 drought")
empty<-c(1300,679,925,550,580,787,737,0,626)
orientation<-c("left","left","left","right","left","left","right","left","left")
namelength <-rep(43,length(list1))
list4<-namelength


list2<-c("Wheat-CS-D-1-control" ,"Wheat-CS-D-1-drought" ,"Wheat-CS-D-2-control" ,"Wheat-CS-D-2-control-extra",  "Wheat-CS-D-2-drought", "Wheat-CS-D-3-control", "Wheat-CS-D-3-drought"  ,"Wheat-CS-D-4-control" , "Wheat-CS-D-4-drought")

  parameters <-read.csv(paste0("~/Downloads/matrixwheatcs_out.csv"), header=T)
name <-read.csv(paste0("~/Downloads/matrixwheatcs.csv"), header=T)
  colnames(name)<-c("location",list2)
 


a="control"
aa="drought"
bb=10
listname<-c("Wheat-CS-D","Wheat-CS-D")
list4<-substr(list2,bb+4,bb+10)


elongationrate_c<-1.0095155
elongationrate_d<-0.4312706


sslist<-NULL
for(i in which((substr(colnames(name),bb+4,bb+10)==a)&(substr(colnames(name),1,bb)%in% listname))){

	i=i-1
L=parameters[which(substr(colnames(name),bb+4,bb+10)==a)[1],1]
   x0=parameters[which(substr(colnames(name),bb+4,bb+10)==a)[1],2]
   k=parameters[which(substr(colnames(name),bb+4,bb+10)==a)[1],3]
   b=parameters[which(substr(colnames(name),bb+4,bb+10)==a)[1],4]
   x=seq(-1000,50000,200)
    y0 = L / (1 + exp(-k*(x-x0))) + b

  L=parameters[i,1]
   x0=parameters[i,2]
   k=parameters[i,3]
   b=parameters[i,4]
 x1=x


rootlist<-NULL
for (ismall in 1:length(x1)){
g <- Vectorize(function(x) parameters[i,5]/elongationrate_c*(integrate(function(t) 1/( L / (1 + exp(-k*(t-x0))) + b), lower = 0, upper = x)$value)-x1[ismall])
root <- uniroot(g,c(-10000,200000))$root
rootlist<-c(rootlist,root)
}

  y1 = L / (1 + exp(-k*(rootlist-x0))) + b

xmatrix3<-c(xmatrix3,x1)
print(Find_Max_CCF(y0,y1)$lag)
  	plot( x1, y1, type="l", col="blue",ylim=c(0,400),xlim=c(0,xmax) ,xlab="Distance(um)", ylab="cell length (μm)" )
par(new=TRUE)	
ymatrix3<-c(ymatrix3,y1)
ss=ss+1
sslist<-c(sslist,rep(ss,length(y1)))

}

all_data_bd21_plot<-rbind(all_data_bd21_plot ,cbind(c(xmatrix3),c(ymatrix3),sslist,c(rep(listname[1],length(ymatrix2)))))





all_data_bd21_plot<-all_data_bd21_plot[which(all_data_bd21_plot[,1]>0),]


colnames(all_data_bd21_plot)<-c("location","size","sample","species")
all_data_bd21_plot[which(all_data_bd21_plot[,4]=="Wheat-Z-W"),4]<-"Wheat-Z-Wild"
all_data_bd21_plot[which(all_data_bd21_plot[,4]=="Wheat-CS-D"),4]<-"Wheat-CS-Domesticated"
all_data_bd21_plot[which(all_data_bd21_plot[,4]=="Wheat-L-D"),4]<-"Wheat-L-Domesticated"

write.csv(as.matrix(all_data_bd21_plot),"~/Downloads/tmp.csv")
all_data_bd21_plot <-read.csv(paste0("~/Downloads/tmp.csv"), header=T,row.names=1)
library(ggplot2)
library(reshape2)
all_data_bd21_plot$location<-all_data_bd21_plot$location/1000
all_data_bd21_plot$species <-as.factor(all_data_bd21_plot$species)
cccccccccc2<-ggplot(all_data_bd21_plot,aes(location,size))+
  theme_classic()+
  stat_summary(aes(colour = species),fun.y = mean,geom = "line",size = 1)+
  labs(x = "Thermal time (°Cd)",y = expression(paste("Cell length (", mu, "m)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  stat_summary(aes(fill = species),fun.data = mean_cl_boot,geom = "ribbon",size = 1,alpha = 0.3)+
  guides(fill = "none")+
  stat_summary(aes(colour = species),fun.y = mean,geom = "line",size = 1)+
  labs(x = "Thermal time (°Cd)",y = expression(paste("Cell length (", mu, "m)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  geom_line(aes(group = interaction(sample, species),colour = species,alpha = 0.2))+
  guides(alpha= "none")+ ggtitle("Wheat")+ scale_color_manual(values = c("Wheat-CS-Domesticated" = "red", "Wheat-L-Domesticated" = "orange","Wheat-Z-Wild"="blue"))+scale_fill_manual(values = c("Wheat-CS-Domesticated" = "red", "Wheat-L-Domesticated" = "orange","Wheat-Z-Wild"="blue"))


  library(ggpubr)
pdf(paste0("~/Downloads/micro_plot3.pdf"), width=16, height = 16)

ggarrange(cccccccccc ,aaaaaaaaaa,bbbbbbbbbb,ncol=3,nrow=4, common.legend=FALSE)
dev.off()


pdf(paste0("~/Downloads/micro_plot4.pdf"), width=16, height = 16)

ggarrange(cccccccccc2 ,aaaaaaaaaa2,bbbbbbbbbb2,ncol=3,nrow=4, common.legend=FALSE)
dev.off()


