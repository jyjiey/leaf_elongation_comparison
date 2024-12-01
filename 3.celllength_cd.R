#aaaa1,
remove.packages("plyr")
library(matrixStats)
library(dplyr)
list1<-c("Bd21 1 control","Bd21 4 control","Bd21 5 control","Bd21 1 drought","Bd21 3 drought redo","Bd21 4 drought","Bd21 5 drought","bd21-3-drought","bd21-extra-drought","bd21-3-drought-black","bd21-3-control","bd21-2extra-drought","bd21-2-control")[-c(10,12,13)]
list2<-c("Bd21-1-control","Bd21-4-control","Bd21-5-control","Bd21-1-drought","Bd21-3-drought-redo","Bd21-4-drought","Bd21-5-drought","bd21-3-drought","bd21-extra-drought","bd21-3-drought-black","bd21-3-control","bd21-2extra-drought","bd21-2-control")[-c(10,12,13)]
#check base images
cutoff1<-c(3,10,10,5,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3)[-c(10,12)]
empty <-c(700,2500,718,1700,560,1400,244,1460,0,950,1375,860,1260)[-c(10,12)]
orientation <-c("left","left","left","right","left","left","right","right","right","right","right","right","right")[-c(10,12)]
matrix1<-matrix3<-matrix8 <-matrix(0,331, length(list1)[-c(10,12)])
matrix1<-matrix(0,401, length(list1)[-c(10,12)])
divisioncellsize <-divisionzonelist<-NULL

namelength<-rep(43,length(list1))
list5<-c("Bd21 5 control","Bd21 1 drought","bd21-extra-drought","bd21-3-control","bd21-2-control")
  pdf(paste0("~/Downloads/bd21_7.pdf"), width=7, height = 4)


for(oo in c(1:length(list2))){
cell8_data<-cell8<-cell3<-cell3_data<-cell1_data<-cell1<-cell4_data<-cell4<-NULL
	step<-200
	#cells
numbercut1=5
#trichome
numbercut2=20
#handmark
numbercut3=1

location <-read.csv(paste0("/Volumes/TOSHIBAEXT/images/leaf\ extension\ 2/",list1[oo],"/stitching_result.csv"), header=T)
head(location)
head(location)
location[,3]<-location[,3]/2.7
location[,4]<-location[,4]/2.7
direction <-read.csv(paste0("/Volumes/TOSHIBAEXT/images/leaf\ extension\ 2/",list1[oo],"/dirs.csv"), header=T)
head(direction)
location[,3]<-location[,3]/cos(direction[-dim(direction)[2],2]) 
emptylength<-empty[oo]/2.7/cos(direction[1,2])


##base cells /Volumes/TOSHIBAEXT/image_results/newbase_results/Bd21-4-control/cells.csv
cell1 <-read.csv(paste0("/Volumes/TOSHIBAEXT/image_results/basenew/",list2[oo],"/cells.csv"), header=T)
head(cell1)
cell1[,3]<-cell1[,3]/2.7
cell1[,4]<-cell1[,4]/2.7
cell1[,5]<-rowMaxs(cbind(cell1[,5]/2.7, cell1[,6]/2.7))
##trichomes  /Volumes/TOSHIBAEXT/image_results/trichome_results/Bd21-4-control/pairs.csv 
cell3 <-read.csv(paste0("/Volumes/TOSHIBAEXT/image_results/trichome_results/",list2[oo],"/pairs.csv"), header=T)
head(cell3)
cell3[,3]<-cell3[,3]/2.7
cell3[,4]<-cell3[,4]/2.7
cell3[,5]<-cell3[,5]/2.7
cell3[,6]<-cell3[,6]/2.7
cell3[,7]<-cell3[,7]/2.7

cell4 <-read.csv(paste0("~/Desktop/divisionzonemark/",list2[oo],"/cells.csv"), header=T)
head(cell4)
cell4[,3]<-cell4[,3]/2.7
cell4[,4]<-cell4[,4]/2.7

##plot with three colors
real_location<-rep(0,dim(location)[1])
real_location[1]<-location$X.offset[1]
for (i in 2:dim(location)[1]){
	real_location[i]<-sum(location$X.offset[1:i])
}
real_location <-real_location[match(substr(cell4$Name,1,43),substr(location[,2],1,43))]
real_location[which(substr(cell4$Name,1,43)==substr(location[1,1],1,43))] <-0
if(orientation[oo]=="right"){
	#going right 
cell4[,8] <-c(cell4[,3]/cos(direction[match(substr(cell4[,1],1,43),substr(direction[,1],1,43) ),2])+ real_location)-emptylength
	}else{
#going left
	cell4[,8] <- c((3826/2.7-cell4[,3])/cos(direction[match(substr(cell4[,1],1,43),substr(direction[,1],1,43) ),2])- real_location)-emptylength
	}

divisionzone<-min(cell4[,8])

##plot with three colors
real_location<-rep(0,dim(location)[1])
real_location[1]<-location$X.offset[1]
for (i in 2:dim(location)[1]){
	real_location[i]<-sum(location$X.offset[1:i])
}
real_location <-real_location[match(substr(cell1$Name,1,43),substr(location[,2],1,43))]
real_location[which(substr(cell1$Name,1,43)==substr(location[1,1],1,43))] <-0
if(orientation[oo]=="right"){
	#going right 
cell1[,8] <-c(cell1[,3]/cos(direction[match(substr(cell1[,1],1,43),substr(direction[,1],1,43) ),2])+ real_location)-emptylength-divisionzone
	}else{
#going left
	cell1[,8] <- c((3826/2.7-cell1[,3])/cos(direction[match(substr(cell1[,1],1,43),substr(direction[,1],1,43) ),2])- real_location)-emptylength-divisionzone
	}
##plot with three colors
real_location<-rep(0,dim(location)[1])
real_location[1]<-location$X.offset[1]
for (i in 2:dim(location)[1]){
	real_location[i]<-sum(location$X.offset[1:i])
	
}
real_location <-real_location[match(substr(cell3$Name,1,43),substr(location[,2],1,43))]
real_location[which(substr(cell3$Name,1,43)==substr(location[1,1],1,43))] <-0
if(orientation[oo]=="right"){
	#going right 
cell3[,8] <-c(cell3[,3]/cos(direction[match(substr(cell3[,1],1,43),substr(direction[,1],1,43) ),2])+ real_location)-emptylength-divisionzone
	}else{
#going left
	cell3[,8] <- c((3826/2.7-cell3[,3])/cos(direction[match(substr(cell3[,1],1,43),substr(direction[,1],1,43) ),2])- real_location)-emptylength-divisionzone
	}
#write.csv(cell3,"~/Downloads/test/pairs_trichome.csv")

cell3<-cell3[which(cell3[,8]>0),]
cell1<-cell1[which(cell1[,8]<100),]





cell3<-na.omit(cell3)
cell1<-na.omit(cell1)

window<-(cell1[,8]) %/% step
df <-as.data.frame(cbind(cell1$Width, window))
cell1_data<-as.matrix(df %>% group_by(window) %>%
  summarize(res=quantile(V1,probs=0.5),number=length(V1)))
cell1_data<-cell1_data[which(cell1_data[,3]>numbercut1),]
#window<-(cell2[,8]-division_zone) %/% step
#df <-as.data.frame(cbind(cell2$Width, window))
#cell2_data<-as.matrix(df %>% group_by(window) %>%
#  summarize(res=quantile(V1,probs=0.5)))


window<-(cell3[,8]) %/% step
df <-as.data.frame(cbind(cell3$Distance, window))
cell3_data<-as.matrix(df %>% group_by(window) %>%
  summarize(res=quantile(V1,probs=0.2),number=length(V1)))
cell3_data<-cell3_data[which(cell3_data[,3]> numbercut2),]


  plot(cell3_data[,1]* step, cell3_data[,2],col="#FF6600",ylim=c(0,200))
#points(cell2_data[,1]* step, cell2_data[,2],col="green")


if(list1[oo]%in%list5){
cell8 <-read.csv(paste0("~/Desktop/handmark/",list1[oo],"/pairs.csv"), header=T)
head(cell8)
cell8[,3]<-cell8[,3]/2.7
cell8[,4]<-cell8[,4]/2.7
cell8[,5]<-cell8[,5]/2.7
cell8[,6]<-cell8[,6]/2.7
cell8[,7]<-cell8[,7]/2.7

real_location<-rep(0,dim(location)[1])
real_location[1]<-location$X.offset[1]
for (i in 2:dim(location)[1]){
	real_location[i]<-sum(location$X.offset[1:i])
	
}
real_location <-real_location[match(substr(cell8$Name,1,43),substr(location[,2],1,43))]
real_location[which(substr(cell8$Name,1,43)==substr(location[1,1],1,43))] <-0
if(orientation[oo]=="right"){
	#going right 
cell8[,8] <-c(cell8[,3]/cos(direction[match(substr(cell8[,1],1,43),substr(direction[,1],1,43) ),2])+ real_location)-emptylength-divisionzone
	}else{
#going left
	cell8[,8] <- c((3826/2.7-cell8[,3])/cos(direction[match(substr(cell8[,1],1,43),substr(direction[,1],1,43) ),2])- real_location)-emptylength-divisionzone
	}
#write.csv(cell3,"~/Downloads/test/pairs_trichome.csv")

cell8<-cell8[which(cell8[,8]>0),]
cell8<-na.omit(cell8)
window<-(cell8[,8]) %/% step
df <-as.data.frame(cbind(cell8$Distance, window))
cell8_data<-as.matrix(df %>% group_by(window) %>%
  summarize(res=quantile(V1,probs=0.2),number=length(V1)))
cell8_data<-cell8_data[which(cell8_data[,3]> 0),]

points(cell8_data[,1]* step, cell8_data[,2],col="green")
}else{
	cell8_data<-cell3_data[1:2,]
	}


testdata<-cell3_data
testdataoriginal<-cell3
for(i in 1:(dim(testdata)[1]-1)){
if((testdata[i+1,1]-testdata[i,1])>3){
	print(c(testdata[i+1,1]* step,testdata[i,1]*step, step*(testdata[i+1,1]-testdata[i,1])))
	print(plyr::count(testdataoriginal[which((testdataoriginal[,8]<testdata[i+1,1]*step)&(testdataoriginal[,8]>testdata[i,1]*step)),1]))
}
}
print(list1[oo])
distance<-problem<-NULL
for(i in 1:(dim(testdata)[1]-1)){
distance<-c(distance,testdata[i+1,2]-testdata[i,2])
}
	
for(i in 1:(dim(testdata)[1]-1)){
if((testdata[i+1,2]-testdata[i,2])%in%boxplot.stats(distance)$out){
	print(c(i, testdata[i+1,1]* step,testdata[i,1]*step, step*(testdata[i+1,1]-testdata[i,1])))
	print(plyr::count(testdataoriginal[which((testdataoriginal[,8]<testdata[i+1,1]*step)&(testdataoriginal[,8]>testdata[i,1]*step)),1]))
	problem<-c(problem,i)
}
}
  #plot(cell3_data[,1]* step, cell3_data[,2],col="#FF6600",ylim=c(0,200))
#points(cell2_data[,1]* step, cell2_data[,2],col="green")
#points(cell1_data[,1]* step, cell1_data[,2],col="black")
#points(cell3_data[problem,1]* step, cell3_data[problem,2],col="green")

print(list1[oo])

#install.packages("fpp2",repos="https://cran.r-project.org")
#library("tsoutliers")
#dat.ts<- ts(cell3_data[1,],frequency=1)
#data.ts.outliers <- tso(dat.ts)
#data.ts.outliers
#plot(data.ts.outliers)

print(cell1_data)
print(divisionzone)

divisioncellsize<-c(divisioncellsize ,mean(cell1[which(cell1[,8]<0),5]))
divisionzonelist<-c(divisionzonelist, divisionzone)
if(length(cell1_data)>3){
points(cell1_data[,1]* step, cell1_data[,2],col="black")

matrix1[match(c(cell1_data[,1]* step), step*c(-100:300)),oo]<-c( cell1_data[,2])
matrix1[1:(min(match(c(cell1_data[,1]* step), step*c(-100:300)))-5),oo]<-mean(cell1[which(cell1[,8]<0),5])

}else{
	matrix1[1:100,oo]<-mean(cell1[which(cell1[,8]<0),5])
	}
matrix3[match(c(cell3_data[,1]* step),step*c(-30:300)),oo]<-c( cell3_data[,2])
matrix8[match(c(cell8_data[,1]* step), step*c(-30:300)),oo]<-c( cell8_data[,2])


}
dev.off()
divisionzonelist/divisioncellsize
matrix<-rbind(matrix1,matrix3, matrix8 )

matrix<-cbind(c(step*c(-100:300),step*c(-30:300),step*c(-30:300)), matrix)
colnames(matrix)<-c("location", list2)
matrix[which(matrix==0)]<-NA
write.csv(matrix,"~/Downloads/matrixbd21.csv", row.names=FALSE)






list2<-c("Bd21-1-control","Bd21-4-control","Bd21-5-control","Bd21-1-drought","Bd21-3-drought-redo","Bd21-4-drought","Bd21-5-drought","bd21-3-drought","bd21-e-drought","bd21-3-drought-black","bd21-3-control","bd212e-drought","bd21-2-control")[-c(10,12,13)]


  pdf(paste0("~/Downloads/bd21_8.pdf"), width=10, height = 20)
par(mfrow = c(6, 2))

parameters <-read.csv(paste0("~/Downloads/matrixbd21_out.csv"), header=T)
name <-read.csv(paste0("~/Downloads/matrixbd21.csv"), header=T)

  colnames(name)<-c("location",list2)
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
res_max<-0 
 return(-res_max)
} 




a="control"
aa="drought"
bb=4
listname<-c("Bd21","bd21")

max<-max(name[,which(substr(colnames(name),1,bb)%in% listname)], na.rm=T)
xmax<-max(parameters[,2], na.rm=T)*7
ymatrix1<-ymatrix2<-NULL
for(i in which(substr(colnames(name),bb+4,bb+10)==a)){
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
x1=Find_Max_CCF(y0,y1)+x
print(Find_Max_CCF(y0,y1))
  	plot( x1, y1, type="l", col="#66CC66",ylim=c(0,400),xlim=c(0,50000) ,xlab="distance(um)", ylab="Cell length (um)" )
points(name[,1],name[,i+1])

ymatrix1<-c(ymatrix1,y1)
}
for(i in which(substr(colnames(name),bb+4,bb+10)==aa)){
	
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
x1=Find_Max_CCF(y0,y1)+x
print(Find_Max_CCF(y0,y1))

  	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,400),xlim=c(0,50000),xlab="distance(um)", ylab="Cell length (um)" )
points(name[,1],name[,i+1])

ymatrix2<-c(ymatrix2,y1)
}

ss<-0
ymatrix1<-ymatrix2<-NULL
sslist<-NULL
for(i in which(substr(colnames(name),bb+4,bb+10)==a)){
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
x1=Find_Max_CCF(y0,y1)+x
print(Find_Max_CCF(y0,y1))
  	plot( x1, y1, type="l", col="#66CC66",ylim=c(0,400),xlim=c(0,xmax) ,xlab="distance(um)", ylab="Cell length (um)" )
par(new=TRUE)	
ymatrix1<-c(ymatrix1,y1)
ss=ss+1
sslist<-c(sslist,rep(ss,length(y1)))

}
for(i in which(substr(colnames(name),bb+4,bb+10)==aa)){
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
x1=Find_Max_CCF(y0,y1)+x
print(Find_Max_CCF(y0,y1))

  	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,400),xlim=c(0,xmax),xlab="distance(um)", ylab="Cell length (um)" )
par(new=TRUE)	
ymatrix2<-c(ymatrix2,y1)
ss=ss+1
sslist<-c(sslist,rep(ss,length(y1)))
}
 	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,400),xlim=c(0,xmax),xlab="distance(um)", ylab="Cell length (um)" )


library(ggplot2)

all_data_bd21_plot<-cbind(rep(x,length(which(substr(colnames(name),1,bb)%in% listname))),c(ymatrix1, ymatrix2),sslist,c(rep("Control",length(ymatrix1)),rep("Drought",length(ymatrix2))))

all_data_bd21_plot<-all_data_bd21_plot[which(all_data_bd21_plot[,1]>0),]



colnames(all_data_bd21_plot)<-c("location","size","sample","treatment")
write.csv(as.matrix(all_data_bd21_plot),"~/Downloads/tmp.csv")
all_data_bd21_plot <-read.csv(paste0("~/Downloads/tmp.csv"), header=T,row.names=1)
library(ggplot2)
library(reshape2)

all_data_bd21_plot$treatment<-as.factor(all_data_bd21_plot$treatment)
aaaaaa<-ggplot(all_data_bd21_plot,aes(location,size))+
  theme_classic()+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = treatment))+
  labs(x = expression(paste("Distance (", mu, "m)")),y = expression(paste("Cell length (", mu, "m)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",size = 1,aes(fill = treatment),alpha = 0.3)+
  guides(fill = "none")+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = treatment))+
  labs(x = expression(paste("Distance (", mu, "m)")),y = expression(paste("Cell length (", mu, "m)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  geom_line(aes(group = interaction(sample, treatment),colour = treatment,alpha = 0.2))+
  guides(alpha= "none") + scale_color_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+scale_fill_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+ ggtitle(
listname[1])
# aaaaaa
  dev.off()
  
list4<-substr(list2,bb+4,bb+10)

write.csv(cbind(list2,rep(listname[1],length(list2)),list4,divisionzonelist, divisioncellsize, parameters),"~/Downloads/matrixbd21_out2.csv")






elongationrate_c<-0.5232780
elongationrate_d<-0.2190562

list2<-c("Bd21-1-control","Bd21-4-control","Bd21-5-control","Bd21-1-drought","Bd21-3-drought-redo","Bd21-4-drought","Bd21-5-drought","bd21-3-drought","bd21-e-drought","bd21-3-drought-black","bd21-3-control","bd212e-drought","bd21-2-control")[-c(10,12,13)]


  pdf(paste0("~/Downloads/bd21_9.pdf"), width=10, height = 20)
par(mfrow = c(6, 2))

parameters <-read.csv(paste0("~/Downloads/matrixbd21_out.csv"), header=T)
name <-read.csv(paste0("~/Downloads/matrixbd21.csv"), header=T)

  colnames(name)<-c("location",list2)
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
res_max<-0 
 return(-res_max)
} 

a="control"
aa="drought"
bb=4
listname<-c("Bd21","bd21")

max<-max(name[,which(substr(colnames(name),1,bb)%in% listname)], na.rm=T)
xmax<-50000
ymatrix1<-ymatrix2<-xmatrix1<-xmatrix2<-NULL
ss<-0
ymatrix1<-ymatrix2<-NULL
sslist<-NULL
for(i in which(substr(colnames(name),bb+4,bb+10)==a)){
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
print(Find_Max_CCF(y0,y1))
  	plot( x1, y1, type="l", col="#66CC66",ylim=c(0,400),xlim=c(0,xmax) ,xlab="distance(um)", ylab="Cell length (um)" )
#par(new=TRUE)	
ymatrix1<-c(ymatrix1,y1)
ss=ss+1
sslist<-c(sslist,rep(ss,length(y1)))

}
for(i in which(substr(colnames(name),bb+4,bb+10)==aa)){
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
  y1 = L / (1 + exp(-k*(x-x0))) + b
x1=x
rootlist<-NULL
for (ismall in 1:length(x1)){
g <- Vectorize(function(x) parameters[i,5]/elongationrate_d*(integrate(function(t) 1/( L / (1 + exp(-k*(t-x0))) + b), lower = 0, upper = x)$value)-x1[ismall])
root <- uniroot(g,c(-10000,200000))$root
rootlist<-c(rootlist,root)
}

  y1 = L / (1 + exp(-k*(rootlist-x0))) + b

xmatrix2<-c(xmatrix2,x1)
print(Find_Max_CCF(y0,y1))
  	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,400),xlim=c(0,xmax) ,xlab="distance(um)", ylab="Cell length (um)" )
#par(new=TRUE)	
ymatrix2<-c(ymatrix2,y1)
ss=ss+1
sslist<-c(sslist,rep(ss,length(y1)))
}
 #plot( x1, y1, type="l", col="#FF6600",ylim=c(0,400),xlim=c(0,xmax),xlab="distance(um)", ylab="Cell length (um)" )


library(ggplot2)

all_data_bd21_plot<-cbind(c(xmatrix1, xmatrix2)/1000,c(ymatrix1, ymatrix2),sslist,c(rep("Control",length(ymatrix1)),rep("Drought",length(ymatrix2))))

all_data_bd21_plot<-all_data_bd21_plot[which(all_data_bd21_plot[,1]>0),]



colnames(all_data_bd21_plot)<-c("location","size","sample","treatment")
write.csv(as.matrix(all_data_bd21_plot),"~/Downloads/tmp.csv")
all_data_bd21_plot <-read.csv(paste0("~/Downloads/tmp.csv"), header=T,row.names=1)
library(ggplot2)
library(reshape2)

all_data_bd21_plot$treatment<-as.factor(all_data_bd21_plot$treatment)
aaaaaa2<-ggplot(all_data_bd21_plot,aes(location,size))+
  theme_classic()+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = treatment))+
  labs(x = "Thermal time (°Cd)",y = expression(paste("Cell length (", mu, "m)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",size = 1,aes(fill = treatment),alpha = 0.3)+
  guides(fill = "none")+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = treatment))+
  labs(x = "Thermal time (°Cd)",y = expression(paste("Cell length (", mu, "m)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  geom_line(aes(group = interaction(sample, treatment),colour = treatment,alpha = 0.2))+
  guides(alpha= "none") + scale_color_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+scale_fill_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+ ggtitle(
listname[1])
# aaaaaa
  dev.off()
  
list4<-substr(list2,bb+4,bb+10)



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

  pdf(paste0("~/Downloads/oat_7_2.pdf"), width=10, height = 20)
par(mfrow = c(6, 2))
for(oo in c(1:length(list1))){

numbercut3=0
step<-500
numbercut1=5
numbercut2=20
location <-read.csv(paste0("/Volumes/TOSHIBAEXT/images/leaf\ extension\ 2/",list1[oo],"/stitching_result.csv"), header=T)
head(location)
head(location)
location[,3]<-location[,3]/2.7
location[,4]<-location[,4]/2.7
direction <-read.csv(paste0("/Volumes/TOSHIBAEXT/images/leaf\ extension\ 2/",list1[oo],"/dirs.csv"), header=T)
head(direction)
location[,3]<-location[,3]/cos(direction[match(substr(location[,1],1,namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2]) 
emptylength<-empty[oo]/2.7/cos(direction[1,2])


cell00 <-read.csv(paste0("/Volumes/TOSHIBAEXT/oat_redo/filtered_young_oat/",list2[oo],"/cells.csv"), header=T)
head(cell00)
cell00[,3]<-cell00[,3]/2.7
cell00[,4]<-cell00[,4]/2.7
cell00[,5]<-rowMaxs(cbind(cell00[,5]/2.7, cell00[,6]/2.7))
#cell00<-cell00[which(substr(cell00[,1],1,43)%in%c(substr(location[1:10,1],1,43))),]

##trichomes  /Volumes/TOSHIBAEXT/image_results/trichome_results/Bd21-4-control/pairs.csv 

cell3 <-read.csv(paste0("/Volumes/TOSHIBAEXT/image_results/trichome_results/",list2[oo],"/pairs.csv"), header=T)
head(cell3)
cell3[,3]<-cell3[,3]/2.7
cell3[,4]<-cell3[,4]/2.7
cell3[,5]<-cell3[,5]/2.7
cell3[,6]<-cell3[,6]/2.7
cell3[,7]<-cell3[,7]/2.7



cell2 <-read.csv(paste0("/Volumes/TOSHIBAEXT/oat_redo/filtered_old_oat/",list2[oo],"/cells.csv"), header=T)
head(cell2,50)
cell2[,3]<-cell2[,3]/2.7*2
cell2[,4]<-cell2[,4]/2.7
cell2[,5]<-((cell2[,5]/2.7*sin(cell2[,7]/180*pi))^2+(cell2[,5]/2.7*2*cos(cell2[,7]/180*pi))^2)^0.5
cell2[,6]<-((cell2[,6]/2.7*2*sin(cell2[,7]/180*pi))^2+(cell2[,6]/2.7*cos(cell2[,7]/180*pi))^2)^0.5
head(cell2,50)
cell2[,8]<-rowMaxs(cbind(cell2[,5], cell2[,6]))
cell2[,5]<-cell2[,8]
#cell2 <-cell2[-which(substr(cell2[,1],1,43)%in%c(substr(location[1:13,1],1,43))),]

cell4 <-read.csv(paste0("~/Desktop/divisionzonemark/",list2[oo],"/cells.csv"), header=T)
head(cell4)
cell4[,3]<-cell4[,3]/2.7
cell4[,4]<-cell4[,4]/2.7
##base cells /Volumes/TOSHIBAEXT/image_results/newbase_results/Bd21-4-control/cells.csv
cell0 <-read.csv(paste0("/Volumes/TOSHIBAEXT/image_results/basenew/",list2[oo],"/cells.csv"), header=T)
head(cell0)
cell0[,3]<-cell0[,3]/2.7
cell0[,4]<-cell0[,4]/2.7
cell0[,8]<-rowMins(cbind(cell0[,5], cell0[,6]))
cell0[,5]<-cell0[,8]


##plot with three colors
real_location<-rep(0,dim(location)[1])
real_location[1]<-location$X.offset[1]
for (i in 2:dim(location)[1]){
	real_location[i]<-sum(location$X.offset[1:i])
}
real_location <-real_location[match(substr(cell4$Name,1,43),substr(location[,2],1,43))]
real_location[which(substr(cell4$Name,1,43)==substr(location[1,1],1,43))] <-0
if(orientation[oo]=="right"){
	#going right 
cell4[,8] <-c(cell4[,3]/cos(direction[match(substr(cell4[,1],1,43),substr(direction[,1],1,43) ),2])+ real_location)-emptylength
	}else{
#going left
	cell4[,8] <- c((3826/2.7-cell4[,3])/cos(direction[match(substr(cell4[,1],1,43),substr(direction[,1],1,43) ),2])- real_location)-emptylength
	}

divisionzone<-min(cell4[,8])


##plot with three colors
real_location<-rep(0,dim(location)[1])
real_location[1]<-location$X.offset[1]
for (i in 2:dim(location)[1]){
	real_location[i]<-sum(location$X.offset[1:i])
	
}
real_location <-real_location[match(substr(cell3$Name,1,43),substr(location[,2],1,43))]
real_location[which(substr(cell3$Name,1,43)==substr(location[1,1],1,43))] <-0
if(orientation[oo]=="right"){
	#going right 
cell3[,8] <-c(cell3[,3]/cos(direction[match(substr(cell3[,1],1,43),substr(direction[,1],1,43) ),2])+ real_location)-emptylength-divisionzone
	}else{
#going left
	cell3[,8] <- c((3826/2.7-cell3[,3])/cos(direction[match(substr(cell3[,1],1,43),substr(direction[,1],1,43) ),2])- real_location)-emptylength-divisionzone
	}


real_location<-rep(0,dim(location)[1])
real_location[1]<-location$X.offset[1]
for (i in 2:dim(location)[1]){
		real_location[i]<-sum((location$X.offset[1:i]))
}
real_location <-real_location[match(substr(cell2$Name,1,list4[oo]),substr(location[,2],1,list4[oo]))]
real_location[which(substr(cell2$Name,1,list4[oo])==substr(location[1,1],1,list4[oo]))] <-0
if(orientation[oo]=="right"){
	#going right 
cell2[,8] <-c(cell2[,3]/cos(direction[match(substr(cell2[,1],1,namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2])+ real_location)-emptylength-divisionzone
	}else{
#going left
	cell2[,8] <- c((3826/2.7-cell2[,3])/cos(direction[match(substr(cell2[,1],1,namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2])- real_location)-emptylength-divisionzone
	}
	
	##plot with three colors
real_location<-rep(0,dim(location)[1])
real_location[1]<-location$X.offset[1]
for (i in 2:dim(location)[1]){
		real_location[i]<-sum((location$X.offset[1:i]))
}
real_location <-real_location[match(substr(cell0$Name,1,list4[oo]),substr(location[,2],1,list4[oo]))]
real_location[which(substr(cell0$Name,1,list4[oo])==substr(location[1,1],1,list4[oo]))] <-0
if(orientation[oo]=="right"){
	#going right 
cell0[,8] <-c(cell0[,3]/cos(direction[match(substr(cell0[,1],1,namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2])+ real_location)-emptylength-divisionzone
	}else{
#going left
	cell0[,8] <- c((3826/2.7-cell0[,3])/cos(direction[match(substr(cell0[,1],1,namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2])- real_location)-emptylength-divisionzone
	}

	##plot with three colors
real_location<-rep(0,dim(location)[1])
real_location[1]<-location$X.offset[1]
for (i in 2:dim(location)[1]){
		real_location[i]<-sum((location$X.offset[1:i]))
}
real_location <-real_location[match(substr(cell00$Name,1,list4[oo]),substr(location[,2],1,list4[oo]))]
real_location[which(substr(cell00$Name,1,list4[oo])==substr(location[1,1],1,list4[oo]))] <-0
if(orientation[oo]=="right"){
	#going right 
cell00[,8] <-c(cell00[,3]/cos(direction[match(substr(cell00[,1],1,namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2])+ real_location)-emptylength-divisionzone
	}else{
#going left
	cell00[,8] <- c((3826/2.7-cell00[,3])/cos(direction[match(substr(cell00[,1],1,namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2])- real_location)-emptylength-divisionzone
	}
	

cell0<-cell0[which(cell0[,8]<0),]


cell0<-na.omit(cell0)
cell00<-na.omit(cell00)
#write.csv(cell3,"~/Downloads/test/pairs_trichome.csv")
cell3<-na.omit(cell3)
cell2<-na.omit(cell2)
cell00<-cell00[which((cell00[,8]<4000)),]
cell2<-cell2[which((cell2[,8]>0)&(cell2[,8]>3000)),]


window<-(cell3[,8]) %/% step
df <-as.data.frame(cbind(cell3$Distance, window))
cell3_data<-as.matrix(df %>% group_by(window) %>%
  summarize(res=quantile(V1,probs=0.1),number=length(V1)))
cell3_data<-cell3_data[which(cell3_data[,3]> 3),]


window<-(cell0[,8]) %/% step
df <-as.data.frame(cbind(cell0$Width, window))
cell0_data<-as.matrix(df %>% group_by(window) %>%
  summarize(res=quantile(V1,probs=0.5),number=length(V1)))
cell0_data<-cell0_data[which(cell0_data[,3]> numbercut1),]

window<-(cell00[,8]) %/% step
df <-as.data.frame(cbind(cell00$Width, window))
cell00_data<-as.matrix(df %>% group_by(window) %>%
  summarize(res=quantile(V1,probs=0.7),number=length(V1)))
cell00_data<-cell00_data[which(cell00_data[,3]> 3),]



window<-(cell2[,8]) %/% step
df <-as.data.frame(cbind(cell2$Width, window))
cell2_data<-as.matrix(df %>% group_by(window) %>%
  summarize(res=quantile(V1,probs=0.7),number=length(V1)))
cell2_data<-cell2_data[which(cell2_data[,3]>1),]



plot(cell2_data[,1]* step, cell2_data[,2],col="#FF6600",ylim=c(0,400),xlim=c(-5000,100000))
  #old sister 3better
#new base 
#sister middle 
#points(cell1_data[,1]* step, cell1_data[,2],col="grey")
points(cell00_data[,1]* step, cell00_data[,2],col="purple")
#sister middle 

if(list2[oo]%in%list5){
cell8 <-read.csv(paste0("~/Desktop/handmark/",list2[oo],"/pairs.csv"), header=T)
head(cell8)
cell8[,3]<-cell8[,3]/2.7
cell8[,4]<-cell8[,4]/2.7
cell8[,5]<-cell8[,5]/2.7
cell8[,6]<-cell8[,6]/2.7
cell8[,7]<-cell8[,7]/2.7

real_location<-rep(0,dim(location)[1])
real_location[1]<-location$X.offset[1]
for (i in 2:dim(location)[1]){
	real_location[i]<-sum(location$X.offset[1:i])
	
}
real_location <-real_location[match(substr(cell8$Name,1,43),substr(location[,2],1,43))]
real_location[which(substr(cell8$Name,1,43)==substr(location[1,1],1,43))] <-0
if(orientation[oo]=="right"){
	#going right 
cell8[,8] <-c(cell8[,3]/cos(direction[match(substr(cell8[,1],1,43),substr(direction[,1],1,43) ),2])+ real_location)-emptylength-divisionzone
	}else{
#going left
	cell8[,8] <- c((3826/2.7-cell8[,3])/cos(direction[match(substr(cell8[,1],1,43),substr(direction[,1],1,43) ),2])- real_location)-emptylength-divisionzone
	}
#write.csv(cell3,"~/Downloads/test/pairs_trichome.csv")


cell8<-na.omit(cell8)
window<-(cell8[,8]) %/% step
df <-as.data.frame(cbind(cell8$Distance, window))
cell8_data<-as.matrix(df %>% group_by(window) %>%
  summarize(res=quantile(V1,probs=0.2),number=length(V1)))
cell8_data<-cell8_data[which(cell8_data[,3]> 0),]

points(cell8_data[,1]* step, cell8_data[,2],col="green")
}else{
	cell8_data<-cell0_data[1:2,]
	}


if(length(cell0_data)>3){
points(cell0_data[,1]* step, cell0_data[,2],col="yellow")

matrix1[match(c(cell0_data[,1]* step), step*c(-100:200)),oo]<-c( cell0_data[,2])
matrix1[1:(min(match(c(cell0_data[,1]* step), step*c(-100:200)))-5),oo]<-mean(cell0[which(cell0[,8]<0),5])

}else{
	matrix1[1:100,oo]<-mean(cell0[which(cell0[,8]<0),5])
	}

divisioncellsize<-c(divisioncellsize ,mean(cell0[which(cell0[,8]<0),5]))
divisionzonelist<-c(divisionzonelist, divisionzone)
matrix3[match(c(cell2_data[,1]* step),step*c(-30:200)),oo]<-c( cell2_data[,2])
matrix4[match(c(cell00_data[,1]* step), step*c(-30:200)),oo]<-c( cell00_data[,2])
matrix2[match(c(cell8_data[,1]* step), step*c(-30:200)),oo]<-c( cell8_data[,2])



testdata<-cell2_data
testdataoriginal<-cell2
for(i in 1:(dim(testdata)[1]-1)){
if((testdata[i+1,1]-testdata[i,1])>3){
	print(c(testdata[i+1,1]* step,testdata[i,1]*step, step*(testdata[i+1,1]-testdata[i,1])))
	print(plyr::count(testdataoriginal[which((testdataoriginal[,8]<testdata[i+1,1]*step)&(testdataoriginal[,8]>testdata[i,1]*step)),1]))
}
}
print(list1[oo])
distance<-problem<-NULL
for(i in 1:(dim(testdata)[1]-1)){
distance<-c(distance,testdata[i+1,2]-testdata[i,2])
}
	
for(i in 1:(dim(testdata)[1]-1)){
if((testdata[i+1,2]-testdata[i,2])%in%boxplot.stats(distance)$out){
	print(c(i, testdata[i+1,1]* step,testdata[i,1]*step, step*(testdata[i+1,1]-testdata[i,1])))
	print(plyr::count(testdataoriginal[which((testdataoriginal[,8]<testdata[i+1,1]*step)&(testdataoriginal[,8]>testdata[i,1]*step)),1]))
	problem<-c(problem,i)

}
}



}
dev.off()
divisionzonelist/divisioncellsize
matrix<-rbind(matrix1,matrix3, matrix4, matrix2)


matrix<-cbind(c(step*c(-100:200),step*c(-30:200),step*c(-30:200),step*c(-30:200)), matrix)
colnames(matrix)<-c("location", list2)
matrix[which(matrix==0)]<-NA
write.csv(matrix,"~/Downloads/matrixoat_2.csv", row.names=FALSE)



list2<-c("Oat-D-1-control",  "Oat-D-1-drought" , "Oat-D-2-control"  ,"Oat-D-2-drought" ,"Oat-D-3-drought"  ,  "Oat-D-4-control" , "Oat-D-4-drought" , "Oat-D-5-control" , "Oat-D-5-drought" ,"Oat-De3-control"  ,"Oat-W-1-drought" , "Oat-W-2-control", "Oat-W-2-drought" , "Oat-W-3-control","Oat-W-3-drought","Oat-W-4-control" ,"Oat-W-4-drought" , "Oat-W-5-control", "Oat-W-5-drought")[-10]

  pdf(paste0("~/Downloads/oat_8.pdf"), width=10, height = 20)
par(mfrow = c(6, 2))
parameters <-read.csv(paste0("~/Downloads/matrixoat_2_out.csv"), header=T)
name <-read.csv(paste0("~/Downloads/matrixoat_2.csv"), header=T)

  colnames(name)<-c("location",list2)
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
res_max<-0 
 return(-res_max)
} 

a="control"
aa="drought"
bb=5
listname<-c("Oat-W","Oat-W")
list4<-substr(list2,bb+4,bb+10)

 
write.csv(cbind(list2,substr(list2,1,5),substr(list2,5+4,5+10),divisionzonelist, divisioncellsize, parameters),"~/Downloads/matrixoat_2_out2.csv")



max<-max(name[,which(substr(colnames(name),1,bb)%in% listname)], na.rm=T)
xmax<-max(parameters[,2], na.rm=T)*7
ymatrix1<-ymatrix2<-NULL
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

  	plot( x1, y1, type="l", col="#66CC66",ylim=c(0,400),xlim=c(0,50000) ,xlab="distance(um)", ylab="Cell length (um)" )
points(name[,1],name[,i+1])

ymatrix1<-c(ymatrix1,y1)
}
for(i in which((substr(colnames(name),bb+4,bb+10)==aa)&(substr(colnames(name),1,bb)%in% listname))){

	
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


  	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,400),xlim=c(0,50000),xlab="distance(um)", ylab="Cell length (um)" )
points(name[,1],name[,i+1])

ymatrix2<-c(ymatrix2,y1)
}

ss<-0
ymatrix1<-ymatrix2<-NULL
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

  	plot( x1, y1, type="l", col="#66CC66",ylim=c(0,400),xlim=c(0,xmax) ,xlab="distance(um)", ylab="Cell length (um)" )
par(new=TRUE)	
ymatrix1<-c(ymatrix1,y1)
ss=ss+1
sslist<-c(sslist,rep(ss,length(y1)))

}
for(i in which((substr(colnames(name),bb+4,bb+10)==aa)&(substr(colnames(name),1,bb)%in% listname))){

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


  	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,400),xlim=c(0,xmax),xlab="distance(um)", ylab="Cell length (um)" )
par(new=TRUE)	
ymatrix2<-c(ymatrix2,y1)
ss=ss+1
sslist<-c(sslist,rep(ss,length(y1)))
}
 	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,400),xlim=c(0,xmax),xlab="distance(um)", ylab="Cell length (um)" )


library(ggplot2)

all_data_bd21_plot<-cbind(rep(x,length(which(substr(colnames(name),1,bb)%in% listname))),c(ymatrix1, ymatrix2),sslist,c(rep("Control",length(ymatrix1)),rep("Drought",length(ymatrix2))))

all_data_bd21_plot<-all_data_bd21_plot[which(all_data_bd21_plot[,1]>0),]



colnames(all_data_bd21_plot)<-c("location","size","sample","treatment")
write.csv(as.matrix(all_data_bd21_plot),"~/Downloads/tmp.csv")
all_data_bd21_plot <-read.csv(paste0("~/Downloads/tmp.csv"), header=T,row.names=1)
library(ggplot2)
library(reshape2)

all_data_bd21_plot$treatment<-as.factor(all_data_bd21_plot$treatment)
dddddd<-ggplot(all_data_bd21_plot,aes(location,size))+
  theme_classic()+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = treatment))+
  labs(x = expression(paste("Distance (", mu, "m)")),y = expression(paste("Cell length (", mu, "m)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",size = 1,aes(fill = treatment),alpha = 0.3)+
  guides(fill = "none")+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = treatment))+
  labs(x = expression(paste("Distance (", mu, "m)")),y = expression(paste("Cell length (", mu, "m)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  geom_line(aes(group = interaction(sample, treatment),colour = treatment,alpha = 0.2))+
  guides(alpha= "none") + scale_color_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+scale_fill_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+ ggtitle(
"Oat-Wild")
#dddddd
 
a="control"
aa="drought"
bb=5
listname<-c("Oat-D","Oat-D")
list4<-substr(list2,bb+4,bb+10)



max<-max(name[,which(substr(colnames(name),1,bb)%in% listname)], na.rm=T)
xmax<-max(parameters[,2], na.rm=T)*7
ymatrix1<-ymatrix2<-NULL
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

  	plot( x1, y1, type="l", col="#66CC66",ylim=c(0,400),xlim=c(0,50000) ,xlab="distance(um)", ylab="Cell length (um)" )
points(name[,1],name[,i+1])

ymatrix1<-c(ymatrix1,y1)
}
for(i in which((substr(colnames(name),bb+4,bb+10)==aa)&(substr(colnames(name),1,bb)%in% listname))){

	
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


  	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,400),xlim=c(0,50000),xlab="distance(um)", ylab="Cell length (um)" )
points(name[,1],name[,i+1])

ymatrix2<-c(ymatrix2,y1)
}

ss<-0
ymatrix1<-ymatrix2<-NULL
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

  	plot( x1, y1, type="l", col="#66CC66",ylim=c(0,400),xlim=c(0,xmax) ,xlab="distance(um)", ylab="Cell length (um)" )
par(new=TRUE)	
ymatrix1<-c(ymatrix1,y1)
ss=ss+1
sslist<-c(sslist,rep(ss,length(y1)))

}
for(i in which((substr(colnames(name),bb+4,bb+10)==aa)&(substr(colnames(name),1,bb)%in% listname))){

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


  	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,400),xlim=c(0,xmax),xlab="distance(um)", ylab="Cell length (um)" )
par(new=TRUE)	
ymatrix2<-c(ymatrix2,y1)
ss=ss+1
sslist<-c(sslist,rep(ss,length(y1)))
}
 	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,400),xlim=c(0,xmax),xlab="distance(um)", ylab="Cell length (um)" )


library(ggplot2)

all_data_bd21_plot<-cbind(rep(x,length(which(substr(colnames(name),1,bb)%in% listname))),c(ymatrix1, ymatrix2),sslist,c(rep("Control",length(ymatrix1)),rep("Drought",length(ymatrix2))))

all_data_bd21_plot<-all_data_bd21_plot[which(all_data_bd21_plot[,1]>0),]



colnames(all_data_bd21_plot)<-c("location","size","sample","treatment")
write.csv(as.matrix(all_data_bd21_plot),"~/Downloads/tmp.csv")
all_data_bd21_plot <-read.csv(paste0("~/Downloads/tmp.csv"), header=T,row.names=1)
library(ggplot2)
library(reshape2)

all_data_bd21_plot$treatment<-as.factor(all_data_bd21_plot$treatment)
eeeeee <-ggplot(all_data_bd21_plot,aes(location,size))+
  theme_classic()+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = treatment))+
  labs(x = expression(paste("Distance (", mu, "m)")),y = expression(paste("Cell length (", mu, "m)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",size = 1,aes(fill = treatment),alpha = 0.3)+
  guides(fill = "none")+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = treatment))+
  labs(x = expression(paste("Distance (", mu, "m)")),y = expression(paste("Cell length (", mu, "m)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  geom_line(aes(group = interaction(sample, treatment),colour = treatment,alpha = 0.2))+
  guides(alpha= "none")+ scale_color_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+scale_fill_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+ ggtitle(
"Oat-Domesticated")
# eeeeee

  dev.off()
  




list2<-c("Oat-D-1-control",  "Oat-D-1-drought" , "Oat-D-2-control"  ,"Oat-D-2-drought" ,"Oat-D-3-drought"  ,  "Oat-D-4-control" , "Oat-D-4-drought" , "Oat-D-5-control" , "Oat-D-5-drought" ,"Oat-De3-control"  ,"Oat-W-1-drought" , "Oat-W-2-control", "Oat-W-2-drought" , "Oat-W-3-control","Oat-W-3-drought","Oat-W-4-control" ,"Oat-W-4-drought" , "Oat-W-5-control", "Oat-W-5-drought")[-10]

  pdf(paste0("~/Downloads/oat_9.pdf"), width=10, height = 20)
par(mfrow = c(6, 2))
parameters <-read.csv(paste0("~/Downloads/matrixoat_2_out.csv"), header=T)
name <-read.csv(paste0("~/Downloads/matrixoat_2.csv"), header=T)

  colnames(name)<-c("location",list2)
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
res_max<-0 
 return(-res_max)
} 
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

print(Find_Max_CCF(y0,y1))
  	plot( x1, y1, type="l", col="#66CC66",ylim=c(0,400),xlim=c(0,xmax) ,xlab="distance(um)", ylab="Cell length (um)" )
par(new=TRUE)	
ymatrix1<-c(ymatrix1,y1)
ss=ss+1
sslist<-c(sslist,rep(ss,length(y1)))

}
for(i in which((substr(colnames(name),bb+4,bb+10)==aa)&(substr(colnames(name),1,bb)%in% listname))){

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
  y1 = L / (1 + exp(-k*(x-x0))) + b
x1=x
rootlist<-NULL
for (ismall in 1:length(x1)){
g <- Vectorize(function(x) parameters[i,5]/elongationrate_d*(integrate(function(t) 1/( L / (1 + exp(-k*(t-x0))) + b), lower = 0, upper = x)$value)-x1[ismall])
root <- uniroot(g,c(-10000,200000))$root
rootlist<-c(rootlist,root)
}

  y1 = L / (1 + exp(-k*(rootlist-x0))) + b

xmatrix2<-c(xmatrix2,x1)

  	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,400),xlim=c(0,xmax),xlab="distance(um)", ylab="Cell length (um)" )
par(new=TRUE)	
ymatrix2<-c(ymatrix2,y1)
ss=ss+1
sslist<-c(sslist,rep(ss,length(y1)))
}
 	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,400),xlim=c(0,xmax),xlab="distance(um)", ylab="Cell length (um)" )


library(ggplot2)

all_data_bd21_plot<-cbind(c(xmatrix1, xmatrix2)/1000,c(ymatrix1, ymatrix2),sslist,c(rep("Control",length(ymatrix1)),rep("Drought",length(ymatrix2))))

all_data_bd21_plot<-all_data_bd21_plot[which(all_data_bd21_plot[,1]>0),]



colnames(all_data_bd21_plot)<-c("location","size","sample","treatment")
write.csv(as.matrix(all_data_bd21_plot),"~/Downloads/tmp.csv")
all_data_bd21_plot <-read.csv(paste0("~/Downloads/tmp.csv"), header=T,row.names=1)
library(ggplot2)
library(reshape2)

all_data_bd21_plot$treatment<-as.factor(all_data_bd21_plot$treatment)
dddddd2<-ggplot(all_data_bd21_plot,aes(location,size))+
  theme_classic()+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = treatment))+
  labs(x = "Thermal time (°Cd)",y = expression(paste("Cell length (", mu, "m)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",size = 1,aes(fill = treatment),alpha = 0.3)+
  guides(fill = "none")+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = treatment))+
  labs(x = "Thermal time (°Cd)",y = expression(paste("Cell length (", mu, "m)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  geom_line(aes(group = interaction(sample, treatment),colour = treatment,alpha = 0.2))+
  guides(alpha= "none")+
   scale_color_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+scale_fill_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+ ggtitle("Oat-Wild")
 #bbbbbb
 
a="control"
aa="drought"
bb=5
listname<-c("Oat-D","Oat-D")
list4<-substr(list2,bb+4,bb+10)

elongationrate_c<-1.2427181
elongationrate_d<-0.7445045


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
print(Find_Max_CCF(y0,y1))
  	plot( x1, y1, type="l", col="#66CC66",ylim=c(0,400),xlim=c(0,xmax) ,xlab="distance(um)", ylab="Cell length (um)" )
par(new=TRUE)	
ymatrix1<-c(ymatrix1,y1)
ss=ss+1
sslist<-c(sslist,rep(ss,length(y1)))

}
for(i in which((substr(colnames(name),bb+4,bb+10)==aa)&(substr(colnames(name),1,bb)%in% listname))){

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
  y1 = L / (1 + exp(-k*(x-x0))) + b
x1=x
rootlist<-NULL
for (ismall in 1:length(x1)){
g <- Vectorize(function(x) parameters[i,5]/elongationrate_d*(integrate(function(t) 1/( L / (1 + exp(-k*(t-x0))) + b), lower = 0, upper = x)$value)-x1[ismall])
root <- uniroot(g,c(-10000,200000))$root
rootlist<-c(rootlist,root)
}

  y1 = L / (1 + exp(-k*(rootlist-x0))) + b

xmatrix2<-c(xmatrix2,x1)

  	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,400),xlim=c(0,xmax),xlab="distance(um)", ylab="Cell length (um)" )
par(new=TRUE)	
ymatrix2<-c(ymatrix2,y1)
ss=ss+1
sslist<-c(sslist,rep(ss,length(y1)))
}
 	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,400),xlim=c(0,xmax),xlab="distance(um)", ylab="Cell length (um)" )


library(ggplot2)

all_data_bd21_plot<-cbind(c(xmatrix1, xmatrix2)/1000,c(ymatrix1, ymatrix2),sslist,c(rep("Control",length(ymatrix1)),rep("Drought",length(ymatrix2))))

all_data_bd21_plot<-all_data_bd21_plot[which(all_data_bd21_plot[,1]>0),]



colnames(all_data_bd21_plot)<-c("location","size","sample","treatment")
write.csv(as.matrix(all_data_bd21_plot),"~/Downloads/tmp.csv")
all_data_bd21_plot <-read.csv(paste0("~/Downloads/tmp.csv"), header=T,row.names=1)
library(ggplot2)
library(reshape2)

all_data_bd21_plot$treatment<-as.factor(all_data_bd21_plot$treatment)
eeeeee2 <-ggplot(all_data_bd21_plot,aes(location,size))+
  theme_classic()+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = treatment))+
  labs(x = "Thermal time (°Cd)",y = expression(paste("Cell length (", mu, "m)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",size = 1,aes(fill = treatment),alpha = 0.3)+
  guides(fill = "none")+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = treatment))+
  labs(x = "Thermal time (°Cd)",y = expression(paste("Cell length (", mu, "m)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  geom_line(aes(group = interaction(sample, treatment),colour = treatment,alpha = 0.2))+
  guides(alpha= "none")+ scale_color_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+scale_fill_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+ ggtitle(
"Oat-Domesticated")
 #cccccc

  dev.off()



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

 pdf(paste0("~/Downloads/barley_7.pdf"), width=7, height = 4)

for(oo in c(1:length(list1))){

	cell00<-cell00_data<-cell1<-cell1_data<-cell2<-cell2_data<-	cell3<-cell3_data<-	cell0<-cell0_data<-	cell4<-cell4_data<-	cell8<-cell8_data<-NULL
step<-500
numbercut1=3
numbercut2=10
numbercut3=1
cell00 <-read.csv(paste0("/Volumes/TOSHIBAEXT/image_results/barley_all/",list2[oo],"/cells.csv"), header=T)
head(cell00)
cell00[,3]<-cell00[,3]/2.7
cell00[,4]<-cell00[,4]/2.7
cell00[,5]<-rowMaxs(cbind(cell00[,5]/2.7, cell00[,6]/2.7))
##trichomes  /Volumes/TOSHIBAEXT/image_results/trichome_results/Bd21-4-control/pairs.csv 

##base cells /Volumes/TOSHIBAEXT/image_results/newbase_results/Bd21-4-control/cells.csv
cell1 <-read.csv(paste0("/Volumes/TOSHIBAEXT/image_results/sister_results/",list2[oo],"/cells.csv"), header=T)
head(cell1)
cell1[,3]<-cell1[,3]/2.7
cell1[,4]<-cell1[,4]/2.7
cell1[,5]<-rowMaxs(cbind(cell1[,5]/2.7, cell1[,6]/2.7))
##trichomes  /Volumes/TOSHIBAEXT/image_results/trichome_results/Bd21-4-control/pairs.csv 

cell3 <-read.csv(paste0("/Volumes/TOSHIBAEXT/image_results/trichome_results/",list2[oo],"/pairs.csv"), header=T)
head(cell3)
cell3[,3]<-cell3[,3]/2.7
cell3[,4]<-cell3[,4]/2.7
cell3[,5]<-cell3[,5]/2.7
cell3[,6]<-cell3[,6]/2.7
cell3[,7]<-cell3[,7]/2.7
location <-read.csv(paste0("/Volumes/TOSHIBAEXT/images/leaf\ extension\ 2/",list1[oo],"/stitching_result.csv"), header=T)
dim(location)
head(location)
location[,3]<-location[,3]/2.7
location[,4]<-location[,4]/2.7



direction <-read.csv(paste0("/Volumes/TOSHIBAEXT/images/leaf\ extension\ 2/",list1[oo],"/dirs.csv"), header=T)
dim(direction)
location[,3]<-location[,3]/cos(direction[match(substr(location[,1],1,namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2]) 
emptylength<-empty[oo]/cos(direction[1,2])/2.7


cell2 <-read.csv(paste0("/Volumes/TOSHIBAEXT/image_results/middlenew/",list2[oo],"/cells.csv"), header=T)
head(cell1)
cell2[,3]<-cell2[,3]/2.7
cell2[,4]<-cell2[,4]/2.7
cell2[,5]<-rowMaxs(cbind(cell2[,5]/2.7, cell2[,6]/2.7))


##base cells /Volumes/TOSHIBAEXT/image_results/newbase_results/Bd21-4-control/cells.csv
cell0 <-read.csv(paste0("/Volumes/TOSHIBAEXT/image_results/basenew/",list2[oo],"/cells.csv"), header=T)
head(cell0)
cell0[,3]<-cell0[,3]/2.7
cell0[,4]<-cell0[,4]/2.7
cell0[,5]<-rowMaxs(cbind(cell0[,5]/2.7, cell0[,6]/2.7))

cell4 <-read.csv(paste0("~/Desktop/divisionzonemark/",list2[oo],"/cells.csv"), header=T)
head(cell4)
cell4[,3]<-cell4[,3]/2.7
cell4[,4]<-cell4[,4]/2.7
##plot with three colors
real_location<-rep(0,dim(location)[1])
real_location[1]<-location$X.offset[1]
for (i in 2:dim(location)[1]){
	real_location[i]<-sum(location$X.offset[1:i])
}
real_location <-real_location[match(substr(cell4$Name,1,namelength[oo]),substr(location[,2],1,namelength[oo]))]
real_location[which(substr(cell4$Name,1,namelength[oo])==substr(location[1,1],1,namelength[oo]))] <-0
if(orientation[oo]=="right"){
	#going right 
cell4[,8] <-c(cell4[,3]/cos(direction[match(substr(cell4[,1],1, namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2])+ real_location)-emptylength
	}else{
#going left
	cell4[,8] <- c((3826/2.7-cell4[,3])/cos(direction[match(substr(cell4[,1],1,namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2])- real_location)-emptylength
	}

divisionzone<-min(cell4[,8])


	##plot with three colors
real_location<-rep(0,dim(location)[1])
real_location[1]<-location$X.offset[1]
for (i in 2:dim(location)[1]){
		real_location[i]<-sum((location$X.offset[1:i]))
}
real_location <-real_location[match(substr(cell00$Name,1,list4[oo]),substr(location[,2],1,list4[oo]))]
real_location[which(substr(cell00$Name,1,list4[oo])==substr(location[1,1],1,list4[oo]))] <-0
if(orientation[oo]=="right"){
	#going right 
cell00[,8] <-c(cell00[,3]/cos(direction[match(substr(cell00[,1],1,namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2])+ real_location)-emptylength-divisionzone
	}else{
#going left
	cell00[,8] <- c((3826/2.7-cell00[,3])/cos(direction[match(substr(cell00[,1],1,namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2])- real_location)-emptylength-divisionzone
	}
	##plot with three colors
real_location<-rep(0,dim(location)[1])
real_location[1]<-location$X.offset[1]
for (i in 2:dim(location)[1]){
		real_location[i]<-sum((location$X.offset[1:i]))
}
real_location <-real_location[match(substr(cell1$Name,1,list4[oo]),substr(location[,2],1,list4[oo]))]
real_location[which(substr(cell1$Name,1,list4[oo])==substr(location[1,1],1,list4[oo]))] <-0
if(orientation[oo]=="right"){
	#going right 
cell1[,8] <-c(cell1[,3]/cos(direction[match(substr(cell1[,1],1,namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2])+ real_location)-emptylength-divisionzone
	}else{
#going left
	cell1[,8] <- c((3826/2.7-cell1[,3])/cos(direction[match(substr(cell1[,1],1,namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2])- real_location)-emptylength-divisionzone
	}
	
	
	##plot with three colors
real_location<-rep(0,dim(location)[1])
real_location[1]<-location$X.offset[1]
for (i in 2:dim(location)[1]){
		real_location[i]<-sum((location$X.offset[1:i]))
}
real_location <-real_location[match(substr(cell0$Name,1,list4[oo]),substr(location[,2],1,list4[oo]))]
real_location[which(substr(cell0$Name,1,list4[oo])==substr(location[1,1],1,list4[oo]))] <-0
if(orientation[oo]=="right"){
	#going right 
cell0[,8] <-c(cell0[,3]/cos(direction[match(substr(cell0[,1],1,namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2])+ real_location)-emptylength-divisionzone
	}else{
#going left
	cell0[,8] <- c((3826/2.7-cell0[,3])/cos(direction[match(substr(cell0[,1],1,namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2])- real_location)-emptylength-divisionzone
	}
	
	
	##plot with three colors
real_location<-rep(0,dim(location)[1])
real_location[1]<-location$X.offset[1]
for (i in 2:dim(location)[1]){
		real_location[i]<-sum((location$X.offset[1:i]))
}
real_location <-real_location[match(substr(cell2$Name,1,list4[oo]),substr(location[,2],1,list4[oo]))]
real_location[which(substr(cell2$Name,1,list4[oo])==substr(location[1,1],1,list4[oo]))] <-0
if(orientation[oo]=="right"){
	#going right 
cell2[,8] <-c(cell2[,3]/cos(direction[match(substr(cell2[,1],1,namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2])+ real_location)-emptylength-divisionzone
	}else{
#going left
	cell2[,8] <- c((3826/2.7-cell2[,3])/cos(direction[match(substr(cell2[,1],1,namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2])- real_location)-emptylength-divisionzone
	}

	##plot with three colors
real_location<-rep(0,dim(location)[1])
real_location[1]<-location$X.offset[1]
for (i in 2:dim(location)[1]){
		real_location[i]<-sum((location$X.offset[1:i]))
}
real_location <-real_location[match(substr(cell3$Name,1,list4[oo]),substr(location[,2],1,list4[oo]))]
real_location[which(substr(cell3$Name,1,list4[oo])==substr(location[1,1],1,list4[oo]))] <-0
if(orientation[oo]=="right"){
	#going right 
cell3[,8] <-c(cell3[,3]/cos(direction[match(substr(cell3[,1],1,namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2])+ real_location)-emptylength-divisionzone
	}else{
#going left
	cell3[,8] <- c((3826/2.7-cell3[,3])/cos(direction[match(substr(cell3[,1],1,namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2])- real_location)-emptylength-divisionzone
	}

cell3<-cell3[which(cell3[,8]>0),]
cell0<-cell0[which(cell0[,8]<0),]
#cell00<-cell00[which(cell00[,8]>0),]
cell2<-cell2[which((cell2[,8]<5000)),]
#cell2[,8]>0)&
cell1<-cell1[which((cell1[,8]>0)&(cell1[,8]>10000)),]

cell0<-na.omit(cell0)
cell00<-na.omit(cell00)
#write.csv(cell3,"~/Downloads/test/pairs_trichome.csv")
cell3<-na.omit(cell3)
cell2<-na.omit(cell2)
cell1<-na.omit(cell1)

window<-(cell3[,8]) %/% step
df <-as.data.frame(cbind(cell3$Distance, window))
cell3_data<-as.matrix(df %>% group_by(window) %>%
  summarize(res=quantile(V1,probs=0.1),number=length(V1)))
cell3_data<-cell3_data[which(cell3_data[,3]> numbercut2),]


window<-(cell0[,8]) %/% step
df <-as.data.frame(cbind(cell0$Width, window))
cell0_data<-as.matrix(df %>% group_by(window) %>%
  summarize(res=quantile(V1,probs=0.5),number=length(V1)))
cell0_data<-cell0_data[which(cell0_data[,3]> numbercut1),]

window<-(cell00[,8]) %/% step
df <-as.data.frame(cbind(cell00$Width, window))
cell00_data<-as.matrix(df %>% group_by(window) %>%
  summarize(res=quantile(V1,probs=0.5),number=length(V1)))
cell00_data<-cell00_data[which(cell00_data[,3]> numbercut1),]



window<-(cell1[,8]) %/% step
df <-as.data.frame(cbind(cell1$Width, window))
cell1_data<-as.matrix(df %>% group_by(window) %>%
  summarize(res=quantile(V1,probs=0.5),number=length(V1)))
cell1_data<-cell1_data[which(cell1_data[,3]>numbercut1),]
window<-(cell2[,8]) %/% step
df <-as.data.frame(cbind(cell2$Width, window))
cell2_data<-as.matrix(df %>% group_by(window) %>%
  summarize(res=quantile(V1,probs=0.5),number=length(V1)))
cell2_data<-cell2_data[which(cell2_data[,3]>numbercut1),]

plot(cell2_data[,1]* step, cell2_data[,2],col="#FF6600",ylim=c(0,400),xlim=c(-5000,100000))

#sister middle 
points(cell1_data[,1]* step, cell1_data[,2],col="grey")
#plot(cell12_data[,1]* step, cell12_data[,2],col="#FF6600",ylim=c(0,400),xlim=c(0,100000))
# points(cell2_data[,1]* step, cell2_data[,2],col="green")
  #old sister 3better
points(cell3_data[,1]* step, cell3_data[,2],col="green")
#new base 
points(cell0_data[,1]* step, cell0_data[,2],col="yellow")
#sister middle 
#points(cell1_data[,1]* step, cell1_data[,2],col="grey")
points(cell00_data[,1]* step, cell00_data[,2],col="purple")
#sister middle 


if(list1[oo]%in%list5){
cell8 <-read.csv(paste0("~/Desktop/handmark/",list1[oo],"/pairs.csv"), header=T)
head(cell8)
cell8[,3]<-cell8[,3]/2.7
cell8[,4]<-cell8[,4]/2.7
cell8[,5]<-cell8[,5]/2.7
cell8[,6]<-cell8[,6]/2.7
cell8[,7]<-cell8[,7]/2.7

real_location<-rep(0,dim(location)[1])
real_location[1]<-location$X.offset[1]
for (i in 2:dim(location)[1]){
	real_location[i]<-sum(location$X.offset[1:i])
	
}
real_location <-real_location[match(substr(cell8$Name,1,43),substr(location[,2],1,43))]
real_location[which(substr(cell8$Name,1,43)==substr(location[1,1],1,43))] <-0
if(orientation[oo]=="right"){
	#going right 
cell8[,8] <-c(cell8[,3]/cos(direction[match(substr(cell8[,1],1,43),substr(direction[,1],1,43) ),2])+ real_location)-emptylength-divisionzone
	}else{
#going left
	cell8[,8] <- c((3826/2.7-cell8[,3])/cos(direction[match(substr(cell8[,1],1,43),substr(direction[,1],1,43) ),2])- real_location)-emptylength-divisionzone
	}
#write.csv(cell3,"~/Downloads/test/pairs_trichome.csv")

cell8<-cell8[which(cell8[,8]>0),]
cell8<-na.omit(cell8)
window<-(cell8[,8]) %/% step
df <-as.data.frame(cbind(cell8$Distance, window))
cell8_data<-as.matrix(df %>% group_by(window) %>%
  summarize(res=quantile(V1,probs=0.2),number=length(V1)))
cell8_data<-cell8_data[which(cell8_data[,3]> 0),]

points(cell8_data[,1]* step, cell8_data[,2],col="green")
}else{
	cell8_data<-cell3_data[1:2,]
	}



testdata<-cell00_data
testdataoriginal<-cell00
for(i in 1:(dim(testdata)[1]-1)){
if((testdata[i+1,1]-testdata[i,1])>3){
	print(c(testdata[i+1,1]* step,testdata[i,1]*step, step*(testdata[i+1,1]-testdata[i,1])))
	print(plyr::count(testdataoriginal[which((testdataoriginal[,8]<testdata[i+1,1]*step)&(testdataoriginal[,8]>testdata[i,1]*step)),1]))
}
}
print(list1[oo])
distance<-problem<-NULL
for(i in 1:(dim(testdata)[1]-1)){
distance<-c(distance,testdata[i+1,2]-testdata[i,2])
}
	
for(i in 1:(dim(testdata)[1]-1)){
if((testdata[i+1,2]-testdata[i,2])%in%boxplot.stats(distance)$out){
	print(c(i, testdata[i+1,1]* step,testdata[i,1]*step, step*(testdata[i+1,1]-testdata[i,1])))
	print(plyr::count(testdataoriginal[which((testdataoriginal[,8]<testdata[i+1,1]*step)&(testdataoriginal[,8]>testdata[i,1]*step)),1]))
	problem<-c(problem,i)
}
}




divisioncellsize<-c(divisioncellsize ,mean(cell0[which(cell0[,8]<0),5]))
divisionzonelist<-c(divisionzonelist, divisionzone)
matrix1[match(c(cell0_data[,1]* step),step*c(-100:300)),oo]<-c( cell0_data[,2])
matrix2[match(c(cell1_data[,1]* step), step*c(-30:300)),oo]<-c( cell1_data[,2])
matrix3[match(c(cell00_data[,1]* step),step*c(-30:300)),oo]<-c( cell00_data[,2])
matrix4[match(c(cell2_data[,1]* step),step*c(-30:300)),oo]<-c( cell2_data[,2])
matrix5[match(c(cell3_data[,1]* step),step*c(-30:300)),oo]<-c( cell3_data[,2])
matrix8[match(c(cell8_data[,1]* step), step*c(-30:300)),oo]<-c( cell8_data[,2])
matrix1[1:(min(match(c(cell0_data[,1]* step), step*c(-100:300)))-5),oo]<-mean(cell0[which(cell0[,8]<0),5])
}
dev.off()
divisionzonelist/divisioncellsize
matrix<-rbind(matrix1, matrix2,matrix3, matrix4, matrix5, matrix8)


matrix<-cbind(c(step*c(-100:300),step*c(-30:300),step*c(-30:300),step*c(-30:300),step*c(-30:300),step*c(-30:300)), matrix)
colnames(matrix)<-c("location", list2)
matrix[which(matrix==0)]<-NA
write.csv(matrix,"~/Downloads/matrixbarley.csv", row.names=FALSE)


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

  pdf(paste0("~/Downloads/barley_8.pdf"), width=10, height = 20)
par(mfrow = c(6, 2))

parameters <-read.csv(paste0("~/Downloads/matrixbarley_out.csv"), header=T)
name <-read.csv(paste0("~/Downloads/matrixbarley.csv"), header=T)

write.csv(cbind(list2,substr(list2,1,8),substr(list2,8+4,8+10),divisionzonelist, divisioncellsize, parameters),"~/Downloads/matrixbarley_out2.csv")

  colnames(name)<-c("location",list2)
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
res_max<-0 
 return(-res_max)
} 

a="control"
aa="drought"
bb=8
listname<-c("Barley-W","Barley-W")
list4<-substr(list2,bb+4,bb+10)

write.csv(cbind(list2,substr(list2,1,8),substr(list2,bb+4,bb+10),divisionzonelist, divisioncellsize, parameters),"~/Downloads/matrixbarley_out2.csv")


max<-max(name[,which(substr(colnames(name),1,bb)%in% listname)], na.rm=T)
xmax<-max(parameters[,2], na.rm=T)*7
ymatrix1<-ymatrix2<-NULL
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
x1=Find_Max_CCF(y0,y1)+x
print(Find_Max_CCF(y0,y1))
  	plot( x1, y1, type="l", col="#66CC66",ylim=c(0,400),xlim=c(0,50000) ,xlab="distance(um)", ylab="Cell length (um)" )
points(name[,1],name[,i+1])

ymatrix1<-c(ymatrix1,y1)
}
for(i in which((substr(colnames(name),bb+4,bb+10)==aa)&(substr(colnames(name),1,bb)%in% listname))){

	
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
x1=Find_Max_CCF(y0,y1)+x
print(Find_Max_CCF(y0,y1))

  	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,400),xlim=c(0,50000),xlab="distance(um)", ylab="Cell length (um)" )
points(name[,1],name[,i+1])

ymatrix2<-c(ymatrix2,y1)
}

ss<-0
ymatrix1<-ymatrix2<-NULL
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
x1=Find_Max_CCF(y0,y1)+x
print(Find_Max_CCF(y0,y1))
  	plot( x1, y1, type="l", col="#66CC66",ylim=c(0,400),xlim=c(0,xmax) ,xlab="distance(um)", ylab="Cell length (um)" )
par(new=TRUE)	
ymatrix1<-c(ymatrix1,y1)
ss=ss+1
sslist<-c(sslist,rep(ss,length(y1)))

}
for(i in which((substr(colnames(name),bb+4,bb+10)==aa)&(substr(colnames(name),1,bb)%in% listname))){

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
x1=Find_Max_CCF(y0,y1)+x
print(Find_Max_CCF(y0,y1))

  	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,400),xlim=c(0,xmax),xlab="distance(um)", ylab="Cell length (um)" )
par(new=TRUE)	
ymatrix2<-c(ymatrix2,y1)
ss=ss+1
sslist<-c(sslist,rep(ss,length(y1)))
}
 	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,400),xlim=c(0,xmax),xlab="distance(um)", ylab="Cell length (um)" )


library(ggplot2)

all_data_bd21_plot<-cbind(rep(x,length(which(substr(colnames(name),1,bb)%in% listname))),c(ymatrix1, ymatrix2),sslist,c(rep("Control",length(ymatrix1)),rep("Drought",length(ymatrix2))))

all_data_bd21_plot<-all_data_bd21_plot[which(all_data_bd21_plot[,1]>0),]



colnames(all_data_bd21_plot)<-c("location","size","sample","treatment")
write.csv(as.matrix(all_data_bd21_plot),"~/Downloads/tmp.csv")
all_data_bd21_plot <-read.csv(paste0("~/Downloads/tmp.csv"), header=T,row.names=1)
library(ggplot2)
library(reshape2)

all_data_bd21_plot$treatment<-as.factor(all_data_bd21_plot$treatment)
bbbbbb<-ggplot(all_data_bd21_plot,aes(location,size))+
  theme_classic()+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = treatment))+
  labs(x = expression(paste("Distance (", mu, "m)")),y = expression(paste("Cell length (", mu, "m)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",size = 1,aes(fill = treatment),alpha = 0.3)+
  guides(fill = "none")+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = treatment))+
  labs(x = expression(paste("Distance (", mu, "m)")),y = expression(paste("Cell length (", mu, "m)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  geom_line(aes(group = interaction(sample, treatment),colour = treatment,alpha = 0.2))+
  guides(alpha= "none")+ scale_color_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+scale_fill_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+ ggtitle("Barley-Wild")
 #bbbbbb
 
a="control"
aa="drought"
bb=8
listname<-c("Barley-D","Barley-D")
list4<-substr(list2,bb+4,bb+10)



max<-max(name[,which(substr(colnames(name),1,bb)%in% listname)], na.rm=T)
xmax<-max(parameters[,2], na.rm=T)*7
ymatrix1<-ymatrix2<-NULL
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
x1=Find_Max_CCF(y0,y1)+x
print(Find_Max_CCF(y0,y1))
  	plot( x1, y1, type="l", col="#66CC66",ylim=c(0,400),xlim=c(0,50000) ,xlab="distance(um)", ylab="Cell length (um)" )
points(name[,1],name[,i+1])

ymatrix1<-c(ymatrix1,y1)
}
for(i in which((substr(colnames(name),bb+4,bb+10)==aa)&(substr(colnames(name),1,bb)%in% listname))){

	
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
x1=Find_Max_CCF(y0,y1)+x
print(Find_Max_CCF(y0,y1))

  	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,400),xlim=c(0,50000),xlab="distance(um)", ylab="Cell length (um)" )
points(name[,1],name[,i+1])

ymatrix2<-c(ymatrix2,y1)
}

ss<-0
ymatrix1<-ymatrix2<-NULL
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
x1=Find_Max_CCF(y0,y1)+x
print(Find_Max_CCF(y0,y1))
  	plot( x1, y1, type="l", col="#66CC66",ylim=c(0,400),xlim=c(0,xmax) ,xlab="distance(um)", ylab="Cell length (um)" )
par(new=TRUE)	
ymatrix1<-c(ymatrix1,y1)
ss=ss+1
sslist<-c(sslist,rep(ss,length(y1)))

}
for(i in which((substr(colnames(name),bb+4,bb+10)==aa)&(substr(colnames(name),1,bb)%in% listname))){

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
x1=Find_Max_CCF(y0,y1)+x
print(Find_Max_CCF(y0,y1))

  	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,400),xlim=c(0,xmax),xlab="distance(um)", ylab="Cell length (um)" )
par(new=TRUE)	
ymatrix2<-c(ymatrix2,y1)
ss=ss+1
sslist<-c(sslist,rep(ss,length(y1)))
}
 	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,400),xlim=c(0,xmax),xlab="distance(um)", ylab="Cell length (um)" )


library(ggplot2)

all_data_bd21_plot<-cbind(rep(x,length(which(substr(colnames(name),1,bb)%in% listname))),c(ymatrix1, ymatrix2),sslist,c(rep("Control",length(ymatrix1)),rep("Drought",length(ymatrix2))))

all_data_bd21_plot<-all_data_bd21_plot[which(all_data_bd21_plot[,1]>0),]



colnames(all_data_bd21_plot)<-c("location","size","sample","treatment")
write.csv(as.matrix(all_data_bd21_plot),"~/Downloads/tmp.csv")
all_data_bd21_plot <-read.csv(paste0("~/Downloads/tmp.csv"), header=T,row.names=1)
library(ggplot2)
library(reshape2)

all_data_bd21_plot$treatment<-as.factor(all_data_bd21_plot$treatment)
cccccc <-ggplot(all_data_bd21_plot,aes(location,size))+
  theme_classic()+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = treatment))+
  labs(x = expression(paste("Distance (", mu, "m)")),y = expression(paste("Cell length (", mu, "m)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",size = 1,aes(fill = treatment),alpha = 0.3)+
  guides(fill = "none")+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = treatment))+
  labs(x = expression(paste("Distance (", mu, "m)")),y = expression(paste("Cell length (", mu, "m)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  geom_line(aes(group = interaction(sample, treatment),colour = treatment,alpha = 0.2))+
  guides(alpha= "none") + scale_color_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+scale_fill_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+ ggtitle(
"Barley-Domesticated")
 #cccccc

  dev.off()
  









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

  pdf(paste0("~/Downloads/barley_9.pdf"), width=10, height = 20)
par(mfrow = c(6, 2))

parameters <-read.csv(paste0("~/Downloads/matrixbarley_out.csv"), header=T)
name <-read.csv(paste0("~/Downloads/matrixbarley.csv"), header=T)

  colnames(name)<-c("location",list2)
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
res_max<-0 
 return(-res_max)
} 
elongationrate_c<-1.1870709
elongationrate_d<-0.4500016
a="control"
aa="drought"
bb=8
listname<-c("Barley-W","Barley-W")
list4<-substr(list2,bb+4,bb+10)


max<-max(name[,which(substr(colnames(name),1,bb)%in% listname)], na.rm=T)
xmax<-50000
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

print(Find_Max_CCF(y0,y1))
  	plot( x1, y1, type="l", col="#66CC66",ylim=c(0,400),xlim=c(0,xmax) ,xlab="distance(um)", ylab="Cell length (um)" )
par(new=TRUE)	
ymatrix1<-c(ymatrix1,y1)
ss=ss+1
sslist<-c(sslist,rep(ss,length(y1)))

}
for(i in which((substr(colnames(name),bb+4,bb+10)==aa)&(substr(colnames(name),1,bb)%in% listname))){

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
  y1 = L / (1 + exp(-k*(x-x0))) + b
x1=x
rootlist<-NULL
for (ismall in 1:length(x1)){
g <- Vectorize(function(x) parameters[i,5]/elongationrate_d*(integrate(function(t) 1/( L / (1 + exp(-k*(t-x0))) + b), lower = 0, upper = x)$value)-x1[ismall])
root <- uniroot(g,c(-10000,200000))$root
rootlist<-c(rootlist,root)
}

  y1 = L / (1 + exp(-k*(rootlist-x0))) + b

xmatrix2<-c(xmatrix2,x1)

  	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,400),xlim=c(0,xmax),xlab="distance(um)", ylab="Cell length (um)" )
par(new=TRUE)	
ymatrix2<-c(ymatrix2,y1)
ss=ss+1
sslist<-c(sslist,rep(ss,length(y1)))
}
 	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,400),xlim=c(0,xmax),xlab="distance(um)", ylab="Cell length (um)" )


library(ggplot2)

all_data_bd21_plot<-cbind(c(xmatrix1, xmatrix2)/1000,c(ymatrix1, ymatrix2),sslist,c(rep("Control",length(ymatrix1)),rep("Drought",length(ymatrix2))))

all_data_bd21_plot<-all_data_bd21_plot[which(all_data_bd21_plot[,1]>0),]



colnames(all_data_bd21_plot)<-c("location","size","sample","treatment")
write.csv(as.matrix(all_data_bd21_plot),"~/Downloads/tmp.csv")
all_data_bd21_plot <-read.csv(paste0("~/Downloads/tmp.csv"), header=T,row.names=1)
library(ggplot2)
library(reshape2)

all_data_bd21_plot$treatment<-as.factor(all_data_bd21_plot$treatment)
bbbbbb2<-ggplot(all_data_bd21_plot,aes(location,size))+
  theme_classic()+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = treatment))+
  labs(x = "Thermal time (°Cd)",y = expression(paste("Cell length (", mu, "m)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",size = 1,aes(fill = treatment),alpha = 0.3)+
  guides(fill = "none")+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = treatment))+
  labs(x = "Thermal time (°Cd)",y = expression(paste("Cell length (", mu, "m)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  geom_line(aes(group = interaction(sample, treatment),colour = treatment,alpha = 0.2))+
  guides(alpha= "none")+ scale_color_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+scale_fill_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+ ggtitle(
"Barley-Wild")
 #bbbbbb
 
a="control"
aa="drought"
bb=8
listname<-c("Barley-D","Barley-D")
list4<-substr(list2,bb+4,bb+10)

elongationrate_c<-1.3098889
elongationrate_d<-0.6172375


max<-max(name[,which(substr(colnames(name),1,bb)%in% listname)], na.rm=T)

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
print(Find_Max_CCF(y0,y1))
  	plot( x1, y1, type="l", col="#66CC66",ylim=c(0,400),xlim=c(0,xmax) ,xlab="distance(um)", ylab="Cell length (um)" )
par(new=TRUE)	
ymatrix1<-c(ymatrix1,y1)
ss=ss+1
sslist<-c(sslist,rep(ss,length(y1)))

}
for(i in which((substr(colnames(name),bb+4,bb+10)==aa)&(substr(colnames(name),1,bb)%in% listname))){

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
  y1 = L / (1 + exp(-k*(x-x0))) + b
x1=x
rootlist<-NULL
for (ismall in 1:length(x1)){
g <- Vectorize(function(x) parameters[i,5]/elongationrate_d*(integrate(function(t) 1/( L / (1 + exp(-k*(t-x0))) + b), lower = 0, upper = x)$value)-x1[ismall])
root <- uniroot(g,c(-10000,200000))$root
rootlist<-c(rootlist,root)
}

  y1 = L / (1 + exp(-k*(rootlist-x0))) + b

xmatrix2<-c(xmatrix2,x1)

  	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,400),xlim=c(0,xmax),xlab="distance(um)", ylab="Cell length (um)" )
par(new=TRUE)	
ymatrix2<-c(ymatrix2,y1)
ss=ss+1
sslist<-c(sslist,rep(ss,length(y1)))
}
 	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,400),xlim=c(0,xmax),xlab="distance(um)", ylab="Cell length (um)" )


library(ggplot2)

all_data_bd21_plot<-cbind(c(xmatrix1, xmatrix2)/1000,c(ymatrix1, ymatrix2),sslist,c(rep("Control",length(ymatrix1)),rep("Drought",length(ymatrix2))))

all_data_bd21_plot<-all_data_bd21_plot[which(all_data_bd21_plot[,1]>0),]



colnames(all_data_bd21_plot)<-c("location","size","sample","treatment")
write.csv(as.matrix(all_data_bd21_plot),"~/Downloads/tmp.csv")
all_data_bd21_plot <-read.csv(paste0("~/Downloads/tmp.csv"), header=T,row.names=1)
library(ggplot2)
library(reshape2)

all_data_bd21_plot$treatment<-as.factor(all_data_bd21_plot$treatment)
cccccc2 <-ggplot(all_data_bd21_plot,aes(location,size))+
  theme_classic()+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = treatment))+
  labs(x = "Thermal time (°Cd)",y = expression(paste("Cell length (", mu, "m)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",size = 1,aes(fill = treatment),alpha = 0.3)+
  guides(fill = "none")+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = treatment))+
  labs(x = "Thermal time (°Cd)",y = expression(paste("Cell length (", mu, "m)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  geom_line(aes(group = interaction(sample, treatment),colour = treatment,alpha = 0.2))+
  guides(alpha= "none")+
   scale_color_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+scale_fill_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+ ggtitle(
"Barley-Domesticated")
 #cccccc

  dev.off()
  



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

matrix1<-matrix3<-matrix2<-matrix8<-matrix(0,531, length(list1))
matrix1<-matrix(0,601, length(list1))
divisioncellsize <-divisionzonelist<-NULL
 pdf(paste0("~/Downloads/wheatz_10.pdf"), width=7, height = 4)
for(oo in c(1:length(list1))){
cell2_data<-cell3_data<-cell0_data<-cell4_data<-cell2<-cell3<-cell0<-cell4<-cell8<-cell8_data<-NULL	
step<-200
numbercut1=0
numbercut2=10
#handmark
numbercut3=2

cell3 <-read.csv(paste0("/Volumes/TOSHIBAEXT/image_results/trichome_results/",list2[oo],"/pairs.csv"), header=T)
head(cell3)
cell3[,3]<-cell3[,3]/2.7
cell3[,4]<-cell3[,4]/2.7
cell3[,5]<-cell3[,5]/2.7
cell3[,6]<-cell3[,6]/2.7
cell3[,7]<-cell3[,7]/2.7

location <-read.csv(paste0("/Volumes/TOSHIBAEXT/images/leaf\ extension\ 2/",list1[oo],"/stitching_result.csv"), header=T)
dim(location)
head(location)
location[,3]<-location[,3]/2.7
location[,4]<-location[,4]/2.7



direction <-read.csv(paste0("/Volumes/TOSHIBAEXT/images/leaf\ extension\ 2/",list1[oo],"/dirs.csv"), header=T)
dim(direction)
location[,3]<-location[,3]/cos(direction[match(substr(location[,1],1,namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2]) 
emptylength<-empty[oo]/cos(direction[1,2])/2.7


cell2 <-read.csv(paste0("/Volumes/TOSHIBAEXT/image_results/sister_results/",list2[oo],"/cells.csv"), header=T)
head(cell2)
cell2[,3]<-cell2[,3]/2.7
cell2[,4]<-cell2[,4]/2.7
cell2[,5]<-cell2[,5]/2.7
cell2[,6]<-cell2[,6]/2.7
head(cell3)
cell2[,5]<-rowMaxs(cbind(cell2[,5], cell2[,6]))
head(cell3)
##base cells /Volumes/TOSHIBAEXT/image_results/newbase_results/Bd21-4-control/cells.csv
cell0 <-read.csv(paste0("/Volumes/TOSHIBAEXT/image_results/basenew/",list2[oo],"/cells.csv"), header=T)
head(cell0)
cell0[,3]<-cell0[,3]/2.7
cell0[,4]<-cell0[,4]/2.7
cell0[,5]<-rowMaxs(cbind(cell0[,5]/2.7, cell0[,6]/2.7))
cell4 <-read.csv(paste0("~/Desktop/divisionzonemark/",list2[oo],"/cells.csv"), header=T)
head(cell4)
cell4[,3]<-cell4[,3]/2.7
cell4[,4]<-cell4[,4]/2.7
##plot with three colors
real_location<-rep(0,dim(location)[1])
real_location[1]<-location$X.offset[1]
for (i in 2:dim(location)[1]){
	real_location[i]<-sum(location$X.offset[1:i])
}
real_location <-real_location[match(substr(cell4$Name,1,namelength[oo]),substr(location[,2],1,namelength[oo]))]
real_location[which(substr(cell4$Name,1,namelength[oo])==substr(location[1,1],1,namelength[oo]))] <-0
if(orientation[oo]=="right"){
	#going right 
cell4[,8] <-c(cell4[,3]/cos(direction[match(substr(cell4[,1],1, namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2])+ real_location)-emptylength
	}else{
#going left
	cell4[,8] <- c((3826/2.7-cell4[,3])/cos(direction[match(substr(cell4[,1],1,namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2])- real_location)-emptylength
	}

divisionzone<-min(cell4[,8])

##plot with three colors

	##plot with three colors
real_location<-rep(0,dim(location)[1])
real_location[1]<-location$X.offset[1]
for (i in 2:dim(location)[1]){
	if(list2[oo]=="Wheat-Z-W-3-control"){
		
	real_location[i]<-sum(abs(location$X.offset[1:i]))
	}else{	
	real_location[i]<-sum((location$X.offset[1:i]))
}
}
real_location <-real_location[match(substr(cell3$Name,1,list4[oo]),substr(location[,2],1,list4[oo]))]
real_location[which(substr(cell3$Name,1,list4[oo])==substr(location[1,1],1,list4[oo]))] <-0

	if(list2[oo]=="Wheat-Z-W-3-control"){
cell3[1:min(which(substr(cell3 $Name,1,list4[oo])==substr("2021-02-19-tris_minimal-0307_2023-05-16_024.jpg",1,list4[oo]))),8] <- c((3826/2.7-cell3[1:min(which(substr(cell3 $Name,1,list4[oo])==substr("2021-02-19-tris_minimal-0307_2023-05-16_024.jpg",1,list4[oo]))),3])/cos(direction[match(substr(cell3[1:min(which(substr(cell3 $Name,1,list4[oo])==substr("2021-02-19-tris_minimal-0307_2023-05-16_024.jpg",1,list4[oo]))),1],1,namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2])+ real_location[1:min(which(substr(cell3 $Name,1,list4[oo])==substr("2021-02-19-tris_minimal-0307_2023-05-16_024.jpg",1,list4[oo])))])-emptylength-divisionzone	
cell3[min(which(substr(cell3 $Name,1,list4[oo])==substr("2021-02-19-tris_minimal-0307_2023-05-16_024.jpg",1,list4[oo]))):dim(cell3)[1],8] <- c((3826/2.7-cell3[min(which(substr(cell3 $Name,1,list4[oo])==substr("2021-02-19-tris_minimal-0307_2023-05-16_024.jpg",1,list4[oo]))):dim(cell3)[1],3])/cos(direction[match(substr(cell3[min(which(substr(cell3 $Name,1,list4[oo])==substr("2021-02-19-tris_minimal-0307_2023-05-16_024.jpg",1,list4[oo]))):dim(cell3)[1],1],1,namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2])+ real_location[min(which(substr(cell3 $Name,1,list4[oo])==substr("2021-02-19-tris_minimal-0307_2023-05-16_024.jpg",1,list4[oo]))):dim(cell3)[1]])-emptylength-divisionzone		
}else{
	if(orientation[oo]=="right"){
	#going right 
cell3[,8] <-c(cell3[,3]/cos(direction[match(substr(cell3[,1],1,namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2])+ real_location)-emptylength-divisionzone
	}else{
#going left
	cell3[,8] <- c((3826/2.7-cell3[,3])/cos(direction[match(substr(cell3[,1],1,namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2])- real_location)-emptylength-divisionzone
	}
	
}

##plot with three colors

	##plot with three colors
real_location<-rep(0,dim(location)[1])
real_location[1]<-location$X.offset[1]
for (i in 2:dim(location)[1]){
	if(list2[oo]=="Wheat-Z-W-3-control"){
		
	real_location[i]<-sum(abs(location$X.offset[1:i]))
	}else{	
	real_location[i]<-sum((location$X.offset[1:i]))
}
}
real_location <-real_location[match(substr(cell0$Name,1,list4[oo]),substr(location[,2],1,list4[oo]))]
real_location[which(substr(cell0$Name,1,list4[oo])==substr(location[1,1],1,list4[oo]))] <-0

	if(orientation[oo]=="right"){
	#going right 
cell0[,8] <-c(cell0[,3]/cos(direction[match(substr(cell0[,1],1,namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2])+ real_location)-emptylength-divisionzone
	}else{
#going left
	cell0[,8] <- c((3826/2.7-cell0[,3])/cos(direction[match(substr(cell0[,1],1,namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2])- real_location)-emptylength-divisionzone
	}
	


		
		
		

##plot with three colors

	##plot with three colors
real_location<-rep(0,dim(location)[1])
real_location[1]<-location$X.offset[1]
for (i in 2:dim(location)[1]){
	if(list2[oo]=="Wheat-Z-W-3-control"){
		
	real_location[i]<-sum(abs(location$X.offset[1:i]))
	}else{	
	real_location[i]<-sum((location$X.offset[1:i]))
}
}
real_location <-real_location[match(substr(cell2$Name,1,list4[oo]),substr(location[,2],1,list4[oo]))]
real_location[which(substr(cell2$Name,1,list4[oo])==substr(location[1,1],1,list4[oo]))] <-0

	if(list2[oo]=="Wheat-Z-W-3-control"){
cell2[1:min(which(substr(cell2$Name,1,list4[oo])==substr("2021-02-19-tris_minimal-0307_2023-05-16_024.jpg",1,list4[oo]))),8] <- c((3826/2.7-cell2[1:min(which(substr(cell2 $Name,1,list4[oo])==substr("2021-02-19-tris_minimal-0307_2023-05-16_024.jpg",1,list4[oo]))),3])/cos(direction[match(substr(cell2[1:min(which(substr(cell2$Name,1,list4[oo])==substr("2021-02-19-tris_minimal-0307_2023-05-16_024.jpg",1,list4[oo]))),1],1,namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2])+ real_location[1:min(which(substr(cell2$Name,1,list4[oo])==substr("2021-02-19-tris_minimal-0307_2023-05-16_024.jpg",1,list4[oo])))])-emptylength-divisionzone	
cell2[min(which(substr(cell2$Name,1,list4[oo])==substr("2021-02-19-tris_minimal-0307_2023-05-16_024.jpg",1,list4[oo]))):dim(cell2)[1],8] <- c((3826/2.7-cell2[min(which(substr(cell2 $Name,1,list4[oo])==substr("2021-02-19-tris_minimal-0307_2023-05-16_024.jpg",1,list4[oo]))):dim(cell2)[1],3])/cos(direction[match(substr(cell2[min(which(substr(cell2 $Name,1,list4[oo])==substr("2021-02-19-tris_minimal-0307_2023-05-16_024.jpg",1,list4[oo]))):dim(cell2)[1],1],1,namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2])+ real_location[min(which(substr(cell2 $Name,1,list4[oo])==substr("2021-02-19-tris_minimal-0307_2023-05-16_024.jpg",1,list4[oo]))):dim(cell2)[1]])-emptylength-divisionzone	
}else{
	if(orientation[oo]=="right"){
	#going right 
cell2[,8] <-c(cell2[,3]/cos(direction[match(substr(cell2[,1],1,namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2])+ real_location)-emptylength-divisionzone
	}else{
#going left
	cell2[,8] <- c((3826/2.7-cell2[,3])/cos(direction[match(substr(cell2[,1],1,namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2])- real_location)-emptylength-divisionzone
	}
	
}
if(list2[oo]=="zavi_dm1"){
	cell3<-cell3[which(cell3[,8]>10000),]
	
}

cell0<-cell0[which(cell0[,8]<0),]
cell2<-cell2[which(cell2[,8]>0),]

cell0<-na.omit(cell0)

#write.csv(cell3,"~/Downloads/test/pairs_trichome.csv")
cell3<-na.omit(cell3)
cell2<-na.omit(cell2)


window<-(cell3[,8]) %/% step
df <-as.data.frame(cbind(cell3$Distance, window))
cell3_data<-as.matrix(df %>% group_by(window) %>%
  summarize(res=quantile(V1,probs=0.2),number=length(V1)))
cell3_data<-cell3_data[which(cell3_data[,3]> numbercut2),]


window<-(cell0[,8]) %/% step
df <-as.data.frame(cbind(cell0$Width, window))
cell0_data<-as.matrix(df %>% group_by(window) %>%
  summarize(res=quantile(V1,probs=0.5),number=length(V1)))
cell0_data<-cell0_data[which(cell0_data[,3]> numbercut1),]

if(length(cell0_data)==3){
	cell0_data<-rbind(cell0_data, cell0_data)
}

window<-(cell2[,8]) %/% step
df <-as.data.frame(cbind(cell2$Width, window))
cell2_data<-as.matrix(df %>% group_by(window) %>%
  summarize(res=quantile(V1,probs=0.5),number=length(V1)))
cell2_data<-cell2_data[which(cell2_data[,3]>numbercut1),]

 plot(cell2_data[,1]* step, cell2_data[,2],col="#FF6600",ylim=c(0,400),xlim=c(-10000,50000))
points(cell0_data[,1]* step, cell0_data[,2],col="yellow")
points(cell3_data[,1]* step, cell3_data[,2],col="green")


if(list1[oo]%in%list5){
cell8 <-read.csv(paste0("~/Desktop/handmark/",list1[oo],"/pairs.csv"), header=T)
head(cell8)
cell8[,3]<-cell8[,3]/2.7
cell8[,4]<-cell8[,4]/2.7
cell8[,5]<-cell8[,5]/2.7
cell8[,6]<-cell8[,6]/2.7
cell8[,7]<-cell8[,7]/2.7

real_location<-rep(0,dim(location)[1])
real_location[1]<-location$X.offset[1]
for (i in 2:dim(location)[1]){
	real_location[i]<-sum(location$X.offset[1:i])
	
}
real_location <-real_location[match(substr(cell8$Name,1,list4[oo]),substr(location[,2],1,list4[oo]))]
real_location[which(substr(cell8$Name,1,list4[oo])==substr(location[1,1],1,list4[oo]))] <-0
if(orientation[oo]=="right"){
	#going right 
cell8[,8] <-c(cell8[,3]/cos(direction[match(substr(cell8[,1],1,list4[oo]),substr(direction[,1],1,list4[oo]) ),2])+ real_location)-emptylength-divisionzone
	}else{
#going left
	cell8[,8] <- c((3826/2.7-cell8[,3])/cos(direction[match(substr(cell8[,1],1,list4[oo]),substr(direction[,1],1,list4[oo]) ),2])- real_location)-emptylength-divisionzone
	}
#write.csv(cell3,"~/Downloads/test/pairs_trichome.csv")


cell8<-na.omit(cell8)
window<-(cell8[,8]) %/% step
df <-as.data.frame(cbind(cell8$Distance, window))
cell8_data<-as.matrix(df %>% group_by(window) %>%
  summarize(res=quantile(V1,probs=0.2),number=length(V1)))
cell8_data<-cell8_data[which(cell8_data[,3]> 3),]

points(cell8_data[,1]* step, cell8_data[,2],col="purple")
}else{
	cell8_data<-cell3_data[1:2,]
	}


testdata<-rbind(cell2_data,cell3_data)
testdata<-testdata[order(testdata[,1]),]
testdataoriginal<-cell3
for(i in 1:(dim(testdata)[1]-1)){
if((testdata[i+1,1]-testdata[i,1])>3){
	print(c(testdata[i+1,1]* step,testdata[i,1]*step, step*(testdata[i+1,1]-testdata[i,1])))
	print(plyr::count(testdataoriginal[which((testdataoriginal[,8]<testdata[i+1,1]*step)&(testdataoriginal[,8]>testdata[i,1]*step)),1]))
}
}
print(list1[oo])


  #plot(cell3_data[,1]* step, cell3_data[,2],col="#FF6600",ylim=c(0,200))
#points(cell2_data[,1]* step, cell2_data[,2],col="green")
#points(cell1_data[,1]* step, cell1_data[,2],col="black")
#points(cell3_data[problem,1]* step, cell3_data[problem,2],col="green")

print(list1[oo])





print(list1[oo])

divisioncellsize<-c(divisioncellsize ,mean(cell0[which(cell0[,8]<0),5]))
divisionzonelist<-c(divisionzonelist, divisionzone)
matrix1 [match(c(cell0_data[,1]* step),step*c(-100:500)),oo]<-c( cell0_data[,2])
matrix2[match(c(cell3_data[,1]* step), step*c(-30:500)),oo]<-c( cell3_data[,2])
matrix3[match(c(cell2_data[,1]* step),step*c(-30:500)),oo]<-c( cell2_data[,2])
matrix8[match(c(cell8_data[,1]* step),step*c(-30:500)),oo]<-c( cell8_data[,2])
matrix1[1:(min(match(c(cell0_data[,1]* step), step*c(-100:300)))-5),oo]<-mean(cell0[which(cell0[,8]<0),5])
print(dim(cell3)[1]+dim(cell0)[1]+dim(cell2)[1])
}
dev.off()

divisionzonelist/divisioncellsize
matrix<-rbind(matrix1, matrix2 ,matrix3, matrix8 )


matrix<-cbind(c(step*c(-100:500),step*c(-30:500),step*c(-30:500),step*c(-30:500)), matrix)
colnames(matrix)<-c("location", list2)
matrix[which(matrix==0)]<-NA
write.csv(matrix,"~/Downloads/matrixwheatzavitan.csv", row.names=FALSE)
#write.csv(matrix,"~/Downloads/matrixwheatcs.csv", row.names=FALSE)
#write.csv(matrix,"~/Downloads/matrixwheatL.csv", row.names=FALSE)



list2<-c("Wheat-Z-W-1-control",
"Wheat-Z-W-2-control",
 "Wheat-Z-W-3-control" ,
"Wheat-Z-W-4-control" ,
"zavi_ce2_normal" ,
"zavi_cm1","Wheat-Z-W7x-drought",
"Wheat-Z-de2-drought","zavi_dex_redo1"	,"Wheat-Z-Wm1-drought",	"Wheat-Z-Wm3-drought")[-c(5,6,9)]


  pdf(paste0("~/Downloads/zavitan_8.pdf"), width=10, height = 20)
par(mfrow = c(6, 2))

parameters <-read.csv(paste0("~/Downloads/matrixwheatzavitan_out.csv"), header=T)
name <-read.csv(paste0("~/Downloads/matrixwheatzavitan.csv"), header=T)

  colnames(name)<-c("location",list2)
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
res_max<-0 
 return(-res_max)
} 

a="control"
aa="drought"
bb=9
listname<-c("Wheat-Z-W","Wheat-Z-W")

max<-max(name[,which(substr(colnames(name),1,bb)%in% listname)], na.rm=T)
xmax<-max(parameters[,2], na.rm=T)*7
ymatrix1<-ymatrix2<-NULL
for(i in which(substr(colnames(name),bb+4,bb+10)==a)){
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
x1=Find_Max_CCF(y0,y1)+x
print(Find_Max_CCF(y0,y1))
  	plot( x1, y1, type="l", col="#66CC66",ylim=c(0,400),xlim=c(0,50000) ,xlab="distance(um)", ylab="Cell length (um)" )
points(name[,1],name[,i+1])

ymatrix1<-c(ymatrix1,y1)
}
for(i in which(substr(colnames(name),bb+4,bb+10)==aa)){
	
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
x1=Find_Max_CCF(y0,y1)+x
print(Find_Max_CCF(y0,y1))

  	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,400),xlim=c(0,50000),xlab="distance(um)", ylab="Cell length (um)" )
points(name[,1],name[,i+1])

ymatrix2<-c(ymatrix2,y1)
}

ss<-0
ymatrix1<-ymatrix2<-NULL
sslist<-NULL
for(i in which(substr(colnames(name),bb+4,bb+10)==a)){
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
x1=Find_Max_CCF(y0,y1)+x
print(Find_Max_CCF(y0,y1))
  	plot( x1, y1, type="l", col="#66CC66",ylim=c(0,400),xlim=c(0,xmax) ,xlab="distance(um)", ylab="Cell length (um)" )
par(new=TRUE)	
ymatrix1<-c(ymatrix1,y1)
ss=ss+1
sslist<-c(sslist,rep(ss,length(y1)))

}
for(i in which(substr(colnames(name),bb+4,bb+10)==aa)){
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
x1=Find_Max_CCF(y0,y1)+x
print(Find_Max_CCF(y0,y1))

  	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,400),xlim=c(0,xmax),xlab="distance(um)", ylab="Cell length (um)" )
par(new=TRUE)	
ymatrix2<-c(ymatrix2,y1)
ss=ss+1
sslist<-c(sslist,rep(ss,length(y1)))
}
 	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,400),xlim=c(0,xmax),xlab="distance(um)", ylab="Cell length (um)" )


library(ggplot2)

all_data_bd21_plot<-cbind(rep(x,length(which(substr(colnames(name),1,bb)%in% listname))),c(ymatrix1, ymatrix2),sslist,c(rep("Control",length(ymatrix1)),rep("Drought",length(ymatrix2))))

all_data_bd21_plot<-all_data_bd21_plot[which(all_data_bd21_plot[,1]>0),]



colnames(all_data_bd21_plot)<-c("location","size","sample","treatment")
write.csv(as.matrix(all_data_bd21_plot),"~/Downloads/tmp.csv")
all_data_bd21_plot <-read.csv(paste0("~/Downloads/tmp.csv"), header=T,row.names=1)
library(ggplot2)
library(reshape2)

all_data_bd21_plot$treatment<-as.factor(all_data_bd21_plot$treatment)
ffffff<-ggplot(all_data_bd21_plot,aes(location,size))+
  theme_classic()+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = treatment))+
  labs(x = expression(paste("Distance (", mu, "m)")),y = expression(paste("Cell length (", mu, "m)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",size = 1,aes(fill = treatment),alpha = 0.3)+
  guides(fill = "none")+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = treatment))+
  labs(x = expression(paste("Distance (", mu, "m)")),y = expression(paste("Cell length (", mu, "m)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  geom_line(aes(group = interaction(sample, treatment),colour = treatment,alpha = 0.2))+
  guides(alpha= "none")+ scale_color_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+scale_fill_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+ ggtitle(
"Wheat-Z-Wild")
 #ffffff
  dev.off()
  
list4<-substr(list2,bb+4,bb+10)

write.csv(cbind(list2,rep(listname[1],length(list2)),list4,divisionzonelist, divisioncellsize, parameters),"~/Downloads/matrixzavitan_out2.csv")



list2<-c("Wheat-Z-W-1-control",
"Wheat-Z-W-2-control",
 "Wheat-Z-W-3-control" ,
"Wheat-Z-W-4-control" ,
"zavi_ce2_normal" ,
"zavi_cm1","Wheat-Z-W7x-drought",
"Wheat-Z-de2-drought","zavi_dex_redo1"	,"Wheat-Z-Wm1-drought",	"Wheat-Z-Wm3-drought")[-c(5,6,9)]


  pdf(paste0("~/Downloads/zavitan_9.pdf"), width=10, height = 20)
par(mfrow = c(6, 2))

parameters <-read.csv(paste0("~/Downloads/matrixwheatzavitan_out.csv"), header=T)
name <-read.csv(paste0("~/Downloads/matrixwheatzavitan.csv"), header=T)

  colnames(name)<-c("location",list2)
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
res_max<-0 
 return(-res_max)
} 

a="control"
aa="drought"
bb=9
listname<-c("Wheat-Z-W","Wheat-Z-W")
elongationrate_c<-0.7984710
elongationrate_d<-0.4778396


max<-max(name[,which(substr(colnames(name),1,bb)%in% listname)], na.rm=T)
xmax<-max(parameters[,2], na.rm=T)*7
ymatrix1<-ymatrix2<-xmatrix1<-xmatrix2<-NULL
ss<-0
ymatrix1<-ymatrix2<-NULL
sslist<-NULL
for(i in which(substr(colnames(name),bb+4,bb+10)==a)){
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
print(Find_Max_CCF(y0,y1))
  	plot( x1, y1, type="l", col="#66CC66",ylim=c(0,400),xlim=c(0,xmax) ,xlab="distance(um)", ylab="Cell length (um)" )
par(new=TRUE)	
ymatrix1<-c(ymatrix1,y1)
ss=ss+1
sslist<-c(sslist,rep(ss,length(y1)))

}
for(i in which(substr(colnames(name),bb+4,bb+10)==aa)){
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
  y1 = L / (1 + exp(-k*(x-x0))) + b
x1=x
rootlist<-NULL
for (ismall in 1:length(x1)){
g <- Vectorize(function(x) parameters[i,5]/elongationrate_d*(integrate(function(t) 1/( L / (1 + exp(-k*(t-x0))) + b), lower = 0, upper = x)$value)-x1[ismall])
root <- uniroot(g,c(-10000,200000))$root
rootlist<-c(rootlist,root)
}

  y1 = L / (1 + exp(-k*(rootlist-x0))) + b

xmatrix2<-c(xmatrix2,x1)
print(Find_Max_CCF(y0,y1))
  	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,400),xlim=c(0,xmax) ,xlab="distance(um)", ylab="Cell length (um)" )
par(new=TRUE)	
ymatrix2<-c(ymatrix2,y1)
ss=ss+1
sslist<-c(sslist,rep(ss,length(y1)))
}
 	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,400),xlim=c(0,xmax),xlab="distance(um)", ylab="Cell length (um)" )


library(ggplot2)

all_data_bd21_plot<-cbind(c(xmatrix1, xmatrix2)/1000,c(ymatrix1, ymatrix2),sslist,c(rep("Control",length(ymatrix1)),rep("Drought",length(ymatrix2))))

all_data_bd21_plot<-all_data_bd21_plot[which(all_data_bd21_plot[,1]>0),]



colnames(all_data_bd21_plot)<-c("location","size","sample","treatment")
write.csv(as.matrix(all_data_bd21_plot),"~/Downloads/tmp.csv")
all_data_bd21_plot <-read.csv(paste0("~/Downloads/tmp.csv"), header=T,row.names=1)
library(ggplot2)
library(reshape2)

all_data_bd21_plot$treatment<-as.factor(all_data_bd21_plot$treatment)
hhhhhh2<-ggplot(all_data_bd21_plot,aes(location,size))+
  theme_classic()+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = treatment))+
  labs(x = "Thermal time (°Cd)",y = expression(paste("Cell length (", mu, "m)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",size = 1,aes(fill = treatment),alpha = 0.3)+
  guides(fill = "none")+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = treatment))+
  labs(x = "Thermal time (°Cd)",y = expression(paste("Cell length (", mu, "m)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  geom_line(aes(group = interaction(sample, treatment),colour = treatment,alpha = 0.2))+
  guides(alpha= "none")+ scale_color_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+scale_fill_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+ ggtitle(
"Wheat-Z-Wild")
# aaaaaa
  dev.off()
  
list4<-substr(list2,bb+4,bb+10)


 list1<- c("Wheat L D 1 control" ,"Wheat L D 1 drought", "Wheat L D 2 control", "Wheat L D 2 drought" ,"Wheat L D 3 control",  "Wheat L D 3 drought", "Wheat L D 4 control" , "Wheat L D 4 drought" )
 list2<-c("Wheat-L-D-1-control" ,"Wheat-L-D-1-drought", "Wheat-L-D-2-control", "Wheat-L-D-2-drought" ,"Wheat-L-D-3-control",  "Wheat-L-D-3-drought", "Wheat-L-D-4-control" , "Wheat-L-D-4-drought" )
orientation<-c("right","right","right","right","right","right","right",   "right")
empty<-c(650,578,1135,447,1011,0,904,583)
namelength <-list4<-rep(43,length(list1))
list5=c("Wheat L D 1 control" ,"Wheat L D 1 drought",  "Wheat L D 3 control" , "Wheat L D 3 drought", "Wheat L D 4 control" , "Wheat L D 4 drought" )



matrix1<-matrix3<-matrix2<-matrix8<-matrix(0,531, length(list1))
matrix1<-matrix(0,601, length(list1))
divisioncellsize <-divisionzonelist<-NULL
 pdf(paste0("~/Downloads/wheatl_10.pdf"), width=7, height = 4)
for(oo in c(1:length(list1))){
cell2_data<-cell3_data<-cell0_data<-cell4_data<-cell2<-cell3<-cell0<-cell4<-cell8<-cell8_data<-NULL	
step<-200
numbercut1=3
numbercut2=10
#handmark
numbercut3=2

cell3 <-read.csv(paste0("/Volumes/TOSHIBAEXT/image_results/trichome_results/",list2[oo],"/pairs.csv"), header=T)
head(cell3)
cell3[,3]<-cell3[,3]/2.7
cell3[,4]<-cell3[,4]/2.7
cell3[,5]<-cell3[,5]/2.7
cell3[,6]<-cell3[,6]/2.7
cell3[,7]<-cell3[,7]/2.7

location <-read.csv(paste0("/Volumes/TOSHIBAEXT/images/leaf\ extension\ 2/",list1[oo],"/stitching_result.csv"), header=T)
dim(location)
head(location)
location[,3]<-location[,3]/2.7
location[,4]<-location[,4]/2.7



direction <-read.csv(paste0("/Volumes/TOSHIBAEXT/images/leaf\ extension\ 2/",list1[oo],"/dirs.csv"), header=T)
dim(direction)
location[,3]<-location[,3]/cos(direction[match(substr(location[,1],1,namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2]) 
emptylength<-empty[oo]/cos(direction[1,2])/2.7


cell2 <-read.csv(paste0("/Volumes/TOSHIBAEXT/image_results/sister_results/",list2[oo],"/cells.csv"), header=T)
head(cell2)
cell2[,3]<-cell2[,3]/2.7
cell2[,4]<-cell2[,4]/2.7
cell2[,5]<-cell2[,5]/2.7
cell2[,6]<-cell2[,6]/2.7
head(cell3)
cell2[,5]<-rowMaxs(cbind(cell2[,5], cell2[,6]))
head(cell3)
##base cells /Volumes/TOSHIBAEXT/image_results/newbase_results/Bd21-4-control/cells.csv
cell0 <-read.csv(paste0("/Volumes/TOSHIBAEXT/image_results/basenew/",list2[oo],"/cells.csv"), header=T)
head(cell0)
cell0[,3]<-cell0[,3]/2.7
cell0[,4]<-cell0[,4]/2.7
cell0[,5]<-rowMaxs(cbind(cell0[,5]/2.7, cell0[,6]/2.7))
cell4 <-read.csv(paste0("~/Desktop/divisionzonemark/",list2[oo],"/cells.csv"), header=T)
head(cell4)
cell4[,3]<-cell4[,3]/2.7
cell4[,4]<-cell4[,4]/2.7
##plot with three colors
real_location<-rep(0,dim(location)[1])
real_location[1]<-location$X.offset[1]
for (i in 2:dim(location)[1]){
	real_location[i]<-sum(location$X.offset[1:i])
}
real_location <-real_location[match(substr(cell4$Name,1,namelength[oo]),substr(location[,2],1,namelength[oo]))]
real_location[which(substr(cell4$Name,1,namelength[oo])==substr(location[1,1],1,namelength[oo]))] <-0
if(orientation[oo]=="right"){
	#going right 
cell4[,8] <-c(cell4[,3]/cos(direction[match(substr(cell4[,1],1, namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2])+ real_location)-emptylength
	}else{
#going left
	cell4[,8] <- c((3826/2.7-cell4[,3])/cos(direction[match(substr(cell4[,1],1,namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2])- real_location)-emptylength
	}

divisionzone<-min(cell4[,8])

##plot with three colors

	##plot with three colors
real_location<-rep(0,dim(location)[1])
real_location[1]<-location$X.offset[1]
for (i in 2:dim(location)[1]){
	if(list2[oo]=="Wheat-Z-W-3-control"){
		
	real_location[i]<-sum(abs(location$X.offset[1:i]))
	}else{	
	real_location[i]<-sum((location$X.offset[1:i]))
}
}
real_location <-real_location[match(substr(cell3$Name,1,list4[oo]),substr(location[,2],1,list4[oo]))]
real_location[which(substr(cell3$Name,1,list4[oo])==substr(location[1,1],1,list4[oo]))] <-0

	if(list2[oo]=="Wheat-Z-W-3-control"){
cell3[1:min(which(substr(cell3 $Name,1,list4[oo])==substr("2021-02-19-tris_minimal-0307_2023-05-16_024.jpg",1,list4[oo]))),8] <- c((3826/2.7-cell3[1:min(which(substr(cell3 $Name,1,list4[oo])==substr("2021-02-19-tris_minimal-0307_2023-05-16_024.jpg",1,list4[oo]))),3])/cos(direction[match(substr(cell3[1:min(which(substr(cell3 $Name,1,list4[oo])==substr("2021-02-19-tris_minimal-0307_2023-05-16_024.jpg",1,list4[oo]))),1],1,namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2])+ real_location[1:min(which(substr(cell3 $Name,1,list4[oo])==substr("2021-02-19-tris_minimal-0307_2023-05-16_024.jpg",1,list4[oo])))])-emptylength-divisionzone	
cell3[min(which(substr(cell3 $Name,1,list4[oo])==substr("2021-02-19-tris_minimal-0307_2023-05-16_024.jpg",1,list4[oo]))):dim(cell3)[1],8] <- c((3826/2.7-cell3[min(which(substr(cell3 $Name,1,list4[oo])==substr("2021-02-19-tris_minimal-0307_2023-05-16_024.jpg",1,list4[oo]))):dim(cell3)[1],3])/cos(direction[match(substr(cell3[min(which(substr(cell3 $Name,1,list4[oo])==substr("2021-02-19-tris_minimal-0307_2023-05-16_024.jpg",1,list4[oo]))):dim(cell3)[1],1],1,namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2])+ real_location[min(which(substr(cell3 $Name,1,list4[oo])==substr("2021-02-19-tris_minimal-0307_2023-05-16_024.jpg",1,list4[oo]))):dim(cell3)[1]])-emptylength-divisionzone		
}else{
	if(orientation[oo]=="right"){
	#going right 
cell3[,8] <-c(cell3[,3]/cos(direction[match(substr(cell3[,1],1,namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2])+ real_location)-emptylength-divisionzone
	}else{
#going left
	cell3[,8] <- c((3826/2.7-cell3[,3])/cos(direction[match(substr(cell3[,1],1,namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2])- real_location)-emptylength-divisionzone
	}
	
}

##plot with three colors

	##plot with three colors
real_location<-rep(0,dim(location)[1])
real_location[1]<-location$X.offset[1]
for (i in 2:dim(location)[1]){
	if(list2[oo]=="Wheat-Z-W-3-control"){
		
	real_location[i]<-sum(abs(location$X.offset[1:i]))
	}else{	
	real_location[i]<-sum((location$X.offset[1:i]))
}
}
real_location <-real_location[match(substr(cell0$Name,1,list4[oo]),substr(location[,2],1,list4[oo]))]
real_location[which(substr(cell0$Name,1,list4[oo])==substr(location[1,1],1,list4[oo]))] <-0

	if(orientation[oo]=="right"){
	#going right 
cell0[,8] <-c(cell0[,3]/cos(direction[match(substr(cell0[,1],1,namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2])+ real_location)-emptylength-divisionzone
	}else{
#going left
	cell0[,8] <- c((3826/2.7-cell0[,3])/cos(direction[match(substr(cell0[,1],1,namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2])- real_location)-emptylength-divisionzone
	}
	


		
		
		

##plot with three colors

	##plot with three colors
real_location<-rep(0,dim(location)[1])
real_location[1]<-location$X.offset[1]
for (i in 2:dim(location)[1]){
	if(list2[oo]=="Wheat-Z-W-3-control"){
		
	real_location[i]<-sum(abs(location$X.offset[1:i]))
	}else{	
	real_location[i]<-sum((location$X.offset[1:i]))
}
}
real_location <-real_location[match(substr(cell2$Name,1,list4[oo]),substr(location[,2],1,list4[oo]))]
real_location[which(substr(cell2$Name,1,list4[oo])==substr(location[1,1],1,list4[oo]))] <-0

	if(list2[oo]=="Wheat-Z-W-3-control"){
cell2[1:min(which(substr(cell2$Name,1,list4[oo])==substr("2021-02-19-tris_minimal-0307_2023-05-16_024.jpg",1,list4[oo]))),8] <- c((3826/2.7-cell2[1:min(which(substr(cell2 $Name,1,list4[oo])==substr("2021-02-19-tris_minimal-0307_2023-05-16_024.jpg",1,list4[oo]))),3])/cos(direction[match(substr(cell2[1:min(which(substr(cell2$Name,1,list4[oo])==substr("2021-02-19-tris_minimal-0307_2023-05-16_024.jpg",1,list4[oo]))),1],1,namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2])+ real_location[1:min(which(substr(cell2$Name,1,list4[oo])==substr("2021-02-19-tris_minimal-0307_2023-05-16_024.jpg",1,list4[oo])))])-emptylength-divisionzone	
cell2[min(which(substr(cell2$Name,1,list4[oo])==substr("2021-02-19-tris_minimal-0307_2023-05-16_024.jpg",1,list4[oo]))):dim(cell2)[1],8] <- c((3826/2.7-cell2[min(which(substr(cell2 $Name,1,list4[oo])==substr("2021-02-19-tris_minimal-0307_2023-05-16_024.jpg",1,list4[oo]))):dim(cell2)[1],3])/cos(direction[match(substr(cell2[min(which(substr(cell2 $Name,1,list4[oo])==substr("2021-02-19-tris_minimal-0307_2023-05-16_024.jpg",1,list4[oo]))):dim(cell2)[1],1],1,namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2])+ real_location[min(which(substr(cell2 $Name,1,list4[oo])==substr("2021-02-19-tris_minimal-0307_2023-05-16_024.jpg",1,list4[oo]))):dim(cell2)[1]])-emptylength-divisionzone	
}else{
	if(orientation[oo]=="right"){
	#going right 
cell2[,8] <-c(cell2[,3]/cos(direction[match(substr(cell2[,1],1,namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2])+ real_location)-emptylength-divisionzone
	}else{
#going left
	cell2[,8] <- c((3826/2.7-cell2[,3])/cos(direction[match(substr(cell2[,1],1,namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2])- real_location)-emptylength-divisionzone
	}
	
}
if(list2[oo]=="zavi_dm1"){
	cell3<-cell3[which(cell3[,8]>10000),]
	
}

cell0<-cell0[which(cell0[,8]<0),]
cell2<-cell2[which(cell2[,8]>0),]

cell0<-na.omit(cell0)

#write.csv(cell3,"~/Downloads/test/pairs_trichome.csv")
cell3<-na.omit(cell3)
cell2<-na.omit(cell2)


window<-(cell3[,8]) %/% step
df <-as.data.frame(cbind(cell3$Distance, window))
cell3_data<-as.matrix(df %>% group_by(window) %>%
  summarize(res=quantile(V1,probs=0.2),number=length(V1)))
cell3_data<-cell3_data[which(cell3_data[,3]> numbercut2),]


window<-(cell0[,8]) %/% step
df <-as.data.frame(cbind(cell0$Width, window))
cell0_data<-as.matrix(df %>% group_by(window) %>%
  summarize(res=quantile(V1,probs=0.5),number=length(V1)))
cell0_data<-cell0_data[which(cell0_data[,3]> numbercut1),]

if(length(cell0_data)==3){
	cell0_data<-rbind(cell0_data, cell0_data)
}

window<-(cell2[,8]) %/% step
df <-as.data.frame(cbind(cell2$Width, window))
cell2_data<-as.matrix(df %>% group_by(window) %>%
  summarize(res=quantile(V1,probs=0.5),number=length(V1)))
cell2_data<-cell2_data[which(cell2_data[,3]>numbercut1),]

 plot(cell2_data[,1]* step, cell2_data[,2],col="#FF6600",ylim=c(0,400),xlim=c(-10000,50000))
points(cell0_data[,1]* step, cell0_data[,2],col="yellow")
points(cell3_data[,1]* step, cell3_data[,2],col="green")


if(list1[oo]%in%list5){
cell8 <-read.csv(paste0("~/Desktop/handmark/",list1[oo],"/pairs.csv"), header=T)
head(cell8)
cell8[,3]<-cell8[,3]/2.7
cell8[,4]<-cell8[,4]/2.7
cell8[,5]<-cell8[,5]/2.7
cell8[,6]<-cell8[,6]/2.7
cell8[,7]<-cell8[,7]/2.7

real_location<-rep(0,dim(location)[1])
real_location[1]<-location$X.offset[1]
for (i in 2:dim(location)[1]){
	real_location[i]<-sum(location$X.offset[1:i])
	
}
real_location <-real_location[match(substr(cell8$Name,1,list4[oo]),substr(location[,2],1,list4[oo]))]
real_location[which(substr(cell8$Name,1,list4[oo])==substr(location[1,1],1,list4[oo]))] <-0
if(orientation[oo]=="right"){
	#going right 
cell8[,8] <-c(cell8[,3]/cos(direction[match(substr(cell8[,1],1,list4[oo]),substr(direction[,1],1,list4[oo]) ),2])+ real_location)-emptylength-divisionzone
	}else{
#going left
	cell8[,8] <- c((3826/2.7-cell8[,3])/cos(direction[match(substr(cell8[,1],1,list4[oo]),substr(direction[,1],1,list4[oo]) ),2])- real_location)-emptylength-divisionzone
	}
#write.csv(cell3,"~/Downloads/test/pairs_trichome.csv")


cell8<-na.omit(cell8)
window<-(cell8[,8]) %/% step
df <-as.data.frame(cbind(cell8$Distance, window))
cell8_data<-as.matrix(df %>% group_by(window) %>%
  summarize(res=quantile(V1,probs=0.2),number=length(V1)))
cell8_data<-cell8_data[which(cell8_data[,3]> 3),]

points(cell8_data[,1]* step, cell8_data[,2],col="purple")
}else{
	cell8_data<-cell3_data[1:2,]
	}


testdata<-rbind(cell2_data,cell3_data)
testdata<-testdata[order(testdata[,1]),]
testdataoriginal<-cell3
for(i in 1:(dim(testdata)[1]-1)){
if((testdata[i+1,1]-testdata[i,1])>3){
	print(c(testdata[i+1,1]* step,testdata[i,1]*step, step*(testdata[i+1,1]-testdata[i,1])))
	print(plyr::count(testdataoriginal[which((testdataoriginal[,8]<testdata[i+1,1]*step)&(testdataoriginal[,8]>testdata[i,1]*step)),1]))
}
}
print(list1[oo])


  #plot(cell3_data[,1]* step, cell3_data[,2],col="#FF6600",ylim=c(0,200))
#points(cell2_data[,1]* step, cell2_data[,2],col="green")
#points(cell1_data[,1]* step, cell1_data[,2],col="black")
#points(cell3_data[problem,1]* step, cell3_data[problem,2],col="green")

print(list1[oo])





print(list1[oo])

divisioncellsize<-c(divisioncellsize ,mean(cell0[which(cell0[,8]<0),5]))
divisionzonelist<-c(divisionzonelist, divisionzone)
matrix1 [match(c(cell0_data[,1]* step),step*c(-100:500)),oo]<-c( cell0_data[,2])
matrix2[match(c(cell3_data[,1]* step), step*c(-30:500)),oo]<-c( cell3_data[,2])
matrix3[match(c(cell2_data[,1]* step),step*c(-30:500)),oo]<-c( cell2_data[,2])
matrix8[match(c(cell8_data[,1]* step),step*c(-30:500)),oo]<-c( cell8_data[,2])
matrix1[1:(min(match(c(cell0_data[,1]* step), step*c(-100:300)))-5),oo]<-mean(cell0[which(cell0[,8]<0),5])

}
dev.off()

divisionzonelist/divisioncellsize
matrix<-rbind(matrix1, matrix2 ,matrix3, matrix8 )


matrix<-cbind(c(step*c(-100:500),step*c(-30:500),step*c(-30:500),step*c(-30:500)), matrix)
colnames(matrix)<-c("location", list2)
matrix[which(matrix==0)]<-NA
#write.csv(matrix,"~/Downloads/matrixwheatzavitan.csv", row.names=FALSE)
#write.csv(matrix,"~/Downloads/matrixwheatcs.csv", row.names=FALSE)
write.csv(matrix,"~/Downloads/matrixwheatL.csv", row.names=FALSE)

 list2<-c("Wheat-L-D-1-control" ,"Wheat-L-D-1-drought", "Wheat-L-D-2-control", "Wheat-L-D-2-drought" ,"Wheat-L-D-3-control",  "Wheat-L-D-3-drought", "Wheat-L-D-4-control" , "Wheat-L-D-4-drought" )

  pdf(paste0("~/Downloads/wheatl_8.pdf"), width=10, height = 20)
par(mfrow = c(6, 2))


parameters <-read.csv(paste0("~/Downloads/matrixwheatL_out.csv"), header=T)
name <-read.csv(paste0("~/Downloads/matrixwheatL.csv"), header=T)

  colnames(name)<-c("location",list2)
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
res_max<-0 
 return(-res_max)
} 

a="control"
aa="drought"
bb=9
listname<-c("Wheat-L-D","Wheat-L-D")

max<-max(name[,which(substr(colnames(name),1,bb)%in% listname)], na.rm=T)
xmax<-max(parameters[,2], na.rm=T)*7
ymatrix1<-ymatrix2<-NULL
for(i in which(substr(colnames(name),bb+4,bb+10)==a)){
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
x1=Find_Max_CCF(y0,y1)+x
print(Find_Max_CCF(y0,y1))
  	plot( x1, y1, type="l", col="#66CC66",ylim=c(0,400),xlim=c(0,50000) ,xlab="distance(um)", ylab="Cell length (um)" )
points(name[,1],name[,i+1])

ymatrix1<-c(ymatrix1,y1)
}
for(i in which(substr(colnames(name),bb+4,bb+10)==aa)){
	
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
x1=Find_Max_CCF(y0,y1)+x
print(Find_Max_CCF(y0,y1))

  	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,400),xlim=c(0,50000),xlab="distance(um)", ylab="Cell length (um)" )
points(name[,1],name[,i+1])

ymatrix2<-c(ymatrix2,y1)
}

ss<-0
ymatrix1<-ymatrix2<-NULL
sslist<-NULL
for(i in which(substr(colnames(name),bb+4,bb+10)==a)){
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
x1=Find_Max_CCF(y0,y1)+x
print(Find_Max_CCF(y0,y1))
  	plot( x1, y1, type="l", col="#66CC66",ylim=c(0,400),xlim=c(0,xmax) ,xlab="distance(um)", ylab="Cell length (um)" )
par(new=TRUE)	
ymatrix1<-c(ymatrix1,y1)
ss=ss+1
sslist<-c(sslist,rep(ss,length(y1)))

}
for(i in which(substr(colnames(name),bb+4,bb+10)==aa)){
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
x1=Find_Max_CCF(y0,y1)+x
print(Find_Max_CCF(y0,y1))

  	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,400),xlim=c(0,xmax),xlab="distance(um)", ylab="Cell length (um)" )
par(new=TRUE)	
ymatrix2<-c(ymatrix2,y1)
ss=ss+1
sslist<-c(sslist,rep(ss,length(y1)))
}
 	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,400),xlim=c(0,xmax),xlab="distance(um)", ylab="Cell length (um)" )


library(ggplot2)

all_data_bd21_plot<-cbind(rep(x,length(which(substr(colnames(name),1,bb)%in% listname))),c(ymatrix1, ymatrix2),sslist,c(rep("Control",length(ymatrix1)),rep("Drought",length(ymatrix2))))

all_data_bd21_plot<-all_data_bd21_plot[which(all_data_bd21_plot[,1]>0),]



colnames(all_data_bd21_plot)<-c("location","size","sample","treatment")
write.csv(as.matrix(all_data_bd21_plot),"~/Downloads/tmp.csv")
all_data_bd21_plot <-read.csv(paste0("~/Downloads/tmp.csv"), header=T,row.names=1)
library(ggplot2)
library(reshape2)

all_data_bd21_plot$treatment<-as.factor(all_data_bd21_plot$treatment)
hhhhhh<-ggplot(all_data_bd21_plot,aes(location,size))+
  theme_classic()+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = treatment))+
  labs(x = expression(paste("Distance (", mu, "m)")),y = expression(paste("Cell length (", mu, "m)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",size = 1,aes(fill = treatment),alpha = 0.3)+
  guides(fill = "none")+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = treatment))+
  labs(x = expression(paste("Distance (", mu, "m)")),y = expression(paste("Cell length (", mu, "m)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  geom_line(aes(group = interaction(sample, treatment),colour = treatment,alpha = 0.2))+
  guides(alpha= "none")+ scale_color_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+scale_fill_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+ ggtitle(
"Wheat-L-Domesticated")
 #hhhhhh
  dev.off()
  
list4<-substr(list2,bb+4,bb+10)

write.csv(cbind(list2,rep(listname[1],length(list2)),list4,divisionzonelist, divisioncellsize, parameters),"~/Downloads/matrixL_out2.csv")



 list2<-c("Wheat-L-D-1-control" ,"Wheat-L-D-1-drought", "Wheat-L-D-2-control", "Wheat-L-D-2-drought" ,"Wheat-L-D-3-control",  "Wheat-L-D-3-drought", "Wheat-L-D-4-control" , "Wheat-L-D-4-drought" )

  pdf(paste0("~/Downloads/wheatl_9.pdf"), width=10, height = 20)
par(mfrow = c(6, 2))


parameters <-read.csv(paste0("~/Downloads/matrixwheatL_out.csv"), header=T)
name <-read.csv(paste0("~/Downloads/matrixwheatL.csv"), header=T)

  colnames(name)<-c("location",list2)
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
res_max<-0 
 return(-res_max)
} 
elongationrate_c<-1.2042611
elongationrate_d<-0.5052139

a="control"
aa="drought"
bb=9
listname<-c("Wheat-L-D","Wheat-L-D")

max<-max(name[,which(substr(colnames(name),1,bb)%in% listname)], na.rm=T)
xmax<-max(parameters[,2], na.rm=T)*7
ymatrix1<-ymatrix2<-xmatrix1<-xmatrix2<-NULL
ss<-0
ymatrix1<-ymatrix2<-NULL
sslist<-NULL
for(i in which(substr(colnames(name),bb+4,bb+10)==a)){
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
print(Find_Max_CCF(y0,y1))
  	plot( x1, y1, type="l", col="#66CC66",ylim=c(0,400),xlim=c(0,xmax) ,xlab="distance(um)", ylab="Cell length (um)" )
par(new=TRUE)	
ymatrix1<-c(ymatrix1,y1)
ss=ss+1
sslist<-c(sslist,rep(ss,length(y1)))

}
for(i in which(substr(colnames(name),bb+4,bb+10)==aa)){
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
  y1 = L / (1 + exp(-k*(x-x0))) + b
x1=x
rootlist<-NULL
for (ismall in 1:length(x1)){
g <- Vectorize(function(x) parameters[i,5]/elongationrate_d*(integrate(function(t) 1/( L / (1 + exp(-k*(t-x0))) + b), lower = 0, upper = x)$value)-x1[ismall])
root <- uniroot(g,c(-10000,200000))$root
rootlist<-c(rootlist,root)
}

  y1 = L / (1 + exp(-k*(rootlist-x0))) + b

xmatrix2<-c(xmatrix2,x1)
print(Find_Max_CCF(y0,y1))
  	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,400),xlim=c(0,xmax) ,xlab="distance(um)", ylab="Cell length (um)" )
par(new=TRUE)	
ymatrix2<-c(ymatrix2,y1)
ss=ss+1
sslist<-c(sslist,rep(ss,length(y1)))
}
 	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,400),xlim=c(0,xmax),xlab="distance(um)", ylab="Cell length (um)" )


library(ggplot2)

all_data_bd21_plot<-cbind(c(xmatrix1, xmatrix2)/1000,c(ymatrix1, ymatrix2),sslist,c(rep("Control",length(ymatrix1)),rep("Drought",length(ymatrix2))))

all_data_bd21_plot<-all_data_bd21_plot[which(all_data_bd21_plot[,1]>0),]



colnames(all_data_bd21_plot)<-c("location","size","sample","treatment")
write.csv(as.matrix(all_data_bd21_plot),"~/Downloads/tmp.csv")
all_data_bd21_plot <-read.csv(paste0("~/Downloads/tmp.csv"), header=T,row.names=1)
library(ggplot2)
library(reshape2)

all_data_bd21_plot$treatment<-as.factor(all_data_bd21_plot$treatment)
ffffff2<-ggplot(all_data_bd21_plot,aes(location,size))+
  theme_classic()+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = treatment))+
  labs(x = "Thermal time (°Cd)",y = expression(paste("Cell length (", mu, "m)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",size = 1,aes(fill = treatment),alpha = 0.3)+
  guides(fill = "none")+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = treatment))+
  labs(x = "Thermal time (°Cd)",y = expression(paste("Cell length (", mu, "m)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  geom_line(aes(group = interaction(sample, treatment),colour = treatment,alpha = 0.2))+
  guides(alpha= "none")+ scale_color_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+scale_fill_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+ ggtitle(
"Wheat-L-Domesticated")
# aaaaaa
  dev.off()
  
list4<-substr(list2,bb+4,bb+10)






list1<-c("Wheat CS D 1 control" ,"Wheat CS D 1 drought" ,"Wheat CS D 2 control" ,"Wheat CS D 2 control extra",  "Wheat CS D 2 drought", "Wheat CS D 3 control", "Wheat CS D 3 drought"  ,"Wheat CS D 4 control" , "Wheat CS D 4 drought")
list2<-c("Wheat-CS-D-1-control" ,"Wheat-CS-D-1-drought" ,"Wheat-CS-D-2-control" ,"Wheat-CS-D-2-control-extra",  "Wheat-CS-D-2-drought", "Wheat-CS-D-3-control", "Wheat-CS-D-3-drought"  ,"Wheat-CS-D-4-control" , "Wheat-CS-D-4-drought")
list5=c( "Wheat CS D 2 drought", "Wheat CS D 3 control",  "Wheat CS D 3 drought" , "Wheat CS D 4 drought" , "Wheat CS D 1 control" , "Wheat CS D 1 drought")
empty<-c(1300,679,925,550,580,787,737,0,626)
orientation<-c("left","left","left","right","left","left","right","left","left")
namelength <-rep(43,length(list1))
list4<-namelength
matrix1<-matrix3<-matrix2<-matrix8<-matrix(0,531, length(list1))
matrix1<-matrix(0,601, length(list1))
divisioncellsize <-divisionzonelist<-NULL
 pdf(paste0("~/Downloads/wheatcs_10.pdf"), width=7, height = 4)


 for(oo in c(1:length(list1))){
cell2_data<-cell3_data<-cell0_data<-cell4_data<-cell2<-cell3<-cell0<-cell4<-cell8<-cell8_data<-NULL	
step<-200
numbercut1=3
numbercut2=10
#handmark
numbercut3=2

cell3 <-read.csv(paste0("/Volumes/TOSHIBAEXT/image_results/trichome_results/",list2[oo],"/pairs.csv"), header=T)
head(cell3)
cell3[,3]<-cell3[,3]/2.7
cell3[,4]<-cell3[,4]/2.7
cell3[,5]<-cell3[,5]/2.7
cell3[,6]<-cell3[,6]/2.7
cell3[,7]<-cell3[,7]/2.7

location <-read.csv(paste0("/Volumes/TOSHIBAEXT/images/leaf\ extension\ 2/",list1[oo],"/stitching_result.csv"), header=T)
dim(location)
head(location)
location[,3]<-location[,3]/2.7
location[,4]<-location[,4]/2.7



direction <-read.csv(paste0("/Volumes/TOSHIBAEXT/images/leaf\ extension\ 2/",list1[oo],"/dirs.csv"), header=T)
dim(direction)
location[,3]<-location[,3]/cos(direction[match(substr(location[,1],1,namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2]) 
emptylength<-empty[oo]/cos(direction[1,2])/2.7


cell2 <-read.csv(paste0("/Volumes/TOSHIBAEXT/image_results/sister_results/",list2[oo],"/cells.csv"), header=T)
head(cell2)
cell2[,3]<-cell2[,3]/2.7
cell2[,4]<-cell2[,4]/2.7
cell2[,5]<-cell2[,5]/2.7
cell2[,6]<-cell2[,6]/2.7
head(cell3)
cell2[,5]<-rowMaxs(cbind(cell2[,5], cell2[,6]))
head(cell3)
##base cells /Volumes/TOSHIBAEXT/image_results/newbase_results/Bd21-4-control/cells.csv
cell0 <-read.csv(paste0("/Volumes/TOSHIBAEXT/image_results/basenew/",list2[oo],"/cells.csv"), header=T)
head(cell0)
cell0[,3]<-cell0[,3]/2.7
cell0[,4]<-cell0[,4]/2.7
cell0[,5]<-rowMaxs(cbind(cell0[,5]/2.7, cell0[,6]/2.7))
cell4 <-read.csv(paste0("~/Desktop/divisionzonemark/",list2[oo],"/cells.csv"), header=T)
head(cell4)
cell4[,3]<-cell4[,3]/2.7
cell4[,4]<-cell4[,4]/2.7
##plot with three colors
real_location<-rep(0,dim(location)[1])
real_location[1]<-location$X.offset[1]
for (i in 2:dim(location)[1]){
	real_location[i]<-sum(location$X.offset[1:i])
}
real_location <-real_location[match(substr(cell4$Name,1,namelength[oo]),substr(location[,2],1,namelength[oo]))]
real_location[which(substr(cell4$Name,1,namelength[oo])==substr(location[1,1],1,namelength[oo]))] <-0
if(orientation[oo]=="right"){
	#going right 
cell4[,8] <-c(cell4[,3]/cos(direction[match(substr(cell4[,1],1, namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2])+ real_location)-emptylength
	}else{
#going left
	cell4[,8] <- c((3826/2.7-cell4[,3])/cos(direction[match(substr(cell4[,1],1,namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2])- real_location)-emptylength
	}

divisionzone<-min(cell4[,8])

##plot with three colors

	##plot with three colors
real_location<-rep(0,dim(location)[1])
real_location[1]<-location$X.offset[1]
for (i in 2:dim(location)[1]){
	if(list2[oo]=="Wheat-Z-W-3-control"){
		
	real_location[i]<-sum(abs(location$X.offset[1:i]))
	}else{	
	real_location[i]<-sum((location$X.offset[1:i]))
}
}
real_location <-real_location[match(substr(cell3$Name,1,list4[oo]),substr(location[,2],1,list4[oo]))]
real_location[which(substr(cell3$Name,1,list4[oo])==substr(location[1,1],1,list4[oo]))] <-0

	if(list2[oo]=="Wheat-Z-W-3-control"){
cell3[1:min(which(substr(cell3 $Name,1,list4[oo])==substr("2021-02-19-tris_minimal-0307_2023-05-16_024.jpg",1,list4[oo]))),8] <- c((3826/2.7-cell3[1:min(which(substr(cell3 $Name,1,list4[oo])==substr("2021-02-19-tris_minimal-0307_2023-05-16_024.jpg",1,list4[oo]))),3])/cos(direction[match(substr(cell3[1:min(which(substr(cell3 $Name,1,list4[oo])==substr("2021-02-19-tris_minimal-0307_2023-05-16_024.jpg",1,list4[oo]))),1],1,namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2])+ real_location[1:min(which(substr(cell3 $Name,1,list4[oo])==substr("2021-02-19-tris_minimal-0307_2023-05-16_024.jpg",1,list4[oo])))])-emptylength-divisionzone	
cell3[min(which(substr(cell3 $Name,1,list4[oo])==substr("2021-02-19-tris_minimal-0307_2023-05-16_024.jpg",1,list4[oo]))):dim(cell3)[1],8] <- c((3826/2.7-cell3[min(which(substr(cell3 $Name,1,list4[oo])==substr("2021-02-19-tris_minimal-0307_2023-05-16_024.jpg",1,list4[oo]))):dim(cell3)[1],3])/cos(direction[match(substr(cell3[min(which(substr(cell3 $Name,1,list4[oo])==substr("2021-02-19-tris_minimal-0307_2023-05-16_024.jpg",1,list4[oo]))):dim(cell3)[1],1],1,namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2])+ real_location[min(which(substr(cell3 $Name,1,list4[oo])==substr("2021-02-19-tris_minimal-0307_2023-05-16_024.jpg",1,list4[oo]))):dim(cell3)[1]])-emptylength-divisionzone		
}else{
	if(orientation[oo]=="right"){
	#going right 
cell3[,8] <-c(cell3[,3]/cos(direction[match(substr(cell3[,1],1,namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2])+ real_location)-emptylength-divisionzone
	}else{
#going left
	cell3[,8] <- c((3826/2.7-cell3[,3])/cos(direction[match(substr(cell3[,1],1,namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2])- real_location)-emptylength-divisionzone
	}
	
}

##plot with three colors

	##plot with three colors
real_location<-rep(0,dim(location)[1])
real_location[1]<-location$X.offset[1]
for (i in 2:dim(location)[1]){
	if(list2[oo]=="Wheat-Z-W-3-control"){
		
	real_location[i]<-sum(abs(location$X.offset[1:i]))
	}else{	
	real_location[i]<-sum((location$X.offset[1:i]))
}
}
real_location <-real_location[match(substr(cell0$Name,1,list4[oo]),substr(location[,2],1,list4[oo]))]
real_location[which(substr(cell0$Name,1,list4[oo])==substr(location[1,1],1,list4[oo]))] <-0

	if(orientation[oo]=="right"){
	#going right 
cell0[,8] <-c(cell0[,3]/cos(direction[match(substr(cell0[,1],1,namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2])+ real_location)-emptylength-divisionzone
	}else{
#going left
	cell0[,8] <- c((3826/2.7-cell0[,3])/cos(direction[match(substr(cell0[,1],1,namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2])- real_location)-emptylength-divisionzone
	}
	


		
		
		

##plot with three colors

	##plot with three colors
real_location<-rep(0,dim(location)[1])
real_location[1]<-location$X.offset[1]
for (i in 2:dim(location)[1]){
	if(list2[oo]=="Wheat-Z-W-3-control"){
		
	real_location[i]<-sum(abs(location$X.offset[1:i]))
	}else{	
	real_location[i]<-sum((location$X.offset[1:i]))
}
}
real_location <-real_location[match(substr(cell2$Name,1,list4[oo]),substr(location[,2],1,list4[oo]))]
real_location[which(substr(cell2$Name,1,list4[oo])==substr(location[1,1],1,list4[oo]))] <-0

	if(list2[oo]=="Wheat-Z-W-3-control"){
cell2[1:min(which(substr(cell2$Name,1,list4[oo])==substr("2021-02-19-tris_minimal-0307_2023-05-16_024.jpg",1,list4[oo]))),8] <- c((3826/2.7-cell2[1:min(which(substr(cell2 $Name,1,list4[oo])==substr("2021-02-19-tris_minimal-0307_2023-05-16_024.jpg",1,list4[oo]))),3])/cos(direction[match(substr(cell2[1:min(which(substr(cell2$Name,1,list4[oo])==substr("2021-02-19-tris_minimal-0307_2023-05-16_024.jpg",1,list4[oo]))),1],1,namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2])+ real_location[1:min(which(substr(cell2$Name,1,list4[oo])==substr("2021-02-19-tris_minimal-0307_2023-05-16_024.jpg",1,list4[oo])))])-emptylength-divisionzone	
cell2[min(which(substr(cell2$Name,1,list4[oo])==substr("2021-02-19-tris_minimal-0307_2023-05-16_024.jpg",1,list4[oo]))):dim(cell2)[1],8] <- c((3826/2.7-cell2[min(which(substr(cell2 $Name,1,list4[oo])==substr("2021-02-19-tris_minimal-0307_2023-05-16_024.jpg",1,list4[oo]))):dim(cell2)[1],3])/cos(direction[match(substr(cell2[min(which(substr(cell2 $Name,1,list4[oo])==substr("2021-02-19-tris_minimal-0307_2023-05-16_024.jpg",1,list4[oo]))):dim(cell2)[1],1],1,namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2])+ real_location[min(which(substr(cell2 $Name,1,list4[oo])==substr("2021-02-19-tris_minimal-0307_2023-05-16_024.jpg",1,list4[oo]))):dim(cell2)[1]])-emptylength-divisionzone	
}else{
	if(orientation[oo]=="right"){
	#going right 
cell2[,8] <-c(cell2[,3]/cos(direction[match(substr(cell2[,1],1,namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2])+ real_location)-emptylength-divisionzone
	}else{
#going left
	cell2[,8] <- c((3826/2.7-cell2[,3])/cos(direction[match(substr(cell2[,1],1,namelength[oo]),substr(direction[,1],1,namelength[oo]) ),2])- real_location)-emptylength-divisionzone
	}
	
}
if(list2[oo]=="zavi_dm1"){
	cell3<-cell3[which(cell3[,8]>10000),]
	
}

cell0<-cell0[which(cell0[,8]<0),]
cell2<-cell2[which(cell2[,8]>0),]

cell0<-na.omit(cell0)

#write.csv(cell3,"~/Downloads/test/pairs_trichome.csv")
cell3<-na.omit(cell3)
cell2<-na.omit(cell2)


window<-(cell3[,8]) %/% step
df <-as.data.frame(cbind(cell3$Distance, window))
cell3_data<-as.matrix(df %>% group_by(window) %>%
  summarize(res=quantile(V1,probs=0.2),number=length(V1)))
cell3_data<-cell3_data[which(cell3_data[,3]> numbercut2),]


window<-(cell0[,8]) %/% step
df <-as.data.frame(cbind(cell0$Width, window))
cell0_data<-as.matrix(df %>% group_by(window) %>%
  summarize(res=quantile(V1,probs=0.5),number=length(V1)))
cell0_data<-cell0_data[which(cell0_data[,3]> numbercut1),]

if(length(cell0_data)==3){
	cell0_data<-rbind(cell0_data, cell0_data)
}

window<-(cell2[,8]) %/% step
df <-as.data.frame(cbind(cell2$Width, window))
cell2_data<-as.matrix(df %>% group_by(window) %>%
  summarize(res=quantile(V1,probs=0.5),number=length(V1)))
cell2_data<-cell2_data[which(cell2_data[,3]>numbercut1),]

 plot(cell2_data[,1]* step, cell2_data[,2],col="#FF6600",ylim=c(0,400),xlim=c(-10000,50000))
points(cell0_data[,1]* step, cell0_data[,2],col="yellow")
points(cell3_data[,1]* step, cell3_data[,2],col="green")


if(list1[oo]%in%list5){
cell8 <-read.csv(paste0("~/Desktop/handmark/",list1[oo],"/pairs.csv"), header=T)
head(cell8)
cell8[,3]<-cell8[,3]/2.7
cell8[,4]<-cell8[,4]/2.7
cell8[,5]<-cell8[,5]/2.7
cell8[,6]<-cell8[,6]/2.7
cell8[,7]<-cell8[,7]/2.7

real_location<-rep(0,dim(location)[1])
real_location[1]<-location$X.offset[1]
for (i in 2:dim(location)[1]){
	real_location[i]<-sum(location$X.offset[1:i])
	
}
real_location <-real_location[match(substr(cell8$Name,1,list4[oo]),substr(location[,2],1,list4[oo]))]
real_location[which(substr(cell8$Name,1,list4[oo])==substr(location[1,1],1,list4[oo]))] <-0
if(orientation[oo]=="right"){
	#going right 
cell8[,8] <-c(cell8[,3]/cos(direction[match(substr(cell8[,1],1,list4[oo]),substr(direction[,1],1,list4[oo]) ),2])+ real_location)-emptylength-divisionzone
	}else{
#going left
	cell8[,8] <- c((3826/2.7-cell8[,3])/cos(direction[match(substr(cell8[,1],1,list4[oo]),substr(direction[,1],1,list4[oo]) ),2])- real_location)-emptylength-divisionzone
	}
#write.csv(cell3,"~/Downloads/test/pairs_trichome.csv")


cell8<-na.omit(cell8)
window<-(cell8[,8]) %/% step
df <-as.data.frame(cbind(cell8$Distance, window))
cell8_data<-as.matrix(df %>% group_by(window) %>%
  summarize(res=quantile(V1,probs=0.2),number=length(V1)))
cell8_data<-cell8_data[which(cell8_data[,3]> 3),]

points(cell8_data[,1]* step, cell8_data[,2],col="purple")
}else{
	cell8_data<-cell3_data[1:2,]
	}


testdata<-rbind(cell2_data,cell3_data)
testdata<-testdata[order(testdata[,1]),]
testdataoriginal<-cell3
for(i in 1:(dim(testdata)[1]-1)){
if((testdata[i+1,1]-testdata[i,1])>3){
	print(c(testdata[i+1,1]* step,testdata[i,1]*step, step*(testdata[i+1,1]-testdata[i,1])))
	print(plyr::count(testdataoriginal[which((testdataoriginal[,8]<testdata[i+1,1]*step)&(testdataoriginal[,8]>testdata[i,1]*step)),1]))
}
}
print(list1[oo])


  #plot(cell3_data[,1]* step, cell3_data[,2],col="#FF6600",ylim=c(0,200))
#points(cell2_data[,1]* step, cell2_data[,2],col="green")
#points(cell1_data[,1]* step, cell1_data[,2],col="black")
#points(cell3_data[problem,1]* step, cell3_data[problem,2],col="green")

print(list1[oo])





print(list1[oo])

divisioncellsize<-c(divisioncellsize ,mean(cell0[which(cell0[,8]<0),5]))
divisionzonelist<-c(divisionzonelist, divisionzone)
matrix1 [match(c(cell0_data[,1]* step),step*c(-100:500)),oo]<-c( cell0_data[,2])
matrix2[match(c(cell3_data[,1]* step), step*c(-30:500)),oo]<-c( cell3_data[,2])
matrix3[match(c(cell2_data[,1]* step),step*c(-30:500)),oo]<-c( cell2_data[,2])
matrix8[match(c(cell8_data[,1]* step),step*c(-30:500)),oo]<-c( cell8_data[,2])
matrix1[1:(min(match(c(cell0_data[,1]* step), step*c(-100:300)))-5),oo]<-mean(cell0[which(cell0[,8]<0),5])

}
dev.off()

divisionzonelist/divisioncellsize
matrix<-rbind(matrix1, matrix2 ,matrix3, matrix8 )


matrix<-cbind(c(step*c(-100:500),step*c(-30:500),step*c(-30:500),step*c(-30:500)), matrix)
colnames(matrix)<-c("location", list2)
matrix[which(matrix==0)]<-NA
#write.csv(matrix,"~/Downloads/matrixwheatzavitan.csv", row.names=FALSE)
write.csv(matrix,"~/Downloads/matrixwheatcs.csv", row.names=FALSE)

 

list2<-c("Wheat-CS-D-1-control" ,"Wheat-CS-D-1-drought" ,"Wheat-CS-D-2-control" ,"Wheat-CS-D-2-control-extra",  "Wheat-CS-D-2-drought", "Wheat-CS-D-3-control", "Wheat-CS-D-3-drought"  ,"Wheat-CS-D-4-control" , "Wheat-CS-D-4-drought")

  pdf(paste0("~/Downloads/wheatcs_8.pdf"), width=10, height = 20)
par(mfrow = c(6, 2))

parameters <-read.csv(paste0("~/Downloads/matrixwheatcs_out.csv"), header=T)
name <-read.csv(paste0("~/Downloads/matrixwheatcs.csv"), header=T)

  colnames(name)<-c("location",list2)
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
res_max<-0 
 return(-res_max)
} 

a="control"
aa="drought"
bb=10
listname<-c("Wheat-CS-D","Wheat-CS-D")

max<-max(name[,which(substr(colnames(name),1,bb)%in% listname)], na.rm=T)
xmax<-max(parameters[,2], na.rm=T)*7
ymatrix1<-ymatrix2<-NULL
for(i in which(substr(colnames(name),bb+4,bb+10)==a)){
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
x1=Find_Max_CCF(y0,y1)+x
print(Find_Max_CCF(y0,y1))
  	plot( x1, y1, type="l", col="#66CC66",ylim=c(0,400),xlim=c(0,50000) ,xlab="distance(um)", ylab="Cell length (um)" )
points(name[,1],name[,i+1])

ymatrix1<-c(ymatrix1,y1)
}
for(i in which(substr(colnames(name),bb+4,bb+10)==aa)){
	
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
x1=Find_Max_CCF(y0,y1)+x
print(Find_Max_CCF(y0,y1))

  	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,400),xlim=c(0,50000),xlab="distance(um)", ylab="Cell length (um)" )
points(name[,1],name[,i+1])

ymatrix2<-c(ymatrix2,y1)
}

ss<-0
ymatrix1<-ymatrix2<-NULL
sslist<-NULL
for(i in which(substr(colnames(name),bb+4,bb+10)==a)){
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
x1=Find_Max_CCF(y0,y1)+x
print(Find_Max_CCF(y0,y1))
  	plot( x1, y1, type="l", col="#66CC66",ylim=c(0,400),xlim=c(0,xmax) ,xlab="distance(um)", ylab="Cell length (um)" )
par(new=TRUE)	
ymatrix1<-c(ymatrix1,y1)
ss=ss+1
sslist<-c(sslist,rep(ss,length(y1)))

}
for(i in which(substr(colnames(name),bb+4,bb+10)==aa)){
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
x1=Find_Max_CCF(y0,y1)+x
print(Find_Max_CCF(y0,y1))

  	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,400),xlim=c(0,xmax),xlab="distance(um)", ylab="Cell length (um)" )
par(new=TRUE)	
ymatrix2<-c(ymatrix2,y1)
ss=ss+1
sslist<-c(sslist,rep(ss,length(y1)))
}
 	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,400),xlim=c(0,xmax),xlab="distance(um)", ylab="Cell length (um)" )


library(ggplot2)

all_data_bd21_plot<-cbind(rep(x,length(which(substr(colnames(name),1,bb)%in% listname))),c(ymatrix1, ymatrix2),sslist,c(rep("Control",length(ymatrix1)),rep("Drought",length(ymatrix2))))

all_data_bd21_plot<-all_data_bd21_plot[which(all_data_bd21_plot[,1]>0),]



colnames(all_data_bd21_plot)<-c("location","size","sample","treatment")
write.csv(as.matrix(all_data_bd21_plot),"~/Downloads/tmp.csv")
all_data_bd21_plot <-read.csv(paste0("~/Downloads/tmp.csv"), header=T,row.names=1)
library(ggplot2)
library(reshape2)

all_data_bd21_plot$treatment<-as.factor(all_data_bd21_plot$treatment)
gggggg<-ggplot(all_data_bd21_plot,aes(location,size))+
  theme_classic()+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = treatment))+
  labs(x = expression(paste("Distance (", mu, "m)")),y = expression(paste("Cell length (", mu, "m)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",size = 1,aes(fill = treatment),alpha = 0.3)+
  guides(fill = "none")+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = treatment))+
  labs(x = expression(paste("Distance (", mu, "m)")),y = expression(paste("Cell length (", mu, "m)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  geom_line(aes(group = interaction(sample, treatment),colour = treatment,alpha = 0.2))+
  guides(alpha= "none")+ scale_color_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+scale_fill_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+ ggtitle(
"Wheat-CS-Domesticated")
 #gggggg
  dev.off()
  
list4<-substr(list2,bb+4,bb+10)

write.csv(cbind(list2,rep(listname[1],length(list2)),list4,divisionzonelist, divisioncellsize, parameters),"~/Downloads/matrixcs_out2.csv")

list2<-c("Wheat-CS-D-1-control" ,"Wheat-CS-D-1-drought" ,"Wheat-CS-D-2-control" ,"Wheat-CS-D-2-control-extra",  "Wheat-CS-D-2-drought", "Wheat-CS-D-3-control", "Wheat-CS-D-3-drought"  ,"Wheat-CS-D-4-control" , "Wheat-CS-D-4-drought")

  pdf(paste0("~/Downloads/wheatcs_9.pdf"), width=10, height = 20)
par(mfrow = c(6, 2))

parameters <-read.csv(paste0("~/Downloads/matrixwheatcs_out.csv"), header=T)
name <-read.csv(paste0("~/Downloads/matrixwheatcs.csv"), header=T)

  colnames(name)<-c("location",list2)
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
res_max<-0 
 return(-res_max)
} 

a="control"
aa="drought"
bb=10
listname<-c("Wheat-CS-D","Wheat-CS-D")
  colnames(name)<-c("location",list2)
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
res_max<-0 
 return(-res_max)
} 
elongationrate_c<-1.0095155
elongationrate_d<-0.4312706


max<-max(name[,which(substr(colnames(name),1,bb)%in% listname)], na.rm=T)
xmax<-max(parameters[,2], na.rm=T)*7
ymatrix1<-ymatrix2<-xmatrix1<-xmatrix2<-NULL
ss<-0
ymatrix1<-ymatrix2<-NULL
sslist<-NULL
for(i in which(substr(colnames(name),bb+4,bb+10)==a)){
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
print(Find_Max_CCF(y0,y1))
  	plot( x1, y1, type="l", col="#66CC66",ylim=c(0,400),xlim=c(0,xmax) ,xlab="distance(um)", ylab="Cell length (um)" )
par(new=TRUE)	
ymatrix1<-c(ymatrix1,y1)
ss=ss+1
sslist<-c(sslist,rep(ss,length(y1)))

}
for(i in which(substr(colnames(name),bb+4,bb+10)==aa)){
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
  y1 = L / (1 + exp(-k*(x-x0))) + b
x1=x
rootlist<-NULL
for (ismall in 1:length(x1)){
g <- Vectorize(function(x) parameters[i,5]/elongationrate_d*(integrate(function(t) 1/( L / (1 + exp(-k*(t-x0))) + b), lower = 0, upper = x)$value)-x1[ismall])
root <- uniroot(g,c(-10000,200000))$root
rootlist<-c(rootlist,root)
}

  y1 = L / (1 + exp(-k*(rootlist-x0))) + b

xmatrix2<-c(xmatrix2,x1)
print(Find_Max_CCF(y0,y1))
  	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,400),xlim=c(0,xmax) ,xlab="distance(um)", ylab="Cell length (um)" )
par(new=TRUE)	
ymatrix2<-c(ymatrix2,y1)
ss=ss+1
sslist<-c(sslist,rep(ss,length(y1)))
}
 	plot( x1, y1, type="l", col="#FF6600",ylim=c(0,400),xlim=c(0,xmax),xlab="distance(um)", ylab="Cell length (um)" )


library(ggplot2)

all_data_bd21_plot<-cbind(c(xmatrix1, xmatrix2)/1000,c(ymatrix1, ymatrix2),sslist,c(rep("Control",length(ymatrix1)),rep("Drought",length(ymatrix2))))

all_data_bd21_plot<-all_data_bd21_plot[which(all_data_bd21_plot[,1]>0),]



colnames(all_data_bd21_plot)<-c("location","size","sample","treatment")
write.csv(as.matrix(all_data_bd21_plot),"~/Downloads/tmp.csv")
all_data_bd21_plot <-read.csv(paste0("~/Downloads/tmp.csv"), header=T,row.names=1)
library(ggplot2)
library(reshape2)

all_data_bd21_plot$treatment<-as.factor(all_data_bd21_plot$treatment)
gggggg2<-ggplot(all_data_bd21_plot,aes(location,size))+
  theme_classic()+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = treatment))+
  labs(x = "Thermal time (°Cd)",y = expression(paste("Cell length (", mu, "m)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",size = 1,aes(fill = treatment),alpha = 0.3)+
  guides(fill = "none")+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = treatment))+
  labs(x = "Thermal time (°Cd)",y = expression(paste("Cell length (", mu, "m)")),colour = "")+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")+
  geom_line(aes(group = interaction(sample, treatment),colour = treatment,alpha = 0.2))+
  guides(alpha= "none")+ scale_color_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+scale_fill_manual(values = c("Control" = "#66CC66", "Drought" = "#FF6600"))+ ggtitle(
"Wheat-CS-Domesticated")
# aaaaaa
  dev.off()
  
list4<-substr(list2,bb+4,bb+10)

 library(ggpubr)
pdf(paste0("~/Downloads/micro_plot.pdf"), width=16, height = 8)

ggarrange(aaaaaa,bbbbbb,cccccc,dddddd,eeeeee,ffffff,gggggg,hhhhhh,ncol=4,nrow=2, common.legend=TRUE, legend="bottom")
dev.off()
  library(ggpubr)
pdf(paste0("~/Downloads/micro_plot2.pdf"), width=16, height = 8)

ggarrange(aaaaaa2,bbbbbb2,cccccc2,dddddd2,eeeeee2,ffffff2,gggggg2,hhhhhh2,ncol=4,nrow=2, common.legend=TRUE, legend="bottom")
dev.off()





  pdf(paste0("~/Downloads/cellcurve.pdf"), width=8, height = 12)
par(mfrow = c(3,2))
dataall <-read.csv(paste0("/Volumes/TOSHIBAEXT/chapter3_info/datasets/celldata/matrixbd21.csv"), header=T)
dataall<-as.matrix(dataall)
parameters <-read.csv(paste0("/Volumes/TOSHIBAEXT/chapter3_info/datasets/celldata/matrixbd21_out2.csv"), header=T)
l10_90<-parameters[,12]-parameters[,11]
parameters<-cbind(parameters,l10_90)
for(i in 2:dim(parameters)[1]){

x=as.numeric(dataall[,1])
colors_list <- c("#FF6600", "#66CC66", "black","purple","green","yellow")
colors <- rep(NA, length(x))
color_index <- 1
for (j in 1:length(x)) {
  if (x[j] == -6000 && i > -6000) {  # Reset color when x == 1
    color_index <- color_index + 1
  }
  colors[j] <- colors_list[color_index]
}
plot(as.numeric(dataall[,1]),as.numeric(dataall[,i]),col= colors, xlim=c(0,max(as.numeric(dataall[,1])[which(as.numeric(dataall[,i])>0)])),ylim=c(0,max(na.omit(as.numeric(dataall[,-1])))),xlab=expression(paste("Distance (", mu, "m)")),ylab=expression(paste("Cell length (", mu, "m)")),main=colnames(dataall)[i])


par(new=TRUE)	
L=parameters[i-1,7]
   x0=parameters[i-1,8]
   k=parameters[i-1,9]
   b=parameters[i-1,10]
   x=seq(-1000,50000,200)
    y0 = L / (1 + exp(-k*(x-x0))) + b
plot( x,y0, type="l", col="#66CC66",lwd=2, xlim=c(0,max(as.numeric(dataall[,1])[which(as.numeric(dataall[,i])>0)])),ylim=c(0,max(na.omit(as.numeric(dataall[,-1])))),xlab="",ylab="")
text(x = 10000, y = 20, labels = paste("R² =", round(parameters$r_squared[i-1], 3)), cex = 1.5, col = "#FF6600")
}
dataall <-read.csv(paste0("/Volumes/TOSHIBAEXT/chapter3_info/datasets/celldata/matrixbarley.csv"), header=T)
dataall<-as.matrix(dataall)
parameters <-read.csv(paste0("/Volumes/TOSHIBAEXT/chapter3_info/datasets/celldata/matrixbarley_out2.csv"), header=T)
l10_90<-parameters[,12]-parameters[,11]
parameters<-cbind(parameters,l10_90)
for(i in 2:dim(parameters)[1]){
	
x=as.numeric(dataall[,1])

colors <- rep(NA, length(x))
color_index <- 1
for (j in 1:length(x)) {
  if (x[j] == -6000 && i > -6000) {  # Reset color when x == 1
    color_index <- color_index + 1
  }
  colors[j] <- colors_list[color_index]
}

plot(as.numeric(dataall[,1]),as.numeric(dataall[,i]),col= colors, xlim=c(0,max(as.numeric(dataall[,1])[which(as.numeric(dataall[,i])>0)])),ylim=c(0,max(na.omit(as.numeric(dataall[,-1])))),xlab=expression(paste("Distance (", mu, "m)")),ylab=expression(paste("Cell length (", mu, "m)")),main=colnames(dataall)[i])
par(new=TRUE)	
L=parameters[i-1,7]
   x0=parameters[i-1,8]
   k=parameters[i-1,9]
   b=parameters[i-1,10]
   x=seq(-1000,50000,200)
    y0 = L / (1 + exp(-k*(x-x0))) + b
plot( x,y0, type="l", col="#66CC66",lwd=2, xlim=c(0,max(as.numeric(dataall[,1])[which(as.numeric(dataall[,i])>0)])),ylim=c(0,max(na.omit(as.numeric(dataall[,-1])))),xlab="",ylab="")
text(x = 10000, y = 20, labels = paste("R² =", round(parameters$r_squared[i-1], 3)), cex = 1.5, col = "#FF6600")
}
dataall <-read.csv(paste0("/Volumes/TOSHIBAEXT/chapter3_info/datasets/celldata/matrixoat_2.csv"), header=T)
dataall<-as.matrix(dataall)
parameters <-read.csv(paste0("/Volumes/TOSHIBAEXT/chapter3_info/datasets/celldata/matrixoat_2_out2.csv"), header=T)
l10_90<-parameters[,12]-parameters[,11]
parameters<-cbind(parameters,l10_90)
for(i in 2:dim(parameters)[1]){
	
x=as.numeric(dataall[,1])

colors <- rep(NA, length(x))
color_index <- 1
for (j in 1:length(x)) {
  if (x[j] == -6000 && i > -6000) {  # Reset color when x == 1
    color_index <- color_index + 1
  }
  colors[j] <- colors_list[color_index]
}
plot(as.numeric(dataall[,1]),as.numeric(dataall[,i]),col= colors, xlim=c(0,max(as.numeric(dataall[,1])[which(as.numeric(dataall[,i])>0)])),ylim=c(0,max(na.omit(as.numeric(dataall[,-1])))),xlab=expression(paste("Distance (", mu, "m)")),ylab=expression(paste("Cell length (", mu, "m)")),main=colnames(dataall)[i])
par(new=TRUE)	
L=parameters[i-1,7]
   x0=parameters[i-1,8]
   k=parameters[i-1,9]
   b=parameters[i-1,10]
   x=seq(-1000,50000,200)
    y0 = L / (1 + exp(-k*(x-x0))) + b
plot( x,y0, type="l", col="#66CC66",lwd=2, xlim=c(0,max(as.numeric(dataall[,1])[which(as.numeric(dataall[,i])>0)])),ylim=c(0,max(na.omit(as.numeric(dataall[,-1])))),xlab="",ylab="")
text(x = 10000, y = 20, labels = paste("R² =", round(parameters$r_squared[i-1], 3)), cex = 1.5, col = "#FF6600")
}
dataall <-read.csv(paste0("/Volumes/TOSHIBAEXT/chapter3_info/datasets/celldata/matrixwheatcs.csv"), header=T)
dataall<-as.matrix(dataall)
parameters <-read.csv(paste0("/Volumes/TOSHIBAEXT/chapter3_info/datasets/celldata/matrixcs_out2.csv"), header=T)
l10_90<-parameters[,12]-parameters[,11]
parameters<-cbind(parameters,l10_90)
for(i in 2:dim(parameters)[1]){
	
x=as.numeric(dataall[,1])

colors <- rep(NA, length(x))
color_index <- 1
for (j in 1:length(x)) {
  if (x[j] == -6000 && i > -6000) {  # Reset color when x == 1
    color_index <- color_index + 1
  }
  colors[j] <- colors_list[color_index]
}
plot(as.numeric(dataall[,1]),as.numeric(dataall[,i]),col= colors, xlim=c(0,max(as.numeric(dataall[,1])[which(as.numeric(dataall[,i])>0)])),ylim=c(0,max(na.omit(as.numeric(dataall[,-1])))),xlab=expression(paste("Distance (", mu, "m)")),ylab=expression(paste("Cell length (", mu, "m)")),main=colnames(dataall)[i])
par(new=TRUE)	
L=parameters[i-1,7]
   x0=parameters[i-1,8]
   k=parameters[i-1,9]
   b=parameters[i-1,10]
   x=seq(-1000,50000,200)
    y0 = L / (1 + exp(-k*(x-x0))) + b
plot( x,y0, type="l", col="#66CC66",lwd=2, xlim=c(0,max(as.numeric(dataall[,1])[which(as.numeric(dataall[,i])>0)])),ylim=c(0,max(na.omit(as.numeric(dataall[,-1])))),xlab="",ylab="")
text(x = 10000, y = 20, labels = paste("R² =", round(parameters$r_squared[i-1], 3)), cex = 1.5, col = "#FF6600")
}
dataall <-read.csv(paste0("/Volumes/TOSHIBAEXT/chapter3_info/datasets/celldata/matrixwheatL.csv"), header=T)
dataall<-as.matrix(dataall)
parameters <-read.csv(paste0("/Volumes/TOSHIBAEXT/chapter3_info/datasets/celldata/matrixL_out2.csv"), header=T)
l10_90<-parameters[,12]-parameters[,11]
parameters<-cbind(parameters,l10_90)
for(i in 2:dim(parameters)[1]){
	
x=as.numeric(dataall[,1])

colors <- rep(NA, length(x))
color_index <- 1
for (j in 1:length(x)) {
  if (x[j] == -6000 && i > -6000) {  # Reset color when x == 1
    color_index <- color_index + 1
  }
  colors[j] <- colors_list[color_index]
}
plot(as.numeric(dataall[,1]),as.numeric(dataall[,i]),col= colors, xlim=c(0,max(as.numeric(dataall[,1])[which(as.numeric(dataall[,i])>0)])),ylim=c(0,max(na.omit(as.numeric(dataall[,-1])))),xlab=expression(paste("Distance (", mu, "m)")),ylab=expression(paste("Cell length (", mu, "m)")),main=colnames(dataall)[i])
par(new=TRUE)	
L=parameters[i-1,7]
   x0=parameters[i-1,8]
   k=parameters[i-1,9]
   b=parameters[i-1,10]
   x=seq(-1000,50000,200)
    y0 = L / (1 + exp(-k*(x-x0))) + b
plot( x,y0, type="l", col="#66CC66",lwd=2, xlim=c(0,max(as.numeric(dataall[,1])[which(as.numeric(dataall[,i])>0)])),ylim=c(0,max(na.omit(as.numeric(dataall[,-1])))),xlab="",ylab="")
text(x = 10000, y = 20, labels = paste("R² =", round(parameters$r_squared[i-1], 3)), cex = 1.5, col = "#FF6600")
}
dataall <-read.csv(paste0("/Volumes/TOSHIBAEXT/chapter3_info/datasets/celldata/matrixwheatzavitan.csv"), header=T)
dataall<-as.matrix(dataall)
parameters <-read.csv(paste0("/Volumes/TOSHIBAEXT/chapter3_info/datasets/celldata/matrixzavitan_out2.csv"), header=T)
l10_90<-parameters[,12]-parameters[,11]
parameters<-cbind(parameters,l10_90)
for(i in 2:dim(parameters)[1]){
	
x=as.numeric(dataall[,1])

colors <- rep(NA, length(x))
color_index <- 1
for (j in 1:length(x)) {
  if (x[j] == -6000 && i > -6000) {  # Reset color when x == 1
    color_index <- color_index + 1
  }
  colors[j] <- colors_list[color_index]
}
plot(as.numeric(dataall[,1]),as.numeric(dataall[,i]),col= colors, xlim=c(0,max(as.numeric(dataall[,1])[which(as.numeric(dataall[,i])>0)])),ylim=c(0,max(na.omit(as.numeric(dataall[,-1])))),xlab=expression(paste("Distance (", mu, "m)")),ylab=expression(paste("Cell length (", mu, "m)")),main=colnames(dataall)[i])
par(new=TRUE)	
L=parameters[i-1,7]
   x0=parameters[i-1,8]
   k=parameters[i-1,9]
   b=parameters[i-1,10]
   x=seq(-1000,50000,200)
    y0 = L / (1 + exp(-k*(x-x0))) + b
plot( x,y0, type="l", col="#66CC66",lwd=2, xlim=c(0,max(as.numeric(dataall[,1])[which(as.numeric(dataall[,i])>0)])),ylim=c(0,max(na.omit(as.numeric(dataall[,-1])))),xlab="",ylab="")
text(x = 10000, y = 20, labels = paste("R² =", round(parameters$r_squared[i-1], 3)), cex = 1.5, col = "#FF6600")
}

dev.off()

















