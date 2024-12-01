
nnn=2000

listall<-listall2<-NULL
mm=3
for(mm in 1:8){

a<-list<-list2<-NULL
bd21_m<-bd21_m2<-bd21_e<-NULL
microscopy<-read.csv("~/Desktop/chapter3final/microscopy_summary_2.csv", header=T)
elongation <-read.csv("~/Desktop/chapter3final/elongation_summary_models.csv", header=T)
info <-read.csv("~/Desktop/chapter3final/elongation_summary_info.csv", header=T)
info<-as.data.frame(t(info))
colnames(info)<-info[1,]
info<-info[-1,]
name2<-levels(as.factor(microscopy $species))[mm]
treatment<-as.factor(info $treatment)[which(info$accession == name2)]
bd21_e<-cbind(treatment,elongation[which(info$accession == name2),c(5,10,14)])
name1<-levels(as.factor(microscopy $species))[mm]

bd21_m<-microscopy[which(microscopy $species == name1),c(4,11,5,6,13,14,15)]

bd21_m <-na.omit(bd21_m)
bd21_e$der
bd21_e$treatment
##elongation rate
a<-summary(lm(der~treatment,data= bd21_e))$coefficient[2,c(1,4)]

model<-lm(der~treatment,data= bd21_e)
# Calculate Cook's Distance
cooks_distance <- cooks.distance(model)
# Set a threshold for influential points (often 4/n, where n is the number of data points)
threshold <- 4 / nrow(bd21_e)
influential_points <- cooks_distance > threshold
# Filter out influential points
bd21_e_filtered <- bd21_e[!influential_points, ]
modelfilter<-lm(der~treatment,data= bd21_e_filtered)
a<-summary(modelfilter)$coefficient[2,c(1,4)]
list<-c(list,a[2])
intercept <- coef(modelfilter)[1]  # beta_0 (mean for control)
treatment_coef <- coef(modelfilter)[2]  # beta_1 (effect of drought)
proportion_change <- treatment_coef / intercept
list2<-c(list2, proportion_change)




##elongation duration
a<-summary(lm(l10_90~ treatment,data= bd21_e))$coefficient[2,c(1,4)]

model<-lm(l10_90 ~treatment,data= bd21_e)
# Calculate Cook's Distance
cooks_distance <- cooks.distance(model)
# Set a threshold for influential points (often 4/n, where n is the number of data points)
threshold <- 4 / nrow(bd21_e)
influential_points <- cooks_distance > threshold
# Filter out influential points
bd21_e_filtered <- bd21_e[!influential_points, ]
modelfilter<-lm(l10_90 ~treatment,data= bd21_e_filtered)
a<-summary(modelfilter)$coefficient[2,c(1,4)]
list<-c(list,a[2])
intercept <- coef(modelfilter)[1]  # beta_0 (mean for control)
treatment_coef <- coef(modelfilter)[2]  # beta_1 (effect of drought)
proportion_change <- treatment_coef / intercept
list2<-c(list2, proportion_change)


a<-summary(lm(lf~ treatment,data= bd21_e))$coefficient[2,c(1,4)]

model<-lm(lf ~treatment,data= bd21_e)
# Calculate Cook's Distance
cooks_distance <- cooks.distance(model)
# Set a threshold for influential points (often 4/n, where n is the number of data points)
threshold <- 4 / nrow(bd21_e)
influential_points <- cooks_distance > threshold
# Filter out influential points
bd21_e_filtered <- bd21_e[!influential_points, ]
modelfilter<-lm(lf ~treatment,data= bd21_e_filtered)
a<-summary(modelfilter)$coefficient[2,c(1,4)]
list<-c(list,a[2])
intercept <- coef(modelfilter)[1]  # beta_0 (mean for control)
treatment_coef <- coef(modelfilter)[2]  # beta_1 (effect of drought)
proportion_change <- treatment_coef / intercept
list2<-c(list2, proportion_change)




bd21_e_summary<-cbind(aggregate(bd21_e $l10_90,list(bd21_e$treatment),mean),aggregate(bd21_e$der,list(bd21_e$treatment),mean)[,2],aggregate(bd21_e$lf,list(bd21_e$treatment),mean)[,2])

colnames(bd21_e_summary)<-c("","duration","rate","lm")
print(bd21_e_summary)
print(name1)
bd21_m<-cbind(bd21_m,bd21_e_summary[match(bd21_m$treatment,bd21_e_summary[,1]),])

a<-NaN
bd21_m$lf <-as.numeric(bd21_m$lf)
##lf
if(0%in%bd21_m$lf){
bd21_m_no0<-bd21_m[-which(bd21_m$lf==0),]
}else{
	bd21_m_no0<-bd21_m
	}
a<-summary(lm(lf ~ treatment,data= as.data.frame(na.omit(bd21_m_no0))))$coefficient[2,c(1,4)]

model<-lm(lf ~treatment,data= as.data.frame(na.omit(bd21_m_no0)))
# Calculate Cook's Distance
cooks_distance <- cooks.distance(model)
# Set a threshold for influential points (often 4/n, where n is the number of data points)
threshold <- 4 / nrow(as.data.frame(na.omit(bd21_m_no0)))
influential_points <- cooks_distance > threshold
# Filter out influential points
bd21_m_no0_filtered <- as.data.frame(na.omit(bd21_m_no0))[!influential_points, ]
modelfilter<-lm(lf ~treatment,data= bd21_m_no0_filtered)
a<-summary(modelfilter)$coefficient[2,c(1,4)]
list<-c(list,a[2])
intercept <- coef(modelfilter)[1]  # beta_0 (mean for control)
treatment_coef <- coef(modelfilter)[2]  # beta_1 (effect of drought)
proportion_change <- treatment_coef / intercept
list2<-c(list2, proportion_change)


##n_all****

#bd21_m_no0 $all<-bd21_m_no0 $lm/bd21_m_no0 $lf

permutation<-cbind(c(rep("c",nnn),rep("d",nnn)),c(sample(bd21_e $lf[which(bd21_e $treatment=="c")],nnn,replace = TRUE)/sample(bd21_m_no0 $lf[which(bd21_m_no0 $treatment=="c")],nnn,replace = TRUE), sample(bd21_e $lf[which(bd21_e $treatment=="d")],nnn,replace = TRUE)/sample(bd21_m_no0 $lf[which(bd21_m_no0 $treatment=="d")],nnn,replace = TRUE)))

colnames(permutation)<-c("treatment","lf")
permutation<-as.data.frame(permutation)
permutation $lf <-as.numeric(permutation $lf)
a<-summary(lm(lf ~ treatment,data= permutation))$coefficient[2,c(1,4)]
model<-lm(lf ~treatment,data= permutation)
# Calculate Cook's Distance
cooks_distance <- cooks.distance(model)
# Set a threshold for influential points (often 4/n, where n is the number of data points)
threshold <- 4 / nrow(permutation)
influential_points <- cooks_distance > threshold
# Filter out influential points
permutation_filtered <- permutation[!influential_points, ]
modelfilter<-lm(lf ~treatment,data= permutation_filtered)
a<-summary(modelfilter)$coefficient[2,c(1,4)]
list<-c(list,a[2])
intercept <- coef(modelfilter)[1]  # beta_0 (mean for control)
treatment_coef <- coef(modelfilter)[2]  # beta_1 (effect of drought)
proportion_change <- treatment_coef / intercept
list2<-c(list2, proportion_change)

##flux*****
permutation<-cbind(c(rep("c",nnn),rep("d",nnn)),c(sample(bd21_e $der[which(bd21_e $treatment=="c")],nnn,replace = TRUE)/sample(bd21_m_no0 $lf[which(bd21_m_no0 $treatment=="c")],nnn,replace = TRUE), sample(bd21_e $der[which(bd21_e $treatment=="d")],nnn,replace = TRUE)/sample(bd21_m_no0 $lf[which(bd21_m_no0 $treatment=="d")],nnn,replace = TRUE)))

colnames(permutation)<-c("treatment","lf")
permutation<-as.data.frame(permutation)
permutation $lf <-as.numeric(permutation $lf)
a<-summary(lm(lf ~ treatment,data= permutation))$coefficient[2,c(1,4)]
model<-lm(lf ~treatment,data= permutation)
# Calculate Cook's Distance
cooks_distance <- cooks.distance(model)
# Set a threshold for influential points (often 4/n, where n is the number of data points)
threshold <- 4 / nrow(permutation)
influential_points <- cooks_distance > threshold
# Filter out influential points
permutation_filtered <- permutation[!influential_points, ]
modelfilter<-lm(lf ~treatment,data= permutation_filtered)
a<-summary(modelfilter)$coefficient[2,c(1,4)]
list<-c(list,a[2])
intercept <- coef(modelfilter)[1]  # beta_0 (mean for control)
treatment_coef <- coef(modelfilter)[2]  # beta_1 (effect of drought)
proportion_change <- treatment_coef / intercept
list2<-c(list2, proportion_change)






##division
a<-summary(lm(divisionzonelist ~ treatment,data= as.data.frame(na.omit(bd21_m))))$coefficient[2,c(1,4)]
model<-lm(divisionzonelist ~treatment,data= as.data.frame(na.omit(bd21_m)))

# Calculate Cook's Distance
cooks_distance <- cooks.distance(model)
# Set a threshold for influential points (often 4/n, where n is the number of data points)
threshold <- 4 / nrow(as.data.frame(na.omit(bd21_m)))
influential_points <- cooks_distance > threshold
# Filter out influential points
bd21_m_filtered <- as.data.frame(na.omit(bd21_m))[!influential_points, ]
modelfilter<-lm(divisionzonelist ~treatment,data= bd21_m_filtered)
a<-summary(modelfilter)$coefficient[2,c(1,4)]
list<-c(list,a[2])
intercept <- coef(modelfilter)[1]  # beta_0 (mean for control)
treatment_coef <- coef(modelfilter)[2]  # beta_1 (effect of drought)
proportion_change <- treatment_coef / intercept
list2<-c(list2, proportion_change)


bd21_m_no0 $divisioncellsize <-as.numeric(bd21_m_no0 $divisioncellsize)
a<-NaN
##e
a<-summary(lm(divisioncellsize ~ treatment,data= as.data.frame(na.omit(bd21_m_no0))))$coefficient[2,c(1,4)]
model<-lm(divisioncellsize ~treatment,data= as.data.frame(na.omit(bd21_m_no0)))

# Calculate Cook's Distance
cooks_distance <- cooks.distance(model)
# Set a threshold for influential points (often 4/n, where n is the number of data points)
threshold <- 4 / nrow(as.data.frame(na.omit(bd21_m_no0)))
influential_points <- cooks_distance > threshold
# Filter out influential points
bd21_m_no0_filtered <- as.data.frame(na.omit(bd21_m_no0))[!influential_points, ]
modelfilter<-lm(divisioncellsize ~treatment,data= bd21_m_no0_filtered)
a<-summary(modelfilter)$coefficient[2,c(1,4)]
list<-c(list,a[2])
intercept <- coef(modelfilter)[1]  # beta_0 (mean for control)
treatment_coef <- coef(modelfilter)[2]  # beta_1 (effect of drought)
proportion_change <- treatment_coef / intercept
list2<-c(list2, proportion_change)

##Llge


##Ndiv
bd21_m_no0 $Ndiv <-bd21_m_no0 $divisionzonelist/bd21_m_no0 $divisioncellsize
model<-lm(Ndiv ~treatment,data= as.data.frame(na.omit(bd21_m_no0)))
# Calculate Cook's Distance
cooks_distance <- cooks.distance(model)
# Set a threshold for influential points (often 4/n, where n is the number of data points)
threshold <- 4 / nrow(as.data.frame(na.omit(bd21_m_no0)))
influential_points <- cooks_distance > threshold
# Filter out influential points
bd21_m_no0_filtered <- as.data.frame(na.omit(bd21_m_no0))[!influential_points, ]
modelfilter<-lm(Ndiv ~treatment,data= bd21_m_no0_filtered)
a<-summary(modelfilter)$coefficient[2,c(1,4)]
list<-c(list,a[2])
intercept <- coef(modelfilter)[1]  # beta_0 (mean for control)
treatment_coef <- coef(modelfilter)[2]  # beta_1 (effect of drought)
proportion_change <- treatment_coef / intercept
list2<-c(list2, proportion_change)


##Ddiv*****
#bd21_m_no0 $Ddiv <-bd21_m_no0 $rate/(bd21_m_no0 $lf*bd21_m_no0 $Ndiv)

permutation<-cbind(c(rep("c",nnn),rep("d",nnn)),c(sample(bd21_e $der[which(bd21_e $treatment=="c")],nnn,replace = TRUE)/sample((bd21_m_no0 $lf*bd21_m_no0 $Ndiv)[which(bd21_m_no0 $treatment=="c")],nnn,replace = TRUE), sample(bd21_e $der[which(bd21_e $treatment=="d")],nnn,replace = TRUE)/sample((bd21_m_no0 $lf*bd21_m_no0 $Ndiv)[which(bd21_m_no0 $treatment=="d")],nnn,replace = TRUE)))

colnames(permutation)<-c("treatment","lf")
permutation<-as.data.frame(permutation)
permutation $lf <-as.numeric(permutation $lf)
a<-summary(lm(lf ~ treatment,data= permutation))$coefficient[2,c(1,4)]
model<-lm(lf ~treatment,data= permutation)
# Calculate Cook's Distance
cooks_distance <- cooks.distance(model)
# Set a threshold for influential points (often 4/n, where n is the number of data points)
threshold <- 4 / nrow(permutation)
influential_points <- cooks_distance > threshold
# Filter out influential points
permutation_filtered <- permutation[!influential_points, ]
modelfilter<-lm(lf ~treatment,data= permutation_filtered)
a<-summary(modelfilter)$coefficient[2,c(1,4)]
list<-c(list,a[2])
intercept <- coef(modelfilter)[1]  # beta_0 (mean for control)
treatment_coef <- coef(modelfilter)[2]  # beta_1 (effect of drought)
proportion_change <- treatment_coef / intercept
list2<-c(list2, proportion_change)




###
bd21_m_no0 $Lge <-as.numeric(bd21_m_no0 $Lge)
model<-lm(Lge ~treatment,data= as.data.frame(na.omit(bd21_m_no0)))

# Calculate Cook's Distance
cooks_distance <- cooks.distance(model)
# Set a threshold for influential points (often 4/n, where n is the number of data points)
threshold <- 4 / nrow(as.data.frame(na.omit(bd21_m_no0)))
influential_points <- cooks_distance > threshold
# Filter out influential points
bd21_m_no0_filtered <- as.data.frame(na.omit(bd21_m_no0))[!influential_points, ]
modelfilter<-lm(Lge ~treatment,data= bd21_m_no0_filtered)
a<-summary(modelfilter)$coefficient[2,c(1,4)]
list<-c(list,a[2])
intercept <- coef(modelfilter)[1]  # beta_0 (mean for control)
treatment_coef <- coef(modelfilter)[2]  # beta_1 (effect of drought)
proportion_change <- treatment_coef / intercept
list2<-c(list2, proportion_change)


##ne
bd21_m_no0 $N <-as.numeric(bd21_m_no0 $N)
model<-lm(N ~treatment,data= as.data.frame(na.omit(bd21_m_no0)))

# Calculate Cook's Distance
cooks_distance <- cooks.distance(model)
# Set a threshold for influential points (often 4/n, where n is the number of data points)
threshold <- 4 / nrow(as.data.frame(na.omit(bd21_m_no0)))
influential_points <- cooks_distance > threshold
# Filter out influential points
bd21_m_no0_filtered <- as.data.frame(na.omit(bd21_m_no0))[!influential_points, ]
modelfilter<-lm(N ~treatment,data= bd21_m_no0_filtered)
a<-summary(modelfilter)$coefficient[2,c(1,4)]
list<-c(list,a[2])
intercept <- coef(modelfilter)[1]  # beta_0 (mean for control)
treatment_coef <- coef(modelfilter)[2]  # beta_1 (effect of drought)
proportion_change <- treatment_coef / intercept
list2<-c(list2, proportion_change)



##E_rate****
#bd21_m_no0 $E_rate <-((bd21_m_no0 $lf-bd21_m_no0 $divisioncellsize)/bd21_m_no0 $lf/bd21_m_no0 $Lge)*bd21_m_no0 $rate
permutation<-cbind(c(rep("c",nnn),rep("d",nnn)),c(sample(((bd21_m_no0 $lf-bd21_m_no0 $divisioncellsize)/bd21_m_no0 $lf/bd21_m_no0 $Lge)[which(bd21_m_no0 $treatment=="c")],nnn,replace = TRUE)*sample(bd21_e $der[which(bd21_e $treatment=="c")],nnn,replace = TRUE), sample(((bd21_m_no0 $lf-bd21_m_no0 $divisioncellsize)/bd21_m_no0 $lf/bd21_m_no0 $Lge)[which(bd21_m_no0 $treatment=="d")],nnn,replace = TRUE)*sample(bd21_e $der[which(bd21_e $treatment=="d")],nnn,replace = TRUE)))

colnames(permutation)<-c("treatment","lf")
permutation<-as.data.frame(permutation)
permutation $lf <-as.numeric(permutation $lf)
a<-summary(lm(lf ~ treatment,data= permutation))$coefficient[2,c(1,4)]
model<-lm(lf ~treatment,data= permutation)
# Calculate Cook's Distance
cooks_distance <- cooks.distance(model)
# Set a threshold for influential points (often 4/n, where n is the number of data points)
threshold <- 4 / nrow(permutation)
influential_points <- cooks_distance > threshold
# Filter out influential points
permutation_filtered <- permutation[!influential_points, ]
modelfilter<-lm(lf ~treatment,data= permutation_filtered)
a<-summary(modelfilter)$coefficient[2,c(1,4)]
list<-c(list,a[2])
intercept <- coef(modelfilter)[1]  # beta_0 (mean for control)
treatment_coef <- coef(modelfilter)[2]  # beta_1 (effect of drought)
proportion_change <- treatment_coef / intercept
list2<-c(list2, proportion_change)




#elongation duration 
##flux*****

#bd21_m_no0 $durationflux<-bd21_m_no0 $N *bd21_m_no0 $lf/bd21_m_no0 $rate
permutation<-cbind(c(rep("c",nnn),rep("d",nnn)),c(sample((bd21_m_no0 $N *bd21_m_no0 $lf)[which(bd21_m_no0 $treatment=="c")],nnn,replace = TRUE)/sample(bd21_e $der[which(bd21_e $treatment=="c")],nnn,replace = TRUE), sample((bd21_m_no0 $N *bd21_m_no0 $lf)[which(bd21_m_no0 $treatment=="d")],nnn,replace = TRUE)/sample(bd21_e $der[which(bd21_e $treatment=="d")],nnn,replace = TRUE)))

colnames(permutation)<-c("treatment","lf")
permutation<-as.data.frame(permutation)
permutation $lf <-as.numeric(permutation $lf)
a<-summary(lm(lf ~ treatment,data= permutation))$coefficient[2,c(1,4)]
model<-lm(lf ~treatment,data= permutation)
# Calculate Cook's Distance
cooks_distance <- cooks.distance(model)
# Set a threshold for influential points (often 4/n, where n is the number of data points)
threshold <- 4 / nrow(permutation)
influential_points <- cooks_distance > threshold
# Filter out influential points
permutation_filtered <- permutation[!influential_points, ]
modelfilter<-lm(lf ~treatment,data= permutation_filtered)
a<-summary(modelfilter)$coefficient[2,c(1,4)]
list<-c(list,a[2])
intercept <- coef(modelfilter)[1]  # beta_0 (mean for control)
treatment_coef <- coef(modelfilter)[2]  # beta_1 (effect of drought)
proportion_change <- treatment_coef / intercept
list2<-c(list2, proportion_change)




listall<-cbind(listall,list)
listall2<-cbind(listall2,list2)




}

  #d<-(-log(listall,10))*abs(listall2)/listall2
  d<-listall2
    d[which(d>1)]<-1
    d[which(d<c(-1))]<-c(-1)
  colnames(d)<-c("Barley-Domesticated","Barley-Wild","Bd21","Oat-Domesticated","Oat-Wild","Wheat-CS-Domesticated","Wheat-L-Domesticated","Wheat-Z-Wild")
  rownames(d)<-c("leaf elongation rate","leaf elongation duration","final leaf length","mature cell size","cell number^","cell flux^","division zone size","dividing cell size","division zone cell number","division rate^","elongation zone size","elongation cells number","relative cell elongation rate^","cell elongation duration^")

  list

library(gplots)
library(RColorBrewer)


labs<-matrix(symnum(c(listall), corr = FALSE, na = FALSE, cutpoints = c(0, 
    0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", " ")),dim(d)[1],dim(d)[2])

library(gplots)
  rownames(d)<-c("leaf elongation rate","leaf elongation duration","final leaf length","mature cell size","cell number^","cell flux^","division zone size","dividing cell size","division zone cell number","division rate^","elongation zone size","elongation cells number","relative cell elongation rate^","cell elongation duration^")

# Save the output to a PDF
pdf(paste0("~/Downloads/enhanced_heatmap3.pdf"), width=10, height=10)

# Improved color palette: More distinguishable colors
custom_palette <- colorRampPalette(c("brown1", "white", "deepskyblue2"))
# Plot the heatmap
heatmap.2(d[1:12,], 
          Colv = FALSE, 
          Rowv = FALSE, 
          trace = "none", 
          cellnote = labs,  # Add labels
          col =custom_palette(100),  # Improved color scheme
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
         margins = c(10, 16),  # Adjust margins for labels
          density.info = "density",  # Add histogram and density plot
          denscol = "black",  # Histogram color
          lwid = c(1.5, 4),  # Adjust width of side color bars
          lhei = c(1.5, 7)   # Adjust height for better aspect ratio
)
dev.off()
pdf(paste0("~/Downloads/enhanced_heatmap4.pdf"), width=10, height=4)
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
          margins = c(10, 16),  # Adjust margins for labels
          density.info = "density",  # Add histogram and density plot
          denscol = "black",  # Histogram color
          lwid = c(1.5, 4),  # Adjust width of side color bars
          lhei = c(1.5, 5)   # Adjust height for better aspect ratio
)

# Close the PDF
dev.off()
