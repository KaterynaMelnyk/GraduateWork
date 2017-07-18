library(dplyr)
setwd("E:/Diplom")
library(car)
library(ggplot2)
data_initial<-read.csv("expression_new.csv", row.names=1)
data_without_norm<-NULL
p_value_norm<-0
list_of_colnames_data_not_norm<-NULL
str_names_not_norm<-NULL
str_names_norm<-NULL
for (i in 2:(dim(data_initial)[2]))
  {
  a1<-subset(data_initial[,i], data_initial$Diagnosis=="control")
  a2<-subset(data_initial[,i], data_initial$Diagnosis=="CA")
  a3<-subset(data_initial[,i], data_initial$Diagnosis=="SA")
  if (shapiro.test(a1)$p.value<=0.05 | shapiro.test(a2)$p.value<=0.05 | shapiro.test(a3)$p.value<=0.05)
    {
    list_of_colnames_data_not_norm<-c(list_of_colnames_data_not_norm,names(data_initial)[i])
  }
  if (shapiro.test(a1)$p.value<0.01 & shapiro.test(a2)$p.value<0.01 & shapiro.test(a3)$p.value>0.5)
  {
    str_names_not_norm<-c(str_names_not_norm, names(data_initial)[i])
  }
  if (shapiro.test(a1)$p.value>0.8 & shapiro.test(a2)$p.value<0.8 & shapiro.test(a3)$p.value<0.8)
  {
    str_names_norm<-c(str_names_norm, names(data_initial)[i])
  }
}

#create new file with data without normal
data_without_norm <- data_initial[names(data_initial) %in% (c(list_of_colnames_data_not_norm,"Diagnosis") )] 
data_with_norm<-data_initial[,!(colnames(data_initial) %in% as.vector(list_of_colnames_data_not_norm))]
write.csv(data_with_norm, "data_with_normal.csv")
write.csv(data_without_norm, "data_without_normal.csv")
data_without_norm<-read.csv("data_without_normal.csv", row.names = 1)
ggplot(data_initial, aes(x=ZFY, fill=Diagnosis))+geom_density(alpha=0.3)+facet_grid(~Diagnosis)
ggplot(data_initial, aes(x=AASS, fill=Diagnosis))+geom_density(alpha=0.6)+facet_grid(~Diagnosis)
ggplot(data_initial, aes(x=RBPMS2, fill=Diagnosis))+geom_density(alpha=0.6)+facet_grid(~Diagnosis)
#Check of equality dispersion
data_with_norm<-read.csv("data_with_normal.csv", row.names = 1, stringsAsFactors = FALSE)
list_of_colnames_without_disp<-NULL
data_not_equality_disp<-NULL
for (i in 2:(dim(data_with_norm)[2]))
  {
  bar_test<-bartlett.test(data_with_norm[,i]~Diagnosis, data=data_with_norm)
  if (bar_test$p.value<0.05)
    {
    list_of_colnames_without_disp<-c(list_of_colnames_without_disp ,names(data_with_norm)[i])
    }
}
data_without_equality_disp<-data_initial[colnames(data_initial) %in% (c(list_of_colnames_without_disp,"Diagnosis") )]
data_with_norm_and_disp<-data_with_norm[,!(colnames(data_with_norm) %in% as.vector(list_of_colnames_without_disp))]
write.csv(data_with_norm_and_disp, "data_with_disp_and_norm.csv")
write.csv(data_without_equality_disp, "data_without_disp.csv")


#ANOVA
data_with_norm_and_disp<-read.csv("data_with_disp_and_norm.csv", row.names=1, stringsAsFactors = FALSE)
list_of_colnames_anova<-NULL
data_not_diff_mean<-NULL
for (i in 2:(dim(data_with_norm_and_disp)[2]))
  {
  aov_test<-aov(data_with_norm_and_disp[,i]~Diagnosis, data=data_with_norm_and_disp)
  
  if (summary(aov_test)[[1]][["Pr(>F)"]][[1]]>0.05)
    {
    list_of_colnames_anova<-c(list_of_colnames_anova,names(data_with_norm_and_disp)[i])
    }
}

data_not_diff_mean<-data_with_norm_and_disp[colnames(data_with_norm_and_disp) %in% (c(list_of_colnames_anova,"Diagnosis") )]
data_with_diff_mean<-data_with_norm_and_disp[,!(colnames(data_with_norm_and_disp) %in% as.vector(list_of_colnames_anova))]
write.csv(data_with_diff_mean, "data_with_diff_mean.csv")
write.csv(data_not_diff_mean, "data_not_diff_mean.csv")


#_____________________________________________________________________

data_with_diff_mean<-read.csv("data_with_diff_mean.csv", row.names = 1, stringsAsFactors = FALSE)
p_value_SA_CA_none_0<-NULL
p_value_SA_control_none_0<-NULL
p_value_CA_control_none_0<-NULL
p_value_SA_CA_none_1<-NULL
p_value_SA_control_none_1<-NULL
p_value_CA_control_none_1<-NULL
names_SA_CA_none<-NULL
names_SA_control_none<-NULL
names_CA_control_none<-NULL

#NONE
for (i in 2:(dim(data_with_diff_mean)[2])){
  t_test<-pairwise.t.test(data_with_diff_mean[,i],data_with_diff_mean$Diagnosis, p.adjust.method="none", paired=FALSE, pool.sd=FALSE)
  if (t_test$p.value[2,1]<=0.05){
    names_SA_CA_none<-c(names_SA_CA_none, names(data_with_diff_mean)[i])
    p_value_SA_CA_none_0<-c(p_value_SA_CA_none_0, t_test$p.value[2,1])
  }
  else{p_value_SA_CA_none_1<-c(p_value_SA_CA_none_1, t_test$p.value[2,1])}
  if (t_test$p.value[1,1]<=0.05){
    names_CA_control_none<-c(names_CA_control_none ,names(data_with_diff_mean)[i])
    p_value_CA_control_none_0<-c(p_value_CA_control_none_0, t_test$p.value[1,1])
  }
  else{p_value_CA_control_none_1<-c(p_value_CA_control_none_1, t_test$p.value[1,1])}
  if (t_test$p.value[2,2]<=0.05){
    names_SA_control_none<-c(names_SA_control_none,names(data_with_diff_mean)[i])
    p_value_SA_control_none_0<-c(p_value_SA_control_none_0,t_test$p.value[2,2])
  }
  else{p_value_SA_control_none_1<-c(p_value_SA_control_none_1,t_test$p.value[2,2])}
}
data_p_value_SA_CA_none<-as.data.frame(rbind(cbind(p_value_SA_CA_none_0, 0),cbind(p_value_SA_CA_none_1, 1)))
colnames(data_p_value_SA_CA_none)<-c("p_value", "Hi")
data_p_value_SA_CA_none$Hi<-factor(data_p_value_SA_CA_none$Hi)
data_p_value_SA_CA_none$color[data_p_value_SA_CA_none$Hi==0]<-"blue"
data_p_value_SA_CA_none$color[data_p_value_SA_CA_none$Hi==1]<-"red"
data_p_value_SA_CA_none<-data_p_value_SA_CA_none[order(data_p_value_SA_CA_none$p_value),]
ggplot(data_p_value_SA_CA_none, aes(x=1:length(data_p_value_SA_CA_none$p_value), y=p_value, color=color))+ scale_color_discrete(labels=c("???????????????????????? ?? SA ?? CA","???? ???????????????????????? ?? SA ?? CA"))+geom_point(size=0.5)+ggtitle("?????? ????????????????")+xlab("")+geom_line(y=0.05, color='black')+theme(legend.text=element_text(size=13))

data_p_value_SA_control_none<-as.data.frame(rbind(cbind(p_value_SA_control_none_0, 0),cbind(p_value_SA_control_none_1, 1)))
colnames(data_p_value_SA_control_none)<-c("p_value", "Hi")
data_p_value_SA_control_none$Hi<-factor(data_p_value_SA_control_none$Hi)
data_p_value_SA_control_none$color[data_p_value_SA_control_none$Hi==0]<-"blue"
data_p_value_SA_control_none$color[data_p_value_SA_control_none$Hi==1]<-"red"
data_p_value_SA_control_none<-data_p_value_SA_control_none[order(data_p_value_SA_control_none$p_value),]
ggplot(data_p_value_SA_control_none, aes(x=1:length(data_p_value_SA_control_none$p_value), y=p_value, color=color))+geom_point(size=0.5)+ggtitle("?????? ????????????????")+xlab("")+geom_line(y=0.05, color='black')+scale_color_discrete(labels=c("???????????????????????? ?? SA ?? control","???? ???????????????????????? ?? SA ?? control"))+theme(legend.text=element_text(size=13))

data_p_value_CA_control_none<-as.data.frame(rbind(cbind(p_value_CA_control_none_0, 0),cbind(p_value_CA_control_none_1, 1)))
colnames(data_p_value_CA_control_none)<-c("p_value", "Hi")
data_p_value_CA_control_none$Hi<-factor(data_p_value_CA_control_none$Hi)
data_p_value_CA_control_none$color[data_p_value_CA_control_none$Hi==0]<-"blue"
data_p_value_CA_control_none$color[data_p_value_CA_control_none$Hi==1]<-"red"
data_p_value_CA_control_none<-data_p_value_CA_control_none[order(data_p_value_CA_control_none$p_value),]
ggplot(data_p_value_CA_control_none, aes(x=1:length(data_p_value_CA_control_none$p_value), y=p_value, color=color))+geom_point(size=0.5)+ggtitle("?????? ????????????????")+xlab("")+geom_line(y=0.05, color='black')+scale_color_discrete(labels=c("???????????????????????? ?? CA ?? control","???? ???????????????????????? ?? CA ?? control"))+theme(legend.text=element_text(size=13))

none_names_and_p_value_SA_control=data.frame(p_value_SA_control_none_0, names_SA_control_none)
none_names_and_p_value_CA_control=data.frame(p_value_CA_control_none_0, names_CA_control_none)
none_names_and_p_value_SA_CA=data.frame(p_value_SA_CA_none_0, names_SA_CA_none)

sort_none_names_and_p_value_CA_control<-none_names_and_p_value_CA_control[order(none_names_and_p_value_CA_control$p_value_CA_control_none_0), ]
sort_none_names_and_p_value_SA_control<-none_names_and_p_value_SA_control[order(none_names_and_p_value_SA_control$p_value_SA_control_none_0), ]
sort_none_names_and_p_value_SA_CA<-none_names_and_p_value_SA_CA[order(none_names_and_p_value_SA_CA$p_value_SA_CA_none_0), ]
write.csv(sort_none_names_and_p_value_SA_CA, "sort_none_p_value_SA_CA.csv")
write.csv(sort_none_names_and_p_value_SA_control, "sort_none_p_value_SA_control.csv")
write.csv(sort_none_names_and_p_value_CA_control, "sort_none_p_value_CA_control.csv")


#Bonferroni
p_value_SA_CA_bonferroni_0<-NULL
p_value_SA_control_bonferroni_0<-NULL
p_value_CA_control_bonferroni_0<-NULL
p_value_SA_CA_bonferroni_1<-NULL
p_value_SA_control_bonferroni_1<-NULL
p_value_CA_control_bonferroni_1<-NULL
names_SA_CA_bonferroni<-NULL
names_SA_control_bonferroni<-NULL
names_CA_control_bonferroni<-NULL
for (i in 2:(dim(data_with_diff_mean)[2])){
  t_test<-pairwise.t.test(data_with_diff_mean[,i],data_with_diff_mean$Diagnosis, p.adjust.method="bonferroni", paired=FALSE, pool.sd=FALSE)
  if (t_test$p.value[2,1]<=0.05){
    names_SA_CA_bonferroni<-c(names_SA_CA_bonferroni,names(data_with_diff_mean)[i])
    p_value_SA_CA_bonferroni_0<-c(p_value_SA_CA_bonferroni_0,t_test$p.value[2,1])
  }
  else{p_value_SA_CA_bonferroni_1<-c(p_value_SA_CA_bonferroni_1,t_test$p.value[2,1])}
  if (t_test$p.value[1,1]<=0.05){
    names_CA_control_bonferroni<-c(names_CA_control_bonferroni,names(data_with_diff_mean)[i])
    p_value_CA_control_bonferroni_0<-c(p_value_CA_control_bonferroni_0,t_test$p.value[1,1])
  }
  else{p_value_CA_control_bonferroni_1<-c(p_value_CA_control_bonferroni_1,t_test$p.value[1,1])}
  if (t_test$p.value[2,2]<=0.05){
    names_SA_control_bonferroni<-c(names_SA_control_bonferroni,names(data_with_diff_mean)[i])
    p_value_SA_control_bonferroni_0<-c(p_value_SA_control_bonferroni_0,t_test$p.value[2,2])
  }
  else{p_value_SA_control_bonferroni_1<-c(p_value_SA_control_bonferroni_1,t_test$p.value[2,2])}
}

data_p_value_SA_CA_bonferroni<-as.data.frame(rbind(cbind(p_value_SA_CA_bonferroni_0, 0),cbind(p_value_SA_CA_bonferroni_1, 1)))
colnames(data_p_value_SA_CA_bonferroni)<-c("p_value", "Hi")
data_p_value_SA_CA_bonferroni$Hi<-factor(data_p_value_SA_CA_bonferroni$Hi)
data_p_value_SA_CA_bonferroni$color[data_p_value_SA_CA_bonferroni$Hi==0]<-"blue"
data_p_value_SA_CA_bonferroni$color[data_p_value_SA_CA_bonferroni$Hi==1]<-"red"
data_p_value_SA_CA_bonferroni<-data_p_value_SA_CA_bonferroni[order(data_p_value_SA_CA_bonferroni$p_value),]
ggplot(data_p_value_SA_CA_bonferroni, aes(x=1:length(data_p_value_SA_CA_bonferroni$p_value), y=p_value, color=color))+ scale_color_discrete(labels=c("???????????????????????? ?? SA ?? CA","???? ???????????????????????? ?? SA ?? CA"))+geom_point(size=0.5)+ggtitle("???????????????? ????????????????????")+xlab("")+geom_line(y=0.05, color='black')+theme(legend.text=element_text(size=13))

data_p_value_SA_control_bonferroni<-as.data.frame(rbind(cbind(p_value_SA_control_bonferroni_0, 0),cbind(p_value_SA_control_bonferroni_1, 1)))
colnames(data_p_value_SA_control_bonferroni)<-c("p_value", "Hi")
data_p_value_SA_control_bonferroni$Hi<-factor(data_p_value_SA_control_bonferroni$Hi)
data_p_value_SA_control_bonferroni$color[data_p_value_SA_control_bonferroni$Hi==0]<-"blue"
data_p_value_SA_control_bonferroni$color[data_p_value_SA_control_bonferroni$Hi==1]<-"red"
data_p_value_SA_control_bonferroni<-data_p_value_SA_control_bonferroni[order(data_p_value_SA_control_bonferroni$p_value),]
ggplot(data_p_value_SA_control_bonferroni, aes(x=1:length(data_p_value_SA_control_bonferroni$p_value), y=p_value, color=color))+geom_point(size=0.5)+ggtitle("???????????????? ????????????????????")+xlab("")+geom_line(y=0.05, color='black')+scale_color_discrete(labels=c("???????????????????????? ?? SA ?? control","???? ???????????????????????? ?? SA ?? control"))+theme(legend.text=element_text(size=13))

data_p_value_CA_control_bonferroni<-as.data.frame(rbind(cbind(p_value_CA_control_bonferroni_0, 0),cbind(p_value_CA_control_bonferroni_1, 1)))
colnames(data_p_value_CA_control_bonferroni)<-c("p_value", "Hi")
data_p_value_CA_control_bonferroni$Hi<-factor(data_p_value_CA_control_bonferroni$Hi)
data_p_value_CA_control_bonferroni$color[data_p_value_CA_control_bonferroni$Hi==0]<-"blue"
data_p_value_CA_control_bonferroni$color[data_p_value_CA_control_bonferroni$Hi==1]<-"red"
data_p_value_CA_control_bonferroni<-data_p_value_CA_control_bonferroni[order(data_p_value_CA_control_bonferroni$p_value),]
ggplot(data_p_value_CA_control_bonferroni, aes(x=1:length(data_p_value_CA_control_bonferroni$p_value), y=p_value, color=color))+geom_point(size=0.5)+ggtitle("???????????????? ????????????????????")+xlab("")+geom_line(y=0.05, color='black')+scale_color_discrete(labels=c("???????????????????????? ?? CA ?? control","???? ???????????????????????? ?? CA ?? control"))+theme(legend.text=element_text(size=13))

bonferroni_names_and_p_value_SA_control=data.frame(p_value_SA_control_bonferroni, names_SA_control_bonferroni)
bonferroni_names_and_p_value_CA_control=data.frame(p_value_CA_control_bonferroni, names_CA_control_bonferroni)
bonferroni_names_and_p_value_SA_CA=data.frame(p_value_SA_CA_bonferroni, names_SA_CA_bonferroni)

sort_bonferroni_names_and_p_value_CA_control<-bonferroni_names_and_p_value_CA_control[order(bonferroni_names_and_p_value_CA_control$p_value_CA_control_bonferroni), ]
sort_bonferroni_names_and_p_value_SA_control<-bonferroni_names_and_p_value_SA_control[order(bonferroni_names_and_p_value_SA_control$p_value_SA_control_bonferroni), ]
sort_bonferroni_names_and_p_value_SA_CA<-bonferroni_names_and_p_value_SA_CA[order(bonferroni_names_and_p_value_SA_CA$p_value_SA_CA_bonferroni), ]

#HOLM
p_value_SA_CA_holm_0<-NULL
p_value_SA_control_holm_0<-NULL
p_value_CA_control_holm_0<-NULL
p_value_SA_CA_holm_1<-NULL
p_value_SA_control_holm_1<-NULL
p_value_CA_control_holm_1<-NULL
names_SA_CA_holm<-NULL
names_SA_control_holm<-NULL
names_CA_control_holm<-NULL
for (i in 2:(dim(data_with_diff_mean)[2])){
  t_test<-pairwise.t.test(data_with_diff_mean[,i],data_with_diff_mean$Diagnosis, p.adjust.method="holm", paired=FALSE, pool.sd=FALSE)
  if (t_test$p.value[2,1]<=0.05){
    names_SA_CA_holm<-c(names_SA_CA_holm,names(data_with_diff_mean)[i])
    p_value_SA_CA_holm_0<-c(p_value_SA_CA_holm_0,t_test$p.value[2,1])
  }
  else{p_value_SA_CA_holm_1<-c(p_value_SA_CA_holm_1,t_test$p.value[2,1])}
  if (t_test$p.value[1,1]<=0.05){
    names_CA_control_holm<-c(names_CA_control_holm,names(data_with_diff_mean)[i])
    p_value_CA_control_holm_0<-c(p_value_CA_control_holm_0,t_test$p.value[1,1])
  }
  else{p_value_CA_control_holm_1<-c(p_value_CA_control_holm_1,t_test$p.value[1,1])}
  if (t_test$p.value[2,2]<=0.05){
    names_SA_control_holm<-c(names_SA_control_holm,names(data_with_diff_mean)[i])
    p_value_SA_control_holm_0<-c(p_value_SA_control_holm_0,t_test$p.value[2,2])
  }
  else{p_value_SA_control_holm_1<-c(p_value_SA_control_holm_1,t_test$p.value[2,2])}
}

data_p_value_SA_CA_holm<-as.data.frame(rbind(cbind(p_value_SA_CA_holm_0, 0),cbind(p_value_SA_CA_holm_1, 1)))
colnames(data_p_value_SA_CA_holm)<-c("p_value", "Hi")
data_p_value_SA_CA_holm$Hi<-factor(data_p_value_SA_CA_holm$Hi)
data_p_value_SA_CA_holm$color[data_p_value_SA_CA_holm$Hi==0]<-"blue"
data_p_value_SA_CA_holm$color[data_p_value_SA_CA_holm$Hi==1]<-"red"
data_p_value_SA_CA_holm<-data_p_value_SA_CA_holm[order(data_p_value_SA_CA_holm$p_value),]
ggplot(data_p_value_SA_CA_holm, aes(x=1:length(data_p_value_SA_CA_holm$p_value), y=p_value, color=color))+ scale_color_discrete(labels=c("???????????????????????? ?? SA ?? CA","???? ???????????????????????? ?? SA ?? CA"))+geom_point(size=0.5)+ggtitle("???????????????? ??????????")+xlab("")+geom_line(y=0.05, color='black')+theme(legend.text=element_text(size=13))

data_p_value_SA_control_holm<-as.data.frame(rbind(cbind(p_value_SA_control_holm_0, 0),cbind(p_value_SA_control_holm_1, 1)))
colnames(data_p_value_SA_control_holm)<-c("p_value", "Hi")
data_p_value_SA_control_holm$Hi<-factor(data_p_value_SA_control_holm$Hi)
data_p_value_SA_control_holm$color[data_p_value_SA_control_holm$Hi==0]<-"blue"
data_p_value_SA_control_holm$color[data_p_value_SA_control_holm$Hi==1]<-"red"
data_p_value_SA_control_holm<-data_p_value_SA_control_holm[order(data_p_value_SA_control_holm$p_value),]
ggplot(data_p_value_SA_control_holm, aes(x=1:length(data_p_value_SA_control_holm$p_value), y=p_value, color=color))+geom_point(size=0.5)+ggtitle("???????????????? ??????????")+xlab("")+geom_line(y=0.05, color='black')+scale_color_discrete(labels=c("???????????????????????? ?? SA ?? control","???? ???????????????????????? ?? SA ?? control"))+theme(legend.text=element_text(size=13))

data_p_value_CA_control_holm<-as.data.frame(rbind(cbind(p_value_CA_control_holm_0, 0),cbind(p_value_CA_control_holm_1, 1)))
colnames(data_p_value_CA_control_holm)<-c("p_value", "Hi")
data_p_value_CA_control_holm$Hi<-factor(data_p_value_CA_control_holm$Hi)
data_p_value_CA_control_holm$color[data_p_value_CA_control_holm$Hi==0]<-"blue"
data_p_value_CA_control_holm$color[data_p_value_CA_control_holm$Hi==1]<-"red"
data_p_value_CA_control_holm<-data_p_value_CA_control_holm[order(data_p_value_CA_control_holm$p_value),]
ggplot(data_p_value_CA_control_holm, aes(x=1:length(data_p_value_CA_control_holm$p_value), y=p_value, color=color))+geom_point(size=0.5)+ggtitle("???????????????? ??????????")+xlab("")+geom_line(y=0.05, color='black')+scale_color_discrete(labels=c("???????????????????????? ?? CA ?? control","???? ???????????????????????? ?? CA ?? control"))+theme(legend.text=element_text(size=13))

holm_names_and_p_value_SA_control=data.frame(p_value_SA_control_holm, names_SA_control_holm)
holm_names_and_p_value_CA_control=data.frame(p_value_CA_control_holm, names_CA_control_holm)
holm_names_and_p_value_SA_CA=data.frame(p_value_SA_CA_holm, names_SA_CA_holm)

sort_holm_names_and_p_value_CA_control<-holm_names_and_p_value_CA_control[order(holm_names_and_p_value_CA_control$p_value_CA_control_holm), ]
sort_holm_names_and_p_value_SA_control<-holm_names_and_p_value_SA_control[order(holm_names_and_p_value_SA_control$p_value_SA_control_holm), ]
sort_holm_names_and_p_value_SA_CA<-holm_names_and_p_value_SA_CA[order(holm_names_and_p_value_SA_CA$p_value_SA_CA_holm), ]

#BY
p_value_SA_CA_BH_0<-NULL
p_value_SA_control_BH_0<-NULL
p_value_CA_control_BH_0<-NULL
p_value_SA_CA_BH_1<-NULL
p_value_SA_control_BH_1<-NULL
p_value_CA_control_BH_1<-NULL
names_SA_CA_BH<-NULL
names_SA_control_BH<-NULL
names_CA_control_BH<-NULL
for (i in 2:(dim(data_with_diff_mean)[2])){
  t_test<-pairwise.t.test(data_with_diff_mean[,i],data_with_diff_mean$Diagnosis, p.adjust.method="BY", paired=FALSE, pool.sd=FALSE)
  if (t_test$p.value[2,1]<=0.05){
    names_SA_CA_BH<-c(names_SA_CA_BH,names(data_with_diff_mean)[i])
    p_value_SA_CA_BH_0<-c(p_value_SA_CA_BH_0,t_test$p.value[2,1])
  }
  else{p_value_SA_CA_BH_1<-c(p_value_SA_CA_BH_1,t_test$p.value[2,1])}
  if (t_test$p.value[1,1]<=0.05){
    names_CA_control_BH<-c(names_CA_control_BH,names(data_with_diff_mean)[i])
    p_value_CA_control_BH_0<-c(p_value_CA_control_BH_0,t_test$p.value[1,1])
  }
  else{p_value_CA_control_BH_1<-c(p_value_CA_control_BH_1,t_test$p.value[1,1])}
  if (t_test$p.value[2,2]<=0.05){
    names_SA_control_BH<-c(names_SA_control_BH,names(data_with_diff_mean)[i])
    p_value_SA_control_BH_0<-c(p_value_SA_control_BH_0,t_test$p.value[2,2])
  }
  else{p_value_SA_control_BH_1<-c(p_value_SA_control_BH_1,t_test$p.value[2,2])}
}

data_p_value_SA_CA_BH<-as.data.frame(rbind(cbind(p_value_SA_CA_BH_0, 0),cbind(p_value_SA_CA_BH_1, 1)))
colnames(data_p_value_SA_CA_BH)<-c("p_value", "Hi")
data_p_value_SA_CA_BH$Hi<-factor(data_p_value_SA_CA_BH$Hi)
data_p_value_SA_CA_BH$color[data_p_value_SA_CA_BH$Hi==0]<-"blue"
data_p_value_SA_CA_BH$color[data_p_value_SA_CA_BH$Hi==1]<-"red"
data_p_value_SA_CA_BH<-data_p_value_SA_CA_BH[order(data_p_value_SA_CA_BH$p_value),]
ggplot(data_p_value_SA_CA_BH, aes(x=1:length(data_p_value_SA_CA_BH$p_value), y=p_value, color=color))+ scale_color_discrete(labels=c("???????????????????????? ?? SA ?? CA","???? ???????????????????????? ?? SA ?? CA"))+geom_point(size=0.5)+ggtitle("???????????????? ????????????????????-??????????????????")+xlab("")+geom_line(y=0.05, color='black')+theme(legend.text=element_text(size=13))+theme(legend.text=element_text(size=13))

data_p_value_SA_control_BH<-as.data.frame(rbind(cbind(p_value_SA_control_BH_0, 0),cbind(p_value_SA_control_BH_1, 1)))
colnames(data_p_value_SA_control_BH)<-c("p_value", "Hi")
data_p_value_SA_control_BH$Hi<-factor(data_p_value_SA_control_BH$Hi)
data_p_value_SA_control_BH$color[data_p_value_SA_control_BH$Hi==0]<-"blue"
data_p_value_SA_control_BH$color[data_p_value_SA_control_BH$Hi==1]<-"red"
data_p_value_SA_control_BH<-data_p_value_SA_control_BH[order(data_p_value_SA_control_BH$p_value),]
ggplot(data_p_value_SA_control_BH, aes(x=1:length(data_p_value_SA_control_BH$p_value), y=p_value, color=color))+geom_point(size=0.5)+ggtitle("???????????????? ????????????????????-??????????????????")+xlab("")+geom_line(y=0.05, color='black')+scale_color_discrete(labels=c("???????????????????????? ?? SA ?? control","???? ???????????????????????? ?? SA ?? control"))+theme(legend.text=element_text(size=13))+theme(legend.text=element_text(size=13))

data_p_value_CA_control_BH<-as.data.frame(rbind(cbind(p_value_CA_control_BH_0, 0),cbind(p_value_CA_control_BH_1, 1)))
colnames(data_p_value_CA_control_BH)<-c("p_value", "Hi")
data_p_value_CA_control_BH$Hi<-factor(data_p_value_CA_control_BH$Hi)
data_p_value_CA_control_BH$color[data_p_value_CA_control_BH$Hi==0]<-"blue"
data_p_value_CA_control_BH$color[data_p_value_CA_control_BH$Hi==1]<-"red"
data_p_value_CA_control_BH<-data_p_value_CA_control_BH[order(data_p_value_CA_control_BH$p_value),]
ggplot(data_p_value_CA_control_BH, aes(x=1:length(data_p_value_CA_control_BH$p_value), y=p_value, color=color))+geom_point(size=0.5)+ggtitle("???????????????? ????????????????????-??????????????????")+xlab("")+geom_line(y=0.05, color='black')+scale_color_discrete(labels=c("???????????????????????? ?? CA ?? control","???? ???????????????????????? ?? CA ?? control"))+theme(legend.text=element_text(size=13))+theme(legend.text=element_text(size=13))

BH_names_and_p_value_SA_control=data.frame(p_value_SA_control_BH_0, names_SA_control_BH)
BH_names_and_p_value_CA_control=data.frame(p_value_CA_control_BH_0, names_CA_control_BH)
BH_names_and_p_value_SA_CA=data.frame(p_value_SA_CA_BH_0, names_SA_CA_BH)

sort_BH_names_and_p_value_CA_control<-BH_names_and_p_value_CA_control[order(BH_names_and_p_value_CA_control$p_value_CA_control_BH_0), ]
sort_BH_names_and_p_value_SA_control<-BH_names_and_p_value_SA_control[order(BH_names_and_p_value_SA_control$p_value_SA_control_BH_0), ]
sort_BH_names_and_p_value_SA_CA<-BH_names_and_p_value_SA_CA[order(BH_names_and_p_value_SA_CA$p_value_SA_CA_BH_0), ]

#Error_bonferroni
bol<-FALSE
error_bonferroni_CA_control<-NULL
for (i in names_CA_control_none){
  for (j in names_CA_control_bonferroni){
    if (i==j){
      bol=TRUE
    }
  }
  if (bol==FALSE){
    error_bonferroni_CA_control<-c(error_bonferroni_CA_control, i)}
  bol<-FALSE
}

bol<-FALSE
error_bonferroni_SA_control<-NULL
for (i in names_SA_control_none){
  for (j in names_SA_control_bonferroni){
    if (i==j){
      bol=TRUE
    }
  }
  if (bol==FALSE){
    error_bonferroni_SA_control<-c(error_bonferroni_SA_control, i)}
  bol<-FALSE
}

bol<-FALSE
error_bonferroni_SA_CA<-NULL
for (i in names_SA_CA_none){
  for (j in names_SA_CA_bonferroni){
    if (i==j){
      bol=TRUE
    }
  }
  if (bol==FALSE){
    error_bonferroni_SA_CA<-c(error_bonferroni_SA_CA, i)}
  bol<-FALSE
}

#Error_holm
bol<-FALSE
error_holm_CA_control<-NULL
for (i in names_CA_control_none){
  for (j in names_CA_control_holm){
    if (i==j){
      bol=TRUE
    }
  }
  if (bol==FALSE){
    error_holm_CA_control<-c(error_holm_CA_control, i)}
  bol<-FALSE
}

bol<-FALSE
error_holm_SA_control<-NULL
for (i in names_SA_control_none){
  for (j in names_SA_control_holm){
    if (i==j){
      bol=TRUE
    }
  }
  if (bol==FALSE){
    error_holm_SA_control<-c(error_holm_SA_control, i)}
  bol<-FALSE
}

bol<-FALSE
error_holm_SA_CA<-NULL
for (i in names_SA_CA_none){
  for (j in names_SA_CA_holm){
    if (i==j){
      bol=TRUE
    }
  }
  if (bol==FALSE){
    error_holm_SA_CA<-c(error_holm_SA_CA, i)}
  bol<-FALSE
}


#Error_BH
bol<-FALSE
error_BH_CA_control<-NULL
for (i in names_CA_control_none){
  for (j in names_CA_control_BH){
    if (i==j){
      bol=TRUE
    }
  }
  if (bol==FALSE){
    error_BH_CA_control<-c(error_BH_CA_control, i)}
  bol<-FALSE
}

bol<-FALSE
error_BH_SA_control<-NULL
for (i in names_SA_control_none){
  for (j in names_SA_control_BH){
    if (i==j){
      bol=TRUE
    }
  }
  if (bol==FALSE){
    error_BH_SA_control<-c(error_BH_SA_control, i)}
  bol<-FALSE
}

bol<-FALSE
error_BH_SA_CA<-NULL
for (i in names_SA_CA_none){
  for (j in names_SA_CA_BH){
    if (i==j){
      bol=TRUE
    }
  }
  if (bol==FALSE){
    error_BH_SA_CA<-c(error_BH_SA_CA, i)}
  bol<-FALSE
}

#Noparametric method
library(dplyr)
library(coin)
library(PMCMR)
library(ggplot2)

setwd("E:/Diplom")
data_norm<-read.csv("data_without_normal.csv", row.names=1)
data_disp<-read.csv("data_without_disp.csv", row.names=1)
View(data_norm)
View(data_disp)
dim(data_norm)
dim(data_disp)
data_without_disp_norm<-data.frame(data_disp, data_norm[,2:3926])
write.csv(data_without_disp_norm, "data_without_disp_norm.csv")
data_without_disp_norm<-read.csv("data_without_disp_norm.csv", row.names=1)
View(data_without_disp_norm)
dim(data_without_disp_norm)
list_names_diff_mean<-NULL
gene_diff_mean<-NULL

for (i in 2:(dim(data_without_disp_norm)[2]))
{
  k_test<-kruskal.test(data_without_disp_norm[,i]~data_without_disp_norm$Diagnosis, data=data_without_disp_norm)
  if (k_test$p.value<0.05)
  {
    list_names_diff_mean<-c(list_names_diff_mean, names(data_without_disp_norm)[i])
  }
}

gene_diff_mean <-data_without_disp_norm[names(data_without_disp_norm) %in% (c(list_names_diff_mean,"Diagnosis") )]
#NONE
list_names_SA_CA_none<-NULL
p_value_SA_CA_none_0<-NULL
list_names_SA_control_none<-NULL
p_value_SA_control_none_0<-NULL
list_names_CA_control_none<-NULL
p_value_CA_control_none_0<-NULL
p_value_CA_control_none_1<-NULL
p_value_SA_control_none_1<-NULL
p_value_SA_CA_none_1<-NULL
for (i in 2:(dim(gene_diff_mean)[2]))
{
  dunn_test<-posthoc.kruskal.dunn.test(gene_diff_mean[,i], gene_diff_mean$Diagnosis, p.adjust.method ="none")
  if (dunn_test$p.value[1]<0.05)
  {
    list_names_CA_control_none<-c(list_names_CA_control_none,names(gene_diff_mean)[i])
    p_value_CA_control_none_0<-c(p_value_CA_control_none_0,dunn_test$p.value[1])
  }
  else{p_value_CA_control_none_1<-c(p_value_CA_control_none_1,dunn_test$p.value[1])}
  if (dunn_test$p.value[2]<0.05)
  {
    list_names_SA_CA_none<-c(list_names_SA_CA_none,names(gene_diff_mean)[i])
    p_value_SA_CA_none_0<-c(p_value_SA_CA_none_0,dunn_test$p.value[2])
  }
  else{p_value_SA_CA_none_1<-c(p_value_SA_CA_none_1,dunn_test$p.value[2])}
  if (dunn_test$p.value[4]<0.05)
  {
    list_names_SA_control_none<-c(list_names_SA_control_none, names(gene_diff_mean)[i])
    p_value_SA_control_none_0<-c(p_value_SA_control_none_0,dunn_test$p.value[4])
  }
  else{p_value_SA_control_none_1<-c(p_value_SA_control_none_1,dunn_test$p.value[4])}
}

data_p_value_SA_CA_none<-as.data.frame(rbind(cbind(p_value_SA_CA_none_0, 0),cbind(p_value_SA_CA_none_1, 1)))
colnames(data_p_value_SA_CA_none)<-c("p_value", "Hi")
data_p_value_SA_CA_none$Hi<-factor(data_p_value_SA_CA_none$Hi)
data_p_value_SA_CA_none$color[data_p_value_SA_CA_none$Hi==0]<-"blue"
data_p_value_SA_CA_none$color[data_p_value_SA_CA_none$Hi==1]<-"red"
data_p_value_SA_CA_none<-data_p_value_SA_CA_none[order(data_p_value_SA_CA_none$p_value),]
ggplot(data_p_value_SA_CA_none, aes(x=1:length(data_p_value_SA_CA_none$p_value), y=p_value, color=color))+ scale_color_discrete(labels=c("???????????????????????? ?? SA ?? CA","???? ???????????????????????? ?? SA ?? CA"))+geom_point(size=0.5)+ggtitle("?????? ????????????????")+xlab("")+geom_line(y=0.05, color='black')+theme(legend.text=element_text(size=13))

data_p_value_SA_control_none<-as.data.frame(rbind(cbind(p_value_SA_control_none_0, 0),cbind(p_value_SA_control_none_1, 1)))
colnames(data_p_value_SA_control_none)<-c("p_value", "Hi")
data_p_value_SA_control_none$Hi<-factor(data_p_value_SA_control_none$Hi)
data_p_value_SA_control_none$color[data_p_value_SA_control_none$Hi==0]<-"blue"
data_p_value_SA_control_none$color[data_p_value_SA_control_none$Hi==1]<-"red"
data_p_value_SA_control_none<-data_p_value_SA_control_none[order(data_p_value_SA_control_none$p_value),]
ggplot(data_p_value_SA_control_none, aes(x=1:length(data_p_value_SA_control_none$p_value), y=p_value, color=color))+geom_point(size=0.5)+ggtitle("?????? ????????????????")+xlab("")+geom_line(y=0.05, color='black')+scale_color_discrete(labels=c("???????????????????????? ?? SA ?? control","???? ???????????????????????? ?? SA ?? control"))+theme(legend.text=element_text(size=13))

data_p_value_CA_control_none<-as.data.frame(rbind(cbind(p_value_CA_control_none_0, 0),cbind(p_value_CA_control_none_1, 1)))
colnames(data_p_value_CA_control_none)<-c("p_value", "Hi")
data_p_value_CA_control_none$Hi<-factor(data_p_value_CA_control_none$Hi)
data_p_value_CA_control_none$color[data_p_value_CA_control_none$Hi==0]<-"blue"
data_p_value_CA_control_none$color[data_p_value_CA_control_none$Hi==1]<-"red"
data_p_value_CA_control_none<-data_p_value_CA_control_none[order(data_p_value_CA_control_none$p_value),]
ggplot(data_p_value_CA_control_none, aes(x=1:length(data_p_value_CA_control_none$p_value), y=p_value, color=color))+geom_point(size=0.5)+ggtitle("?????? ????????????????")+xlab("")+geom_line(y=0.05, color='black')+scale_color_discrete(labels=c("???????????????????????? ?? CA ?? control","???? ???????????????????????? ?? CA ?? control"))+theme(legend.text=element_text(size=13))


data_none_p_value_list_names_CA_control<-data.frame(p_value_CA_control_none_0, list_names_CA_control_none)
data_none_p_value_list_names_SA_control<-data.frame(p_value_SA_control_none_0, list_names_SA_control_none)
data_none_p_value_list_names_SA_CA<-data.frame(p_value_SA_CA_none_0, list_names_SA_CA_none)


#Sort
sort_none_data_p_value_names_CA_control<-data_none_p_value_list_names_CA_control[order(data_none_p_value_list_names_CA_control$p_value_CA_control_none_0),]
sort_none_data_p_value_names_SA_control<-data_none_p_value_list_names_SA_control[order(data_none_p_value_list_names_SA_control$p_value_SA_control_none_0),]
sort_none_data_p_value_names_SA_CA<-data_none_p_value_list_names_SA_CA[order(data_none_p_value_list_names_SA_CA$p_value_SA_CA_none_0),]


#BONFERRONI
list_names_SA_CA_bonferroni<-NULL
p_value_SA_CA_bonferroni_0<-NULL
list_names_SA_control_bonferroni<-NULL
p_value_SA_control_bonferroni_0<-NULL
list_names_CA_control_bonferroni<-NULL
p_value_CA_control_bonferroni_0<-NULL
p_value_SA_CA_bonferroni_1<-NULL
p_value_SA_control_bonferroni_1<-NULL
p_value_CA_control_bonferroni_1<-NULL
for (i in 2:(dim(gene_diff_mean)[2]))
{
  dunn_test<-posthoc.kruskal.dunn.test(gene_diff_mean[,i],gene_diff_mean$Diagnosis, method="bonferroni")
  if (dunn_test$p.value[1]<0.05)
  {
    list_names_CA_control_bonferroni<-c(list_names_CA_control_bonferroni,names(gene_diff_mean)[i])
    p_value_CA_control_bonferroni_0<-c(p_value_CA_control_bonferroni_0,dunn_test$p.value[1])
  }
  else{p_value_CA_control_bonferroni_1<-c(p_value_CA_control_bonferroni_1,dunn_test$p.value[1])}
  if (dunn_test$p.value[2]<0.05)
  {
    list_names_SA_CA_bonferroni<-c(list_names_SA_CA_bonferroni,names(gene_diff_mean)[i])
    p_value_SA_CA_bonferroni_0<-c(p_value_SA_CA_bonferroni_0,dunn_test$p.value[2])
  }
  else{p_value_SA_CA_bonferroni_1<-c(p_value_SA_CA_bonferroni_1,dunn_test$p.value[2])}
  if (dunn_test$p.value[4]<0.05)
  {
    list_names_SA_control_bonferroni<-c(list_names_SA_control_bonferroni, names(gene_diff_mean)[i])
    p_value_SA_control_bonferroni_0<-c(p_value_SA_control_bonferroni_0,dunn_test$p.value[4])
  }
  else{p_value_SA_control_bonferroni_1<-c(p_value_SA_control_bonferroni_1,dunn_test$p.value[4])}
}

data_p_value_SA_CA_bonferroni<-as.data.frame(rbind(cbind(p_value_SA_CA_bonferroni_0, 0),cbind(p_value_SA_CA_bonferroni_1, 1)))
colnames(data_p_value_SA_CA_bonferroni)<-c("p_value", "Hi")
data_p_value_SA_CA_bonferroni$Hi<-factor(data_p_value_SA_CA_bonferroni$Hi)
data_p_value_SA_CA_bonferroni$color[data_p_value_SA_CA_bonferroni$Hi==0]<-"blue"
data_p_value_SA_CA_bonferroni$color[data_p_value_SA_CA_bonferroni$Hi==1]<-"red"
data_p_value_SA_CA_bonferroni<-data_p_value_SA_CA_bonferroni[order(data_p_value_SA_CA_bonferroni$p_value),]
ggplot(data_p_value_SA_CA_bonferroni, aes(x=1:length(data_p_value_SA_CA_bonferroni$p_value), y=p_value, color=color))+ scale_color_discrete(labels=c("???????????????????????? ?? SA ?? CA","???? ???????????????????????? ?? SA ?? CA"))+geom_point(size=0.5)+ggtitle("???????????????? ????????????????????")+xlab("")+geom_line(y=0.05, color='black')+theme(legend.text=element_text(size=13))

data_p_value_SA_control_bonferroni<-as.data.frame(rbind(cbind(p_value_SA_control_bonferroni_0, 0),cbind(p_value_SA_control_bonferroni_1, 1)))
colnames(data_p_value_SA_control_bonferroni)<-c("p_value", "Hi")
data_p_value_SA_control_bonferroni$Hi<-factor(data_p_value_SA_control_bonferroni$Hi)
data_p_value_SA_control_bonferroni$color[data_p_value_SA_control_bonferroni$Hi==0]<-"blue"
data_p_value_SA_control_bonferroni$color[data_p_value_SA_control_bonferroni$Hi==1]<-"red"
data_p_value_SA_control_bonferroni<-data_p_value_SA_control_bonferroni[order(data_p_value_SA_control_bonferroni$p_value),]
ggplot(data_p_value_SA_control_bonferroni, aes(x=1:length(data_p_value_SA_control_bonferroni$p_value), y=p_value, color=color))+geom_point(size=0.5)+ggtitle("???????????????? ????????????????????")+xlab("")+geom_line(y=0.05, color='black')+scale_color_discrete(labels=c("???????????????????????? ?? SA ?? control","???? ???????????????????????? ?? SA ?? control"))+theme(legend.text=element_text(size=13))

data_p_value_CA_control_bonferroni<-as.data.frame(rbind(cbind(p_value_CA_control_bonferroni_0, 0),cbind(p_value_CA_control_bonferroni_1, 1)))
colnames(data_p_value_CA_control_bonferroni)<-c("p_value", "Hi")
data_p_value_CA_control_bonferroni$Hi<-factor(data_p_value_CA_control_bonferroni$Hi)
data_p_value_CA_control_bonferroni$color[data_p_value_CA_control_bonferroni$Hi==0]<-"blue"
data_p_value_CA_control_bonferroni$color[data_p_value_CA_control_bonferroni$Hi==1]<-"red"
data_p_value_CA_control_bonferroni<-data_p_value_CA_control_bonferroni[order(data_p_value_CA_control_bonferroni$p_value),]
ggplot(data_p_value_CA_control_bonferroni, aes(x=1:length(data_p_value_CA_control_bonferroni$p_value), y=p_value, color=color))+geom_point(size=0.5)+ggtitle("???????????????? ????????????????????")+xlab("")+geom_line(y=0.05, color='black')+scale_color_discrete(labels=c("???????????????????????? ?? CA ?? control","???? ???????????????????????? ?? CA ?? control"))+theme(legend.text=element_text(size=13))


data_bonferroni_p_value_list_names_CA_control<-data.frame(p_value_CA_control_bonferroni_0, list_names_CA_control_bonferroni)
data_bonferroni_p_value_list_names_SA_control<-data.frame(p_value_SA_control_bonferroni_0, list_names_SA_control_bonferroni)
data_bonferroni_p_value_list_names_SA_CA<-data.frame(p_value_SA_CA_bonferroni_0, list_names_SA_CA_bonferroni)


#Sort
sort_bonferroni_data_p_value_names_CA_control<-data_bonferroni_p_value_list_names_CA_control[order(data_bonferroni_p_value_list_names_CA_control$p_value_CA_control_bonferroni_0),]
sort_bonferroni_data_p_value_names_SA_control<-data_bonferroni_p_value_list_names_SA_control[order(data_bonferroni_p_value_list_names_SA_control$p_value_SA_control_bonferroni_0),]
sort_bonferroni_data_p_value_names_SA_CA<-data_bonferroni_p_value_list_names_SA_CA[order(data_bonferroni_p_value_list_names_SA_CA$p_value_SA_CA_bonferroni_0),]



#HOLM
list_names_SA_CA_holm<-NULL
p_value_SA_CA_holm_0<-NULL
list_names_SA_control_holm<-NULL
p_value_SA_control_holm_0<-NULL
list_names_CA_control_holm<-NULL
p_value_CA_control_holm_0<-NULL
p_value_CA_control_holm_1<-NULL
p_value_SA_control_holm_1<-NULL
p_value_SA_CA_holm_1<-NULL
for (i in 2:(dim(gene_diff_mean)[2]))
{
  dunn_test<-posthoc.kruskal.dunn.test(gene_diff_mean[,i],gene_diff_mean$Diagnosis, p.adjust.method ="holm")
  if (dunn_test$p.value[1]<0.05)
  {
    list_names_CA_control_holm<-c(list_names_CA_control_holm,names(gene_diff_mean)[i])
    p_value_CA_control_holm_0<-c(p_value_CA_control_holm_0,dunn_test$p.value[1])
  }
  else{p_value_CA_control_holm_1<-c(p_value_CA_control_holm_1,dunn_test$p.value[1])}
  if (dunn_test$p.value[2]<0.05)
  {
    list_names_SA_CA_holm<-c(list_names_SA_CA_holm,names(gene_diff_mean)[i])
    p_value_SA_CA_holm_0<-c(p_value_SA_CA_holm_0,dunn_test$p.value[2])
  }
  else{p_value_SA_CA_holm_1<-c(p_value_SA_CA_holm_1,dunn_test$p.value[2])}
  if (dunn_test$p.value[4]<0.05)
  {
    list_names_SA_control_holm<-c(list_names_SA_control_holm, names(gene_diff_mean)[i])
    p_value_SA_control_holm_0<-c(p_value_SA_control_holm_0,dunn_test$p.value[4])
  }
  else{p_value_SA_control_holm_1<-c(p_value_SA_control_holm_1,dunn_test$p.value[4])}
}

data_p_value_SA_CA_holm<-as.data.frame(rbind(cbind(p_value_SA_CA_holm_0, 0),cbind(p_value_SA_CA_holm_1, 1)))
colnames(data_p_value_SA_CA_holm)<-c("p_value", "Hi")
data_p_value_SA_CA_holm$Hi<-factor(data_p_value_SA_CA_holm$Hi)
data_p_value_SA_CA_holm$color[data_p_value_SA_CA_holm$Hi==0]<-"blue"
data_p_value_SA_CA_holm$color[data_p_value_SA_CA_holm$Hi==1]<-"red"
data_p_value_SA_CA_holm<-data_p_value_SA_CA_holm[order(data_p_value_SA_CA_holm$p_value),]
ggplot(data_p_value_SA_CA_holm, aes(x=1:length(data_p_value_SA_CA_holm$p_value), y=p_value, color=color))+ scale_color_discrete(labels=c("???????????????????????? ?? SA ?? CA","???? ???????????????????????? ?? SA ?? CA"))+geom_point(size=0.5)+ggtitle("???????????????? ??????????")+xlab("")+geom_line(y=0.05, color='black')+theme(legend.text=element_text(size=13))

data_p_value_SA_control_holm<-as.data.frame(rbind(cbind(p_value_SA_control_holm_0, 0),cbind(p_value_SA_control_holm_1, 1)))
colnames(data_p_value_SA_control_holm)<-c("p_value", "Hi")
data_p_value_SA_control_holm$Hi<-factor(data_p_value_SA_control_holm$Hi)
data_p_value_SA_control_holm$color[data_p_value_SA_control_holm$Hi==0]<-"blue"
data_p_value_SA_control_holm$color[data_p_value_SA_control_holm$Hi==1]<-"red"
data_p_value_SA_control_holm<-data_p_value_SA_control_holm[order(data_p_value_SA_control_holm$p_value),]
ggplot(data_p_value_SA_control_holm, aes(x=1:length(data_p_value_SA_control_holm$p_value), y=p_value, color=color))+geom_point(size=0.5)+ggtitle("???????????????? ??????????")+xlab("")+geom_line(y=0.05, color='black')+scale_color_discrete(labels=c("???????????????????????? ?? SA ?? control","???? ???????????????????????? ?? SA ?? control"))+theme(legend.text=element_text(size=13))

data_p_value_CA_control_holm<-as.data.frame(rbind(cbind(p_value_CA_control_holm_0, 0),cbind(p_value_CA_control_holm_1, 1)))
colnames(data_p_value_CA_control_holm)<-c("p_value", "Hi")
data_p_value_CA_control_holm$Hi<-factor(data_p_value_CA_control_holm$Hi)
data_p_value_CA_control_holm$color[data_p_value_CA_control_holm$Hi==0]<-"blue"
data_p_value_CA_control_holm$color[data_p_value_CA_control_holm$Hi==1]<-"red"
data_p_value_CA_control_holm<-data_p_value_CA_control_holm[order(data_p_value_CA_control_holm$p_value),]
ggplot(data_p_value_CA_control_holm, aes(x=1:length(data_p_value_CA_control_holm$p_value), y=p_value, color=color))+geom_point(size=0.5)+ggtitle("???????????????? ??????????")+xlab("")+geom_line(y=0.05, color='black')+scale_color_discrete(labels=c("???????????????????????? ?? CA ?? control","???? ???????????????????????? ?? CA ?? control"))+theme(legend.text=element_text(size=13))


data_holm_p_value_list_names_CA_control<-data.frame(p_value_CA_control_holm_0, list_names_CA_control_holm)
data_holm_p_value_list_names_SA_control<-data.frame(p_value_SA_control_holm_0, list_names_SA_control_holm)
data_holm_p_value_list_names_SA_CA<-data.frame(p_value_SA_CA_holm_0, list_names_SA_CA_holm)


#Sort
sort_holm_data_p_value_names_CA_control<-data_holm_p_value_list_names_CA_control[order(data_holm_p_value_list_names_CA_control$p_value_CA_control_holm_0),]
sort_holm_data_p_value_names_SA_control<-data_holm_p_value_list_names_SA_control[order(data_holm_p_value_list_names_SA_control$p_value_SA_control_holm_0),]
sort_holm_data_p_value_names_SA_CA<-data_holm_p_value_list_names_SA_CA[order(data_holm_p_value_list_names_SA_CA$p_value_SA_CA_holm_0),]



#BH
list_names_SA_CA_BH<-NULL
p_value_SA_CA_BH_0<-NULL
list_names_SA_control_BH<-NULL
p_value_SA_control_BH_0<-NULL
list_names_CA_control_BH<-NULL
p_value_CA_control_BH_0<-NULL
p_value_SA_CA_BH_1<-NULL
p_value_SA_control_BH_1<-NULL
p_value_CA_control_BH_1<-NULL
for (i in 2:(dim(gene_diff_mean)[2]))
{
  dunn_test<-posthoc.kruskal.dunn.test(gene_diff_mean[,i],gene_diff_mean$Diagnosis, p.adjust.methods="BY")
  if (dunn_test$p.value[1]<0.05)
  {
    list_names_CA_control_BH<-c(list_names_CA_control_BH,names(gene_diff_mean)[i])
    p_value_CA_control_BH_0<-c(p_value_CA_control_BH_0,dunn_test$p.value[1])
  }
  else{p_value_CA_control_BH_1<-c(p_value_CA_control_BH_1,dunn_test$p.value[1])}
  if (dunn_test$p.value[2]<0.05)
  {
    list_names_SA_CA_BH<-c(list_names_SA_CA_BH,names(gene_diff_mean)[i])
    p_value_SA_CA_BH_0<-c(p_value_SA_CA_BH_0,dunn_test$p.value[2])
  }
  else{p_value_SA_CA_BH_1<-c(p_value_SA_CA_BH_1,dunn_test$p.value[2])}
  if (dunn_test$p.value[4]<0.05)
  {
    list_names_SA_control_BH<-c(list_names_SA_control_BH, names(gene_diff_mean)[i])
    p_value_SA_control_BH_0<-c(p_value_SA_control_BH_0,dunn_test$p.value[4])
  }
  else{p_value_SA_control_BH_1<-c(p_value_SA_control_BH_1,dunn_test$p.value[4])}
}

data_p_value_SA_CA_BH<-as.data.frame(rbind(cbind(p_value_SA_CA_BH_0, 0),cbind(p_value_SA_CA_BH_1, 1)))
colnames(data_p_value_SA_CA_BH)<-c("p_value", "Hi")
data_p_value_SA_CA_BH$Hi<-factor(data_p_value_SA_CA_BH$Hi)
data_p_value_SA_CA_BH$color[data_p_value_SA_CA_BH$Hi==0]<-"blue"
data_p_value_SA_CA_BH$color[data_p_value_SA_CA_BH$Hi==1]<-"red"
data_p_value_SA_CA_BH<-data_p_value_SA_CA_BH[order(data_p_value_SA_CA_BH$p_value),]
ggplot(data_p_value_SA_CA_BH, aes(x=1:length(data_p_value_SA_CA_BH$p_value), y=p_value, color=color))+ scale_color_discrete(labels=c("???????????????????????? ?? SA ?? CA","???? ???????????????????????? ?? SA ?? CA"))+geom_point(size=0.5)+ggtitle("???????????????? ????????????????????-????????????????")+xlab("")+geom_line(y=0.05, color='black')+theme(legend.text=element_text(size=13))+theme(legend.text=element_text(size=13))

data_p_value_SA_control_BH<-as.data.frame(rbind(cbind(p_value_SA_control_BH_0, 0),cbind(p_value_SA_control_BH_1, 1)))
colnames(data_p_value_SA_control_BH)<-c("p_value", "Hi")
data_p_value_SA_control_BH$Hi<-factor(data_p_value_SA_control_BH$Hi)
data_p_value_SA_control_BH$color[data_p_value_SA_control_BH$Hi==0]<-"blue"
data_p_value_SA_control_BH$color[data_p_value_SA_control_BH$Hi==1]<-"red"
data_p_value_SA_control_BH<-data_p_value_SA_control_BH[order(data_p_value_SA_control_BH$p_value),]
ggplot(data_p_value_SA_control_BH, aes(x=1:length(data_p_value_SA_control_BH$p_value), y=p_value, color=color))+geom_point(size=0.5)+ggtitle("???????????????? ????????????????????-????????????????")+xlab("")+geom_line(y=0.05, color='black')+scale_color_discrete(labels=c("???????????????????????? ?? SA ?? control","???? ???????????????????????? ?? SA ?? control"))+theme(legend.text=element_text(size=13))+theme(legend.text=element_text(size=13))

data_p_value_CA_control_BH<-as.data.frame(rbind(cbind(p_value_CA_control_BH_0, 0),cbind(p_value_CA_control_BH_1, 1)))
colnames(data_p_value_CA_control_BH)<-c("p_value", "Hi")
data_p_value_CA_control_BH$Hi<-factor(data_p_value_CA_control_BH$Hi)
data_p_value_CA_control_BH$color[data_p_value_CA_control_BH$Hi==0]<-"blue"
data_p_value_CA_control_BH$color[data_p_value_CA_control_BH$Hi==1]<-"red"
data_p_value_CA_control_BH<-data_p_value_CA_control_BH[order(data_p_value_CA_control_BH$p_value),]
ggplot(data_p_value_CA_control_BH, aes(x=1:length(data_p_value_CA_control_BH$p_value), y=p_value, color=color))+geom_point(size=0.5)+ggtitle("???????????????? ????????????????????-????????????????")+xlab("")+geom_line(y=0.05, color='black')+scale_color_discrete(labels=c("???????????????????????? ?? CA ?? control","???? ???????????????????????? ?? CA ?? control"))+theme(legend.text=element_text(size=13))+theme(legend.text=element_text(size=13))



data_BH_p_value_list_names_CA_control<-data.frame(p_value_CA_control_BH_0, list_names_CA_control_BH)
data_BH_p_value_list_names_SA_control<-data.frame(p_value_SA_control_BH_0, list_names_SA_control_BH)
data_BH_p_value_list_names_SA_CA<-data.frame(p_value_SA_CA_BH_0, list_names_SA_CA_BH)


#Sort
sort_BH_data_p_value_names_CA_control<-data_BH_p_value_list_names_CA_control[order(data_BH_p_value_list_names_CA_control$p_value_CA_control_BH_0),]
sort_BH_data_p_value_names_SA_control<-data_BH_p_value_list_names_SA_control[order(data_BH_p_value_list_names_SA_control$p_value_SA_control_BH_0),]
sort_BH_data_p_value_names_SA_CA<-data_BH_p_value_list_names_SA_CA[order(data_BH_p_value_list_names_SA_CA$p_value_SA_CA_BH_0),]


#Error_bonferroni
bol<-FALSE
error_bonferroni_CA_control<-NULL
for (i in list_names_CA_control_none){
  for (j in list_names_CA_control_bonferroni){
    if (i==j){
      bol=TRUE
    }
  }
  if (bol==FALSE){
    error_bonferroni_CA_control<-c(error_bonferroni_CA_control, i)}
  bol<-FALSE
}

bol<-FALSE
error_bonferroni_SA_control<-NULL
for (i in list_names_SA_control_none){
  for (j in list_names_SA_control_bonferroni){
    if (i==j){
      bol=TRUE
    }
  }
  if (bol==FALSE){
    error_bonferroni_SA_control<-c(error_bonferroni_SA_control, i)}
  bol<-FALSE
}

bol<-FALSE
error_bonferroni_SA_CA<-NULL
for (i in list_names_SA_CA_none){
  for (j in list_names_SA_CA_bonferroni){
    if (i==j){
      bol=TRUE
    }
  }
  if (bol==FALSE){
    error_bonferroni_SA_CA<-c(error_bonferroni_SA_CA, i)}
  bol<-FALSE
}

#Error_holm
bol<-FALSE
error_holm_CA_control<-NULL
for (i in list_names_CA_control_none){
  for (j in list_names_CA_control_holm){
    if (i==j){
      bol=TRUE
    }
  }
  if (bol==FALSE){
    error_holm_CA_control<-c(error_holm_CA_control, i)}
  bol<-FALSE
}

bol<-FALSE
error_holm_SA_control<-NULL
for (i in list_names_SA_control_none){
  for (j in list_names_SA_control_holm){
    if (i==j){
      bol=TRUE
    }
  }
  if (bol==FALSE){
    error_holm_SA_control<-c(error_holm_SA_control, i)}
  bol<-FALSE
}

bol<-FALSE
error_holm_SA_CA<-NULL
for (i in list_names_SA_CA_none){
  for (j in list_names_SA_CA_holm){
    if (i==j){
      bol=TRUE
    }
  }
  if (bol==FALSE){
    error_holm_SA_CA<-c(error_holm_SA_CA, i)}
  bol<-FALSE
}


#Error_BH
bol<-FALSE
error_BH_CA_control<-NULL
for (i in list_names_CA_control_none){
  for (j in list_names_CA_control_BH){
    if (i==j){
      bol=TRUE
    }
  }
  if (bol==FALSE){
    error_BH_CA_control<-c(error_BH_CA_control, i)}
  bol<-FALSE
}

bol<-FALSE
error_BH_SA_control<-NULL
for (i in list_names_SA_control_none){
  for (j in list_names_SA_control_BH){
    if (i==j){
      bol=TRUE
    }
  }
  if (bol==FALSE){
    error_BH_SA_control<-c(error_BH_SA_control, i)}
  bol<-FALSE
}

bol<-FALSE
error_BH_SA_CA<-NULL
for (i in list_names_SA_CA_none){
  for (j in list_names_SA_CA_BH){
    if (i==j){
      bol=TRUE
    }
  }
  if (bol==FALSE){
    error_BH_SA_CA<-c(error_BH_SA_CA, i)}
  bol<-FALSE
}

#Diagramm
list_names_SA_CA_BH_not_notm<-read.csv("list_SA_CA_not_norm.csv", row.names = 1)
list_names_SA_control_BH_not_notm<-read.csv("list_SA_control_not_norm.csv", row.names = 1)
list_names_CA_control_BH_not_notm<-read.csv("list_CA_control_not_norm.csv", row.names = 1)
#NONE
general_names_SA_CA_none<-c(names_SA_CA_none, list_names_SA_CA_none)
general_names_SA_control_none<-c(names_SA_control_none, list_names_SA_control_none)
general_names_CA_control_none<-c(names_CA_control_none, list_names_CA_control_none)
intersect_SA_CA_and_CA_control_none<-intersect(general_names_CA_control_none, general_names_SA_CA_none)
intersect_SA_CA_and_SA_control_none<-intersect(general_names_SA_CA_none, general_names_SA_control_none)
intersect_CA_control_and_SA_control_none<-intersect(general_names_CA_control_none, general_names_SA_control_none)
intersect_inters_CA_control_and_SA_control_none<-intersect(intersect_CA_control_and_SA_control_none, intersect_SA_CA_and_CA_control_none)
intesect_SA_CA_none<-intersect(intersect_inters_CA_control_and_SA_control_none, intersect_SA_CA_and_SA_control_none)
#BONFERRONI
general_names_SA_CA_bonferroni<-c(names_SA_CA_bonferroni, list_names_SA_CA_bonferroni)
general_names_SA_control_bonferroni<-c(names_SA_control_bonferroni, list_names_SA_control_bonferroni)
general_names_CA_control_bonferroni<-c(names_CA_control_bonferroni, list_names_CA_control_bonferroni)
intersect_SA_CA_and_CA_control_bonferroni<-intersect(general_names_CA_control_bonferroni, general_names_SA_CA_bonferroni)
intersect_SA_CA_and_SA_control_bonferroni<-intersect(general_names_SA_CA_bonferroni, general_names_SA_control_bonferroni)
intersect_CA_control_and_SA_control_bonferroni<-intersect(general_names_CA_control_bonferroni, general_names_SA_control_bonferroni)
intersect_inters_CA_control_and_SA_control_bonferroni<-intersect(intersect_CA_control_and_SA_control_bonferroni, intersect_SA_CA_and_CA_control_bonferroni)
intesect_SA_CA_bonferroni<-intersect(intersect_inters_CA_control_and_SA_control_bonferroni, intersect_SA_CA_and_SA_control_bonferroni)
#HOLM
general_names_SA_CA_holm<-c(names_SA_CA_holm, list_names_SA_CA_holm)
general_names_SA_control_holm<-c(names_SA_control_holm, list_names_SA_control_holm)
general_names_CA_control_holm<-c(names_CA_control_holm, list_names_CA_control_holm)
intersect_SA_CA_and_CA_control_holm<-intersect(general_names_CA_control_holm, general_names_SA_CA_holm)
intersect_SA_CA_and_SA_control_holm<-intersect(general_names_SA_CA_holm, general_names_SA_control_holm)
intersect_CA_control_and_SA_control_holm<-intersect(general_names_CA_control_holm, general_names_SA_control_holm)
intersect_inters_CA_control_and_SA_control_holm<-intersect(intersect_CA_control_and_SA_control_holm, intersect_SA_CA_and_CA_control_holm)
intesect_SA_CA_holm<-intersect(intersect_inters_CA_control_and_SA_control_holm, intersect_SA_CA_and_SA_control_holm)

#BH
general_names_SA_CA<-c(names_SA_CA_BH, list_names_SA_CA_BH)
general_names_SA_control<-c(names_SA_control_BH, list_names_SA_control_BH)
general_names_CA_control<-c(names_CA_control_BH, list_names_CA_control_BH)
intersect_SA_CA_and_CA_control<-intersect(general_names_CA_control, general_names_SA_CA)
intersect_SA_CA_and_SA_control<-intersect(general_names_SA_CA, general_names_SA_control)
intersect_CA_control_and_SA_control<-intersect(general_names_CA_control, general_names_SA_control)
intersect_inters_CA_control_and_SA_control<-intersect(intersect_CA_control_and_SA_control, intersect_SA_CA_and_CA_control)
intesect_SA_CA<-intersect(intersect_inters_CA_control_and_SA_control, intersect_SA_CA_and_SA_control)
# Compare SA_CA and CA_control
general_genes_of_SA_CA_and_CA_control<-c()
general_genes_of_SA_CA_and_SA_control<-c()
general_genes_of_SA_control_and_CA_control<-c()
for (i in names_SA_CA){
  for (j in names_CA_control){
    if (i==j){general_genes_of_SA_CA_and_CA_control<-c(general_genes_of_SA_CA_and_CA_control,i)}
  }
}

for (i in names_SA_CA){
  for (j in names_SA_control){
    if (i==j){general_genes_of_SA_CA_and_SA_control<-c(general_genes_of_SA_CA_and_SA_control,i)}
  }
}
for (i in names_SA_control){
  for (j in names_CA_control){
    if (i==j){general_genes_of_SA_control_and_CA_control<-c(general_genes_of_SA_control_and_CA_control,i)}
  }
}
write.table(names_SA_CA, "Names_of_general_SA_CA.txt")
write.table(names_SA_control, "Names_of_general_SA_control.txt")
write.table(names_CA_control, "Names_of_general_CA_control.txt")

data_general_SA_CA_and_CA_control<-data.frame(Diagnosis=data_end[,1])
data_general_SA_CA_and_SA_control<-data.frame(Diagnosis=data_end[,1])
data_general_SA_control_and_CA_control<-data.frame(Diagnosis=data_end[,1])
for( i in general_genes_of_SA_CA_and_CA_control){
  data_general_SA_CA_and_CA_control[,i]<-data_SA_CA[,grep(i, colnames(data_SA_CA))]
}
for( i in general_genes_of_SA_CA_and_SA_control){
  data_general_SA_CA_and_SA_control[,i]<-data_SA_CA[,grep(i, colnames(data_SA_CA))]
}
for( i in general_genes_of_SA_control_and_CA_control){
  data_general_SA_control_and_CA_control[,i]<-data_SA_control[,grep(i, colnames(data_SA_control))]
}
write.csv(data_general_SA_CA_and_CA_control, "General_genes_SA_CA_and_CA_control.csv")
write.csv(data_general_SA_CA_and_SA_control, "General_genes_SA_CA_and_SA_control.csv")
write.csv(data_general_SA_control_and_CA_control, "General_genes_SA_control_and_CA_control.csv")



