d<-read.csv("data/WA_Fn-UseC_-HR-Employee-Attrition.csv")
colnames(d)[1] = "Age"
d$Attrition= as.integer(as.factor(d$Attrition))-1
#d$BusinessTravel= as.integer(as.factor(d$BusinessTravel))
#d$Department= as.integer(as.factor(d$Department))
#d$Gender= as.integer(as.factor(d$Gender))
#d$JobRole= as.integer(as.factor(d$JobRole))
#d$MaritalStatus= as.integer(as.factor(d$MaritalStatus))
#d$OverTime= as.integer(as.factor(d$OverTime))
#d$EducationField= as.integer(as.factor(d$EducationField))
#d$StandardHours<-NULL
#d$PerformanceRating<-NULL
#d$Over18<-NULL
#d$EmployeeCount<-NULL
#d$JobLevel<-NULL
#d$DailyRate<-NULL
#d$HourlyRate<-NULL
#d$DailyRate<-NULL
#d$MonthlyRate<-NULL
#d$PercentSalaryHike<-NULL

library(ggplot2)
library(grid)
library(gridExtra)
library(ggthemes)

d$Education = as.factor(d$Education)

Age_plot1 <- ggplot(d, aes(as.factor(Attrition),Age,fill=Education)) + 
             geom_boxplot(width = 0.5) +xlab("Attrition") + 
             theme(axis.text=element_text(size=12,face="bold"),axis.title=element_text(size=14,face="bold"),legend.text=element_text(size=10),legend.title=element_text(size=14)) +
             theme_hc()+ scale_colour_hc()


Age_plot2 <- ggplot(d, aes(as.factor(Attrition),Age,fill=Gender)) +
             geom_boxplot(width = 0.4) +xlab("Attrition")+
             theme(axis.text=element_text(size=12,face="bold"),axis.title=element_text(size=14,face="bold"),legend.text=element_text(size=10),legend.title=element_text(size=14)) +
             theme_hc()+ scale_colour_hc()


Income_plot1 <- ggplot(d, aes(as.factor(Attrition),MonthlyIncome,fill=Education)) + 
                geom_boxplot(width = 0.5)+xlab("Attrition")+
                theme(axis.text=element_text(size=12,face="bold"),axis.title=element_text(size=14,face="bold"),legend.text=element_text(size=10),legend.title=element_text(size=14)) +
                theme_hc()+ scale_colour_hc()


Income_plot2 <- ggplot(d, aes(as.factor(Attrition),MonthlyIncome,fill=Gender)) + 
                geom_boxplot(width = 0.4) +xlab("Attrition") + 
                theme(axis.text=element_text(size=12,face="bold"),axis.title=element_text(size=14,face="bold"),legend.text=element_text(size=10),legend.title=element_text(size=14)) +
                theme_hc()+ scale_colour_hc()


#grid.arrange(Age_plot1,Income_plot1,ncol = 2, top = textGrob("Boxplots of Age and MonthlyIncome, grid by Education",gp=gpar(fontsize=15,font=2)))

#grid.arrange(Age_plot2,Income_plot2,ncol = 2, top = textGrob("Boxplots of Age and MonthlyIncome, grid by Gender",gp=gpar(fontsize=15,font=2)))

d$Attrition = as.factor(d$Attrition)
ggplot(d, aes(MonthlyIncome,Age, color=Attrition,shape=Attrition))+ geom_point(data = transform(d, Education = NULL, Gender = NULL), colour = "grey85") + geom_point(size = 2,alpha = 0.7)+ facet_grid(Education~Gender,labeller = "label_both") + theme(axis.text=element_text(size=12,face="bold"),axis.title=element_text(size=14,face="bold"),strip.text.x = element_text(size = 14),legend.text=element_text(size=10),legend.title=element_text(size=14)) +theme_hc()+ scale_colour_hc()

