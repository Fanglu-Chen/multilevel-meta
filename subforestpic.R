library(metafor)
library(dmetar)
library(tidyverse)
library(forestplot)
library(openxlsx)
library(meta)
library(openxlsx)
#1:2123:37
datasub<-read.csv("C:\\Users\\chenf\\Desktop\\metadzj\\subgrouppic.csv")[39:45,1:17]
#data$p<-c(" ","0.0002","0.0869","<.0001","0.0094"," ","<.0001",
#          '<.0001'," ","0.3047","0.2371")
#                                                                    data$Estimate, data$LCI, data$UCI))%>%paste(data$Estimategap)
tm<-forest_theme(core = list(bg_params=list(fill = c("white"))),base_family = "sans",base_size = 10,
                 ci_pch = 15,ci_col = "black",ci_fill = "darkblue",
                 refline_col = "grey20",ci_Theight = 0.2,
                 refline_lty = 2,
                 summary_fill = "white",       #汇总部分大菱形的颜色
                 summary_col = "black")

plotdata = data.frame(X = c(1:7))
plotdata$`Economics Outcomes` =  datasub$X
plotdata$Subgroup = datasub$Subgroup
plotdata$`# of study` = datasub$X..of.study
plotdata$`Intervention (N)` = datasub$Intervention..N.
plotdata$`Control (N)` = datasub$Control..N.
plotdata$` ` <- paste(rep("   ", 11), collapse = " ")
plotdata$`Type of estimate` = datasub$Type.of.estimate
plotdata$`Estimate(95% CI)` <- ifelse(is.na(datasub$Estmate), " ",
                                      sprintf("%.2f (%.2f, %.2f)",
                                              datasub$Estmate, 
                                              datasub$LCI, 
                                              datasub$UCI))
plotdata$`% Weight` = datasub$X..Weight
plotdata$`P Value` = datasub$P.value
plotdata$`I²` = datasub$I2
#plotdata$`Q` = as.character(datasub$Q)
plotdata$`Q` = c(datasub$Q[1],rep("",6),
                 datasub$Q[8],rep("",3),
                 datasub$Q[12],rep("",6),
                 datasub$Q[19],rep("",2))
plotdata$`Df` = c(datasub$Df[1],rep("",6),
                 datasub$Df[8],rep("",3),
                 datasub$Df[12],rep("",6),
                 datasub$Df[19],rep("",2))
plotdata$`Q` = c(datasub$Q[1],rep("",7),
                 datasub$Q[9],rep("",6))
plotdata$`Df` = c(datasub$Df[1],rep("",7),
                  datasub$Df[9],rep("",6))
plotdata$`Q` = c(datasub$Q[1],rep("",2),
                 datasub$Q[4],rep("",3))
plotdata$`Df` = c(datasub$Df[1],rep("",2),
                  datasub$Df[4],rep("",3))


plotdata$`P Value of Heterogeneity` = datasub$P.value.1


A<-as.matrix(plotdata[,-1])
#col=c(1,15,3:4,13,17,16,9:11,14)
p1 <- forest(A,
             est = datasub$Estmate,
             lower = datasub$LCI,
             upper = datasub$UCI,
             #sizes = dt$se,
             ci_column =6 ,
             ref_line = 0,
             is_summary = c(rep(T, 7)),
             #arrow_lab = c("Placebo Better", "Treatment Better"),
             xlim = c(-12000, 2000),
             ticks_at = c(-12000,-6000,-2000,0,2000),
             #footnote = "This is the demo data. Please feel free to change\nanything you want.",
             theme = tm)
print(p1)
library(tidyverse)
g1<-add_border(p1, 
               part = "body", 
               #col="lightgrey",
               row = c(1:5),
               col = c(1:4,6:11),gp=gpar(col="lightgrey",
                                         lwd=1.5))%>%add_border(part = "header",col = c(1:4,6:11),row = 1)
print(g1)
p2<-forest(data[6:8,col],
           est = data$Estimate[6:8],
           lower = data$LCI[6:8],
           upper = data$UCI[6:8],
           #sizes = dt$se,
           ci_column =5 ,
           ref_line = 1,vert_line = 0,
           #arrow_lab = c("Placebo Better", "Treatment Better"),
           xlim = c(-1,6),
           ticks_at = c(seq(-1,6,1)),
           #footnote = "This is the demo data. Please feel free to change\nanything you want.",
           theme = tm)
print(p2)
library(tidyverse)
g2<-add_border(p2, 
               part = "body", 
               #col="lightgrey",
               row = c(1:2),
               col = c(1:4,6:11),gp=gpar(col="lightgrey",
                                         lwd=1.5))%>%add_border(part = "header",col = c(1:4,6:11),row = 1)
print(g2)
p3<-forest(data[9:11,col],
           est = data$Estimate[9:11],
           lower = data$LCI[9:11],
           upper = data$UCI[9:11],
           #sizes = dt$se,
           ci_column =5 ,
           ref_line = 0,
           #arrow_lab = c("Placebo Better", "Treatment Better"),
           xlim = c(-4000,1000),
           ticks_at = c(seq(-4000,1000,1000)),
           #footnote = "This is the demo data. Please feel free to change\nanything you want.",
           theme = tm)
print(p3)
library(tidyverse)
g3<-add_border(p3, 
               part = "body", 
               #col="lightgrey",
               row = c(1:2),
               col = c(1:4,6:11),gp=gpar(col="lightgrey",
                                         lwd=1.5))%>%add_border(part = "header",col = c(1:4,6:11),row = 1)
print(g3)
setwd("C:\\Users\\chenf\\Desktop\\metadzj\\suppic2402")
tiff("mainage.tif",res=300,width = 1650*3,height = 280*3)
p1
#grid.arrange(g1,g2,g3)
dev.off()
options(scipen = 200)
library(cowplot)
library(gridExtra)

