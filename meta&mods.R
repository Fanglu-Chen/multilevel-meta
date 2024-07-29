library(metafor)
library(dmetar)
library(tidyverse)

for (i in c(1:3,7,9)) {
  data<-datasheet[[i]]
  data$`Age.Group`=factor(data$`Age.Group`)
  name<-shtnames[i]
  model<-escalc(measure="OR", ai=IntEvent, bi=IntFailure,
                ci=CtrlEvent, 
                di=CtrlFailure,data=data)%>%
    rma.mv(data=.,yi, vi, random = ~ 1 |Disease/No.,test = 't',
           method = 'REML', mods = ~ Age.Group)
  I<-model%>%var.comp()
  data$W_FU <- model%>%weights()
  print(name)
  print(model)
  print(I)}



for (s in c(4,5,6,8)) {
  name<-shtnames[s]
  data=datasheet[[s]]
  data$`Age.Group`=factor(data$`Age.Group`)
  
  model<-escalc(measure = 'MD',m1i = IM,sd1i = ISD,n1i = TotInt
                ,m2i = CM,
                sd2i = CSD,n2i = TotCtrl
                ,data=data)%>%
    rma.mv(data=.,yi, vi, random = ~ 1 | Disease/No., test = 't',
           method = 'REML', mods = ~ Age.Group)
  I<-model%>%var.comp()
  print(name)
  print(model)
  print(I)
  
}