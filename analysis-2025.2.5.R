#species accumulative curve———————————————————————————————
{
#bird——————————————
a = read.table('clipboard',header=TRUE)
library("vegan")
library("picante")
library("fossil")
sp1 = specaccum(a, method="random")
plot(sp1, ci.type="poly", col="black", lwd=1, ci.lty=0, ci.col="gray85")
boxplot(sp1, col="white", add=TRUE, pch="+")

#arthropod——————————————
a = read.table('clipboard',header=TRUE)
library("vegan")
library("picante")
library("fossil")
sp1 = specaccum(a, method="random")
plot(sp1, ci.type="poly", col="black", lwd=1, ci.lty=0, ci.col="gray85")
boxplot(sp1, col="white", add=TRUE, pch="+")

#plant——————————————
a = read.table('clipboard',header=TRUE)
library("vegan")
library("picante")
library("fossil")
sp1 = specaccum(a, method="random")
plot(sp1, ci.type="poly", col="black", lwd=1, ci.lty=0, ci.col="gray85")
boxplot(sp1, col="white", add=TRUE, pch="+")
}

#ANOVA：degradation———————————————————————————————————————
{
data1 = read.table('clipboard',header=TRUE)
#y
data2 = aov(bric2~habitat,data =data1) 
summary(data2)
TukeyHSD(data2)
data2 = aov(babu2~habitat,data =data1) 
summary(data2)
TukeyHSD(data2)
data2 = aov(bsim2~habitat,data =data1) 
summary(data2)
TukeyHSD(data2)
data2 = aov(bsha2~habitat,data =data1) 
summary(data2)
TukeyHSD(data2)
data2 = aov(bpie2~habitat,data =data1) 
summary(data2)
TukeyHSD(data2)

data2 = aov(aric2~habitat,data =data1) 
summary(data2)
TukeyHSD(data2)
data2 = aov(aabu2~habitat,data =data1) 
summary(data2)
TukeyHSD(data2)
data2 = aov(asim2~habitat,data =data1) 
summary(data2)
TukeyHSD(data2)
data2 = aov(asha2~habitat,data =data1) 
summary(data2)
TukeyHSD(data2)
data2 = aov(apie2~habitat,data =data1) 
summary(data2)
TukeyHSD(data2)

data2 = aov(pric2~habitat,data =data1) 
summary(data2)
TukeyHSD(data2)
data2 = aov(pcov2~habitat,data =data1) 
summary(data2)
TukeyHSD(data2)
data2 = aov(ppie2~habitat,data =data1) 
summary(data2)
TukeyHSD(data2)

data1 = read.table('clipboard',header=TRUE)
data2 = aov(vegsim~habitat,data =data1) 
summary(data2)
TukeyHSD(data2)
data2 = aov(vegsha~habitat,data =data1)
summary(data2)
TukeyHSD(data2)

#biomass
data1 = read.table('clipboard',header=TRUE)
data2 = aov(bma2~habitat,data =data1) 
summary(data2)
TukeyHSD(data2)
data2 = aov(ama2~habitat,data =data1) 
summary(data2)
TukeyHSD(data2)
data2 = aov(pma2~habitat,data =data1) 
summary(data2)
TukeyHSD(data2)

#feeding habits~degradation
data1 = read.table('clipboard',header=TRUE)
data2 = aov(食虫2~habitat,data =data1) 
summary(data2)
TukeyHSD(data2)
data2 = aov(食肉2~habitat,data =data1) 
summary(data2)
TukeyHSD(data2)
data2 = aov(食植2~habitat,data =data1) 
summary(data2)
TukeyHSD(data2)
data2 = aov(杂食2~habitat,data =data1) 
summary(data2)
TukeyHSD(data2)

data1 = read.table('clipboard',header=TRUE)
data2 = aov(寄生2~habitat,data =data1) 
summary(data2)
TukeyHSD(data2)
data2 = aov(食腐2~habitat,data =data1) 
summary(data2)
TukeyHSD(data2)
data2 = aov(食肉2~habitat,data =data1) 
summary(data2)
TukeyHSD(data2)
data2 = aov(食植2~habitat,data =data1) 
summary(data2)
TukeyHSD(data2)
data2 = aov(杂食2~habitat,data =data1) 
summary(data2)
TukeyHSD(data2)

#feeding habits~degradation（arthropod shannon & abundence）
data1 = read.table('clipboard',header=TRUE)
data2 = aov(寄生2~habitat,data =data1) 
summary(data2)
TukeyHSD(data2)
data2 = aov(食腐2~habitat,data =data1) 
summary(data2)
TukeyHSD(data2)
data2 = aov(食肉2~habitat,data =data1) 
summary(data2)
TukeyHSD(data2)
data2 = aov(食植2~habitat,data =data1) 
summary(data2)
TukeyHSD(data2)
data2 = aov(杂食2~habitat,data =data1) 
summary(data2)
TukeyHSD(data2)

data2 = aov(寄生~habitat,data =data1) 
summary(data2)
TukeyHSD(data2)
data2 = aov(食腐~habitat,data =data1) 
summary(data2)
TukeyHSD(data2)
data2 = aov(食肉~habitat,data =data1) 
summary(data2)
TukeyHSD(data2)
data2 = aov(食植~habitat,data =data1) 
summary(data2)
TukeyHSD(data2)
data2 = aov(杂食~habitat,data =data1) 
summary(data2)
TukeyHSD(data2)
}

#plot1
{
one = read.table('clipboard',header=TRUE)
#richness~degradation——————————————1
  {
#bird
one$habitat = factor(one$habitat,levels = c("OG","LG","MG","SG"))
p  =  ggboxplot(one,x='habitat', y='bric',bxp.errorbar = T,bxp.errorbar.width = 0.2)+
  labs(x='Degradations',y='richness')+
  theme(plot.title = element_text(colour = 'black',size = 14,hjust = 0.5),
        axis.text.x = element_text(colour ='black',size = 9,angle = 0),
        axis.text.y = element_text(colour ='black',size = 9,angle = 0),
        axis.title.x = element_text(colour ='black',size = 12,angle = 0),
        axis.title.y = element_text(colour ='black',size = 12,angle = 90),
        legend.title = element_text(colour ='black',size =16),
        legend.text = element_text(colour ='black',size = 16),
        axis.line.y = element_line(colour = 'black',linetype = 'solid'),
        axis.line.x = element_line(colour = 'black',linetype = 'solid'),
        panel.border = element_rect(linetype = 'solid',size = 0.5,fill=NA))
p

#arthropod
one$habitat = factor(one$habitat,levels = c("OG","LG","MG","SG"))
p  =  ggboxplot(one,x='habitat', y='aric',bxp.errorbar = T,bxp.errorbar.width = 0.2)+
  labs(x='Degradations',y='richness')+
  theme(plot.title = element_text(colour = 'black',size = 14,hjust = 0.5),
        axis.text.x = element_text(colour ='black',size = 9,angle = 0),
        axis.text.y = element_text(colour ='black',size = 9,angle = 0),
        axis.title.x = element_text(colour ='black',size = 12,angle = 0),
        axis.title.y = element_text(colour ='black',size = 12,angle = 90),
        legend.title = element_text(colour ='black',size =16),
        legend.text = element_text(colour ='black',size = 16),
        axis.line.y = element_line(colour = 'black',linetype = 'solid'),
        axis.line.x = element_line(colour = 'black',linetype = 'solid'),
        panel.border = element_rect(linetype = 'solid',size = 0.5,fill=NA))
p

#plant
one$habitat = factor(one$habitat,levels = c("OG","LG","MG","SG"))
p  =  ggboxplot(one,x='habitat', y='pric',bxp.errorbar = T,bxp.errorbar.width = 0.2)+
  labs(x='Degradations',y='richness')+
  theme(plot.title = element_text(colour = 'black',size = 14,hjust = 0.5),
        axis.text.x = element_text(colour ='black',size = 9,angle = 0),
        axis.text.y = element_text(colour ='black',size = 9,angle = 0),
        axis.title.x = element_text(colour ='black',size = 12,angle = 0),
        axis.title.y = element_text(colour ='black',size = 12,angle = 90),
        legend.title = element_text(colour ='black',size =16),
        legend.text = element_text(colour ='black',size = 16),
        axis.line.y = element_line(colour = 'black',linetype = 'solid'),
        axis.line.x = element_line(colour = 'black',linetype = 'solid'),
        panel.border = element_rect(linetype = 'solid',size = 0.5,fill=NA))#上右边框
p
  }
#abundance~degradation——————————————2
  {
#bird
one$habitat = factor(one$habitat,levels = c("OG","LG","MG","SG"))
p  =  ggboxplot(one,x='habitat', y='babu',bxp.errorbar = T,bxp.errorbar.width = 0.2)+
  labs(x='Degradations',y='abundance')+
  theme(plot.title = element_text(colour = 'black',size = 14,hjust = 0.5),
        axis.text.x = element_text(colour ='black',size = 9,angle = 0),
        axis.text.y = element_text(colour ='black',size = 9,angle = 0),
        axis.title.x = element_text(colour ='black',size = 12,angle = 0),
        axis.title.y = element_text(colour ='black',size = 12,angle = 90),
        legend.title = element_text(colour ='black',size =16),
        legend.text = element_text(colour ='black',size = 16),
        axis.line.y = element_line(colour = 'black',linetype = 'solid'),
        axis.line.x = element_line(colour = 'black',linetype = 'solid'),
        panel.border = element_rect(linetype = 'solid',size = 0.5,fill=NA))
p

#arthropod
one$habitat = factor(one$habitat,levels = c("OG","LG","MG","SG"))
p  =  ggboxplot(one,x='habitat', y='aabu',bxp.errorbar = T,bxp.errorbar.width = 0.2)+
  labs(x='Degradations',y='abundance')+
  theme(plot.title = element_text(colour = 'black',size = 14,hjust = 0.5),
        axis.text.x = element_text(colour ='black',size = 9,angle = 0),
        axis.text.y = element_text(colour ='black',size = 9,angle = 0),
        axis.title.x = element_text(colour ='black',size = 12,angle = 0),
        axis.title.y = element_text(colour ='black',size = 12,angle = 90),
        legend.title = element_text(colour ='black',size =16),
        legend.text = element_text(colour ='black',size = 16),
        axis.line.y = element_line(colour = 'black',linetype = 'solid'),
        axis.line.x = element_line(colour = 'black',linetype = 'solid'),
        panel.border = element_rect(linetype = 'solid',size = 0.5,fill=NA))
p

#plant
one$habitat = factor(one$habitat,levels = c("OG","LG","MG","SG"))
p  =  ggboxplot(one,x='habitat', y='pcov',bxp.errorbar = T,bxp.errorbar.width = 0.2)+
  labs(x='Degradations',y='cover')+
  theme(plot.title = element_text(colour = 'black',size = 14,hjust = 0.5),
        axis.text.x = element_text(colour ='black',size = 9,angle = 0),
        axis.text.y = element_text(colour ='black',size = 9,angle = 0),
        axis.title.x = element_text(colour ='black',size = 12,angle = 0),
        axis.title.y = element_text(colour ='black',size = 12,angle = 90),
        legend.title = element_text(colour ='black',size =16),
        legend.text = element_text(colour ='black',size = 16),
        axis.line.y = element_line(colour = 'black',linetype = 'solid'),
        axis.line.x = element_line(colour = 'black',linetype = 'solid'),
        panel.border = element_rect(linetype = 'solid',size = 0.5,fill=NA))
p
  }
#simpson~degradation——————————————3
  {
#bird
one$habitat = factor(one$habitat,levels = c("OG","LG","MG","SG"))
p  =  ggboxplot(one,x='habitat', y='bsim',bxp.errorbar = T,bxp.errorbar.width = 0.2)+
  labs(x='Degradations',y='Simpson index')+
  theme(plot.title = element_text(colour = 'black',size = 14,hjust = 0.5),
        axis.text.x = element_text(colour ='black',size = 9,angle = 0),
        axis.text.y = element_text(colour ='black',size = 9,angle = 0),
        axis.title.x = element_text(colour ='black',size = 12,angle = 0),
        axis.title.y = element_text(colour ='black',size = 12,angle = 90),
        legend.title = element_text(colour ='black',size =16),
        legend.text = element_text(colour ='black',size = 16),
        axis.line.y = element_line(colour = 'black',linetype = 'solid'),
        axis.line.x = element_line(colour = 'black',linetype = 'solid'),
        panel.border = element_rect(linetype = 'solid',size = 0.5,fill=NA))
p

#arthropod
one$habitat = factor(one$habitat,levels = c("OG","LG","MG","SG"))
p  =  ggboxplot(one,x='habitat', y='asim',bxp.errorbar = T,bxp.errorbar.width = 0.2)+
  labs(x='Degradations',y='Simpson index')+
  theme(plot.title = element_text(colour = 'black',size = 14,hjust = 0.5),
        axis.text.x = element_text(colour ='black',size = 9,angle = 0),
        axis.text.y = element_text(colour ='black',size = 9,angle = 0),
        axis.title.x = element_text(colour ='black',size = 12,angle = 0),
        axis.title.y = element_text(colour ='black',size = 12,angle = 90),
        legend.title = element_text(colour ='black',size =16),
        legend.text = element_text(colour ='black',size = 16),
        axis.line.y = element_line(colour = 'black',linetype = 'solid'),
        axis.line.x = element_line(colour = 'black',linetype = 'solid'),
        panel.border = element_rect(linetype = 'solid',size = 0.5,fill=NA))
p

#plant
one$habitat = factor(one$habitat,levels = c("OG","LG","MG","SG"))
p  =  ggboxplot(one,x='habitat', y='vegsim2',bxp.errorbar = T,bxp.errorbar.width = 0.2)+
  labs(x='Degradations',y='Simpson index')+
  theme(plot.title = element_text(colour = 'black',size = 14,hjust = 0.5),
        axis.text.x = element_text(colour ='black',size = 9,angle = 0),
        axis.text.y = element_text(colour ='black',size = 9,angle = 0),
        axis.title.x = element_text(colour ='black',size = 12,angle = 0),
        axis.title.y = element_text(colour ='black',size = 12,angle = 90),
        legend.title = element_text(colour ='black',size =16),
        legend.text = element_text(colour ='black',size = 16),
        axis.line.y = element_line(colour = 'black',linetype = 'solid'),
        axis.line.x = element_line(colour = 'black',linetype = 'solid'),
        panel.border = element_rect(linetype = 'solid',size = 0.5,fill=NA))
p
  }

one = read.table('clipboard',header=TRUE)
#shannon~degradation——————————————4
  {
#bird
one$habitat = factor(one$habitat,levels = c("OG","LG","MG","SG"))
p  =  ggboxplot(one,x='habitat', y='bsha',bxp.errorbar = T,bxp.errorbar.width = 0.2)+
  labs(x='Degradations',y='Shannon index')+
  theme(plot.title = element_text(colour = 'black',size = 14,hjust = 0.5),
        axis.text.x = element_text(colour ='black',size = 9,angle = 0),
        axis.text.y = element_text(colour ='black',size = 9,angle = 0),
        axis.title.x = element_text(colour ='black',size = 12,angle = 0),
        axis.title.y = element_text(colour ='black',size = 12,angle = 90),
        legend.title = element_text(colour ='black',size =16),
        legend.text = element_text(colour ='black',size = 16),
        axis.line.y = element_line(colour = 'black',linetype = 'solid'),
        axis.line.x = element_line(colour = 'black',linetype = 'solid'),
        panel.border = element_rect(linetype = 'solid',size = 0.5,fill=NA))
p

#arthropod
one$habitat = factor(one$habitat,levels = c("OG","LG","MG","SG"))
p  =  ggboxplot(one,x='habitat', y='asha',bxp.errorbar = T,bxp.errorbar.width = 0.2)+
  labs(x='Degradations',y='Shannon index')+
  theme(plot.title = element_text(colour = 'black',size = 14,hjust = 0.5),
        axis.text.x = element_text(colour ='black',size = 9,angle = 0),
        axis.text.y = element_text(colour ='black',size = 9,angle = 0),
        axis.title.x = element_text(colour ='black',size = 12,angle = 0),
        axis.title.y = element_text(colour ='black',size = 12,angle = 90),
        legend.title = element_text(colour ='black',size =16),
        legend.text = element_text(colour ='black',size = 16),
        axis.line.y = element_line(colour = 'black',linetype = 'solid'),
        axis.line.x = element_line(colour = 'black',linetype = 'solid'),
        panel.border = element_rect(linetype = 'solid',size = 0.5,fill=NA))
p

one = read.table('clipboard',header=TRUE)
#plant
one$habitat = factor(one$habitat,levels = c("OG","LG","MG","SG"))
p  =  ggboxplot(one,x='habitat', y='vegsha2',bxp.errorbar = T,bxp.errorbar.width = 0.2)+
  labs(x='Degradations',y='Shannon index')+
  theme(plot.title = element_text(colour = 'black',size = 14,hjust = 0.5),
        axis.text.x = element_text(colour ='black',size = 9,angle = 0),
        axis.text.y = element_text(colour ='black',size = 9,angle = 0),
        axis.title.x = element_text(colour ='black',size = 12,angle = 0),
        axis.title.y = element_text(colour ='black',size = 12,angle = 90),
        legend.title = element_text(colour ='black',size =16),
        legend.text = element_text(colour ='black',size = 16),
        axis.line.y = element_line(colour = 'black',linetype = 'solid'),
        axis.line.x = element_line(colour = 'black',linetype = 'solid'),
        panel.border = element_rect(linetype = 'solid',size = 0.5,fill=NA))
p
  }

one = read.table('clipboard',header=TRUE)
#pielou~degradation——————————————5
  {
#bird
one$habitat = factor(one$habitat,levels = c("OG","LG","MG","SG"))
p  =  ggboxplot(one,x='habitat', y='bpie',bxp.errorbar = T,bxp.errorbar.width = 0.2)+
  labs(x='Degradations',y='Pielou index')+
  theme(plot.title = element_text(colour = 'black',size = 14,hjust = 0.5),
        axis.text.x = element_text(colour ='black',size = 9,angle = 0),
        axis.text.y = element_text(colour ='black',size = 9,angle = 0),
        axis.title.x = element_text(colour ='black',size = 12,angle = 0),
        axis.title.y = element_text(colour ='black',size = 12,angle = 90),
        legend.title = element_text(colour ='black',size =16),
        legend.text = element_text(colour ='black',size = 16),
        axis.line.y = element_line(colour = 'black',linetype = 'solid'),
        axis.line.x = element_line(colour = 'black',linetype = 'solid'),
        panel.border = element_rect(linetype = 'solid',size = 0.5,fill=NA))
p

#arthropod
one$habitat = factor(one$habitat,levels = c("OG","LG","MG","SG"))
p  =  ggboxplot(one,x='habitat', y='apie',bxp.errorbar = T,bxp.errorbar.width = 0.2)+
  labs(x='Degradations',y='Pielou index')+
  theme(plot.title = element_text(colour = 'black',size = 14,hjust = 0.5),
        axis.text.x = element_text(colour ='black',size = 9,angle = 0),
        axis.text.y = element_text(colour ='black',size = 9,angle = 0),
        axis.title.x = element_text(colour ='black',size = 12,angle = 0),
        axis.title.y = element_text(colour ='black',size = 12,angle = 90),
        legend.title = element_text(colour ='black',size =16),
        legend.text = element_text(colour ='black',size = 16),
        axis.line.y = element_line(colour = 'black',linetype = 'solid'),
        axis.line.x = element_line(colour = 'black',linetype = 'solid'),
        panel.border = element_rect(linetype = 'solid',size = 0.5,fill=NA))
p

#plant
one$habitat = factor(one$habitat,levels = c("OG","LG","MG","SG"))
p  =  ggboxplot(one,x='habitat', y='ppie',bxp.errorbar = T,bxp.errorbar.width = 0.2)+
  labs(x='Degradations',y='Pielou index')+
  theme(plot.title = element_text(colour = 'black',size = 14,hjust = 0.5),
        axis.text.x = element_text(colour ='black',size = 9,angle = 0),
        axis.text.y = element_text(colour ='black',size = 9,angle = 0),
        axis.title.x = element_text(colour ='black',size = 12,angle = 0),
        axis.title.y = element_text(colour ='black',size = 12,angle = 90),
        legend.title = element_text(colour ='black',size =16),
        legend.text = element_text(colour ='black',size = 16),
        axis.line.y = element_line(colour = 'black',linetype = 'solid'),
        axis.line.x = element_line(colour = 'black',linetype = 'solid'),
        panel.border = element_rect(linetype = 'solid',size = 0.5,fill=NA))
p
  }

one = read.table('clipboard',header=TRUE)
#biomass~degradation——————————————6
  {
#bird
one$habitat = factor(one$habitat,levels = c("OG","LG","MG","SG"))
p  =  ggboxplot(one,x='habitat', y='bma',bxp.errorbar = T,bxp.errorbar.width = 0.2)+
  labs(x='Degradations',y='Biomass')+
  theme(plot.title = element_text(colour = 'black',size = 14,hjust = 0.5),
        axis.text.x = element_text(colour ='black',size = 9,angle = 0),
        axis.text.y = element_text(colour ='black',size = 9,angle = 0),
        axis.title.x = element_text(colour ='black',size = 12,angle = 0),
        axis.title.y = element_text(colour ='black',size = 12,angle = 90),
        legend.title = element_text(colour ='black',size =16),
        legend.text = element_text(colour ='black',size = 16),
        axis.line.y = element_line(colour = 'black',linetype = 'solid'),
        axis.line.x = element_line(colour = 'black',linetype = 'solid'),
        panel.border = element_rect(linetype = 'solid',size = 0.5,fill=NA))
p

#arthropod
one$habitat = factor(one$habitat,levels = c("OG","LG","MG","SG"))
p  =  ggboxplot(one,x='habitat', y='ama',bxp.errorbar = T,bxp.errorbar.width = 0.2)+
  labs(x='Degradations',y='Biomass')+
  theme(plot.title = element_text(colour = 'black',size = 14,hjust = 0.5),
        axis.text.x = element_text(colour ='black',size = 9,angle = 0),
        axis.text.y = element_text(colour ='black',size = 9,angle = 0),
        axis.title.x = element_text(colour ='black',size = 12,angle = 0),
        axis.title.y = element_text(colour ='black',size = 12,angle = 90),
        legend.title = element_text(colour ='black',size =16),
        legend.text = element_text(colour ='black',size = 16),
        axis.line.y = element_line(colour = 'black',linetype = 'solid'),
        axis.line.x = element_line(colour = 'black',linetype = 'solid'),
        panel.border = element_rect(linetype = 'solid',size = 0.5,fill=NA))
p

#plant
one$habitat = factor(one$habitat,levels = c("OG","LG","MG","SG"))
p  =  ggboxplot(one,x='habitat', y='pma',bxp.errorbar = T,bxp.errorbar.width = 0.2)+
  labs(x='Degradations',y='Biomass')+
  theme(plot.title = element_text(colour = 'black',size = 14,hjust = 0.5),
        axis.text.x = element_text(colour ='black',size = 9,angle = 0),
        axis.text.y = element_text(colour ='black',size = 9,angle = 0),
        axis.title.x = element_text(colour ='black',size = 12,angle = 0),
        axis.title.y = element_text(colour ='black',size = 12,angle = 90),
        legend.title = element_text(colour ='black',size =16),
        legend.text = element_text(colour ='black',size = 16),
        axis.line.y = element_line(colour = 'black',linetype = 'solid'),
        axis.line.x = element_line(colour = 'black',linetype = 'solid'),
        panel.border = element_rect(linetype = 'solid',size = 0.5,fill=NA))
p
  }
}

#arthropod feeding habits (shannon & abundence bar)———————
{
#data1 = read_xlsx("variable.xlsx",58)
data1 = read.table('clipboard',header=TRUE)
data1$habitat = factor(data1$habitat,levels = c("wth","qd","zd","zd2"))

#Arthropod-Shannon
f = ggplot(data1,aes(x=habitat, y=寄生)) +
  geom_bar(stat = "identity", position=position_dodge(), width=0.6,
           color="black", fill=c("green","blue","yellow","grey"))+
  geom_errorbar(aes(ymin=寄生-寄生3, ymax=寄生+寄生3), width=0.2, 
                color="black", position=position_dodge(0.6))
f+theme_bw()

f = ggplot(data1,aes(x=habitat, y=食腐)) +
  geom_bar(stat = "identity", position=position_dodge(), width=0.6,
           color="black", fill=c("green","blue","yellow","grey"))+
  geom_errorbar(aes(ymin=食腐-食腐3, ymax=食腐+食腐3), width=0.2, 
                color="black", position=position_dodge(0.6))
f+theme_bw()

f = ggplot(data1,aes(x=habitat, y=食肉)) +
  geom_bar(stat = "identity", position=position_dodge(), width=0.6,
           color="black", fill=c("green","blue","yellow","grey"))+
  geom_errorbar(aes(ymin=食肉-食肉3, ymax=食肉+食肉3), width=0.2, 
                color="black", position=position_dodge(0.6))
f+theme_bw()

f = ggplot(data1,aes(x=habitat, y=食植)) +
  geom_bar(stat = "identity", position=position_dodge(), width=0.6,
           color="black", fill=c("green","blue","yellow","grey"))+
  geom_errorbar(aes(ymin=食植-食植3, ymax=食植+食植3), width=0.2, 
                color="black", position=position_dodge(0.6))
f+theme_bw()

f = ggplot(data1,aes(x=habitat, y=杂食)) +
  geom_bar(stat = "identity", position=position_dodge(), width=0.6,
           color="black", fill=c("green","blue","yellow","grey"))+
  geom_errorbar(aes(ymin=杂食-杂食3, ymax=杂食+杂食3), width=0.2, 
                color="black", position=position_dodge(0.6))
f+theme_bw()


#节肢多度
f = ggplot(data1,aes(x=habitat, y=寄生2)) +
  geom_bar(stat = "identity", position=position_dodge(), width=0.6,
           color="black", fill=c("green","blue","yellow","grey"))+
  geom_errorbar(aes(ymin=寄生2-寄生4, ymax=寄生2+寄生4), width=0.2, 
                color="black", position=position_dodge(0.6))
f+theme_bw()

f = ggplot(data1,aes(x=habitat, y=食腐2)) +
  geom_bar(stat = "identity", position=position_dodge(), width=0.6,
           color="black", fill=c("green","blue","yellow","grey"))+
  geom_errorbar(aes(ymin=食腐2-食腐4, ymax=食腐2+食腐4), width=0.2, 
                color="black", position=position_dodge(0.6))
f+theme_bw()

f = ggplot(data1,aes(x=habitat, y=食肉2)) +
  geom_bar(stat = "identity", position=position_dodge(), width=0.6,
           color="black", fill=c("green","blue","yellow","grey"))+
  geom_errorbar(aes(ymin=食肉2-食肉4, ymax=食肉2+食肉4), width=0.2, 
                color="black", position=position_dodge(0.6))
f+theme_bw()

f = ggplot(data1,aes(x=habitat, y=食植2)) +
  geom_bar(stat = "identity", position=position_dodge(), width=0.6,
           color="black", fill=c("green","blue","yellow","grey"))+
  geom_errorbar(aes(ymin=食植2-食植4, ymax=食植2+食植4), width=0.2, 
                color="black", position=position_dodge(0.6))
f+theme_bw()

f = ggplot(data1,aes(x=habitat, y=杂食2)) +
  geom_bar(stat = "identity", position=position_dodge(), width=0.6,
           color="black", fill=c("green","blue","yellow","grey"))+
  geom_errorbar(aes(ymin=杂食2-杂食4, ymax=杂食2+杂食4), width=0.2, 
                color="black", position=position_dodge(0.6))
f+theme_bw()
}

#feces & grass height—————————————————————————————————————
{
#height
#data1 = read_xlsx("variable.xlsx",58)
data1 = read.table('clipboard',header=TRUE)
data1$habitat = factor(data1$habitat,levels = c("wth","qd","zd","zd2"))

f = ggplot(data1,aes(x=habitat, y=height)) +
  geom_bar(stat = "identity", position=position_dodge(), width=0.6,
           color="black", fill=c("green","blue","yellow","grey"))+
  geom_errorbar(aes(ymin=height-height2, ymax=height+height2), width=0.2, 
                color="black", position=position_dodge(0.6))
f+theme_bw()


#feces
#data1 = read_xlsx("variable.xlsx",58)
data1 = read.table('clipboard',header=TRUE)
data1$habitat = factor(data1$habitat,levels = c("qd","zd","zd2"))

f = ggplot(data1,aes(x=habitat, y=feces)) +
  geom_bar(stat = "identity", position=position_dodge(), width=0.6,
           color="black", fill=c("blue","yellow","grey"))+
  geom_errorbar(aes(ymin=feces-feces2, ymax=feces+feces2), width=0.2, 
                color="black", position=position_dodge(0.6))
f+theme_bw()

f = ggplot(data1,aes(x=habitat, y=cst)) +
  geom_bar(stat = "identity", position=position_dodge(), width=0.6,
           color="black", fill=c("blue","yellow","grey"))+
  geom_errorbar(aes(ymin=cst-cst2, ymax=cst+cst2), width=0.2, 
                color="black", position=position_dodge(0.6))
f+theme_bw()

f = ggplot(data1,aes(x=habitat, y=sst)) +
  geom_bar(stat = "identity", position=position_dodge(), width=0.6,
           color="black", fill=c("blue","yellow","grey"))+
  geom_errorbar(aes(ymin=sst-sst2, ymax=sst+sst2), width=0.2, 
                color="black", position=position_dodge(0.6))
f+theme_bw()

f = ggplot(data1,aes(x=habitat, y=hst)) +
  geom_bar(stat = "identity", position=position_dodge(), width=0.6,
           color="black", fill=c("blue","yellow","grey"))+
  geom_errorbar(aes(ymin=hst-hst2, ymax=hst+hst2), width=0.2, 
                color="black", position=position_dodge(0.6))
f+theme_bw()
}

#variable co-linearity—————————————————————————————————————————————
{
#degradation
#data1 = read_xlsx("variable.xlsx",54)
data1 = read.table('clipboard',header=TRUE)
corr  =  cor(data1, method="spearman")#"pearson"/"kendall"/"spearman"—————————————
corrplot(corr = corr,order="AOE",type="upper",tl.pos="tp")
corrplot(corr = corr,add=TRUE, type="lower", 
         method="number",order="AOE",col="black",diag=FALSE,tl.pos="n", cl.pos="n")
#feces
#data1 = read_xlsx("variable.xlsx",54)
data1 = read.table('clipboard',header=TRUE)
corr  =  cor(data1, method="spearman")#"pearson"/"kendall"/"spearman"—————————————
corrplot(corr = corr,order="AOE",type="upper",tl.pos="tp")
corrplot(corr = corr,add=TRUE, type="lower", 
         method="number",order="AOE",col="black",diag=FALSE,tl.pos="n", cl.pos="n")


#data1  = read_xlsx("variable.xlsx",58)
data1 = read.table('clipboard',header=TRUE)
corr  =  cor(data1, method="spearman")#"pearson"/"kendall"/"spearman"—————————————
corrplot(corr = corr,order="AOE",type="upper",tl.pos="tp")
corrplot(corr = corr,add=TRUE, type="lower", 
         method="number",order="AOE",col="black",diag=FALSE,tl.pos="n", cl.pos="n")
}

#cascade(piecewise sem)：———————————————————————————————————————
{
b = read_xlsx("variable.xlsx",58)
#b = read.table('clipboard',header=TRUE)
datasem  =  as.data.frame(b)
shapiro.test(datasem$birdsha)
shapiro.test(datasem$arsha)
shapiro.test(datasem$plsha)
#attempt1—————————————— < 1 >
model1  =  psem(
  lme(birdsha ~ arsha+plsha+cst+sst+hst+covcom+pasleg+covwee+height,random = ~1|habitat,data = datasem),
  lme(arsha ~ plsha+cst+sst+hst+covcom+pasleg+covwee+height,random = ~1|habitat,data = datasem),
  lme(plsha ~ cst+sst+hst+covcom+pasleg+covwee+height,random = ~1|habitat,data = datasem),
  lme(height ~ cst+sst+hst,random = ~1|habitat,data = datasem),
  data = datasem
)
summary(model1, conserve = T)

model1  =  psem(
  lme(birdsha ~ arsha+cst+height,random = ~1|habitat,data = datasem),
  lme(arsha ~ plsha+sst+height,random = ~1|habitat,data = datasem),
  lme(plsha ~ sst,random = ~1|habitat,data = datasem),
  lme(height ~ cst+sst,random = ~1|habitat,data = datasem),
  data = datasem
)
summary(model1, conserve = T)

#attempt2—————————————— < 2 >
model2  =  psem(
  lme(birdsha ~ 寄生2+plsha+cst+sst+hst+covcom+pasleg+covwee+height,random = ~1|habitat,data = datasem),
  lme(寄生2 ~ plsha+cst+sst+hst+covcom+pasleg+covwee+height,random = ~1|habitat,data = datasem),
  lme(plsha ~ cst+sst+hst+covcom+pasleg+covwee+height,random = ~1|habitat,data = datasem),
  lme(height ~ cst+sst+hst,random = ~1|habitat,data = datasem),
  data = datasem
)
summary(model2, conserve = T)

model2  =  psem(
  lme(birdsha ~ 寄生2+cst+height,random = ~1|habitat,data = datasem),
  lme(寄生2 ~ plsha+sst+height,random = ~1|habitat,data = datasem),
  lme(plsha ~ sst,random = ~1|habitat,data = datasem),
  lme(height ~ cst+sst,random = ~1|habitat,data = datasem),
  data = datasem
)
summary(model2, conserve = T)

#attempt3—————————————— < 3 >
model3  =  psem(
  lme(birdsha ~ 食腐2+plsha+cst+sst+hst+covcom+pasleg+covwee+height,random = ~1|habitat,data = datasem),
  lme(食腐2 ~ plsha+cst+sst+hst+covcom+pasleg+covwee+height,random = ~1|habitat,data = datasem),
  lme(plsha ~ cst+sst+hst+covcom+pasleg+covwee+height,random = ~1|habitat,data = datasem),
  lme(height ~ cst+sst+hst,random = ~1|habitat,data = datasem),
  data = datasem
)
summary(model3, conserve = T)

model3  =  psem(
  lme(birdsha ~ 食腐2+sst+height,random = ~1|habitat,data = datasem),
  lme(食腐2 ~ plsha+cst+height,random = ~1|habitat,data = datasem),
  lme(plsha ~ sst,random = ~1|habitat,data = datasem),
  lme(height ~ cst+sst,random = ~1|habitat,data = datasem),
  data = datasem
)
summary(model3, conserve = T)

#attempt4—————————————— < 4 >
model4  =  psem(
  lme(birdsha ~ 食肉2+plsha+cst+sst+hst+covcom+pasleg+covwee+height,random = ~1|habitat,data = datasem),
  lme(食肉2 ~ plsha+cst+sst+hst+covcom+pasleg+covwee+height,random = ~1|habitat,data = datasem),
  lme(plsha ~ cst+sst+hst+covcom+pasleg+covwee+height,random = ~1|habitat,data = datasem),
  lme(height ~ cst+sst+hst,random = ~1|habitat,data = datasem),
  data = datasem
)
summary(model4, conserve = T)

model4  =  psem(
  lme(birdsha ~ 食肉2+sst+height,random = ~1|habitat,data = datasem),
  lme(食肉2 ~ plsha+sst+height,random = ~1|habitat,data = datasem),
  lme(plsha ~ sst,random = ~1|habitat,data = datasem),
  lme(height ~ cst+sst,random = ~1|habitat,data = datasem),
  data = datasem
)
summary(model4, conserve = T)

#attempt5—————————————— < 5 >
model5  =  psem(
  lme(birdsha ~ 食植2+plsha+cst+sst+hst+covcom+pasleg+covwee+height,random = ~1|habitat,data = datasem),
  lme(食植2 ~ plsha+cst+sst+hst+covcom+pasleg+covwee+height,random = ~1|habitat,data = datasem),
  lme(plsha ~ cst+sst+hst+covcom+pasleg+covwee+height,random = ~1|habitat,data = datasem),
  lme(height ~ cst+sst+hst,random = ~1|habitat,data = datasem),
  data = datasem
)
summary(model5, conserve = T)

model5  =  psem(
  lme(birdsha ~ 食植2+sst+height,random = ~1|habitat,data = datasem),
  lme(食植2 ~ plsha+cst+height,random = ~1|habitat,data = datasem),
  lme(plsha ~ sst,random = ~1|habitat,data = datasem),
  lme(height ~ cst+sst,random = ~1|habitat,data = datasem),
  data = datasem
)
summary(model5, conserve = T)

#attempt6—————————————— < 6 >
model6  =  psem(
  lme(birdsha ~ 杂食2+plsha+cst+sst+hst+covcom+pasleg+covwee+height,random = ~1|habitat,data = datasem),
  lme(杂食2 ~ plsha+cst+sst+hst+covcom+pasleg+covwee+height,random = ~1|habitat,data = datasem),
  lme(plsha ~ cst+sst+hst+covcom+pasleg+covwee+height,random = ~1|habitat,data = datasem),
  lme(height ~ cst+sst+hst,random = ~1|habitat,data = datasem),
  data = datasem
)
summary(model6, conserve = T)

model6  =  psem(
  lme(birdsha ~ 杂食2+sst+height,random = ~1|habitat,data = datasem),
  lme(杂食2 ~ plsha+sst+height,random = ~1|habitat,data = datasem),
  lme(plsha ~ sst,random = ~1|habitat,data = datasem),
  lme(height ~ cst+sst,random = ~1|habitat,data = datasem),
  data = datasem
)
summary(model6, conserve = T)



#attempt1—————————————— < 1 >
#b = read_xlsx("variable.xlsx",27)
b = read.table('clipboard',header=TRUE)
datasem  =  as.data.frame(b)
shapiro.test(datasem$birdsim)
shapiro.test(datasem$arsim)
shapiro.test(datasem$plsim)
model1  =  psem(
  #glmer(birdsim ~ arsim+plsim+cst+sst+hst+(1|habitat),"poisson",data = datasem),
  lme(birdsim ~ arsim+cst+sst+hst,random = ~1|habitat,data = datasem),
  lme(arsim ~ plsim+sst+hst,random = ~1|habitat,data = datasem),
  lme(plsim ~ cst+sst,random = ~1|habitat,data = datasem),
  #glmer(plsim ~ cst+sst+hst+(1|habitat),"poisson",data = datasem),
  data = datasem
)
summary(model1, conserve = T)

#attempt2—————————————— < 2 >
#a = read_xlsx("variable.xlsx",20)
a = read.table('clipboard',header=TRUE)
datasem  =  as.data.frame(a)
shapiro.test(datasem$birdma)#
shapiro.test(datasem$arthma)#
model2  =  psem(
  #glmer(birdma ~ arthma+vegma+cst+sst+hst+(1|habitat),"poisson",data = datasem),
  #glmer(arthma ~ cst+sst+hst+(1|habitat),"poisson",data = datasem),
  lme(birdma ~ hst+arthma,random = ~1|habitat,data = datasem),
  lme(arthma ~ vegma,random = ~1|habitat,data = datasem),
  lme(vegma ~ cst+sst,random = ~1|habitat,data = datasem),
  data = datasem
)
summary(model2, conserve = T)

#attempt3—————————————— < 3 >
#b = read_xlsx("variable.xlsx",27)
b = read.table('clipboard',header=TRUE)
datasem  =  as.data.frame(b)
shapiro.test(datasem$birdsim)#
shapiro.test(datasem$plsim)#
model3  =  psem(
  #glmer(birdsim ~ arsim+plsim+covcom+pasleg+(1|habitat),"poisson",data = datasem),
  lme(birdsim ~ arsim+plsim+covcom,random = ~1|habitat,data = datasem),
  lme(arsim ~ plsim+covcom+pasleg,random = ~1|habitat,data = datasem),
  lme(plsim ~ covcom+pasleg,random = ~1|habitat,data = datasem),
  #glmer(plsim ~ covcom+pasleg+(1|habitat),"poisson",data = datasem),
  data = datasem
)
summary(model3, conserve = T)

#attempt4—————————————— < 4 >
#a = read_xlsx("variable.xlsx",20)
a = read.table('clipboard',header=TRUE)
datasem  =  as.data.frame(a)
shapiro.test(datasem$birdma)#
shapiro.test(datasem$arthma)#
model4  =  psem(
  #glmer(birdma ~ arthma+vegma+covcom+pasleg+(1|habitat),"poisson",data = datasem),
  #glmer(arthma ~ covcom+pasleg+(1|habitat),"poisson",data = datasem),
  lme(birdma ~ arthma,random = ~1|habitat,data = datasem),
  lme(arthma ~ vegma+covcom+pasleg,random = ~1|habitat,data = datasem),
  lme(vegma ~ pasleg,random = ~1|habitat,data = datasem),
  data = datasem
)
summary(model4, conserve = T)

res1=residuals(model1)
qqnorm(res1)
}

#bird index/biomass~arthropod index/biomass~plant index/biomass——————————————————————————
#data = read_xlsx("variable.xlsx",55-3)
data = read.table('clipboard',header=TRUE)
shapiro.test(data$birdma)
shapiro.test(data$arthma)
shapiro.test(data$birdsim)
shapiro.test(data$arthsim)

#Simpson——————————
lm.reg  =  lm(birdsim~arthsim,data=data)
summary(lm.reg)
summary(lm.reg)[c("r.squared", "adj.r.squared")]#0.008,AIC=-31.264
p  =  ggplot(data,aes(x=arthsim,y=birdsim)) + geom_point(shape=19,size=3)+
  xlab("birdSimpson")+ylab("arthropodSimpson")
p+geom_smooth(method = lm)+theme_set(theme_bw())+
  theme(axis.text.x = element_text(colour ='black',size = 14,angle = 0),
        axis.text.y = element_text(colour ='black',size = 14,angle = 0),
        axis.title.x = element_text(colour ='black',size = 18,angle = 0),
        axis.title.y = element_text(colour ='black',size = 18,angle = 90))

lm.reg  =  lm(arthsim~vegsim,data=data)
summary(lm.reg)
summary(lm.reg)[c("r.squared", "adj.r.squared")]#0.008,AIC=-31.264
p  =  ggplot(data,aes(x=vegsim,y=arthsim)) + geom_point(shape=19,size=3)+
  xlab("arthropodSimpson")+ylab("plantSimpson")
p+geom_smooth(method = lm)+theme_set(theme_bw())+
  theme(axis.text.x = element_text(colour ='black',size = 14,angle = 0),
        axis.text.y = element_text(colour ='black',size = 14,angle = 0),
        axis.title.x = element_text(colour ='black',size = 18,angle = 0),
        axis.title.y = element_text(colour ='black',size = 18,angle = 90))

#Biomass——————————
lm.reg  =  lm(birdma~arthma,data=data)
summary(lm.reg)
summary(lm.reg)[c("r.squared", "adj.r.squared")]#0.008,AIC=-31.264
p  =  ggplot(data,aes(x=arthma,y=birdma)) + geom_point(shape=19,size=3)+
  xlab("bird biomass")+ylab("arthropod biomass")
p+geom_smooth(method = lm)+theme_set(theme_bw())+
  theme(axis.text.x = element_text(colour ='black',size = 14,angle = 0),
        axis.text.y = element_text(colour ='black',size = 14,angle = 0),
        axis.title.x = element_text(colour ='black',size = 18,angle = 0),
        axis.title.y = element_text(colour ='black',size = 18,angle = 90))

lm.reg  =  lm(arthma~vegma,data=data)
summary(lm.reg)
summary(lm.reg)[c("r.squared", "adj.r.squared")]#0.008,AIC=-31.264
p  =  ggplot(data,aes(x=vegma,y=arthma)) + geom_point(shape=19,size=3)+
  xlab("arthropod biomass")+ylab("plant biomass",angle = 0)+
  theme(axis.text.y = element_text(colour ='black',size = 14,angle = 0),
        axis.title.x = element_text(colour ='black',size = 18,angle = 0),
        axis.title.y = element_text(colour ='black',size = 18,angle = 90))


