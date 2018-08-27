library(e1071);require(mclust);library(RColorBrewer);require(ggthemes);
require(ggpubr);require(dplyr)

Lyrae <- read.table("blg_met_rrl.dat",header=T) %>% mutate(.,Period = log10(Period)) %>%
  filter(X.Fe.H. > -1.647874 && X.Fe.H. < -0.279256)

test_index <- sample(seq_len(nrow(Lyrae)),replace=F, size = 5000)
Lyrae_short <- Lyrae[test_index,c("Period","X.Fe.H.","R21")] 

CLUST <- Mclust(Lyrae_short,G = 1:4)
plot(CLUST)

gdata <- data.frame(x=AGN_short[,1],y=AGN_short[,2],type=as.factor(CLUST$classification))

#-----------------------
# BPT PLOT
#-----------------------
xx = seq(-4, 0.0, 0.01)
Ka = 0.61 / (xx - 0.05) + 1.30
gKa <- data.frame(xx,Ka)
#-----------------------
xx1 = seq(-4, 0.4, 0.01)
Ke = 0.61 / (xx1 - 0.47) + 1.19
gKe <- data.frame(xx1,Ke)

#-----------------------
xx2 = seq(-0.43, 5, 0.01)
Sey = 1.05 * xx2 + 0.45
gSey <- data.frame(xx2,Sey)



ggplot(data=gdata,aes(x=x,y=y))+geom_point(aes(color=type))+
  xlab(expression(paste('log ([NII]/H', alpha, ')'))) +
  ylab(expression(paste('log ([OIII]/H', beta, ')'))) +
  scale_colour_manual(values = c("#66c2a5","#fc8d62","#8da0cb","#e78ac3"))+
  theme_pubr() + 
  geom_line(aes(x=xx,y=Ka),data=gKa,size=1.25,linetype="dashed",color="gray25")+
  geom_line(aes(x=xx1,y=Ke),data=gKe,size=1.25,linetype="dotted",color="gray25")+
  geom_line(aes(x=xx2,y=Sey),data=gSey,size=0.75,linetype="dotdash",color="gray25")+
  coord_cartesian(xlim=c(-1.8,1.3),ylim=c(-1.5,1.55))+
  theme(legend.position = "none",plot.title = element_text(hjust=0.5),
        axis.title.y=element_text(vjust=0.75),
        axis.title.x=element_text(vjust=-0.25),
        text = element_text(size=20))
  
