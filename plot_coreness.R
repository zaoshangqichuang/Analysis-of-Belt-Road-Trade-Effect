library(igraph)#复杂网络包
library(RColorBrewer)
library(reshape2)
library(plyr)#igraph辅助包
library(ggplot2)#绘图包
library(tcltk)#进度条包
library(gridExtra)#改变字体的包
library(grid)
windowsFonts(Times=windowsFont("Times New Roman"), D=windowsFont("华文楷体"))
#计算lambda值
cc1<-c()
cc2<-c()
rank1<-c()
coreness1<-c()
rank2<-c()
coreness2<-c()
tab1<-read.csv("D:\\我爱学习学习爱我\\读论文啊\\R绘图\\bs_k.csv",header=T)
tab2<-read.csv("D:\\我爱学习学习爱我\\读论文啊\\R绘图\\global_k.csv",header=T)
tab1<-melt(tab1,id.vars="国家",variable.name="Year",value.name="coreness")
tab2<-melt(tab2,id.vars="国家",variable.name="Year",value.name="coreness")
for (i in c(1:10))
{
  a1<-tab1[tab1$Year==paste("x",2006+i,sep=""),"coreness"]
  a1=(a1-min(a1))/(max(a1)-min(a1))
  #lm1<-lm(a1~log(rank(-a1)))
  #cc1<-c(cc1,lm1$coefficients[[2]])
  a2<-tab2[tab2$Year==paste("x",2006+i,sep=""),"coreness"]
  a2=(a2-min(a2))/(max(a2)-min(a2))
  #lm2<-lm(a2~log(rank(-a2)))
  #cc2<-c(cc2,lm2$coefficients[[2]])
  coreness1<-c(coreness1,a1)
  rank1<-c(rank1,rank(-a1))
  coreness2<-c(coreness2,a2)
  rank2<-c(rank2,rank(-a2))
}
tab1<-data.frame(tab1,coreness1,rank1)#计算每年的国家排序
cols<-brewer.pal(10,"Spectral")
g1<-ggplot(tab1)+theme_bw()+theme(panel.grid =element_blank())
g1<-g1+geom_smooth(aes(y=coreness1,x=log(rank1),color=cols[factor(Year)]),method ='lm',formula = y~x,se=TRUE,linetype="dashed")
g1<-g1+geom_point(aes(y=coreness1,x=log(rank1),color=cols[factor(Year)]),size=1)
g1<-g1+xlab("排序(e^)")+ylab("重要性")+guides(color=F)
g1<-g1+theme(text=element_text(family="D", size=12))
x1<-c()
y1<-c()
label1<-c()
for (j in c(1,2,4,15,20,35,60))
{
  x=log(j)
  y=tab1[tab1$Year=="x2016"&tab1$rank1==j,]$coreness1
  label=paste(tab1[j,"国家"],j,sep=",")
  x1<-c(x1,x)
  y1<-c(y1,y)
  label1<-c(label1,label)
}
g1<-g1+geom_text(data=data.frame(x1,y1,label1),aes(x=x1,y=y1,label=label1),size=4,family="D")






tab2<-data.frame(tab2,coreness2,rank2)#计算每年的国家排序
cols<-brewer.pal(10,"Spectral")
g2<-ggplot(tab2)+theme_bw()+theme(panel.grid =element_blank())
g2<-g2+geom_smooth(aes(y=coreness2,x=log(rank2),color=cols[factor(Year)]),method ='lm',formula = y~x,se=TRUE,linetype="dashed")
g2<-g2+geom_point(aes(y=coreness2,x=log(rank2),color=cols[factor(Year)]),size=1)
g2<-g2+xlab("排序(e^)")+ylab("重要性")+guides(color=F)
g2<-g2+theme(text=element_text(family="D", size=12))
x2<-c()
y2<-c()
label2<-c()
for (j in c(1,2,4,8,14,54,79,106,140,182))
{
  x=log(j)
  y=tab2[tab2$Year=="x2016"&tab2$rank2==j,]$coreness2
  label=paste(country[j,],j,sep=",")
  x2<-c(x2,x)
  y2<-c(y2,y)
  label2<-c(label2,label)
}
g2<-g2+geom_text(data=data.frame(x2,y2,label2),aes(x=x2,y=y2,label=label2),size=4,family="D")




cc1<-read.csv("clipboard",header=F)$V1
cc2<-read.csv("clipboard",header=F)$V1
cc<-data.frame(x=c(2007:2016),cc1=cc1,cc2=cc2)
f1<-ggplot(cc)+theme_bw()+theme(text=element_text(family="Times", size=12),panel.grid =element_blank())+geom_line(aes(y=cc1,x=x),size=1,linetype="dashed")+geom_point(aes(y=cc1,x=x),size = I(5), alpha = I(0.2))
f2<-ggplot(cc)+theme_bw()+theme(text=element_text(family="Times", size=12),panel.grid =element_blank())+geom_line(aes(x=x,y=cc2),size=1,linetype="dashed")+geom_point(aes(y=cc2,x=x),size = I(5), alpha = I(0.2))
f1<-f1+xlab("")+ylab("λ")
f2<-f2+xlab("")+ylab("λ")

a1<-g1+annotation_custom(
    grob = ggplotGrob(f1),
    xmin = 0, xmax = 2.5, ymin = 0, ymax = 0.5
  )
a1

a2<-g2+annotation_custom(
  grob = ggplotGrob(f2),
  xmin = 0, xmax = 3.2, ymin = 0, ymax = 0.5
)
a2
p1<-a2+labs(title="a.全球贸易流网络")+theme(plot.title = element_text(family="D",hjust = 0,vjust=0.01,size=12))
p2<-a1+labs(title="b.一带一路贸易流网络")+theme(plot.title = element_text(family="D",hjust = 0,vjust=0.01,size=12))
grid.arrange(p1,p2,nrow=2)


x1<-read.csv("clipboard",header=F)$V1
x2<-read.csv("clipboard",header=F)$V1
xx1=c(x1,x2)
yy1=rep(c(2007:2016),2)
cc2=c(rep("N=199",10),rep("N=63",10))
cc<-data.frame(x=yy1,cc1=xx1,cc2=cc2)
p3<-ggplot(cc)+theme_bw()+theme(text=element_text(family="Times", size=12),panel.grid =element_blank())+geom_line(aes(y=cc1,x=x,color=factor(cc2),linetype=factor(cc2)),size=1)+geom_point(aes(y=cc1,x=x,color=factor(cc2)),size = I(5), alpha = I(0.2))
p3<-p3+theme(text=element_text(family="Times",  size=12),legend.title=element_blank(),
         legend.box = "vertical",
         legend.box.spacing=unit(0,'cm'),
         legend.margin=margin(0.1,0.1,0.1,0.1,'cm'),
         legend.position = "bottom",
         legend.text= element_text(family="Times",size=9),
)+ylab("λ")+xlab("")
p3<-p3+labs(title="c.随机流网络")+theme(plot.title = element_text(family="D",hjust = 0,vjust=0.01,size=12))
plots <- list(p1, p2, p3)
lay1=matrix(c(1,1,2,2,1,1,2,2,3,3,3,3),nrow=3,byrow=TRUE)
grid.arrange(grobs = plots,
             layout_matrix = lay1)
             #widths = c(1, 1),  #第一列的宽度为1，第二列的宽度为2
             #eigths = c(1, 0.2))  #第二行的高度设置小,为什么我没有发现变化




q=read.csv("D:\\我爱学习学习爱我\\读论文啊\\R绘图\\q.csv",header=T)
q=data.frame(q)
g3<-ggplot(q)+theme_bw()+theme(panel.grid =element_blank())
g3<-g3+geom_point(aes(x=year,y=Q,color=factor(label),shape=factor(label)),size=3)+geom_line(aes(x=year,y=Q,color=factor(label),linetype=factor(c)),size=1)
g3<-g3+theme(text=element_text(family="Times",  size=12),legend.title=element_blank(),
  legend.key.size=unit(0.5,'cm'),
  legend.box = "vertical",
  legend.box.spacing=unit(0.5,'cm'),
  legend.margin=margin(0.1,0.1,0.1,0.1,'cm'),
  legend.text= element_text(family="D",size=9),
)
g3+geom_vline(xintercept =c(2009,2013),linetype="dotted")
