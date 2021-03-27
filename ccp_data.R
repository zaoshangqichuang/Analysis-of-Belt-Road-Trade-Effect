#绘制洛伦兹曲线并计算基尼系数
library(R.matlab)
library(ineq)
mat<-readMat("newv.mat")
h.W<-mat$h.W
h.W.ER<-mat$h.W.ER 
h.BS<-mat$h.BS
h.BS.ER<-mat$h.BS.ER 
ccp<-mat$ccp
ccp.bs<-mat$ccp.bs
id_global<-read.csv("id_global.csv",header=T,sep=',')
id_bs<-read.csv("id_bs.csv",header=T,sep=',')
id_global<-id_global[order(-(as.numeric(rownames(id_global)))),]
id_bs<-id_bs[order(-(as.numeric(rownames(id_bs)))),]
W<-mat$total.W
BS.W<-mat$BS.W
ex<-function(i,net){
  export0=sum(net[i,])
  export0
}
im<-function(i,net){
  import0=sum(net[,i])
  import0
}

#全球贸易网络每个国家的进出口额
ex_global_list<-list()
im_global_list<-list()
for (net in W){
  net=net[[1]]
  end_=length(net[1,])
  net=net[c(3:end_-1),c(3:end_-1)]
  ex_global_vec<-c()
  im_global_vec<-c()
  for (id in id_global$code){
    ex_global_vec<-c(ex_global_vec,ex(id,net))
    im_global_vec<-c(im_global_vec,im(id,net))
  }
  ex_global_list<-c(ex_global_list,list(ex_global_vec))
  im_global_list<-c(im_global_list,list(im_global_vec))
}

#“一带一路”贸易网络每个国家的进出口额
ex_bs_list<-list()
im_bs_list<-list()
for (net in BS.W){
  net=net[[1]]
  end_=length(net[1,])
  net=net[c(2:(end_-1)),c(2:(end_-1))]
  ex_bs_vec<-c()
  im_bs_vec<-c()
  for (id in c(1:(end_-2))){
    ex_bs_vec<-c(ex_bs_vec,ex(id,net))
    im_bs_vec<-c(im_bs_vec,im(id,net))
  }
  ex_bs_list<-c(ex_bs_list,list(ex_bs_vec))
  im_bs_list<-c(im_bs_list,list(im_bs_vec))
}

#计算累积概率
cumulatve_pro_vec<-function(vec){
  c_p_v<-c()
  size=length(vec)
  for (i in c(1:size)){
    c_p_v<-c(c_p_v,sum(vec[c(1:i)])/sum(vec))
  }
  c_p_v
}

cumulatve_s_vec<-function(vec){
  c_s_v<-c()
  size=length(vec)
  for (i in c(1:size)){
    c_s_v<-c(c_s_v,sum(vec[c(1:i)]))
  }
  c_s_v
}

#全球贸易网络数据框
global_df<-data.frame(code=numeric(),name<-character(),ex_s=numeric(),ex_s_p=numeric(),
             im_s=numeric(),im_s_p<-numeric(),year=character(),num=numeric())
for (i in c(1:10)){
  code=id_global$code
  name=id_global$country
  ex_s=ex_global_list[[i]]
  ex_s_p=cumulatve_pro_vec(ex_s)
  ex_s=cumulatve_s_vec(ex_s)
  im_s=im_global_list[[i]]
  im_s_p=cumulatve_pro_vec(im_s)
  im_s=cumulatve_s_vec(im_s)
  year=rep(as.character(2006+i),length(code))
  num=c(1:length(code))
  df0<-data.frame(code=code,name=name,ex_s=ex_s,ex_s_p=ex_s_p,
                  im_s=im_s,im_s_p=im_s_p,year=year,num=num)
  global_df<-rbind(global_df,df0)
}

#“一带一路”贸易网络数据框
bs_df<-data.frame(code=numeric(),name<-character(),ex_s=numeric(),ex_s_p=numeric(),
                      im_s=numeric(),im_s_p<-numeric(),year=character())
for (i in c(1:10)){
  code=id_bs$node
  name=id_bs$country
  ex_s=ex_bs_list[[i]]
  ex_s_p=cumulatve_pro_vec(ex_s)
  ex_s=cumulatve_s_vec(ex_s)
  im_s=im_bs_list[[i]]
  im_s_p=cumulatve_pro_vec(im_s)
  im_s=cumulatve_s_vec(im_s)
  year=rep(as.character(2006+i),length(code))
  num=c(1:length(code))
  df0<-data.frame(code=code,name=name,ex_s=ex_s,ex_s_p=ex_s_p,
                  im_s=im_s,im_s_p=im_s_p,year=year,num=num)
  bs_df<-rbind(bs_df,df0)
}
save(global_df,file="global_df.Rdata")
save(bs_df,file="bs_df.Rdata")


load("global_df.Rdata")
load("bs_df.Rdata")
library(ggplot2)
library(gridExtra)
p_g<-ggplot(data=global_df)+theme_bw()+theme(panel.grid =element_blank(),
                                             axis.text.x = element_text(face="bold", size=8), 
                                             axis.text.y = element_text(face="bold",size=8), 
                                             axis.title=element_text(size=10,face="bold"),
                                             plot.margin = unit(c(0.5,3.5,0.5,3.5),"cm")
                                             )+scale_y_continuous(limits=c(0,1),expand=c(0,0))+scale_x_continuous(limits=c(1,199),expand=c(0,0))

p_g<-p_g+geom_point(aes(x=num,y=ex_s_p),color='blue',size=1.5)
p_g<-p_g+geom_point(aes(x=num,y=im_s_p),color='yellow',size=1.5)
p_g<-p_g+geom_line(aes(x=num,y=1/198*num-1/198),color='red',size=1.2,linetype='dashed')
p_g<-p_g+xlab("国家数目")+ylab("贸易额/总量")+geom_line(aes(x=rep(c(25,40),1990/2),y=rep(c(0.75,0.75),1990/2)),linetype=1,color='blue',size=1.2)
p_g+geom_text(aes(x=52,y=0.75,label="出口"))+geom_line(aes(x=rep(c(25,40),1990/2),y=rep(c(0.6,0.6),1990/2)),linetype=1,color='yellow',size=1.2)+geom_text(aes(x=52,y=0.6,label="进口"))

p_g



p_b<-ggplot(data=bs_df)+theme_bw()+theme(panel.grid =element_blank(),
                                             axis.text.x = element_text(face="bold", size=8), 
                                             axis.text.y = element_text(face="bold",size=8), 
                                             axis.title=element_text(size=10,face="bold"),
                                             plot.margin = unit(c(0.5,3.5,0.5,3.5),"cm")
)+scale_y_continuous(limits=c(0,1),expand=c(0,0))+scale_x_continuous(limits=c(1,63),expand=c(0,0))

p_b<-p_b+geom_point(aes(x=num,y=ex_s_p),color='blue',size=1.5)
p_b<-p_b+geom_point(aes(x=num,y=im_s_p),color='yellow',size=1.5)
p_b<-p_b+geom_line(aes(x=num,y=1/62*num-1/62),color='red',size=1.2,linetype='dashed')
p_b<-p_b+xlab("国家数目")+ylab("贸易额/总量")+geom_line(aes(x=rep(c(25,40),630/2),y=rep(c(0.75,0.75),630/2)),linetype=1,color='blue',size=1.2)
p_b+geom_text(aes(x=52,y=0.75,label="出口"))+geom_line(aes(x=rep(c(25,40),630/2),y=rep(c(0.6,0.6),630/2)),linetype=1,color='yellow',size=1.2)+geom_text(aes(x=52,y=0.6,label="进口"))

p_g




