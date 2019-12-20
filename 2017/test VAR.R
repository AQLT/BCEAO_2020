library(facts)
library(vars)
library(Rconj)
library(zoo)
require(xts)
library(tseries)
require(ggplot2)
require(reshape2)
require(scales)
require(grid)

load("Input/donnees_etalonnages.RData")
ProdManuf=list()
ProdManuf$Tprecedent<-lastIndex(donnees[,"cprodm_ch"])
ProdManuf$Tcoincident<-ProdManuf$Tprecedent+0.25
ProdManuf$Tplus1<-ProdManuf$Tprecedent+0.50
ProdManuf$Tplus2<-ProdManuf$Tprecedent+0.75
ProdManuf$horizon <- switch(1 + (as.numeric(as.yearqtr(ProdManuf$Tcoincident)) %% 1)*4,2,3,2,3)

ProdManuf$moisModele=sum(!is.na(donnees[time(donnees)==ProdManuf$Tcoincident,c("facind_m1","facind_m2","facind_m3","facind_m4")]))
ProdManuf$defaultStartEst=c(1992,2)
ProdManuf$defaultEndEst=c(2014,4)
ProdManuf$donnees<-cbind(donnees,ev(donnees[,"td.p74_a17de_7ch"]))
colnames(ProdManuf$donnees)<-c(colnames(donnees),"import_de")


ProdManuf$dateFinPrevVar<-with(ProdManuf,get(paste0("Tplus",ProdManuf$horizon-1)))

ind_manuf_tppa_av=lag(donnees[,paste0("ind_manuf_tppa_m",ProdManuf$moisModele)],1)
ind_manuf_tppre_av=lag(donnees[,paste0("ind_manuf_tppre_m",ProdManuf$moisModele)],1)
cprom_ch=donnees[,"cprodm_ch"]

exo<-window(donnees[,c("ind2009Q1")], start=ProdManuf$defaultStartEst,end=lastIndex(donnees[,"cprodm_ch"]))
ProdManuf$donneesVAR1<-window(cbind(cprom_ch, ind_manuf_tppa_av, 
                                    ind_manuf_tppre_av),
                              start=ProdManuf$defaultStartEst,end=lastIndex(donnees[,"cprodm_ch"]))
ProdManuf$Var1<-VAR(ProdManuf$donneesVAR1,p=2,type="const")
ProdManuf$Var2<-VAR(ProdManuf$donneesVAR1,p=2,type="const",exogen = data.frame(x=exo))

predict(ProdManuf$Var1,n.ahead=ProdManuf$horizon,ci=0.95)
fitted(ProdManuf$Var1)
predict(ProdManuf$Var2,n.ahead=ProdManuf$horizon,ci=0.95,dumvar=data.frame(x=c(rep(0,ProdManuf$horizon))))

normality.test(ProdManuf$Var1,multivariate.only = F)
plot(stability(ProdManuf$Var1))
vars::arch.test(ProdManuf$Var1)
serial.test(ProdManuf$Var1)


plot()
fanchart(predict(ProdManuf$Var1,n.ahead=ProdManuf$horizon),names="cprom_ch",
         cis=0.9)
date_lab<-time(window(cprom_ch,start=c(1992,4),end=lastIndex(donnees[,"cprodm_ch"])+ProdManuf$horizon*0.25,extend=T))
axis(2) # plot the y axis
axis(1, labels=as.yearmon(date_lab), at=seq(from=1, by=0.25, length.out=length(date_lab)))
box()

vars::arch.test(ProdManuf$Var2)
serial.test(ProdManuf$Var2)


predict(ProdManuf$Var2,exo.fcst=as.matrix(0))
exo.fcst
plot(fitted(ProdManuf$Var2)[,1],type="l")
lines(fitted(ProdManuf$Var1)[,1],type="l",col="red")
lines(fitted(ProdManuf$Var2)[,1]-residuals(ProdManuf$Var2)[,1],col="green")
lines()

apply(residuals(ProdManuf$Var1),2,function(x) round(sqrt(sd(x)),2))
apply(residuals(ProdManuf$Var2),2,function(x) round(sqrt(sd(x)),2))
predic
sqrt(sd(residuals(ProdManuf$Var1)[,1]))
sqrt(sd(residuals(ProdManuf$Var2)[,1]))
resid(ProdManuf$Var2)
causality(ProdManuf$Var1,cause = "ind_manuf_tppa_av")

plot(ProdManuf$Var1)
summary(ProdManuf$Var1)
plot(stability(ProdManuf$Var1))
serial.test(ProdManuf$Var1)
fevd(ProdManuf$Var1,n.ahead=3)
fanchart(predict(ProdManuf$Var1,n.head=ProdManuf$horizon,ci=0.95))
sd(residuals(ProdManuf$Var1))
VARselect(ProdManuf$donneesVAR1,type="const")
refVAR(ProdManuf$Var2,thres=1.65)

seri
jarque.bera.test(residuals(ProdManuf$Var1)[,1])
jarque.bera.test(residuals(ProdManuf$Var1)[,2])
jarque.bera.test(residuals(ProdManuf$Var1)[,3])



traceSerie<-function(data,titre="",legende=colnames(data),afficheETvar=T,cex=0.6,diviserParPeriode=F){
    gg_color_hue <- function(n) {
        hues = seq(15, 375, length = n + 1)
        hcl(h = hues, l = 65, c = 100)[1:n]
    }
    
    time<-time(data)
    freq<-frequency(data)
    dataGraph<-data.frame(cbind(time,data))
    colnames(dataGraph)<-c("date",paste0("val",seq(ncol(dataGraph)-1)))
    valCouleurs<-gg_color_hue(ncol(dataGraph)-1)
    names(valCouleurs)<-colnames(dataGraph)[-1]
    
    dataGraph <- melt(dataGraph, id="date")  # convert to long format
    if (freq==4){
        periode<-Hmisc::capitalize(quarters(as.yearqtr(dataGraph$date)))
        periode<-factor(periode,levels=Hmisc::capitalize(quarters(as.yearqtr((0:3)/4))),ordered = T)
    }
    if (freq==12){
        periode<-Hmisc::capitalize(months(as.yearmon(dataGraph$date)))
        periode<-factor(periode,levels=Hmisc::capitalize(months(as.yearmon((0:11)/12))),ordered = T)
    }
    
    dataGraph<-data.frame(dataGraph,periode=periode)
    p<-ggplot(data=dataGraph,aes(x=date, y=value,group=variable,colour=variable,fill=variable))+coord_cartesian(xlim=c(min(time)+1,max(time))) +
        geom_line(size=0.70)+theme_bw()
    #Paramètres graphiques (titre, labels etc.)
    p<-p+labs(title=titre,x="",y="")+
        scale_x_continuous(breaks=scales::pretty_breaks(n=round(length(unique(round(time)))/2)),labels=function(x) substr(x,3,4)) + 
        scale_y_continuous(breaks=scales::pretty_breaks(n=12))+
        theme(plot.title=element_text(hjust = 0.5),legend.text=element_text(size=8)
        )
    if(is.mts(data)){#Il y a au moins 2 séries et on fait une légende
        
        p<-p+theme(legend.background = element_rect(fill=alpha('gray99', 0.4),colour="gray80",linetype = "solid"),legend.justification=c(0,0), legend.position=c(0,0),legend.key = element_blank())+
            scale_colour_manual(name=NULL,breaks = names(valCouleurs)
                                ,values = valCouleurs
                                ,labels=legende)
    }else{
        p<-p + theme(legend.position="none")+
            scale_colour_manual(name=NULL,breaks = c("val1", "val2","val3")
                                ,values = c("val1"="black", "val2"="red","val3"="darkgreen"))
    }
    if(afficheETvar){
        volatilite<-round(sqrt(apply(ts.union(data[,1],data[,-1]-data[,1]),2,sd,na.rm=T)),1)
        volatilite<-gsub(".",",",format(volatilite,digits=1,nsmall=1),fixed=T)
        
        text<- grobTree(textGrob(paste(paste(c("ET","RMSE IS","RMSE IS"),legende,"=",volatilite),collapse = "\n")
                                 , x=0.98,  y=0.10,hjust = 1,
                                 gp=gpar(col="black", cex=cex)))
        p<-p+annotation_custom(text)
    }
    if(diviserParPeriode){
        p<-p+facet_wrap(~periode)
    }
    p
}
mod<-modele( titreEtalonnage = "Modèle 1: Modèle linéaire au mois 3 pour le trimestre T+1 (Banque de France)", titreCourt = "Modele1TrimSuivant",
             formule = cprodm_ch ~  lag(diff(bdf_ind_prodpre_m2,1),-1) +lag(bdf_ind_prodpre_m2,-1),
             startEst=ProdManuf$defaultStartEst,endEst=ProdManuf$defaultEndEst )
mod<-previsions(mod,donnees = donnees)

y<-cprom_ch
y_prev<-ts(fitted(ProdManuf$Var1),start=c(1992,4),frequency=4)[,1]
y_prev_lag<-mod$prevInSample
y_prev_IC_var<-ts(rbind(c(tail(fitted(ProdManuf$Var1)[,1],1),NA,NA,NA),predict(ProdManuf$Var1,n.ahead=2,ci=0.95)[[1]]$cprom_ch),start=end(y_prev),frequency = 4)
colnames(y_prev_IC_var)<-colnames(predict(ProdManuf$Var1,n.ahead=2,ci=0.95)[[1]]$cprom_ch)
y_prev_IC_var<-data.frame(date=time(y_prev_IC_var),y_prev_IC_var,variable="val2",colour="darkgreen")
colnames(y_prev_IC_var)[2]<-"value"
graph_IC_VAR<-function(ci=0.95,VAR=ProdManuf$Var1){
    y_prev_IC_var<-ts(rbind(c(tail(fitted(VAR)[,1],1),NA,NA,NA),predict(ProdManuf$Var1,n.ahead=2,ci=ci)[[1]]$cprom_ch),start=end(y_prev),frequency = 4)
    colnames(y_prev_IC_var)<-colnames(predict(ProdManuf$Var1,n.ahead=2,ci=ci)[[1]]$cprom_ch)
    y_prev_IC_var<-data.frame(date=time(y_prev_IC_var),y_prev_IC_var,variable="val2")
    colnames(y_prev_IC_var)[2]<-"value"
    geom_ribbon(data=y_prev_IC_var,mapping=aes(ymin=lower, ymax=upper), alpha=0.2,show.legend = F)
}



valeurs<-window(na.omit(ts.union(y,y_prev,y_prev_lag)),start=2000)

p<-traceSerie(valeurs,legende=c("Prod manuf","Prév VAR","Prév modèle lag"),cex = 0.6)
p_IC<-p+graph_IC_VAR(0.90)+graph_IC_VAR(0.80)+graph_IC_VAR(0.70)+graph_IC_VAR(0.60)+graph_IC_VAR(0.50)+
    geom_line(data=y_prev_IC_var,linetype="dashed",size=1.,color="darkgreen")

ggsave("M:/Echanges.ECJ/Morgane Glotain/Mission Dakar/Beamer/8 - Prévision à un horizon lointain/img/Prev_t_suivant.png"
       ,plot = p,device="png",width=13,height=9,units = "cm")
ggsave("M:/Echanges.ECJ/Morgane Glotain/Mission Dakar/Beamer/8 - Prévision à un horizon lointain/img/Prev_t_suivant_IC.png",
       plot = p_IC,device="png",width=13,height=9,units = "cm")

ggsave("Z:/Prev_t_suivant.png",plot = p,device="png",width=9,height=5)
ggsave("Z:/Prev_t_suivant_IC.png",plot = p_IC,device="png",width=9,height=5)

