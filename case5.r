setwd('/Users/liuyang/Desktop/UTOFUN/case6/')
library(gdata)
library(graphics)
library(ggplot2)
####----------year over year function-------
YearOverYear<-function (x,periodsPerYear){
  if(NROW(x)<=periodsPerYear){
    stop("too few rows")
  }
  else{
    indexes<-1:(NROW(x)-periodsPerYear)
    return(c(rep(NA,periodsPerYear),(x[indexes+periodsPerYear]-x[indexes])/x[indexes]))
  }
}

cal_yoy<-function(x){
  x<-cbind(x,YOY_low=YearOverYear(x$LowTier,12))
  x<-cbind(x,YOY_middle=YearOverYear(x$MiddleTier,12))
  x<-cbind(x,YOY_high=YearOverYear(x$HighTier,12))
  x<-cbind(x,YOY_whole=YearOverYear(x$Whole,12))
  
  return(x)
}

#####---------read data function------------
read_tier<-function(x){
  tier<-read.xls('395144_sa-cs-tieredprices-0830.xls',sheet=x,header=TRUE,
                 stringsAsFactors=FALSE)##is.as=TRUE
  tier<-tier[-c(1,2),1:6]
  colnames(tier)<-c('Year','Month','LowTier','MiddleTier','HighTier','Whole')
  row.names(tier)<-1:nrow(tier)
  
  tier$Year<-as.numeric(tier$Year)
  tier$Month<-as.numeric(tier$Month)
  tier$LowTier<-as.numeric(tier$LowTier)
  tier$MiddleTier<-as.numeric(tier$MiddleTier)
  tier$HighTier<-as.numeric(tier$HighTier)
  tier$Whole<-as.numeric(tier$Whole)
  
  return(tier)
}

######-----------aggregate data-----------------------
aggregate_tier<-function(x){
  attach(x)
  year_ave<-aggregate(x,by=list(Group.Year=Year),FUN=mean)
  year_ave<-year_ave[-c(2,3)]
  detach(x)
  colnames(year_ave)<-c('Year','LowTier','MiddleTier','HighTier','Whole',
                        'YOY_Low','YOY_Middle','YOY_High','YOY_Whole')
  return(year_ave)
}
########------------read and plot others--------------
######--------calculate yoy----------------
tier_bos<-read_tier(2)
tier_bos<-cal_yoy(tier_bos)
year_ave_bos<-aggregate_tier(tier_bos)

tier_la<-read_tier(7)
tier_la<-cal_yoy(tier_la)
year_ave_la<-aggregate_tier(tier_la)

tier_ny<-read_tier(10)
tier_ny<-cal_yoy(tier_ny)
year_ave_ny<-aggregate_tier(tier_ny)

tier_sf<-read_tier(14)
tier_sf<-cal_yoy(tier_sf)
year_ave_sf<-aggregate_tier(tier_sf)


tier_chi<-read_tier(3)
tier_chi<-cal_yoy(tier_chi)
year_ave_chi<-aggregate_tier(tier_chi)


########----------plot tier--------------------------------
library(scales)
title<-'Tier House Price Index Analysis in '
xlab<-'Year'
ylab<-'Year-Over-Year Change in Case Shiller Index '
cl<-c('dodgerblue2',
      'seagreen2',
      'palevioletred2',
      'black')
###
p1<-ggplot(year_ave_bos,aes(Year))+
  geom_line(aes(y=YOY_Low,colour='LowTier'))+
  geom_line(aes(y=YOY_Middle,colour='MiddleTier'))+
  geom_line(aes(y=YOY_High,colour='HighTier'))+
  geom_line(aes(y=YOY_Whole,colour='Whole'))+
  scale_colour_manual(name='TierName',values=c('LowTier'=cl[1],'MiddleTier'=cl[2],'HighTier'=cl[3],'Whole'=cl[4]))+
  scale_x_discrete(limit=seq(1992,2016,2))+
  scale_y_continuous(labels=scales::percent)

theme1<-theme(axis.text.x=element_text(size=15),
              axis.text.y=element_text(size=15),
              axis.title.x=element_text(size=15),
              axis.title.y=element_text(size=15),
              plot.title=element_text(size=20),
              legend.text=element_text(size=16)
              )

p1<-p1 %+% year_ave_sf

p1+theme1+labs(title=paste(title,'San Francisco'),x=xlab,y=ylab)+
  geom_vline(xintercept = 2012,linetype='dotted')+
  geom_hline(yintercept = 0,linetype='dotted')

#####---------plot whole change monthly----------------------------
c<-c(353,354)
tier_boschangemonthly<-tier_bos[c,]
tier_boschangemonthly$City<-c('Boston','Boston')

tier_chichangemonthly<-tier_chi[c(293,294),]
tier_chichangemonthly$City<-c('Chicago','Chicago')

tier_lachangemonthly<-tier_la[c,]
tier_lachangemonthly$City<-c('Los Angeles','Los Angeles')

tier_sfchangemonthly<-tier_sf[c,]
tier_sfchangemonthly$City<-c('San Francisco','San Francisco')

tier_nychangemonthly<-tier_ny[c,]
tier_nychangemonthly$City<-c('New York','New York')

library(reshape)
mergemonth<-merge_all(list(tier_nychangemonthly,tier_sfchangemonthly,
                           tier_lachangemonthly,tier_chichangemonthly,
                           tier_boschangemonthly))
mergemonth$YOY_low<-lapply(mergemonth$YOY_low,round,3)
mergemonth$YOY_middle<-lapply(mergemonth$YOY_middle,round,3)
mergemonth$YOY_high<-lapply(mergemonth$YOY_high,round,3)
mergemonth$YOY_whole<-lapply(mergemonth$YOY_whole,round,3)

mergemonth$YOY_low<-as.numeric(mergemonth$YOY_low)
mergemonth$YOY_middle<-as.numeric(mergemonth$YOY_middle)
mergemonth$YOY_high<-as.numeric(mergemonth$YOY_high)
mergemonth$YOY_whole<-as.numeric(mergemonth$YOY_whole)

p2<-qplot(Month,YOY_whole,data=mergemonth,
          facets = .~City,fill=City,label=paste(YOY_whole*100,'%'))+
  geom_bar(stat='identity')+scale_x_discrete(limits=c(5,6),labels=c('May','Jun'))+
  theme(panel.margin=unit(1.2,'lines'))+geom_text(aes(y=YOY_whole+0.002),size=5,
                                                  position = position_dodge(0.9),vjust=0)+
  labs(y=ylab)+
  theme(text = element_text(size=18),
                     axis.text.x=element_text(size=20),
                     axis.text.y=element_text(size=20),
                     axis.title.x=element_text(size=18),
                     axis.title.y=element_text(size=18),
                     legend.text=element_text(size=18))+
  scale_y_continuous(labels=scales::percent)
  
p2
#####------------change yearly-------------
c2<-c(342,354)
tier_boschangeyearly<-tier_bos[c2,]
tier_boschangeyearly$City<-'Boston'

tier_chichangeyearly<-tier_chi[c(282,294),]
tier_chichangeyearly$City<-'Chicago'

tier_lachangeyearly<-tier_la[c2,]
tier_lachangeyearly$City<-'Los Angeles'

tier_nychangeyearly<-tier_ny[c2,]
tier_nychangeyearly$City<-'New York'

tier_sfchangeyearly<-tier_sf[c2,]
tier_sfchangeyearly$City<-'San Francisco'

mergeyear<-merge_all(list(tier_boschangeyearly,tier_chichangeyearly,
                          tier_lachangeyearly,tier_nychangeyearly,
                          tier_sfchangeyearly))
mergeyear$YOY_low<-lapply(mergeyear$YOY_low,round,3)
mergeyear$YOY_middle<-lapply(mergeyear$YOY_middle,round,3)
mergeyear$YOY_high<-lapply(mergeyear$YOY_high,round,3)
mergeyear$YOY_whole<-lapply(mergeyear$YOY_whole,round,3)

mergeyear$YOY_low<-as.numeric(mergeyear$YOY_low)
mergeyear$YOY_middle<-as.numeric(mergeyear$YOY_middle)
mergeyear$YOY_high<-as.numeric(mergeyear$YOY_high)
mergeyear$YOY_whole<-as.numeric(mergeyear$YOY_whole)

p3<-qplot(Year,YOY_whole,data=mergeyear,facets = .~City,fill=City,label=paste(YOY_whole*100,'%'))+
  geom_bar(stat='identity')+scale_x_discrete(limits=c(2015,2016))+
  geom_text(aes(y=YOY_whole+0.002),size=5)+theme(panel.margin=unit(0.5,'lines'))+
  labs(y=ylab,x='June')+theme(text = element_text(size=18),
                             axis.text.x=element_text(size=20),
                             axis.text.y=element_text(size=20),
                             axis.title.x=element_text(size=18),
                             axis.title.y=element_text(size=18),
                             legend.text=element_text(size=18))+
  scale_y_continuous(labels=scales::percent)
p3

####### jun vs may---------------------
mom<-function(x){
  monchange<-(tail(x,1)-tail(x,2))[1]/tail(x,2)[1]
  monchange<-lapply(monchange, round,3)
  return(monchange)
}
c_city<-c(1:5)
cityName<-c('Boston','Chicago','Los Angeles','New York','San Francisco')
c_mom<-c(mom(tier_bos$Whole),mom(tier_chi$Whole),
         mom(tier_la$Whole),mom(tier_ny$Whole),
         mom(tier_sf$Whole))

momchange<- as.data.frame(cbind(c_city,c_mom))
momchange$c_city<-as.numeric(momchange$c_city)
momchange$c_mom<-as.numeric(momchange$c_mom)
title4<-'Change between May and June in 2016'
colnames(momchange)<-c('City','Change')
p4<-qplot(City,Change,data=momchange,fill=cityName,label=paste(Change*100,'%'))+
  geom_bar(stat='identity')+
  geom_text(aes(y=Change+0.0007),size=5)+
  scale_x_discrete(limits=c(1:5),labels=cityName)+
  labs(y='Case Shiller Index Change',x='City',title=title4)+theme(text = element_text(size=18),
                             axis.text.x=element_text(size=20),
                             axis.text.y=element_text(size=20),
                             axis.title.x=element_text(size=18),
                             axis.title.y=element_text(size=18),
                             legend.text=element_text(size=18))+
  scale_y_continuous(labels=scales::percent)
p4

####-------------map---------------
location1<-c('Boston','Chicago','Los Angeles','New York','San Francisco')
library(ggmap)

df <- round(data.frame(
  x = c(42.3600825,41.8781136,40.7127837,34.0522342,
        37.7749295),
  y = c(-71.05888010000001,-87.62979819999998,
        -74.00594130000002,-118.2436849,-122.41941550000001),digits=2))
  
map <- get_googlemap('USA', markers = df, scale = 1)
ggmap(map,extent = 'device')
