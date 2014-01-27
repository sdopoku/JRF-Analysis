### 

source("EnsurePackage.R") # Load EnsurePackage function

# Use EnsurePackage function to load relevant libraries
EnsurePackage("ggplot2")
#EnsurePackage("xlsx")  


####Analysis of National DTP Stockout Data########
#NSO_DTP <- read.csv("/home/sela/Documents/UNICEF/JRF Analysis/JRF Analysis 2014 - N-SO DTP Data.csv") #Read in data
NSO_DTP <- read.csv("/home/sela/Documents/UNICEF/JRF Analysis/Data/JRF_Analysis_2014_NSO_DTP_Data.csv")

str(NSO_DTP) #look at structure of data

#Set Avg. duration to 3 sig figs.
NSO_DTP$Avg.duration <-signif(NSO_DTP$Avg.duration,3)
str(NSO_DTP)

###Select GAVI countries###
NSO_DTP_GAVI <- subset(NSO_DTP, NSO_DTP$GAVI=="Yes")
str(NSO_DTP_GAVI) # look at structure of data
head(NSO_DTP_GAVI,6) # look at first 6 rows

####Draw SCATTER PLOT OF AVG DURATION VS TOTAL COUNTS####
#Take subset of countries with total count >= 3 or avg distribution >= 3
#NSO_DTP_GAVI_G3 <- subset(NSO_DTP_GAVI,NSO_DTP_GAVI$Total.N.SO>=3 | 
#                            NSO_DTP_GAVI$Avg.duration >=3)
#str(NSO_DTP_GAVI_G3)

#Add label column to data
#Create new label to annotate points with on graph : C_Name, Total count, Avg. dur
NSO_DTP_GAVI$Label <- paste(NSO_DTP_GAVI$C_Name,NSO_DTP_GAVI$Total.N.SO,
                               NSO_DTP_GAVI$Avg.duration,sep=",")

##Draw basic scatter plot without BC
sp_ndtp <- ggplot(NSO_DTP_GAVI, aes(x=Total.N.SO,y=Avg.duration))+
  geom_point(size=5, colour="red",alpha=I(0.3)) #"#214478" for deep blue colour

#Add labels using geom_text
sp_ndtp + geom_text(aes(label=Label),size=4)+
  scale_x_continuous(name="Total Count of DTP Stock-out",limits=c(0,6))+
  scale_y_continuous(name="Average Duration of Stock-out(months)", limit=c(0,14))


####Draw BUBBLE PLOT OF AVG DURATION VS TOTAL COUNTS WITH BC####
#Take a subset of countries with 
NSO_DTP_GAVI_WO_MAX <- subset(NSO_DTP_GAVI, NSO_DTP_GAVI$wuenic_avg1< max(NSO_DTP_GAVI$wuenic_avg1))
NSO_DTP_GAVI_WO_MAX$Label <- paste(NSO_DTP_GAVI_WO_MAX$C_Name,NSO_DTP_GAVI_WO_MAX$wuenic_avg1,sep=",")
str(NSO_DTP_GAVI_WO_MAX)

sp_ndtp_bub <- ggplot(NSO_DTP_GAVI_WO_MAX, aes(x=Total.N.SO,y=Avg.duration, size=wuenic_avg1,,  label=Label), legend=TRUE)+
  geom_point(colour="white", fill="#214478", shape=21) #"#214478" for deep blue colour

NSO_DTP_GAVI_LS_1M <- subset(NSO_DTP_GAVI, NSO_DTP_GAVI$wuenic_avg1 < 1000000)
NSO_DTP_GAVI_LS_1M$Label <- paste(NSO_DTP_GAVI_LS_1M$C_Name,NSO_DTP_GAVI_LS_1M$wuenic_avg1,sep=",")
str(NSO_DTP_GAVI_LS_1M)le

NSO_DTP_GAVI$Label <- paste(NSO_DTP_GAVI$C_Name,NSO_DTP_GAVI$Total.N.SO,NSO_DTP_GAVI$Avg.duration,sep=",")
sp_ndtp_bub <- ggplot(NSO_DTP_GAVI, aes(x=Total.N.SO,y=Avg.duration, size=wuenic_avg1_factor,  label=Label), legend=TRUE)+
  geom_point(colour="white", fill="#214478", shape=21,alpha=I(0.8) ) #"#214478" for deep blue colour

sp_ndtp_bub + scale_size_area(max_size=15)+ geom_text(aes(label=Label),size=4)+
  scale_x_continuous(name="Total Count of DTP Stock-out",limits=c(0,6))+
  scale_y_continuous(name="Average Duration of Stock-out(months)", limit=c(0,14))

####Draw SCATTER PLOT OF  AVG DURATION VS PERC STOCK-OUT REPORTED####
#Take subset of countries with perc stock-out >=0.375 or avg distribution >=3
#NSO_DTP_GAVI_GP375 <- subset(NSO_DTP_GAVI,NSO_DTP_GAVI$Perc..N.SO >=0.375 | 
#                               NSO_DTP_GAVI$Avg.duration >=3)
#str(NSO_DTP_GAVI_GP375)

#Add label column to data
#Create new label to annotate points with on graph : C_Name, Perc. stock-out, Avg. dur
NSO_DTP_GAVI$Label <- paste(NSO_DTP_GAVI$C_Name,NSO_DTP_GAVI$Perc..N.SO,
                               NSO_DTP_GAVI$Avg.duration,sep=",")
str(NSO_DTP_GAVI)

####Draw basic scatter plot##
sp_ndtp_perc_avg <- ggplot(NSO_DTP_GAVI, aes(x=Perc..N.SO, y=Avg.duration))+
  geom_point(size=5,colour="#214478")


#Add labels using geom_text
sp_ndtp_perc_avg + geom_text(aes(label=Label), size=4)+
  scale_x_continuous(name="Percentage of Stock-out Reported", limit=c(0,1.0))+
  scale_y_continuous(name="Average Duration of Stock-out(months)", limit=c(0,14))


####Draw SCATTER PLOT OF MAX DURATION VS TOTAL COUNTS####
#Take subset of countries with total count >= 3 or max distribution >= 3
#NSO_DTP_GAVI_Max3 <- subset(NSO_DTP_GAVI,NSO_DTP_GAVI$Total.N.SO >=3 | 
#                            NSO_DTP_GAVI$Max.duration >=3)
#str(NSO_DTP_GAVI_Max3)

#Add label column to data
#Create new label to annotate points with on graph : C_Name, Total count, Max dur
NSO_DTP_GAVI$Label <- paste(NSO_DTP_GAVI$C_Name,NSO_DTP_GAVI$Total.N.SO,
                               NSO_DTP_GAVI$Max.duration,sep=",")
str(NSO_DTP_GAVI)

####Draw basic scatter plot without BC###
sp_ndtp_cnt_max <- ggplot(NSO_DTP_GAVI, aes(x=Total.N.SO,y=Max.duration))+
  geom_point(size=5, colour="#214478", alpha=I(0.8)) #"#214478" for deep blue colour

#Add labels using geom_text
sp_ndtp_cnt_max + geom_text(aes(label=Label),size=4)+
  scale_x_continuous(name="Total Count of DTP Stock-out",limits=c(0,6))+
  scale_y_continuous(name="Maximum Duration of Stock-out(months)", limit=c(0,14))

####Draw SCATTER PLOT OF MIN DURATION VS TOTAL COUNTS####
#Take subset of countries with total count >= 3 or max distribution >= 3
NSO_DTP_GAVI_Min3 <- subset(NSO_DTP_GAVI,NSO_DTP_GAVI$Total.N.SO >=3 | 
                              NSO_DTP_GAVI$Min.duration >=3)
str(NSO_DTP_GAVI_Min3)

#Add label column to data
#Create new label to annotate points with on graph : C_Name, Total count, Max dur
NSO_DTP_GAVI$Label <- paste(NSO_DTP_GAVI$C_Name,NSO_DTP_GAVI$Total.N.SO,
                                 NSO_DTP_GAVI$Min.duration,sep=",")
str(NSO_DTP_GAVI)

####Draw basic scatter plot without BC###
sp_ndtp_min3 <- ggplot(NSO_DTP_GAVI, aes(x=Total.N.SO,y=Min.duration))+
  geom_point(size=5, colour="#214478") #"#214478" for deep blue colour

#Add labels using geom_text
sp_ndtp_min3 + geom_text(aes(label=Label),size=4)+
  scale_x_continuous(name="Total Count of DTP Stock-out",limits=c(0,6))+
  scale_y_continuous(name="Minimum Duration of Stock-out(months)", limit=c(0,14))

####Draw frequency distribution of Avg. duration####
NSO_DTP_SO_Dur <- read.csv("/home/sela/Documents/UNICEF/JRF Analysis/Data/JRF_Analysis_2014_SO_Duration.csv")
str(NSO_DTP_SO_Dur)

ggplot(NSO_DTP_SO_Dur, aes(x=factor(SO.Duration)))+
  geom_histogram(fill="#214478", width=0.7)+
  ylim(0,35)+ theme_bw()

NSO_DTP_GAVI$wuenic_avg1_factor <- ifelse(NSO_DTP_GAVI$wuenic_avg1 <= 5000,1,
                                          ifelse(NSO_DTP_GAVI$wuenic_avg1 > 5000 & NSO_DTP_GAVI$wuenic_avg1 <=10000, 5,
                                                 ifelse(NSO_DTP_GAVI$wuenic_avg1 > 10000 & NSO_DTP_GAVI$wuenic_avg1 <=50000,10,
                                                        ifelse(NSO_DTP_GAVI$wuenic_avg1 > 50000 & NSO_DTP_GAVI$wuenic_avg1 <=100000,15,
                                                               ifelse(NSO_DTP_GAVI$wuenic_avg1 > 100000 & NSO_DTP_GAVI$wuenic_avg1 <=500000,20,
                                                                      ifelse(NSO_DTP_GAVI$wuenic_avg1 > 500000 & NSO_DTP_GAVI$wuenic_avg1 <=1000000,25,
                                                                             ifelse(NSO_DTP_GAVI$wuenic_avg1 > 1000000 & NSO_DTP_GAVI$wuenic_avg1 <=5000000,30,
                                                                                    ifelse(NSO_DTP_GAVI$wuenic_avg1 > 5000000 & NSO_DTP_GAVI$wuenic_avg1 <=1000000,35,40))))))))

####Barplot for National DTP Stockout#### 
#Load data
NSO_DTP_YEARS <- read.csv("/home/sela/Documents/UNICEF/JRF Analysis/Data/JRF_Analysis_2014_NSO_DTP_GAVI_YEARS.csv")
str(NSO_DTP_YEARS)
NSO_DTP_YEARS$Perc <- NSO_DTP_YEARS$NSO_Count/NSO_DTP_YEARS$Countries_Responded
head(NSO_DTP_YEARS,8)

#Create barplot using gglot
EnsurePackage("ggplot2")

nso_years <- ggplot(NSO_DTP_YEARS, aes(x=factor(Year),y=NSO_Count))+
  geom_bar(stat="identity",fill="#214478", width=0.7)+
  geom_text(aes(label=NSO_Count), vjust=1.5, colour="white",
            position=position_dodge(0.9), size=4)+
  ylim(0,20)+ theme_bw()

#Plot bar graph with line at mean 
nso_years + geom_hline(yintercept=mean(NSO_DTP_YEARS$NSO_Count),linetype="dashed")

#Plot bar graph with line at median
nso_years + geom_hline(yintercept=median(NSO_DTP_YEARS$NSO_Count),linetype="dashed")

#Plot bar graph with line at mean and median
nso_years + geom_hline(yintercept=mean(NSO_DTP_YEARS$NSO_Count),linetype="dashed")+
  geom_hline(yintercept=median(NSO_DTP_YEARS$NSO_Count),linetype="dashed",colour="#782121")

####Distric level stock-out####

DSO_DTP_YEARS <- read.csv("/home/sela/Documents/UNICEF/JRF Analysis/Data/JRF_Analysis_2014_DSO_DTP_GAVI_YEARS.csv")
str(DSO_DTP_YEARS)
DSO_DTP_YEARS$Perc <- DSO_DTP_YEARS$DSO_Count/DSO_DTP_YEARS$Countries_Responded
head(DSO_DTP_YEARS,8)

#Create barplot using gglot
EnsurePackage("ggplot2")

dso_years <- ggplot(DSO_DTP_YEARS, aes(x=factor(Year),y=DSO_Count))+
  geom_bar(stat="identity",fill="#214478", width=0.7)+
  geom_text(aes(label=DSO_Count), vjust=1.5, colour="white",
            position=position_dodge(0.9), size=4)+
  ylim(0,20)+ theme_bw()

#Plot bar graph with line at mean 
dso_years + geom_hline(yintercept=mean(DSO_DTP_YEARS$DSO_Count),linetype="dashed")

#Plot bar graph with line at median
dso_years + geom_hline(yintercept=median(DSO_DTP_YEARS$DSO_Count),linetype="dashed")

#Plot bar graph with line at mean and median
dso_years + geom_hline(yintercept=mean(DSO_DTP_YEARS$DSO_Count),linetype="dashed")+
  geom_hline(yintercept=median(DSO_DTP_YEARS$DSO_Count),linetype="dashed",colour="#782121")

####Barplot of number DTP districts with SO####
DSO_DTP_GAVI_District <- read.csv("/home/sela/Documents/UNICEF/JRF Analysis/Data/JRF_Analysis_2014_DSO_DTP_District.csv")
str(DSO_DTP_GAVI_District)

DSO_DTP_GAVI_District$intervals <- cut(DSO_DTP_GAVI_District$Perc_districts_DTP_DSO,
                                       breaks=c(-1,0,10,20,30,40,50,60,70,80,90,100))

str(DSO_DTP_GAVI_District)

dso_dtp_dist_intervals <- data.frame(table(DSO_DTP_GAVI_District$intervals),c)

ggplot(dso_dtp_dist_intervals,aes(x=Var1, y=Freq))+
  geom_bar(stat="identity", fill="#214478", width=0.7)+
  ylim(0,40)+theme_bw()
