### 

source("EnsurePackage.R") # Load EnsurePackage function

# Use EnsurePackage function to load relevant libraries
EnsurePackage("ggplot2")
#EnsurePackage("xlsx")  


####Analysis of National DTP Stockout Data########
#NSO_DTP <- read.csv("/home/sela/Documents/UNICEF/JRF Analysis/JRF Analysis 2014 - N-SO DTP Data.csv") #Read in data
NSO_DTP <- read.csv("/home/sela/Documents/UNICEF/JRF Analysis/JRF_Analysis_2014_NSO_DTP_Data.csv")

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

NSO_DTP_test$Label <- paste(NSO_DTP_test$C_Name,NSO_DTP_test$wuenic_avg1,sep=",")
sp_ndtp_bub <- ggplot(NSO_DTP_test, aes(x=Total.N.SO,y=Avg.duration, size=wuenic_avg1,  label=Label), legend=TRUE)+
  geom_point(colour="white", fill="#214478", shape=21, alpha=I(0.9)) #"#214478" for deep blue colour

sp_ndtp_bub + scale_size_area(max_size=10)+ geom_text(aes(label=Label),size=4)+
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
  scale_x_continuous(name="Percentage of Stock-out Reported", limit=c(0,0.8))+
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
  geom_point(size=5, colour="#214478") #"#214478" for deep blue colour

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
NSO_DTP_GAVI_Min3$Label <- paste(NSO_DTP_GAVI_Min3$C_Name,NSO_DTP_GAVI_Min3$Total.N.SO,
                                 NSO_DTP_GAVI_Min3$Min.duration,sep=",")
str(NSO_DTP_GAVI_Max3)

####Draw basic scatter plot without BC###
sp_ndtp_min3 <- ggplot(NSO_DTP_GAVI_Min3, aes(x=Total.N.SO,y=Min.duration))+
  geom_point(size=5, colour="#214478") #"#214478" for deep blue colour

#Add labels using geom_text
sp_ndtp_min3 + geom_text(aes(label=Label),size=4)+
  scale_x_continuous(name="Total Count of DTP Stock-out",limits=c(0,6))+
  scale_y_continuous(name="Minimum Duration of Stock-out(months)", limit=c(0,14))

####Draw frequency distribution of Avg. duration####
NSO_DTP_SO_Dur <- read.csv("/home/sela/Documents/UNICEF/JRF Analysis/JRF Analysis 2014 - SO Duration Data.csv")
str(NSO_DTP_SO_Dur)

ggplot(NSO_DTP_SO_Dur, aes(x=factor(SO.Duration)))+
  geom_histogram(fill="#214478", width=0.7)+
  ylim(0,35)+ theme_bw()
