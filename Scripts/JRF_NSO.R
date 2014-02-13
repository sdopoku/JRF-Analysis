# Load EnsurePackage function to use packages
source("EnsurePackage.R") 

EnsurePackage("ggplot2")

####Load National Stock-out data####
data_location <- file.choose()
nso_data <- read.csv(data_location)

#Check structure of data
str(nso_data)


#Set columns 5 through 27  to 3 significant figures
nso_data[,5:27] <- signif(nso_data[,5:27],3)


                                      ##AGGREGATE DATA ANALYSIS AND VISUALIZATION###
#crrete dataset of aggregate columns
nso_agg_data <- data.frame(nso_data[,1:12])

              #Scatter Plot of Average duration vs Total SO Count#

#Create new label to annotate points with on graph : C_Name, Total count, Avg. dur
nso_agg_data$label <- paste(nso_agg_data$C_Name,nso_agg_data$Agg_Total_Count,
                            nso_agg_data$Agg_Avg_Dur,sep=",")

#Draw basic scatter plot without birth cohort
sp_nso_agg <- ggplot(nso_agg_data, aes(x=Agg_Total_Count,y=Agg_Avg_Dur))+
  geom_point(size=5, colour="#214478",alpha=I(0.6)) +#"#214478" for deep blue colour
  theme_bw()

sp_nso_agg + geom_text(aes(label=label),size=4)+
  scale_x_continuous(name="Total Count of Aggregate Stock-out",limit=c(0,14))+
  scale_y_continuous(name="Average Duration of Stock-out(months)", limit=c(0,14))

              #Draw basic scatter plot with birth cohort

#Create new scale for birth cohort. This will be used to create sizes of bubbles. 
#Note: Could use more efficient technique with cut
nso_agg_data$birth_scale <- ifelse(nso_agg_data$Births_UNDP_2012 <= 5000,1,
                                          ifelse(nso_agg_data$Births_UNDP_2012 > 5000 & nso_agg_data$Births_UNDP_2012 <=10000, 5,
                                                 ifelse(nso_agg_data$Births_UNDP_2012 > 10000 & nso_agg_data$Births_UNDP_2012 <=50000,10,
                                                        ifelse(nso_agg_data$Births_UNDP_2012 > 50000 & nso_agg_data$Births_UNDP_2012 <=100000,15,
                                                               ifelse(nso_agg_data$Births_UNDP_2012 > 100000 & nso_agg_data$Births_UNDP_2012 <=500000,20,
                                                                      ifelse(nso_agg_data$Births_UNDP_2012 > 500000 & nso_agg_data$Births_UNDP_2012 <=1000000,25,
                                                                             ifelse(nso_agg_data$Births_UNDP_2012 > 1000000 & nso_agg_data$Births_UNDP_2012 <=5000000,30,
                                                                                    ifelse(nso_agg_data$Births_UNDP_2012 > 5000000 &nso_agg_data$Births_UNDP_2012 <=1000000,35,40))))))))



#Plot bubble chart
sp_nso_agg_bub <- ggplot(nso_agg_data, aes(x=Agg_Total_Count,y=Agg_Avg_Dur, size=birth_scale, label=label), legend=TRUE)+
  geom_point(colour="white", fill="#214478",alpha=I(0.6), shape=21) #"#214478" for deep blue colour

sp_nso_agg_bub + scale_size_area(max_size=15)+ geom_text(aes(label=label),size=4)+
  scale_x_continuous(name="Total Count of Aggregate Stock-out",limits=c(0,14))+
  scale_y_continuous(name="Average Duration of Stock-out(months)", limit=c(0,14))+
  theme_bw()


####Load NSO Aggregate Duration Distribution Data####
#select location of CSV data
data_location <- file.choose() 

#Read CSV data file into data frame
nso_dur_dist <- read.csv(data_location)

str(nso_dur_dist)

#Create dataframes of counts
count_agg_nso_dur <- as.data.frame(table(nso_dur_dist$NSO_DUR_AGG))
count_dtp_nso_dur <- as.data.frame(table(nso_dur_dist$NSO_DUR_DTP))
count_opv_nso_dur <- as.data.frame(table(nso_dur_dist$NSO_DUR_OPV))
count_mv_nso_dur <- as.data.frame(table(nso_dur_dist$NSO_DUR_MV))

#Add percentage column
#Scale to 100 and round of to 1 d.p
count_agg_nso_dur$Perc <- round(prop.table(count_agg_nso_dur$Freq)*100,1)
count_dtp_nso_dur$Perc <- round(prop.table(count_dtp_nso_dur$Freq)*100,1)
count_opv_nso_dur$Perc <- round(prop.table(count_opv_nso_dur$Freq)*100,1)
count_mv_nso_dur$Perc <- round(prop.table(count_mv_nso_dur$Freq)*100,1)

#Create bar plot of NSO AGGREGATE duration
hst_agg_nso_dur <- ggplot(count_agg_nso_dur, aes(x=factor(Var1), y=Freq))+
  geom_bar(stat= "identity", fill="#214478", width=0.7)+
  ylim(0,80)+ theme_bw()+
  geom_text(aes(label=paste(Freq,"(",Perc,")",sep="")), colour="black", vjust=-0.2,size=2.5)


#Create bar plot for NSO DTP duration
hst_dtp_nso_dur <- ggplot(count_dtp_nso_dur, aes(x=factor(Var1), y=Freq))+
  geom_bar(stat= "identity", fill="#214478", width=0.7)+
  ylim(0,80)+ theme_bw()+
  geom_text(aes(label=paste(Freq,"(",Perc,")",sep="")), colour="black", vjust=-0.2,size=2.5)

#Create bar plot for NSO OPV duration
hst_opv_nso_dur <- ggplot(count_opv_nso_dur, aes(x=factor(Var1), y=Freq))+
  geom_bar(stat= "identity", fill="#214478", width=0.7)+
  ylim(0,80)+ theme_bw()+
  geom_text(aes(label=paste(Freq,"(",Perc,")",sep="")), colour="black", vjust=-0.2,size=2.5)

#Create bar plot of NSO MV duration
hst_mv_nso_dur <- ggplot(count_mv_nso_dur, aes(x=factor(Var1), y=Freq))+
  geom_bar(stat= "identity", fill="#214478", width=0.7)+
  ylim(0,80)+ theme_bw()+
  geom_text(aes(label=paste(Freq,"(",Perc,")",sep="")), colour="black", vjust=-0.2,size=2.5)

#Plot bar graphs on 2x2 window
EnsurePackage("gridExtra")    # Package for plotting multiple graphs in same window
grid.arrange(hst_agg_nso_dur, hst_dtp_nso_dur, hst_opv_nso_dur, hst_mv_nso_dur,ncol=2)
