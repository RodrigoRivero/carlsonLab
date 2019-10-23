library(tidyr)
library(dplyr)
library(stringr)
library(gtools)
library(MatchIt)
library(ggplot2)
library(cobalt)
library(broom)
library(purrr)
library(tableone)
library(ggpubr)
library(ggrepel)
library(egg)
library(readr)
library(car)
library(lubridate)
library(Hmisc)

## Import NPP and cAll01 tables from CSV

nppc <- read.csv("/Users/Char/Dropbox/char_HCV_table_data_fall2019/npp_nonRSPO_forMatching.csv")

View(nppc)
colnames(nppc)


## Subset the combined dataset and drop rows with NA so that the Match It program accepts the input data

npp_cAll_both_subset <- nppc %>% 
  select(unique_ID, rspo_npp, island, province, area_km2, elev, slope, opl_dist, roads_dist, 
         city_dist, f90, peat, f90_defor_01, f90_defor_02, f90_defor_03,f90_defor_04, f90_defor_05, 
         f90_defor_06, f90_defor_07, f90_defor_08, f90_defor_09,  f90_defor_10, f90_defor_11, 
         f90_defor_12, f90_defor_13, f90_defor_14, f90_defor_15, f90_defor_16, precip_2000, 
         precip_2001, precip_2002, precip_2003, precip_2004, precip_2005, precip_2006, 
         precip_2007, precip_2008, precip_2009, temp_2000, temp_2001, temp_2002, temp_2003, 
         temp_2004, temp_2005, temp_2006, temp_2007, temp_2008, temp_2009) %>%
  drop_na(.)
  
View(npp_cAll_both_subset)


########################################## T-test Table for Un-matched Sample ###############################################

myVars <- names(npp_cAll_both_subset)[5:44]

indo.stats.raw<-CreateTableOne(vars = myVars, 
                         data = npp_cAll_both_subset,
                         strata = 'rspo_npp',
                         smd = T)

indo.stats.raw <- data.frame(print(indo.stats.raw,
                  printToggle = FALSE,
                  noSpaces = TRUE,smd=TRUE))

indo.stats.raw$var <- row.names(indo.stats.raw)

#View(indo.stats.raw)


################################### Propensity Score Matching ##############################################
############ Matching with Caliper##############################

npp_forMatch <- npp_cAll_both_subset %>%
  drop_na(elev, slope, area_km2, opl_dist, roads_dist, city_dist, f90, peat, f90_defor_01, f90_defor_02, f90_defor_03,f90_defor_04, f90_defor_05, f90_defor_06, f90_defor_07, f90_defor_08, f90_defor_09, precip_2000, precip_2001, precip_2002, precip_2003, precip_2004, precip_2005, precip_2006, precip_2007, precip_2008, precip_2009, temp_2000, temp_2001, temp_2002, temp_2003, temp_2004, temp_2005, temp_2006, temp_2007, temp_2008, temp_2009)


#View(npp_forMatch)

m.out.indo =matchit(rspo_npp ~  city_dist + slope + elev + opl_dist + roads_dist + area_km2 + f90_defor_01 + f90_defor_02 + f90_defor_03 +f90_defor_04 + f90_defor_05 + f90_defor_06 + f90_defor_07 + f90_defor_08 + f90_defor_09 + precip_2001 + precip_2002 + precip_2003 + precip_2004 + precip_2005 + precip_2006 + precip_2007 + precip_2008 + precip_2009 +  temp_2001 + temp_2002 + temp_2003 + temp_2004 + temp_2005 + temp_2006 + temp_2007 + temp_2008 + temp_2009, data = npp_forMatch, method="nearest", distance="logit", caliper=.25, ratio = 10)

# Print the balance information to the console
bal.tab(m.out.indo, m.threshold = 0.1)
bal.tab(m.out.indo, v.threshold = 2)

# View a summary of the match
summary(m.out.indo)

############################## BALANCE PLOTS #####################################################

#Display jitter plot of the match
#plot(m.out.indo, type = "jitter")
# Display a histogram of the match
#plot(m.out.indo, type = "hist")

# Create a bal plot of the match
#plot.indo.elev <-bal.plot(m.out.indo, var.name = "elev", which = "both")
#plot.indo.slope <- bal.plot(m.out.indo, var.name = "slope", which = "both")
#plot.indo.opl_dist <- bal.plot(m.out.indo, var.name = "opl_dist", which = "both")
#plot.indo.roads_dist <- bal.plot(m.out.indo, var.name = "roads_dist", which = "both")
#plot.indo.city_dist <- bal.plot(m.out.indo, var.name = "city_dist", which = "both")
#plot.indo.f90_defor_01 <- bal.plot(m.out.indo, var.name = "f90_defor_01", which = "both")

# Arrange the balance plots on a panel
#indo.bal.plots <- ggarrange(plot.indo.elev, plot.indo.slope, plot.indo.opl_dist, plot.indo.city_dist, plot.indo.roads_dist, plot.indo.f90_defor_01 ,  ncol =3, nrow=2)

# Save the balance plots
#ggsave(filename="indo.balance.plots.JPG", plot = indo.bal.plots, height = 12, width = 7)

########################################## T-test Table for Matched Sample ###############################################

# Export the matched data for the summary tables
m.out.indo.match<-match.data(m.out.indo)[1:ncol(npp_cAll_both_subset)]

#View(m.out.indo.match)

# Set the variable names for the summary table
myVars <- names(npp_cAll_both_subset)[5:44]

# Create the summary table
indo.match<-CreateTableOne(vars = myVars, 
                         data = m.out.indo.match,
                         strata = 'rspo_npp',
                         smd = T)

# Export the summary table to data frame
indo.match <- data.frame(print(indo.match,
                  printToggle = FALSE,
                  noSpaces = TRUE,smd=TRUE))

# Assign the row names to a column
indo.match$var <- row.names(indo.match)

#View(indo.match)




#####  Export the data frame of the matched treatment and contoled plantations

m.out.indo.data<-match.data(object=m.out.indo, group="all", distance = "distance",
                            weights = "weights", subclass = "subclass")

View(m.out.indo.data)

############ TESTING FOR PROGNOSTIC SCORE #######################

#ctrl.data.indo <- npp_forMatch[npp_forMatch$rspo_npp == 0,]
#ctrl.fit.indo <- glm(mort ~ Xcont.1+Xcont.2+Xcat.1+Xcat.2, data = ctrl.data)
#dt$prog.score <- predict(ctrl.fit, dt)
#bal.tab(m.out.indo, distance = npp_forMatch["prog.score"])

######################################### TESTING FOR MODEL MIS-SPECIFICATION ################### 
#Check for model mis-specification

#mod1<-glm(rspo_npp~city_dist + slope + elev + opl_dist + roads_dist +  f90_defor_01 + f90_defor_02 + f90_defor_03 +f90_defor_04 + f90_defor_05 + f90_defor_06 + f90_defor_07 + f90_defor_08 + f90_defor_09 + precip_2001 + precip_2002 + precip_2003 + precip_2004 + precip_2005 + precip_2006 + precip_2007 + precip_2008 + precip_2009 +  temp_2001 + temp_2002 + temp_2003 + temp_2004 + temp_2005 + temp_2006 + temp_2007 + temp_2008 + temp_2009, data = npp_forMatch,family = binomial)

# View a plot of the residuals. Distribution of residuals should not change vertically in height
#residualPlots(mod1,terms=~city_dist + slope + elev + opl_dist + roads_dist +  f90_defor_01 + f90_defor_02 + f90_defor_03 +f90_defor_04 + f90_defor_05 + f90_defor_06 + f90_defor_07 + f90_defor_08 + f90_defor_09 + precip_2001 + precip_2002 + precip_2003 + precip_2004 + precip_2005 + precip_2006 + precip_2007 + precip_2008 + precip_2009 +  temp_2001 + temp_2002 + temp_2003 + temp_2004 + temp_2005 + temp_2006 + temp_2007 + temp_2008 + temp_2009,fitted=T)



######################## Matching Within Island #############################

# Make kalimantan subset dataset
kali <- npp_forMatch %>%
  filter(island == "Kalimantan") 

View(kali)

# Make papua subset dataset
papua <- npp_forMatch %>%
  filter(island == "Papua")

View(papua)

# Make sumatra subset dataset
sumatra <- npp_forMatch %>%
  filter(island == "Sumatra")

View(sumatra)


########### Kalimantan Matching ####################

m.out.kali =matchit(rspo_npp ~  city_dist + slope + elev + opl_dist + roads_dist +  f90_defor_01 + f90_defor_02 + f90_defor_03 +f90_defor_04 + f90_defor_05 + f90_defor_06 + f90_defor_07 + f90_defor_08 + f90_defor_09 + precip_2001 + precip_2002 + precip_2003 + precip_2004 + precip_2005 + precip_2006 + precip_2007 + precip_2008 + precip_2009 +  temp_2001 + temp_2002 + temp_2003 + temp_2004 + temp_2005 + temp_2006 + temp_2007 + temp_2008 + temp_2009, data = kali, method="nearest", distance="logit", caliper=.25, ratio = 4)

# Print the balance information to the console
bal.tab(m.out.kali, m.threshold = 0.1)


############ Papua Matching ########################

m.out.papua =matchit(rspo_npp ~  city_dist + slope + elev + opl_dist + roads_dist +  f90_defor_01 + f90_defor_02 + f90_defor_03 +f90_defor_04 + f90_defor_05 + f90_defor_06 + f90_defor_07 + f90_defor_08 + f90_defor_09 + precip_2001 + precip_2002 + precip_2003 + precip_2004 + precip_2005 + precip_2006 + precip_2007 + precip_2008 + precip_2009 +  temp_2001 + temp_2002 + temp_2003 + temp_2004 + temp_2005 + temp_2006 + temp_2007 + temp_2008 + temp_2009, data = papua, method="nearest", distance="logit", caliper=.25, ratio = 2)

# Print the balance information to the console
bal.tab(m.out.papua, m.threshold = 0.1)


write.csv(complete.indo.summary, "/Users/Char/Desktop/summary_output.csv")

############ Sumatra Matching ####################

m.out.sumatra =matchit(rspo_npp ~  city_dist + slope + elev + opl_dist + roads_dist +  f90_defor_01 + f90_defor_02 + f90_defor_03 +f90_defor_04 + f90_defor_05 + f90_defor_06 + f90_defor_07 + f90_defor_08 + f90_defor_09 + precip_2001 + precip_2002 + precip_2003 + precip_2004 + precip_2005 + precip_2006 + precip_2007 + precip_2008 + precip_2009 +  temp_2001 + temp_2002 + temp_2003 + temp_2004 + temp_2005 + temp_2006 + temp_2007 + temp_2008 + temp_2009, data = sumatra, method="nearest", distance="logit", caliper=.25, ratio=6)

# Print the balance information to the console
bal.tab(m.out.sumatra, m.threshold = 0.1)



########################## BUILD SUMMARY TABLE ##############################

matched.summary <- indo.match %>%
  tbl_df(.) %>%
  select(var, X0, X1, p, -test, SMD) %>%
  mutate(var = str_replace(var, " \\(.*\\)", "")) %>% 
  separate(col=X0, into= c("Mean_Control", "SD_Control"), sep="[(]") %>%
  separate(col=X1, into= c("Mean_Treated", "SD_Treated"), sep="[(]") %>%
  mutate(SD_Treated = str_extract(SD_Treated, "-?[0-9.]+")) %>%
  mutate(SD_Control = str_extract(SD_Control, "-?[0-9.]+")) %>%
  select(var, Mean_Control, Mean_Treated, SD_Control, SD_Treated, everything()) %>%
  add_row(var="Sample", Mean_Control = "Post-Matching" , Mean_Treated ="Post-Matching", SD_Control="Post-Matching", SD_Treated="Post-Matching", p="Post-Matching", SMD="Post-Matching",  .before=TRUE) %>%
  rename_at(vars(-var), ~ paste0("post_matching_",.))
  

str(matched.summary)
View(matched.summary)

###### Create the table or the un-matched data ##########
unmatched.summary <- indo.stats.raw %>%
  tbl_df(.) %>%
  select(var, X0, X1, p, -test, SMD) %>%
  mutate(var = str_replace(var, " \\(.*\\)", "")) %>% 
  separate(col=X0, into= c("Mean_Control", "SD_Control"), sep="[(]") %>%
  separate(col=X1, into= c("Mean_Treated", "SD_Treated"), sep="[(]") %>%
  mutate(SD_Treated = str_extract(SD_Treated, "-?[0-9.]+")) %>%
  mutate(SD_Control = str_extract(SD_Control, "-?[0-9.]+")) %>%
  select(var, Mean_Control, Mean_Treated, SD_Control, SD_Treated, everything()) %>%
  add_row(var="Sample", Mean_Control = "Pre-Matching" , Mean_Treated ="Pre-Matching", SD_Control="Pre-Matching", SD_Treated="Pre-Matching", p="Pre-Matching", SMD="Pre-Matching",  .before=TRUE) %>%
  rename_at(vars(-var), ~ paste0("pre_matching_",.))

View(unmatched.summary)

# Bind together the before and after matching data frames
complete.indo.summary<- bind_cols(unmatched.summary, matched.summary)

View(complete.indo.summary)

################################################3#############################################3############################################### Plotting Parallel Trends ####################


## Melt the annual defor rate columns in preparation for graphing
####### Prepare the raw before matching data for graphing ##########

forGraphs.indo.raw<- npp_forMatch %>% 
  group_by(rspo_npp) %>%
  select(rspo_npp, slope, elev, city_dist, opl_dist, roads_dist, f90_defor_01, f90_defor_02, f90_defor_03, f90_defor_04, f90_defor_05, f90_defor_06, f90_defor_07, f90_defor_08, f90_defor_09, f90_defor_10, f90_defor_11, f90_defor_12, f90_defor_13, f90_defor_15, f90_defor_16) %>%
  gather(key = defor_year, value = defor_rate, -c(rspo_npp, elev, slope, opl_dist, city_dist, roads_dist)) 

View(forGraphs.indo.raw)

######### After matching data for graphing ###########
forGraphs.indo<- m.out.indo.data %>% 
  group_by(rspo_npp) %>%
  select(rspo_npp, slope, elev, city_dist, opl_dist, roads_dist, f90_defor_01, f90_defor_02, f90_defor_03, f90_defor_04, f90_defor_05, f90_defor_06, f90_defor_07, f90_defor_08, f90_defor_09, f90_defor_10, f90_defor_11, f90_defor_12, f90_defor_13, f90_defor_15, f90_defor_16) %>%
  gather(key = defor_year, value = defor_rate, -c(rspo_npp, elev, slope, opl_dist, city_dist, roads_dist)) 

View(forGraphs.indo)

# Find and replace the old column names with the year of the data observation
forGraphs.indo <- forGraphs.indo  %>% 
  mutate_if(is.character, str_replace_all, pattern = 'f90_defor_01', replacement = '2001') %>%
  mutate_if(is.character, str_replace_all, pattern = 'f90_defor_02', replacement = '2002') %>%
  mutate_if(is.character, str_replace_all, pattern = 'f90_defor_03', replacement = '2003') %>%
  mutate_if(is.character, str_replace_all, pattern = 'f90_defor_04', replacement = '2004') %>%
  mutate_if(is.character, str_replace_all, pattern = 'f90_defor_05', replacement = '2005') %>%
  mutate_if(is.character, str_replace_all, pattern = 'f90_defor_06', replacement = '2006') %>%
  mutate_if(is.character, str_replace_all, pattern = 'f90_defor_07', replacement = '2007') %>%
  mutate_if(is.character, str_replace_all, pattern = 'f90_defor_08', replacement = '2008') %>%
  mutate_if(is.character, str_replace_all, pattern = 'f90_defor_09', replacement = '2009') %>%
  mutate_if(is.character, str_replace_all, pattern = 'f90_defor_10', replacement = '2010') %>%
  mutate_if(is.character, str_replace_all, pattern = 'f90_defor_11', replacement = '2011') %>%
  mutate_if(is.character, str_replace_all, pattern = 'f90_defor_12', replacement = '2012') %>%
  mutate_if(is.character, str_replace_all, pattern = 'f90_defor_13', replacement = '2013') %>%
  mutate_if(is.character, str_replace_all, pattern = 'f90_defor_14', replacement = '2014') %>%
  mutate_if(is.character, str_replace_all, pattern = 'f90_defor_15', replacement = '2015') %>%
  mutate_if(is.character, str_replace_all, pattern = 'f90_defor_16', replacement = '2016') 

forGraphs.indo.raw <- forGraphs.indo.raw  %>% 
  mutate_if(is.character, str_replace_all, pattern = 'f90_defor_01', replacement = '2001') %>%
  mutate_if(is.character, str_replace_all, pattern = 'f90_defor_02', replacement = '2002') %>%
  mutate_if(is.character, str_replace_all, pattern = 'f90_defor_03', replacement = '2003') %>%
  mutate_if(is.character, str_replace_all, pattern = 'f90_defor_04', replacement = '2004') %>%
  mutate_if(is.character, str_replace_all, pattern = 'f90_defor_05', replacement = '2005') %>%
  mutate_if(is.character, str_replace_all, pattern = 'f90_defor_06', replacement = '2006') %>%
  mutate_if(is.character, str_replace_all, pattern = 'f90_defor_07', replacement = '2007') %>%
  mutate_if(is.character, str_replace_all, pattern = 'f90_defor_08', replacement = '2008') %>%
  mutate_if(is.character, str_replace_all, pattern = 'f90_defor_09', replacement = '2009') %>%
  mutate_if(is.character, str_replace_all, pattern = 'f90_defor_10', replacement = '2010') %>%
  mutate_if(is.character, str_replace_all, pattern = 'f90_defor_11', replacement = '2011') %>%
  mutate_if(is.character, str_replace_all, pattern = 'f90_defor_12', replacement = '2012') %>%
  mutate_if(is.character, str_replace_all, pattern = 'f90_defor_13', replacement = '2013') %>%
  mutate_if(is.character, str_replace_all, pattern = 'f90_defor_14', replacement = '2014') %>%
  mutate_if(is.character, str_replace_all, pattern = 'f90_defor_15', replacement = '2015') %>%
  mutate_if(is.character, str_replace_all, pattern = 'f90_defor_16', replacement = '2016') 

str(forGraphs.indo)

# Change the defor_year column to date format
forGraphs.indo$defor_year<-as.POSIXct(forGraphs.indo$defor_year,format="%Y")
forGraphs.indo.raw$defor_year<-as.POSIXct(forGraphs.indo.raw$defor_year,format="%Y")


# Format dates to 01-01
forGraphs.indo$defor_year<-ymd(format(forGraphs.indo$defor_year, "%Y-01-01"))
forGraphs.indo.raw$defor_year<-ymd(format(forGraphs.indo.raw$defor_year, "%Y-01-01"))


# Change the rspo_npp column data type to factor
forGraphs.indo$rspo_npp<- as.factor(forGraphs.indo$rspo_npp)
forGraphs.indo.raw$rspo_npp<- as.factor(forGraphs.indo.raw$rspo_npp)

View(forGraphs.indo)

########## RAW DATA#################
# Plot the both annual defor trends for NPP and the concessions all 01 matched pairs to see parallel trends
plot.defor.indo.raw <- ggplot(data = forGraphs.indo.raw, aes(x=defor_year, y=defor_rate)) + 
  stat_summary(fun.data=mean_cl_normal, geom="ribbon", alpha = 0.3, aes(fill=forGraphs.indo.raw$rspo_npp, group=forGraphs.indo.raw$rspo_npp)) + stat_summary(geom = "line", fun.y = mean, aes(group=forGraphs.indo.raw$rspo_npp, color=forGraphs.indo.raw$rspo_npp))+ xlab("Date") + ylab("Mean Deforestation Rate") +theme_bw()+ggtitle("Pre-Matching Parallel Trends of NPPs VS Non-RSPO Plantations")+theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) +scale_color_manual(values = c("darkorchid3", "goldenrod"), name="Legend")+ scale_fill_manual(values=c("darkorchid3", "goldenrod"), name="Legend")+scale_x_date(date_breaks = "1 year", date_labels =  "%Y") +geom_vline(xintercept=as.numeric(forGraphs.indo$defor_year[8000]), linetype=4)+theme(legend.title=element_blank()) +theme(panel.grid.minor.x = element_blank())
  

######## AFTER MATCHING ##########
# Plot the both annual defor trends for NPP and the concessions all 01 matched pairs to see parallel trends
plot.defor.indo.matched <-ggplot(data = forGraphs.indo, aes(x=defor_year, y=defor_rate)) + 
  stat_summary(fun.data=mean_cl_normal, geom="ribbon", alpha = 0.3, aes(fill=forGraphs.indo$rspo_npp, group=forGraphs.indo$rspo_npp)) + stat_summary(geom = "line", fun.y = mean, aes(group=forGraphs.indo$rspo_npp, color=forGraphs.indo$rspo_npp))+ xlab("Date") + ylab("Mean Deforestation Rate") +theme_bw()+ggtitle("Post-Matching Parallel Trends of NPPs VS Non-RSPO Plantations") +theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) + scale_color_manual(values = c("darkorchid3", "goldenrod"), name="Legend")+ scale_fill_manual(values=c("darkorchid3", "goldenrod"), name="Legend")+scale_x_date(date_breaks = "1 year", date_labels =  "%Y") +geom_vline(xintercept=as.numeric(forGraphs.indo$defor_year[7922]), linetype=4)+theme(legend.title=element_blank()) +theme(panel.grid.minor.x = element_blank())


# Arrange the parallel trends plots on a panel
indo.parTrend.plots <- ggarrange(plot.defor.indo.raw, plot.defor.indo.matched ,  ncol =1, nrow=2)

ggsave(filename="parallel_trends_plots", plot = indo.parTrend.plots, height = 12, width = 5)

#+ scale_y_continuous(breaks=seq(0, 0.5, 0.25),limits = c(0,0.5))




