### RQ#1 T-tests between HCV and non-HCV

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
library(egg)
library(readr)
library(car)
library(lubridate)
library(Hmisc)


## Import the CSV datasets
hcv_nonhcv <- read.csv("/Users/Char/Dropbox/char_HCV_table_data_fall2019/hcv_nonHCV_cleaned.csv")

View(hcv_nonhcv)

colnames(hcv_nonhcv)

# Fix HCV ID Column Numbering

hcv_nonhcv <- hcv_nonhcv %>%
  mutate(HCV_ID = replace(area_type, area_type== "HCV, HCVMA" | area_type=="HCv" | area_type=="HCV/HCVMA" | area_type=="HCV, HCS", "HCV")) %>%
  filter( HCV_ID=="HCV" | HCV_ID=="Concession") %>%
  mutate(HCV_ID = factor(HCV_ID, levels = c("HCV", "Concession")))


########################### t TEST INDONESIA ############################################

### NEED TO FIX HCV ID COLUMN! Values are incorrect
# select only the variables for t-test
full_hcv_forTtest <- hcv_nonhcv%>%
  select(unique_ID, area_type, island, province, area_km2, slope, elev, opl_dist, roads_dist, city_dist, f90_strt_perc, HCV_ID) 
View(full_hcv_forTtest) 

summary(full_hcv_forTtest$HCV_ID)


## Run t-test table 
indo.hcv.nonhcv <- full_hcv_forTtest %>%
  select(- unique_ID, -island, -province, -area_type) %>%
  pivot_longer(cols = -HCV_ID, names_to = "variable", values_to = "value") %>%
  group_nest(variable) %>%
  mutate(t.test = map(data, ~tidy(t.test(value ~ HCV_ID, data = .x)))) %>%
  unnest(t.test) %>%
  select(variable, estimate, estimate1, estimate2, statistic, p.value, parameter, conf.low, conf.high,method, alternative)

View(indo.hcv.nonhcv)

# Export indonesia t-test table to CSV
#write_csv(indo.hcv.nonhcv, "/Users/Char/Dropbox/char_HCV_table_data_fall2019/RQ#1/indo_hcv_nonhcv_ttest.csv")

## Create a density plots for t-test inputs
indo.slope <- ggplot(full_hcv_forTtest, aes(slope, fill = HCV_ID)) + geom_density(alpha = 0.2)+theme_bw()
indo.elev<- ggplot(full_hcv_forTtest, aes(elev, fill = HCV_ID)) + geom_density(alpha = 0.2)+theme_bw()
indo.opl_dist <- ggplot(full_hcv_forTtest, aes(opl_dist, fill = HCV_ID)) + geom_density(alpha = 0.2)+theme_bw()
indo.city_dist <- ggplot(full_hcv_forTtest, aes(city_dist, fill = HCV_ID)) + geom_density(alpha = 0.2)+theme_bw()
indo.roads_dist <- ggplot(full_hcv_forTtest, aes(roads_dist, fill = HCV_ID)) + geom_density(alpha = 0.2)+theme_bw()
indo.f90_strt_perc <- ggplot(full_hcv_forTtest, aes(f90_strt_perc, fill = HCV_ID)) + geom_density(alpha = 0.2)+theme_bw()
indo.area_km2 <- ggplot(full_hcv_forTtest, aes(area_km2, fill = HCV_ID)) + geom_density(alpha = 0.2)+theme_bw()


################ Arrange indonesia density plots for export ####################
indo.density.plots <- ggarrange(indo.slope, indo.elev, indo.opl_dist, indo.city_dist, indo.roads_dist, indo.f90_strt_perc, indo.area_km2,  ncol =3, nrow=3)

### Export the plots ###
#ggsave(filename="/Users/Char/Dropbox/char_HCV_table_data_fall2019/RQ#1/indo_density_plots.jpg", plot = indo.density.plots, height = 12, width = 16)

########################### t TEST Kalimantan ############################################
# select only the variables for t-test
kali <- full_hcv_forTtest%>%
  filter(island=="Kalimantan")
View(kali) 

## Run t-test table 
kali.ttest <- kali %>%
  select(- unique_ID, -island, -province, -area_type) %>%
  pivot_longer(cols = -HCV_ID, names_to = "variable", values_to = "value") %>%
  group_nest(variable) %>%
  mutate(t.test = map(data, ~tidy(t.test(value ~ HCV_ID, data = .x)))) %>%
  unnest(t.test) %>%
  select(variable, estimate, estimate1, estimate2, statistic, p.value, parameter, conf.low, conf.high,method, alternative)

View(kali.ttest)

# Export kali t-test table to CSV
#write_csv(kali.ttest, "/Users/Char/Dropbox/char_HCV_table_data_fall2019/RQ#1/kali_hcv_nonhcv_ttest.csv")

## Create a density plots for kali t-test inputs
kali.slope <- ggplot(kali, aes(slope, fill = HCV_ID)) + geom_density(alpha = 0.2)+theme_bw()
kali.elev<- ggplot(kali, aes(elev, fill = HCV_ID)) + geom_density(alpha = 0.2)+theme_bw()
kali.opl_dist <- ggplot(kali, aes(opl_dist, fill = HCV_ID)) + geom_density(alpha = 0.2)+theme_bw()
kali.city_dist <- ggplot(kali, aes(city_dist, fill = HCV_ID)) + geom_density(alpha = 0.2)+theme_bw()
kali.roads_dist <- ggplot(kali, aes(roads_dist, fill = HCV_ID)) + geom_density(alpha = 0.2)+theme_bw()
kali.f90_strt_perc <- ggplot(kali, aes(f90_strt_perc, fill = HCV_ID)) + geom_density(alpha = 0.2)+theme_bw()
kali.area_km2 <- ggplot(kali, aes(area_km2, fill = HCV_ID)) + geom_density(alpha = 0.2)+theme_bw()

################ Arrange Kalimantan density plots for export ####################
kali.density.plots <- ggarrange(kali.slope, kali.elev, kali.opl_dist, kali.city_dist, kali.roads_dist, kali.f90_strt_perc, kali.area_km2,  ncol =3, nrow=3)

### Export the plots ###
#ggsave(filename="/Users/Char/Dropbox/char_HCV_table_data_fall2019/RQ#1/kali_density_plots.jpg", plot = kali.density.plots, height = 12, width = 16)

########################### t TEST Sumatra ############################################
# select only the variables for t-test
sumatra <- full_hcv_forTtest%>%
  filter(island=="Sumatra")
View(sumatra) 

## Run t-test table 
sumatra.ttest <- sumatra %>%
  select(- unique_ID, -island, -province, -area_type) %>%
  pivot_longer(cols = -HCV_ID, names_to = "variable", values_to = "value") %>%
  group_nest(variable) %>%
  mutate(t.test = map(data, ~tidy(t.test(value ~ HCV_ID, data = .x)))) %>%
  unnest(t.test) %>%
  select(variable, estimate, estimate1, estimate2, statistic, p.value, parameter, conf.low, conf.high,method, alternative)

View(sumatra.ttest)

# Export sumatra t-test table to CSV
#write_csv(sumatra.ttest, "/Users/Char/Dropbox/char_HCV_table_data_fall2019/RQ#1/sumatra_hcv_nonhcv_ttest.csv")

## Create a density plots for sumatra t-test inputs
sumatra.slope <- ggplot(sumatra, aes(slope, fill = HCV_ID)) + geom_density(alpha = 0.2)+theme_bw()
sumatra.elev<- ggplot(sumatra, aes(elev, fill = HCV_ID)) + geom_density(alpha = 0.2)+theme_bw()
sumatra.opl_dist <- ggplot(sumatra, aes(opl_dist, fill = HCV_ID)) + geom_density(alpha = 0.2)+theme_bw()
sumatra.city_dist <- ggplot(sumatra, aes(city_dist, fill = HCV_ID)) + geom_density(alpha = 0.2)+theme_bw()
sumatra.roads_dist <- ggplot(sumatra, aes(roads_dist, fill = HCV_ID)) + geom_density(alpha = 0.2)+theme_bw()
sumatra.f90_strt_perc <- ggplot(sumatra, aes(f90_strt_perc, fill = HCV_ID)) + geom_density(alpha = 0.2)+theme_bw()
sumatra.area_km2 <- ggplot(sumatra, aes(area_km2, fill = HCV_ID)) + geom_density(alpha = 0.2)+theme_bw()

################ Arrange Sumatra density plots for export ####################
sumatra.density.plots <- ggarrange(sumatra.slope, sumatra.elev, sumatra.opl_dist, sumatra.city_dist, sumatra.roads_dist, sumatra.f90_strt_perc, sumatra.area_km2,  ncol =3, nrow=3)

### Export the plots ###
#ggsave(filename="/Users/Char/Dropbox/char_HCV_table_data_fall2019/RQ#1/sumatra_density_plots.jpg", plot = sumatra.density.plots, height = 12, width = 16)


########################### t TEST PAPUA ############################################
# select only the variables for t-test
papua <- full_hcv_forTtest %>%
  filter(island=="Papua")
View(papua) 

## Run t-test table 
papua.ttest <- papua %>%
  select(- unique_ID, -island, -province, -area_type) %>%
  pivot_longer(cols = -HCV_ID, names_to = "variable", values_to = "value") %>%
  group_nest(variable) %>%
  mutate(t.test = map(data, ~tidy(t.test(value ~ HCV_ID, data = .x)))) %>%
  unnest(t.test) %>%
  select(variable, estimate, estimate1, estimate2, statistic, p.value, parameter, conf.low, conf.high,method, alternative)

View(papua.ttest)

# Export papua t-test table to CSV
#write_csv(papua.ttest, "/Users/Char/Dropbox/char_HCV_table_data_fall2019/RQ#1/papua_hcv_nonhcv_ttest.csv")

## Create a density plots for papua t-test inputs
papua.slope <- ggplot(papua, aes(slope, fill = HCV_ID)) + geom_density(alpha = 0.2)+theme_bw()
papua.elev<- ggplot(papua, aes(elev, fill = HCV_ID)) + geom_density(alpha = 0.2)+theme_bw()
papua.opl_dist <- ggplot(papua, aes(opl_dist, fill = HCV_ID)) + geom_density(alpha = 0.2)+theme_bw()
papua.city_dist <- ggplot(papua, aes(city_dist, fill = HCV_ID)) + geom_density(alpha = 0.2)+theme_bw()
papua.roads_dist <- ggplot(papua, aes(roads_dist, fill = HCV_ID)) + geom_density(alpha = 0.2)+theme_bw()
papua.f90_strt_perc <- ggplot(papua, aes(f90_strt_perc, fill = HCV_ID)) + geom_density(alpha = 0.2)+theme_bw()
papua.area_km2 <- ggplot(papua, aes(area_km2, fill = HCV_ID)) + geom_density(alpha = 0.2)+theme_bw()

################ Arrange Papua density plots for export ####################
papua.density.plots <- ggarrange(papua.slope, papua.elev, papua.opl_dist, papua.city_dist, papua.roads_dist, papua.f90_strt_perc, papua.area_km2,  ncol =3, nrow=3)

### Export the plots ###
#ggsave(filename="/Users/Char/Dropbox/char_HCV_table_data_fall2019/RQ#1/papua_density_plots.jpg", plot = papua.density.plots, height = 12, width = 16)

################## Plotting Deforestation Rates ###########################

indo.plot.data<- hcv_nonhcv %>% 
  group_by(HCV_ID) %>%
  select(unique_ID, HCV_ID, island, slope, elev, city_dist, opl_dist, roads_dist, f90_strt_perc, area_km2, f90_defor_01, f90_defor_02, f90_defor_03, f90_defor_04, f90_defor_05, f90_defor_06, f90_defor_07, f90_defor_08, f90_defor_09, f90_defor_10, f90_defor_11, f90_defor_12, f90_defor_13, f90_defor_15, f90_defor_16) %>%
  gather(key = defor_year, value = defor_rate, -c(unique_ID, HCV_ID, elev, slope, opl_dist, city_dist, roads_dist, area_km2, f90_strt_perc, island)) 
View(indo.plot.data)

# Find and replace the old column names with the year of the data observation
indo.plot.data <- indo.plot.data  %>% 
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

# Change the defor_year column to date format
indo.plot.data$defor_year<-as.POSIXct(indo.plot.data$defor_year,format="%Y")

# Format dates to 01-01
indo.plot.data$defor_year<-ymd(format(indo.plot.data$defor_year, "%Y-01-01"))

View(indo.plot.data)

# Export deforestation rate table to csv
#write_csv(indo.plot.data, "/Users/Char/Dropbox/char_HCV_table_data_fall2019/RQ#1/hcv_nonhcv_indo_plot_data.csv")

#### PLot deforestation trends In Indonesia ####
indo.plot <-ggplot(data = indo.plot.data, aes(x=defor_year, y=defor_rate)) + 
  stat_summary(fun.data=mean_cl_normal, geom="ribbon", alpha = 0.3, aes(fill=indo.plot.data$HCV_ID, group=indo.plot.data$HCV_ID)) + stat_summary(geom = "line", fun.y = mean, aes(group=indo.plot.data$HCV_ID, color=indo.plot.data$HCV_ID))+ xlab("Date") + ylab("Mean Deforestation Rate") +theme_bw()+ggtitle("Indonesia Mean Annual Deforestation Rate 2001-2016") +theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) + scale_color_manual(values = c("springgreen3", "orchid3"), name="Legend")+ scale_fill_manual(values=c("springgreen3", "orchid3"), name="Legend")+scale_x_date(date_breaks = "1 year", date_labels =  "%Y") +geom_vline(xintercept=as.numeric(indo.plot.data$defor_year[19459]), linetype=4)+theme(legend.title=element_blank()) +theme(panel.grid.minor.x = element_blank())

#+ scale_y_continuous(breaks=seq(0, 20, 5),limits = c(0,20))

############ Plot deforestation trends in Kalimantan #############

# Subset the indo.plot.data to only include kalimantan observations
kali.plot.data <- indo.plot.data %>%
  filter(island =="Kalimantan")

View(kali.plot.data)

# Create the plot
kali.plot <- ggplot(data = kali.plot.data, aes(x=defor_year, y=defor_rate)) + 
  stat_summary(fun.data=mean_cl_normal, geom="ribbon", alpha = 0.3, aes(fill=kali.plot.data$HCV_ID, group=kali.plot.data$HCV_ID)) + stat_summary(geom = "line", fun.y = mean, aes(group=kali.plot.data$HCV_ID, color=kali.plot.data$HCV_ID))+ xlab("Date") + ylab("Mean Deforestation Rate") +theme_bw()+ggtitle("Kalimantan Mean Annual Deforestation Rate 2001-2016") +theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) + scale_color_manual(values = c("springgreen3", "orchid3"), name="Legend")+ scale_fill_manual(values=c("springgreen3", "orchid3"), name="Legend")+scale_x_date(date_breaks = "1 year", date_labels =  "%Y") +geom_vline(xintercept=as.numeric(kali.plot.data$defor_year[16459]), linetype=4)+theme(legend.title=element_blank()) +theme(panel.grid.minor.x = element_blank())

############ Plot deforestation trends in Sumatra #############

# Subset the indo.plot.data to only include sumatra observations
sumatra.plot.data <- indo.plot.data %>%
  filter(island =="Sumatra")

View(sumatra.plot.data)

# Create the plot
sumatra.plot <- ggplot(data = sumatra.plot.data, aes(x=defor_year, y=defor_rate)) + 
  stat_summary(fun.data=mean_cl_normal, geom="ribbon", alpha = 0.3, aes(fill=sumatra.plot.data$HCV_ID, group=sumatra.plot.data$HCV_ID)) + stat_summary(geom = "line", fun.y = mean, aes(group=sumatra.plot.data$HCV_ID, color=sumatra.plot.data$HCV_ID))+ xlab("Date") + ylab("Mean Deforestation Rate") +theme_bw()+ggtitle("Sumatra Mean Annual Deforestation Rate 2001-2016") +theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) + scale_color_manual(values = c("springgreen3", "orchid3"), name="Legend")+ scale_fill_manual(values=c("springgreen3", "orchid3"), name="Legend")+scale_x_date(date_breaks = "1 year", date_labels =  "%Y") +geom_vline(xintercept=as.numeric(sumatra.plot.data$defor_year[2294]), linetype=4)+theme(legend.title=element_blank()) +theme(panel.grid.minor.x = element_blank())

############ Plot deforestation trends in Papua #############

# Subset the indo.plot.data to only include sumatra observations
papua.plot.data <- indo.plot.data %>%
  filter(island =="Papua")

View(papua.plot.data)

# Create the plot
papua.plot <- ggplot(data = papua.plot.data, aes(x=defor_year, y=defor_rate)) + 
  stat_summary(fun.data=mean_cl_normal, geom="ribbon", alpha = 0.3, aes(fill=papua.plot.data$HCV_ID, group=papua.plot.data$HCV_ID)) + stat_summary(geom = "line", fun.y = mean, aes(group=papua.plot.data$HCV_ID, color=papua.plot.data$HCV_ID))+ xlab("Date") + ylab("Mean Deforestation Rate") +theme_bw()+ggtitle("Papua Mean Annual Deforestation Rate 2001-2016") +theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) + scale_color_manual(values = c("springgreen3", "orchid3"), name="Legend")+ scale_fill_manual(values=c("springgreen3", "orchid3"), name="Legend")+scale_x_date(date_breaks = "1 year", date_labels =  "%Y") +geom_vline(xintercept=as.numeric(papua.plot.data$defor_year[1672]), linetype=4)+theme(legend.title=element_blank()) +theme(panel.grid.minor.x = element_blank())


################ Arrange All Four Plots on One Panel for Export ####################
indo.plots <- ggarrange(indo.plot, kali.plot, sumatra.plot, papua.plot,  ncol =1, nrow=4)

### Export the plots ###
ggsave(filename="/Users/Char/Dropbox/char_HCV_table_data_fall2019/RQ#1/indo_hcv_npp_forest_trends.jpg", plot = indo.plots, height = 16, width = 12)
