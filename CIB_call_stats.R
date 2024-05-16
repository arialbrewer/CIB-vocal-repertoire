#Arial Brewer
#PhD- Chapter 1
#CIB vocal repertoire- summary stats

#load libraries
library(tidyverse)

#Import data
bsfiles <- c("C:/Users/Arial/OneDrive - UW/Desktop/CH.1 Repertoire and masking/CIB repertoire code/218.enc.11.csv",
             "C:/Users/Arial/OneDrive - UW/Desktop/CH.1 Repertoire and masking/CIB repertoire code/218.enc.93.csv",
             "C:/Users/Arial/OneDrive - UW/Desktop/CH.1 Repertoire and masking/CIB repertoire code/218.enc.238.csv",
             "C:/Users/Arial/OneDrive - UW/Desktop/CH.1 Repertoire and masking/CIB repertoire code/218.enc.298.csv",
             "C:/Users/Arial/OneDrive - UW/Desktop/CH.1 Repertoire and masking/CIB repertoire code/218.enc.313.csv")

tbfiles <- c("C:/Users/Arial/OneDrive - UW/Desktop/CH.1 Repertoire and masking/CIB repertoire code/229.enc.15.csv",
             "C:/Users/Arial/OneDrive - UW/Desktop/CH.1 Repertoire and masking/CIB repertoire code/229.enc.21.csv",
             "C:/Users/Arial/OneDrive - UW/Desktop/CH.1 Repertoire and masking/CIB repertoire code/229.enc.61.csv",
             "C:/Users/Arial/OneDrive - UW/Desktop/CH.1 Repertoire and masking/CIB repertoire code/229.enc.69.csv",
             "C:/Users/Arial/OneDrive - UW/Desktop/CH.1 Repertoire and masking/CIB repertoire code/229.enc.116.csv",
             "C:/Users/Arial/OneDrive - UW/Desktop/CH.1 Repertoire and masking/CIB repertoire code/229.enc.120.csv",
             "C:/Users/Arial/OneDrive - UW/Desktop/CH.1 Repertoire and masking/CIB repertoire code/229.enc.130.csv",
             "C:/Users/Arial/OneDrive - UW/Desktop/CH.1 Repertoire and masking/CIB repertoire code/229.enc.131.csv",
             "C:/Users/Arial/OneDrive - UW/Desktop/CH.1 Repertoire and masking/CIB repertoire code/229.enc.190.csv",
             "C:/Users/Arial/OneDrive - UW/Desktop/CH.1 Repertoire and masking/CIB repertoire code/229.enc.205.csv")

#convert files into tibble and combine csv files for each location
bigsu <- bsfiles %>% 
  map(read_csv, id="file_name") %>% 
  reduce(rbind) %>% 
  mutate(file_name=basename(file_name),location="bigsu")

tradbay <- tbfiles %>% 
  map(read_csv, id="file_name") %>% 
  reduce(rbind) %>% 
  mutate(file_name=basename(file_name),location="tradbay") 

ci_total <- bind_rows(bigsu,tradbay) 


#Tidy dataframe and remove columns not needed and add duration and bandwidth
call.info <- ci_total %>% 
  select(-Begin_File, -location, -Selection) %>% 
  mutate(duration= End_Time-Begin_Time,
         bw= High_Freq-Low_Freq)

#Get summary stats for each call type
call.stats <- call.info %>% 
  group_by(Call_type) %>%
  summarise(mean_dur= mean(duration),
            sd_dur= sd(duration),
            mean_LF= mean(Low_Freq), 
            sd_LF= sd(Low_Freq), 
            mean_HF= mean(High_Freq), 
            sd_HF= sd(High_Freq), 
            mean_bw= mean(bw),
            sd_bw= sd(bw),
            mean_center= mean(Center_Freq),
            sd_center= sd(Center_Freq),
            mean_peak= mean(Peak_Freq),
            sd_peak= sd(Peak_Freq))

#make DF and adjust decimal points
call.stats <- call.stats %>% 
    as.data.frame(.) %>%
    dplyr::mutate_if(is.numeric,round,1)


###DISCRETE/STEREOTYPED CALLS ONLY
call.info_discrete <- read_csv("call.info_discrete.csv")

#Get summary stats for each call type
call.stats_dis <- call.info_discrete %>% 
  group_by(Call_type) %>%
  summarise(mean_dur= mean(duration),
            sd_dur= sd(duration),
            mean_LF= mean(Low_Freq), 
            sd_LF= sd(Low_Freq), 
            mean_HF= mean(High_Freq), 
            sd_HF= sd(High_Freq), 
            mean_bw= mean(bw),
            sd_bw= sd(bw),
            mean_center= mean(Center_Freq),
            sd_center= sd(Center_Freq),
            mean_peak= mean(Peak_Freq),
            sd_peak= sd(Peak_Freq))

#make DF and adjust decimal points
call.stats_dis <- call.stats_dis %>% 
  as.data.frame(.) %>%
  dplyr::mutate_if(is.numeric,round,1)

#save csv
write_csv(call.stats_dis,"C:/Users/Arial/OneDrive - UW/Desktop/CH.1 call classification/CIB repertoire code/call.stats_discrete.csv")

  
###### PRR measurements for pc and cc ############
prr <- read_csv("prr_measurements.csv")
  
prr.stats <- prr %>% 
  group_by(call_type) %>%
  summarise(mean_min=mean(min),
            sd_min=sd(min),
            mean_max=mean(max),
            sd_max=sd(max))
  
#save csv
write_csv(prr.stats,"C:/Users/Arial/OneDrive - UW/Desktop/CH.1 call classification/CIB repertoire code/PRR.stats.csv")



#### Additional measurements per JASA for CART/random forest model 
#Added start freq, end freq, trend, #inflections, #segments, #steps, #units on a subset of all ws and pc
rf_data <- read_csv("RF_call_measurements.csv")

rf.call.stats <- rf_data %>% 
  group_by(call_type) %>%
  summarise(mean_start= mean(start_freq),
            sd_start= sd(start_freq),
            mean_end= mean(end_freq), 
            sd_end= sd(end_freq), 
            mean_trend= mean(trend), 
            sd_trend= sd(trend), 
            mean_inflec= mean(inflec),
            sd_inflec= sd(inflec),
            mean_seg= mean(seg),
            sd_seg= sd(seg),
            mean_steps= mean(steps),
            sd_steps= sd(steps),
            mean_units= mean(units),
            sd_units= sd(units)) %>% 
  dplyr::mutate_if(is.numeric,round,1)

