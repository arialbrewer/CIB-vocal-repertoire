#Noise spectra analysis for masking on call types
#1/12 octave bands

library(tidyverse)
library(scales)
library(ggtext)


#read in ship data sets
ship_distant <- read_csv("NoiseData_distant_ship.csv") %>% 
  mutate(ship_type="distant ship")

ship_close <- read_csv("NoiseData_close_ship.csv") %>% 
  mutate(ship_type="close ship")

#combine ship data
ship_data <- bind_rows(ship_distant, ship_close) %>% 
  rename(ship_db = db)

#read in beluga call data
dws <- read_csv("NoiseData_dws.csv") %>% 
  mutate(call_type="dws")

flatws <- read_csv("NoiseData_flatws.csv") %>% 
  mutate(call_type="flatws")

modws <- read_csv("NoiseData_modws.csv") %>% 
  mutate(call_type="modws")

pulsed <- read_csv("NoiseData_pulsed.csv") %>% 
  mutate(call_type="pulsed")

pulseflat <- read_csv("NoiseData_pulseflat.csv") %>% 
  mutate(call_type="pulseflat")

pulseflatseg <- read_csv("NoiseData_pulseflatseg.csv") %>% 
  mutate(call_type="pulseflatseg")

c5 <- read_csv("NoiseData_c5.csv") %>% 
  mutate(call_type="c5")

#combine call data with ship data
call_data <- bind_rows(dws,flatws,modws,pulsed,pulseflat,pulseflatseg,c5) %>% 
  left_join(ship_data, by = c('low.freq','freq','high.freq')) %>% 
  mutate(category=case_when(
    call_type %in% c("dws","flatws","modws") ~ "whistle",
    call_type %in% c("pulsed","pulseflat","pulseflatseg") ~ "pulsed call",
    call_type %in% c("c5") ~ "combined call",
    TRUE ~ "other"
  )) %>% 
  mutate(category = fct_relevel(category,
                                c("whistle","pulsed call","combined call")))



########## plot with 1/12 octave bin widths ################################

ggplot(data=data_dws, aes(x=low.freq, xend=high.freq, y=db, yend=db,color=dataset))+
  geom_step(size=0.7)+
  theme_classic()+
  theme(legend.title = element_blank())+
  scale_x_log10(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  scale_color_manual(values=c("firebrick","goldenrod","cadetblue"))+
  labs(x="Frequency (Hz)", y="Sound pressure (dB re 1µPa)")+
  annotation_logticks(sides="b")

ggplot(data=data_flatws, aes(x=low.freq, xend=high.freq, y=db, yend=db,color=dataset))+
  geom_step(size=0.7)+
  theme_classic()+
  theme(legend.title = element_blank())+
  scale_x_log10(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  scale_color_manual(values=c("firebrick","goldenrod","cadetblue"))+
  labs(x="Frequency (Hz)", y="Sound pressure (dB re 1µPa)")+
  annotation_logticks(sides="b")

ggplot(data=data_modws, aes(x=low.freq, xend=high.freq, y=db, yend=db,color=dataset))+
  geom_step(size=0.7)+
  theme_classic()+
  theme(legend.title = element_blank())+
  scale_x_log10(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  scale_color_manual(values=c("firebrick","goldenrod","cadetblue"))+
  labs(x="Frequency (Hz)", y="Sound pressure (dB re 1µPa)")+
  annotation_logticks(sides="b")

ggplot(data=data_pulseflat, aes(x=low.freq, xend=high.freq, y=db, yend=db,color=dataset))+
  geom_step(size=0.7)+
  theme_classic()+
  theme(legend.title = element_blank())+
  scale_x_log10(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  scale_color_manual(values=c("firebrick","goldenrod","cadetblue"))+
  labs(x="Frequency (Hz)", y="Sound pressure (dB re 1µPa)")+
  annotation_logticks(sides="b")

ggplot(data=data_pulseflatseg, aes(x=low.freq, xend=high.freq, y=db, yend=db,color=dataset))+
  geom_step(size=0.7)+
  theme_classic()+
  theme(legend.title = element_blank())+
  scale_x_log10(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  scale_color_manual(values=c("firebrick","goldenrod","cadetblue"))+
  labs(x="Frequency (Hz)", y="Sound pressure (dB re 1µPa)")+
  annotation_logticks(sides="b")

ggplot(data=data_pulsed, aes(x=low.freq, xend=high.freq, y=db, yend=db,color=dataset))+
  geom_step(size=0.7)+
  theme_classic()+
  theme(legend.title = element_blank())+
  scale_x_log10(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  scale_color_manual(values=c("firebrick","goldenrod","cadetblue"))+
  labs(x="Frequency (Hz)", y="Sound pressure (dB re 1µPa)")+
  annotation_logticks(sides="b")

ggplot(data=data_c5, aes(x=low.freq, xend=high.freq, y=db, yend=db,color=dataset))+
  geom_step(size=0.7)+
  theme_classic()+
  theme(legend.title = element_blank())+
  scale_x_log10(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  scale_color_manual(values=c("cadetblue","firebrick","goldenrod"))+
  labs(x="Frequency (Hz)", y="Sound pressure (dB re 1µPa)")+
  annotation_logticks(sides="b")

########################################################


#facet wrap plot
ggplot(data=call_data)+
  geom_line(aes(x=freq, y=db), color="cadetblue")+
  facet_wrap(category~call_type)+
  geom_line(aes(x=freq, y=ship_db, color=ship_type))+
  theme_classic()+
  theme(legend.title = element_blank())+
  scale_x_log10(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  scale_color_manual(values=c("firebrick","goldenrod"))+
  labs(x="Frequency (Hz)", y="Sound pressure (dB re 1µPa)")+
  annotation_logticks(sides="b")




  
  