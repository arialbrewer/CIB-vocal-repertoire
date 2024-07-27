#Noise spectra analysis for masking on call types
#PSD

library(tidyverse)
library(scales)
library(ggtext)
library(lemon)
library(grid)


#read in ship data (this is 50th percentile from AET)
ship_distant <- read_csv("NoiseData_distant_ship_PSD+CR.csv") %>% 
  mutate(ship_type="distant ship")

ship_close <- read_csv("NoiseData_close_ship_PSD+CR.csv") %>% 
  mutate(ship_type="close ship")

#combine ship data
ship_data <- bind_rows(ship_distant, ship_close) %>% 
  rename(ship_db = db)

#read in beluga call data
dws <- read_csv("NoiseData_dws_PSD.csv") %>% 
  mutate(call_type="dws")

flatws <- read_csv("NoiseData_flatws_PSD.csv") %>% 
  mutate(call_type="flatws")

modws <- read_csv("NoiseData_modws_PSD.csv") %>% 
  mutate(call_type="modws")

pulsed <- read_csv("NoiseData_pulsed_PSD.csv") %>% 
  mutate(call_type="pulse.d")

pulseflat <- read_csv("NoiseData_pulseflat_PSD.csv") %>% 
  mutate(call_type="pulse.flat")

pulseflatseg <- read_csv("NoiseData_pulseflatseg_PSD.csv") %>% 
  mutate(call_type="pulse.flat.seg")

c5 <- read_csv("NoiseData_c5_PSD.csv") %>% 
  mutate(call_type="c.5")

#combine call data with ship data
call_data <- bind_rows(dws,flatws,modws,pulsed,pulseflat,pulseflatseg,c5) %>% 
  left_join(ship_data, by = c('freq')) %>% 
  mutate(category=case_when(
    call_type %in% c("dws","flatws","modws") ~ "Whistle",
    call_type %in% c("pulse.d","pulse.flat","pulse.flat.seg") ~ "Pulsed call",
    call_type %in% c("c.5") ~ "Combined call",
    TRUE ~ "other"
  )) %>% 
  mutate(category = fct_relevel(category,
                                c("Whistle","Pulsed call","Combined call")))


audiogram <- read_csv("beluga_audiogram.csv")

######################################################################


#facet wrap plot
ggplot(data=call_data)+
  geom_line(aes(x=freq, y=db), color="cadetblue")+
  facet_rep_wrap(category~call_type, 
                 labeller=labeller(.multi_line = FALSE))+
  geom_line(aes(x=freq, y=ship_db, color=ship_type))+
  geom_line(data=audiogram, aes(x=freq,y=db),linetype=2,linewidth=1,color="gray")+
  theme_classic()+
  scale_x_log10(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  scale_color_manual(values=c("firebrick","goldenrod","gray"))+
  labs(x="Frequency (Hz)",
       y= bquote("Power spectral density (dB re 1µPa^2/Hz)"))+
       #y= expression("Power spectral density (dB re 1µPa^{'2'}/Hz)"))+
  theme(legend.title = element_blank(),
        legend.position = c(0.5,0.18),
        legend.text = element_text(size=10),
        legend.key.width = unit(1,"cm"),
        axis.title = element_text(size=12),
        panel.spacing = unit(0.01,'cm'),
        strip.background = element_rect(color="gray20",fill="cadetblue3"))+
  annotation_logticks(sides="b")


ggsave('figs\\PSD chart.jpg', dpi=600, width=9, height=7)



###########test with 99th percentile PSD instead of 50th
ship_distant99 <- read_csv("NoiseData_distant_ship_PSD-99.csv") %>% 
  mutate(ship_type="distant ship")

ship_close99 <- read_csv("NoiseData_close_ship_PSD-99.csv") %>% 
  mutate(ship_type="close ship")

#combine ship data
ship_data99 <- bind_rows(ship_distant99, ship_close99) %>% 
  rename(ship_db = db)

audiogram <- read_csv("audiogram.csv")


ggplot(data=ship_data99)+
  geom_line(aes(x=freq, y=ship_db, color=ship_type))+
  geom_line(data=audiogram, aes(x=freq,y=db))+
  theme_classic()+
  scale_x_log10(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  scale_color_manual(values=c("firebrick","goldenrod","gray"))+
  labs(x="Frequency (Hz)",
       y= bquote("Power spectral density (dB re 1µPa^2/Hz)"))+
  annotation_logticks(sides="b")
