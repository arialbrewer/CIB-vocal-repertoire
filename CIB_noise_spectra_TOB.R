#Arial Brewer
#PhD- Chapter 1
#CIB vocal repertoire- noise spectra analysis for masking (1/3 octave bands)

library(tidyverse)
library(scales)
library(lemon)
library(grid)

#read in ship data 
ship_distant <- read_csv("NoiseData_distant_ship_TOB.csv") %>% 
  mutate(ship_type="Distant ship")

ship_close <- read_csv("NoiseData_close_ship_TOB.csv") %>% 
  mutate(ship_type="Close ship")

#combine ship data
ship_data <- bind_rows(ship_distant, ship_close) %>% 
  rename(ship_db = db)

#read in beluga audiogram
audiogram <- read_csv("beluga_audiogram.csv") 

#read in beluga call data
dws <- read_csv("NoiseData_dws_TOB.csv") %>% 
  mutate(call_type="dws")

flatws <- read_csv("NoiseData_flatws_TOB.csv") %>% 
  mutate(call_type="flatws")

modws <- read_csv("NoiseData_modws_TOB.csv") %>% 
  mutate(call_type="modws")

pulsed <- read_csv("NoiseData_pulsed_TOB.csv") %>% 
  mutate(call_type="pulse.d")

pulseflat <- read_csv("NoiseData_pulseflat_TOB.csv") %>% 
  mutate(call_type="pulse.flat")

pulseflatseg <- read_csv("NoiseData_pulseflatseg_TOB.csv") %>% 
  mutate(call_type="pulse.flat.seg")

c5 <- read_csv("NoiseData_c5_TOB.csv") %>% 
  mutate(call_type="CI.c.5")

#combine call data with ship data
call_data <- bind_rows(dws,flatws,modws,pulsed,pulseflat,pulseflatseg,c5) %>% 
  left_join(ship_data, by = c('freq')) %>% 
  mutate(category=case_when(
    call_type %in% c("dws",
                     "flatws",
                     "modws") ~ "Whistle",
    call_type %in% c("pulse.d",
                     "pulse.flat",
                     "pulse.flat.seg") ~ "Pulsed call",
    call_type %in% c("CI.c.5") ~ "Combined call",
    TRUE ~ "other"
  )) %>% 
  mutate(category = fct_relevel(category,
                                c("Whistle","Pulsed call","Combined call")))


#Multi-panel plot
ggplot(data=call_data)+
  geom_line(aes(x=freq, y=db), color="cadetblue", linewidth=1)+
  facet_rep_wrap(category~call_type,
                 labeller=function(labels){
                   labels <- lapply(labels,as.character)
                   a <- do.call(paste,c(labels,list(sep=",")))
                   list(gsub("\\,"," - ",a))
                 })+
  geom_line(aes(x=freq, y=ship_db, color=ship_type), linewidth=1)+
  geom_line(data=audiogram, aes(x=freq,y=db),linetype=2,linewidth=1,color="gray")+
  theme_classic()+
  scale_x_log10(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  scale_color_manual(values=c("firebrick","goldenrod1","gray"))+
  labs(x="Frequency (Hz)",
       y=("Sound pressure (dB re 1µPa)"))+
  theme(legend.position = "none",
        axis.title = element_text(size=12),
        panel.spacing = unit(0.01,'cm'),
        strip.background = element_rect(color="gray20",fill="cadetblue3"),
        plot.margin = margin(10,15,10,10))+
  annotation_logticks(sides="b")


ggsave('figs\\TOB plots w audiogram.tiff', dpi=600, width=10, height=7)


