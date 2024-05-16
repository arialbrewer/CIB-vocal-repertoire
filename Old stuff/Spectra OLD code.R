## My old way


#read in data sets
#ship_distant <- read_csv("NoiseData_distant_ship.csv") %>% mutate(dataset="distant ship")
#ship_close <- read_csv("NoiseData_close_ship.csv") %>% mutate(dataset="close ship")
#c5 <- read_csv("NoiseData_c5.csv") %>% mutate(dataset="c.5")
#dws <- read_csv("NoiseData_dws.csv") %>% mutate(dataset="dws")
#flatws <- read_csv("NoiseData_flatws.csv") %>% mutate(dataset="flatws")
#modws <- read_csv("NoiseData_modws.csv") %>% mutate(dataset="modws")
#pulsed <- read_csv("NoiseData_pulsed.csv") %>% mutate(dataset="pulse.d")
#pulseflat <- read_csv("NoiseData_pulseflat.csv") %>% mutate(dataset="pulse.flat")
#pulseflatseg <- read_csv("NoiseData_pulseflatseg.csv") %>% mutate(dataset="pulse.flat.seg")

#combine datasets with ship noise
#data_total <- bind_rows(ship_distant,ship_close,c5,dws,flatws,modws,pulsed,pulseflat,pulseflatseg)
#data_c5 <- bind_rows(ship_distant,ship_close,c5)
#data_dws <- bind_rows(ship_distant,ship_close,dws)
#data_flatws <- bind_rows(ship_distant,ship_close,flatws)
#data_modws <- bind_rows(ship_distant,ship_close,modws)
#data_pulsed<- bind_rows(ship_distant,ship_close,pulsed)
#data_pulseflat<- bind_rows(ship_distant,ship_close,pulseflat)
#data_pulseflatseg<- bind_rows(ship_distant,ship_close,pulseflatseg)



#line plot in log of each call individually
ggplot(data=data_c5, aes(x=freq, y=db,color=dataset))+
  geom_line(size=1)+
  theme_classic()+
  theme(legend.title = element_blank(),axis.title=element_text(size=10))+
  scale_x_log10(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  scale_color_manual(values=c("cadetblue","firebrick","goldenrod"))+
  labs(x="Frequency (Hz)", y="Sound pressure (dB re 1µPa)")+
  annotation_logticks(sides="b")

ggsave('figs\\shipnoise_c5.jpg', dpi=600, width=9, height=7)

ggplot(data=data_dws, aes(x=freq, y=db,color=dataset))+
  geom_line(size=1)+
  theme_classic()+
  theme(legend.title = element_blank(),axis.title=element_text(size=15))+
  scale_x_log10(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  scale_color_manual(values=c("firebrick","goldenrod","cadetblue"))+
  labs(x="Frequency (Hz)", y="Sound pressure (dB re 1µPa)")+
  annotation_logticks(sides="b")

ggplot(data=data_flatws, aes(x=freq, y=db,color=dataset))+
  geom_line()+
  theme_classic()+
  theme(legend.title = element_blank())+
  scale_x_log10(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  scale_color_manual(values=c("firebrick","goldenrod","cadetblue"))+
  labs(x="Frequency (Hz)", y="Sound pressure (dB re 1µPa)")+
  annotation_logticks(sides="b")

ggplot(data=data_modws, aes(x=freq, y=db,color=dataset))+
  geom_line()+
  theme_classic()+
  theme(legend.title = element_blank())+
  scale_x_log10(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  scale_color_manual(values=c("firebrick","goldenrod","cadetblue"))+
  labs(x="Frequency (Hz)", y="Sound pressure (dB re 1µPa)")+
  annotation_logticks(sides="b")

ggplot(data=data_pulsed, aes(x=freq, y=db,color=dataset))+
  geom_line()+
  theme_classic()+
  theme(legend.title = element_blank())+
  scale_x_log10(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  scale_color_manual(values=c("firebrick","goldenrod","cadetblue"))+
  labs(x="Frequency (Hz)", y="Sound pressure (dB re 1µPa)")+
  annotation_logticks(sides="b")

ggplot(data=data_pulseflat, aes(x=freq, y=db,color=dataset))+
  geom_line(size=1)+
  theme_classic()+
  theme(legend.title = element_blank(),axis.title=element_text(size=15))+
  scale_x_log10(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  scale_color_manual(values=c("firebrick","goldenrod","cadetblue"))+
  labs(x="Frequency (Hz)", y="Sound pressure (dB re 1µPa)")+
  annotation_logticks(sides="b")

ggplot(data=data_pulseflatseg, aes(x=freq, y=db,color=dataset))+
  geom_line()+
  theme_classic()+
  theme(legend.title = element_blank())+
  scale_x_log10(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  scale_color_manual(values=c("firebrick","goldenrod","cadetblue"))+
  labs(x="Frequency (Hz)", y="Sound pressure (dB re 1µPa)")+
  annotation_logticks(sides="b")

