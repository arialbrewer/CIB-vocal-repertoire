#Arial Brewer
#PhD- Chapter 1
#CIB vocal repertoire- call type and category plots and rarefaction curve

library(tidyverse)
library(vegan)
library(patchwork)
library(cowplot)
library(viridis)

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
  mutate(file_name=basename(file_name),location="bigsu") %>% 
  mutate(n = Call_type %>% accumulate(append) %>% map_int(n_distinct),
         total_n= 1:nrow(.))

tradbay <- tbfiles %>% 
    map(read_csv, id="file_name") %>% 
    reduce(rbind) %>% 
    mutate(file_name=basename(file_name),location="tradbay") %>% 
    mutate(n = Call_type %>% accumulate(append) %>% map_int(n_distinct),
           total_n= 1:nrow(.)) 
    
ci_total <- bind_rows(bigsu,tradbay) %>% 
  mutate(n = Call_type %>% accumulate(append) %>% map_int(n_distinct),
         total_n= 1:nrow(.))

#counts for each call type total and per location
callcount_total <- ci_total %>% 
  group_by(Call_type) %>% 
  summarise(number=n())

callcount_bs <- bigsu %>% 
  group_by(Call_type) %>% 
  summarise(number=n())

callcount_tb <- tradbay %>% 
  group_by(Call_type) %>% 
  summarise(number=n())

#manually added in call category column and re-read updated data in
callcount_bs_cat <- read_csv("callcount_bs_cat.csv")
callcount_tb_cat <- read_csv("callcount_tb_cat.csv")
callcount_total_cat <- read_csv("callcount_total_cat.csv")

#counts per category (ws,cc,pc) Total
catcounts <- callcount_total_cat %>% 
  group_by(category) %>% 
  summarise(number = sum(number))%>% 
  mutate(perc = number / sum(number)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

#counts per category (ws,cc,pc) Big Su
catcounts_bs <- callcount_bs_cat %>% 
  group_by(category) %>% 
  summarise(number = sum(number))%>% 
  mutate(perc = number / sum(number)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

#counts per category (ws,cc,pc) Trading Bay
catcounts_tb <- callcount_tb_cat %>% 
  group_by(category) %>% 
  summarise(number = sum(number))%>% 
  mutate(perc = number / sum(number)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))


## plots for all calls
#Cook Inlet total (BS & TB combined)
p1 <- ggplot(data=callcount_total_cat, aes(x=number, y=reorder(Call_type,number),fill=category)) +
  geom_col()+
  theme_classic()+
  theme(legend.position="none",
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14))+
  labs(x="Number", y="Call type")+
  scale_fill_viridis(discrete=TRUE,guide=guide_legend(reverse=TRUE),
                     n, alpha = 0.8, begin = 0.5, end = 1, direction = -1, option = "D")+
  scale_x_continuous(expand=c(0,0))

p2 <- ggplot(data=catcounts, aes(x="", y=number,fill=category)) +
  geom_bar(stat='identity',width=1, color='white')+
  coord_polar("y",start=0)+
  theme_void()+
  scale_fill_viridis(discrete=TRUE,guide=guide_legend(title="Category",reverse=TRUE),
                     n, alpha = 0.8, begin = 0.5, end = 1, direction = -1, option = "D")+
  geom_label(aes(label = labels), color = c(1, "black", "black"),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE)

ggdraw(p1)+
  draw_plot(p2, x=0.45, y=0.2, width=0.5, height=0.5) 

ggsave('figs\\CI_rep_distribution.jpg', dpi=600, width=9, height=7)


#Susitna
p3 <- ggplot(data=callcount_bs_cat, aes(x=number, y=reorder(Call_type,number),fill=category)) +
  geom_col()+
  theme_classic()+
  theme(legend.position="none",
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14))+
  labs(x="Number", y="Call type")+
  scale_fill_viridis(discrete=TRUE,guide=guide_legend(reverse=TRUE),
                     n, alpha = 0.8, begin = 0.5, end = 1, direction = -1, option = "D")+
  scale_x_continuous(expand=c(0,0))

p4 <- ggplot(data=catcounts_bs, aes(x="", y=number,fill=category)) +
  geom_bar(stat='identity',width=1, color='white')+
  coord_polar("y",start=0)+
  theme_void()+
  scale_fill_viridis(discrete=TRUE,guide=guide_legend(title="Category",reverse=TRUE),
                     n, alpha = 0.8, begin = 0.5, end = 1, direction = -1, option = "D")+
  geom_label(aes(label = labels), color = c(1, "black", "black"),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE)

ggdraw(p3)+
  draw_plot(p4, x=0.45, y=0.2, width=0.5, height=0.5) 

ggsave('figs\\Susitna_rep_distribution.jpg', dpi=600, width=9, height=7)


#Trading Bay
p5 <- ggplot(data=callcount_tb_cat, aes(x=number, y=reorder(Call_type,number),fill=category)) +
  geom_col()+
  theme_classic()+
  theme(legend.position="none",
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14))+
  labs(x="Number", y="Call type")+
  scale_fill_viridis(discrete=TRUE,guide=guide_legend(reverse=TRUE),
                     n, alpha = 0.8, begin = 0.5, end = 1, direction = -1, option = "D")+
  scale_x_continuous(expand=c(0,0))

p6 <- ggplot(data=catcounts_tb, aes(x="", y=number,fill=category)) +
  geom_bar(stat='identity',width=1, color='white')+
  coord_polar("y",start=0)+
  theme_void()+
  scale_fill_viridis(discrete=TRUE,guide=guide_legend(title="Category",reverse=TRUE),
                     n, alpha = 0.8, begin = 0.5, end = 1, direction = -1, option = "D")+
  geom_label(aes(label = labels), color = c(1, "black", "black"),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE)

ggdraw(p5)+
  draw_plot(p6, x=0.45, y=0.2, width=0.5, height=0.5) 

ggsave('figs\\TradingBay_rep_distribution.jpg', dpi=600, width=9, height=7)


###PLOT FOR MANUSCRIPT
#TOTAL CI 
#p1,p2=total  
#p4=big su
#p6=trad bay

ggdraw(p1)+
  draw_plot(p2, x=0.45, y=0.2, width=0.5, height=0.5)

ggsave('figs\\CI_rep_distribution_total.jpg', dpi=600, width=9, height=7)

#pie chart on the side for each location separately
p4/p6

ggsave('figs\\CI_rep_distribution_pie_both_sites.jpg', dpi=600, width=9, height=7)


#######Chi2 test for differences by location
##call category
catcounts_compare <-left_join(catcounts_bs,catcounts_tb, by="category") %>% 
                    rename("bigsu"="number.x", "tradbay"="number.y") %>% 
                    select(-perc.x, -perc.y, -labels.x,-labels.y) %>% 
                    column_to_rownames(.,var = "category") %>% 
                    t()

chisq.test(catcounts_compare)
chisq.test(catcounts_compare,simulate.p.value = TRUE)

##call type
callcounts_compare <-full_join(callcount_bs,callcount_tb, by= "Call_type") %>% 
                     rename("bigsu"="number.x", "tradbay"="number.y") %>% 
                     column_to_rownames(.,var = "Call_type") %>% 
                     mutate_all(~replace_na(.,0)) %>% 
                     t()

chisq.test(callcounts_compare)
chisq.test(callcounts_compare,simulate.p.value = TRUE)


###Rarefaction curves by location
rare.curve <- ci_total %>% 
  select(Call_type, location) %>% 
  group_by(location) %>% 
  count(Call_type, name = "ncalls") %>% 
  pivot_wider(id_cols = location, names_from = Call_type, values_from = ncalls) %>% 
  replace(is.na(.), 0) %>% 
  column_to_rownames("location")

rarecurve(rare.curve, xlab="Number of calls sampled",ylab="Number of call types",
          col=c("firebrick","darkcyan"), lwd=3, label=FALSE)
legend(700, 35, legend=c("Susitna","Trading Bay"),
       col=c("firebrick2", "darkcyan"), lty=1, lwd=2, cex=0.6)


####Rarefaction curve for CI total
rare.curve_CItotal <- ci_total %>% 
  select(Call_type) %>% 
  count(Call_type, name = "ncalls") %>% 
  pivot_wider(names_from = Call_type, values_from = ncalls) %>% 
  replace(is.na(.), 0) 

rarecurve(rare.curve_CItotal, xlab="Number of calls sampled", ylab="Number of call types", 
          col="black", lwd=3, label=FALSE)


####Total CI + each location
rare.curve <- ci_total %>% 
  select(Call_type, location) %>% 
  group_by(location) %>% 
  count(Call_type, name = "ncalls") %>% 
  pivot_wider(id_cols = location, names_from = Call_type, values_from = ncalls) %>% 
  replace(is.na(.), 0) %>% 
  column_to_rownames("location") %>% 
  bind_rows(summarise(.,across(where(is.numeric),sum),
                              across(where(is.character),~"total")))

rarecurve(rare.curve, xlab="Number of calls sampled",ylab="Number of unique call types",
          col=c("cadetblue","grey", "black"), lty=1, lwd=3, label=FALSE)
legend(1300,40, legend=c("Susitna","Trading Bay", "Total"),
       col=c("cadetblue", "grey", "black"), lty=1, lwd=3, cex=0.8)



#### Discrete/stereotyped calls only for revised JASA manuscript Fig.3 ####
#calls that occur n=3 or more
callcount_bs_dis <- read_csv("callcount_bs-discrete.csv")
callcount_tb_dis <- read_csv("callcount_tb-discrete.csv")
callcount_total_dis <- read_csv("callcount_total-discrete.csv")
callcount_bs_cat_dis <- read_csv("callcount_bs_cat-discrete.csv")
callcount_tb_cat_dis <- read_csv("callcount_tb_cat-discrete.csv")
callcount_total_cat_dis <- read_csv("callcount_total_cat-discrete.csv")

#counts per category (ws,cc,pc) Total
catcounts_dis <- callcount_total_cat_dis %>% 
  group_by(category) %>% 
  summarise(number = sum(number))%>% 
  mutate(perc = number / sum(number)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc, accuracy=0.1))

write_csv(catcounts_dis,"C:/Users/Arial/OneDrive - UW/Desktop/CH.1 call classification/CIB repertoire code/catcounts_dis.csv")

#counts per category (ws,cc,pc) Big Su
catcounts_bs_dis <- callcount_bs_cat_dis %>% 
  group_by(category) %>% 
  summarise(number = sum(number))%>% 
  mutate(perc = number / sum(number)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc, accuracy=0.1))

#counts per category (ws,cc,pc) Trading Bay
catcounts_tb_dis <- callcount_tb_cat_dis %>% 
  group_by(category) %>% 
  summarise(number = sum(number))%>% 
  mutate(perc = number / sum(number)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc, accuracy=0.1))

## Plots for discrete calls only- Cook Inlet total (BS & TB combined)
#bar chart
ggplot(data=callcount_total_cat_dis, aes(x=number, y=reorder(Call_type,number),fill=category)) +
  geom_col()+
  theme_classic()+
  theme(legend.position="none",
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14))+
  labs(x="Number", y="Call type")+
  scale_fill_viridis(discrete=TRUE,guide=guide_legend(reverse=TRUE),
                     n, alpha = 0.8, begin = 0.5, end = 1, direction = -1, option = "D")+
  scale_x_continuous(expand=c(0,0))

ggsave('figs\\CI_rep_calltype_discrete.jpg', dpi=600, width=9, height=7)

#pie chart
ggplot(data=catcounts_dis, aes(x="", y=number,fill=category)) +
  geom_bar(stat='identity',width=1, color='white')+
  coord_polar("y",start=0)+
  theme_void()+
  scale_fill_viridis(discrete=TRUE,guide=guide_legend(title="Category",reverse=TRUE),
                     n, alpha = 0.8, begin = 0.5, end = 1, direction = -1, option = "D")+
  geom_label(aes(label = labels, size=5), color = c(1, "black", "black"),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE)

ggsave('figs\\CI_rep_callcat_discrete.jpg', dpi=600, width=9, height=7)


#Susitna pie chart
ggplot(data=catcounts_bs_dis, aes(x="", y=number,fill=category)) +
  geom_bar(stat='identity',width=1, color='white')+
  coord_polar("y",start=0)+
  theme_void()+
  scale_fill_viridis(discrete=TRUE,guide=guide_legend(title="Category",reverse=TRUE),
                     n, alpha = 0.8, begin = 0.5, end = 1, direction = -1, option = "D")+
  geom_label(aes(label = labels, size=5), color = c(1, "black", "black"),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE)

ggsave('figs\\Susitna_cat_discrete.jpg', dpi=600, width=9, height=7)


#Trading Bay pie chart
ggplot(data=catcounts_tb_dis, aes(x="", y=number,fill=category)) +
  geom_bar(stat='identity',width=1, color='white')+
  coord_polar("y",start=0)+
  theme_void()+
  scale_fill_viridis(discrete=TRUE,guide=guide_legend(title="Category",reverse=TRUE),
                     n, alpha = 0.8, begin = 0.5, end = 1, direction = -1, option = "D")+
  geom_label(aes(label = labels, size=5), color = c(1, "black", "black"),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE)


ggsave('figs\\TradingBay_cat_discrete.jpg', dpi=600, width=9, height=7)


#######Chi2 test for differences by location with stereotyped/discrete calls only
##call category
catcounts_compare_dis <-left_join(catcounts_bs_dis,catcounts_tb_dis, by="category") %>% 
  rename("bigsu"="number.x", "tradbay"="number.y") %>% 
  select(-perc.x, -perc.y, -labels.x,-labels.y) %>% 
  column_to_rownames(.,var = "category") %>% 
  t()

chisq.test(catcounts_compare_dis)
chisq.test(catcounts_compare_dis,simulate.p.value = TRUE)

##call type
callcounts_compare_dis <-full_join(callcount_bs_dis,callcount_tb_dis, by= "Call_type") %>% 
  rename("bigsu"="number.x", "tradbay"="number.y") %>% 
  column_to_rownames(.,var = "Call_type") %>% 
  mutate_all(~replace_na(.,0)) %>% 
  t()

chisq.test(callcounts_compare_dis)
chisq.test(callcounts_compare_dis,simulate.p.value = TRUE)
