library(tidyverse)
library(ggplot2)
library(viridis)
library(lubridate)

setwd("C:/Users/gg9824/Dropbox/00ESRC Fellowship/Projects/COVID19/Time varying selection/")  

df <- read.csv("studyDates.csv")
df$StartDate <- df$StartDate %>% dmy()
df$EndDate <- df$EndDate %>% dmy()

colnames <- c("Period", "Start", "End")
colnames(df) <- colnames
df$Period <- factor(df$Period, levels=c(1,2,3,4))

dateplot <- ggplot(df)+
  geom_segment(aes(x=Start, xend=End, y=Period, yend=Period, colour=Period), linetype =1, size=6)+
  scale_fill_discrete(viridis) +
  #ticks for X axis at bimonthly
  theme(axis.line.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.x =element_blank(),
        legend.position = "bottom")+
  scale_x_date(date_breaks="2 months")+
  theme(axis.text.x=element_text(angle=45, size=8, vjust=0.5))+
  scale_x_date(date_breaks = "2 months", date_labels="%b %y")+
  geom_vline(xintercept = lubridate::dmy('01-01-2021'), linetype = "dashed", colour = "darkblue") 

ann.text <- data.frame(Start = df$Start, y=df$Period, lab= c("Pre-Mass Test", "Mass Testing", "Tiered Lockdown", "Winter Wave")) 

annot_date_plot <- dateplot+ 
  geom_text(data=ann.text,
            mapping = aes(x=Start, y=y, label = lab),
            size=3,
            hjust=-0.1,
            vjust=-1.2,
            colour="black")
annot_date_plot

ggsave("datePlot.png", dpi=325, width=25, height=8, units="cm")

