library(readxl)
library(tidyverse)
library(ggplot2)
library(viridis)

setwd("C:/Users/gg9824/Dropbox/00ESRC Fellowship/Projects/COVID19/Time varying selection/")  

### function to read in dataset



df_test <- read_data("test_20211018.xlsx")
df_pos <- read_data("positive_test_20211018.xlsx")

### Combine datasets with indicator for "test"
df <- bind_rows(df_test, df_pos, .id="test")

### Recode "test" indicator to give testing vs testing positive indicator.
df$test <- df$test %>% recode(`1`="Tested", `2`="Tested Pos")

# Order Time Periods so "whole year" is last on the list. 
df$`Time period` <- factor(df$`Time period`, levels=c("1", "2", "3", "4", "Whole year"))

### offset viridis colours by 2 to remove yellow and light green
viridis_man <- (c(viridis::viridis(n=22)))[1:18]

### Graphics

### Recode Risk Factors to order required for graphic

df$`Risk factor` <- factor(df$`Risk factor`, levels=c("B","AB","A","O", "GCSE/O level or less", "AS/A level", "Vocational qualifications",
                                                      "Degree level or higher", "5", "4", "3", "2", "1", "Less than £18,000",
                                                      "£18,000-£30,999", "£31,000-51,599", "£52,000-£100,000", "Greater than £100,000"))


testPlot <- ggplot(df %>% filter(type==1),
                   aes(x= `Time period`,
                       y=Beta,
                       group = `Risk factor`,
                       colour=`Risk factor`)) +
                    geom_point (size=1.5) +
                    geom_line(data = df %>% filter(type==1 & `Time period`!="Whole year"),
                              size=0.5, linetype=1) +
                    ylab("Odds Ratio") +
                    theme(axis.text.x=element_text(color = "black",
                                                   size=8, angle=30, hjust=0.8)) +
                    #geom_ribbon(aes(ymin=LCI, ymax=UCI),
                    #           linetype=1, alpha=0.2)+
                    facet_grid(expo~test) +
                    theme(strip.background = element_blank(), strip.text.y = element_blank(),
                          panel.border=element_blank(),
                          panel.grid.minor.x = element_blank()) +
                    scale_color_manual(values=viridis_man)+
  geom_text(data=df %>% filter(type==1) %>%  group_by(test, expo) %>% filter(row_number()==1),
            label=(df %>% filter(type==1) %>%  group_by(test, expo) %>% filter(row_number()==1))$expo,
                   y=2, size=2.5, colour="black", x = 5, hjust=0.6)

testPlot

ggsave("testPlot.png", dpi=600)

## 

## function to read and generate dataframe 

read_data <- function(data){
  ### Create empty data.frame
  df <- data.frame()
  
  
  ### Loop over each of separate excel sheets & store sheet number as "exposure"
  for (i in 1:9){
    res <- read_xlsx(data, sheet=i)
    res$expo <- i
    ### drop missing rows and columns
    res <- res[complete.cases(res[, 1:2]),][,-c(4,9)]
    ### append to all sheets
    df <- rbind(df, res)
  }
  
  
  ## recode numbers to exposures
  df$expo <- df$expo %>% recode(`1`="N Rooms", `2`="N Vehicles", `3`="Hair Colour", `4`="Highest Qual",
                                `5`="ABO Type", `6` ="Income", `7`="House Type", `8`="IMD Quint",
                                `9`="House Ownership")
  
  
  # generate column for "type" for main manuscript vs supplement exposures.
    mains <- c("Highest Qual", "Income", "IMD Quint", "ABO Type")
    supps <- c("N Rooms", "N Vehicles", "Hair Colour", "House Type", "House Ownership")
    df <- df %>% mutate(type = (case_when(
    `expo` %in% mains ~ 1,
    `expo` %in% supps ~ 0)
  ))
  #return dataset
    return(df)
}
