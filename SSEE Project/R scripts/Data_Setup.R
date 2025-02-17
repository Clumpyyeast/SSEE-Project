library(tidyverse)
library()

knitr::opts_knit$set(root.dir = "C:/git/SSEE/SSEE Project")


setwd("C:/git/SSEE/SSEE Project")


galdata=read.csv("./Original Data/GAL settling phenotyping 2024 T3 14.csv")


galdata$Treatment <- factor(galdata$Treatment, levels = c("YPD","YPDSET","GAL", "GALSET"))

