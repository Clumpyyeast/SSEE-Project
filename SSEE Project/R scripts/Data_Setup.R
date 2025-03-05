library(tidyverse)
library()

knitr::opts_knit$set(root.dir = "C:/git/SSEE/SSEE Project")

#### The code below will set the relative working directory and load the data 
# for each of the analysis and graphs used in this project. They are seperated 
# below by their distinct graph. 


setwd("C:/git/SSEE/SSEE Project")


galdata=read.csv("./Original Data/GAL settling phenotyping 2024 T3 14.csv")


galdata$Treatment <- factor(galdata$Treatment, levels = c("YPD","YPDSET","GAL", "GALSET"))

################################################################################
#####the code below is used for making the various clump sizes over time graphs
# between the four treatments and together in one combined graph as well. 

galdata$Binned_Number <- cut(galdata$Number, 
                             breaks = c(1, 5, 10, 20, 30, 40, 50, 100, 205),  # Updated breaks
                             labels = c("1-4","5-9", "10-19", "20-29", "30-39", "40-49", "50-100", "101-204"),
                             right = FALSE)

# Recalculate totals, excluding NA bins
galdata_summary <- galdata %>%
  group_by(Transfer, Treatment, Binned_Number) %>%
  summarise(Total_Cells = sum(Number), .groups = "drop") %>%
  complete(Transfer, Treatment, Binned_Number, fill = list(Total_Cells = 0)) %>%
  filter(!is.na(Binned_Number))

# Define custom x-axis ticks
custom_x_ticks <- c(1, 4, 8, 12, 16, 20, 24, 28)



################################################################################
### Code below is for the stacked bar graphs comparing cell counts directly from
# the galdata between treaments



# Filter out NAs
data <- galdata %>% filter(!is.na(Number) & !is.na(Treatment) & !is.na(Transfer))

# Define the exact clump size bins
data <- data %>%
  mutate(Clump_Size_Bin = cut(Number,
                              breaks = c(0, 4, 9, 19, 29, 39, 49, 100, 204),
                              labels = c("1-4", "5-9", "10-19", "20-29", 
                                         "30-39", "40-49", "50-100", "101-204"),
                              include.lowest = TRUE))

# Ensure bins are ordered properly
data$Clump_Size_Bin <- factor(data$Clump_Size_Bin, 
                              levels = c("1-4", "5-9", "10-19", "20-29", 
                                         "30-39", "40-49", "50-100", "101-204"))

# Set the order of treatments
data$Treatment <- factor(data$Treatment, levels = c("YPD", "YPDSET","GAL", "GALSET"))

# Calculate proportions of clump sizes for each Transfer and Treatment
data_summary <- data %>%
  group_by(Treatment, Transfer, Clump_Size_Bin) %>%
  summarise(Total = n(), .groups = "drop") %>%
  group_by(Treatment, Transfer) %>%
  mutate(Proportion = Total / sum(Total))

###############################################################################
## This data below is for the total cells stacked bar charts which look at the 
## total cell number instead of just counts by themselves, gives a better idea
## where DNA and the actual number of cells are. 

# Calculate proportions of total cells for each binned clump size
Total_Cells <- galdata_summary %>%
  group_by(Treatment, Transfer) %>%
  mutate(Proportion = Total_Cells / sum(Total_Cells))

# Set the order of treatments
data$Treatment <- factor(data$Treatment, levels = c("YPD", "YPDSET","GAL", "GALSET"))



