### Practice for final QMEE project

library(tidyverse)
library(lme4)
library(lmPerm)
library(car)
library(ggplot2); theme_set(theme_bw())

# Read in our main data frame of female fly lifespan
fly_dat1 <- read.csv("lifespan.csv")