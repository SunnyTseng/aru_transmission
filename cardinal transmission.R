#' ---
#' title: Detection of Cardinal Song on ARUs - Transmission Testing . 
#' date: "`r format(Sys.Date())`"
#' output:
#'   html_document:
#'     toc: true
#' ---
#' First, set up a folder somewhere called "Sophia Wong" and dump into it the 
#' master_dataset file and your R code.  Then use the file menu to set up 
#' "New Project" and point at this folder
#' 
#' This will set you up the project files in the folder.  You would then
#' start a new R-script file (or use this one) and put in the same folder.
#' Open up the R script using Open File then hit save.  This will associate 
#' this script with the project, they you just need to open this project
#' each time you want to work on it.  It will then automatically set this 
#' as your working directory and save any output files into your folder. 
#'
#' ------------ 
#' Next, you need to make sure you have the packages you will use installed.  
#' Some of the stats functions are generic to R, but some require data wrangling
#' packages like dplyr, tidyr and broom, also want ggplot2 for graphs, and we need 
#' lmerTest and lme4 for running mixed-effect models.
#' Make sure you have installed the following (unhashtag and run):

#install.packages("ggplot2")
#install.packages("tidyverse")
#install.packages("lmerTest")
#install.packages("lme4")

#' you only need to do this once, so if they are already installed, just 
#' hash tag out the lines above.

#' ----------------
#' Next, use the library command to tell your computer which packages you will 
#' be using in the following analysis.  This simply gives your working memory 
#' notice of which types of functions (commands built within particular packages) 
#' you might be calling on in your script.  General stats tests, like 't.test' 
#' and 'wilcox.test' we might want to run are in basic R, so you don't have to 
#' load those.  If you are using general linear mixed models, also include lmerTest (this is based on the linear mixed effects regression package lmer and its upgrade lme4 but also gives you P values using something called an Satherwaith's approximation).
#'   
#' There are other packages for things like generalized linear models etc.  
#' Any more complex models, or things like mapping, you will need to use other 
#' packages like ggmap etc.  Tidyr and Dplyr are packages with specific functions embedded, that just make data management a little easier, so I load them into every file I use (whether I rely much on them or not)

library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(lmerTest)

#'Load in your data by reading the file you have saved in your working directory.  
#'Mine would be in a folder called Cara's data 

data <- read.csv("cardinal_playback.csv")

#' couple of quick visuals of the data

names(data)
str(data)


#'the str() command lets you check the structure of your data, so you can check that things you want as factors/numbers are correctly coded

#' to change things in to factors, use the following process.

data$plot_type <- as.factor(data$plot_type)
data$location <- as.factor(data$location)
data$beaufort <- as.integer(data$beaufort)
data$ARU <- as.factor(data$ARU)
data$direction <- as.factor(data$direction)

##' VISUALIZING DATA
#' now we can look at a couple of your continuous predictors to see if they seem to be normally distributed etc.

hist(data$confidence)  

#very skewed, so we might want to used generalized linear models instead of general linear models.


#' To make a boxplot of the data to see what your data look like, use "boxplot" 
#' of <dependent variable> ~ <coding factor> to inspect variances

#' In ggplot <aes> is the command used to tell it what your variables are, 
#' and things like <fill> gives you colour patterns.  
#' <geom> is the visual marks (e.g. dot on an x/y plot) representing 
#' your individual data, so specifying things like <geom_boxplot()> will 
#' mean "represent the data as a boxplot".  
#' In ggplot2, you use a "+" at the end of each line of code to represent 
#' a pipe (this tells it the NEXT sequential command to do on the same 
#' linked data), equivalent to %>% in tidyr.

ggplot(data = data, aes(x = distance, y = confidence, fill=plot_type)) + 
  theme_classic() + 
  geom_boxplot() +
  geom_smooth(method='lm', formula = y~x) +
  labs(x = "Distance (m)", y = "Detection Confidence in BirdNET") +  
  scale_fill_manual(values = c("white", "grey", "black", "red")) 

#' this didn't work to separate things the way I thought, so I wonder if that 
#' is due to the inclusion of the Playback Plot type.  

data2 <- data [!(data$plot_type== "PB"), ]

#' this just removes all the lines of data with PB in the Plot_type variable

ggplot(data = data2, aes(x = plot_type, y = confidence, fill=plot_type)) + 
  theme_classic() + 
  geom_boxplot() +
  geom_smooth(method='lm', formula = y~x) +
  labs(x = "Distance (m)", y = "Detection Confidence in BirdNET") +  
  scale_fill_manual(values = c("white", "red", "black")) +
  facet_wrap(~distance)

#' this is closer to what I was trying to get, but still not right!
#' I am trying to get a boxplot of distance by Detection confidence showing 
#' Control, FOrest and Partial Cut seperately at 50, 75 and 100m.  Ideally,
#' with a trend line for each treatment.  Just not sure what I am doing wrong!

#'-------
#'# Linear Models 
#'-------

#' doing this on the dataset (data2) with the original detection confidences of 
#' the playback removed (data2).  Looked at the effect of distance, plot type, 
#' direction and ARU age on confidence while controlling for wind speed (beaufort)
#' and song exemplar (if I have done this right)

lme.fit <- lmer(confidence ~ distance * plot_type * direction * ARU + (1|beaufort) +(1|song_exp), data = data2)
summary(lme.fit)
plot(lme.fit)

#' This suggest that distance, plot type and direction effect detection confidence
#' as does the interaction between plot type and disance, but that old vs new 
#' was not significant.  Removed non-sig interactions. 
#' 
#' residual plots are way off, so maybe try and generalized linear model?

glme.fit2 <- glmer(confidence ~ distance + plot_type + direction + ARU + distance*plot_type + (1|beaufort) +(1|song_exp), family=binomial, data = data2)
summary(glme.fit2)
plot(glme.fit2)

#' still not convinced I am doing this correctly.  Time to ask Sunny.


