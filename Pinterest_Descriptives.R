#===========================================================================================================================================
# SCRIPT PURPOSE:
# This script is used to obtain descriptive statistics for the the output of the mTurk results for the Pinterest data project
# aftey they have been cleaned in the Pinterest_Cleaning.R script
# Date: 11/25/2015
#===========================================================================================================================================

setwd("/Users/taylorbrown/Google\ Drive/Projects\ Working/Little_Monster/Pinterest\ Project/littlemonster")

#===========================================================================================================================================
# (1) SETUP
#===========================================================================================================================================
# LOAD PACKAGES
#-------------------------------------------------------------------------------------------------------------------------------------------

library(foreign)
library(doBy)
library(ggplot2)
library(psych)
library(xtable)
library(pastecs)
library(lme4)
library(reporttools)
library(xtable)

#-------------------------------------------------------------------------------------------------------------------------------------------
# LOAD DATA
#-------------------------------------------------------------------------------------------------------------------------------------------

load("Pinterest_data_analysis.Rdata")






#===========================================================================================================================================
# (2) DESCRIPTIVES
#===========================================================================================================================================

#-------------------------------------------------------------------------------------------------------------------------------------------
# Demographics of Turkers
#-------------------------------------------------------------------------------------------------------------------------------------------
# Unique # of Turkers
length(unique(pinterest$WorkerId))

# Gender
table(pinterest$female_turker)

# Age
summary(pinterest$age_turker)
pinterest$age_turker[pinterest$age_turker == 858] <- NA

# Age by gender
summaryBy(age_turker~female_turker, data=pinterest, FUN=c(min, mean, median, max, skew), na.rm=TRUE)

# Race
table(pinterest$race_turker)

#-------------------------------------------------------------------------------------------------------------------------------------------
# General
#-------------------------------------------------------------------------------------------------------------------------------------------

# How many fell into each section?
pinterest$section <- ''
pinterest$section[!is.na(pinterest$image_background_sA)] <- 1
pinterest$section[!is.na(pinterest$image_background_sB)] <- 2
pinterest$section[!is.na(pinterest$image_background_sC)] <- 3
pinterest$section[pinterest$section == ''] <- NA

table(pinterest$section, useNA = c("always"))

# How many people are in each?
summary(pinterest$image_count_sA)
summary(pinterest$image_count_sB)
summary(pinterest$image_count_sC)


#-------------------------------------------------------------------------------------------------------------------------------------------
# Section A
#-------------------------------------------------------------------------------------------------------------------------------------------

# I realize that I can just identify section as above 
# (i.e. using the decision rule of whether the relevant 'image_background' question was answered),
# but I wanted to be more meticulous and make sure any coding errors on that question didn't drop relevant rows.
# To do this, I define dataframes according to all of teh relevant section variables:

pinterest_SectionAs <- pinterest[c("WorkerId", "Input.url", "Input.pinner_avatar", "Input.board_id", "Input.index", "Input.batch", 
                                   "female_turker", "age_turker", "race_turker", "image_count_sA", "image_background_sA",
                                   "content_FoodDrink_sA", "content_Supplement_sA", "content_FitnessEqp_sA", "content_Clothes_sA", 
                                   "content_Other_sA", "image_content_sA", "image_contentother_sA", "theme_Health_sA", "theme_DiseaseIll_sA",
                                   "theme_Medicine_sA", "theme_Mensuration_sA", "theme_Science_sA", "theme_Infographic_sA", "theme_PhysicalFit_sA",
                                   "theme_Cycling_sA", "theme_Running_sA", "theme_Yoga_sA", "theme_FitnessEqp_sA", "theme_WorkoutPlan_sA",
                                   "theme_Fashion_sA", "theme_Weightloss_sA", "theme_FoodDrink_sA", "theme_Sugar_sA", "theme_Detox_sA",
                                   "theme_Supplement_sA", "theme_MentalHealth_sA", "theme_EmotionalWell_sA", "theme_Beauty_sA", "theme_Sleep_sA",
                                   "theme_Other_sA", "image_themes_sA", "image_themesother_sA","text_sA")]


# CONTINUOUS VARIABLES (SECTION A)
Pinterest_desc_cont <- pinterest_SectionAs[, c()]

cap1 <- "Continuous Variables"
stats <- list("n", "min", "median", "$\\bar{x}_{\\mathrm{trim}}$" = function(x){return(mean(x, trim = .05))}, "max", "iqr", "c$_{\\mathrm{v}}$" = function(x){return(sd(x) / mean(x))}, "s", "na")
tableContinuous(vars = Pinterest_desc_cont, stats = stats, cap = cap1)



# CATEGORICAL VARIABLES (SECTION A)
Pinterest_desc_cat <- pinterest_SectionAs[, c()]
cap2 <- "Categorical Variables"
tableNominal(vars = Pinterest_desc_cat, cap = cap2, vertical = FALSE)



#-------------------------------------------------------------------------------------------------------------------------------------------
# Section B
#-------------------------------------------------------------------------------------------------------------------------------------------

pinterest_SectionBs <- pinterest[c("WorkerId", "Input.url", "Input.pinner_avatar", "Input.board_id", "Input.index", "Input.batch", 
                                   "female_turker", "age_turker", "race_turker", "image_count_sB", "actualperson_sB", "bodyparts_Head_sB",
                                   "bodyparts_Face_sB", "bodyparts_UpperTorsoF_sB", "bodyparts_UpperTorsoB_sB", "bodyparts_Stomach_sB",
                                   "bodyparts_UpperArm_sB","bodyparts_LowerArm_sB", "bodyparts_Hand_sB", "bodyparts_Bum_sB", "bodyparts_Hip_sB",
                                   "bodyparts_UpperLeg_sB", "bodyparts_LowerLeg_sB", "bodyparts_Feet_sB", "bodyparts_sB", "bodyparts_naked_Head_sB",
                                   "bodyparts_naked_Face_sB", "bodyparts_naked_UpperTorsoF_sB", "bodyparts_naked_UpperTorsoB_sB", "bodyparts_naked_Stomach_sB",
                                   "bodyparts_naked_UpperArm_sB","bodyparts_naked_LowerArm_sB", "bodyparts_naked_Hand_sB", "bodyparts_naked_Bum_sB",
                                   "bodyparts_naked_Hip_sB", "bodyparts_naked_UpperLeg_sB", "bodyparts_naked_LowerLeg_sB",
                                   "bodyparts_naked_Feet_sB", "bodyparts_naked_sB", "race_sB", "female_sB", "age_sB", "emotion_sB", "health_sB",
                                   "attractive_sB", "sexually_sB", "sexualized_sB", "physcondition_sB", "bodytype_sB", "image_background_sB",
                                   "content_FoodDrink_sB", "content_Supplement_sB", "content_FitnessEqp_sB", "content_Clothes_sB",
                                   "content_Nature_sB", "content_Urban_sB", "content_Other_sB", "image_content_sB", "image_contentother_sB",
                                   "text_sB","theme_Health_sB", "theme_DiseaseIll_sB", "theme_Medicine_sB", "theme_Mensuration_sB",
                                   "theme_Science_sB", "theme_Infographic_sB", "theme_PhysicalFit_sB", "theme_Cycling_sB", "theme_Running_sB",
                                   "theme_Yoga_sB", "theme_FitnessEqp_sB", "theme_WorkoutPlan_sB", "theme_Fashion_sB", "theme_Weightloss_sB",
                                   "theme_FoodDrink_sB", "theme_Sugar_sB", "theme_Detox_sB", "theme_Supplement_sB", "theme_MentalHealth_sB",
                                   "theme_EmotionalWell_sB","theme_Beauty_sB", "theme_Sleep_sB", "theme_Other_sB", "image_themes_sB",
                                   "image_themesother_sB", "improve_health_sB", "improve_fitness_sB", "improve_attractive_sB","cure_sB")]

#-------------------------------------------------------------------------------------------------------------------------------------------
# Section C
#-------------------------------------------------------------------------------------------------------------------------------------------

pinterest_SectionCs <- pinterest[c("WorkerId", "Input.url", "Input.pinner_avatar", "Input.board_id", "Input.index", "Input.batch", 
                                   "female_turker", "age_turker", "race_turker","image_count_sC", "actualperson_sC", "people_number_sC",
                                   "race_sC", "female_sC", "emotion_sC", "image_background_sC", "content_FoodDrink_sC", "content_Supplement_sC",
                                   "content_FitnessEqp_sC", "content_Clothes_sC", "content_Nature_sC", "content_Urban_sC", "content_Other_sC",
                                   "image_content_sC", "image_contentother_sC", "theme_Health_sC", "theme_DiseaseIll_sC", "theme_Medicine_sC",
                                   "theme_Mensuration_sC", "theme_Science_sC", "theme_Infographic_sC", "theme_PhysicalFit_sC",
                                   "theme_Cycling_sC", "theme_Running_sC", "theme_Yoga_sC", "theme_FitnessEqp_sC", "theme_WorkoutPlan_sC",
                                   "theme_Fashion_sC", "theme_Weightloss_sC", "theme_FoodDrink_sC", "theme_Sugar_sC",
                                   "theme_Supplement_sC", "theme_MentalHealth_sC", "theme_EmotionalWell_sC", "theme_Beauty_sC",
                                   "theme_Sleep_sC", "theme_Other_sC", "improve_health_sC", "improve_fitness_sC", "cure_sC",
                                   "prevent_sC", "bodyparts_Head_sC", "bodyparts_Face_sC", "bodyparts_UpperTorsoF_sC", 
                                   "bodyparts_UpperTorsoB_sC", "bodyparts_Stomach_sC", "bodyparts_UpperArm_sC", "bodyparts_LowerArm_sC",
                                   "bodyparts_Hand_sC", "bodyparts_Bum_sC", "bodyparts_Hip_sC", "bodyparts_UpperLeg_sC",
                                   "bodyparts_LowerLeg_sC", "bodyparts_Feet_sC", "bodyparts_sC", "bodyparts_naked_Head_sC", "bodyparts_naked_Face_sC",
                                   "bodyparts_naked_UpperTorsoF_sC", "bodyparts_naked_UpperTorsoB_sC", "bodyparts_naked_Stomach_sC",
                                   "bodyparts_naked_UpperArm_sC", "bodyparts_naked_LowerArm_sC", "bodyparts_naked_Hand_sC", "bodyparts_naked_Bum_sC",
                                   "bodyparts_naked_Hip_sC", "bodyparts_naked_UpperLeg_sC", "bodyparts_naked_LowerLeg_sC", "bodyparts_naked_Feet_sC",
                                   "text_sC", "health_sC", "sexualized_sC", "physcondition_sC", "improve_attractive_sC", "bodytype_sC")] 




