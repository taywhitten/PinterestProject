#===========================================================================================================================================
# SCRIPT PURPOSE:
# This script is used to clean the output of the mTurk results for the Pinterest data project
#===========================================================================================================================================


#===========================================================================================================================================
# (1) SETUP
#===========================================================================================================================================
# LOAD PACKAGES & DATA
#-------------------------------------------------------------------------------------------------------------------------------------------
setwd("/Users/taylorbrown/Google\ Drive/Projects\ Working/Little_Monster/Pinterest\ Project/littlemonster")
load("~/Downloads/FinalData.Rdata")

pinterest <- data[, c(16,28:98)]

library(car)





#===========================================================================================================================================
# (2) CORRECT & FORMAT COLUMNS
#===========================================================================================================================================

#-------------------------------------------------------------------------------------------------------------------------------------------
# female_turker : the gender of the turker
#-------------------------------------------------------------------------------------------------------------------------------------------
pinterest$female_turker <- recode(pinterest$Answer.Q01Answer0, '"Male" = 0; "Female" = 1; "Don\'t want to answer" = 3; "" = NA')
class(pinterest$female_turker)
table(pinterest$female_turker, useNA = c("always"))
recode <- c(Male = 0, Female = 1, Refuse = 3)
pinterest$female_turker <- factor(pinterest$female_turker, levels = recode, labels = names(recode), exclude = NULL)
class(pinterest$female_turker)
table(pinterest$female_turker, useNA = c("always"))
pinterest$Answer.Q01Answer0 <- NULL 


#-------------------------------------------------------------------------------------------------------------------------------------------
# age_turker : the age of the turker
#-------------------------------------------------------------------------------------------------------------------------------------------
pinterest$age_turker <- pinterest$Answer.Q02Answer0
class(pinterest$age_turker)
table(pinterest$age_turker, useNA = c("always"))
pinterest$age_turker <- as.integer(pinterest$age_turker)
class(pinterest$age_turker)
table(pinterest$age_turker, useNA = c("always"))
pinterest$Answer.Q02Answer0 <- NULL

#-------------------------------------------------------------------------------------------------------------------------------------------
# race_turker : the race of the turker
# [White/Non-Hispanic, Black/Non-hispanic, Hispanic, Asian, Other, Don’t want to answer]
#-------------------------------------------------------------------------------------------------------------------------------------------
class(pinterest$Answer.Q03Answer0)
table(pinterest$Answer.Q03Answer0, useNA = c("always"))
pinterest$race_turker <- recode(pinterest$Answer.Q03Answer0, '"White / Non-Hispanic" = 1; "Black / Non-Hispanic" = 2; "Hispanic" = 3; "Asian" = 4; "Other" = 5; "Don\'t want to answer" = 6; "" = NA')
class(pinterest$race_turker)
table(pinterest$race_turker, useNA = c("always"))
recode_race <- c(WhiteNonHispanic = 1, BlackNonHispanic = 2, Hispanic = 3, Asian = 4, Other = 5, Refuse = 6)
pinterest$race_turker <- factor(pinterest$race_turker, levels = recode_race, labels = names(recode_race), exclude = NULL)
class(pinterest$race_turker)
table(pinterest$race_turker, useNA = c("always"))
pinterest$Answer.Q03Answer0 <- NULL 


# SECTION A
#===========================================================================================================================================

#-------------------------------------------------------------------------------------------------------------------------------------------
# image_count_sA : count of the number of people in the image
#-------------------------------------------------------------------------------------------------------------------------------------------
class(pinterest$Answer.Q1Answera)
table(pinterest$Answer.Q1Answera, useNA = c("always"))
pinterest$image_count_sA <- recode(pinterest$Answer.Q1Answera, '"0" = 0; "1" = 1; "2 or more" = 2; "" = NA')  
class(pinterest$image_count_sA)
table(pinterest$image_count_sA, useNA = c("always"))
recode_count <- c(None = 0, One = 1, Many = 2, NoAnswer = 3)
pinterest$image_count_sA <- factor(pinterest$image_count_sA, levels = recode_count, labels = names(recode_count))
class(pinterest$image_count_sA)
table(pinterest$image_count_sA, useNA = c("always"))
pinterest$Answer.Q1Answera <- NULL 



#-------------------------------------------------------------------------------------------------------------------------------------------
# image_background_sA : image background section A
# [indoors, outdoors, image shows artificial background, don’t know]
#-------------------------------------------------------------------------------------------------------------------------------------------
class(pinterest$Answer.Q12Answera)
table(pinterest$Answer.Q12Answera, useNA = c("always"))
pinterest$image_background_sA <- recode(pinterest$Answer.Q12Answera, '"Indoors" = 1; "Outdoors" = 2; "Artificial" = 3; "Don\'t know" = 4; "" = NA')  
class(pinterest$image_background_sA)
table(pinterest$image_background_sA, useNA = c("always"))
recode_background <- c(Indoors = 1, Outdoors = 2, Artificial = 3, DontKnow = 4)
pinterest$image_background_sA <- factor(pinterest$image_background_sA, levels = recode_background, labels = names(recode_background))
class(pinterest$image_background_sA)
table(pinterest$image_background_sA, useNA = c("always"))
pinterest$Answer.Q12Answera <- NULL 




#-------------------------------------------------------------------------------------------------------------------------------------------
# image_content_sA : image content section A
# [Food/Drink, Nutritional supplements, Fitness equipment, Clothes, Nature scene, Urban scene, Other]
#-------------------------------------------------------------------------------------------------------------------------------------------

class(pinterest$Answer.Q13Answera)
table(pinterest$Answer.Q13Answera, useNA = c("always"))


pinterest$content_FoodDrink_sA <- gregexpr(".*Food.*", pinterest$Answer.Q13Answera) > 0
table(pinterest$content_FoodDrink_sA)
pinterest$content_Supplement_sA <- gregexpr(".*Supplement.*", pinterest$Answer.Q13Answera) > 0
table(pinterest$content_FoodDrink_sA)
pinterest$content_FitnessEqp_sA <- gregexpr(".*Fitness.*", pinterest$Answer.Q13Answera) > 0
table(pinterest$content_FitnessEqp_sA)
pinterest$content_Clothes_sA <- gregexpr(".*Clothes.*", pinterest$Answer.Q13Answera) > 0
table(pinterest$content_Clothes_sA)
pinterest$content_Nature_sA <- gregexpr(".*Nature.*", pinterest$Answer.Q13Answera) > 0
table(pinterest$content_Nature_sA)
pinterest$content_Urban_sA <- gregexpr(".*Urban.*", pinterest$Answer.Q13Answera) > 0
table(pinterest$content_Urban_sA)
pinterest$content_Other_sA <- gregexpr(".*Food.*", pinterest$Answer.Q13Answera) > 0
table(pinterest$content_Other_sA)

pinterest$image_content_sA <- pinterest$Answer.Q13Answera
pinterest$Answer.Q13Answera <- NULL 




#-------------------------------------------------------------------------------------------------------------------------------------------
# image_contentother_sA : the text answer to 'other' from image_content_sA
#-------------------------------------------------------------------------------------------------------------------------------------------

class(pinterest$Answer.Q13Answer_other_a)
table(pinterest$Answer.Q13Answer_other_a, useNA = c("always"))
pinterest$image_contentother_sA <- pinterest$Answer.Q13Answer_other_a
pinterest$Answer.Q13Answer_other_a <- NULL 






#-------------------------------------------------------------------------------------------------------------------------------------------
# image_themes_sA : What is the image about section A
# [Health, Disease/Illness, Medicine, Mensuration, Science, Infographic, Physical fitness/Sport, Cycling, Running, 
# Yoga/Pilates, Fitness equipment, Workout plan/instructions, Fashion, Weight loss/Dieting, Food/drink, Sugar, 
# Detoxing, Nutritional supplements/Herbs, Mental health, Emotional wellbeing, Beauty/Physical appearance, Sleep, Other]
#-------------------------------------------------------------------------------------------------------------------------------------------
class(pinterest$Answer.Q11Answera)
table(pinterest$Answer.Q11Answera, useNA = c("always"))


pinterest$theme_Health_sA <- gregexpr(".*Health.*", pinterest$Answer.Q11Answera) > 0
table(pinterest$theme_Health_sA)
pinterest$theme_DiseaseIll_sA <- gregexpr(".*Disease.*", pinterest$Answer.Q11Answera) > 0
table(pinterest$theme_DiseaseIll_sA)
pinterest$theme_Medicine_sA <- gregexpr(".*Medicine.*", pinterest$Answer.Q11Answera) > 0
table(pinterest$theme_Medicine_sA)
pinterest$theme_Mensuration_sA <- gregexpr(".*Mensuration.*", pinterest$Answer.Q11Answera) > 0
table(pinterest$theme_Mensuration_sA)
pinterest$theme_Science_sA <- gregexpr(".*Science.*", pinterest$Answer.Q11Answera) > 0
table(pinterest$theme_Science_sA)
pinterest$theme_Infographic_sA <- gregexpr(".*Infographic.*", pinterest$Answer.Q11Answera) > 0
table(pinterest$theme_Infographic_sA)
pinterest$theme_PhysicalFit_sA <- gregexpr(".*Physical.*", pinterest$Answer.Q11Answera) > 0
table(pinterest$theme_PhysicalFit_sA)
pinterest$theme_Cycling_sA <- gregexpr(".*Cycling.*", pinterest$Answer.Q11Answera) > 0
table(pinterest$theme_Cycling_sA)
pinterest$theme_Running_sA <- gregexpr(".*Running.*", pinterest$Answer.Q11Answera) > 0
table(pinterest$theme_Running_sA)
pinterest$theme_Yoga_sA <- gregexpr(".*Yoga.*", pinterest$Answer.Q11Answera) > 0
table(pinterest$theme_Yoga_sA)
pinterest$theme_FitnessEqp_sA <- gregexpr(".*Fitness.*", pinterest$Answer.Q11Answera) > 0
table(pinterest$theme_FitnessEqp_sA)
pinterest$theme_WorkoutPlan_sA <- gregexpr(".*Workout.*", pinterest$Answer.Q11Answera) > 0
table(pinterest$theme_WorkoutPlan_sA)
pinterest$theme_WorkoutPlan_sA <- gregexpr(".*Workout.*", pinterest$Answer.Q11Answera) > 0
table(pinterest$theme_WorkoutPlan_sA)
pinterest$theme_Fashion_sA <- gregexpr(".*Fashion.*", pinterest$Answer.Q11Answera) > 0
table(pinterest$theme_Fashion_sA)
pinterest$theme_Weightloss_sA <- gregexpr(".*Weight.*", pinterest$Answer.Q11Answera) > 0
table(pinterest$theme_Weightloss_sA)
pinterest$theme_FoodDrink_sA <- gregexpr(".*Food.*", pinterest$Answer.Q11Answera) > 0
table(pinterest$theme_FoodDrink_sA)
pinterest$theme_Sugar_sA <- gregexpr(".*Sugar.*", pinterest$Answer.Q11Answera) > 0
table(pinterest$theme_Sugar_sA)
pinterest$theme_Detox_sA <- gregexpr(".*Detoxing.*", pinterest$Answer.Q11Answera) > 0
table(pinterest$theme_Detox_sA)
pinterest$theme_Supplement_sA <- gregexpr(".*Nutritional.*", pinterest$Answer.Q11Answera) > 0
table(pinterest$theme_Supplement_sA)
pinterest$theme_MentalHealth_sA <- gregexpr(".*Mental.*", pinterest$Answer.Q11Answera) > 0
table(pinterest$theme_MentalHealth_sA)
pinterest$theme_EmotionalWell_sA <- gregexpr(".*Emotional.*", pinterest$Answer.Q11Answera) > 0
table(pinterest$theme_EmotionalWell_sA)
pinterest$theme_Beauty_sA <- gregexpr(".*Beauty.*", pinterest$Answer.Q11Answera) > 0
table(pinterest$theme_Beauty_sA)
pinterest$theme_Sleep_sA <- gregexpr(".*Sleep.*", pinterest$Answer.Q11Answera) > 0
table(pinterest$theme_Sleep_sA)
pinterest$theme_Other_sA <- gregexpr(".*Other.*", pinterest$Answer.Q11Answera) > 0
table(pinterest$theme_Other_sA)

pinterest$image_themes_sA <- pinterest$Answer.Q11Answera
pinterest$Answer.Q11Answera <- NULL 





#-------------------------------------------------------------------------------------------------------------------------------------------
# image_themesother_sA : content of the 'other' for image_themes_sA
#-------------------------------------------------------------------------------------------------------------------------------------------
class(pinterest$Answer.Q11Answer_other_a)
table(pinterest$Answer.Q11Answer_other_a, useNA = c("always"))
pinterest$image_themesother_sA <- pinterest$Answer.Q11Answer_other_a
pinterest$Answer.Q11Answer_other_a <- NULL 




#-------------------------------------------------------------------------------------------------------------------------------------------
# image_text_sA : If there is text in the image, please type it into the field below. (If it is more than about 100 words, please 
# focus on the most important parts such as the title and the main headings.)
#-------------------------------------------------------------------------------------------------------------------------------------------

class(pinterest$Answer.Q14Answera)
pinterest$text_sA <- pinterest$Answer.Q14Answera
pinterest$Answer.Q14Answera<- NULL 



# SECTION B
#===========================================================================================================================================

#-------------------------------------------------------------------------------------------------------------------------------------------
# image_count_sB : count of the number of people in the image
#-------------------------------------------------------------------------------------------------------------------------------------------
class(pinterest$Answer.Q1Answerb)
table(pinterest$Answer.Q1Answerb, useNA = c("always"))
pinterest$image_count_sB <- recode(pinterest$Answer.Q1Answerb, '"0" = 0; "1" = 1; "2 or more" = 2; "" = NA')  
class(pinterest$image_count_sB)
table(pinterest$image_count_sB, useNA = c("always"))
recode_count <- c(None = 0, One = 1, Many = 2, NoAnswer = 3)
pinterest$image_count_sB <- factor(pinterest$image_count_sB, levels = recode_count, labels = names(recode_count))
class(pinterest$image_count_sB)
table(pinterest$image_count_sB, useNA = c("always"))
pinterest$Answer.Q1Answerb <- NULL 




#-------------------------------------------------------------------------------------------------------------------------------------------
# actualperson_sB : Is this an image of an actual person or body parts of an actual person (i.e. not a drawing)?
# [yes, no]
#-------------------------------------------------------------------------------------------------------------------------------------------

class(pinterest$Answer.Q2Answerb)
table(pinterest$Answer.Q2Answerb, useNA = c("always"))
pinterest$actualperson_sB <- recode(pinterest$Answer.Q2Answerb, '"No" = 0; "Yes" = 1; "" = NA')  
class(pinterest$actualperson_sB)
table(pinterest$actualperson_sB, useNA = c("always"))
recode_yesno <- c(No = 0, Yes = 1)
pinterest$actualperson_sB <- factor(pinterest$actualperson_sB, levels = recode_yesno, labels = names(recode_yesno))
class(pinterest$actualperson_sB)
table(pinterest$actualperson_sB, useNA = c("always"))
pinterest$Answer.Q2Answerb <- NULL 





#-------------------------------------------------------------------------------------------------------------------------------------------
# bodyparts_sB : Which parts of the body do you see? (Tick all that apply)
# [Head, Face, Upper torso (front), Upper torso (back), Stomach, Upper arm, 
# Lower arm, Hand, Bum, Hip, Upper leg, Lower leg, Feet]
#-------------------------------------------------------------------------------------------------------------------------------------------

class(pinterest$Answer.Q3Answerb)
table(pinterest$Answer.Q3Answerb, useNA = c("always"))
pinterest$bodyparts_Head_sB <- gregexpr(".*Head.*", pinterest$Answer.Q3Answerb) > 0
table(pinterest$bodyparts_Head_sB)
pinterest$bodyparts_Face_sB <- gregexpr(".*Face.*", pinterest$Answer.Q3Answerb) > 0
table(pinterest$bodyparts_Face_sB)
pinterest$bodyparts_UpperTorsoF_sB <- gregexpr(".*front.*", pinterest$Answer.Q3Answerb) > 0
table(pinterest$bodyparts_UpperTorsoF_sB)
pinterest$bodyparts_UpperTorsoB_sB <- gregexpr(".*back.*", pinterest$Answer.Q3Answerb) > 0
table(pinterest$bodyparts_UpperTorsoB_sB)
pinterest$bodyparts_Stomach_sB <- gregexpr(".*Stomach.*", pinterest$Answer.Q3Answerb) > 0
table(pinterest$bodyparts_Stomach_sB)
pinterest$bodyparts_UpperArm_sB <- gregexpr(".*Upper arm.*", pinterest$Answer.Q3Answerb) > 0
table(pinterest$bodyparts_UpperArm_sB)
pinterest$bodyparts_LowerArm_sB <- gregexpr(".*Lower arm.*", pinterest$Answer.Q3Answerb) > 0
table(pinterest$bodyparts_LowerArm_sB)
pinterest$bodyparts_Hand_sB <- gregexpr(".*Hand.*", pinterest$Answer.Q3Answerb) > 0
table(pinterest$bodyparts_Hand_sB)
pinterest$bodyparts_Bum_sB <- gregexpr(".*Bum.*", pinterest$Answer.Q3Answerb) > 0
table(pinterest$bodyparts_Bum_sB)
pinterest$bodyparts_Hip_sB <- gregexpr(".*Hip.*", pinterest$Answer.Q3Answerb) > 0
table(pinterest$bodyparts_Hip_sB)
pinterest$bodyparts_UpperLeg_sB <- gregexpr(".*Upper leg.*", pinterest$Answer.Q3Answerb) > 0
table(pinterest$bodyparts_UpperLeg_sB)
pinterest$bodyparts_LowerLeg_sB <- gregexpr(".*Lower leg.*", pinterest$Answer.Q3Answerb) > 0
table(pinterest$bodyparts_LowerLeg_sB)
pinterest$bodyparts_Feet_sB <- gregexpr(".*Feet.*", pinterest$Answer.Q3Answerb) > 0
table(pinterest$bodyparts_Feet_sB)


pinterest$bodyparts_sB <- pinterest$Answer.Q3Answerb
pinterest$Answer.Q3Answerb <- NULL 





#-------------------------------------------------------------------------------------------------------------------------------------------
# bodyparts_naked_sB : Which parts of the body do you see naked? (Tick all that apply)
# [Head, Face, Upper torso (front), Upper torso (back), Stomach, Upper arm, 
# Lower arm, Hand, Bum, Hip, Upper leg, Lower leg, Feet]
#-------------------------------------------------------------------------------------------------------------------------------------------

class(pinterest$Answer.Q4Answerb)
table(pinterest$Answer.Q4Answerb, useNA = c("always"))
pinterest$bodyparts_naked_Head_sB <- gregexpr(".*Head.*", pinterest$Answer.Q4Answerb) > 0
table(pinterest$bodyparts_naked_Head_sB)
pinterest$bodyparts_naked_Face_sB <- gregexpr(".*Face.*", pinterest$Answer.Q4Answerb) > 0
table(pinterest$bodyparts_naked_Face_sB)
pinterest$bodyparts_naked_UpperTorsoF_sB <- gregexpr(".*front.*", pinterest$Answer.Q4Answerb) > 0
table(pinterest$bodyparts_naked_UpperTorsoF_sB)
pinterest$bodyparts_naked_UpperTorsoB_sB <- gregexpr(".*back.*", pinterest$Answer.Q4Answerb) > 0
table(pinterest$bodyparts_naked_UpperTorsoB_sB)
pinterest$bodyparts_naked_Stomach_sB <- gregexpr(".*Stomach.*", pinterest$Answer.Q4Answerb) > 0
table(pinterest$bodyparts_naked_Stomach_sB)
pinterest$bodyparts_naked_UpperArm_sB <- gregexpr(".*Upper arm.*", pinterest$Answer.Q4Answerb) > 0
table(pinterest$bodyparts_naked_UpperArm_sB)
pinterest$bodyparts_naked_LowerArm_sB <- gregexpr(".*Lower arm.*", pinterest$Answer.Q4Answerb) > 0
table(pinterest$bodyparts_naked_LowerArm_sB)
pinterest$bodyparts_naked_Hand_sB <- gregexpr(".*Hand.*", pinterest$Answer.Q4Answerb) > 0
table(pinterest$bodyparts_naked_Hand_sB)
pinterest$bodyparts_naked_Bum_sB <- gregexpr(".*Bum.*", pinterest$Answer.Q4Answerb) > 0
table(pinterest$bodyparts_naked_Bum_sB)
pinterest$bodyparts_naked_Hip_sB <- gregexpr(".*Hip.*", pinterest$Answer.Q4Answerb) > 0
table(pinterest$bodyparts_naked_Hip_sB)
pinterest$bodyparts_naked_UpperLeg_sB <- gregexpr(".*Upper leg.*", pinterest$Answer.Q4Answerb) > 0
table(pinterest$bodyparts_naked_UpperLeg_sB)
pinterest$bodyparts_naked_LowerLeg_sB <- gregexpr(".*Lower leg.*", pinterest$Answer.Q4Answerb) > 0
table(pinterest$bodyparts_naked_LowerLeg_sB)
pinterest$bodyparts_naked_Feet_sB <- gregexpr(".*Feet.*", pinterest$Answer.Q4Answerb) > 0
table(pinterest$bodyparts_naked_Feet_sB)


pinterest$bodyparts_naked_sB <- pinterest$Answer.Q4Answerb
pinterest$Answer.Q4Answerb <- NULL 




#-------------------------------------------------------------------------------------------------------------------------------------------
# race_sB : What is the race of this person? Please take your best guess. (Tick only one)
# [White/Non-Hispanic, Black/Non-hispanic, Hispanic, Asian, Other, Don’t want to answer]
#-------------------------------------------------------------------------------------------------------------------------------------------

class(pinterest$Answer.Q5Answerb)
table(pinterest$Answer.Q5Answerb, useNA = c("always"))
pinterest$race_sB <- recode(pinterest$Answer.Q5Answerb, '"White / Non-Hispanic" = 1; "Black / Non-Hispanic" = 2; "Hispanic" = 3; "Asian" = 4; "Other" = 5; "Don\'t know" = 6; "" = NA')
class(pinterest$race_sB)
table(pinterest$race_sB, useNA = c("always"))
recode_race <- c(WhiteNonHispanic = 1, BlackNonHispanic = 2, Hispanic = 3, Asian = 4, Other = 5, Refuse = 6)
pinterest$race_sB <- factor(pinterest$race_sB, levels = recode_race, labels = names(recode_race), exclude = NULL)
class(pinterest$race_sB)
table(pinterest$race_sB, useNA = c("always"))
pinterest$Answer.Q5Answerb <- NULL 





#-------------------------------------------------------------------------------------------------------------------------------------------
# female_sB : What is the gender of this person? Please take your best guess. (Tick only one)
# [Female, Male, Don’t know]
#-------------------------------------------------------------------------------------------------------------------------------------------
class(pinterest$Answer.Q6Answerb )
table(pinterest$Answer.Q6Answerb , useNA = c("always"))
pinterest$female_sB  <- recode(pinterest$Answer.Q6Answerb , '"Male" = 0; "Female" = 1; "Don\'t know" = 2; "" = NA')
class(pinterest$female_sB)
table(pinterest$female_sB, useNA = c("always"))
recode <- c(Male = 0, Female = 1, DK = 2)
pinterest$female_sB <- factor(pinterest$female_sB, levels = recode, labels = names(recode), exclude = NULL)
class(pinterest$female_sB)
table(pinterest$female_sB, useNA = c("always"))
pinterest$Answer.Q6Answerb <- NULL 






#-------------------------------------------------------------------------------------------------------------------------------------------
# age_sB : What is the age of this person? Please take your best guess. (Tick only one)
# [Younger than 18, 18-29, 30-49, 50 or older, Don’t know]
#-------------------------------------------------------------------------------------------------------------------------------------------
class(pinterest$Answer.Q7Answerb)
table(pinterest$Answer.Q7Answerb, useNA = c("always"))
pinterest$age_sB  <- recode(pinterest$Answer.Q7Answerb , '"Younger than 18" = 1; "18 to 29" = 2; "30 to 49" = 3; "50 or older" = 4; "Don\'t know" = 5; "" = NA')
class(pinterest$age_sB)
table(pinterest$age_sB, useNA = c("always"))
recode <- c("Younger than 18" = 1, "18 to 29" = 2, "30 to 49" = 3, "50 or older" = 4, "Don\'t know" = 5)
pinterest$age_sB <- factor(pinterest$age_sB, levels = recode, labels = names(recode), exclude = NULL)
class(pinterest$age_sB)
table(pinterest$age_sB, useNA = c("always"))
pinterest$Answer.Q7Answerb <- NULL 




#-------------------------------------------------------------------------------------------------------------------------------------------
# emotion_sB : How does this person appear to feel? (Tick only one)
# [Happy, Neither happy nor sad, Sad, Don’t know]
#-------------------------------------------------------------------------------------------------------------------------------------------
class(pinterest$Answer.Q8Answerb)
table(pinterest$Answer.Q8Answerb, useNA = c("always"))
pinterest$emotion_sB  <- recode(pinterest$Answer.Q8Answerb, '"Happy" = 1; "Neither happy nor sad" = 2; "Sad" = 3; "Don\'t know" = 4; "" = NA')
class(pinterest$emotion_sB)
table(pinterest$emotion_sB, useNA = c("always"))
recode <- c("Happy" = 1, "Neither happy nor sad" = 2, "Sad" = 3, "Don\'t know" = 4)
pinterest$emotion_sB <- factor(pinterest$emotion_sB, levels = recode, labels = names(recode), exclude = NULL)
class(pinterest$emotion_sB)
table(pinterest$emotion_sB, useNA = c("always"))
pinterest$Answer.Q8Answerb <- NULL 





#-------------------------------------------------------------------------------------------------------------------------------------------
# health_sB : How health do you think this person is? (Tick only one)
# [Very unhealthy, Somewhat unhealthy, Average, Somewhat healthy, Very healthy]
#-------------------------------------------------------------------------------------------------------------------------------------------
class(pinterest$Answer.Q15Answerb)
table(pinterest$Answer.Q15Answerb, useNA = c("always"))
pinterest$health_sB  <- recode(pinterest$Answer.Q15Answerb, '"Very unhealthy" = 1; "Somewhat unhealthy" = 2; "Average" = 3; "Somewhat healthy" = 4; "Very healthy" = 5; "" = NA')
class(pinterest$health_sB)
table(pinterest$health_sB, useNA = c("always"))
recode <- c("Very unhealthy" = 1, "Somewhat unhealthy" = 2, "Average" = 3, "Somewhat healthy" = 4, "Very healthy" = 5)
pinterest$health_sB <- factor(pinterest$health_sB, levels = recode, labels = names(recode), exclude = NULL)
class(pinterest$health_sB)
table(pinterest$health_sB, useNA = c("always"))
pinterest$Answer.Q15Answerb <- NULL 






#-------------------------------------------------------------------------------------------------------------------------------------------
# attractive_sB : Do you find this person physically attractive? (Tick only one)
# [Not at all, Somewhat, Very much]
#-------------------------------------------------------------------------------------------------------------------------------------------
class(pinterest$Answer.Q25Answerb)
table(pinterest$Answer.Q25Answerb, useNA = c("always"))
pinterest$attractive_sB  <- recode(pinterest$Answer.Q25Answerb, '"Not at all" = 1; "Somewhat" = 2; "Very much" = 3; "" = NA')
class(pinterest$attractive_sB)
table(pinterest$attractive_sB, useNA = c("always"))
recode <- c("Not at all" = 1, "Somewhat" = 2, "Very" = 3)
pinterest$attractive_sB <- factor(pinterest$attractive_sB, levels = recode, labels = names(recode), exclude = NULL)
class(pinterest$attractive_sB)
table(pinterest$attractive_sB, useNA = c("always"))
pinterest$Answer.Q25Answerb <- NULL 




#-------------------------------------------------------------------------------------------------------------------------------------------
# sexually_sB : Do you find this person sexually attractive? (Tick only one)
# [Not at all, Somewhat, Very much]
#-------------------------------------------------------------------------------------------------------------------------------------------
class(pinterest$Answer.Q26Answerb)
table(pinterest$Answer.Q26Answerb, useNA = c("always"))
pinterest$sexually_sB  <- recode(pinterest$Answer.Q26Answerb, '"Not at all" = 1; "Somewhat" = 2; "Very much" = 3; "" = NA')
class(pinterest$sexually_sB)
table(pinterest$sexually_sB, useNA = c("always"))
recode <- c("Not at all" = 1, "Somewhat" = 2, "Very" = 3)
pinterest$sexually_sB <- factor(pinterest$sexually_sB, levels = recode, labels = names(recode), exclude = NULL)
class(pinterest$sexually_sB)
table(pinterest$sexually_sB, useNA = c("always"))
pinterest$Answer.Q26Answerb <- NULL





#-------------------------------------------------------------------------------------------------------------------------------------------
# sexualized_sB : Does this image portray the person in a sexualized manner? (Tick only one)
# [Not at all, Somewhat, Very much]
#-------------------------------------------------------------------------------------------------------------------------------------------
class(pinterest$Answer.Q19Answerb)
table(pinterest$Answer.Q19Answerb, useNA = c("always"))
pinterest$sexualized_sB  <- recode(pinterest$Answer.Q19Answerb, '"Not at all" = 1; "Somewhat" = 2; "Very much" = 3; "" = NA')
class(pinterest$sexualized_sB)
table(pinterest$sexualized_sB, useNA = c("always"))
recode <- c("Not at all" = 1, "Somewhat" = 2, "Very" = 3)
pinterest$sexualized_sB <- factor(pinterest$sexualized_sB, levels = recode, labels = names(recode), exclude = NULL)
class(pinterest$sexualized_sB)
table(pinterest$sexualized_sB, useNA = c("always"))
pinterest$Answer.Q19Answerb <- NULL






#-------------------------------------------------------------------------------------------------------------------------------------------
# physcondition_sB : How would you describe the physical condition of this person? (Tick only one)
# [In-shape/athletic, Average, Out of shape, Don’t know]
#-------------------------------------------------------------------------------------------------------------------------------------------
class(pinterest$Answer.Q9Answerb)
table(pinterest$Answer.Q9Answerb, useNA = c("always"))
pinterest$physcondition_sB  <- recode(pinterest$Answer.Q9Answerb, '"In-shape/athletic" = 1; "Average" = 2; "Out of shape" = 3; "Don\'t know" = 4; "" = NA')
class(pinterest$physcondition_sB)
table(pinterest$physcondition_sB, useNA = c("always"))
recode <- c("In-shape/athletic" = 1, "Average" = 2, "Out of shape" = 3, "Don\'t know" = 4)
pinterest$physcondition_sB <- factor(pinterest$physcondition_sB, levels = recode, labels = names(recode), exclude = NULL)
class(pinterest$physcondition_sB)
table(pinterest$physcondition_sB, useNA = c("always"))
pinterest$Answer.Q9Answerb <- NULL









#-------------------------------------------------------------------------------------------------------------------------------------------
# bodytype_sB: Which of the following sketches comes closest to the body shape of this person? (Tick only one)
# [A, B, C, D, E]
#-------------------------------------------------------------------------------------------------------------------------------------------
class(pinterest$Answer.Q10Answerb)
table(pinterest$Answer.Q10Answerb, useNA = c("always"))
pinterest$bodytype_sB  <- recode(pinterest$Answer.Q10Answerb, '"A" = 1; "B" = 2; "C" = 3; "D" = 4; "E" = 5; "" = NA')
class(pinterest$bodytype_sB)
table(pinterest$bodytype_sB, useNA = c("always"))
recode <- c("A" = 1, "B" = 2, "C" = 3, "D" = 4, "E" = 5)
pinterest$bodytype_sB <- factor(pinterest$bodytype_sB, levels = recode, labels = names(recode), exclude = NULL)
class(pinterest$bodytype_sB)
table(pinterest$bodytype_sB, useNA = c("always"))
pinterest$Answer.Q10Answerb <- NULL




#-------------------------------------------------------------------------------------------------------------------------------------------
# image_background_sB : image background section A
# [indoors, outdoors, image shows artificial background, don’t know]
#-------------------------------------------------------------------------------------------------------------------------------------------
class(pinterest$Answer.Q12Answerb)
table(pinterest$Answer.Q12Answerb, useNA = c("always"))
pinterest$image_background_sB <- recode(pinterest$Answer.Q12Answerb, '"Indoors" = 1; "Outdoors" = 2; "Artificial" = 3; "Don\'t know" = 4; "" = NA')  
class(pinterest$image_background_sB)
table(pinterest$image_background_sB, useNA = c("always"))
recode_background <- c(Indoors = 1, Outdoors = 2, Artificial = 3, DontKnow = 4)
pinterest$image_background_sB <- factor(pinterest$image_background_sB, levels = recode_background, labels = names(recode_background))
class(pinterest$image_background_sB)
table(pinterest$image_background_sB, useNA = c("always"))
pinterest$Answer.Q12Answerb <- NULL 






#-------------------------------------------------------------------------------------------------------------------------------------------
# image_background_sB : What, other than the person, do you see in the image? (Tick all that apply)
# [Food/Drink, Nutritional supplements, Fitness Equipment, Clothes, Nature scene, Urban scene, Other]
#-------------------------------------------------------------------------------------------------------------------------------------------
class(pinterest$Answer.Q13Answerb)
table(pinterest$Answer.Q13Answerb, useNA = c("always"))


pinterest$content_FoodDrink_sB <- gregexpr(".*Food.*", pinterest$Answer.Q13Answerb) > 0
table(pinterest$content_FoodDrink_sB)
pinterest$content_Supplement_sB <- gregexpr(".*Supplement.*", pinterest$Answer.Q13Answerb) > 0
table(pinterest$content_FoodDrink_sB)
pinterest$content_FitnessEqp_sB <- gregexpr(".*Fitness.*", pinterest$Answer.Q13Answerb) > 0
table(pinterest$content_FitnessEqp_sB)
pinterest$content_Clothes_sB <- gregexpr(".*Clothes.*", pinterest$Answer.Q13Answerb) > 0
table(pinterest$content_Clothes_sB)
pinterest$content_Nature_sB <- gregexpr(".*Nature.*", pinterest$Answer.Q13Answerb) > 0
table(pinterest$content_Nature_sB)
pinterest$content_Urban_sB <- gregexpr(".*Urban.*", pinterest$Answer.Q13Answerb) > 0
table(pinterest$content_Urban_sB)
pinterest$content_Other_sB <- gregexpr(".*Food.*", pinterest$Answer.Q13Answerb) > 0
table(pinterest$content_Other_sB)

pinterest$image_content_sB <- pinterest$Answer.Q13Answerb
pinterest$Answer.Q13Answerb <- NULL 





#-------------------------------------------------------------------------------------------------------------------------------------------
# image_contentother_sB : the text answer to 'other' from image_content_sA
#-------------------------------------------------------------------------------------------------------------------------------------------

class(pinterest$Answer.Q13Answer_other_b)
table(pinterest$Answer.Q13Answer_other_b, useNA = c("always"))

pinterest$image_contentother_sB <- pinterest$Answer.Q13Answer_other_b
pinterest$Answer.Q13Answer_other_b <- NULL 




#-------------------------------------------------------------------------------------------------------------------------------------------
# image_text_sB : If there is text in the image, please type it into the field below. (If it is more than about 100 words, please 
# focus on the most important parts such as the title and the main headings.)
#-------------------------------------------------------------------------------------------------------------------------------------------

class(pinterest$Answer.Q14Answerb )
pinterest$text_sB <- pinterest$Answer.Q14Answerb
pinterest$Answer.Q14Answerb <- NULL 




#-------------------------------------------------------------------------------------------------------------------------------------------
# image_themes_sB : What is the image about section A
# [Health, Disease/Illness, Medicine, Mensuration, Science, Infographic, Physical fitness/Sport, Cycling, Running, 
# Yoga/Pilates, Fitness equipment, Workout plan/instructions, Fashion, Weight loss/Dieting, Food/drink, Sugar, 
# Detoxing, Nutritional supplements/Herbs, Mental health, Emotional wellbing, Beauty/Physical appearance, Sleep, Other]
#-------------------------------------------------------------------------------------------------------------------------------------------
class(pinterest$Answer.Q11Answerb)
table(pinterest$Answer.Q11Answerb, useNA = c("always"))

pinterest$theme_Health_sB <- gregexpr(".*Health.*", pinterest$Answer.Q11Answerb) > 0
table(pinterest$theme_Health_sB)
pinterest$theme_DiseaseIll_sB <- gregexpr(".*Disease.*", pinterest$Answer.Q11Answerb) > 0
table(pinterest$theme_DiseaseIll_sB)
pinterest$theme_Medicine_sB <- gregexpr(".*Medicine.*", pinterest$Answer.Q11Answerb) > 0
table(pinterest$theme_Medicine_sB)
pinterest$theme_Mensuration_sB <- gregexpr(".*Mensuration.*", pinterest$Answer.Q11Answerb) > 0
table(pinterest$theme_Mensuration_sB)
pinterest$theme_Science_sB <- gregexpr(".*Science.*", pinterest$Answer.Q11Answerb) > 0
table(pinterest$theme_Science_sB)
pinterest$theme_Infographic_sB <- gregexpr(".*Infographic.*", pinterest$Answer.Q11Answerb) > 0
table(pinterest$theme_Infographic_sB)
pinterest$theme_PhysicalFit_sB <- gregexpr(".*Physical.*", pinterest$Answer.Q11Answerb) > 0
table(pinterest$theme_PhysicalFit_sB)
pinterest$theme_Cycling_sB <- gregexpr(".*Cycling.*", pinterest$Answer.Q11Answerb) > 0
table(pinterest$theme_Cycling_sB)
pinterest$theme_Running_sB <- gregexpr(".*Running.*", pinterest$Answer.Q11Answerb) > 0
table(pinterest$theme_Running_sB)
pinterest$theme_Yoga_sB <- gregexpr(".*Yoga.*", pinterest$Answer.Q11Answerb) > 0
table(pinterest$theme_Yoga_sB)
pinterest$theme_FitnessEqp_sB <- gregexpr(".*Fitness.*", pinterest$Answer.Q11Answerb) > 0
table(pinterest$theme_FitnessEqp_sB)
pinterest$theme_WorkoutPlan_sB <- gregexpr(".*Workout.*", pinterest$Answer.Q11Answerb) > 0
table(pinterest$theme_WorkoutPlan_sB)
pinterest$theme_WorkoutPlan_sA <- gregexpr(".*Workout.*", pinterest$Answer.Q11Answerb) > 0
table(pinterest$theme_WorkoutPlan_sA)
pinterest$theme_Fashion_sB <- gregexpr(".*Fashion.*", pinterest$Answer.Q11Answerb) > 0
table(pinterest$theme_Fashion_sB)
pinterest$theme_Weightloss_sB <- gregexpr(".*Weight.*", pinterest$Answer.Q11Answerb) > 0
table(pinterest$theme_Weightloss_sB)
pinterest$theme_FoodDrink_sB <- gregexpr(".*Food.*", pinterest$Answer.Q11Answerb) > 0
table(pinterest$theme_FoodDrink_sB)
pinterest$theme_Sugar_sB <- gregexpr(".*Sugar.*", pinterest$Answer.Q11Answerb) > 0
table(pinterest$theme_Sugar_sB)
pinterest$theme_Detox_sB <- gregexpr(".*Detoxing.*", pinterest$Answer.Q11Answerb) > 0
table(pinterest$theme_Detox_sB)
pinterest$theme_Supplement_sB <- gregexpr(".*Nutritional.*", pinterest$Answer.Q11Answerb) > 0
table(pinterest$theme_Supplement_sB)
pinterest$theme_MentalHealth_sB <- gregexpr(".*Mental.*", pinterest$Answer.Q11Answerb) > 0
table(pinterest$theme_MentalHealth_sB)
pinterest$theme_EmotionalWell_sB <- gregexpr(".*Emotional.*", pinterest$Answer.Q11Answerb) > 0
table(pinterest$theme_EmotionalWell_sB)
pinterest$theme_Beauty_sB <- gregexpr(".*Beauty.*", pinterest$Answer.Q11Answerb) > 0
table(pinterest$theme_Beauty_sB)
pinterest$theme_Sleep_sB <- gregexpr(".*Sleep.*", pinterest$Answer.Q11Answerb) > 0
table(pinterest$theme_Sleep_sB)
pinterest$theme_Other_sB <- gregexpr(".*Other.*", pinterest$Answer.Q11Answerb) > 0
table(pinterest$theme_Other_sB)

pinterest$image_themes_sB <- pinterest$Answer.Q11Answerb
pinterest$Answer.Q11Answerb <- NULL 




#-------------------------------------------------------------------------------------------------------------------------------------------
# image_themesother_sB : content of the 'other' for image_themes_sB
#-------------------------------------------------------------------------------------------------------------------------------------------
class(pinterest$Answer.Q11Answer_other_b)
table(pinterest$Answer.Q11Answer_other_b, useNA = c("always"))

pinterest$image_themesother_sB <- pinterest$Answer.Q11Answer_other_b
pinterest$Answer.Q11Answer_other_b <- NULL 





#-------------------------------------------------------------------------------------------------------------------------------------------
# improve_health_sB : Is this image about improving health? (Tick only one)
# [Not at all, Somewhat, Very much]
#-------------------------------------------------------------------------------------------------------------------------------------------
class(pinterest$Answer.Q16Answerb)
table(pinterest$Answer.Q16Answerb, useNA = c("always"))
pinterest$improve_health_sB  <- recode(pinterest$Answer.Q16Answerb, '"Not at all" = 1; "Somewhat" = 2; "Very much" = 3; "" = NA')
class(pinterest$improve_health_sB)
table(pinterest$improve_health_sB, useNA = c("always"))
recode <- c("Not at all" = 1, "Somewhat" = 2, "Very" = 3)
pinterest$improve_health_sB <- factor(pinterest$improve_health_sB, levels = recode, labels = names(recode), exclude = NULL)
class(pinterest$improve_health_sB)
table(pinterest$improve_health_sB, useNA = c("always"))
pinterest$Answer.Q16Answerb <- NULL 




#-------------------------------------------------------------------------------------------------------------------------------------------
# improve_fitness_sB : Is this image about improving health? (Tick only one)
# [Not at all, Somewhat, Very much]
#-------------------------------------------------------------------------------------------------------------------------------------------
class(pinterest$Answer.Q17Answerb)
table(pinterest$Answer.Q17Answerb, useNA = c("always"))
pinterest$improve_fitness_sB  <- recode(pinterest$Answer.Q17Answerb, '"Not at all" = 1; "Somewhat" = 2; "Very much" = 3; "" = NA')
class(pinterest$improve_fitness_sB)
table(pinterest$improve_fitness_sB, useNA = c("always"))
recode <- c("Not at all" = 1, "Somewhat" = 2, "Very" = 3)
pinterest$improve_fitness_sB <- factor(pinterest$improve_fitness_sB, levels = recode, labels = names(recode), exclude = NULL)
class(pinterest$improve_fitness_sB)
table(pinterest$improve_fitness_sB, useNA = c("always"))
pinterest$Answer.Q17Answerb <- NULL 






#-------------------------------------------------------------------------------------------------------------------------------------------
# improve_attractive_sB : Is this image about improving physical attractiveness? (Tick only one)
# [Not at all, Somewhat, Very much]
#-------------------------------------------------------------------------------------------------------------------------------------------
class(pinterest$Answer.Q18Answerb)
table(pinterest$Answer.Q18Answerb, useNA = c("always"))
pinterest$improve_attractive_sB  <- recode(pinterest$Answer.Q18Answerb, '"Not at all" = 1; "Somewhat" = 2; "Very much" = 3; "" = NA')
class(pinterest$improve_attractive_sB)
table(pinterest$improve_attractive_sB, useNA = c("always"))
recode <- c("Not at all" = 1, "Somewhat" = 2, "Very" = 3)
pinterest$improve_attractive_sB <- factor(pinterest$improve_attractive_sB, levels = recode, labels = names(recode), exclude = NULL)
class(pinterest$improve_attractive_sB)
table(pinterest$improve_attractive_sB, useNA = c("always"))
pinterest$Answer.Q18Answerb <- NULL 





#-------------------------------------------------------------------------------------------------------------------------------------------
# cure_sB : Is this image about curing health problems/illness? (Tick only one)
# [Yes, No, Don’t know]
#-------------------------------------------------------------------------------------------------------------------------------------------
class(pinterest$Answer.Q20Answerb)
table(pinterest$Answer.Q20Answerb, useNA = c("always"))
pinterest$cure_sB  <- recode(pinterest$Answer.Q20Answerb, '"No" = 0; "Yes" = 1; "Don\'t know" = 2; "" = NA')
class(pinterest$cure_sB)
table(pinterest$cure_sB, useNA = c("always"))
recode <- c("No" = 0, "Yes" = 1, "Don\'t know" = 2)
pinterest$cure_sB <- factor(pinterest$cure_sB, levels = recode, labels = names(recode), exclude = NULL)
class(pinterest$cure_sB)
table(pinterest$cure_sB, useNA = c("always"))
pinterest$Answer.Q20Answerb <- NULL 







#-------------------------------------------------------------------------------------------------------------------------------------------
# prevent_sB : Is this image about preventing health problems/illness? (Tick only one)
# [Yes, No, Don’t know]
#-------------------------------------------------------------------------------------------------------------------------------------------
class(pinterest$Answer.Q21Answerb)
table(pinterest$Answer.Q21Answerb, useNA = c("always"))
pinterest$prevent_sB  <- recode(pinterest$Answer.Q21Answerb, '"No" = 0; "Yes" = 1; "Don\'t know" = 2; "" = NA')
class(pinterest$prevent_sB)
table(pinterest$prevent_sB, useNA = c("always"))
recode <- c("No" = 0, "Yes" = 1, "Don\'t know" = 2)
pinterest$prevent_sB <- factor(pinterest$prevent_sB, levels = recode, labels = names(recode), exclude = NULL)
class(pinterest$prevent_sB)
table(pinterest$prevent_sB, useNA = c("always"))
pinterest$Answer.Q21Answerb <- NULL 






# SECTION C
#===========================================================================================================================================

#-------------------------------------------------------------------------------------------------------------------------------------------
# image_count_sC : count of the number of people in the image
#-------------------------------------------------------------------------------------------------------------------------------------------
class(pinterest$Answer.Q1Answerc)
table(pinterest$Answer.Q1Answerc, useNA = c("always"))
pinterest$image_count_sC <- recode(pinterest$Answer.Q1Answerc, '"0" = 0; "1" = 1; "2 or more" = 2; "" = NA')  
class(pinterest$image_count_sC)
table(pinterest$image_count_sC, useNA = c("always"))
recode_count <- c(None = 0, One = 1, Many = 2, NoAnswer = 3)
pinterest$image_count_sC <- factor(pinterest$image_count_sC, levels = recode_count, labels = names(recode_count))
class(pinterest$image_count_sC)
table(pinterest$image_count_sC, useNA = c("always"))
pinterest$Answer.Q1Answerc <- NULL 


#-------------------------------------------------------------------------------------------------------------------------------------------
# actualperson_sC : Is this an image of actual people or body parts of actual people (i.e. not a drawing)?
# [yes, no]
#-------------------------------------------------------------------------------------------------------------------------------------------

class(pinterest$Answer.Q2Answerc)
table(pinterest$Answer.Q2Answerc, useNA = c("always"))
pinterest$actualperson_sC <- recode(pinterest$Answer.Q2Answerc, '"No" = 0; "Yes" = 1; "" = NA')  
class(pinterest$actualperson_sC)
table(pinterest$actualperson_sC, useNA = c("always"))
recode_yesno <- c(No = 0, Yes = 1)
pinterest$actualperson_sC <- factor(pinterest$actualperson_sC, levels = recode_yesno, labels = names(recode_yesno))
class(pinterest$actualperson_sC)
table(pinterest$actualperson_sC, useNA = c("always"))
pinterest$Answer.Q2Answerc <- NULL 



#-------------------------------------------------------------------------------------------------------------------------------------------
# people_number_sC : How many people are in this image? (Tick only one)
# [2, 3, 4, 5, 6, 7, 8, 9, 10, 11 or more]
#-------------------------------------------------------------------------------------------------------------------------------------------

class(pinterest$Answer.Q22Answerc)
table(pinterest$Answer.Q22Answerc, useNA = c("always"))
pinterest$people_number_sC <- recode(pinterest$Answer.Q22Answerc, '"2" = 2; "3" = 3; "4" = 4; "5" = 5; "6" = 6; "7" = 7; "8" = 8; "9" = 9; "10" = 10; "11 or more" = 11; "" = NA')  
class(pinterest$people_number_sC)
table(pinterest$people_number_sC, useNA = c("always"))
recode_count <- c("2" = 2, "3" = 3, "4" = 4, "5" = 5, "6" = 6, "7" = 7, "8" = 8, "9" = 9, "10" = 10, "11 or more" = 11)
pinterest$people_number_sC <- factor(pinterest$people_number_sC, levels = recode_count, labels = names(recode_count))
class(pinterest$people_number_sC)
table(pinterest$people_number_sC, useNA = c("always"))
pinterest$Answer.Q22Answerc <- NULL 



#-------------------------------------------------------------------------------------------------------------------------------------------
# race_sC : What is the race of these people? Please take your best guess. (Tick only one)
# [ALL White/Non-Hispanic,  ALL Black/Non-hispanic, ALL Hispanic, ALL Asian, ALL Other, Mixed, Don’t know]
#-------------------------------------------------------------------------------------------------------------------------------------------

class(pinterest$Answer.Q5Answerc )
table(pinterest$Answer.Q5Answerc , useNA = c("always"))
pinterest$race_sC <- recode(pinterest$Answer.Q5Answerc , '"ALL White/Non-Hispanic" = 1; "ALL Black/Non-Hispanic" = 2; "ALL Hispanic" = 3; "ALL Asian" = 4; "ALL Other" = 5; "Mixed" = 6; "Don\'t know" = 7; "" = NA')
class(pinterest$race_sC)
table(pinterest$race_sC, useNA = c("always"))
recode_race <- c(ALLWhiteNonHispanic = 1, ALLBlackNonHispanic = 2, ALLHispanic = 3, ALLAsian = 4, ALLOther = 5, Mixed = 6, DontKnow = 7)
pinterest$race_sC <- factor(pinterest$race_sC, levels = recode_race, labels = names(recode_race), exclude = NULL)
class(pinterest$race_sC)
table(pinterest$race_sC, useNA = c("always"))
pinterest$Answer.Q5Answerc  <- NULL 




#-------------------------------------------------------------------------------------------------------------------------------------------
#female_sC : What is the gender of these people? (Tick only one)
# [ALL female, ALL male, Mixed, Don’t know]
#-------------------------------------------------------------------------------------------------------------------------------------------

class(pinterest$Answer.Q6Answerc)
table(pinterest$Answer.Q6Answerc, useNA = c("always"))
pinterest$female_sC <- recode(pinterest$Answer.Q6Answerc, '"Male" = 0; "Female" = 1; "Mixed" = 3; "Don\'t know" = 4; "" = NA')
class(pinterest$female_sC)
table(pinterest$female_sC, useNA = c("always"))
recode <- c(Male = 0, Female = 1, Mixed = 3, DontKnow = 4)
pinterest$female_sC <- factor(pinterest$female_sC, levels = recode, labels = names(recode), exclude = NULL)
class(pinterest$female_sC)
table(pinterest$female_sC, useNA = c("always"))
pinterest$Answer.Q6Answerc <- NULL 



#-------------------------------------------------------------------------------------------------------------------------------------------
# emotion_sC : How does these people appear to feel? (Tick only one)
# [Happy, Neither happy nor sad, Sad, Don’t know]
#-------------------------------------------------------------------------------------------------------------------------------------------
class(pinterest$Answer.Q8Answerc)
table(pinterest$Answer.Q8Answerc, useNA = c("always"))
pinterest$emotion_sC  <- recode(pinterest$Answer.Q8Answerc, '"Happy" = 1; "Neither happy nor sad" = 2; "Sad" = 3; "Don\'t know" = 4; "" = NA')
class(pinterest$emotion_sC)
table(pinterest$emotion_sC, useNA = c("always"))
recode <- c("Happy" = 1, "Neither happy nor sad" = 2, "Sad" = 3, "Don\'t know" = 4)
pinterest$emotion_sC <- factor(pinterest$emotion_sC, levels = recode, labels = names(recode), exclude = NULL)
class(pinterest$emotion_sC)
table(pinterest$emotion_sC, useNA = c("always"))
pinterest$Answer.Q8Answerc <- NULL 



#!!!!
#-------------------------------------------------------------------------------------------------------------------------------------------
# interaction_sC : Are these people interacting with each other? (Tick only one)
#[Yes, No, Don’t know]
#-------------------------------------------------------------------------------------------------------------------------------------------

class(pinterest$Q23Answerc)
#table(pinterest$, useNA = c("always"))
#pinterest$actualperson_sC <- recode(pinterest$Answer.Q2Answerc, '"No" = 0; "Yes" = 1; "" = NA')  
#class(pinterest$actualperson_sC)
#table(pinterest$actualperson_sC, useNA = c("always"))
#recode_yesno <- c(No = 0, Yes = 1)
#pinterest$actualperson_sC <- factor(pinterest$actualperson_sC, levels = recode_yesno, labels = names(recode_yesno))
#class(pinterest$actualperson_sC)
#table(pinterest$actualperson_sC, useNA = c("always"))
#pinterest$Answer.Q2Answerc <- NULL 




#-------------------------------------------------------------------------------------------------------------------------------------------
# image_background_sC : image background section A
# [indoors, outdoors, image shows artificial background, don’t know]
#-------------------------------------------------------------------------------------------------------------------------------------------
class(pinterest$Answer.Q12Answerc)
table(pinterest$Answer.Q12Answerc, useNA = c("always"))
pinterest$image_background_sC <- recode(pinterest$Answer.Q12Answerc, '"Indoors" = 1; "Outdoors" = 2; "Artificial" = 3; "Don\'t know" = 4; "" = NA')  
class(pinterest$image_background_sC)
table(pinterest$image_background_sC, useNA = c("always"))
recode_background <- c(Indoors = 1, Outdoors = 2, Artificial = 3, DontKnow = 4)
pinterest$image_background_sC <- factor(pinterest$image_background_sC, levels = recode_background, labels = names(recode_background))
class(pinterest$image_background_sC)
table(pinterest$image_background_sC, useNA = c("always"))
pinterest$Answer.Q12Answerc <- NULL 



#-------------------------------------------------------------------------------------------------------------------------------------------
# image_background_sC : What, other than the person, do you see in the image? (Tick all that apply)
# [Food/Drink, Nutritional supplements, Fitness Equipment, Clothes, Nature scene, Urban scene, Other]
#-------------------------------------------------------------------------------------------------------------------------------------------
class(pinterest$Answer.Q13Answerc)
table(pinterest$Answer.Q13Answerc, useNA = c("always"))


pinterest$content_FoodDrink_sC <- gregexpr(".*Food.*", pinterest$Answer.Q13Answerc) > 0
table(pinterest$content_FoodDrink_sC)
pinterest$content_Supplement_sC <- gregexpr(".*Supplement.*", pinterest$Answer.Q13Answerc) > 0
table(pinterest$content_FoodDrink_sC)
pinterest$content_FitnessEqp_sC <- gregexpr(".*Fitness.*", pinterest$Answer.Q13Answerc) > 0
table(pinterest$content_FitnessEqp_sC)
pinterest$content_Clothes_sC <- gregexpr(".*Clothes.*", pinterest$Answer.Q13Answerc) > 0
table(pinterest$content_Clothes_sB)
pinterest$content_Nature_sC <- gregexpr(".*Nature.*", pinterest$Answer.Q13Answerc) > 0
table(pinterest$content_Nature_sC)
pinterest$content_Urban_sC <- gregexpr(".*Urban.*", pinterest$Answer.Q13Answerc) > 0
table(pinterest$content_Urban_sC)
pinterest$content_Other_sC <- gregexpr(".*Food.*", pinterest$Answer.Q13Answerc) > 0
table(pinterest$content_Other_sC)

pinterest$image_content_sC <- pinterest$Answer.Q13Answerc
pinterest$Answer.Q13Answerc <- NULL 





#-------------------------------------------------------------------------------------------------------------------------------------------
# image_contentother_sC : the text answer to 'other' from image_content_sC
#-------------------------------------------------------------------------------------------------------------------------------------------

class(pinterest$Answer.Q13Answer_other_c)
table(pinterest$Answer.Q13Answer_other_c, useNA = c("always"))
pinterest$image_contentother_sC <- pinterest$Answer.Q13Answer_other_c
pinterest$Answer.Q13Answer_other_c <- NULL 






#-------------------------------------------------------------------------------------------------------------------------------------------
# image_themes_sC : What is the image about section C
# [Health, Disease/Illness, Medicine, Mensuration, Science, Infographic, Physical fitness/Sport, Cycling, Running, 
# Yoga/Pilates, Fitness equipment, Workout plan/instructions, Fashion, Weight loss/Dieting, Food/drink, Sugar, 
# Detoxing, Nutritional supplements/Herbs, Mental health, Emotional wellbeing, Beauty/Physical appearance, Sleep, Other]
#-------------------------------------------------------------------------------------------------------------------------------------------
class(pinterest$Answer.Q11Answerc)
table(pinterest$Answer.Q11Answerc, useNA = c("always"))


pinterest$theme_Health_sC <- gregexpr(".*Health.*", pinterest$Answer.Q11Answerc) > 0
table(pinterest$theme_Health_sC)
pinterest$theme_DiseaseIll_sC <- gregexpr(".*Disease.*", pinterest$Answer.Q11Answerc) > 0
table(pinterest$theme_DiseaseIll_sC)
pinterest$theme_Medicine_sC <- gregexpr(".*Medicine.*", pinterest$Answer.Q11Answerc) > 0
table(pinterest$theme_Medicine_sC)
pinterest$theme_Mensuration_sC <- gregexpr(".*Mensuration.*", pinterest$Answer.Q11Answerc) > 0
table(pinterest$theme_Mensuration_sC)
pinterest$theme_Science_sC <- gregexpr(".*Science.*", pinterest$Answer.Q11Answerc) > 0
table(pinterest$theme_Science_sC)
pinterest$theme_Infographic_sC <- gregexpr(".*Infographic.*", pinterest$Answer.Q11Answerc) > 0
table(pinterest$theme_Infographic_sC)
pinterest$theme_PhysicalFit_sC <- gregexpr(".*Physical.*", pinterest$Answer.Q11Answerc) > 0
table(pinterest$theme_PhysicalFit_sC)
pinterest$theme_Cycling_sC <- gregexpr(".*Cycling.*", pinterest$Answer.Q11Answerc) > 0
table(pinterest$theme_Cycling_sC)
pinterest$theme_Running_sC <- gregexpr(".*Running.*", pinterest$Answer.Q11Answerc) > 0
table(pinterest$theme_Running_sC)
pinterest$theme_Yoga_sC <- gregexpr(".*Yoga.*", pinterest$Answer.Q11Answerc) > 0
table(pinterest$theme_Yoga_sC)
pinterest$theme_FitnessEqp_sC <- gregexpr(".*Fitness.*", pinterest$Answer.Q11Answerc) > 0
table(pinterest$theme_FitnessEqp_sC)
pinterest$theme_WorkoutPlan_sC <- gregexpr(".*Workout.*", pinterest$Answer.Q11Answerc) > 0
table(pinterest$theme_WorkoutPlan_sC)
pinterest$theme_WorkoutPlan_sC <- gregexpr(".*Workout.*", pinterest$Answer.Q11Answerc) > 0
table(pinterest$theme_WorkoutPlan_sC)
pinterest$theme_Fashion_sC <- gregexpr(".*Fashion.*", pinterest$Answer.Q11Answerc) > 0
table(pinterest$theme_Fashion_sC)
pinterest$theme_Weightloss_sC <- gregexpr(".*Weight.*", pinterest$Answer.Q11Answerc) > 0
table(pinterest$theme_Weightloss_sC)
pinterest$theme_FoodDrink_sC <- gregexpr(".*Food.*", pinterest$Answer.Q11Answerc) > 0
table(pinterest$theme_FoodDrink_sC)
pinterest$theme_Sugar_sC <- gregexpr(".*Sugar.*", pinterest$Answer.Q11Answerc) > 0
table(pinterest$theme_Sugar_sA)
pinterest$theme_Detox_sA <- gregexpr(".*Detoxing.*", pinterest$Answer.Q11Answerc) > 0
table(pinterest$theme_Detox_sC)
pinterest$theme_Supplement_sC <- gregexpr(".*Nutritional.*", pinterest$Answer.Q11Answerc) > 0
table(pinterest$theme_Supplement_sC)
pinterest$theme_MentalHealth_sC <- gregexpr(".*Mental.*", pinterest$Answer.Q11Answerc) > 0
table(pinterest$theme_MentalHealth_sC)
pinterest$theme_EmotionalWell_sC <- gregexpr(".*Emotional.*", pinterest$Answer.Q11Answerc) > 0
table(pinterest$theme_EmotionalWell_sC)
pinterest$theme_Beauty_sC <- gregexpr(".*Beauty.*", pinterest$Answer.Q11Answerc) > 0
table(pinterest$theme_Beauty_sC)
pinterest$theme_Sleep_sC <- gregexpr(".*Sleep.*", pinterest$Answer.Q11Answerc) > 0
table(pinterest$theme_Sleep_sC)
pinterest$theme_Other_sC <- gregexpr(".*Other.*", pinterest$Answer.Q11Answerc) > 0
table(pinterest$theme_Other_sC)

pinterest$image_themes_sC <- pinterest$Answer.Q11Answerc
pinterest$Answer.Q11Answerc <- NULL 


#-------------------------------------------------------------------------------------------------------------------------------------------
# image_themesother_sC : content of the 'other' for image_themes_sB
#-------------------------------------------------------------------------------------------------------------------------------------------
class(pinterest$Answer.Q11Answer_other_c)
table(pinterest$Answer.Q11Answer_other_c, useNA = c("always"))

pinterest$image_themesother_sC <- pinterest$Answer.Q11Answer_other_c
pinterest$Answer.Q11Answer_other_c <- NULL 





#-------------------------------------------------------------------------------------------------------------------------------------------
# improve_health_sC : Is this image about improving health? (Tick only one)
# [Not at all, Somewhat, Very much]
#-------------------------------------------------------------------------------------------------------------------------------------------
class(pinterest$Answer.Q16Answerc)
table(pinterest$Answer.Q16Answerc, useNA = c("always"))
pinterest$improve_health_sC  <- recode(pinterest$Answer.Q16Answerc, '"Not at all" = 1; "Somewhat" = 2; "Very much" = 3; "" = NA')
class(pinterest$improve_health_sC)
table(pinterest$improve_health_sC, useNA = c("always"))
recode <- c("Not at all" = 1, "Somewhat" = 2, "Very" = 3)
pinterest$improve_health_sC <- factor(pinterest$improve_health_sC, levels = recode, labels = names(recode), exclude = NULL)
class(pinterest$improve_health_sC)
table(pinterest$improve_health_sC, useNA = c("always"))
pinterest$Answer.Q16Answerc <- NULL 




#-------------------------------------------------------------------------------------------------------------------------------------------
# improve_fitness_sC : Is this image about improving health? (Tick only one)
# [Not at all, Somewhat, Very much]
#-------------------------------------------------------------------------------------------------------------------------------------------
class(pinterest$Answer.Q17Answerc)
table(pinterest$Answer.Q17Answerc, useNA = c("always"))
pinterest$improve_fitness_sC  <- recode(pinterest$Answer.Q17Answerc, '"Not at all" = 1; "Somewhat" = 2; "Very much" = 3; "" = NA')
class(pinterest$improve_fitness_sC)
table(pinterest$improve_fitness_sC, useNA = c("always"))
recode <- c("Not at all" = 1, "Somewhat" = 2, "Very" = 3)
pinterest$improve_fitness_sC <- factor(pinterest$improve_fitness_sC, levels = recode, labels = names(recode), exclude = NULL)
class(pinterest$improve_fitness_sC)
table(pinterest$improve_fitness_sC, useNA = c("always"))
pinterest$Answer.Q17Answerc <- NULL 






#-------------------------------------------------------------------------------------------------------------------------------------------
# improve_attractive_sB : Is this image about improving health? (Tick only one)
# [Not at all, Somewhat, Very much]
#-------------------------------------------------------------------------------------------------------------------------------------------
class(pinterest$Answer.Q18Answerb)
table(pinterest$Answer.Q18Answerb, useNA = c("always"))
pinterest$improve_attractive_sB  <- recode(pinterest$Answer.Q18Answerb, '"Not at all" = 1; "Somewhat" = 2; "Very much" = 3; "" = NA')
class(pinterest$improve_attractive_sB)
table(pinterest$improve_attractive_sB, useNA = c("always"))
recode <- c("Not at all" = 1, "Somewhat" = 2, "Very" = 3)
pinterest$improve_attractive_sB <- factor(pinterest$improve_attractive_sB, levels = recode, labels = names(recode), exclude = NULL)
class(pinterest$improve_attractive_sB)
table(pinterest$improve_attractive_sB, useNA = c("always"))
pinterest$Answer.Q18Answerb <- NULL 





#-------------------------------------------------------------------------------------------------------------------------------------------
# cure_sC : Is this image about curing health problems/illness? (Tick only one)
# [Yes, No, Don’t know]
#-------------------------------------------------------------------------------------------------------------------------------------------
class(pinterest$Answer.Q20Answerc)
table(pinterest$Answer.Q20Answerc, useNA = c("always"))
pinterest$cure_sC  <- recode(pinterest$Answer.Q20Answerc, '"No" = 0; "Yes" = 1; "Don\'t know" = 2; "" = NA')
class(pinterest$cure_sC)
table(pinterest$cure_sC, useNA = c("always"))
recode <- c("No" = 0, "Yes" = 1, "Don\'t know" = 2)
pinterest$cure_sC <- factor(pinterest$cure_sC, levels = recode, labels = names(recode), exclude = NULL)
class(pinterest$cure_sC)
table(pinterest$cure_sC, useNA = c("always"))
pinterest$Answer.Q20Answerc <- NULL 







#-------------------------------------------------------------------------------------------------------------------------------------------
# prevent_sC : Is this image about preventing health problems/illness? (Tick only one)
# [Yes, No, Don’t know]
#-------------------------------------------------------------------------------------------------------------------------------------------
class(pinterest$Answer.Q21Answerc)
table(pinterest$Answer.Q21Answerc, useNA = c("always"))
pinterest$prevent_sC  <- recode(pinterest$Answer.Q21Answerc, '"No" = 0; "Yes" = 1; "Don\'t know" = 2; "" = NA')
class(pinterest$prevent_sC)
table(pinterest$prevent_sC, useNA = c("always"))
recode <- c("No" = 0, "Yes" = 1, "Don\'t know" = 2)
pinterest$prevent_sC <- factor(pinterest$prevent_sC, levels = recode, labels = names(recode), exclude = NULL)
class(pinterest$prevent_sC)
table(pinterest$prevent_sC, useNA = c("always"))
pinterest$Answer.Q21Answerc <- NULL 







#!!!!!
#-------------------------------------------------------------------------------------------------------------------------------------------
# prevent_sC : Does this image focus on a particular person? (Tick only one)
# [Yes, No, Don’t know]
#-------------------------------------------------------------------------------------------------------------------------------------------
#class(pinterest$Q24Answerc)
#table(pinterest$Q24Answerc , useNA = c("always"))
#pinterest$prevent_sC  <- recode(pinterest$Answer.Q21Answerc, '"No" = 0; "Yes" = 1; "Don\'t know" = 2; "" = NA')
#class(pinterest$prevent_sC)
#table(pinterest$prevent_sC, useNA = c("always"))
#recode <- c("No" = 0, "Yes" = 1, "Don\'t know" = 2)
#pinterest$prevent_sC <- factor(pinterest$prevent_sC, levels = recode, labels = names(recode), exclude = NULL)
#class(pinterest$prevent_sC)
#table(pinterest$prevent_sC, useNA = c("always"))
#pinterest$Q24Answerc  <- NULL 





#-------------------------------------------------------------------------------------------------------------------------------------------
# bodyparts_sC : Which parts of the body do you see? (Tick all that apply)
# [Head, Face, Upper torso (front), Upper torso (back), Stomach, Upper arm, 
# Lower arm, Hand, Bum, Hip, Upper leg, Lower leg, Feet]
#-------------------------------------------------------------------------------------------------------------------------------------------

class(pinterest$Answer.Q3Answerc)
table(pinterest$Answer.Q3Answerc, useNA = c("always"))
pinterest$bodyparts_Head_sC <- gregexpr(".*Head.*", pinterest$Answer.Q3Answerc) > 0
table(pinterest$bodyparts_Head_sC)
pinterest$bodyparts_Face_sC <- gregexpr(".*Face.*", pinterest$Answer.Q3Answerc) > 0
table(pinterest$bodyparts_Face_sC)
pinterest$bodyparts_UpperTorsoF_sC <- gregexpr(".*front.*", pinterest$Answer.Q3Answerc) > 0
table(pinterest$bodyparts_UpperTorsoF_sC)
pinterest$bodyparts_UpperTorsoB_sC <- gregexpr(".*back.*", pinterest$Answer.Q3Answerc) > 0
table(pinterest$bodyparts_UpperTorsoB_sC)
pinterest$bodyparts_Stomach_sC <- gregexpr(".*Stomach.*", pinterest$Answer.Q3Answerc) > 0
table(pinterest$bodyparts_Stomach_sC)
pinterest$bodyparts_UpperArm_sC <- gregexpr(".*Upper arm.*", pinterest$Answer.Q3Answerc) > 0
table(pinterest$bodyparts_UpperArm_sC)
pinterest$bodyparts_LowerArm_sC <- gregexpr(".*Lower arm.*", pinterest$Answer.Q3Answerc) > 0
table(pinterest$bodyparts_LowerArm_sC)
pinterest$bodyparts_Hand_sC <- gregexpr(".*Hand.*", pinterest$Answer.Q3Answerc) > 0
table(pinterest$bodyparts_Hand_sC)
pinterest$bodyparts_Bum_sC <- gregexpr(".*Bum.*", pinterest$Answer.Q3Answerc) > 0
table(pinterest$bodyparts_Bum_sC)
pinterest$bodyparts_Hip_sC <- gregexpr(".*Hip.*", pinterest$Answer.Q3Answerc) > 0
table(pinterest$bodyparts_Hip_sC)
pinterest$bodyparts_UpperLeg_sC <- gregexpr(".*Upper leg.*", pinterest$Answer.Q3Answerc) > 0
table(pinterest$bodyparts_UpperLeg_sC)
pinterest$bodyparts_LowerLeg_sC <- gregexpr(".*Lower leg.*", pinterest$Answer.Q3Answerc) > 0
table(pinterest$bodyparts_LowerLeg_sC)
pinterest$bodyparts_Feet_sC <- gregexpr(".*Foot*", pinterest$Answer.Q3Answerc) > 0
table(pinterest$bodyparts_Feet_sC)


pinterest$bodyparts_sC <- pinterest$Answer.Q3Answerc
pinterest$Answer.Q3Answerc <- NULL 



#-------------------------------------------------------------------------------------------------------------------------------------------
# bodyparts_naked_sC : Which parts of the body do you see? (Tick all that apply)
# [Head, Face, Upper torso (front), Upper torso (back), Stomach, Upper arm, 
# Lower arm, Hand, Bum, Hip, Upper leg, Lower leg, Feet]
#-------------------------------------------------------------------------------------------------------------------------------------------

class(pinterest$Answer.Q4Answerc)
table(pinterest$Answer.Q4Answerc, useNA = c("always"))
pinterest$bodyparts_naked_Head_sC <- gregexpr(".*Head.*", pinterest$Answer.Q4Answerc) > 0
table(pinterest$bodyparts_naked_Head_sC)
pinterest$bodyparts_naked_Face_sC <- gregexpr(".*Face.*", pinterest$Answer.Q4Answerc) > 0
table(pinterest$bodyparts_naked_Face_sC)
pinterest$bodyparts_naked_UpperTorsoF_sC <- gregexpr(".*front.*", pinterest$Answer.Q4Answerc) > 0
table(pinterest$bodyparts_vUpperTorsoF_sC)
pinterest$bodyparts_naked_UpperTorsoB_sC <- gregexpr(".*back.*", pinterest$Answer.Q4Answerc) > 0
table(pinterest$bodyparts_naked_UpperTorsoB_sC)
pinterest$bodyparts_naked_Stomach_sC <- gregexpr(".*Stomach.*", pinterest$Answer.Q4Answerc) > 0
table(pinterest$bodyparts_vStomach_sC)
pinterest$bodyparts_naked_UpperArm_sC <- gregexpr(".*Upper arm.*", pinterest$Answer.Q4Answerc) > 0
table(pinterest$bodyparts_naked_UpperArm_sC)
pinterest$bodyparts_naked_LowerArm_sC <- gregexpr(".*Lower arm.*", pinterest$Answer.Q4Answerc) > 0
table(pinterest$bodyparts_naked_LowerArm_sC)
pinterest$bodyparts_naked_Hand_sC <- gregexpr(".*Hand.*", pinterest$Answer.Q4Answerc) > 0
table(pinterest$bodyparts_naked_Hand_sC)
pinterest$bodyparts_naked_Bum_sC <- gregexpr(".*Bum.*", pinterest$Answer.Q4Answerc) > 0
table(pinterest$bodyparts_naked_Bum_sC)
pinterest$bodyparts_naked_Hip_sC <- gregexpr(".*Hip.*", pinterest$Answer.Q4Answerc) > 0
table(pinterest$bodyparts_naked_Hip_sC)
pinterest$bodyparts_naked_UpperLeg_sC <- gregexpr(".*Upper leg.*", pinterest$Answer.Q4Answerc) > 0
table(pinterest$bodyparts_naked_UpperLeg_sC)
pinterest$bodyparts_naked_LowerLeg_sC <- gregexpr(".*Lower leg.*", pinterest$Answer.Q4Answerc) > 0
table(pinterest$bodyparts_naked_LowerLeg_sC)
pinterest$bodyparts_naked_Feet_sC <- gregexpr(".*Foot*", pinterest$Answer.Q4Answerc) > 0
table(pinterest$bodyparts_naked_Feet_sC)


pinterest$bodyparts_sC <- pinterest$Answer.Q4Answerc
pinterest$Answer.Q4Answerc <- NULL 




#-------------------------------------------------------------------------------------------------------------------------------------------
# image_themes_sC : What is the image about section A
# [Health, Disease/Illness, Medicine, Mensuration, Science, Infographic, Physical fitness/Sport, Cycling, Running, 
# Yoga/Pilates, Fitness equipment, Workout plan/instructions, Fashion, Weight loss/Dieting, Food/drink, Sugar, 
# Detoxing, Nutritional supplements/Herbs, Mental health, Emotional wellbeing, Beauty/Physical appearance, Sleep, Other]
#-------------------------------------------------------------------------------------------------------------------------------------------
class(pinterest$Answer.Q11Answerc)
table(pinterest$Answer.Q11Answerc, useNA = c("always"))


pinterest$theme_Health_sC <- gregexpr(".*Health.*", pinterest$Answer.Q11Answerc) > 0
table(pinterest$theme_Health_sC)
pinterest$theme_DiseaseIll_sC <- gregexpr(".*Disease.*", pinterest$Answer.Q11Answerc) > 0
table(pinterest$theme_DiseaseIll_sC)
pinterest$theme_Medicine_sC <- gregexpr(".*Medicine.*", pinterest$Answer.Q11Answerc) > 0
table(pinterest$theme_Medicine_sC)
pinterest$theme_Mensuration_sC <- gregexpr(".*Mensuration.*", pinterest$Answer.Q11Answerc) > 0
table(pinterest$theme_Mensuration_sC)
pinterest$theme_Science_sC <- gregexpr(".*Science.*", pinterest$Answer.Q11Answerc) > 0
table(pinterest$theme_Science_sC)
pinterest$theme_Infographic_sC <- gregexpr(".*Infographic.*", pinterest$Answer.Q11Answerc) > 0
table(pinterest$theme_Infographic_sC)
pinterest$theme_PhysicalFit_sC <- gregexpr(".*Physical.*", pinterest$Answer.Q11Answerc) > 0
table(pinterest$theme_PhysicalFit_sC)
pinterest$theme_Cycling_sC <- gregexpr(".*Cycling.*", pinterest$Answer.Q11Answerc) > 0
table(pinterest$theme_Cycling_sC)
pinterest$theme_Running_sC <- gregexpr(".*Running.*", pinterest$Answer.Q11Answerc) > 0
table(pinterest$theme_Running_sC)
pinterest$theme_Yoga_sC <- gregexpr(".*Yoga.*", pinterest$Answer.Q11Answerc) > 0
table(pinterest$theme_Yoga_sC)
pinterest$theme_FitnessEqp_sC <- gregexpr(".*Fitness.*", pinterest$Answer.Q11Answerc) > 0
table(pinterest$theme_FitnessEqp_sC)
pinterest$theme_WorkoutPlan_sC <- gregexpr(".*Workout.*", pinterest$Answer.Q11Answerc) > 0
table(pinterest$theme_WorkoutPlan_sC)
pinterest$theme_WorkoutPlan_sC <- gregexpr(".*Workout.*", pinterest$Answer.Q11Answerc) > 0
table(pinterest$theme_WorkoutPlan_sC)
pinterest$theme_Fashion_sC <- gregexpr(".*Fashion.*", pinterest$Answer.Q11Answerc) > 0
table(pinterest$theme_Fashion_sC)
pinterest$theme_Weightloss_sC <- gregexpr(".*Weight.*", pinterest$Answer.Q11Answerc) > 0
table(pinterest$theme_Weightloss_sC)
pinterest$theme_FoodDrink_sC <- gregexpr(".*Food.*", pinterest$Answer.Q11Answerc) > 0
table(pinterest$theme_FoodDrink_sC)
pinterest$theme_Sugar_sC <- gregexpr(".*Sugar.*", pinterest$Answer.Q11Answerc) > 0
table(pinterest$theme_Sugar_sC)
pinterest$theme_Detox_sC <- gregexpr(".*Detoxing.*", pinterest$Answer.Q11Answerc) > 0
table(pinterest$theme_Detox_sC)
pinterest$theme_Supplement_sC <- gregexpr(".*Nutritional.*", pinterest$Answer.Q11Answerc) > 0
table(pinterest$theme_Supplement_sC)
pinterest$theme_MentalHealth_sC <- gregexpr(".*Mental.*", pinterest$Answer.Q11Answerc) > 0
table(pinterest$theme_MentalHealth_sC)
pinterest$theme_EmotionalWell_sC <- gregexpr(".*Emotional.*", pinterest$Answer.Q11Answerc) > 0
table(pinterest$theme_EmotionalWell_sC)
pinterest$theme_Beauty_sC <- gregexpr(".*Beauty.*", pinterest$Answer.Q11Answerc) > 0
table(pinterest$theme_Beauty_sC)
pinterest$theme_Sleep_sC <- gregexpr(".*Sleep.*", pinterest$Answer.Q11Answerc) > 0
table(pinterest$theme_Sleep_sC)
pinterest$theme_Other_sC <- gregexpr(".*Other.*", pinterest$Answer.Q11Answerc) > 0
table(pinterest$theme_Other_sC)

pinterest$image_themes_sC <- pinterest$Answer.Q11Answerc
pinterest$Answer.Q11Answerc <- NULL 





#-------------------------------------------------------------------------------------------------------------------------------------------
# image_themesother_sC : content of the 'other' for image_themes_sA
#-------------------------------------------------------------------------------------------------------------------------------------------
class(pinterest$Answer.Q11Answer_other_c)
table(pinterest$Answer.Q11Answer_other_c, useNA = c("always"))
pinterest$image_themesother_sC <- pinterest$Answer.Q11Answer_other_c
pinterest$Answer.Q11Answer_other_c <- NULL 




#-------------------------------------------------------------------------------------------------------------------------------------------
# image_text_sA : If there is text in the image, please type it into the field below. (If it is more than about 100 words, please 
# focus on the most important parts such as the title and the main headings.)
#-------------------------------------------------------------------------------------------------------------------------------------------

class(pinterest$Answer.Q14Answerc)
pinterest$text_sC <- pinterest$Answer.Q14Answerc
pinterest$Answer.Q14Answerc<- NULL 


#!!!!
#-------------------------------------------------------------------------------------------------------------------------------------------
# race_sB : If this image focuses on a particular person,
# what is the race of this person? Please take your best guess. (Tick only one)
# [White/Non-Hispanic, Black/Non-hispanic, Hispanic, Asian, Other, Don’t want to answer]
#-------------------------------------------------------------------------------------------------------------------------------------------

#class(pinterest$Answer.Q23Answerc)
#table(pinterest$Answer.Q23Answerc, useNA = c("always"))
#pinterest$race_sC <- recode(pinterest$Answer.Q23Answerc, '"White/Non-Hispanic" = 1; "Black/Non-Hispanic" = 2; "Hispanic" = 3; "Asian" = 4; "Other" = 5; "Don\'t know" = 6; "" = NA')
#class(pinterest$race_sC)
#table(pinterest$race_sC, useNA = c("always"))
#recode_race <- c(WhiteNonHispanic = 1, BlackNonHispanic = 2, Hispanic = 3, Asian = 4, Other = 5, Refuse = 6)
#pinterest$race_sC <- factor(pinterest$race_sC, levels = recode_race, labels = names(recode_race), exclude = NULL)
#class(pinterest$race_sC)
#table(pinterest$race_sC, useNA = c("always"))
#pinterest$Answer.Q5Answerc <- NULL 


#!!!!
#-------------------------------------------------------------------------------------------------------------------------------------------
#female_sC : If this images focuses on a particular person, what is the gender of these people? (Tick only one)
# [famle, male, don't know]
#-------------------------------------------------------------------------------------------------------------------------------------------

#class(pinterest$Answer.Q24Answerc)
#table(pinterest$Answer.Q24Answerc, useNA = c("always"))
#pinterest$female_sC <- recode(pinterest$Answer.Q6Answerc, '"Male" = 0; "Female" = 1; "Mixed" = 3; "Don\'t know" = 4; "" = NA')
#class(pinterest$female_sC)
#table(pinterest$female_sC, useNA = c("always"))
#recode <- c(Male = 0, Female = 1, Mixed = 3, DontKnow = 4)
#pinterest$female_sC <- factor(pinterest$female_sC, levels = recode, labels = names(recode), exclude = NULL)
#class(pinterest$female_sC)
#table(pinterest$female_sC, useNA = c("always"))
#pinterest$Answer.Q6Answerc <- NULL


#!!!!
#-------------------------------------------------------------------------------------------------------------------------------------------
# age_sC : If this image focuses on a particular person, 
# what is the age of this person? Please take your best guess. (Tick only one)
# [Younger than 18, 18-29, 30-49, 50 or older, Don’t know]
#-------------------------------------------------------------------------------------------------------------------------------------------
#class(pinterest$Answer.Q25Answerc)
#table(pinterest$Answer.Q25Answerc, useNA = c("always"))
#pinterest$age_sB  <- recode(pinterest$Answer.Q7Answerb , '"Younger than 18" = 1; "18 to 29" = 2; "30 to 49" = 3; "50 or older" = 4; "Don\'t know" = 5; "" = NA')
#class(pinterest$age_sB)
#table(pinterest$age_sB, useNA = c("always"))
#recode <- c("Younger than 18" = 1, "18 to 29" = 2, "30 to 49" = 3, "50 or older" = 4, "Don\'t know" = 5)
#pinterest$age_sB <- factor(pinterest$age_sB, levels = recode, labels = names(recode), exclude = NULL)
#class(pinterest$age_sB)
#table(pinterest$age_sB, useNA = c("always"))
#pinterest$Answer.Q7Answerb <- NULL 



#-------------------------------------------------------------------------------------------------------------------------------------------
# emotion_sC : How do these peopl appear to feel? (Tick only one)
# [Happy, Neither happy nor sad, Sad, Don’t know]
#-------------------------------------------------------------------------------------------------------------------------------------------
class(pinterest$Answer.Q8Answerc)
table(pinterest$Answer.Q8Answerc, useNA = c("always"))
pinterest$emotion_sC  <- recode(pinterest$Answer.Q8Answerc, '"Happy" = 1; "Neither happy nor sad" = 2; "Sad" = 3; "Don\'t know" = 4; "" = NA')
class(pinterest$emotion_sC)
table(pinterest$emotion_sC, useNA = c("always"))
recode <- c("Happy" = 1, "Neither happy nor sad" = 2, "Sad" = 3, "Don\'t know" = 4)
pinterest$emotion_sC <- factor(pinterest$emotion_sC, levels = recode, labels = names(recode), exclude = NULL)
class(pinterest$emotion_sC)
table(pinterest$emotion_sC, useNA = c("always"))
pinterest$Answer.Q8Answerc <- NULL 




#-------------------------------------------------------------------------------------------------------------------------------------------
# health_sC : If this images focuses on one particular person, 
# how health do you think this person is? (Tick only one)
# [Very unhealthy, Somewhat unhealthy, Average, Somewhat healthy, Very healthy]
#-------------------------------------------------------------------------------------------------------------------------------------------
class(pinterest$Answer.Q15Answerc)
table(pinterest$Answer.Q15Answerc, useNA = c("always"))
pinterest$health_sC  <- recode(pinterest$Answer.Q15Answerc, '"Very unhealthy" = 1; "Somewhat unhealthy" = 2; "Average" = 3; "Somewhat healthy" = 4; "Very healthy" = 5; "" = NA')
class(pinterest$health_sC)
table(pinterest$health_sC, useNA = c("always"))
recode <- c("Very unhealthy" = 1, "Somewhat unhealthy" = 2, "Average" = 3, "Somewhat healthy" = 4, "Very healthy" = 5)
pinterest$health_sC <- factor(pinterest$health_sC, levels = recode, labels = names(recode), exclude = NULL)
class(pinterest$health_sC)
table(pinterest$health_sC, useNA = c("always"))
pinterest$Answer.Q15Answerc <- NULL 






#-------------------------------------------------------------------------------------------------------------------------------------------
# attractive_sC : If this images focuses on one particular person, 
# do you find this person physically attractive? (Tick only one)
# [Not at all, Somewhat, Very much]
#-------------------------------------------------------------------------------------------------------------------------------------------
class(pinterest$Answer.Q25Answerc)
table(pinterest$Answer.Q25Answerc, useNA = c("always"))
pinterest$attractive_sC  <- recode(pinterest$Answer.Q25Answerc, '"Not at all" = 1; "Somewhat" = 2; "Very much" = 3; "" = NA')class(pinterest$attractive_sC)
table(pinterest$attractive_sC, useNA = c("always"))
recode <- c("Not at all" = 1, "Somewhat" = 2, "Very" = 3)
pinterest$attractive_sC <- factor(pinterest$attractive_sC, levels = recode, labels = names(recode), exclude = NULL)
class(pinterest$attractive_sC)
table(pinterest$attractive_sC, useNA = c("always"))
pinterest$Answer.Q25Answerc <- NULL 



#!!!!
#-------------------------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------------------
#class(pinterest$Answer.Q26Answerc)
#table(pinterest$Answer.Q26Answerc, useNA = c("always"))
#pinterest$emotion_focal_sC  <- recode(pinterest$Answer.Q26Answerc, '"Happy" = 1; "Neither happy nor sad" = 2; "Sad" = 3; "Don\'t know" = 4; "" = NA')
#class(pinterest$emotion_focal_sC)
#table(pinterest$emotion_focal_sC, useNA = c("always"))
#recode <- c("Happy" = 1, "Neither happy nor sad" = 2, "Sad" = 3, "Don\'t know" = 4)
#pinterest$emotion_focal_sC <- factor(pinterest$emotion_focal_sC, levels = recode, labels = names(recode), exclude = NULL)
#class(pinterest$emotion_focal_sC)
#table(pinterest$emotion_focal_sC, useNA = c("always"))
#pinterest$Answer.Q26Answerc <- NULL 



#-------------------------------------------------------------------------------------------------------------------------------------------
# sexualized_sC : If this images focuses on one particular person,
# does this image portray the person in a sexualized manner? (Tick only one)
# [Not at all, Somewhat, Very much]
#-------------------------------------------------------------------------------------------------------------------------------------------
class(pinterest$Answer.Q19Answerc)
table(pinterest$Answer.Q19Answerc, useNA = c("always"))
pinterest$sexualized_sC  <- recode(pinterest$Answer.Q19Answerc, '"Not at all" = 1; "Somewhat" = 2; "Very much" = 3; "" = NA')
class(pinterest$sexualized_sC)
table(pinterest$sexualized_sC, useNA = c("always"))
recode <- c("Not at all" = 1, "Somewhat" = 2, "Very" = 3)
pinterest$sexualized_sC <- factor(pinterest$sexualized_sC, levels = recode, labels = names(recode), exclude = NULL)
class(pinterest$sexualized_sC)
table(pinterest$sexualized_sC, useNA = c("always"))
pinterest$Answer.Q19Answerc <- NULL






#-------------------------------------------------------------------------------------------------------------------------------------------
# physcondition_sC : If this images focuses on one particular person,
# how would you describe the physical condition of this person? (Tick only one)
# [In-shape/athletic, Average, Out of shape, Don’t know]
#-------------------------------------------------------------------------------------------------------------------------------------------
class(pinterest$Answer.Q9Answerc)
table(pinterest$Answer.Q9Answerc, useNA = c("always"))
pinterest$physcondition_sC  <- recode(pinterest$Answer.Q9Answerc, '"In-shape/athletic" = 1; "Average" = 2; "Out of shape" = 3; "Don\'t know" = 4; "" = NA')
class(pinterest$physcondition_sC)
table(pinterest$physcondition_sC, useNA = c("always"))
recode <- c("In-shape/athletic" = 1, "Average" = 2, "Out of shape" = 3, "Don\'t know" = 4)
pinterest$physcondition_sC <- factor(pinterest$physcondition_sC, levels = recode, labels = names(recode), exclude = NULL)
class(pinterest$physcondition_sC)
table(pinterest$physcondition_sC, useNA = c("always"))
pinterest$Answer.Q9Answerc <- NULL




#-------------------------------------------------------------------------------------------------------------------------------------------
# improve_attractive_sC : Is this image about improving physical attractiveness? (Tick only one)
# [Not at all, Somewhat, Very much]
#-------------------------------------------------------------------------------------------------------------------------------------------
class(pinterest$Answer.Q18Answerc)
table(pinterest$Answer.Q18Answerc, useNA = c("always"))
pinterest$improve_attractive_sC  <- recode(pinterest$Answer.Q18Answerc, '"Not at all" = 1; "Somewhat" = 2; "Very much" = 3; "" = NA')
class(pinterest$improve_attractive_sC)
table(pinterest$improve_attractive_sC, useNA = c("always"))
recode <- c("Not at all" = 1, "Somewhat" = 2, "Very" = 3)
pinterest$improve_attractive_sC <- factor(pinterest$improve_attractive_sC, levels = recode, labels = names(recode), exclude = NULL)
class(pinterest$improve_attractive_sC)
table(pinterest$improve_attractive_sC, useNA = c("always"))
pinterest$Answer.Q18Answerc <- NULL 






#-------------------------------------------------------------------------------------------------------------------------------------------
# bodytype_sC: If this images focuses on one particular person,
# which of the following sketches comes closest to the body shape of this person? (Tick only one)
# [A, B, C, D, E]
#-------------------------------------------------------------------------------------------------------------------------------------------
class(pinterest$Answer.Q10Answerc)
table(pinterest$Answer.Q10Answerc, useNA = c("always"))
pinterest$bodytype_sC  <- recode(pinterest$Answer.Q10Answerc, '"A" = 1; "B" = 2; "C" = 3; "D" = 4; "E" = 5; "" = NA')
class(pinterest$bodytype_sC)
table(pinterest$bodytype_sC, useNA = c("always"))
recode <- c("A" = 1, "B" = 2, "C" = 3, "D" = 4, "E" = 5)
pinterest$bodytype_sC <- factor(pinterest$bodytype_sC, levels = recode, labels = names(recode), exclude = NULL)
class(pinterest$bodytype_sC)
table(pinterest$bodytype_sC, useNA = c("always"))
pinterest$Answer.Q10Answerc <- NULL


