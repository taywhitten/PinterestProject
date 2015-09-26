#===========================================================================================================================================
# SCRIPT PURPOSE:
# This script is used to clean the output of the mTurk results for the Pinterest data project
#===========================================================================================================================================


#===========================================================================================================================================
# (1) SETUP
#===========================================================================================================================================
# LOAD PACKAGES & DATA
#-------------------------------------------------------------------------------------------------------------------------------------------
load("~/Downloads/FinalData.Rdata")
pinterest <- data[, 28:98]


#===========================================================================================================================================
# (2) CORRECT & FORMAT COLUMNS
#===========================================================================================================================================
#-------------------------------------------------------------------------------------------------------------------------------------------
# female_turker : the gender of the turker
#-------------------------------------------------------------------------------------------------------------------------------------------
pinterest$female_turker[pinterest$Answer.Q01Answer0 == "Female"] <- 1
pinterest$female_turker[pinterest$Answer.Q01Answer0 == "Male"] <- 0
pinterest$female_turker[pinterest$Answer.Q01Answer0 == "Don't want to answer"] <- 3
pinterest$female_turker[pinterest$Answer.Q01Answer0 == ""] <- NA
class(pinterest$female_turker)
table(pinterest$female_turker)
recode <- c(Male = 0, Female = 1, Refuse = 3)
pinterest$female_turker <- factor(pinterest$female_turker, levels = recode, labels = names(recode))
class(pinterest$female_turker)
table(pinterest$female_turker)
pinterest <- pinterest[ , -which(names(pinterest) %in% c("Answer.Q01Answer0"))]



#-------------------------------------------------------------------------------------------------------------------------------------------
# age_turker : the age of the turker
#-------------------------------------------------------------------------------------------------------------------------------------------
pinterest$age_turker <- pinterest$Answer.Q02Answer0
class(pinterest$age_turker)
table(pinterest$age_turker)
pinterest$age_turker <- as.integer(pinterest$age_turker)
class(pinterest$age_turker)
table(pinterest$age_turker)


#-------------------------------------------------------------------------------------------------------------------------------------------
# race_turker : the race of the turker
# [White/Non-Hispanic, Black/Non-hispanic, Hispanic, Asian, Other, Don’t want to answer]
#-------------------------------------------------------------------------------------------------------------------------------------------
class(pinterest$Answer.Q03Answer0)
table(pinterest$Answer.Q03Answer0)
pinterest$race_turker[pinterest$Answer.Q03Answer0 == "White / Non-Hispanic"] <- 1
pinterest$race_turker[pinterest$Answer.Q03Answer0 == "Black / Non-Hispanic"] <- 2
pinterest$race_turker[pinterest$Answer.Q03Answer0 == "Hispanic"] <- 3
pinterest$race_turker[pinterest$Answer.Q03Answer0 == "Asian"] <- 4
pinterest$race_turker[pinterest$Answer.Q03Answer0 == "Other"] <- 5
pinterest$race_turker[pinterest$Answer.Q03Answer0 == "Don’t want to answer"] <- 6
class(pinterest$race_turker)
table(pinterest$race_turker)
recode_race <- c(WhiteNonHispanic = 1, BlackNonHispanic = 2, Hispanic = 3, Asian = 4, Other = 5, Refuse = 6)
pinterest$race_turker <- factor(pinterest$race_turker, levels = recode_race, labels = names(recode_race))
class(pinterest$race_turker)
table(pinterest$race_turker)
pinterest <- pinterest[ , -which(names(pinterest) %in% c("Answer.Q03Answer0"))]




#-------------------------------------------------------------------------------------------------------------------------------------------
# image_count_sA : count of the number of people in the image
#-------------------------------------------------------------------------------------------------------------------------------------------
class(Answer.Q1Answera)
table(Answer.Q1Answera)
pinterest$image_count_sA[pinterest$Answer.Q1Answera == "0"] <- 0
pinterest$image_count_sA[pinterest$Answer.Q1Answera == "1"] <- 1
pinterest$image_count_sA[pinterest$Answer.Q1Answera == "2 or more"] <- 2
pinterest$image_count_sA[pinterest$Answer.Q1Answera == ""] <- 3
class(pinterest$image_count_sA)
table(pinterest$image_count_sA)
recode_count <- c(None = 0, One = 1, Many = 2, NoAnswer = 3)
pinterest$image_count_sA <- factor(pinterest$image_count_sA, levels = recode_count, labels = names(recode_count))
class(pinterest$image_count_sA)
table(pinterest$image_count_sA)
pinterest <- pinterest[ , -which(names(pinterest) %in% c("Answer.Q1Answera"))]



#-------------------------------------------------------------------------------------------------------------------------------------------
# image_background_sA : image background section A
# [indoors, outdoors, image shows artificial background, don’t know]
#-------------------------------------------------------------------------------------------------------------------------------------------
class(Answer.Q12Answera)
table(Answer.Q12Answera)
pinterest$image_background_sA[pinterest$Answer.Q12Answera == "Indoors"] <- 1
pinterest$image_background_sA[pinterest$Answer.Q12Answera == "Outdoors"] <- 2
pinterest$image_background_sA[pinterest$Answer.Q12Answera == "Artificial"] <- 3
pinterest$image_background_sA[pinterest$Answer.Q12Answera == "Don't know"] <- 4
pinterest$image_background_sA[pinterest$Answer.Q12Answera == ""] <- NA
class(pinterest$image_background_sA)
table(pinterest$image_background_sA)
recode_background <- c(Indoors = 1, Outdoors = 2, Artificial = 3, DontKnow = 4)
pinterest$image_background_sA <- factor(pinterest$image_background_sA, levels = recode_background, labels = names(recode_background))
class(pinterest$image_background_sA)
table(pinterest$image_background_sA)
pinterest <- pinterest[ , -which(names(pinterest) %in% c("Answer.Q12Answera"))]
