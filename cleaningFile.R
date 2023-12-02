#research question: what are the clusters of behaviors women exhibit when it comes to preventative health?

library(readr)
cleanedFullSet <- read_csv("Documents/GradSchool/DSC424/Final Project/FinalistDatasets/BRFSS/cleanedFullSet.csv")

library(tidyverse)

codVars <- c('MSCODE','_STSTR','_STRWT','_RAWRAKE','_WT2RAKE','_CLLCPWT',
             '_DUALCOR','_LLCPWT')

allVars <- cleanedFullSet %>%
            select(-all_of(codVars))

#breast/cervical cancer - PAP/HPV test and other behaviors
#filter for women
#include variables in modules 12, 14, 15

womensHealth <- allVars %>%
                  filter(SEX == 2)
                   

relevantVars <- c('HPVADVC2','HPVADSHT','HPVTEST',
                  'HADPAP2','HADMAM','HOWLONG',
                  'LASTPAP2','HPLSTTST','HADHYST2',
                  'PROFEXAM','LENGEXAM','GENHLTH',
                  'PHYSHLTH','MENTHLTH','POORHLTH',
                  'HLTHPLN1','MEDCOST','CHECKUP1',
                  'EDUCA','EMPLOY1','CHILDREN',
                  'INCOME2','INTERNET','WEIGHT2',
                  'HTIN4','PREGNANT','SMOKE100',
                  'SMOKDAY2','USENOW3','ALCDAY5',
                  'AVEDRNK2','DRNK3GE5','MAXDRNKS',
                  '_FRUTSUM','_VEGESUM','STRENGTH',
                  'EXEROFT1','EXERHMM1','EXEROFT2',
                  'EXERHMM2','MAXVO2_','FC60_',
                  'ACTIN11_','ACTIN21_','_PAINDX1',
                  'SEATBELT','FLUSHOT6','HIVTST6',
                  'CAREGIV1','SCNTWRK1','SXORIENT',
                  'TRNSGNDR','EMTSUPRT','ADDOWN',
                  'ADSLEEP','ADPLEASR','ADENERGY',
                  'ADEAT1', '_AGEG5YR')

limitedVars <- womensHealth %>%
                select(all_of(relevantVars))

#combining HADPAP2 and HPVTEST for measure of 1 or the other
summary(limitedVars$HADPAP2)
summary(limitedVars$HPVTEST)
summary(limitedVars)

cleaningVars <- limitedVars %>%
                  mutate(HADHPVORPAP = case_when(HPVTEST == 1 | HADPAP2 == 1 ~ 1,
                                                 HPVTEST == 2 | HADPAP2 == 2 ~ 2,
                                                 HPVTEST == 7 | HADPAP2 == 7 ~ 99,
                                                 HPVTEST == 9 | HADPAP2 == 9 ~ 99,
                                                 .default = 99))
summary(cleaningVars$HADHPVORPAP)

combinedTest <- cleaningVars %>%
                  #filter(HPVTEST == 1 | HADPAP2 == 1) %>%
                  mutate(HPVTEST2 = ifelse(HPVTEST != 1,0,1),
                         HADPAP22 = ifelse(HADPAP2 != 1,0,1)) %>%
                  group_by(HADHPVORPAP, HPVTEST, HADPAP2) %>%
                  summarize(n = n(),
                            sumHPV = sum(HPVTEST2),
                            sumPap = sum(HADPAP22))

#eliminating all 99s from data set
testSet <- cleaningVars %>%
            filter(HADHPVORPAP != 99) %>%
            select(-c(HPVTEST, HADPAP2))

summary(testSet)

#whittling variables based on %N/A - any variable with more than 11k NAs should go
highlyNAVars <- c('HPVADVC2','HPVADSHT','HPLSTTST','PROFEXAM','LENGEXAM','PREGNANT',
                  'SMOKDAY2','AVEDRNK2','DRNK3GE5','MAXDRNKS','EXEROFT2','EXERHMM2',
                  'SCNTWRK1','SXORIENT','TRNSGNDR','EMTSUPRT','ADDOWN','ADSLEEP',
                  'ADPLEASR','ADENERGY','ADEAT1')

testSet2 <- testSet %>%
              select(-all_of(highlyNAVars))
summary(testSet2)

#need to clean individual variables - can't do no missing cases, so will need to be case by case
summary(testSet2)

#HOWLONG: time since last mammogram within last X years (if val = 1-3). 4 is last 5, 
#5 is more than 5. 7 is don't know, 9 is refused, NA is missing.
testSet2 %>% select(HOWLONG) %>% group_by(HOWLONG) %>% summarise(n = n())

testSet2$HOWLONG <- replace(testSet2$HOWLONG, is.na(testSet2$HOWLONG), 0)
testSet2$HOWLONG <- replace(testSet2$HOWLONG, testSet2$HOWLONG==9, 0)
testSet2$HOWLONG <- replace(testSet2$HOWLONG, testSet2$HOWLONG==7, 0)
testSet2$HOWLONG <- replace(testSet2$HOWLONG, testSet2$HOWLONG==5, 6)
testSet2$HOWLONG <- replace(testSet2$HOWLONG, testSet2$HOWLONG==4, 5)
summary(testSet2$HOWLONG)

#LASTPAP2: same scale as HOWLONG, will clean the same
testSet2 %>% select(LASTPAP2) %>% group_by(LASTPAP2) %>% summarise(n = n())
testSet2$LASTPAP2 <- replace(testSet2$LASTPAP2, is.na(testSet2$LASTPAP2), 0)
testSet2$LASTPAP2 <- replace(testSet2$LASTPAP2, testSet2$LASTPAP2==9, 0)
testSet2$LASTPAP2 <- replace(testSet2$LASTPAP2, testSet2$LASTPAP2==7, 0)
testSet2$LASTPAP2 <- replace(testSet2$LASTPAP2, testSet2$LASTPAP2==5, 6)
testSet2$LASTPAP2 <- replace(testSet2$LASTPAP2, testSet2$LASTPAP2==4, 5)
summary(testSet2$LASTPAP2)

#HADHYST2: 1 is yes, 2 is no. Would like to keep as binary. Few refusals
#coding refusals as "no hysterectomy"
testSet2 %>% select(HADHYST2) %>% group_by(HADHYST2) %>% summarise(n = n())
testSet2$HADHYST2 <- replace(testSet2$HADHYST2, is.na(testSet2$HADHYST2), 2)
testSet2$HADHYST2 <- replace(testSet2$HADHYST2, testSet2$HADHYST2==9, 2)
testSet2$HADHYST2 <- replace(testSet2$HADHYST2, testSet2$HADHYST2==7, 2)

#GENHLTH: categorical with 5 levels. Only a handful rows with N/A or don't know, will remove
testSet2 %>% select(GENHLTH) %>% group_by(GENHLTH) %>% summarise(n = n())
testSet3 <- testSet2 %>%
              filter(GENHLTH != 7,
                     !is.na(GENHLTH))
testSet3 %>% select(GENHLTH) %>% group_by(GENHLTH) %>% summarise(n = n())

#PHYSHLTH: quant variable with values 1-30 as valid (X days in 30 physical health not good)
#88 is none, 77 is don't know, 99 refused
#none of those three remaining
#460 NAs - a bit too many to remove, so will code 0 (median val)
summary(testSet3$PHYSHLTH)
testSet3$PHYSHLTH <- replace(testSet3$PHYSHLTH, is.na(testSet3$PHYSHLTH),0)

#MENTHLTH: quant variable, same scale as PHYSHLTH
#coding NAs to median of 0
summary(testSet3$MENTHLTH)
testSet3$MENTHLTH <- replace(testSet3$MENTHLTH, is.na(testSet3$MENTHLTH),0)

#POORHLTH: same scale as PHYS and MENT, skipped if 0s for previous
#will replace NA with 0 here
testSet3$POORHLTH <- replace(testSet3$POORHLTH, is.na(testSet3$POORHLTH),0)

#HLTHPLN1: binary variable about existence of health insurance
#want to be a true binary; N/As are going into yes (overwhelming majority)
testSet3 %>% select(HLTHPLN1) %>% group_by(HLTHPLN1) %>% summarize(n = n())
testSet3$HLTHPLN1 <- replace(testSet3$HLTHPLN1,is.na(testSet3$HLTHPLN1),1)

#MEDCOST - binary variable asking if doctor was prohibitive due to cost
#want to make this a true binary - don't knows and not sures to be removed
testSet3 %>% select(MEDCOST) %>% group_by(MEDCOST) %>% summarize(n = n())
testSet4 <- testSet3 %>%
              filter(MEDCOST != 7,
                     !is.na(MEDCOST))
testSet4 %>% select(MEDCOST) %>% group_by(MEDCOST) %>% summarize(n = n())

#CHECKUP1: similar scale as other 1-4 qs.
#1 and 2 are within X years ago, 3 is within 5 years ago (switch to 5)
#4 is more than 5 years ago (switch to 6)
#7 is don't know, 8 is never. 4 NAs
testSet4 %>% select(CHECKUP1) %>% group_by(CHECKUP1) %>% summarize(n = n())
testSet4$CHECKUP1 <- replace(testSet4$CHECKUP1, is.na(testSet4$CHECKUP1),0)
testSet4$CHECKUP1 <- replace(testSet4$CHECKUP1, testSet4$CHECKUP1 == 8,0)
testSet4$CHECKUP1 <- replace(testSet4$CHECKUP1, testSet4$CHECKUP1 == 7,0)
testSet4$CHECKUP1 <- replace(testSet4$CHECKUP1, testSet4$CHECKUP1 == 4,6)
testSet4$CHECKUP1 <- replace(testSet4$CHECKUP1, testSet4$CHECKUP1 == 3,5)
summary(testSet4$CHECKUP1)

#EDUCA: categorical variable with 1 = no school, 2 = elementary, 3 = high school (some),
#4 = high school graduate, 5 = some college, 6 = college grad/beyond. 9 = refused
#going to remove the 64 9s here
testSet4 %>% select(EDUCA) %>% group_by(EDUCA) %>% summarize(n = n())
testSet5 <- testSet4 %>% filter(EDUCA != 9)

#EMPLOY1: categorical variable: 1 = employed, 2 = self-employed, 3 = out of work 1+ yrs,
#4 = out of work < 1 year, 5 = homemaker, 6 = student, 7 = retired, 8 = unable to work
#9 = refused
#I want to combine 3 and 4 into just "out of work" == 3
#and I will remove NAs here as well
testSet5 %>% select(EMPLOY1) %>% group_by(EMPLOY1) %>% summarize(n = n())
testSet6 <- testSet5 %>% filter(EMPLOY1 != 9)
testSet6$EMPLOY1 <- replace(testSet6$EMPLOY1, testSet6$EMPLOY1 == 4, 3)
testSet6 %>% select(EMPLOY1) %>% group_by(EMPLOY1) %>% summarize(n = n())

#CHILDREN: quantitative variable, number of children in house
kids <- testSet6 %>% select(CHILDREN) %>% group_by(CHILDREN) %>% summarize(n = n())
#88 is 0, 99 is refused - will code both to 0 (0 is median)
testSet6$CHILDREN <- replace(testSet6$CHILDREN, testSet6$CHILDREN == 88, 0)
summary(testSet6$CHILDREN)
testSet6$CHILDREN <- replace(testSet6$CHILDREN, testSet6$CHILDREN == 99, 0)

#INCOME2: categorical variable representing income brackets on household income
#levels are <10k, <15k, <20k, <25k, <35k, <50k, <75k, >=75k
testSet6 %>% select(INCOME2) %>% group_by(INCOME2) %>% summarize(n = n())
#this is highly categorical and I'm kicking myself for not using _INCOMG instead
#will calculate _INCOMG and remove INCOME2
#_INCOMG will have 5 levels: <15k, <25k, <35k, <50k, 50k+
#there are a lot of NAs here so will replace with median after re-grouping

testSet7 <- testSet6 %>% 
              mutate(INCOMG = case_when(INCOME2 == 1 | INCOME2 == 2 ~ 1,
                                         INCOME2 == 3 | INCOME2 == 4 ~ 2,
                                         INCOME2 == 5 ~ 3,
                                         INCOME2 == 6 ~ 4,
                                         INCOME2 == 7 | INCOME2 == 8 ~ 5,
                                         .default = 99)) %>%
              select(-INCOME2)

testSet7 %>% filter(INCOMG != 99) %>% summarize(med = median(INCOMG))
#median value is 4, will replace 99s with 4
testSet7$INCOMG <- replace(testSet7$INCOMG, testSet7$INCOMG == 99,4)

#INTERNET: Binary variable about internet usage in past 30 days
#making a true binary, moving 7s to no and 9s to yes
testSet7 %>% select(INTERNET) %>% group_by(INTERNET) %>% summarise(n = n())
testSet7$INTERNET <- replace(testSet7$INTERNET, testSet7$INTERNET == 7, 2)
testSet7$INTERNET <- replace(testSet7$INTERNET, testSet7$INTERNET == 9, 1)

#WEIGHT2: 'weight without shoes", measured in either pounds or kgs, whichever was provided
#going to convert to pounds, about 1320 need conversion from kgs or from a null value
#will convert missing to 0 and find median of remaining (non-0 values), then sub missing with median
testSet7 %>% filter(WEIGHT2 >= 9000) %>% summarise(n = n())
testSet7 <- testSet7 %>%
              mutate(WEIGHTLBS = case_when(WEIGHT2 == 9999 ~ 0,
                                           WEIGHT2 == 7777 ~ 0,
                                           WEIGHT2 < 9000 ~ WEIGHT2,
                                           .default = (WEIGHT2 - 9000) * 2.20462))
testSet7 %>% select(WEIGHTLBS) %>% filter(WEIGHTLBS != 0) %>% summarise(med = median(WEIGHTLBS))
testSet7$WEIGHTLBS <- replace(testSet7$WEIGHTLBS, testSet7$WEIGHTLBS == 0, 150)
testSet7 <- testSet7 %>% select(-WEIGHT2)                     
summary(testSet7$WEIGHTLBS)
#confirmed there are several outliers of sizable weight, incl. 700 pounds, in original data

#HTIN4: height in inches (this is a cheating calc field)
summary(testSet7$HTIN4)
#converting NAs to median
testSet7 %>% filter(!is.na(HTIN4)) %>% summarize(med = median(HTIN4))
testSet7$HTIN4 <- replace(testSet7$HTIN4, is.na(testSet7$HTIN4),64)

#SMOKE100: binary - have you smoked 100 cigarettes/5 packs in your life?
#7 and 9s are few, converting them to 2s
testSet7 %>% group_by(SMOKE100) %>% summarise(n = n())
testSet7$SMOKE100 <- replace(testSet7$SMOKE100, testSet7$SMOKE100 == 7, 2)
testSet7$SMOKE100 <- replace(testSet7$SMOKE100, testSet7$SMOKE100 == 9, 2)

#I actually didn't realize USENOW3 was just smokeless tobacco and not all forms of tobacco
#this is incredibly limited so I'm going to remove the variable
testSet8 <- testSet7 %>%
              select(-USENOW3)

#ALCDAY5: quantitative variable that is days per week OR days in the last month with drinking
#converting missing, don't know, and never to 0
#converting "in last month" to itself minus prefix
#converting "per week" to itself minus prefix * 4 weeks
#resulting column will be days in last 30 days
alc <- testSet9 %>%
          group_by(ALCDAYSCLEAN) %>%
          summarize(n = n())
testSet9 <- testSet8 %>%
              mutate(ALCDAYSCLEAN = case_when(ALCDAY5 == 999 ~ 0,
                                              ALCDAY5 == 888 ~ 0,
                                              ALCDAY5 == 777 ~ 0,
                                              ALCDAY5 > 200 ~ ALCDAY5 - 200,
                                              .default = (ALCDAY5 - 100) * 4)) %>%
              select(-ALCDAY5)

#need a new df name to make autocorrect easier lol
#_FRUTSUM: calculated fruits per day
#this needs to be converted down to the two decimal places - dividing by 100
summary(testSet9$`_FRUTSUM`)
newTestSet <- testSet9 %>%
                mutate(SUMFRUIT = `_FRUTSUM`/100) %>%
                select(-`_FRUTSUM`)
summary(newTestSet$SUMFRUIT)
#will convert NAs to median value
newTestSet %>% filter(!is.na(SUMFRUIT)) %>% summarise(med = median(SUMFRUIT))
newTestSet$SUMFRUIT <- replace(newTestSet$SUMFRUIT, is.na(newTestSet$SUMFRUIT),1.07)
#note: there's an outlier here with someone who's chomping on 109 fruits per day

#_VEGESUM: calculated veg per day
#same conversion as _FRUTSUM
newTestSet <- newTestSet %>%
                mutate(SUMVEG = `_VEGESUM`/100) %>%
                select(-`_VEGESUM`)
summary(newTestSet$SUMVEG)
newTestSet %>% filter(!is.na(SUMVEG)) %>% summarise(med = median(SUMVEG))
newTestSet$SUMVEG <- replace(newTestSet$SUMVEG, is.na(newTestSet$SUMVEG),1.86)

#STRENGTH:how many times per day/week do you do strength exercise?
#needs converting like drinks
strength <- newTestSet2 %>%
              group_by(STRENCLEAN) %>%
              summarize(n = n())
newTestSet2 <- newTestSet %>%
                  mutate(STRENCLEAN = case_when(STRENGTH == 999 ~ 0,
                                                STRENGTH == 888 ~ 0,
                                                STRENGTH == 777 ~ 0,
                                                STRENGTH > 200 ~ STRENGTH - 200,
                                                .default = (STRENGTH - 100) * 4)) %>%
                  select(-STRENGTH)
summary(newTestSet2$STRENCLEAN)
#there are now a handful of outliers that I'm going to convert to 30; 
#I think this is multiple times per day
#should be considered equivalent to number of days in a month that exercise is performed

newTestSet2$STRENCLEAN <- replace(newTestSet2$STRENCLEAN, newTestSet2$STRENCLEAN == 240, 30)
newTestSet2$STRENCLEAN <- replace(newTestSet2$STRENCLEAN, newTestSet2$STRENCLEAN == 396, 30)
newTestSet2$STRENCLEAN <- replace(newTestSet2$STRENCLEAN, newTestSet2$STRENCLEAN > 30, 30)

#EXEROFT1: times per week or month doing the single physical activity performed most
#will clean similar to STRENGTH and ALCDAY5
exercise <- newTestSet3 %>%
              group_by(EXERTIMES) %>%
              summarise(n = n())

#there are outliers here that are multiple times per day, so will round those down to 30
#like alcohol and strength, want this to be number of days in a month
newTestSet3 <- newTestSet2 %>%
                mutate(EXERTIMES = case_when(EXEROFT1 == 999 ~ 0,
                                             is.na(EXEROFT1) ~ 0,
                                             EXEROFT1 == 777 ~ 0,
                                             EXEROFT1 > 230 ~ 30,
                                             EXEROFT1 > 200 ~ EXEROFT1 - 200,
                                             EXEROFT1 > 107 ~ 30,
                                             .default = (EXEROFT1 - 100) * 4)) %>%
                select(-EXEROFT1)


#I don't understand the coding on EXERHMM1 so removing
newTestSet4 <- newTestSet3 %>% select(-EXERHMM1)

#MAXVO2_: estimated maximum oxygen consumption
#needs missing values recoded and needs to be divided by 100
#will code missing values to the median
newTestSet4 <- newTestSet4 %>%
                mutate(MAXVO2 = MAXVO2_/100)

o2 <- newTestSet4 %>% group_by(MAXVO2) %>% summarise(n = n())
newTestSet4 %>% filter(MAXVO2 != 999) %>% summarise(med = median(MAXVO2))
newTestSet4$MAXVO2 <- replace(newTestSet4$MAXVO2, newTestSet4$MAXVO2 == 999, 26.2)
newTestSet4 <- newTestSet4 %>% select(-MAXVO2_)

#FC60: estimated functional capacity in two decimal places
#like MAXVO2, will divide by 100 and code any missing values to the median
newTestSet4 <- newTestSet4 %>%
                mutate(FUNCCAP = FC60_/100)
summary(newTestSet4$FUNCCAP)
newTestSet4 %>% filter(FUNCCAP != 999) %>% summarise(med = median(FUNCCAP))
newTestSet4$FUNCCAP <- replace(newTestSet4$FUNCCAP, newTestSet4$FUNCCAP == 999, 4.49)
newTestSet4 <- newTestSet4 %>% select(-FC60_)

#to simplify the analysis, I removed the "second activity" variables
#however, ACTIN11_ and ACTIN21_ classify both activities as moderate, vigorous, or neither
#I want to combine these into a categorical variable: moderate activity, vigorous activity, or no activity
#1 = moderate, 2 = vigorous, 0 = none, blank = missing
newTestSet4$ACTIN11_ <- as.integer(newTestSet4$ACTIN11_)
newTestSet4$ACTIN21_ <- as.integer(newTestSet4$ACTIN21_)

summary(newTestSet4$ACTIN11_)
summary(newTestSet4$ACTIN21_)


newTestSet5 <- newTestSet4 %>%
                mutate(ACTIVLVL = case_when(ACTIN11_ == 2 | ACTIN21_ == 2 ~ 2,
                                            ACTIN11_ == 1 | ACTIN21_ == 1 ~ 1,
                                            ACTIN11_ == 0 ~ 0,
                                            ACTIN21_ == 0 ~ 0,
                                            .default = 99))

activTest <- newTestSet5 %>%
                  mutate(Activity1 = ifelse(ACTIN11_ != 1 | ACTIN11_ != 2,0,1),
                         Activity2 = ifelse(ACTIN21_ != 1 | ACTIN21_ != 2,0,1)) %>%
                  group_by(ACTIVLVL, ACTIN11_, ACTIN21_) %>%
                  summarize(n = n(),
                            sumAct1 = sum(Activity1),
                            sumAct2 = sum(Activity2))
#final clean: convert 99 to 0, remove ACTIN11_ and ACTIN21_
newTestSet5$ACTIVLVL <- replace(newTestSet5$ACTIVLVL, newTestSet5$ACTIVLVL == 99, 0)
newTestSet5 <- newTestSet5 %>% select(-ACTIN11_)
newTestSet5 <- newTestSet5 %>% select(-ACTIN21_)

#_PAINDX1: calculated binary variable, want to clean to true binary
#measure of physical activity index - 1 is meets aerobic recs, 2 is does not
#there are about 800 9s in this group, but the split is quite close between 1 and 2
#I'll move 9s where activity level is 0 and 1 to "did not meet"
#9s where activity level is 2 to "did meet"
newTestSet5 %>% group_by(ACTIVLVL, `_PAINDX1`) %>% summarize(n = n())

newTestSet6 <- newTestSet5 %>%
                mutate(AERORECS = case_when(ACTIVLVL == 0 & `_PAINDX1` == 9 ~ 2,
                                            ACTIVLVL == 2 & `_PAINDX1` == 9 ~ 1,
                                            ACTIVLVL == 1 & `_PAINDX1` == 9 ~ 2,
                                            .default = `_PAINDX1`)) %>%
                select(-`_PAINDX1`)

summary(newTestSet6$AERORECS)
newTestSet6 %>% group_by(AERORECS) %>% summarize(n = n())

#SEATBELT: how often do you use a seatbelt? Categorical variable with 5 levels
#7 is don't know, 8 is "never ride in a car", 9 is refused
#there are less than 100 responses with 7,8, or 9 - will just remove
summary(newTestSet6$SEATBELT)
newTestSet6 %>% group_by(SEATBELT) %>% summarise(n = n())

newTestSet6 <- newTestSet6 %>%
                filter(SEATBELT < 6)

#FLUSHOT6 - binary variable, have you had a flu vaccine in last 12 months?
#want to turn into a true binary - will move "don't know" to no, 
#will remove 8 rows without response
summary(newTestSet6$FLUSHOT6)
newTestSet6 %>% group_by(FLUSHOT6) %>% summarise(n = n())

newTestSet6$FLUSHOT6 <- replace(newTestSet6$FLUSHOT6, newTestSet6$FLUSHOT6 == 7,2)
newTestSet6 <- newTestSet6 %>%
                filter(FLUSHOT6 != 9)

#HIVTST6 - have you ever been tested for HIV? binary variable, 1 is yes and 2 is no
#will add 7s and 9s to median value of no 
summary(newTestSet6$HIVTST6)
newTestSet6 %>% filter(HIVTST6 < 3) %>% summarize(med = median(HIVTST6))
newTestSet6 %>% group_by(HIVTST6) %>% summarise(n = n())

newTestSet6$HIVTST6 <- replace(newTestSet6$HIVTST6, newTestSet6$HIVTST6 == 7, 2)
newTestSet6$HIVTST6 <- replace(newTestSet6$HIVTST6, newTestSet6$HIVTST6 == 9, 2)

#CAREGIV1: have you provided regular care or assistance to someone with a disability in last 30 days?
#replacing 7 and 9 with 2, na with 2, 8 with 1
summary(newTestSet6$CAREGIV1)
newTestSet6 %>% group_by(CAREGIV1) %>% summarise(n = n())
newTestSet6$CAREGIV1 <- replace(newTestSet6$CAREGIV1, newTestSet6$CAREGIV1 == 7, 2)
newTestSet6$CAREGIV1 <- replace(newTestSet6$CAREGIV1, newTestSet6$CAREGIV1 == 8, 1)
newTestSet6$CAREGIV1 <- replace(newTestSet6$CAREGIV1, newTestSet6$CAREGIV1 == 9, 2)
newTestSet6$CAREGIV1 <- replace(newTestSet6$CAREGIV1, is.na(newTestSet6$CAREGIV1), 2)

#_ageg5yr: 5-year age group from 18 to 80+
ages <- newTestSet6 %>%
          group_by(`_AGEG5YR`) %>%
          summarize(n = n())
#251 without age, will remove those
newTestSet6 <- newTestSet6 %>% filter(`_AGEG5YR` != 14)

#HADMAM: binary asking if they have ever had a mammogram
#a handful of 7s and 9s; will just remove those
newTestSet6 <- newTestSet6 %>% filter(HADMAM < 3)
testSet2 %>% group_by(HADMAM) %>% summarise(n = n())

summary(newTestSet6)

#need to scale variables
#will scale variables that are of the form "X days in last 30 days" with decimal scaling
#htin and weight will be scaled with z-score
#min-max used for sumfruit, sumveg, maxvo2

#starting with z-scores
meanWt <- mean(newTestSet6$WEIGHTLBS)
sdWt <- sd(newTestSet6$WEIGHTLBS)

normTestSet1 <- newTestSet6 %>%
                  mutate(WEIGHTNORM = (WEIGHTLBS - meanWt)/sdWt)
summary(normTestSet1$WEIGHTNORM)

normTestSet1 <- normTestSet1 %>% select(-WEIGHTLBS)

meanHt <- mean(normTestSet1$HTIN4)
sdHt <- sd(normTestSet1$HTIN4)

normTestSet1 <- normTestSet1 %>%
                  mutate(HEIGHTNORM = (HTIN4 - meanHt)/sdHt)
summary(normTestSet1$HEIGHTNORM)
normTestSet1 <- normTestSet1 %>% select(-HTIN4)

#min-max scaling
#will scale each from 0-6 - in line with scale of other variables

targetMin <- 0
targetMax <- 6
targetRange <- targetMax - targetMin

minFruit <- min(normTestSet1$SUMFRUIT)
maxFruit <- max(normTestSet1$SUMFRUIT)
rangeFruit <- maxFruit - minFruit

minVeg <- min(normTestSet1$SUMVEG)
maxVeg <- max(normTestSet1$SUMVEG)
rangeVeg <- maxVeg - minVeg

minO2 <- min(normTestSet1$MAXVO2)
maxO2 <- max(normTestSet1$MAXVO2)
rangeO2 <- maxO2 - minO2

normTestSet2 <- normTestSet1 %>%
                  mutate(FRUITNORM = (((SUMFRUIT - minFruit)/rangeFruit)*targetRange),
                         VEGNORM = (((SUMVEG - minVeg)/rangeVeg)*targetRange),
                         MAXVO2NORM = (((MAXVO2 - minO2)/rangeO2)*targetRange),
                  ) %>%
                  select(-all_of(c('SUMFRUIT','SUMVEG','MAXVO2')))
summary(normTestSet2)

#decimal scaling for variables in the X days in last 30 days group
#max here is 30 so will divide by 10 - max will be 3, min stays 0
normTestSet3 <- normTestSet2 %>%
                  mutate(NORMPHYS = PHYSHLTH/10,
                         NORMMENT = MENTHLTH/10,
                         NORMPOOR = POORHLTH/10,
                         NORMALC = ALCDAYSCLEAN/10,
                         NORMSTREN = STRENCLEAN/10,
                         NORMEXER = EXERTIMES/10) %>%
                  select(-all_of(c('PHYSHLTH','MENTHLTH','POORHLTH','ALCDAYSCLEAN',
                                   'STRENCLEAN','EXERTIMES')))

summary(normTestSet3)
#all variables now in same scale, ~-2 to ~13


#final look and converting to .csv
finalTestSet <- normTestSet3

#happy with this dataset - will convert to csv and start new file for analysis
setwd('/Users/alexandradegrandchamp/Documents/GradSchool/DSC424/Final Project/SelfAnalysis')
cwd <- getwd()
fullFN <- 'clusteringSet.csv'
write.csv(finalTestSet, paste(cwd,fullFN,sep='/'), row.names = FALSE)
