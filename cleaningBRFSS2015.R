#libraries used
library(tidyverse)
library(naniar)

#importing data, first peeks
summary(X2015)

#joining in states (copied from pdf as separate .txt file)
states_clean <- states %>%
                  select(Value,Value_1)
states_clean <- na.omit(states_clean)
states_clean$Value <- as.double(states_clean$Value)

dataset2015 <- X2015 %>%
                left_join(states_clean, by = c('_STATE' = 'Value'))

#cleaning date values
dataset2015$IDATE <- str_sub(dataset2015$IDATE,start = 3,-1)
dataset2015$IDATE <- mdy(dataset2015$IDATE)
dataset2015$IMONTH <- month(dataset2015$IDATE)
dataset2015$IDAY <- day(dataset2015$IDATE)
dataset2015$IYEAR <- year(dataset2015$IDATE)

#gathering only complete rows
completedInterviews <- dataset2015 %>%
                        filter(DISPCODE == 1100)

#taking a look at some intro question dispersions
summary(completedInterviews$CTELENUM)
summary(completedInterviews$PVTRESD1)
summary(completedInterviews$COLGHOUS)

#cleaning up and factorizing state values
completedInterviews <- completedInterviews %>%
                        rename('StateText' = Value_1)
completedInterviews$StateText <- as.factor(completedInterviews$StateText)

#checking for empty columns
summary(completedInterviews)

#not all qs asked in 2015
interviewsNoBlankColumns <- completedInterviews %>%
                              select(-c('COLGHOUS','LADULT','PAINACT2',
                                        'QLMENTL2','QLSTRES2','QLHLTH2',
                                        'ASERVIST','ASDRVIST'))


#looking for incomplete cases based on survey flow
testingState <- interviewsNoBlankColumns %>%
                  filter(STATERES != 1)
rm(testingState) #0 results
testingCell <- interviewsNoBlankColumns %>%
                  filter(CELLFON3 == 2)
rm(testingCell) #doesn't actually terminate survey

#testing cell a second time
testingCell2 <- interviewsNoBlankColumns %>%
                  filter(CELLFON3 == 2 & CELLFON2 == 2)
rm(testingCell2) #0 results

#rounding some funky looking should-be-integers-but-aren't
interviewsNoBlankColumns$NUMADULT <- as.integer(interviewsNoBlankColumns$NUMADULT)
interviewsNoBlankColumns$NUMMEN <- as.integer(interviewsNoBlankColumns$NUMMEN)
interviewsNoBlankColumns$NUMWOMEN <- as.integer(interviewsNoBlankColumns$NUMWOMEN)

#"duplicate" column cleanup
#this dataset aggregates the land-line and cell-phone surveys, with different variables for each
#this merges columns together or removes them if they are irrelevant

#CTELENUM - land line ask if number is correct
#CTELNUM1 - cell phone ask if number is correct
summary(interviewsNoBlankColumns$CTELENUM)
summary(interviewsNoBlankColumns$CTELNUM1)
#these are irrelevant columns

deDupe <- interviewsNoBlankColumns %>%
            select(-c('CTELENUM','CTELNUM1'))

#PVTRESD1 - land line ask if number is private residence
#PVTRESD2 - cell phone ask if number is private residence
summary(deDupe$PVTRESD1)
summary(deDupe$PVTRESD2)

deDupe2 <- deDupe %>%
            unite('PVTRES',c('PVTRESD1','PVTRESD2'), na.rm = TRUE) 
deDupe2$PVTRES <- as.integer(deDupe2$PVTRES)
summary(deDupe2$PVTRES)

#COLGHOUS - land line ask if number is college housing, removed from set
#CCLGHOUS - cell phone ask if number is college housing, kept in set
#no changes here just noting the duplication has been handled

#NUMADULT - land line ask for number of adults in household
#HHADULT - cell phone ask for number of adults in household
summary(deDupe2$NUMADULT)
summary(deDupe2$HHADULT)

deDupe3 <- deDupe2 %>%
            unite('NUMADULTS',c('NUMADULT','HHADULT'), na.rm = TRUE) 
#getting NAs when converting to numeric, so seeing what is up there
deDupe3$NUMADULTS <- as.factor(deDupe3$NUMADULTS)
numAdultsCheck <- deDupe3 %>%
                    group_by(NUMADULTS) %>%
                    summarize(n = n())
#further cleaning - 77 is don't know, 99 is refused to answer - will turn those to N/As for now
#will also turn '' into N/A
deDupe3 <- deDupe3 %>%
            replace_with_na(replace = list(NUMADULTS = c('77','99','')))
#there's also one row with a 4_3, which means on landline someone said 4 
#and the same someone said 3 on a cellphone
#that shouldn't happen either, but deleting it is giving me trouble, so changing it to NA by coercion
deDupe3$NUMADULTS <- as.character(deDupe3$NUMADULTS)
deDupe3$NUMADULTS <- as.integer(deDupe3$NUMADULTS)
summary(deDupe3$NUMADULTS)

#id or duplicated columns - will create subset without
irrelevantColumns = c('IMONTH','IDAY','IYEAR','DISPCODE','FMONTH','_STATE', 'STATERES','CELLFON3',
                      'CELLFON2')
#note: keeping SEQNO and PSU to join back together if data is separated

relevantData <- deDupe3 %>%
                  select(-all_of(irrelevantColumns))

#starting on actual questions cleaning
#GENHLTH - categorical where 9 is refused
cleaningVars <- relevantData %>%
                  replace_with_na(replace = list(GENHLTH = 9))
cleaningVars$GENHLTH <- as.factor(cleaningVars$GENHLTH)

#PHYSHLTH - replacing 88 with 0, 77 and 99 with N/A
cleaningVars$PHYSHLTH[cleaningVars$PHYSHLTH == 88] <- 0
cleaningVars <- cleaningVars %>%
                  replace_with_na(replace = list(PHYSHLTH = c(77,99)))

#MENTHLTH - replacing 88 with 0, 77 and 99 with N/A
cleaningVars$MENTHLTH[cleaningVars$MENTHLTH == 88] <- 0
cleaningVars <- cleaningVars %>%
                  replace_with_na(replace = list(MENTHLTH = c(77,99)))

#POORHLTH - replacing 88 with 0, 77 and 99 with N/A
#note: skipped q if 0 days in either PHYSHLTH or MENTHLTH
cleaningVars$POORHLTH[cleaningVars$POORHLTH == 88] <- 0
cleaningVars <- cleaningVars %>%
                  replace_with_na(replace = list(POORHLTH = c(77,99)))

#HLTHPLN1 - changing 7 and 9 with N/A. This is a categorical variable
cleaningVars <- cleaningVars %>%
                  replace_with_na(replace = list(HLTHPLN1 = c(7,9)))
cleaningVars$HLTHPLN1 <- as.factor(cleaningVars$HLTHPLN1)

#PERSDOC2 - changing 9 with N/A. This is a categorical variable
cleaningVars <- cleaningVars %>%
                  replace_with_na(replace = list(PERSDOC2 = c(9)))
cleaningVars$PERSDOC2 <- as.factor(cleaningVars$PERSDOC2)

#MEDCOST - changing 9 with N/A. This is a categorical variable
cleaningVars <- cleaningVars %>%
                  replace_with_na(replace = list(MEDCOST = c(9)))
cleaningVars$MEDCOST <- as.factor(cleaningVars$MEDCOST)

#CHECKUP1 - changing 9 to N/A. This is a categorical variable
cleaningVars <- cleaningVars %>%
                  replace_with_na(replace = list(CHECKUP1 = c(9)))
cleaningVars$MEDCOST <- as.factor(cleaningVars$MEDCOST)

#this is about the point where I realized this was going to take 100 years this way
#switching it up
#read through the codebook and put each variable in either quant or cat
#I also classified weighting/coding variables and identified calculated fields separately (for multicollinearity concerns)
#IDATE, SEQNO, _PSU kept in each set as identifiers

quantVars <- cleaningVars %>%
              select(IDATE, SEQNO, `_PSU`, NUMADULTS, NUMMEN, NUMWOMEN, PHYSHLTH,
                     MENTHLTH, POORHLTH, DIABAGE2, NUMPHON2, CHILDREN, WEIGHT2,
                     HEIGHT3, ALCDAY5, AVEDRNK2, DRNK3GE5, MAXDRNKS, FRUITJU1,
                     FRUIT1, FVBEANS, FVGREEN, FVORANG, VEGETAB1, EXEROFT1, EXERHMM1, EXEROFT2, 
                     EXERHMM2, STRENGTH, BLDSUGAR, FEETCHK2, DOCTDIAB, CHKHEMO3, FEETCHK, LONGWTCH,
                     ASTHMAGE, ASRCHKUP, ASACTLIM, SCNTWRK1, SCNTLWK1, ADPLEASR, ADDOWN, ADSLEEP,
                     ADENERGY, ADEAT1, ADFAIL, ADTHINK, ADMOVE)

catVars <- cleaningVars %>%
              select(IDATE, SEQNO, `_PSU`, StateText, PVTRES, CCLGHOUS, CSTATE, LANDLINE,
                     GENHLTH, HLTHPLN1, PERSDOC2, MEDCOST, CHECKUP1, BPHIGH4, BPMEDS,
                     BLOODCHO, CHOLCHK, TOLDHI2, CVDINFR4, CVDCRHD4, CVDSTRK3, ASTHMA3,
                     ASTHNOW, CHCSCNCR, CHCOCNCR, CHCCOPD1, HAVARTH3, ADDEPEV2,
                     CHCKIDNY, DIABETE3, SEX, MARITAL, EDUCA, RENTHOM1, NUMHHOL2,
                     CPDEMO1, VETERAN3, EMPLOY1, INCOME2, INTERNET, PREGNANT, QLACTLM2,
                     USEEQUIP, BLIND, DECIDE, DIFFWALK, DIFFDRES, DIFFALON, SMOKE100,
                     SMOKDAY2, STOPSMK2, LASTSMK2, USENOW3, EXERANY2, EXRACT11, EXRACT21,
                     LMTJOIN3, ARTHDIS2, ARTHSOCL, JOINPAIN, SEATBELT, FLUSHOT6, FLSHTMY2, IMFVPLAC,
                     PNEUVAC3, HIVTST6, HIVTSTD3, WHRTST10, PDIABTST, PREDIAB1, INSULIN,
                     EYEEXAM, DIABEYE, DIABEDU, CAREGIV1, CRGVREL1, CRGVLNG1, CRGVHRS1,
                     CRGVPRB1, CRGVPERS, CRGVHOUS, CRGVMST2, CRGVEXPT, VIDFCLT2, VIREDIF3,
                     VIPRFVS2, VINOCRE2, VIEYEXM2, VIINSUR2, VICTRCT4, VIGLUMA2, VIMACDG2,
                     CIMEMLOS, CDHOUSE, CDASSIST, CDHELP, CDSOCIAL, CDDISCUS, WTCHSALT,
                     DRADVISE, ASATTACK, ASYMPTOM, ASNOSLEP, ASTHMED3, ASINHALR, HAREHAB1,
                     STREHAB1, CVDASPRN, ASPUNSAF, RLIVPAIN, RDUCHART, RDUCSTRK, ARTTODAY,
                     ARTHWGT, ARTHEXER, ARTHEDU, TETANUS, HPVADVC2, HPVADSHT, SHINGLE2,HADMAM,
                     HOWLONG, HADPAP2, LASTPAP2, HPVTEST, HPLSTTST, HADHYST2, PROFEXAM,LENGEXAM,
                     BLDSTOOL, LSTBLDS3, HADSIGM3, HADSGCO1, LASTSIG3, PCPSAAD2, PCPSADI1,
                     PCPSARE1, PSATEST1, PSATIME, PCPSARS1, PCPSADE1, PCDMDECN, SCNTMNY1,
                     SCNTMEL1, SCNTPAID, SCNTLPAD, SXORIENT, TRNSGNDR, RCSGENDR, RCSRLTN2, CASTHDX2,
                     CASTHNO2, EMTSUPRT, LSATISFY, MISTMNT, ADANXEV, QSTVER, QSTLANG, EXACTOT1,
                     EXACTOT2, `_CHISPNC`, `_CRACE1`, `_CPRACE`, `_DUALUSE`)

codingVars <- cleaningVars %>%
                select(IDATE, SEQNO, `_PSU`, MSCODE, `_STSTR`, `_STRWT`, `_RAWRAKE`,
                       `_WT2RAKE`, `_CLLCPWT`, `_DUALCOR`, `_LLCPWT`)

calcVars <- cleaningVars %>%
              select(IDATE, SEQNO, `_PSU`, `_RFHLTH`, `_HCVU651`, `_RFHYPE5`, `_CHOLCHK`,
                     `_RFCHOL`, `_MICHD`, `_LTASTH1`, `_CASTHM1`, `_ASTHMS1`, `_DRDXAR1`,
                     `_PRACE1`, `_MRACE1`, `_HISPANC`, `_RACE`, `_RACEG21`, `_RACEGR3`,
                     `_RACE_G1`, `_AGEG5YR`, `_AGE65YR`, `_AGE80`, `_AGE_G`, HTIN4, HTM4,
                     WTKG3, `_BMI5`, `_BMI5CAT`, `_RFBMI5`, `_CHLDCNT`, `_EDUCAG`, `_INCOMG`,
                     `_SMOKER3`, `_RFSMOK3`, DRNKANY5, DROCDY3_, `_RFBING5`, `_DRNKWEK`,
                     `_RFDRHV5`, FTJUDA1_, FRUTDA1_, BEANDAY_, GRENDAY_, ORNGDAY_, VEGEDA1_,
                     `_MISFRTN`, `_MISVEGN`, `_FRTRESP`, `_VEGRESP`, `_FRUTSUM`, `_VEGESUM`,
                     `_FRTLT1`, `_VEGLT1`, `_FRT16`, `_VEG23`, `_FRUITEX`, `_VEGETEX`,
                     `_TOTINDA`, METVL11_, METVL21_, MAXVO2_, FC60_, ACTIN11_, ACTIN21_,
                     PADUR1_, PADUR2_, PAFREQ1_, PAFREQ2_, `_MINAC11`, `_MINAC21`, STRFREQ_,
                     PAMISS1_, PAMIN11_, PAMIN21_, PA1MIN_, PAVIG11_, PAVIG21_, PA1VIGM_,
                     `_PACAT1`, `_PAINDX1`, `_PA150R2`, `_PA300R2`, `_PA30021`, `_PASTRNG`,
                     `_PAREC1`, `_PASTAE1`, `_LMTACT1`, `_LMTWRK1`, `_LMTSCL1`, `_RFSEAT2`,
                     `_RFSEAT3`, `_FLSHOT6`, `_PNEUMO2`, `_AIDTST3`)


#exporting clean data set and subsets to csv files
setwd('/Users/alexandradegrandchamp/Documents/GradSchool/DSC424/Final Project/FinalistDatasets/BRFSS')
cwd <- getwd()
fullFN <- 'cleanedFullSet.csv'
write.csv(cleaningVars, paste(cwd,fullFN,sep='/'), row.names = FALSE)

quantFN <- 'quantData.csv'
catFN <- 'categoricalData.csv'
codingFN <- 'codingVariables.csv'
calcFN <- 'calculatedFields.csv'

write.csv(quantVars, paste(cwd,quantFN,sep='/'), row.names = FALSE)
write.csv(catVars, paste(cwd,catFN,sep='/'), row.names = FALSE)
write.csv(codingVars, paste(cwd,codingFN,sep='/'), row.names = FALSE)
write.csv(calcVars, paste(cwd,calcFN,sep='/'), row.names = FALSE)
