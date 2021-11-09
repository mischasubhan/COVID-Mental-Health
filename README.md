# COVID-Mental-Health
# load packages
library(data.table)
library(dplyr)
library(tidyverse)

############################
# CDC Data
############################
# read CDC daily covid case count data
cdc_cases<-fread("United_States_COVID-19_Cases_and_Deaths_by_State_over_Time.csv", header=TRUE)
view(cdc_cases)

# restrict data to dates of survey (below)
cdc_cases_v2 <- cdc_cases %>% 
  separate(submission_date, into = c("Month", "Day", "Year"), sep="/") %>%
  unite("MonthDay", Month:Day, sep="") %>%
  filter(as.numeric(MonthDay) >= 0423 & as.numeric(MonthDay) <= 1109) 

# convert MonthDay to numeric
cdc_cases_v2$MonthDay <- as.numeric(cdc_cases_v2$MonthDay)

# divide dates into weeks that correspond with household survey 
cdc_cases_v2$WEEK[cdc_cases_v2$MonthDay >= 423 & cdc_cases_v2$MonthDay <= 505] <- 1
cdc_cases_v2$WEEK[cdc_cases_v2$MonthDay >= 507 & cdc_cases_v2$MonthDay <= 512] <- 2
cdc_cases_v2$WEEK[cdc_cases_v2$MonthDay >= 514 & cdc_cases_v2$MonthDay <= 519] <- 3
cdc_cases_v2$WEEK[cdc_cases_v2$MonthDay >= 521 & cdc_cases_v2$MonthDay <= 526] <- 4
cdc_cases_v2$WEEK[cdc_cases_v2$MonthDay >= 528 & cdc_cases_v2$MonthDay <= 602] <- 5
cdc_cases_v2$WEEK[cdc_cases_v2$MonthDay >= 604 & cdc_cases_v2$MonthDay <= 609] <- 6
cdc_cases_v2$WEEK[cdc_cases_v2$MonthDay >= 611 & cdc_cases_v2$MonthDay <= 616] <- 7
cdc_cases_v2$WEEK[cdc_cases_v2$MonthDay >= 618 & cdc_cases_v2$MonthDay <= 623] <- 8
cdc_cases_v2$WEEK[cdc_cases_v2$MonthDay >= 625 & cdc_cases_v2$MonthDay <= 630] <- 9
cdc_cases_v2$WEEK[cdc_cases_v2$MonthDay >= 702 & cdc_cases_v2$MonthDay <= 707] <- 10
cdc_cases_v2$WEEK[cdc_cases_v2$MonthDay >= 709 & cdc_cases_v2$MonthDay <= 714] <- 11
cdc_cases_v2$WEEK[cdc_cases_v2$MonthDay >= 716 & cdc_cases_v2$MonthDay <= 721] <- 12
cdc_cases_v2$WEEK[cdc_cases_v2$MonthDay >= 819 & cdc_cases_v2$MonthDay <= 831] <- 13
cdc_cases_v2$WEEK[cdc_cases_v2$MonthDay >= 902 & cdc_cases_v2$MonthDay <= 914] <- 14
cdc_cases_v2$WEEK[cdc_cases_v2$MonthDay >= 916 & cdc_cases_v2$MonthDay <= 928] <- 15
cdc_cases_v2$WEEK[cdc_cases_v2$MonthDay >= 930 & cdc_cases_v2$MonthDay <= 1012] <- 16
cdc_cases_v2$WEEK[cdc_cases_v2$MonthDay >= 1014 & cdc_cases_v2$MonthDay <= 1026] <- 17
cdc_cases_v2$WEEK[cdc_cases_v2$MonthDay >= 1028 & cdc_cases_v2$MonthDay <= 1109] <- 18
#new cases per population
view(cdc_cases_v2)

# add total populations of each state
cdc_cases_v2$StatePop[cdc_cases_v2$state == "AL"] <- 4908620
cdc_cases_v2$StatePop[cdc_cases_v2$state == "AK"] <- 734002
cdc_cases_v2$StatePop[cdc_cases_v2$state == "AZ"] <- 7378490
cdc_cases_v2$StatePop[cdc_cases_v2$state == "AR"] <- 3039000
cdc_cases_v2$StatePop[cdc_cases_v2$state == "CA"] <- 39937500
cdc_cases_v2$StatePop[cdc_cases_v2$state == "CO"] <- 5845530
cdc_cases_v2$StatePop[cdc_cases_v2$state == "CT"] <- 3563080
cdc_cases_v2$StatePop[cdc_cases_v2$state == "DC"] <- 720687
cdc_cases_v2$StatePop[cdc_cases_v2$state == "DE"] <- 982895
cdc_cases_v2$StatePop[cdc_cases_v2$state == "FL"] <- 21993000
cdc_cases_v2$StatePop[cdc_cases_v2$state == "GA"] <- 10736100
cdc_cases_v2$StatePop[cdc_cases_v2$state == "HI"] <- 1412690
cdc_cases_v2$StatePop[cdc_cases_v2$state == "ID"] <- 1826160
cdc_cases_v2$StatePop[cdc_cases_v2$state == "IL"] <- 12659700
cdc_cases_v2$StatePop[cdc_cases_v2$state == "IN"] <- 6745350
cdc_cases_v2$StatePop[cdc_cases_v2$state == "IA"] <- 3179850
cdc_cases_v2$StatePop[cdc_cases_v2$state == "KS"] <- 2910360
cdc_cases_v2$StatePop[cdc_cases_v2$state == "KY"] <- 4499690
cdc_cases_v2$StatePop[cdc_cases_v2$state == "LA"] <- 4645180
cdc_cases_v2$StatePop[cdc_cases_v2$state == "ME"] <- 1345790
cdc_cases_v2$StatePop[cdc_cases_v2$state == "MD"] <- 6083120
cdc_cases_v2$StatePop[cdc_cases_v2$state == "MA"] <- 6976600
cdc_cases_v2$StatePop[cdc_cases_v2$state == "MI"] <- 10045000
cdc_cases_v2$StatePop[cdc_cases_v2$state == "MN"] <- 5700670
cdc_cases_v2$StatePop[cdc_cases_v2$state == "MS"] <- 2989260
cdc_cases_v2$StatePop[cdc_cases_v2$state == "MO"] <- 6169270
cdc_cases_v2$StatePop[cdc_cases_v2$state == "MT"] <- 1086760
cdc_cases_v2$StatePop[cdc_cases_v2$state == "NE"] <- 1952570
cdc_cases_v2$StatePop[cdc_cases_v2$state == "NV"] <- 3139660
cdc_cases_v2$StatePop[cdc_cases_v2$state == "NH"] <- 1371250
cdc_cases_v2$StatePop[cdc_cases_v2$state == "NJ"] <- 8936570
cdc_cases_v2$StatePop[cdc_cases_v2$state == "NM"] <- 2096640
cdc_cases_v2$StatePop[cdc_cases_v2$state == "NY"] <- 19440500
cdc_cases_v2$StatePop[cdc_cases_v2$state == "NC"] <- 10611900
cdc_cases_v2$StatePop[cdc_cases_v2$state == "ND"] <- 761723
cdc_cases_v2$StatePop[cdc_cases_v2$state == "OH"] <- 11747700
cdc_cases_v2$StatePop[cdc_cases_v2$state == "OK"] <- 3954820
cdc_cases_v2$StatePop[cdc_cases_v2$state == "OR"] <- 4301090
cdc_cases_v2$StatePop[cdc_cases_v2$state == "PA"] <- 12820900
cdc_cases_v2$StatePop[cdc_cases_v2$state == "RI"] <- 1056160
cdc_cases_v2$StatePop[cdc_cases_v2$state == "SC"] <- 5210100
cdc_cases_v2$StatePop[cdc_cases_v2$state == "SD"] <- 903027
cdc_cases_v2$StatePop[cdc_cases_v2$state == "TN"] <- 6897580
cdc_cases_v2$StatePop[cdc_cases_v2$state == "TX"] <- 29472300
cdc_cases_v2$StatePop[cdc_cases_v2$state == "UT"] <- 3282120
cdc_cases_v2$StatePop[cdc_cases_v2$state == "VT"] <- 628061
cdc_cases_v2$StatePop[cdc_cases_v2$state == "VA"] <- 8626210
cdc_cases_v2$StatePop[cdc_cases_v2$state == "WA"] <- 7797100
cdc_cases_v2$StatePop[cdc_cases_v2$state == "WV"] <- 1778070
cdc_cases_v2$StatePop[cdc_cases_v2$state == "WI"] <- 5851750
cdc_cases_v2$StatePop[cdc_cases_v2$state == "WY"] <- 567025

cdc_cases_v3 <- na.omit(cdc_cases_v2)
cdc_cases_v3$new_case <- as.numeric(cdc_cases_v3$new_case)
cdc_cases_v3$StatePop <- as.numeric(cdc_cases_v3$StatePop)

# find average cases by week and by state
cdc_cases_v4 <- cdc_cases_v3 %>%
  group_by(WEEK,state) %>%
  summarise(ave_cases=mean(new_case))

# add average cases by state by week to previous cdc dataset
cdc_cases_v5 <- full_join(cdc_cases_v4,cdc_cases_v3)
cdc_cases_v5$ave_cases <- round(cdc_cases_v5$ave_cases,0)

#rename state column to match corresponding column in survey data
colnames(cdc_cases_v5)[2] <- "ST_NAME"

# find average percentage of cases per state population for each day
cdc_cases_v5$case_per <- ((cdc_cases_v5$ave_cases/cdc_cases_v5$StatePop)*100)
view(cdc_cases_v5)

# select relevant columns (week, state, percent of cases compared to total population) to join with survey data
cdc_cases_v6 <- cdc_cases_v5 %>%
  select(WEEK, ST_NAME, case_per) 

# remove duplicates
cdc_cases_v7 <- cdc_cases_v6[!duplicated(cdc_cases_v6), ]

# add percentage of cases for each week to survey data

############################
# Census Data
############################

# read household pulse survey data
survey1<-fread("pulse2020_puf_01.csv", header=TRUE)
survey2<-fread("pulse2020_puf_02.csv", header=TRUE)
survey3<-fread("pulse2020_puf_03.csv", header=TRUE)
survey4<-fread("pulse2020_puf_04.csv", header=TRUE)
survey5<-fread("pulse2020_puf_05.csv", header=TRUE)
survey6<-fread("pulse2020_puf_06.csv", header=TRUE)
survey7<-fread("pulse2020_puf_07.csv", header=TRUE)
survey8<-fread("pulse2020_puf_08.csv", header=TRUE)
survey9<-fread("pulse2020_puf_09.csv", header=TRUE)
survey10<-fread("pulse2020_puf_10.csv", header=TRUE)
survey11<-fread("pulse2020_puf_11.csv", header=TRUE)
survey12<-fread("pulse2020_puf_12.csv", header=TRUE)
survey13<-fread("pulse2020_puf_13.csv", header=TRUE)
survey14<-fread("pulse2020_puf_14.csv", header=TRUE)
survey15<-fread("pulse2020_puf_15.csv", header=TRUE)
survey16<-fread("pulse2020_puf_16.csv", header=TRUE)
survey17<-fread("pulse2020_puf_17.csv", header=TRUE)
survey18<-fread("pulse2020_puf_18.csv", header=TRUE)

# join data
join_surveys_v2 <- rbind(survey1,survey2,survey3,survey4,survey5,survey6,survey7,survey8,survey9,survey10,survey11,survey12,survey13,survey14,survey15,survey16,survey17,survey18,fill=TRUE)

# view joined data
view(join_surveys_v2)
length(unique(join_surveys_v2$SCRAM))
nrow(join_surveys_v2)
# determine which columns are different across datasets
col_diff <- list(survey1,survey2,survey3,survey4,survey5,survey6,survey7,survey8,survey9,survey10,survey11,survey12,survey13,survey14,survey15,survey16,survey17,survey18)

compare_vars <- function(data_frame){
  x <- tibble(`var_name` = colnames(data_frame),
              `var_type` = sapply(data_frame, class))
  return(x)
}

final <- lapply(1:length(col_diff),function(i)compare_vars(col_diff[[i]]) %>% 
                   mutate(element =i)) %>%
  bind_rows() %>%
  spread(element, var_type)

# view column differences
view(final)

# determine which states survey participants are from (across all weeks)
join_surveys_v2 %>%
  distinct(EST_ST) %>%
  arrange(EST_ST)

# rename state codes to state abbreviations
join_surveys_v2$ST_NAME[join_surveys_v2$EST_ST == 01] <- "AL"
join_surveys_v2$ST_NAME[join_surveys_v2$EST_ST == 02] <- "AK"
join_surveys_v2$ST_NAME[join_surveys_v2$EST_ST == 04] <- "AZ"
join_surveys_v2$ST_NAME[join_surveys_v2$EST_ST == 05] <- "AR"
join_surveys_v2$ST_NAME[join_surveys_v2$EST_ST == 06] <- "CA"
join_surveys_v2$ST_NAME[join_surveys_v2$EST_ST == 08] <- "CO"
join_surveys_v2$ST_NAME[join_surveys_v2$EST_ST == 09] <- "CT"
join_surveys_v2$ST_NAME[join_surveys_v2$EST_ST == 10] <- "DE"
join_surveys_v2$ST_NAME[join_surveys_v2$EST_ST == 11] <- "DC"
join_surveys_v2$ST_NAME[join_surveys_v2$EST_ST == 12] <- "FL"
join_surveys_v2$ST_NAME[join_surveys_v2$EST_ST == 13] <- "GA"
join_surveys_v2$ST_NAME[join_surveys_v2$EST_ST == 15] <- "HI"
join_surveys_v2$ST_NAME[join_surveys_v2$EST_ST == 16] <- "ID"
join_surveys_v2$ST_NAME[join_surveys_v2$EST_ST == 17] <- "IL"
join_surveys_v2$ST_NAME[join_surveys_v2$EST_ST == 18] <- "IN"
join_surveys_v2$ST_NAME[join_surveys_v2$EST_ST == 19] <- "IA"
join_surveys_v2$ST_NAME[join_surveys_v2$EST_ST == 20] <- "KS"
join_surveys_v2$ST_NAME[join_surveys_v2$EST_ST == 21] <- "KY"
join_surveys_v2$ST_NAME[join_surveys_v2$EST_ST == 22] <- "LA"
join_surveys_v2$ST_NAME[join_surveys_v2$EST_ST == 23] <- "ME"
join_surveys_v2$ST_NAME[join_surveys_v2$EST_ST == 24] <- "MD"
join_surveys_v2$ST_NAME[join_surveys_v2$EST_ST == 25] <- "MA"
join_surveys_v2$ST_NAME[join_surveys_v2$EST_ST == 26] <- "MI"
join_surveys_v2$ST_NAME[join_surveys_v2$EST_ST == 27] <- "MN"
join_surveys_v2$ST_NAME[join_surveys_v2$EST_ST == 28] <- "MS"
join_surveys_v2$ST_NAME[join_surveys_v2$EST_ST == 29] <- "MO"
join_surveys_v2$ST_NAME[join_surveys_v2$EST_ST == 30] <- "MT"
join_surveys_v2$ST_NAME[join_surveys_v2$EST_ST == 31] <- "NE"
join_surveys_v2$ST_NAME[join_surveys_v2$EST_ST == 32] <- "NV"
join_surveys_v2$ST_NAME[join_surveys_v2$EST_ST == 33] <- "NH"
join_surveys_v2$ST_NAME[join_surveys_v2$EST_ST == 34] <- "NJ"
join_surveys_v2$ST_NAME[join_surveys_v2$EST_ST == 35] <- "NM"
join_surveys_v2$ST_NAME[join_surveys_v2$EST_ST == 36] <- "NY"
join_surveys_v2$ST_NAME[join_surveys_v2$EST_ST == 37] <- "NC"
join_surveys_v2$ST_NAME[join_surveys_v2$EST_ST == 38] <- "ND"
join_surveys_v2$ST_NAME[join_surveys_v2$EST_ST == 39] <- "OH"
join_surveys_v2$ST_NAME[join_surveys_v2$EST_ST == 40] <- "OK"
join_surveys_v2$ST_NAME[join_surveys_v2$EST_ST == 41] <- "OR"
join_surveys_v2$ST_NAME[join_surveys_v2$EST_ST == 42] <- "PA"
join_surveys_v2$ST_NAME[join_surveys_v2$EST_ST == 44] <- "RI"
join_surveys_v2$ST_NAME[join_surveys_v2$EST_ST == 45] <- "SC"
join_surveys_v2$ST_NAME[join_surveys_v2$EST_ST == 46] <- "SD"
join_surveys_v2$ST_NAME[join_surveys_v2$EST_ST == 47] <- "TN"
join_surveys_v2$ST_NAME[join_surveys_v2$EST_ST == 48] <- "TX"
join_surveys_v2$ST_NAME[join_surveys_v2$EST_ST == 49] <- "UT"
join_surveys_v2$ST_NAME[join_surveys_v2$EST_ST == 50] <- "VT"
join_surveys_v2$ST_NAME[join_surveys_v2$EST_ST == 51] <- "VA"
join_surveys_v2$ST_NAME[join_surveys_v2$EST_ST == 53] <- "WA"
join_surveys_v2$ST_NAME[join_surveys_v2$EST_ST == 54] <- "WV"
join_surveys_v2$ST_NAME[join_surveys_v2$EST_ST == 55] <- "WI"
join_surveys_v2$ST_NAME[join_surveys_v2$EST_ST == 56] <- "WY"

# analyze state distribution
# total number of survey respondents in each week
week_n <- join_surveys_v2 %>%
  group_by(WEEK) %>%
  summarise(sum_week=n())

# total number of survey respondents in each state in each week
state_n <- join_surveys_v2 %>%
  group_by(WEEK,ST_NAME) %>%
  summarise(sum_state=n())

# join above two datasets together
state_week_combo <- full_join(week_n,state_n)

# add new column that calculates percentage of survey respondents in each state in each week 
state_week_combo$state_per <- (state_week_combo$sum_state/state_week_combo$sum_week)*100

# view new dataset
view(state_week_combo)

final_data <- as.data.frame(right_join(join_surveys_v2,cdc_cases_v7,by=c("ST_NAME","WEEK")))
view(final_data)
# find number of NAs in each row (1 represents row)
x <- apply(final, 1, function(x) sum(is.na(x)))

# select variables that have zero NAs in their columns, along with the latter three variables
# ^DOUBLE CHECK MEANING
variable <- c(final$var_name[x==0],"ST_NAME","WEEK","case_per")

# create final clean dataset
final_data_clean <- final_data[variable]

# remove MSA because too many NA values
final_data_clean$EST_MSA=NULL

# print number of NA values for each variable in final data 
apply(final_data_clean, 2, function(x) sum(is.na(x)))

view(final_data_clean)

####################################
# recode variables
####################################

final_data_clean_v2 <- final_data_clean %>%
  select(ANXIOUS,WORRY,INTEREST,DOWN,TBIRTH_YEAR,SCRAM,EGENDER,RHISPANIC,RRACE,EEDUC,MS,THHLD_NUMPER,THHLD_NUMKID,WEEK,WRKLOSS,EXPCTLOSS,KINDWORK,CURFOODSUF,FOODCONF,HLTHSTATUS,DELAY,NOTGET,INCOME,ENROLL1,ENROLL2,ENROLL3,TEACH1,TEACH2,TEACH3,TEACH4,TEACH5,case_per,ST_NAME)

final_data_clean_v2$EGENDER[final_data_clean_v2$EGENDER == 2] <- 0
final_data_clean_v2$EGENDER[final_data_clean_v2$EGENDER < 0] <- NA
final_data_clean_v2$RHISPANIC[final_data_clean_v2$RHISPANIC == 2] <- 0
final_data_clean_v2$RHISPANIC[final_data_clean_v2$RHISPANIC <0] <- NA
final_data_clean_v2$WRKLOSS[final_data_clean_v2$WRKLOSS == 2] <- 0
final_data_clean_v2$WRKLOSS[final_data_clean_v2$WRKLOSS < 0] <- NA
final_data_clean_v2$EXPCTLOSS[final_data_clean_v2$EXPCTLOSS == 2] <- 0
final_data_clean_v2$EXPCTLOSS[final_data_clean_v2$EXPCTLOSS < 0] <- NA
final_data_clean_v2$DELAY[final_data_clean_v2$DELAY == 2] <- 0
final_data_clean_v2$DELAY[final_data_clean_v2$DELAY < 0] <- NA
final_data_clean_v2$DELAY[final_data_clean_v2$DELAY == "NA"] <- NA
final_data_clean_v2$NOTGET[final_data_clean_v2$NOTGET == 2] <- 0
final_data_clean_v2$NOTGET[final_data_clean_v2$NOTGET < 0] <- NA
final_data_clean_v2$KINDWORK[final_data_clean_v2$KINDWORK < 0] <- NA
final_data_clean_v2$CURFOODSUF[final_data_clean_v2$CURFOODSUF < 0] <- NA
final_data_clean_v2$FOODCONF[final_data_clean_v2$FOODCONF < 0] <- NA
final_data_clean_v2$HLTHSTATUS[final_data_clean_v2$HLTHSTATUS < 0] <- NA
final_data_clean_v2$INCOME[final_data_clean_v2$INCOME < 0] <- NA
final_data_clean_v2$ENROLL1[final_data_clean_v2$ENROLL1 < 0] <- NA
final_data_clean_v2$ENROLL2[final_data_clean_v2$ENROLL2 < 0] <- NA
final_data_clean_v2$ENROLL3[final_data_clean_v2$ENROLL3 < 0] <- NA
final_data_clean_v2$TEACH1[final_data_clean_v2$TEACH1 < 0] <- NA
final_data_clean_v2$TEACH2[final_data_clean_v2$TEACH2 < 0] <- NA
final_data_clean_v2$TEACH3[final_data_clean_v2$TEACH3 < 0] <- NA
final_data_clean_v2$TEACH4[final_data_clean_v2$TEACH4 < 0] <- NA
final_data_clean_v2$TEACH5[final_data_clean_v2$TEACH5 < 0] <- NA
final_data_clean_v2$ANXIOUS[final_data_clean_v2$ANXIOUS < 0] <- NA
final_data_clean_v2$WORRY[final_data_clean_v2$WORRY < 0] <- NA
final_data_clean_v2$INTEREST[final_data_clean_v2$INTEREST < 0] <- NA
final_data_clean_v2$DOWN[final_data_clean_v2$DOWN < 0] <- NA

view(final_data_clean_v2)

# print number of NAs under each variable
apply(final_data_clean_v2, 2, function(y) sum(is.na(y)))

# remove ENROLL1-3 and TEACH1-5 variables
final_data_clean_v3 <- final_data_clean_v2 %>%
  select(ANXIOUS,WORRY,INTEREST,DOWN,TBIRTH_YEAR,SCRAM,EGENDER,RHISPANIC,RRACE,EEDUC,MS,THHLD_NUMPER,THHLD_NUMKID,WEEK,WRKLOSS,EXPCTLOSS,KINDWORK,CURFOODSUF,FOODCONF,HLTHSTATUS,DELAY,NOTGET,INCOME,case_per,ST_NAME) %>%
  na.omit()

view(final_data_clean_v3)

# convert year of birth to age (2020 - year of birth)
final_data_clean_v4 <- final_data_clean_v3 %>%
  mutate(AGE=2020-TBIRTH_YEAR)

# remove birth year column
final_data_clean_v5 <- final_data_clean_v4[,-5]
view(final_data_clean_v5)

# determine total number of unique SCRAM IDs across all weeks
final_data_clean_v5 %>%
  distinct(SCRAM) %>%
  summarise(total_observations=n())

# remove all rows with duplicated SCRAM IDs
final_data_clean_v6 <- final_data_clean_v5[!duplicated(final_data_clean_v5[,5]),] 
view(final_data_clean_v6)

# determine number of observations in each week
final_data_clean_v6 %>%
  group_by(WEEK) %>%
  summarise(total_observations_v2=n())

# recode race
final_data_clean_v6$WHITE <- ifelse(final_data_clean_v6$RRACE == 1, 1, 0)
final_data_clean_v6$BLACK <- ifelse(final_data_clean_v6$RRACE == 2, 1, 0)
final_data_clean_v6$ASIAN <- ifelse(final_data_clean_v6$RRACE == 3, 1, 0)

# recode education
final_data_clean_v6$LESS_HIGHSCHOOL <- ifelse(final_data_clean_v6$EEDUC == 1, 1, 0)
final_data_clean_v6$SOME_HIGHSCHOOL <- ifelse(final_data_clean_v6$EEDUC == 2, 1, 0)
final_data_clean_v6$GRAD_HIGHSCHOOL <- ifelse(final_data_clean_v6$EEDUC == 3, 1, 0)
final_data_clean_v6$SOME_COLLEGE <- ifelse(final_data_clean_v6$EEDUC == 4, 1, 0)
final_data_clean_v6$ASSOCIATES <- ifelse(final_data_clean_v6$EEDUC == 5, 1, 0)
final_data_clean_v6$BACHELORS <- ifelse(final_data_clean_v6$EEDUC == 6, 1, 0)

# recode marital status
final_data_clean_v6$MARRIED <- ifelse(final_data_clean_v6$MS == 1, 1, 0)
final_data_clean_v6$WIDOWED <- ifelse(final_data_clean_v6$MS == 2, 1, 0)
final_data_clean_v6$DIVORCED <- ifelse(final_data_clean_v6$MS == 3, 1, 0)
final_data_clean_v6$SEPARATED <- ifelse(final_data_clean_v6$MS == 4, 1, 0)

# recode work
final_data_clean_v6$GOVT <- ifelse(final_data_clean_v6$KINDWORK == 1, 1, 0)
final_data_clean_v6$PRIVATE <- ifelse(final_data_clean_v6$KINDWORK == 2, 1, 0)
final_data_clean_v6$NONPROFIT <- ifelse(final_data_clean_v6$KINDWORK == 3, 1, 0)
final_data_clean_v6$SELF <- ifelse(final_data_clean_v6$KINDWORK == 4, 1, 0)

# recode food sufficiency in past week
final_data_clean_v6$FOOD_YES <- ifelse(final_data_clean_v6$CURFOODSUF == 1, 1, 0)
final_data_clean_v6$FOOD_SOMETIMESNO <- ifelse(final_data_clean_v6$CURFOODSUF == 3, 1, 0)
final_data_clean_v6$FOOD_NO <- ifelse(final_data_clean_v6$CURFOODSUF == 4, 1, 0)

# recode food sufficiency confidence for next week
final_data_clean_v6$CONF_NO <- ifelse(final_data_clean_v6$FOODCONF == 1, 1, 0)
final_data_clean_v6$CONF_SOMEWHAT <- ifelse(final_data_clean_v6$FOODCONF == 2, 1, 0)
final_data_clean_v6$CONF_YES <- ifelse(final_data_clean_v6$FOODCONF == 4, 1, 0)

# recode health status
final_data_clean_v6$HEALTH_EXCELLENT <- ifelse(final_data_clean_v6$HLTHSTATUS == 1, 1, 0)
final_data_clean_v6$HEALTH_GOOD <- ifelse(final_data_clean_v6$HLTHSTATUS == 3, 1, 0)
final_data_clean_v6$HEALTH_FAIR <- ifelse(final_data_clean_v6$HLTHSTATUS == 4, 1, 0)
final_data_clean_v6$HEALTH_POOR <- ifelse(final_data_clean_v6$HLTHSTATUS == 5, 1, 0)

# recode income
final_data_clean_v6$INC1 <- ifelse(final_data_clean_v6$INCOME == 1, 1, 0)
final_data_clean_v6$INC2 <- ifelse(final_data_clean_v6$INCOME == 2, 1, 0)
final_data_clean_v6$INC3 <- ifelse(final_data_clean_v6$INCOME == 3, 1, 0)
final_data_clean_v6$INC4 <- ifelse(final_data_clean_v6$INCOME == 4, 1, 0)
final_data_clean_v6$INC5 <- ifelse(final_data_clean_v6$INCOME == 5, 1, 0)
final_data_clean_v6$INC6 <- ifelse(final_data_clean_v6$INCOME == 6, 1, 0)
final_data_clean_v6$INC7 <- ifelse(final_data_clean_v6$INCOME == 7, 1, 0)

view(final_data_clean_v6)

final_data_clean_v7 <- final_data_clean_v6[,c(-5,-8,-9,-10,-16,-17,-18,-19,-22)]
view(final_data_clean_v7)

# write csv file
write.csv(final_data_clean_v7,file="DA_final_data.csv")

final.data = read.csv("DA_final_data.csv")
library(ISLR)

#lasso
#ANXIOUS
install.packages("glmnet")
library(glmnet)
x=model.matrix(ANXIOUS1~. , data = final.data)[,-1]
y=final.data$ANXIOUS1
set.seed(1)
cv.out1=cv.glmnet(x,y,alpha=1, nfolds = 5)
bestlam1=cv.out1$lambda.min
bestlam1
lasso.final1=glmnet(x,y,alpha=1, lambda = bestlam1)
lasso.final1
coef(lasso.final1)
#WORRY
library(glmnet)
x=model.matrix(WORRY1~. , data = final.data)[,-1]
y=final.data$WORRY1
set.seed(1)
cv.out2=cv.glmnet(x,y,alpha=1, nfolds = 5)
bestlam2=cv.out2$lambda.min
bestlam2
lasso.final2=glmnet(x,y,alpha=1, lambda = bestlam2)
lasso.final2
coef(lasso.final1)
#INTEREST
library(glmnet)
x=model.matrix(INTEREST1~. , data = final.data)[,-1]
y=final.data$INTEREST1
set.seed(1)
cv.out3=cv.glmnet(x,y,alpha=1, nfolds = 5)
bestlam3=cv.out3$lambda.min
bestlam3
lasso.final3=glmnet(x,y,alpha=1, lambda = bestlam3)
lasso.final3
coef(lasso.final1)

#KNN
library(class)
set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)




