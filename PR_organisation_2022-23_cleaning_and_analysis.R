#---------------------------------------------------------------#
#                                                               #
#      P R   o r g a n i s a t i o n a l   a n a l y s i s      #
#                                                               #
#                                                               #
#      Author: H a r l e y                                      #
#      Date created: 2 0 2 4 - 0 4 - 1 8                        #
#                                                               #
#                                                               #
#---------------------------------------------------------------#


library(tidyverse)
library(dplyr)
library(readxl)
library(tidyr)


dat <- read.csv("C:/Alex Harley/Audit_2023_onwards/2022-2023/PR/Data/rawData/NRAP-PR---Org-Audit---2024---All-Data-20240401-111152-218.csv")


# Check whether there are duplicates
print(dat[duplicated(dat$Name) | duplicated(dat$Name, fromLast = TRUE),])
# 0 duplicates


# Load hosp_dat
range <- cell_cols("A:F")
hosp_dat <- read_excel("C:/Alex Harley/Audit_2023_onwards/General UK data/PR Region - ICS mapping.xlsx", sheet = "cleaned", range = range)

str(hosp_dat)

hosp_dat <- hosp_dat %>% rename(service_name = Service,
                                org_code = `Org. code`,
                                trust_name = `Trust (England) / Local Health Board (Wales)`,
                                region = Region,
                                country = Country,
                                integrated_care_system = `Integrated care system (England)`)



# Organisational Audit data
dat <- dat %>% rename(org_code = Code,
                      service_name = Name)


# Check how many services matching in both dat and hosp_dat
hosp_matched <- dat$service_name %in% hosp_dat$service_name
sum(hosp_matched)

not_matched <- dat$service_name[!dat$service_name %in% hosp_dat$service_name]
if(length(not_matched) > 0) {
  cat("Services in dat not existing in hosp_dat:\n")
  print(not_matched)
} else {
  cat("Services existing in both dat and hosp_dat:\n")
}
## in audit data but not in the service list:
# "The North Lincolnshire Respiratory Service"                                  
# "First Community Health and Care - East Surrey"                             
# "HCRG Care Group - Bath and NE Somerset"                                       
# "Bromley Pulmonary Rehabilitation"                                             
# "Central Cheshire Integrated Care Partnership Pulmonary Rehabilitation Service"
# "Airedale - Wharfedale and Craven Pulmonary Rehabilitation Service"            
# "The High Weald Lewis and Haven Community Respiratory Service"                 
# "AIR Service"                                                                  
# "George Elliot Hospital Pulmonary Rehabilitation - Physiotherapy"              
# "Bury Community Respiratory Service"                                           
# "Wrightington Wigan and Leigh tier 2 Respiratory Services"                     
# "Solihull Community Respiratory Team"                                          
# "St. Helens Pulmonary Rehabilitation Service"                                  
# "Calderdale Pulmonary Rehabilitation Service (Audit paused)"                   
# "Buckinghamshire Pulmonary Rehabilitation Services"                            
# "Norfolk Community Pulmonary Rehabilitation Service"


dat %>% filter(service_name %in% c("The North Lincolnshire Respiratory Service", "First Community Health and Care - East Surrey", "HCRG Care Group - Bath and NE Somerset",
                                "Bromley Pulmonary Rehabilitation", "Central Cheshire Integrated Care Partnership Pulmonary Rehabilitation Service",
                                "Airedale - Wharfedale and Craven Pulmonary Rehabilitation Service", "The High Weald Lewis and Haven Community Respiratory Service",
                                "AIR Service", "George Elliot Hospital Pulmonary Rehabilitation - Physiotherapy", "Bury Community Respiratory Service",
                                "Wrightington Wigan and Leigh tier 2 Respiratory Services", "Solihull Community Respiratory Team", "St. Helens Pulmonary Rehabilitation Service",
                                "Calderdale Pulmonary Rehabilitation Service (Audit paused)", "Buckinghamshire Pulmonary Rehabilitation Services", 
                                "Norfolk Community Pulmonary Rehabilitation Service")) %>% select(org_code, service_name)
# NAD6, NDJ1, NQT1, NQV1, RBT1, RCF4, RDR5, RDU1, RLT2, RM32, RRF1, RRK2, RTV1, RWY1, RXQ1, RY32
# Find hsop_name in hosp_dat for the above hosp_code
hosp_dat %>% filter(org_code %in% c("NAD6", "NDJ1", "NQT1", "NQV1", "RBT1", "RCF4", "RDR5", "RDU1", "RLT2", "RM32", "RRF1", "RRK2", "RTV1", "RWY1", "RXQ1", "RY32")) %>% select(org_code, service_name)

# Hospitals not having the same hosp_code in dat and hosp_dat
not_matched <- dat$org_code[!dat$org_code %in% hosp_dat$org_code]
if(length(not_matched) > 0) {
  cat("Services in dat not existing in hosp_dat:\n")
  print(not_matched)
} else {
  cat("Services existing in both dat and hosp_dat:\n")
}
# All hosp_code match!

# Change service_name in dat
dat <- dat %>%
  mutate(service_name = case_when(
    service_name == "The North Lincolnshire Respiratory Service" ~ "The North Lincolnshire Respiratory service",
    service_name == "First Community Health and Care - East Surrey" ~ "First Community Health and Care - Surrey Community Respiratory Service", # hyphen change
    service_name == "HCRG Care Group - Bath and NE Somerset" ~ "HCRG Care Group Community Respiratory Service - Bath and North East Somerset",
    service_name == "Bromley Pulmonary Rehabilitation" ~ "Bromley Healthcare Pulmonary Rehabilitation",
    service_name == "Central Cheshire Integrated Care Partnership Pulmonary Rehabilitation Service" ~ "Central Cheshire Integrated Care Partnership",
    service_name == "Airedale - Wharfedale and Craven Pulmonary Rehabilitation Service" ~ "Airedale - Wharfedale and Craven PR Service",
    service_name == "The High Weald Lewis and Haven Community Respiratory Service" ~ "The High Weald Lewes and Havens Community Respiratory Service",
    service_name == "AIR Service" ~ "AIR service",
    service_name == "George Elliot Hospital Pulmonary Rehabilitation - Physiotherapy" ~ "George Eliot Hospital Pulmonary Rehabilitation - Physiotherapy",
    service_name == "Bury Community Respiratory Service" ~ "Bury Community Pulmonary Rehab Team",
    service_name == "Wrightington Wigan and Leigh tier 2 Respiratory Services" ~ "Wrightington Wigan & Leigh tier 2 Respiratory Services",
    service_name == "Solihull Community Respiratory Team" ~ "Solihull Community respiratory Team",
    service_name == "St. Helens Pulmonary Rehabilitation Service" ~ "St Helens Pulmonary Rehabilitation Service",
    service_name == "Calderdale Pulmonary Rehabilitation Service (Audit paused)" ~ "Calderdale Pulmonary Rehabilitation Service",
    service_name == "Buckinghamshire Pulmonary Rehabilitation Services" ~ "Buckinghamshire Pulmonary Rehabilitation Service",
    service_name == "Norfolk Community Pulmonary Rehabilitation Service" ~ "Norfolk Community Pulmonary Rehabilitation",
    TRUE ~ service_name))


# Merge
dat <- left_join(dat, hosp_dat, by = "service_name")
colnames(dat)


# Get rid of those who do not consent to participate:
dat <- dat %>%
  filter(!(service_name %in% c("Wiltshire Community Respiratory Team", "General Hospital (YYY)")))
# Originally: 163; after removal of non-consent: 161



# Get rid of useless columns
dat <- dat %>% 
  select(-org_code.y, -Modified.Date, -Modified.By, -Dataset) %>% 
  rename(org_code = org_code.x) %>%
  relocate(country:trust_name, .after = service_name)



# Now we order them how they want in the spreadsheet

dat <- dat %>% 
  select(country, region, integrated_care_system, trust_name, service_name, everything())

dat %>% select(country, region, trust_name, service_name)

table(dat$region)



dat <- dat %>% filter(Complete == "Yes")   # n=160 after removing incompleteness



# # # # # #

# Check the levels of the questions! 
# And just relevel the factors so the order is correct:

dat$X1.2.Do.you.offer.PR.to.COPD.patients <- factor(dat$X1.2.Do.you.offer.PR.to.COPD.patients,
                                                    levels = c("Yes - if within a year of completing PR",
                                                               "Yes - if completed between one to three years ago",
                                                               "Yes - if completed over three years ago",
                                                               "No"))

dat$X1.3.Do.you.offer.PR.to.patients.discharged <- factor(dat$X1.3.Do.you.offer.PR.to.patients.discharged,
                                                          levels = c("Yes", "No"))

dat$X2.1.Do.you.run.cohort.or.rolling.PR.programmes <- factor(dat$X2.1.Do.you.run.cohort.or.rolling.PR.programmes,
                                                              levels = c("Cohort only", "Rolling only", "Both"))

dat$X2.4.opportunity.to.access.directly.supervised..centre.based.PR <- factor(dat$X2.4.opportunity.to.access.directly.supervised..centre.based.PR,
                                                                              levels = c("Yes", "No"))

dat$X2.5.Do.you.offer.a.home.based.PR.programme. <- factor(dat$X2.5.Do.you.offer.a.home.based.PR.programme.,
                                                           levels = c("Yes", "No"))      

dat$X2.5a.If..Yes...are.all.patients.offered.initial.and.discharge.assessments. <- factor(dat$X2.5a.If..Yes...are.all.patients.offered.initial.and.discharge.assessments.,
                                                                                          levels = c("Yes", "No"))      

dat$X2.5b.If..Yes...are.patients.offered.supervised.PR.sessions.in.their.homes. <- factor(dat$X2.5b.If..Yes...are.patients.offered.supervised.PR.sessions.in.their.homes.,
                                                                                          levels = c("Yes", "No"))

dat$X2.7.Is.transport.funded.for.patients. <- factor(dat$X2.7.Is.transport.funded.for.patients.,
                                            levels = c("Yes", "No"))

dat$X2.8a.If..6MWT...how.many.of.your.sites.use.a.30.metre.course. <- factor(dat$X2.8a.If..6MWT...how.many.of.your.sites.use.a.30.metre.course.,
                                                                             levels = c("All sites use a 30m course",
                                                                                        "At least 1 site uses a 30m course",
                                                                                        "No sites use a 30m course"))

dat$X2.9a.Is.lower.limb.muscle.strength.measured.at.the.initial.assessment <- factor(dat$X2.9a.Is.lower.limb.muscle.strength.measured.at.the.initial.assessment,
                                                                                     levels = c("Yes", "No"))

dat$X2.9b.Is.lower.limb.muscle.strength.measured.at.the.discharge.assessment <- factor(dat$X2.9b.Is.lower.limb.muscle.strength.measured.at.the.discharge.assessment,
                                                                                       levels = c("Yes", "No"))

dat$X2.11.Is.aerobic.training.offered.during.the.PR.programme. <- factor(dat$X2.11.Is.aerobic.training.offered.during.the.PR.programme.,
                                                                         levels = c("Yes", "No"))

dat$X2.12.Is.aerobic.training.individually.prescribed. <- factor(dat$X2.12.Is.aerobic.training.individually.prescribed.,
                                                                 levels = c("Yes", "No"))

dat$X2.12b.What.intensity.of.aerobic.exercise.prescription.is.used. <- factor(dat$X2.12b.What.intensity.of.aerobic.exercise.prescription.is.used.,
                                                                              levels = c("<65%", "65 - <75%", "75 - 85%", ">85%"))

dat$X2.13.Is.resistance.training.offered.during.the.PR.programme. <- factor(dat$X2.13.Is.resistance.training.offered.during.the.PR.programme.,
                                                                            levels = c("Yes", "No"))

dat$X2.14.Is.resistance.training.individually.prescribed. <- factor(dat$X2.14.Is.resistance.training.individually.prescribed.,
                                                                    levels = c("Yes", "No"))

dat$X2.15.Are.patients.advised.to.do.unsupervised.home <- factor(dat$X2.15.Are.patients.advised.to.do.unsupervised.home,
                                                                 levels = c("Yes as part of a centre-based programme",
                                                                            "Yes as part of a home-based programme",
                                                                            "Yes as part of centre and home based programmes",
                                                                            "No"))

dat$X2.20.Routinely.provide.patients.with.an.individualised.structured..written.plan. <- factor(dat$X2.20.Routinely.provide.patients.with.an.individualised.structured..written.plan.,
                                                                                                levels = c("Yes", "No"))

dat$X4.1.Is.there.a.named.clinical.lead.for.the.service. <- factor(dat$X4.1.Is.there.a.named.clinical.lead.for.the.service.,
                                                                   levels = c("Yes - filled",
                                                                              "Yes - currently unfilled",
                                                                              "No"))

dat$X4.1b.If..Yes...does.the.clinical.lead.receive.dedicated.sessional.time <- factor(dat$X4.1b.If..Yes...does.the.clinical.lead.receive.dedicated.sessional.time,
                                                                                      levels = c("Yes", "No"))

dat$X4.5.Is.there.audit.support.provided. <- factor(dat$X4.5.Is.there.audit.support.provided.,
                                                    levels = c("Yes", "No"))

dat$X5.1.Do.you.have.a.Standard.Operating.Procedure..SOP..detailing.local.policies. <- factor(dat$X5.1.Do.you.have.a.Standard.Operating.Procedure..SOP..detailing.local.policies.,
                                                                                              levels = c("Yes", "No"))




# # # # # # # 


# It's at this point, because I didn't take out the columns with 3 dots... I need to do it here. By creating this vector for use at the end.
# Have to do it here because they are in numeric format here

# inspectvect <- dat %>% select(where(is.numeric)) %>% select(contains("...")) %>% colnames()
# 
# 


# Split columns with multiple answers by ";"
# Define the variables to be split
col_to_separate <- c("X1.0.Referrals", "X1.1.PR.offerred.to.MRC.grades", "X2.2.Duration.of.centre.based.PR.programme",
                     "X2.3.How.many.supervised.centre.based.PR", "X2.5c.If..Yes...what.is.the.typical.duration.of.your.home.based.PR.programme.",
                     "X2.5d.If..Yes...how.many.supervised.home.based.PR.sessions.per.week", "X2.6.Do.you.send.patients.information.about.your.PR.programme",
                     "X2.8.Which.measures.of.aerobic.exercise", "X2.10.Are.any.of.the.following.measured.at.assessment.",
                     "X2.11a.What.type.of.aerobic.training.is.undertaken.during.the.PR.programme.", "X2.12a.How.is.aerobic.training.prescribed.",
                     "X2.14a.How.is.resistance.training.prescribed.", "X2.18a.How.is.education.provided", "X2.18b.If..Yes..to.2.5..how.is.education.provided.for.home.based.PR.programmes",
                     "X2.19a.Who.delivers.group.sessions.",
                     "X2.19b.face.to.face.or.virtual.group.sessions.include", "X3.0.What.type.of.organisation.provides.your.service.",
                     "X3.1.What.type.of.funding.does.your.service.have.", "X3.2.Are.you.funded.for.all.the.conditions.listed",
                     "X4.4.What.are.the.designations.of.the.staff.who.contribute", "X5.1a.What.does.the.Standard.Operating.Procedure.cover.")

# Split combined answers and create new columns

split_create_col <- function(data, column) {
  
  # Split combined answers into separate elements
  separated_answers <- strsplit(as.character(data[[column]]), ";")
  
  # Identify unique answers
  unique_answers <- unique(unlist(separated_answers))
  
  # Create new columns for each unique answer
  for (answer in unique_answers) {
    # Create a new column indicating presence of the answer
    data[[paste0(column, "_", answer)]] <- ifelse(sapply(separated_answers, function(x) answer %in% x), "Yes", "No")
  }
  
  # Remove the original column
  data[[column]] <- NULL
  
  return(data)
}

# Apply the function to each column
for (column in col_to_separate) {
  dat <- split_create_col(dat, column)
}


# Rearrange the order of columns
dat <- dat %>%
  relocate(X1.0.Referrals_Asthma:`X1.1.PR.offerred.to.MRC.grades_Grade 1 and above`, .after = Complete) %>%
  relocate(`X2.2.Duration.of.centre.based.PR.programme_7 weeks`:`X2.3.How.many.supervised.centre.based.PR_More than 3`, .after = X2.1.Do.you.run.cohort.or.rolling.PR.programmes) %>%
  relocate(`X2.5c.If..Yes...what.is.the.typical.duration.of.your.home.based.PR.programme._6 weeks`:X2.6.Do.you.send.patients.information.about.your.PR.programme_No, .after = X2.5b.If..Yes...are.patients.offered.supervised.PR.sessions.in.their.homes.) %>%
  relocate(`X2.8.Which.measures.of.aerobic.exercise_Six minute walk test (6MWT)`:`X2.8.Which.measures.of.aerobic.exercise_Endurance shuttle walk test (ESWT)`, .after = X2.7.Is.transport.funded.for.patients.) %>%
  relocate(`X2.10.Are.any.of.the.following.measured.at.assessment._Other psychological status scores`:X2.10.Are.any.of.the.following.measured.at.assessment._No, .after = X2.9b.Is.lower.limb.muscle.strength.measured.at.the.discharge.assessment) %>%
  relocate(X2.11a.What.type.of.aerobic.training.is.undertaken.during.the.PR.programme._Cycling:X2.11a.What.type.of.aerobic.training.is.undertaken.during.the.PR.programme._Other, .after = X2.11.Is.aerobic.training.offered.during.the.PR.programme.) %>%
  relocate(`X2.12a.How.is.aerobic.training.prescribed._Borg or perceived exertion scores`:`X2.12a.How.is.aerobic.training.prescribed._Endurance shuttle walking test (ESWT) level from ISWT`, .after = X2.12.Is.aerobic.training.individually.prescribed.) %>%
  relocate(`X2.14a.How.is.resistance.training.prescribed._Measurement of 1RM or strength`:`X2.14a.How.is.resistance.training.prescribed._Borg or perceived exertion scores`, .after = X2.14.Is.resistance.training.individually.prescribed.) %>%
  relocate(`X2.18a.How.is.education.provided_Face-to-face group education sessions`:`X2.19b.face.to.face.or.virtual.group.sessions.include_Advance directives`, .after = X2.17.How.many.hours.of.education.home.based) %>%
  relocate(`X3.0.What.type.of.organisation.provides.your.service._NHS Health Board`:`X3.2.Are.you.funded.for.all.the.conditions.listed_Chronic heart failure`, .after = X2.20.Routinely.provide.patients.with.an.individualised.structured..written.plan.) %>%
  relocate(X4.4.What.are.the.designations.of.the.staff.who.contribute_None:`X4.4.What.are.the.designations.of.the.staff.who.contribute_Social worker`, .after = X4.3.What.is.the.current.WTE.of.staff.vacancies.at.the.service.)


dat <- dat %>% arrange(region, integrated_care_system, trust_name, org_code, service_name) %>% #select(-country) %>%
  relocate(region, integrated_care_system, trust_name, org_code, service_name)

# dat_hosp_level %>% select(where(is.numeric)) %>% select(contains("...")) %>% colnames()

# No columns with "..."



# # (dat_hosp_level %>% select(where(is.numeric)) %>% select(contains("..."))) <- 
#   
# 
#   (dat_hosp_level %>% select(where(is.numeric)) %>% select(contains("..."))) <-   dat_hosp_level %>% select(where(is.numeric)) %>% select(contains("...")) %>%
#     mutate_all(as.character) %>%  mutate_all(~recode(., "1" = "Yes", .missing = "No"))
#   
# 
# dat_hosp_level %>% mutate(across(where(~is.numeric() & contains("..."))), ~round(2)) # ~recode(as.character(.), "1" = "Yes", .missing = "No"))


# Need to do this because of bloody dplyr being a nightmare.
# find the columns that are numeric or logical, and combine with grepl,
# grepl is finding all the columns that contain '...', which are the the ones with multiple options. 
# For the hospital-level table all of these need to be converted from '1's and missing to 'yes' and 'no'
# There are some blanks - we don't need to worry about that here.


# for (i in 1:ncol(dat_hosp_level)) {
#   if (grepl("\\.\\.\\.", colnames(dat_hosp_level))[i] == TRUE & 
#       (is.numeric(dat_hosp_level[ , i]) == TRUE | is.logical(dat_hosp_level[ , i]) == TRUE)) {
# 
#     dat_hosp_level[[i]] <- as.character(dat_hosp_level[[i]])
#     dat_hosp_level[[i]] <- recode(dat_hosp_level[[i]], "1" = "Yes", .missing = "No")
#   
#     }
# }



# Need to do some rounding. (for service level only)

dat_service_level_output <- dat %>% mutate(across(starts_with("X2.17"), ~sprintf("%.1f", round(., 1))))

dat_service_level_output <- dat_service_level_output %>% select(-country)



write.csv(dat_service_level_output, "C:/Alex Harley/Audit_2023_onwards/2022-2023/PR/Analysis/Output/Service_level_data_PR_org_2022-23.csv",
         row.names = FALSE, na = "")


# # # # # # that is it for the service-level data!!! # # # # # # # # #

dat_service_level <- dat






# For the summary statistics, we need to do some more cleaning



# Try without doing this...

# # # # 

# # This time, we replace the missing values with '0's
# 
# for (i in 1:ncol(dat)) {
#   if (grepl("\\.\\.\\.", colnames(dat))[i] == TRUE & 
#       is.numeric(dat[ , i]) == TRUE) {
#     
#     dat[[i]] <- recode(dat[[i]], `1` = 1, .missing = 0)
#     
#   }
# }

# # # # 


# for (i in 1:ncol(dat)) {
#   if (grepl("\\.\\.\\.", colnames(dat))[i] == TRUE & 
#       (is.numeric(dat[ , i]) == TRUE | is.logical(dat[ , i]) == TRUE)) {
#     
#     dat[[i]] <- as.character(dat[[i]])
#     dat[[i]] <- recode(dat[[i]], "1" = "Yes", .missing = "No")
#     dat[[i]] <- factor(dat[[i]])
#     
#   }
# }


# This was another bloody nightmare. for some reason the function that you're doing on the column
# goes within the 'across' bracket. The squiggle on fct_recode is essential. "is.factor" can't have brackets.

# dat <- dat %>% mutate(across(where(is.factor), ~fct_recode(., NULL = "")))


summary(dat)

# Also, we need to recode the ones that are just yes and no to 1s and 0s
# again, more inconsistency - if we are selecting specific columns, we now use 'c' to select them

# Or... we try it without doing this?

# # # # #

# dat <- dat %>% mutate(across(c(X1.5.Paediatric.HDU,
#                     X1.6.Paediatric.Intensive.Care.Unit,
#                     X3.6.Specific.service.for.paediatric.asthma,
#                     X3.6a.If.not...Set.criteria.for.referral.offsite,
#                     X5.1.Electronic.Patient.Record..EPR..system,
#                     X5.2.Early.warning.detection),
#                ~as.numeric(as.character(fct_recode(., `1` = "Yes", `0` = "No")))))

# # # # # # 


# Now.... we create a new function.


sum_stats_total <- function(x, varname, roundno = 0, total = TRUE, mean = TRUE, sd = TRUE) {
  
  # Get the summary statistics for each country:
  
  
  # [!is.ininite(x[[varname]]) & !is.nan(x[[varname]]) #
  
  tabby_all <- data.frame(Total = sum(x[[varname]], na.rm = TRUE),
    Mean = mean(x[[varname]], na.rm = TRUE),
    SD = sd(x[[varname]], na.rm = TRUE),
    Median = median(x[[varname]], na.rm = TRUE),
    Low_quart = as.numeric(unname(summary(x[[varname]]))[2]),
    High_quart = as.numeric(unname(summary(x[[varname]]))[5]))
  
  eng <- x %>% filter(country == "England")
  wal <- x %>% filter(country == "Wales")
  
  tabby_eng <- data.frame(Total = sum(eng[[varname]], na.rm = TRUE),
    Mean = mean(eng[[varname]], na.rm = TRUE),
    SD = sd(eng[[varname]], na.rm = TRUE),
    Median = median(eng[[varname]], na.rm = TRUE),
    Low_quart = as.numeric(unname(summary(eng[[varname]]))[2]),
    High_quart = as.numeric(unname(summary(eng[[varname]]))[5]))
  
  
  tabby_wal <- data.frame(Total = sum(wal[[varname]], na.rm = TRUE),
    Mean = mean(wal[[varname]], na.rm = TRUE),
    SD = sd(wal[[varname]], na.rm = TRUE),
    Median = median(wal[[varname]], na.rm = TRUE),
    Low_quart = as.numeric(unname(summary(wal[[varname]]))[2]),
    High_quart = as.numeric(unname(summary(wal[[varname]]))[5]))
  
  # Bind them all together
  
  tabby_all <- rbind(tabby_all, tabby_eng, tabby_wal)
  
  # round them to what you want
  
  
  tabby_all <- round(tabby_all, roundno)
  
  
  # Format them how they need to be formatted
  
  tabby_all$Mean <- sprintf(paste0("%.", roundno, "f"), tabby_all$Mean)
  tabby_all$SD <- sprintf(paste0("%.", roundno, "f"), tabby_all$SD)
  tabby_all$Median <- sprintf(paste0("%.", roundno, "f"), tabby_all$Median)
  tabby_all$Low_quart <- sprintf(paste0("%.", roundno, "f"), tabby_all$Low_quart)
  tabby_all$High_quart <- sprintf(paste0("%.", roundno, "f"), tabby_all$High_quart)
  
  
  # Remove the unnecessary statistics for that variable
  
  if (total == FALSE) {
    tabby_all$Total <- NULL
  }
  
  if (mean == FALSE) {
    tabby_all$Mean <- NULL
  }
  
  if (sd == FALSE) {
    tabby_all$SD <- NULL
  }
  
  
  colnames(tabby_all) <- paste0(varname, "_", colnames(tabby_all))
  
  
  
  return(tabby_all)
}




sum_stats <- function(x, varname, roundno = 0, #total = TRUE, 
                      mean = TRUE, sd = TRUE) {
  
  # Get the summary statistics for each country:
  
    
  # [!is.ininite(x[[varname]]) & !is.nan(x[[varname]]) #
  
  tabby_all <- data.frame(#Total = sum(x[[varname]], na.rm = TRUE),
                      Mean = mean(x[[varname]], na.rm = TRUE),
                      SD = sd(x[[varname]], na.rm = TRUE),
                      Median = median(x[[varname]], na.rm = TRUE),
                      Low_quart = as.numeric(unname(summary(x[[varname]]))[2]),
                      High_quart = as.numeric(unname(summary(x[[varname]]))[5]))

  eng <- x %>% filter(country == "England")
  wal <- x %>% filter(country == "Wales")
  
  tabby_eng <- data.frame(#Total = sum(eng[[varname]], na.rm = TRUE),
                          Mean = mean(eng[[varname]], na.rm = TRUE),
                          SD = sd(eng[[varname]], na.rm = TRUE),
                          Median = median(eng[[varname]], na.rm = TRUE),
                          Low_quart = as.numeric(unname(summary(eng[[varname]]))[2]),
                          High_quart = as.numeric(unname(summary(eng[[varname]]))[5]))
  
  
  tabby_wal <- data.frame(#Total = sum(wal[[varname]], na.rm = TRUE),
                          Mean = mean(wal[[varname]], na.rm = TRUE),
                          SD = sd(wal[[varname]], na.rm = TRUE),
                          Median = median(wal[[varname]], na.rm = TRUE),
                          Low_quart = as.numeric(unname(summary(wal[[varname]]))[2]),
                          High_quart = as.numeric(unname(summary(wal[[varname]]))[5]))
  
  # Bind them all together
  
  tabby_all <- rbind(tabby_all, tabby_eng, tabby_wal)

  # round them to what you want
  
  
  tabby_all <- round(tabby_all, roundno)
  
  
  # Format them how they need to be formatted
  
  tabby_all$Mean <- sprintf(paste0("%.", roundno, "f"), tabby_all$Mean)
  tabby_all$SD <- sprintf(paste0("%.", roundno, "f"), tabby_all$SD)
  tabby_all$Median <- sprintf(paste0("%.", roundno, "f"), tabby_all$Median)
  tabby_all$Low_quart <- sprintf(paste0("%.", roundno, "f"), tabby_all$Low_quart)
  tabby_all$High_quart <- sprintf(paste0("%.", roundno, "f"), tabby_all$High_quart)
  

  # Remove the unnecessary statistics for that variable
                          
  # if (total == FALSE) {
  #   tabby_all$Total <- NULL
  # }
  
  if (mean == FALSE) {
    tabby_all$Mean <- NULL
  }
  
  if (sd == FALSE) {
    tabby_all$SD <- NULL
  }

  
  colnames(tabby_all) <- paste0(varname, "_", colnames(tabby_all))

  
      
  return(tabby_all)
}





# And another one.


freq_stats_total <- function(x, varname, remove_nos = FALSE, KPI = FALSE) {
  
  all <- x
  eng <- x %>% filter(country == "England")
  wal <- x %>% filter(country == "Wales")
  
  # for all:
  
  gen <- all %>% dplyr::select(!!varname) %>% drop_na()
  var_N <- data.frame(nrow(gen))
  tabby_all <- data.frame(delete_me = 0)
  
  #   if(nrow(gen) == 0) {return(var_N)}
  
  #  else {
  
  gen0 <- as.data.frame(table(gen[[1]]))
  gen1 <- as.data.frame(round(prop.table(table(gen[[1]]))*100, 1), nsmall = 1) %>% 
    dplyr::rename(perc = Freq)
  gen2 <- inner_join(gen0, gen1, by = "Var1")
  gen2$perc <- sprintf("%.1f", gen2$perc)
  
  if (remove_nos == TRUE) {
    
    gen2 <- gen2 %>% filter(Var1 != "No")
    
  }
  
  # gen.E2$England <- paste(gen.E2$Freq, " (", gen.E2$perc, ")", sep = "")
  # gen.E2 <- select(gen.E2, Var1, England)
  for (i in 1:nrow(gen2)) {
    gen3 <- gen2
    gen3$Var1 <- as.character(gen3$Var1)
    gen3 <- gen3[i, ]
    colnames(gen3) <- c("Var1", paste0(varname, "_", gsub(" ", "_", gen3[1,1]), "_n"),
                        paste0(varname, "_", gsub(" ", "_", gen3[1,1]), "_perc")) 
    gen4 <- cbind(var_N, gen3[ ,2:3])
    colnames(gen4)[1] <- paste0(varname, "_", gsub(" ", "_", gen3[1,1]), "_Total_N")
    tabby_all <- cbind(tabby_all, gen4)
  }
  
  
  # for England:
  
  gen <- eng %>% dplyr::select(!!varname) %>% drop_na()
  var_N <- data.frame(nrow(gen))
  tabby_eng <- data.frame(delete_me = 0)
  
  gen0 <- as.data.frame(table(gen[[1]]))
  gen1 <- as.data.frame(round(prop.table(table(gen[[1]]))*100, 1), nsmall = 1) %>% 
    dplyr::rename(perc = Freq)
  gen2 <- inner_join(gen0, gen1, by = "Var1")
  gen2$perc <- sprintf("%.1f", gen2$perc)
  
  if (remove_nos == TRUE) {
    
    gen2 <- gen2 %>% filter(Var1 != "No")
    
  }
  
  
  for (i in 1:nrow(gen2)) {
    gen3 <- gen2
    gen3$Var1 <- as.character(gen3$Var1)
    gen3 <- gen3[i, ]
    colnames(gen3) <- c("Var1", paste0(varname, "_", gsub(" ", "_", gen3[1,1]), "_n"),
                        paste0(varname, "_", gsub(" ", "_", gen3[1,1]), "_perc")) 
    gen4 <- cbind(var_N, gen3[ ,2:3])
    colnames(gen4)[1] <- paste0(varname, "_", gsub(" ", "_", gen3[1,1]), "_Total_N")
    tabby_eng <- cbind(tabby_eng, gen4)
  }
  
  # for Wales:
  
  gen <- wal %>% dplyr::select(!!varname) %>% drop_na()
  var_N <- data.frame(nrow(gen))
  tabby_wal <- data.frame(delete_me = 0)
  
  gen0 <- as.data.frame(table(gen[[1]]))
  gen1 <- as.data.frame(round(prop.table(table(gen[[1]]))*100, 1), nsmall = 1) %>% 
    dplyr::rename(perc = Freq)
  gen2 <- inner_join(gen0, gen1, by = "Var1")
  gen2$perc <- sprintf("%.1f", gen2$perc)
  
  if (remove_nos == TRUE) {
    
    gen2 <- gen2 %>% filter(Var1 != "No")
    
  }
  
  
  for (i in 1:nrow(gen2)) {
    gen3 <- gen2
    gen3$Var1 <- as.character(gen3$Var1)
    gen3 <- gen3[i, ]
    colnames(gen3) <- c("Var1", paste0(varname, "_", gsub(" ", "_", gen3[1,1]), "_n"),
                        paste0(varname, "_", gsub(" ", "_", gen3[1,1]), "_perc")) 
    gen4 <- cbind(var_N, gen3[ ,2:3])
    colnames(gen4)[1] <- paste0(varname, "_", gsub(" ", "_", gen3[1,1]), "_Total_N")
    tabby_wal <- cbind(tabby_wal, gen4)
  }
  
  
  tabby <- rbind(tabby_all, tabby_eng, tabby_wal)
  
  tabby$delete_me <- NULL
  
  if (KPI == TRUE) {   # If it's a KPI variable the order is slightly different
    
    tabby <- tabby[ , c(2, 1, 3)]
    
  }
  
  return(tabby)
  
}




freq_stats <- function(x, varname, remove_nos = FALSE, KPI = FALSE) {
  
  all <- x
  eng <- x %>% filter(country == "England")
  wal <- x %>% filter(country == "Wales")

  # for all:
    
    gen <- all %>% dplyr::select(!!varname) %>% drop_na()
    var_N <- data.frame(nrow(gen))
    tabby_all <- data.frame(delete_me = 0)
    
    #   if(nrow(gen) == 0) {return(var_N)}
    
    #  else {
    
    gen0 <- as.data.frame(table(gen[[1]]))
    gen1 <- as.data.frame(round(prop.table(table(gen[[1]]))*100, 1), nsmall = 1) %>% 
      dplyr::rename(perc = Freq)
    gen2 <- inner_join(gen0, gen1, by = "Var1")
    gen2$perc <- sprintf("%.1f", gen2$perc)
    
    if (remove_nos == TRUE) {
      
      gen2 <- gen2 %>% filter(Var1 != "No")

    }
    
    # gen.E2$England <- paste(gen.E2$Freq, " (", gen.E2$perc, ")", sep = "")
    # gen.E2 <- select(gen.E2, Var1, England)
    for (i in 1:nrow(gen2)) {
      gen3 <- gen2
      gen3$Var1 <- as.character(gen3$Var1)
      gen3 <- gen3[i, ]
      colnames(gen3) <- c("Var1", paste0(varname, "_", gsub(" ", "_", gen3[1,1]), "_n"),
                          paste0(varname, "_", gsub(" ", "_", gen3[1,1]), "_perc")) 
      # gen4 <- cbind(var_N, gen3[ ,2:3])
      # colnames(gen4)[1] <- paste0(varname, "_", gsub(" ", "_", gen3[1,1]), "_Total_N")
      tabby_all <- cbind(tabby_all, gen3)
    }
    
    
    # for England:
    
    gen <- eng %>% dplyr::select(!!varname) %>% drop_na()
    var_N <- data.frame(nrow(gen))
    tabby_eng <- data.frame(delete_me = 0)
    
    gen0 <- as.data.frame(table(gen[[1]]))
    gen1 <- as.data.frame(round(prop.table(table(gen[[1]]))*100, 1), nsmall = 1) %>% 
      dplyr::rename(perc = Freq)
    gen2 <- inner_join(gen0, gen1, by = "Var1")
    gen2$perc <- sprintf("%.1f", gen2$perc)
    
    if (remove_nos == TRUE) {
      
      gen2 <- gen2 %>% filter(Var1 != "No")
      
    }
    
   
    for (i in 1:nrow(gen2)) {
      gen3 <- gen2
      gen3$Var1 <- as.character(gen3$Var1)
      gen3 <- gen3[i, ]
      colnames(gen3) <- c("Var1", paste0(varname, "_", gsub(" ", "_", gen3[1,1]), "_n"),
                          paste0(varname, "_", gsub(" ", "_", gen3[1,1]), "_perc")) 
      # gen4 <- cbind(var_N, gen3[ ,2:3])
      # colnames(gen4)[1] <- paste0(varname, "_", gsub(" ", "_", gen3[1,1]), "_Total_N")
      tabby_eng <- cbind(tabby_eng, gen3)
    }
    
    # for Wales:
    
    gen <- wal %>% dplyr::select(!!varname) %>% drop_na()
    var_N <- data.frame(nrow(gen))
    tabby_wal <- data.frame(delete_me = 0)
    
    gen0 <- as.data.frame(table(gen[[1]]))
    gen1 <- as.data.frame(round(prop.table(table(gen[[1]]))*100, 1), nsmall = 1) %>% 
      dplyr::rename(perc = Freq)
    gen2 <- inner_join(gen0, gen1, by = "Var1")
    gen2$perc <- sprintf("%.1f", gen2$perc)
    
    if (remove_nos == TRUE) {
      
      gen2 <- gen2 %>% filter(Var1 != "No")
      
    }
    

    for (i in 1:nrow(gen2)) {
      gen3 <- gen2
      gen3$Var1 <- as.character(gen3$Var1)
      gen3 <- gen3[i, ]
      colnames(gen3) <- c("Var1", paste0(varname, "_", gsub(" ", "_", gen3[1,1]), "_n"),
                          paste0(varname, "_", gsub(" ", "_", gen3[1,1]), "_perc")) 
      # gen4 <- cbind(var_N, gen3[ ,2:3])
      # colnames(gen4)[1] <- paste0(varname, "_", gsub(" ", "_", gen3[1,1]), "_Total_N")
      tabby_wal <- cbind(tabby_wal, gen3)
    }
    
    
    tabby <- rbind(tabby_all, tabby_eng, tabby_wal)
    
    tabby$delete_me <- NULL
    
    if (KPI == TRUE) {   # If it's a KPI variable the order is slightly different
    
        tabby <- tabby[ , c(2, 1, 3)]
    
    }
    
    return(tabby)
    
}



# # # Start # # #




# # # # Start the data frame # # # # 

# Second column as the total number of hospitals in England and Wales

all <- data.frame(Country = c("National (All)", "England", "Wales"),
                  Total_hospitals = numeric(3))

# # # # # 

# Count the number of hospitals in each country

hosp_c <- table(dat_service_level$country)

all$Total_hospitals[1] <- sum(hosp_c)
all$Total_hospitals[2] <- hosp_c["England"]
all$Total_hospitals[3] <- hosp_c["Wales"]




dat_service_level <- dat_service_level %>%
  mutate(`X1.0.Referrals_Chronic heart failure` = as.factor(`X1.0.Referrals_Chronic heart failure`),
         `X1.1.PR.offerred.to.MRC.grades_Grade 1 and above` = as.factor(`X1.1.PR.offerred.to.MRC.grades_Grade 1 and above`),
         `X1.1.PR.offerred.to.MRC.grades_Grade 2 and above` = as.factor(`X1.1.PR.offerred.to.MRC.grades_Grade 2 and above`),
         `X2.2.Duration.of.centre.based.PR.programme_More than 8 weeks` = as.factor(`X2.2.Duration.of.centre.based.PR.programme_More than 8 weeks`),
         `X2.3.How.many.supervised.centre.based.PR_1` = as.factor(`X2.3.How.many.supervised.centre.based.PR_1`),
         `X2.3.How.many.supervised.centre.based.PR_More than 3` = as.factor(`X2.3.How.many.supervised.centre.based.PR_More than 3`),
         `X2.5c.If..Yes...what.is.the.typical.duration.of.your.home.based.PR.programme._5 weeks` = as.factor(`X2.5c.If..Yes...what.is.the.typical.duration.of.your.home.based.PR.programme._5 weeks`),
         `X2.5c.If..Yes...what.is.the.typical.duration.of.your.home.based.PR.programme._6 weeks` = as.factor(`X2.5c.If..Yes...what.is.the.typical.duration.of.your.home.based.PR.programme._6 weeks`),
         `X2.5c.If..Yes...what.is.the.typical.duration.of.your.home.based.PR.programme._7 weeks` = as.factor(`X2.5c.If..Yes...what.is.the.typical.duration.of.your.home.based.PR.programme._7 weeks`),
         `X2.5c.If..Yes...what.is.the.typical.duration.of.your.home.based.PR.programme._8 weeks` = as.factor(`X2.5c.If..Yes...what.is.the.typical.duration.of.your.home.based.PR.programme._8 weeks`),
         `X2.5c.If..Yes...what.is.the.typical.duration.of.your.home.based.PR.programme._More than 8 weeks` = as.factor(`X2.5c.If..Yes...what.is.the.typical.duration.of.your.home.based.PR.programme._More than 8 weeks`),
         `X2.5d.If..Yes...how.many.supervised.home.based.PR.sessions.per.week_1` = as.factor(`X2.5d.If..Yes...how.many.supervised.home.based.PR.sessions.per.week_1`),
         `X2.5d.If..Yes...how.many.supervised.home.based.PR.sessions.per.week_2` = as.factor(`X2.5d.If..Yes...how.many.supervised.home.based.PR.sessions.per.week_2`),
         `X2.5d.If..Yes...how.many.supervised.home.based.PR.sessions.per.week_3` = as.factor(`X2.5d.If..Yes...how.many.supervised.home.based.PR.sessions.per.week_3`),
         `X2.5d.If..Yes...how.many.supervised.home.based.PR.sessions.per.week_More than 3` = as.factor(`X2.5d.If..Yes...how.many.supervised.home.based.PR.sessions.per.week_More than 3`),
         `X2.6.Do.you.send.patients.information.about.your.PR.programme_Yes - sent via email` = as.factor(`X2.6.Do.you.send.patients.information.about.your.PR.programme_Yes - sent via email`), # changing hyphen here
         `X2.6.Do.you.send.patients.information.about.your.PR.programme_Yes - directed to website` = as.factor(`X2.6.Do.you.send.patients.information.about.your.PR.programme_Yes - directed to website`),
         `X2.6.Do.you.send.patients.information.about.your.PR.programme_No` = as.factor(`X2.6.Do.you.send.patients.information.about.your.PR.programme_No`),
         `X2.8.Which.measures.of.aerobic.exercise_Endurance shuttle walk test (ESWT)` = as.factor(`X2.8.Which.measures.of.aerobic.exercise_Endurance shuttle walk test (ESWT)`),
         X2.10.Are.any.of.the.following.measured.at.assessment._No = as.factor(X2.10.Are.any.of.the.following.measured.at.assessment._No),
         `X2.12a.How.is.aerobic.training.prescribed._Endurance shuttle walking test (ESWT) level from ISWT` = as.factor(`X2.12a.How.is.aerobic.training.prescribed._Endurance shuttle walking test (ESWT) level from ISWT`),
         `X2.12a.How.is.aerobic.training.prescribed._From incremental shuttle walk test (ISWT)` = as.factor(`X2.12a.How.is.aerobic.training.prescribed._From incremental shuttle walk test (ISWT)`),
         X2.18a.How.is.education.provided_Other = as.factor(X2.18a.How.is.education.provided_Other),
         `X2.18b.If..Yes..to.2.5..how.is.education.provided.for.home.based.PR.programmes_Information given digitally (including via memory stick/ email attachment/ CD/ DVD)` = as.factor(`X2.18b.If..Yes..to.2.5..how.is.education.provided.for.home.based.PR.programmes_Information given digitally (including via memory stick/ email attachment/ CD/ DVD)`),
         `X2.18b.If..Yes..to.2.5..how.is.education.provided.for.home.based.PR.programmes_Virtual group sessions (including telehealth/remote delivery)` = as.factor(`X2.18b.If..Yes..to.2.5..how.is.education.provided.for.home.based.PR.programmes_Virtual group sessions (including telehealth/remote delivery)`),
         X2.18b.If..Yes..to.2.5..how.is.education.provided.for.home.based.PR.programmes_Telephone = as.factor(X2.18b.If..Yes..to.2.5..how.is.education.provided.for.home.based.PR.programmes_Telephone),
         X2.18b.If..Yes..to.2.5..how.is.education.provided.for.home.based.PR.programmes_Other = as.factor(X2.18b.If..Yes..to.2.5..how.is.education.provided.for.home.based.PR.programmes_Other),
         `X2.19a.Who.delivers.group.sessions._Clinical psychologist` = as.factor(`X2.19a.Who.delivers.group.sessions._Clinical psychologist`),
         `X2.19a.Who.delivers.group.sessions._Health psychologist` = as.factor(`X2.19a.Who.delivers.group.sessions._Health psychologist`),
         `X2.19a.Who.delivers.group.sessions._Social worker` = as.factor(`X2.19a.Who.delivers.group.sessions._Social worker`),
         `X2.19b.face.to.face.or.virtual.group.sessions.include_Advance directives` = as.factor(`X2.19b.face.to.face.or.virtual.group.sessions.include_Advance directives`),
         X2.19b.face.to.face.or.virtual.group.sessions.include_Other = as.factor(X2.19b.face.to.face.or.virtual.group.sessions.include_Other),
         `X3.0.What.type.of.organisation.provides.your.service._Community interest company (CIC)` = as.factor(`X3.0.What.type.of.organisation.provides.your.service._Community interest company (CIC)`),
         X3.0.What.type.of.organisation.provides.your.service._Council = as.factor(X3.0.What.type.of.organisation.provides.your.service._Council),
         `X3.0.What.type.of.organisation.provides.your.service._Integrated Care Organisations (ICO)` = as.factor(`X3.0.What.type.of.organisation.provides.your.service._Integrated Care Organisations (ICO)`),
         `X3.0.What.type.of.organisation.provides.your.service._NHS acute trust` = as.factor(`X3.0.What.type.of.organisation.provides.your.service._NHS acute trust`),
         `X3.0.What.type.of.organisation.provides.your.service._NHS non-acute or community trust` = as.factor(`X3.0.What.type.of.organisation.provides.your.service._NHS non-acute or community trust`),
         `X3.0.What.type.of.organisation.provides.your.service._Private healthcare provider` = as.factor(`X3.0.What.type.of.organisation.provides.your.service._Private healthcare provider`),
         `X3.0.What.type.of.organisation.provides.your.service._Primary care network` = as.factor(`X3.0.What.type.of.organisation.provides.your.service._Primary care network`),
         `X3.2.Are.you.funded.for.all.the.conditions.listed_Chronic heart failure` = as.factor(`X3.2.Are.you.funded.for.all.the.conditions.listed_Chronic heart failure`),
         `X4.4.What.are.the.designations.of.the.staff.who.contribute_Exercise practitioner` = as.factor(`X4.4.What.are.the.designations.of.the.staff.who.contribute_Exercise practitioner`),
         `X4.4.What.are.the.designations.of.the.staff.who.contribute_Healthcare support worker` = as.factor(`X4.4.What.are.the.designations.of.the.staff.who.contribute_Healthcare support worker`),
         `X4.4.What.are.the.designations.of.the.staff.who.contribute_Lay person/patient representative` = as.factor(`X4.4.What.are.the.designations.of.the.staff.who.contribute_Lay person/patient representative`),
         X4.4.What.are.the.designations.of.the.staff.who.contribute_Psychologist = as.factor(X4.4.What.are.the.designations.of.the.staff.who.contribute_Psychologist),
         `X4.4.What.are.the.designations.of.the.staff.who.contribute_Qualified occupational therapist` = as.factor(`X4.4.What.are.the.designations.of.the.staff.who.contribute_Qualified occupational therapist`),
         `X4.4.What.are.the.designations.of.the.staff.who.contribute_Social worker` = as.factor(`X4.4.What.are.the.designations.of.the.staff.who.contribute_Social worker`),
         `X4.4.What.are.the.designations.of.the.staff.who.contribute_Therapy Assistant` = as.factor(`X4.4.What.are.the.designations.of.the.staff.who.contribute_Therapy Assistant`))



all <- cbind(all, freq_stats(dat_service_level, "X1.0.Referrals_Asthma", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X1.0.Referrals_Bronchiectasis", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X1.0.Referrals_COPD", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X1.0.Referrals_Interstitial lung disease", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X1.0.Referrals_Long covid", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X1.0.Referrals_Other chronic respiratory disease", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X1.0.Referrals_Pre/post thoracic surgery (including lung volume reduction)", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X1.0.Referrals_Pulmonary hypertension", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X1.0.Referrals_Chronic heart failure", remove_nos = TRUE))

all <- cbind(all, freq_stats(dat_service_level, "X1.1.PR.offerred.to.MRC.grades_Grade 1 and above", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X1.1.PR.offerred.to.MRC.grades_Grade 2 and above", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X1.1.PR.offerred.to.MRC.grades_Grade 3 and above", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X1.1.PR.offerred.to.MRC.grades_Grade 4 and above", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X1.1.PR.offerred.to.MRC.grades_Grade 5 and above", remove_nos = TRUE))


# Create the first load of column names

first <- dat_service_level %>% select(X1.2.Do.you.offer.PR.to.COPD.patients:X2.1.Do.you.run.cohort.or.rolling.PR.programmes) %>% colnames()


for (i in first) {
  all <- cbind(all, freq_stats(dat_service_level, i, remove_nos = TRUE))
}


# then we carry on

all <- cbind(all, freq_stats(dat_service_level, "X2.2.Duration.of.centre.based.PR.programme_6 weeks", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.2.Duration.of.centre.based.PR.programme_7 weeks", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.2.Duration.of.centre.based.PR.programme_8 weeks", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.2.Duration.of.centre.based.PR.programme_More than 8 weeks", remove_nos = TRUE))

all <- cbind(all, freq_stats(dat_service_level, "X2.3.How.many.supervised.centre.based.PR_1", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.3.How.many.supervised.centre.based.PR_2", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.3.How.many.supervised.centre.based.PR_More than 3", remove_nos = TRUE))


# Create the second load of column names

second.1 <- dat_service_level %>% select(X2.4.opportunity.to.access.directly.supervised..centre.based.PR:X2.5b.If..Yes...are.patients.offered.supervised.PR.sessions.in.their.homes.) %>% colnames()

for (i in second.1) {
  all <- cbind(all, freq_stats(dat_service_level, i, remove_nos = TRUE))
}


all <- cbind(all, freq_stats(dat_service_level, "X2.5c.If..Yes...what.is.the.typical.duration.of.your.home.based.PR.programme._5 weeks", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.5c.If..Yes...what.is.the.typical.duration.of.your.home.based.PR.programme._6 weeks", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.5c.If..Yes...what.is.the.typical.duration.of.your.home.based.PR.programme._7 weeks", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.5c.If..Yes...what.is.the.typical.duration.of.your.home.based.PR.programme._8 weeks", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.5c.If..Yes...what.is.the.typical.duration.of.your.home.based.PR.programme._More than 8 weeks", remove_nos = TRUE))

all <- cbind(all, freq_stats(dat_service_level, "X2.5d.If..Yes...how.many.supervised.home.based.PR.sessions.per.week_1", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.5d.If..Yes...how.many.supervised.home.based.PR.sessions.per.week_2", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.5d.If..Yes...how.many.supervised.home.based.PR.sessions.per.week_3", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.5d.If..Yes...how.many.supervised.home.based.PR.sessions.per.week_More than 3", remove_nos = TRUE))

all <- cbind(all, freq_stats(dat_service_level, "X2.6.Do.you.send.patients.information.about.your.PR.programme_Yes - verbal information", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.6.Do.you.send.patients.information.about.your.PR.programme_Yes - sent through the post", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.6.Do.you.send.patients.information.about.your.PR.programme_Yes - sent via email", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.6.Do.you.send.patients.information.about.your.PR.programme_Yes - directed to website", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.6.Do.you.send.patients.information.about.your.PR.programme_No", remove_nos = TRUE))


all <- cbind(all, freq_stats(dat_service_level, "X2.7.Is.transport.funded.for.patients.", remove_nos = TRUE))


all <- cbind(all, freq_stats(dat_service_level, "X2.8.Which.measures.of.aerobic.exercise_Endurance shuttle walk test (ESWT)", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.8.Which.measures.of.aerobic.exercise_Incremental shuttle walk test (ISWT)", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.8.Which.measures.of.aerobic.exercise_Six minute walk test (6MWT)", remove_nos = TRUE))


all <- cbind(all, freq_stats(dat_service_level, "X2.8a.If..6MWT...how.many.of.your.sites.use.a.30.metre.course.", remove_nos = TRUE))

all <- cbind(all, freq_stats(dat_service_level, "X2.9a.Is.lower.limb.muscle.strength.measured.at.the.initial.assessment", remove_nos = TRUE))

all <- cbind(all, freq_stats(dat_service_level, "X2.9b.Is.lower.limb.muscle.strength.measured.at.the.discharge.assessment", remove_nos = TRUE))


all <- cbind(all, freq_stats(dat_service_level, "X2.10.Are.any.of.the.following.measured.at.assessment._Activities of daily living scale", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.10.Are.any.of.the.following.measured.at.assessment._Hospital Anxiety and Depression Scores", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.10.Are.any.of.the.following.measured.at.assessment._Knowledge gained during education", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.10.Are.any.of.the.following.measured.at.assessment._Other psychological status scores", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.10.Are.any.of.the.following.measured.at.assessment._Patient satisfaction", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.10.Are.any.of.the.following.measured.at.assessment._Patient experience", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.10.Are.any.of.the.following.measured.at.assessment._Physical activity", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.10.Are.any.of.the.following.measured.at.assessment._No", remove_nos = TRUE))


all <- cbind(all, freq_stats(dat_service_level, "X2.11.Is.aerobic.training.offered.during.the.PR.programme.", remove_nos = TRUE))


all <- cbind(all, freq_stats(dat_service_level, "X2.11a.What.type.of.aerobic.training.is.undertaken.during.the.PR.programme._Cycling", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.11a.What.type.of.aerobic.training.is.undertaken.during.the.PR.programme._Walking", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.11a.What.type.of.aerobic.training.is.undertaken.during.the.PR.programme._Other", remove_nos = TRUE))


all <- cbind(all, freq_stats(dat_service_level, "X2.12.Is.aerobic.training.individually.prescribed.", remove_nos = TRUE))


all <- cbind(all, freq_stats(dat_service_level, "X2.12a.How.is.aerobic.training.prescribed._Borg or perceived exertion scores", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.12a.How.is.aerobic.training.prescribed._Endurance shuttle walking test (ESWT) level from ISWT", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.12a.How.is.aerobic.training.prescribed._From six-minute walk test (6MWT)", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.12a.How.is.aerobic.training.prescribed._From incremental shuttle walk test (ISWT)", remove_nos = TRUE))


all <- cbind(all, freq_stats(dat_service_level, "X2.12b.What.intensity.of.aerobic.exercise.prescription.is.used.", remove_nos = TRUE))

all <- cbind(all, freq_stats(dat_service_level, "X2.13.Is.resistance.training.offered.during.the.PR.programme.", remove_nos = TRUE))

all <- cbind(all, freq_stats(dat_service_level, "X2.14.Is.resistance.training.individually.prescribed.", remove_nos = TRUE))


all <- cbind(all, freq_stats(dat_service_level, "X2.14a.How.is.resistance.training.prescribed._Borg or perceived exertion scores", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.14a.How.is.resistance.training.prescribed._Measurement of 1RM or strength", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.14a.How.is.resistance.training.prescribed._Other", remove_nos = TRUE))


all <- cbind(all, freq_stats(dat_service_level, "X2.15.Are.patients.advised.to.do.unsupervised.home", remove_nos = TRUE))


# carry on...

all <- cbind(all, sum_stats_total(dat_service_level, "X2.16.How.many.hours.of.education.centre.based", roundno = 0))

all <- cbind(all, sum_stats_total(dat_service_level, "X2.17.How.many.hours.of.education.home.based", roundno = 0))


all <- cbind(all, freq_stats(dat_service_level, "X2.18a.How.is.education.provided_Information given digitally (including via memory stick/ email attachment/ CD/ DVD)", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.18a.How.is.education.provided_Face-to-face group education sessions", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.18a.How.is.education.provided_Virtual group sessions (including telehealth/remote delivery)", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.18a.How.is.education.provided_Information on a dedicated website including pre-recorded videos via YouTube etc", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.18a.How.is.education.provided_Written hand-outs", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.18a.How.is.education.provided_Telephone", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.18a.How.is.education.provided_Other", remove_nos = TRUE))


all <- cbind(all, freq_stats(dat_service_level, "X2.18b.If..Yes..to.2.5..how.is.education.provided.for.home.based.PR.programmes_Information given digitally (including via memory stick/ email attachment/ CD/ DVD)", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.18b.If..Yes..to.2.5..how.is.education.provided.for.home.based.PR.programmes_Face-to-face group education sessions", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.18b.If..Yes..to.2.5..how.is.education.provided.for.home.based.PR.programmes_Virtual group sessions (including telehealth/remote delivery)", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.18b.If..Yes..to.2.5..how.is.education.provided.for.home.based.PR.programmes_Information on a dedicated website including pre-recorded videos via YouTube etc", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.18b.If..Yes..to.2.5..how.is.education.provided.for.home.based.PR.programmes_Written hand-outs", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.18b.If..Yes..to.2.5..how.is.education.provided.for.home.based.PR.programmes_Telephone", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.18b.If..Yes..to.2.5..how.is.education.provided.for.home.based.PR.programmes_Other", remove_nos = TRUE))


all <- cbind(all, freq_stats(dat_service_level, "X2.19a.Who.delivers.group.sessions._Clinical psychologist", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.19a.Who.delivers.group.sessions._Dietician", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.19a.Who.delivers.group.sessions._Exercise physiologist", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.19a.Who.delivers.group.sessions._Fitness instructor", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.19a.Who.delivers.group.sessions._Health care/therapy assistant", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.19a.Who.delivers.group.sessions._Health psychologist", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.19a.Who.delivers.group.sessions._Occupational therapist", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.19a.Who.delivers.group.sessions._Pharmacist", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.19a.Who.delivers.group.sessions._Physiotherapist", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.19a.Who.delivers.group.sessions._Registered nurse", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.19a.Who.delivers.group.sessions._Respiratory physician", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.19a.Who.delivers.group.sessions._Social worker", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.19a.Who.delivers.group.sessions._Technical instructor", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.19a.Who.delivers.group.sessions._Volunteer", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.19a.Who.delivers.group.sessions._Other", remove_nos = TRUE))


all <- cbind(all, freq_stats(dat_service_level, "X2.19b.face.to.face.or.virtual.group.sessions.include_Advance directives", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.19b.face.to.face.or.virtual.group.sessions.include_Anatomy/physiology/pathology-in health and in chronic respiratory disease", remove_nos = TRUE)) # note the hyphen
all <- cbind(all, freq_stats(dat_service_level, "X2.19b.face.to.face.or.virtual.group.sessions.include_Anxiety management and relaxation", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.19b.face.to.face.or.virtual.group.sessions.include_Benefits system and welfare rights", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.19b.face.to.face.or.virtual.group.sessions.include_Chest clearance techniques", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.19b.face.to.face.or.virtual.group.sessions.include_Confidence/self-efficacy and self-management", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.19b.face.to.face.or.virtual.group.sessions.include_Dyspnoea/symptom management", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.19b.face.to.face.or.virtual.group.sessions.include_Energy conservation/pacing", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.19b.face.to.face.or.virtual.group.sessions.include_Exacerbation management (including coping with setbacks and relapses)", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.19b.face.to.face.or.virtual.group.sessions.include_Goal setting and rewards", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.19b.face.to.face.or.virtual.group.sessions.include_Identifying and changing beliefs about exercise and health related behaviours", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.19b.face.to.face.or.virtual.group.sessions.include_Loving relationships/sexuality", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.19b.face.to.face.or.virtual.group.sessions.include_Managing travel", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.19b.face.to.face.or.virtual.group.sessions.include_Medication (including oxygen therapy)", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.19b.face.to.face.or.virtual.group.sessions.include_Nutritional advice", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.19b.face.to.face.or.virtual.group.sessions.include_Opportunities to exercise after pulmonary rehabilitation", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.19b.face.to.face.or.virtual.group.sessions.include_Patient support groups", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.19b.face.to.face.or.virtual.group.sessions.include_Relaxation", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.19b.face.to.face.or.virtual.group.sessions.include_Smoking cessation", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.19b.face.to.face.or.virtual.group.sessions.include_The benefits of physical exercise", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.19b.face.to.face.or.virtual.group.sessions.include_Use of self-management plans", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X2.19b.face.to.face.or.virtual.group.sessions.include_Other", remove_nos = TRUE))


all <- cbind(all, freq_stats(dat_service_level, "X2.20.Routinely.provide.patients.with.an.individualised.structured..written.plan.", remove_nos = TRUE))


all <- cbind(all, freq_stats(dat_service_level, "X3.0.What.type.of.organisation.provides.your.service._Community interest company (CIC)", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X3.0.What.type.of.organisation.provides.your.service._Council", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X3.0.What.type.of.organisation.provides.your.service._Integrated Care Organisations (ICO)", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X3.0.What.type.of.organisation.provides.your.service._NHS acute trust", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X3.0.What.type.of.organisation.provides.your.service._NHS Health Board", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X3.0.What.type.of.organisation.provides.your.service._NHS non-acute or community trust", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X3.0.What.type.of.organisation.provides.your.service._Private healthcare provider", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X3.0.What.type.of.organisation.provides.your.service._Primary care network", remove_nos = TRUE))


all <- cbind(all, freq_stats(dat_service_level, "X3.1.What.type.of.funding.does.your.service.have._Fixed-term", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X3.1.What.type.of.funding.does.your.service.have._Non fixed-term", remove_nos = TRUE))


all <- cbind(all, freq_stats(dat_service_level, "X3.2.Are.you.funded.for.all.the.conditions.listed_Asthma", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X3.2.Are.you.funded.for.all.the.conditions.listed_Bronchiectasis", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X3.2.Are.you.funded.for.all.the.conditions.listed_COPD", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X3.2.Are.you.funded.for.all.the.conditions.listed_Interstitial lung disease", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X3.2.Are.you.funded.for.all.the.conditions.listed_Long covid", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X3.2.Are.you.funded.for.all.the.conditions.listed_Other chronic respiratory disease", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X3.2.Are.you.funded.for.all.the.conditions.listed_Pre/post thoracic surgery (including lung volume reduction)", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X3.2.Are.you.funded.for.all.the.conditions.listed_Pulmonary hypertension", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X3.2.Are.you.funded.for.all.the.conditions.listed_Chronic heart failure", remove_nos = TRUE))


all <- cbind(all, freq_stats(dat_service_level, "X4.1.Is.there.a.named.clinical.lead.for.the.service.", remove_nos = TRUE))


## X4.1a.Clinical.lead.profession..grade.and.WTE
dat_freq <- dat_service_level
dat_freq <- dat_freq %>%
  select(country, X4.1a.Clinical.lead.profession..grade.and.WTE)
# Function to split answers and form new rows
splitting <- function(data, cols_to_split) {
  for (col in cols_to_split) {
    data <- separate_rows(data, col, sep = ";")
  }
  return(data)
}
cols_to_split <- "X4.1a.Clinical.lead.profession..grade.and.WTE"
dat_freq <- splitting(dat_freq, cols_to_split)

# Split info into 3 columns
dat_freq <- separate(dat_freq, X4.1a.Clinical.lead.profession..grade.and.WTE, into = c("x4.1a.clinical.lead.profession", "x4.1a.grade", "x4.1a.WTE"), sep = ",")

dat_freq$x4.1a.clinical.lead.profession <- factor(dat_freq$x4.1a.clinical.lead.profession,
                                                  levels = c("Doctor",
                                                             "Exercise practitioner",
                                                             "Qualified nurse",
                                                             "Qualified occupational therapist",
                                                             "Qualified physiotherapist"))

dat_freq$x4.1a.grade <- factor(dat_freq$x4.1a.grade,
                               levels = c("4", "5", "6", "7", "8a", "8b", "8c", "9", "Not applicable"))

dat_freq$x4.1a.WTE <- as.numeric(dat_freq$x4.1a.WTE)

all <- cbind(all, freq_stats(dat_freq, "x4.1a.clinical.lead.profession", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_freq, "x4.1a.grade", remove_nos = TRUE))
all <- cbind(all, sum_stats_total(dat_freq, "x4.1a.WTE", roundno = 2))


all <- cbind(all, freq_stats(dat_service_level, "X4.1b.If..Yes...does.the.clinical.lead.receive.dedicated.sessional.time", remove_nos = TRUE))


## X4.2.How.many.types.of.posts.are.funded.for.the.service
dat_freq <- dat_service_level
dat_freq <- dat_freq %>%
  select(country, X4.2.How.many.types.of.posts.are.funded.for.the.service)
# Function to split answers and form new rows
splitting <- function(data, cols_to_split) {
  for (col in cols_to_split) {
    data <- separate_rows(data, col, sep = ";")
  }
  return(data)
}
cols_to_split <- "X4.2.How.many.types.of.posts.are.funded.for.the.service"
dat_freq <- splitting(dat_freq, cols_to_split)

# Split info into 3 columns
dat_freq <- separate(dat_freq, X4.2.How.many.types.of.posts.are.funded.for.the.service, into = c("x4.2.types.of.posts.funded.for.the.service", "x4.2.band", "x4.2.WTE"), sep = ",")

dat_freq$x4.2.types.of.posts.funded.for.the.service <- factor(dat_freq$x4.2.types.of.posts.funded.for.the.service,
                                                              levels = c("Admin and clerical",
                                                                         "Dietician/Nutritionist",
                                                                         "Exercise practitioner",
                                                                         "Healthcare support worker",
                                                                         "Pharmacist",
                                                                         "Psychologist",
                                                                         "Qualified nurse",
                                                                         "Qualified physiotherapist",
                                                                         "Qualified occupational therapist",
                                                                         "Therapy assistant"))

dat_freq$x4.2.band <- factor(dat_freq$x4.2.band,
                             levels = c("1", "2", "3", "4", "5", "6", "7", "8a", "8b", "8c", "8d", "9"))

dat_freq$x4.2.WTE <- as.numeric(dat_freq$x4.2.WTE)

all <- cbind(all, freq_stats(dat_freq, "x4.2.types.of.posts.funded.for.the.service", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_freq, "x4.2.band", remove_nos = TRUE))
all <- cbind(all, sum_stats_total(dat_freq, "x4.2.WTE", roundno = 2))


all <- cbind(all, sum_stats_total(dat_service_level, "X4.3.What.is.the.current.WTE.of.staff.vacancies.at.the.service.", roundno = 1))


# # # #

all <- cbind(all, freq_stats(dat_service_level, "X4.4.What.are.the.designations.of.the.staff.who.contribute_Admin and clerical", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X4.4.What.are.the.designations.of.the.staff.who.contribute_Community exercise instructor", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X4.4.What.are.the.designations.of.the.staff.who.contribute_Dietician/Nutritionist", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X4.4.What.are.the.designations.of.the.staff.who.contribute_Exercise practitioner", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X4.4.What.are.the.designations.of.the.staff.who.contribute_Healthcare support worker", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X4.4.What.are.the.designations.of.the.staff.who.contribute_Lay person/patient representative", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X4.4.What.are.the.designations.of.the.staff.who.contribute_Pharmacist", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X4.4.What.are.the.designations.of.the.staff.who.contribute_Physician", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X4.4.What.are.the.designations.of.the.staff.who.contribute_Psychologist", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X4.4.What.are.the.designations.of.the.staff.who.contribute_Qualified nurse", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X4.4.What.are.the.designations.of.the.staff.who.contribute_Qualified physiotherapist", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X4.4.What.are.the.designations.of.the.staff.who.contribute_Qualified occupational therapist", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X4.4.What.are.the.designations.of.the.staff.who.contribute_Social worker", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X4.4.What.are.the.designations.of.the.staff.who.contribute_Therapy Assistant", remove_nos = TRUE))



all <- cbind(all, freq_stats(dat_service_level, "X4.5.Is.there.audit.support.provided.", remove_nos = TRUE))

all <- cbind(all, sum_stats_total(dat_service_level, "X4.5a.How.many.WTE.of.audit.support.are.provided.", roundno = 1))

all <- cbind(all, freq_stats(dat_service_level, "X5.1.Do.you.have.a.Standard.Operating.Procedure..SOP..detailing.local.policies.", remove_nos = TRUE))


all <- cbind(all, freq_stats(dat_service_level, "X5.1a.What.does.the.Standard.Operating.Procedure.cover._Accessibility", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X5.1a.What.does.the.Standard.Operating.Procedure.cover._Capacity", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X5.1a.What.does.the.Standard.Operating.Procedure.cover._Digital or alternative modes of delivery", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X5.1a.What.does.the.Standard.Operating.Procedure.cover._DNA management", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X5.1a.What.does.the.Standard.Operating.Procedure.cover._Environment: facilities/kit and equipment", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X5.1a.What.does.the.Standard.Operating.Procedure.cover._Maintaining dignity and respect", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X5.1a.What.does.the.Standard.Operating.Procedure.cover._Managing waiting times", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X5.1a.What.does.the.Standard.Operating.Procedure.cover._Measurement of exercise outcomes", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X5.1a.What.does.the.Standard.Operating.Procedure.cover._Medication management", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X5.1a.What.does.the.Standard.Operating.Procedure.cover._Minimum staffing levels", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X5.1a.What.does.the.Standard.Operating.Procedure.cover._Patient and carer experience/satisfaction/feedback", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X5.1a.What.does.the.Standard.Operating.Procedure.cover._Patients needing oxygen", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X5.1a.What.does.the.Standard.Operating.Procedure.cover._Patient safety", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X5.1a.What.does.the.Standard.Operating.Procedure.cover._Patient security", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X5.1a.What.does.the.Standard.Operating.Procedure.cover._Risk assessments", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X5.1a.What.does.the.Standard.Operating.Procedure.cover._Staff training/development and well-being", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X5.1a.What.does.the.Standard.Operating.Procedure.cover._Transition care", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X5.1a.What.does.the.Standard.Operating.Procedure.cover._Use of IT equipment", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_service_level, "X5.1a.What.does.the.Standard.Operating.Procedure.cover._Whistle blowing", remove_nos = TRUE))



# # # # The end!!!!!



all %>% select(contains(inspectvect)) %>% colnames() # & ends_with(c("No_Total_N", "No_)) 

# Use the vector we created earlier of the columns we have to drop

ncol(all)   # 725

# Check which column has duplicated column name
col_names <- colnames(all)
col_counts <- table(col_names)
total_dup <- sum(col_counts > 1)
print(total_dup)
# name of duplicated column
dup_names <- names(all)[duplicated(names(all))]
dup_count <- table(names(all))[names(table(names(all))) %in% dup_names]
dup_info <- data.frame(Column_Name = names(dup_count), Count = as.vector(dup_count))
print(dup_info)


# There are several [Var1] in [all]
# We want to delete them
duplicated_col <- which(duplicated(names(all)))
all <- all[, -duplicated_col]
all <- all %>% select(-Var1)

ncol(all)   # 496


write.csv(all, "C:/Alex Harley/Audit_2023_onwards/2022-2023/PR/Analysis/Output/National_level_data_PR_org_2022-23.csv", row.names = FALSE)





# # # # # # # Benchmarking


# Split combined answers and create new columns
dat_benchmark <- dat_service_level

colnames(dat_benchmark)
# KPI_2_initial_discharge_assessment_homebased

dat_benchmark$KPI_2_initial_discharge_assessment_homebased <- NA
dat_benchmark$KPI_2_initial_discharge_assessment_homebased[dat_benchmark$X2.5.Do.you.offer.a.home.based.PR.programme. == "Yes"] <- "No"
dat_benchmark$KPI_2_initial_discharge_assessment_homebased[dat_benchmark$X2.5a.If..Yes...are.all.patients.offered.initial.and.discharge.assessments. == "Yes"] <- "Yes"
dat_benchmark$KPI_2_initial_discharge_assessment_homebased <- factor(dat_benchmark$KPI_2_initial_discharge_assessment_homebased, levels = c("Yes", "No"))


# KPI_3_6MWT_ex_cap_30_min
dat_benchmark$KPI_3_6MWT_ex_cap_30_min <- NA
dat_benchmark$KPI_3_6MWT_ex_cap_30_min[dat_benchmark$`X2.8.Which.measures.of.aerobic.exercise_Six minute walk test (6MWT)` == "Yes"] <- "No"
dat_benchmark$KPI_3_6MWT_ex_cap_30_min[dat_benchmark$`X2.8.Which.measures.of.aerobic.exercise_Six minute walk test (6MWT)` == "Yes" &
                                         (dat_benchmark$X2.8a.If..6MWT...how.many.of.your.sites.use.a.30.metre.course. == "All sites use a 30m course" |
                                            dat_benchmark$X2.8a.If..6MWT...how.many.of.your.sites.use.a.30.metre.course. == "At least 1 site uses a 30m course")] <- "Yes"
dat_benchmark$KPI_3_6MWT_ex_cap_30_min <- factor(dat_benchmark$KPI_3_6MWT_ex_cap_30_min, levels = c("Yes", "No"))


# KPI_4_written_plan
dat_benchmark$KPI_4_written_plan <- "No"
dat_benchmark$KPI_4_written_plan[dat_benchmark$X2.20.Routinely.provide.patients.with.an.individualised.structured..written.plan. == "Yes"] <- "Yes"
dat_benchmark$KPI_4_written_plan <- factor(dat_benchmark$KPI_4_written_plan, levels = c("Yes", "No"))


# KPI_5_clinical_leads_designated_sessional_time
dat_benchmark$KPI_5_clinical_leads_designated_sessional_time <- NA
dat_benchmark$KPI_5_clinical_leads_designated_sessional_time[dat_benchmark$X4.1.Is.there.a.named.clinical.lead.for.the.service. == "Yes - filled"] <- "No"
dat_benchmark$KPI_5_clinical_leads_designated_sessional_time[dat_benchmark$X4.1b.If..Yes...does.the.clinical.lead.receive.dedicated.sessional.time == "Yes"] <- "Yes"
dat_benchmark$KPI_5_clinical_leads_designated_sessional_time <- factor(dat_benchmark$KPI_5_clinical_leads_designated_sessional_time, levels = c("Yes", "No"))


# KPI_6_agreed_SOP
dat_benchmark$KPI_6_agreed_SOP <- "No"
dat_benchmark$KPI_6_agreed_SOP[dat_benchmark$X5.1.Do.you.have.a.Standard.Operating.Procedure..SOP..detailing.local.policies. == "Yes"] <- "Yes"
dat_benchmark$KPI_6_agreed_SOP <- factor(dat_benchmark$KPI_6_agreed_SOP, levels = c("Yes", "No"))



# Hospital level
bmk_service_level <- dat_benchmark %>% select(region, integrated_care_system, trust_name, service_name, starts_with("KPI_"))

bmk_service_level

write.csv(bmk_service_level, "C:/Alex Harley/Audit_2023_onwards/2022-2023/PR/Analysis/Output/Service_level_benchmarking_PR_org_2022-23.csv", row.names = FALSE)



# National level
bmk <- data.frame(Country = c("National (All)", "England", "Wales"))

KPI_vars <- dat_benchmark %>% select(starts_with("KPI_")) %>% colnames()

freq_stats_total(dat_benchmark, KPI_vars[1], remove_nos = TRUE, KPI = TRUE)

bmk <- cbind(bmk, 
             freq_stats_total(dat_benchmark, KPI_vars[1], remove_nos = TRUE, KPI = TRUE),
             freq_stats_total(dat_benchmark, KPI_vars[2], remove_nos = TRUE, KPI = TRUE),
             freq_stats_total(dat_benchmark, KPI_vars[3], remove_nos = TRUE, KPI = TRUE),
             freq_stats_total(dat_benchmark, KPI_vars[4], remove_nos = TRUE, KPI = TRUE),
             freq_stats_total(dat_benchmark, KPI_vars[5], remove_nos = TRUE, KPI = TRUE))
             # freq_stats(dat_benchmark, KPI_vars[6], remove_nos = TRUE, KPI = TRUE))




write.csv(bmk, "C:/Alex Harley/Audit_2023_onwards/2022-2023/PR/Analysis/Output/National_level_benchmarking_PR_org_2022-23.csv", row.names = FALSE)

