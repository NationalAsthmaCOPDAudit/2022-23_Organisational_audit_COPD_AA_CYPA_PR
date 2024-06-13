#---------------------------------------------------------------#
#                                                               #
#     C Y P   o r g a n i s a t i o n a l   a n a l y s i s     #
#                                                               #
#     Author: A l e x                                           #
#     Date created: 2 0 2 1 - 1 1 - 1 7                         #
#                                                               #
#     Edited by: H a r l e y                                    #
#     Date edited: 2 0 2 4 - 0 3 - 2 0                          #
#                                                               #
#---------------------------------------------------------------#


library(tidyverse)
library(tidyr)
library(dplyr)
library(readxl)
library(stringr)


dat <- read.csv("C:/Alex Harley/Audit_2023_onwards/2022-2023/CYPA/Data/rawData/NRAP-CYPAA---Org-Audit-2024---All-Data-20240401-111143-406.csv")

# Check any duplicates
print(dat[duplicated(dat$Hospital) | duplicated(dat$Hospital, fromLast = TRUE),])   # No duplicate for CYPA

# Probably easier with just full stops actually.


# 
# # Let's replace all the fullstops in the column names with underscores so it's nicer to look at
# 
# colnames(dat) <- gsub("\\.", "_", colnames(dat)) 
# colnames(dat)
# 
# # It's going to be annoying when there are multiple full stops though...
# 
# colnames(dat) <- gsub("____", "_", colnames(dat))
# colnames(dat) <- gsub("___", "_", colnames(dat))
# colnames(dat) <- gsub("__", "_", colnames(dat))
# 
# colnames(dat)


# Merge new hospital codes with original hospital code file
# hosp_dat <- read.csv("C:/Alex Harley/Audit_2023_onwards/General UK data/NACAP-ORGS-CYPA-230621.csv")
hosp_dat <- read_excel("C:/Alex Harley/Audit_2023_onwards/General UK data/CYPA Region-ICS mapping.xlsx", sheet = "All")

# str(hosp_dat_new)
str(hosp_dat)

# hosp_dat_new <- hosp_dat_new %>% rename(hosp_name = Hospital)
hosp_dat <- hosp_dat %>% rename(hosp_name = `Hospital name`,
                                hosp_code = `Hospital code`,
                                trust_name = `Trust name`,
                                trust_code = `Trust code`,
                                country = Country,
                                region = Region,
                                integrated_care_system = ICS)


# Check and print which hospitals are not matching in the 2 hospital data
# hosp_matched <- hosp_dat_new$hosp_name %in% hosp_dat$hosp_name
# sum(hosp_matched)
# 
# not_matched <- hosp_dat_new$hosp_name[!hosp_dat_new$hosp_name %in% hosp_dat$hosp_name]
# if(length(not_matched) > 0) {
#   cat("Hospitals in hosp_dat_new not existing in hosp_dat:\n")
#   print(not_matched)
# } else {
#   cat("Hospitals existing in both hosp_dat_new and hosp_dat:\n")
# }
# 
# 
# # Merge 
# hosp_dat <- left_join(hosp_dat_new, hosp_dat, by = "hosp_name")   # Based on the new dataset
# colnames(hosp_dat)
# 
# hosp_dat <- hosp_dat %>% select(-Trust.y, -ICS.y, -Region..SHA., -Country.y, -ODS.Code)
# hosp_dat <- hosp_dat %>% 
#   rename(trust_name = Trust.x,
#          integrated_care_system = ICS.x,
#          region = Region,
#          country = Country.x,
#          hosp_code = Crown.Code,
#          trust_code = Trust.Code)


# Double check which hospital code is missing after merging the 2 hospital datasets
# print(hosp_dat %>% filter(is.na(hosp_code)))   # 0 hospitals


# Organisational Audit data
dat <- dat %>% rename(hosp_code = OrgUnit,
                      hosp_name = Hospital)


# Check how many hospitals matching in both dat and hosp_dat
hosp_matched <- dat$hosp_name %in% hosp_dat$hosp_name
sum(hosp_matched)

not_matched <- dat$hosp_name[!dat$hosp_name %in% hosp_dat$hosp_name]
if(length(not_matched) > 0) {
  cat("Hospitals in dat not existing in hosp_dat:\n")
  print(not_matched)
} else {
  cat("Hospitals existing in both dat and hosp_dat:\n")
}



# Hospitals not having the same hosp_code in dat and hosp_dat
not_matched <- dat$hosp_code[!dat$hosp_code %in% hosp_dat$hosp_code]
if(length(not_matched) > 0) {
  cat("Hospitals in dat not existing in hosp_dat:\n")
  print(not_matched)
} else {
  cat("Hospitals existing in both dat and hosp_dat:\n")
}


# In dat, find those hospital with their corresponding hosp_code
dat %>% 
  filter(hosp_name %in% c("Ormskirk and District General Hospital", "Royal Devon and Exeter Hospital", "Chelsea and Westminster Hospital")) %>% 
  select(hosp_code, hosp_name)

# In hosp_dat, find those hospitals using their hosp_code
hosp_dat %>%
  filter(hosp_code %in% c("ORD", "RDE", "WES")) %>%
  select(hosp_code, hosp_name)

# The spelling of hospital differs in the 2 datasets...
# Correct the spelling manually (in hosp_dat)
hosp_dat <- hosp_dat %>%
  mutate(hosp_name = case_when(
    hosp_name == "Ormskirk & District General Hospital" ~ "Ormskirk and District General Hospital",
    hosp_name == "Royal Devon & Exeter Hospital" ~ "Royal Devon and Exeter Hospital",
    hosp_name == "Chelsea & Westminster Hospital" ~ "Chelsea and Westminster Hospital",
    TRUE ~ hosp_name))





# Merge
dat <- left_join(dat, hosp_dat, by = "hosp_name")
str(dat)


# Remove the dummy RCP hospital
dat <- dat %>%
  filter(hosp_name != "General Hospital (YYY)")


# Get rid of useless columns
dat <- dat %>% 
  select(-hosp_code.y, -EDeadline, -WebModifiedDate, -WebModifiedBy, -Form) %>% 
  rename(hosp_code = hosp_code.x) %>%
  relocate(trust_name:trust_code, .after = hosp_name)  # %>% rename(region = region_alt)


# hosp_dat <- read.csv("Z:/Group_work/PS_AA/General UK data/Hospital_codes_CYP_asthma_R_format.csv")
# 
# dat <- dat %>% rename(hosp_code = OrgUnit)
# 
# dat <- left_join(dat, hosp_dat, by = "hosp_code")
# colnames(dat)
# 
# dat <- dat %>% select(-Hospital, -region) %>% relocate(hosp_name:region_alt, .after = hosp_code) %>% rename(region = region_alt)


# Now we order them how they want in the spreadsheet

dat <- dat %>% 
  select(country, region, integrated_care_system, trust_name, hosp_name, everything())

dat %>% select(country, region, trust_name, hosp_name)

table(dat$region)


# Get rid of those who aren't complete:


# For CYPA this time, they said they will include all hospitals
dat <- dat %>% filter(Complete == "Yes")
# dat <- dat %>% select(-Complete)




# And then get rid of useless columns:

# colnames(dat)

# dat <- dat %>% select(-Complete, -EDeadline, -X3.3.On.call.paediatric.respiratory.consultants, -X3.8.Is.there.a.tobacco.dependence.service, -X3.8a.Is.there.a.vaping.dependence.service,
#                       -X3.10.Diag.tools.for.paediatric.asthma.patients, -X4.1.Senior.PAU.round.days.and.times, -X4.2.Senior.ward.round.days.and.times,
#                       -X4.3.Resp.nurse.s..available.to.review, -X5.2.system.of.paediatric.early.warning.detection.e.g..PEWS, -X7.1.transitioning.young.people.from.paediatric.to.adult, -Form)



# yeovil seem to be trying to get around the missing data thing, or they're having a joke...

# dat$X2.1.Asthma.nurse.specialist.unfilled[dat$X2.1.Asthma.nurse.specialist.unfilled == 99] <- NA
# dat$X2.1.Paediatric.consultants.unfilled[dat$X2.1.Paediatric.consultants.unfilled == 99] <- NA
# dat$X2.1.Physician.posts.ST3.and.above.unfilled[dat$X2.1.Physician.posts.ST3.and.above.unfilled == 99] <- NA
# dat$X2.1.Nurse.consultant.other.specialist.nurse.unfilled[dat$X2.1.Nurse.consultant.other.specialist.nurse.unfilled == 99] <- NA
# dat$X2.1.Paediatric.respiratory.consultants.unfilled[dat$X2.1.Paediatric.respiratory.consultants.unfilled == 99] <- NA


# Clean up the columns for calculation

dat %>% filter(hosp_code %in% c("FRM", "WEX", "CRY")) 

# Assign NA for inputs from 3 tricky hospitals
dat[dat$hosp_code %in% c("FRM", "WEX", "CRY"), c("X1.1.Paediatric.emergencies.hospital.admit",
                                                 "X1.2.Paediatric.respiratory.coded.emergencies",
                                                 "X1.3.Paediatric.asthma.coded.emergencies")] <- NA

dat %>% select(X1.4.do.you.have.a.short.stay.paediatric.assessment.unit, X1.4b..how.many.beds.within.the.paediatric.assessment.unit)
# ALEX EDITS - ZERO IS A LEGITIMATE ANSWER HERE
# dat$X1.4b..how.many.beds.within.the.paediatric.assessment.unit[dat$X1.4.do.you.have.a.short.stay.paediatric.assessment.unit == "Yes" & dat$X1.4b..how.many.beds.within.the.paediatric.assessment.unit == 0] <- NA

# ALEX EDITS - ZERO IS A LEGITIMATE ANSWER HERE
# dat$X1.5.Paediatric.medical.beds[dat$X1.5.Paediatric.medical.beds == 0] <- NA


# We need to actually do some calculations: 

dat <- dat %>% mutate(paed_resp_per_bed = round(X1.2.Paediatric.respiratory.coded.emergencies/X1.5.Paediatric.medical.beds, 1),
                      paed_asthma_per_1000_admission = round((X1.3.Paediatric.asthma.coded.emergencies/X1.1.Paediatric.emergencies.hospital.admit)*1000, 1)) %>%
  relocate(paed_resp_per_bed, .after = X1.2.Paediatric.respiratory.coded.emergencies) %>%
  relocate(paed_asthma_per_1000_admission, .after = X1.3.Paediatric.asthma.coded.emergencies)


# Create some functions to use with mutate for the WTE stuff
dat %>% select(X1.2.Paediatric.respiratory.coded.emergencies, X1.3.Paediatric.asthma.coded.emergencies)

all_paed_WTE <- function(x) {round((x/dat$X1.2.Paediatric.respiratory.coded.emergencies)*300, 1)}
just_asthma_WTE <- function(x) {round((x/dat$X1.3.Paediatric.asthma.coded.emergencies)*300, 1)}

dat %>% select(X1.2.Paediatric.respiratory.coded.emergencies, X1.3.Paediatric.asthma.coded.emergencies, starts_with("X2.1.Paediatric.consultants")) %>%
  head()
# Create all the extra columns that need to be created
head(dat)

dat %>% select(contains("filled")) %>% colnames()

dat <- dat %>% 
  mutate(X2.1.Physician.posts.ST3.and.above.filled_WTE_just_asthma = just_asthma_WTE(X2.1.Physician.posts.ST3.and.above.filled), 
         .after = X2.1.Physician.posts.ST3.and.above.unfilled) %>%
  mutate(X2.1.Physician.posts.ST3.and.above.filled_WTE_all_paed = all_paed_WTE(X2.1.Physician.posts.ST3.and.above.filled),
         .after = X2.1.Physician.posts.ST3.and.above.unfilled)

dat %>% select(X1.2.Paediatric.respiratory.coded.emergencies, X1.3.Paediatric.asthma.coded.emergencies, starts_with("X2.1.Physician.posts.ST3.and.above")) %>%
  head()


dat <- dat %>% 
  mutate(X2.1.Paediatric.consultants.filled_WTE_just_asthma = just_asthma_WTE(X2.1.Paediatric.consultants.filled),
         .after = X2.1.Paediatric.consultants.unfilled) %>% 
  mutate(X2.1.Paediatric.consultants.filled_WTE_all_paed = all_paed_WTE(X2.1.Paediatric.consultants.filled), 
         .after = X2.1.Paediatric.consultants.unfilled)

dat %>% select(X1.2.Paediatric.respiratory.coded.emergencies, X1.3.Paediatric.asthma.coded.emergencies, starts_with("X2.1.Paediatric.consultants")) %>%
  head()

dat <- dat %>% 
  mutate(X2.1.Paediatric.respiratory.consultants.filled_WTE_just_asthma = just_asthma_WTE(X2.1.Paediatric.respiratory.consultants.filled),
         .after = X2.1.Paediatric.respiratory.consultants.unfilled) %>% 
  mutate(X2.1.Paediatric.respiratory.consultants.filled_WTE_all_paed = all_paed_WTE(X2.1.Paediatric.respiratory.consultants.filled), 
         .after = X2.1.Paediatric.respiratory.consultants.unfilled)

dat <- dat %>% 
  mutate(X2.1.Paediatric.respiratory.sub.specialty.consultant.filled_WTE_just_asthma = just_asthma_WTE(X2.1.Paediatric.respiratory.sub.specialty.consultant.filled),
         .after = X2.1.Paediatric.respiratory.sub.specialty.consultant.unfilled) %>% 
  mutate(X2.1.Paediatric.respiratory.sub.specialty.consultant.filled_WTE_all_paed = all_paed_WTE(X2.1.Paediatric.respiratory.sub.specialty.consultant.filled), 
         .after = X2.1.Paediatric.respiratory.sub.specialty.consultant.unfilled)

dat <- dat %>% 
  mutate(X2.1.General.respiratory.specialist.nurse.filled_WTE_just_asthma = just_asthma_WTE(X2.1.General.respiratory.specialist.nurse.filled),
         .after = X2.1.General.respiratory.specialist.nurse.unfilled) %>% 
  mutate(X2.1.General.respiratory.specialist.nurse.filled_WTE_all_paed = all_paed_WTE(X2.1.General.respiratory.specialist.nurse.filled), 
         .after = X2.1.General.respiratory.specialist.nurse.unfilled)

dat <- dat %>% 
  mutate(X2.1.Asthma.nurse.specialist.filled_WTE_just_asthma = just_asthma_WTE(X2.1.Asthma.nurse.specialist.filled),
         .after = X2.1.Asthma.nurse.specialist.unfilled) %>% 
  mutate(X2.1.Asthma.nurse.specialist.filled_WTE_all_paed = all_paed_WTE(X2.1.Asthma.nurse.specialist.filled), 
         .after = X2.1.Asthma.nurse.specialist.unfilled)

dat <- dat %>% 
  mutate(X2.1.Nurse.consultant.other.specialist.nurse.filled_WTE_just_asthma = just_asthma_WTE(X2.1.Nurse.consultant.other.specialist.nurse.filled),
         .after = X2.1.Nurse.consultant.other.specialist.nurse.unfilled) %>% 
  mutate(X2.1.Nurse.consultant.other.specialist.nurse.filled_WTE_all_paed = all_paed_WTE(X2.1.Nurse.consultant.other.specialist.nurse.filled), 
         .after = X2.1.Nurse.consultant.other.specialist.nurse.unfilled)

head(dat)

# # # # # #

# Check the levels of the questions! 
# And just relevel the factors so the order is correct:

dat$X1.4.do.you.have.a.short.stay.paediatric.assessment.unit <- factor(dat$X1.4.do.you.have.a.short.stay.paediatric.assessment.unit,
                                                                       levels = c("Yes", "No"))

dat$X1.6.Does.your.hospital.have.access.to.a.virtual.ward <- factor(dat$X1.6.Does.your.hospital.have.access.to.a.virtual.ward,
                                                                    levels = c("Yes", "No"))

dat$X1.7.Paediatric.HDU <- factor(dat$X1.7.Paediatric.HDU,
                                  levels = c("Yes", "No"))

dat$X1.8.Paediatric.Intensive.Care.Unit <- factor(dat$X1.8.Paediatric.Intensive.Care.Unit,
                                                  levels = c("Yes", "No"))

dat$X3.1.Paediatric.admissions.routinely.review.weekdays <- factor(dat$X3.1.Paediatric.admissions.routinely.review.weekdays,
                                                                   levels = c("Twice daily", "Daily",  "More than twice daily", "Less than daily"))      

dat$X3.1.Paediatric.admissions.routinely.review.weekends <- factor(dat$X3.1.Paediatric.admissions.routinely.review.weekends,
                                                                   levels = c("Twice daily", "Daily",  "Other"))      

dat$X3.2.Access.to.a.paediatric.respiratory.nurse <- factor(dat$X3.2.Access.to.a.paediatric.respiratory.nurse,
                                                            levels = c("None", "All paediatric asthma patients", "Those under the care of either a paediatric respiratory consultant or a paediatric consultant with a respiratory special interest", "Other"))

dat$X3.2a..access.to.a.paediatric.respiratory.nurse.every.day.of.the.week <- factor(dat$X3.2a..access.to.a.paediatric.respiratory.nurse.every.day.of.the.week,
                                                                                    levels = c("None", "All paediatric asthma patients", "Those under the care of either a paediatric respiratory consultant or a paediatric consultant with a respiratory special interest", "Other"))

dat$X3.4.Regional.paediatric.asthma.network <- factor(dat$X3.4.Regional.paediatric.asthma.network,
                                                      levels = c("Yes", "No", "Not known"))

dat$X3.5.Designated.named.clinical.lead.for.asthma.services <- factor(dat$X3.5.Designated.named.clinical.lead.for.asthma.services,
  levels = c("Paediatric lead only", "Adult lead only", "Single lead for both adults and paediatrics", "No lead"))

# dat$X3.5a.Is.this.role.currently.filled[dat$X3.5a.Is.this.role.currently.filled == ""] <- "No"
dat$X3.5a.Is.this.role.currently.filled <- factor(dat$X3.5a.Is.this.role.currently.filled,
                                                      levels = c("Yes", "No", "Not known"))

dat$X3.6.Specific.service.for.paediatric.asthma <- factor(dat$X3.6.Specific.service.for.paediatric.asthma,
                                                          levels = c("Yes", "No", "Not known"))

# dat$X3.6a.If.not...Set.criteria.for.referral.offsite[dat$X3.6a.If.not...Set.criteria.for.referral.offsite == ""] <- "Not known"
dat$X3.6a.If.not...Set.criteria.for.referral.offsite <- factor(dat$X3.6a.If.not...Set.criteria.for.referral.offsite,
                                                               levels = c("Yes", "No"))

dat$X3.7.Asthma.lead.review.the.child.prior.to.referral <- factor(dat$X3.7.Asthma.lead.review.the.child.prior.to.referral,
                                            levels = c("Yes", "No", "Not applicable - we have specialist advice on site", "Not known"))

dat$X3.9.Home.based.community.service.post.discharge <- factor(dat$X3.9.Home.based.community.service.post.discharge,
                                                               levels = c("Yes", "No", "Not known"))

dat$X5.1.Electronic.Patient.Record..EPR..system <- factor(dat$X5.1.Electronic.Patient.Record..EPR..system,
                                                          levels = c("Yes", "No"))

dat$X5.2.system.of.paediatric.early.warning.detection.e.g..PEWS <- factor(dat$X5.2.system.of.paediatric.early.warning.detection.e.g..PEWS,
                                                                          levels = c("Yes", "No"))

dat$X5.3.oxygen.prescription.policy <- factor(dat$X5.3.oxygen.prescription.policy,
                                              levels = c("Yes", "No", "Not known"))

dat$X6.1.Formal.patient.parent.carer.surveys.undertaken <- factor(dat$X6.1.Formal.patient.parent.carer.surveys.undertaken,
     levels = c("Continuous (every patient)", "More than 4 times a year", "3-4 times a year", "1-2 times a year", "Less than once a year", "Never"))

dat$X6.2.Does.your.trust.have.a.strategic.group.for.asthma.services <- factor(dat$X6.2.Does.your.trust.have.a.strategic.group.for.asthma.services,
                                                                              levels = c("Yes", "No", "Not known"))

# dat$X6.2a.does.this.group.have.CYP.representation[dat$X6.2a.does.this.group.have.CYP.representation == ""] <- "Not known"
dat$X6.2a.does.this.group.have.CYP.representation <- factor(dat$X6.2a.does.this.group.have.CYP.representation,
                                                            levels = c("Yes", "No", "Not known"))

# dat$X6.2b.does.this.group.have.parent.carer.representation[dat$X6.2b.does.this.group.have.parent.carer.representation == ""] <- "Not known"
dat$X6.2b.does.this.group.have.parent.carer.representation <- factor(dat$X6.2b.does.this.group.have.parent.carer.representation,
                                                                     levels = c("Yes", "No", "Not known"))



# # # # # # # 

dat$X1.8a.How.many.beds.does.in.PICU <- as.numeric(dat$X1.8a.How.many.beds.does.in.PICU)

# # # # # # # 


# It's at this point, because I didn't take out the columns with 3 dots... I need to do it here. By creating this vector for use at the end.
# Have to do it here because they are in numeric format here

#inspectvect <- dat %>% select(where(is.numeric)) %>% select(contains("...")) %>% colnames()


dat_hosp_level <- dat
colnames(dat_hosp_level)


# Split columns with multiple answers by ";"
# Define the variables to be split
col_to_separate <- c("X2.2.expertises.that.members.of.staff.have", "X3.3.On.call.paediatric.respiratory.consultants",
                     "X3.8.Is.there.a.tobacco.dependence.service", "X3.8a.Is.there.a.vaping.dependence.service",
                     "X3.10.Diag.tools.for.paediatric.asthma.patients", "X4.1.Senior.PAU.round.days.and.times",
                     "X4.2.Senior.ward.round.days.and.times", "X4.3.Resp.nurse.s..available.to.review",
                     "X7.1.transitioning.young.people.from.paediatric.to.adult")

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
  dat_hosp_level <- split_create_col(dat_hosp_level, column)
}


# Rearrange the order of columns
dat_hosp_level <- dat_hosp_level %>%
  relocate(`X2.2.expertises.that.members.of.staff.have_Advanced clinical practitioner`:`X2.2.expertises.that.members.of.staff.have_Nicotine dependence advisor`, .after = X2.1.Nurse.consultant.other.specialist.nurse.filled_WTE_just_asthma) %>%
  relocate(`X3.3.On.call.paediatric.respiratory.consultants_On the phone`:`X3.3.On.call.paediatric.respiratory.consultants_No access`, .after = X3.2a..access.to.a.paediatric.respiratory.nurse.every.day.of.the.week) %>%
  relocate(`X3.8.Is.there.a.tobacco.dependence.service_No tobacco dependence service`:`X3.8a.Is.there.a.vaping.dependence.service_Paediatric asthma patients`, .after = X3.7.Asthma.lead.review.the.child.prior.to.referral) %>%
  relocate(X3.10.Diag.tools.for.paediatric.asthma.patients_Spirometry:`X4.3.Resp.nurse.s..available.to.review_Out of hours`, .after = X3.9.Home.based.community.service.post.discharge) %>%
  relocate(`X7.1.transitioning.young.people.from.paediatric.to.adult_The young person has a full record of their condition`:`X7.1.transitioning.young.people.from.paediatric.to.adult_We do not have any formal transition arrangements`, .after = X6.2b.does.this.group.have.parent.carer.representation)


dat_hosp_level <- dat_hosp_level %>% arrange(region, integrated_care_system, trust_code, trust_name, hosp_code, hosp_name) %>% # select(-country) %>%
  relocate(region, integrated_care_system, trust_code, trust_name, hosp_code, hosp_name)

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



# Need to do some rounding.(for hospital level only)

# Alex edits - changed from here so that it is dat_hosp_level_output from here, leaving dat_hosp_level able to be used
# as is for the national level analysis

dat_hosp_level_output <- dat_hosp_level

dat_hosp_level_output$paed_asthma_per_1000_admission <- round(dat_hosp_level_output$paed_asthma_per_1000_admission, 1)
dat_hosp_level_output$paed_asthma_per_1000_admission <- sprintf("%.1f", dat_hosp_level_output$paed_asthma_per_1000_admission)

dat_hosp_level_output$paed_resp_per_bed <- round(dat_hosp_level_output$paed_resp_per_bed, 1)
dat_hosp_level_output$paed_resp_per_bed <- sprintf("%.1f", dat_hosp_level_output$paed_resp_per_bed)

dat_hosp_level_output <- dat_hosp_level_output %>% mutate(across(starts_with("X2.1"), ~sprintf("%.1f", round(., 1))))

dat_hosp_level_output <- dat_hosp_level_output %>% select(-country)

write.csv(dat_hosp_level_output, "C:/Alex Harley/Audit_2023_onwards/2022-2023/CYPA/Analysis/Output/Hospital_level_data_CYP_org_2022-23_per300.csv",
         row.names = FALSE, na = "")


# # # # # # that is it for the hospital-level data!!! # # # # # # # # #







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

#dat <- dat %>% mutate(across(where(is.factor), ~fct_recode(., NULL = "")))


#summary(dat)

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
      
      
      
#       gen4 <- cbind(var_N, gen3[ ,2:3])
 #      colnames(gen4)[1] <- paste0(varname, "_", gsub(" ", "_", gen3[1,1]), "_Total_N")
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
     
      
     #  gen4 <- cbind(var_N, gen3[ ,2:3])
      # colnames(gen4)[1] <- paste0(varname, "_", gsub(" ", "_", gen3[1,1]), "_Total_N")
      tabby_wal <- cbind(tabby_wal, gen3)
    }
    
    
    tabby <- rbind(tabby_all, tabby_eng, tabby_wal)
    
    tabby$delete_me <- NULL
    
    if (KPI == TRUE) {   # If it's a KPI variable the order is slightly different
    
        tabby <- tabby[ , c(2, 1, 3)]
    
    }
    
    # Alex Edits - turn the first column to null to get rid of var1
#    print(tabby)
 #   print(str(tabby))
  #  colnames(tabby)
    
   # tabby$Var1 <- NULL
    tabby <- tabby %>% select(-Var1)
    return(tabby)
    

}
table(dat$X3.5.Designated.named.clinical.lead.for.asthma.services)


# Alex edits - it's at this point that we should remove the infinite rates as not helpful.

dat_hosp_level$paed_resp_per_bed[dat_hosp_level$paed_resp_per_bed == Inf] <- NA
dat_hosp_level$paed_resp_per_bed[dat_hosp_level$paed_resp_per_bed == Inf] <- NA

dat_hosp_level$paed_asthma_per_1000_admission

# # # # Start the data frame # # # # 

# Second column as the total number of hospitals in England and Wales

all <- data.frame(Country = c("National (All)", "England", "Wales"),
                  Total_hospitals = numeric(3))

# # # # # 

# Count the number of hospitals in each country

hosp_c <- table(dat_hosp_level$country)

all$Total_hospitals[1] <- sum(hosp_c)
all$Total_hospitals[2] <- hosp_c["England"]
all$Total_hospitals[3] <- hosp_c["Wales"]


# Create the first load of column names

#first.1 <- dat %>% select(X1.1.Paediatric.emergencies.hospital.admit:paed_asthma_per_paed_admission) %>% colnames()
                            
                            # X1.5.Paediatric.medical.beds) %>% colnames()


# for (i in first.1) {
#   all <- cbind(all, sum_stats(dat, i, roundno = 0))
# }

all <- cbind(all, sum_stats_total(dat_hosp_level, "X1.1.Paediatric.emergencies.hospital.admit", roundno = 0))

all <- cbind(all, sum_stats_total(dat_hosp_level, "X1.2.Paediatric.respiratory.coded.emergencies", roundno = 0))

all <- cbind(all, sum_stats(dat_hosp_level, "paed_resp_per_bed", roundno = 1))

all <- cbind(all, sum_stats_total(dat_hosp_level, "X1.3.Paediatric.asthma.coded.emergencies", roundno = 0))

all <- cbind(all, sum_stats(dat_hosp_level, "paed_asthma_per_1000_admission", roundno = 1))

all <- cbind(all, freq_stats(dat_hosp_level, "X1.4.do.you.have.a.short.stay.paediatric.assessment.unit", remove_nos = TRUE))

all <- cbind(all, sum_stats_total(dat_hosp_level, "X1.4a.coded.emerg.admit.to.a.short.stay.unit", roundno = 0))

all <- cbind(all, sum_stats_total(dat_hosp_level, "X1.4b..how.many.beds.within.the.paediatric.assessment.unit", roundno = 0))

all <- cbind(all, sum_stats_total(dat_hosp_level, "X1.5.Paediatric.medical.beds", roundno = 0))


# then we carry on

all <- cbind(all, freq_stats(dat_hosp_level, "X1.6.Does.your.hospital.have.access.to.a.virtual.ward", remove_nos = TRUE))

all <- cbind(all, sum_stats_total(dat_hosp_level, "X1.6.a.how.many.beds.does.your.paediatric.virtual.ward.have", roundno = 0))

all <- cbind(all, freq_stats(dat_hosp_level, "X1.7.Paediatric.HDU", remove_nos = TRUE))

all <- cbind(all, sum_stats_total(dat_hosp_level, "X1.7a..How.many.beds.paediatric.in.HDU", roundno = 0))

all <- cbind(all, freq_stats(dat_hosp_level, "X1.8.Paediatric.Intensive.Care.Unit", remove_nos = TRUE))

all <- cbind(all, sum_stats_total(dat_hosp_level, "X1.8a.How.many.beds.does.in.PICU", roundno = 1))

# all$X2.1.Physician.posts.ST3.and.above.filled_WTE_all_paed_Total <- all$X2.1.Physician.posts.ST3.and.above.filled_Total/all$X1.2.Paediatric.respiratory.coded.emergencies_Total

dat %>% select(contains("X2.1.Asthma.nurse.specialist"))

head(dat_hosp_level$X2.1.Physician.posts.ST3.and.above.filled)

second <- dat_hosp_level %>% select(X2.1.Physician.posts.ST3.and.above.filled:X2.1.Nurse.consultant.other.specialist.nurse.filled_WTE_just_asthma) %>% colnames()

dat %>% select(X2.1.Paediatric.consultants.filled:X2.1.Paediatric.consultants.unfilled) %>% head()

# Have to do this because slightly different things are wanted for each variable (some want totals, etc)
# 
# for (i in 1:5) {
#   all <- cbind(all, sum_stats(dat, second[(i*4) - 3], mean = FALSE, sd = FALSE, total = TRUE, roundno = 1))
#   all <- cbind(all, sum_stats(dat, second[(i*4) - 2], mean = FALSE, sd = FALSE, total = TRUE, roundno = 1))
#   all <- cbind(all, sum_stats(dat, second[(i*4) - 1], mean = FALSE, sd = FALSE, total = FALSE, roundno = 1))
#   all <- cbind(all, sum_stats(dat, second[(i*4) - 0], mean = FALSE, sd = FALSE, total = FALSE, roundno = 1))
# }

# Alex edits - add in total and take out mean and SD
# then remove the totals for the WTE rates

for (i in second) {
  all <- cbind(all, sum_stats_total(dat_hosp_level, i, mean = FALSE, sd = FALSE, total = TRUE, roundno = 1))
}

all <- all %>% select(-ends_with("all_paed_Total"), -ends_with("just_asthma_Total"))

all$X2.1.Nurse.consultant.other.specialist.nurse.filled_Total
dat$X2.1.Asthma.nurse.specialist.filled_WTE_all_paed

dat %>% select(starts_with("X2.1.Paediatric.consultants"))

dat_hosp_level <- dat_hosp_level %>%
  mutate(`X2.2.expertises.that.members.of.staff.have_Nicotine dependence advisor` = as.factor(`X2.2.expertises.that.members.of.staff.have_Nicotine dependence advisor`),
         X2.2.expertises.that.members.of.staff.have_Physiologist = as.factor(X2.2.expertises.that.members.of.staff.have_Physiologist),
         `X3.3.On.call.paediatric.respiratory.consultants_On site / virtual` = as.factor(`X3.3.On.call.paediatric.respiratory.consultants_On site / virtual`),
         `X3.3.On.call.paediatric.respiratory.consultants_No access` = as.factor(`X3.3.On.call.paediatric.respiratory.consultants_No access`),
         `X3.8a.Is.there.a.vaping.dependence.service_Parents/carers of paediatric asthma patients` = as.factor(`X3.8a.Is.there.a.vaping.dependence.service_Parents/carers of paediatric asthma patients`),
         `X3.8a.Is.there.a.vaping.dependence.service_Paediatric asthma patients` = as.factor(`X3.8a.Is.there.a.vaping.dependence.service_Paediatric asthma patients`),
         X3.10.Diag.tools.for.paediatric.asthma.patients_None = as.factor(X3.10.Diag.tools.for.paediatric.asthma.patients_None),
         X4.1.Senior.PAU.round.days.and.times_None = as.factor(X4.1.Senior.PAU.round.days.and.times_None),
         X4.2.Senior.ward.round.days.and.times_None = as.factor(X4.2.Senior.ward.round.days.and.times_None),
         X4.3.Resp.nurse.s..available.to.review_Weekends = as.factor(X4.3.Resp.nurse.s..available.to.review_Weekends),
         `X4.3.Resp.nurse.s..available.to.review_Out of hours` = as.factor(`X4.3.Resp.nurse.s..available.to.review_Out of hours`),
         `X4.3.Resp.nurse.s..available.to.review_No paediatric respiratory nurse available` = as.factor(`X4.3.Resp.nurse.s..available.to.review_No paediatric respiratory nurse available`),
         `X7.1.transitioning.young.people.from.paediatric.to.adult_The young person has a full record of their condition` = as.factor(`X7.1.transitioning.young.people.from.paediatric.to.adult_The young person has a full record of their condition`))



all <- cbind(all, freq_stats(dat_hosp_level, "X2.2.expertises.that.members.of.staff.have_Advanced clinical practitioner", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X2.2.expertises.that.members.of.staff.have_Dietician", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X2.2.expertises.that.members.of.staff.have_Nicotine dependence advisor", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X2.2.expertises.that.members.of.staff.have_Pharmacist", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X2.2.expertises.that.members.of.staff.have_Physiologist", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X2.2.expertises.that.members.of.staff.have_Physiotherapist", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X2.2.expertises.that.members.of.staff.have_Psychologist", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X2.2.expertises.that.members.of.staff.have_Psychiatrist", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X2.2.expertises.that.members.of.staff.have_Social worker", remove_nos = TRUE))



# Continue (x3.1 to x3.2a)
third_1 <- dat_hosp_level %>% select(X3.1.Paediatric.admissions.routinely.review.weekdays:X3.2a..access.to.a.paediatric.respiratory.nurse.every.day.of.the.week) %>% colnames()

for (i in third_1) {
  all <- cbind(all, freq_stats(dat_hosp_level, i, remove_nos = TRUE))
}


all <- cbind(all, freq_stats(dat_hosp_level, "X3.3.On.call.paediatric.respiratory.consultants_On site / virtual", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X3.3.On.call.paediatric.respiratory.consultants_On the phone", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X3.3.On.call.paediatric.respiratory.consultants_Regional outreach service", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X3.3.On.call.paediatric.respiratory.consultants_No access", remove_nos = TRUE))


# Continue (x3.4 to x3.7)
third_2 <- dat_hosp_level %>% select(X3.4.Regional.paediatric.asthma.network:X3.7.Asthma.lead.review.the.child.prior.to.referral) %>% colnames()

for (i in third_2) {
  all <- cbind(all, freq_stats(dat_hosp_level, i, remove_nos = FALSE))
}


all <- cbind(all, freq_stats(dat_hosp_level, "X3.8.Is.there.a.tobacco.dependence.service_Parents/carers of paediatric asthma patients", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X3.8.Is.there.a.tobacco.dependence.service_Paediatric asthma patients", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X3.8.Is.there.a.tobacco.dependence.service_No tobacco dependence service", remove_nos = TRUE))


all <- cbind(all, freq_stats(dat_hosp_level, "X3.8a.Is.there.a.vaping.dependence.service_Parents/carers of paediatric asthma patients", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X3.8a.Is.there.a.vaping.dependence.service_Paediatric asthma patients", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X3.8a.Is.there.a.vaping.dependence.service_No vaping dependence service", remove_nos = TRUE))


all <- cbind(all, freq_stats(dat_hosp_level, "X3.9.Home.based.community.service.post.discharge", remove_nos = TRUE))


all <- cbind(all, freq_stats(dat_hosp_level, "X3.10.Diag.tools.for.paediatric.asthma.patients_Spirometry", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X3.10.Diag.tools.for.paediatric.asthma.patients_Bronchodilator reversibility", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X3.10.Diag.tools.for.paediatric.asthma.patients_Peak expiratory flow (PEF)", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X3.10.Diag.tools.for.paediatric.asthma.patients_Exhaled nitric oxide (FeNO)", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X3.10.Diag.tools.for.paediatric.asthma.patients_Skin prick test", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X3.10.Diag.tools.for.paediatric.asthma.patients_None", remove_nos = TRUE))


all <- cbind(all, freq_stats(dat_hosp_level, "X4.1.Senior.PAU.round.days.and.times_Weekdays", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X4.1.Senior.PAU.round.days.and.times_Weekends", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X4.1.Senior.PAU.round.days.and.times_Out of hours", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X4.1.Senior.PAU.round.days.and.times_None", remove_nos = TRUE))


all <- cbind(all, freq_stats(dat_hosp_level, "X4.2.Senior.ward.round.days.and.times_Weekdays", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X4.2.Senior.ward.round.days.and.times_Weekends", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X4.2.Senior.ward.round.days.and.times_Out of hours", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X4.2.Senior.ward.round.days.and.times_None", remove_nos = TRUE))


all <- cbind(all, freq_stats(dat_hosp_level, "X4.3.Resp.nurse.s..available.to.review_Weekdays", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X4.3.Resp.nurse.s..available.to.review_Weekends", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X4.3.Resp.nurse.s..available.to.review_Out of hours", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X4.3.Resp.nurse.s..available.to.review_No paediatric respiratory nurse available", remove_nos = TRUE))


all <- cbind(all, freq_stats(dat_hosp_level, "X5.1.Electronic.Patient.Record..EPR..system", remove_nos = TRUE))



fifth <- dat_hosp_level %>% select(X5.2.system.of.paediatric.early.warning.detection.e.g..PEWS:X6.2b.does.this.group.have.parent.carer.representation) %>% colnames()

for (i in fifth) {
  all <- cbind(all, freq_stats(dat_hosp_level, i))
}


all <- cbind(all, freq_stats(dat_hosp_level, "X7.1.transitioning.young.people.from.paediatric.to.adult_The young person has a full record of their condition", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X7.1.transitioning.young.people.from.paediatric.to.adult_The GP is sent the same record", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X7.1.transitioning.young.people.from.paediatric.to.adult_The young person is given the opportunity to be seen without their parents/carers", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X7.1.transitioning.young.people.from.paediatric.to.adult_The young person has a transition plan that has been agreed with both paediatric and adult clinicians", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X7.1.transitioning.young.people.from.paediatric.to.adult_The young person is allocated a coordinator/case worker to support them until settled within the adult system", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X7.1.transitioning.young.people.from.paediatric.to.adult_We do not have any formal transition arrangements", remove_nos = TRUE))



# # # # The end!!!!!

all %>% select(contains(inspectvect)) %>% colnames() # & ends_with(c("No_Total_N", "No_)) 

# Use the vector we created earlier of the columns we have to drop

ncol(all)   # 503

#all <- all %>% select(!(contains(inspectvect) & ends_with(c("No_n", "No_perc")))) # %>% colnames()


# Check which column has duplicated column name
col_names <- colnames(all)
col_counts <- table(col_names)
print(col_counts)
# Var1?
all %>% select(contains("Var")) %>% colnames()
# name of duplicated column
dup_names <- names(all)[duplicated(names(all))]
dup_count <- table(names(all))[names(table(names(all))) %in% dup_names]
dup_info <- data.frame(Column_Name = names(dup_count), Count = as.vector(dup_count))
print(dup_info)

# There are several [Var1] in [all]
# We want to delete them
duplicated_col <- which(duplicated(names(all)))

# Alex edits - hashed out two lines below
# all <- all[, -duplicated_col]
# all <- all %>% select(-Var1)

ncol(all)   # 404
summary(dat)

write.csv(all, "C:/Alex Harley/Audit_2023_onwards/2022-2023/CYPA/Analysis/Output/National_level_data_CYP_org_2022-23_per300.csv", row.names = FALSE)



# # # # # # # Benchmarking

# Make the benchmarking variables
col_to_separate <- c("X3.10.Diag.tools.for.paediatric.asthma.patients",
                     "X3.8.Is.there.a.tobacco.dependence.service",
                     "X7.1.transitioning.young.people.from.paediatric.to.adult")

# Split combined answers and create new columns
dat_benchmark <- dat

split_create_col <- function(data, column) {
  
  # Split combined answers into separate elements
  separated_answers <- strsplit(data[[column]], ";")
  
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
  dat_benchmark <- split_create_col(dat_benchmark, column)
}


# KPI_1_spiro_feno
dat_benchmark$KPI_1_spiro_feno <- "No"
dat_benchmark$KPI_1_spiro_feno[dat_benchmark$X3.10.Diag.tools.for.paediatric.asthma.patients_Spirometry == "Yes" &
                                 dat_benchmark$`X3.10.Diag.tools.for.paediatric.asthma.patients_Exhaled nitric oxide (FeNO)` == "Yes"] <- "Yes"
dat_benchmark$KPI_1_spiro_feno <- factor(dat_benchmark$KPI_1_spiro_feno, levels = c("Yes", "No"))


# KPI_2_paed_rep_nurse
dat_benchmark$KPI_2_paed_resp_nurse <- "No"
dat_benchmark$KPI_2_paed_resp_nurse[dat_benchmark$X3.2.Access.to.a.paediatric.respiratory.nurse == "All paediatric asthma patients"] <- "Yes"
dat_benchmark$KPI_2_paed_resp_nurse <- factor(dat_benchmark$KPI_2_paed_resp_nurse, levels = c("Yes", "No"))


# KPI_3_clinical_lead_asthma
dat_benchmark$KPI_3_clinical_lead_asthma <- "No"
# dat_benchmark$KPI_3_clinical_lead_asthma[dat_benchmark$X3.5.Designated.named.clinical.lead.for.asthma.services != "No lead"] <- "Yes"
dat_benchmark$KPI_3_clinical_lead_asthma[dat_benchmark$X3.5.Designated.named.clinical.lead.for.asthma.services %in% 
                                           c("Paediatric lead only", "Single lead for both adults and paediatrics")] <- "Yes"

dat_benchmark$KPI_3_clinical_lead_asthma <- factor(dat_benchmark$KPI_3_clinical_lead_asthma, levels = c("Yes", "No"))


# KPI_4_smoking_cess
dat_benchmark$KPI_4_smoking_cess <- "No"
dat_benchmark$KPI_4_smoking_cess[dat_benchmark$`X3.8.Is.there.a.tobacco.dependence.service_Parents/carers of paediatric asthma patients` == "Yes" &
                                   dat_benchmark$`X3.8.Is.there.a.tobacco.dependence.service_Paediatric asthma patients` == "Yes"] <- "Yes"
dat_benchmark$KPI_4_smoking_cess <- factor(dat_benchmark$KPI_4_smoking_cess, levels = c("Yes", "No"))


# KPI_5_transition
dat_benchmark$KPI_5_transition <- "No"
dat_benchmark$KPI_5_transition[dat_benchmark$`X7.1.transitioning.young.people.from.paediatric.to.adult_The GP is sent the same record` == "Yes"] <- "Yes"
dat_benchmark$KPI_5_transition[dat_benchmark$`X7.1.transitioning.young.people.from.paediatric.to.adult_The young person has a full record of their condition` == "Yes"] <- "Yes"
dat_benchmark$KPI_5_transition[dat_benchmark$`X7.1.transitioning.young.people.from.paediatric.to.adult_The young person has a transition plan that has been agreed with both paediatric and adult clinicians` == "Yes"] <- "Yes"
dat_benchmark$KPI_5_transition[dat_benchmark$`X7.1.transitioning.young.people.from.paediatric.to.adult_The young person is allocated a coordinator/case worker to support them until settled within the adult system` == "Yes"] <- "Yes"
dat_benchmark$KPI_5_transition[dat_benchmark$`X7.1.transitioning.young.people.from.paediatric.to.adult_The young person is given the opportunity to be seen without their parents/carers` == "Yes"] <- "Yes"
dat_benchmark$KPI_5_transition <- factor(dat_benchmark$KPI_5_transition, levels = c("Yes", "No"))



# Hospital level
bmk_hosp_level <- dat_benchmark %>% select(region, integrated_care_system, trust_name, hosp_name, starts_with("KPI_"))

bmk_hosp_level$region[is.na(bmk_hosp_level$region)] <- "Wales" 

bmk_hosp_level %>% filter(region == "Wales")

write.csv(bmk_hosp_level, "C:/Alex Harley/Audit_2023_onwards/2022-2023/CYPA/Analysis/Output/Hospital_level_benchmarking_CYP_org_2022-23.csv", row.names = FALSE)



# National level
bmk <- data.frame(Country = c("National (All)", "England", "Wales"),
                  Total_N = c(nrow(bmk_hosp_level), nrow(bmk_hosp_level %>% filter(region != "Wales")), 
                                                    nrow(bmk_hosp_level %>% filter(region == "Wales"))))




KPI_vars <- dat_benchmark %>% select(starts_with("KPI_")) %>% colnames()

bmk <- cbind(bmk, 
             freq_stats(dat_benchmark, KPI_vars[1], remove_nos = TRUE, KPI = TRUE),
             freq_stats(dat_benchmark, KPI_vars[2], remove_nos = TRUE, KPI = TRUE),
             freq_stats(dat_benchmark, KPI_vars[3], remove_nos = TRUE, KPI = TRUE),
             freq_stats(dat_benchmark, KPI_vars[4], remove_nos = TRUE, KPI = TRUE),
             freq_stats(dat_benchmark, KPI_vars[5], remove_nos = TRUE, KPI = TRUE))

# 
# # Check which column has duplicated column name
# col_names <- colnames(bmk)
# col_counts <- table(col_names)
# print(col_counts)
# # Var1?
# bmk %>% select(contains("Var")) %>% colnames()
# # name of duplicated column
# dup_names <- names(bmk)[duplicated(names(bmk))]
# dup_count <- table(names(bmk))[names(table(names(bmk))) %in% dup_names]
# dup_info <- data.frame(Column_Name = names(dup_count), Count = as.vector(dup_count))
# print(dup_info)
# 
# # There are several [Var1] in [bmk]
# # We want to delete them
# duplicated_col <- which(duplicated(names(bmk)))
# bmk <- bmk[, -duplicated_col]
# bmk <- bmk %>% select(-Var1)
# 
# ncol(bmk)


write.csv(bmk, "C:/Alex Harley/Audit_2023_onwards/2022-2023/CYPA/Analysis/Output/National_level_benchmarking_CYP_org_2022-23.csv", row.names = FALSE)

