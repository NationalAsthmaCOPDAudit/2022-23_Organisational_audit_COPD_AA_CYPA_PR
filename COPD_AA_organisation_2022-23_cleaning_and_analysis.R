#---------------------------------------------------------------#
#                                                               #
#  A A  C O P D  o r g a n i s a t i o n a l   a n a l y s i s  #
#                                                               #
#                                                               #
#  Author: H a r l e y                                          #
#  Date created: 2 0 2 4 - 0 4 - 1 6                            #
#                                                               #
#                                                               #
#---------------------------------------------------------------#


library(tidyverse)
library(dplyr)
library(readxl)
library(tidyr)

dat_old <- read.csv("C:/Alex Harley/Previous_pre-2023_audits_data_and_analysis/NRAP audit - Phil's analyses/National Asthma and COPD Audit Programme (NACAP)/2021 COPD & Adult Asthma Organisational Audit/raw_data/NACAP-COPD---AA-Org-Audit-2021---Data-20211019-141924-329.csv")

dat <- read.csv("C:/Alex Harley/Audit_2023_onwards/2022-2023/AA/Data/rawData/NRAP-COPD---AA-Org-Audit-2024---Data-20240401-111131-250.csv")


# Check whether there are duplicates
print(dat[duplicated(dat$Hospital.Name) | duplicated(dat$Hospital.Name, fromLast = TRUE),])
# 3 duplicates:
# Darlington Memorial Hospital (DAR) [Exclude this time]
# King George Hospital (KGG)
# Lincoln County Hospital (LIN) [Exclude this time]

# Remove duplicates based on the completeness of the entry
dat <- dat %>%
  filter(!(duplicated(Hospital.Name) & Completed. == ""))  
# Since Lincoln has 2 complete entries -> further remove duplicate
dat <- dat %>%
  distinct(Hospital.Name, .keep_all = TRUE)


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
# hosp_dat <- read.csv("C:/Alex Harley/Audit_2023_onwards/General UK data/NACAP-ORGS-AA-230621.csv")
hosp_dat <- read_excel("C:/Alex Harley/Audit_2023_onwards/General UK data/AA Region-ICS mapping.xlsx", sheet = "All")

# str(hosp_dat_new)
str(hosp_dat)

# hosp_dat_new <- hosp_dat_new %>% rename(hosp_name = Hospital)
hosp_dat <- hosp_dat %>% rename(hosp_name = `Hospital name`,
                                hosp_code = `Hospital code`,
                                trust_name = Trust,
                                trust_code = `Trust code`,
                                region = Region,
                                country = Country,
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
# All match!


# Merge 
# hosp_dat <- left_join(hosp_dat_new, hosp_dat, by = "hosp_name")   # Based on the new dataset
# colnames(hosp_dat)
# 
# hosp_dat <- hosp_dat %>% select(-trust_name, -ICS.y, -region, -country, -ODS.Code)
# hosp_dat <- hosp_dat %>% 
#   rename(trust_name = Trust,
#          integrated_care_system = ICS.x,
#          region = Region,
#          country = Country)


# Double check which hospital code is missing after merging the 2 hospital datasets
# print(hosp_dat %>% filter(is.na(hosp_code)))   # 0 hospitals


# Organisational Audit data
dat <- dat %>% rename(hosp_code = Hospital.Code,
                      hosp_name = Hospital.Name)


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


dat %>% filter(hosp_name %in% c("Royal Devon and Exeter Hospital", "Chelsea and Westminster Hospital")) %>% select(hosp_code, hosp_name)
# RDE
# WES


# Hospitals not having the same hosp_code in dat and hosp_dat
not_matched <- dat$hosp_code[!dat$hosp_code %in% hosp_dat$hosp_code]
if(length(not_matched) > 0) {
  cat("Hospitals in dat not existing in hosp_dat:\n")
  print(not_matched)
} else {
  cat("Hospitals existing in both dat and hosp_dat:\n")
}



hosp_dat %>% filter(hosp_code %in% c("RDE", "WES")) %>% select(hosp_code, hosp_name)
# Chelsea & Westminster Hospital
# Royal Devon & Exeter Hospital

# Change name in hosp_dat
hosp_dat <- hosp_dat %>%
  mutate(hosp_name = case_when(
    hosp_name == "Chelsea & Westminster Hospital" ~ "Chelsea and Westminster Hospital",
    hosp_name == "Royal Devon & Exeter Hospital" ~ "Royal Devon and Exeter Hospital",
    TRUE ~ hosp_name))



# Merge
dat <- left_join(dat, hosp_dat, by = "hosp_name")
colnames(dat)


# Get rid of those who do not consent to participate:
dat <- dat %>%
  filter(!(hosp_name %in% c("Royal Bolton Hospital", "Bronglais General Hospital", "Chesterfield Royal", "Croydon University Hospital",
                            "Darlington Memorial Hospital", "University Hospital of North Durham", "Ysbyty Gwynedd Hospital",
                            "Lincoln County Hospital", "James Cook University Hospital", "Wythenshawe Hospital", "General Hospital (YYY)")))
# Originally: 163; after removal of non-consent: 152



# Get rid of useless columns
dat <- dat %>% 
  select(-hosp_code.y, -Deadline, -Modified.Date, -Modified.By, -Dataset) %>% 
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



dat <- dat %>% filter(Completed. == "Yes")   # n=138 after removing incompleteness (14 incomplete entries)
# dat <- dat %>% select(-Complete)

dat$X1.1.Adult.Admissions.20.21
dat$X1.2.Respiratory.Coded.Emergency.Admissions


####################
### Adult asthma ###
####################

# X1.5.AA.Coded.Emergency.Admissions, X1.6a..AA.Coded.Emergency.non.Ward.Discharges, X1.6.AA.Coded.Emergency.Ward.Discharges,
# X3.3.Clinical.Lead.AA, X4.4b.Respiratory.Nurse.Reviews.AA, X4.5b.Physio.Review.AA, X6.1.Does.your.hospital.have.a.severe.asthma.service,
# X6.1a.does.your.hospital.have.access.to.severe.asthma.MDT, X6.1b.do.you.have.a.referral.pathway.to.a.severe.asthma.service,
# X6.2.hospital.provide.access.to.a.Asthma.biologics.drugs, X6.2a.biotypes.all.that.apply, X8.1.Transition.Paediatric.To.Adult


############
### COPD ###
############

# X1.3.COPD.Coded.Emergency.Admissions, X1.4a..COPD.Coded.Emergency.non.Ward.Discharges, X1.4b.COPD.Coded.Emergency.Ward.Discharges,
# X3.2.Clinical.Lead.COPD, X4.4a.Respiratory.Nurse.Reviews.COPD, X4.5a.Physio.Review.COPD,
# X5.3.pulmonary.rehabilitation.service.available.to.COPD.patients, X5.3a.PR.Service.30.Days.COPD


# Clean up the columns for calculation

# remove hospitals missing admissions data

dat <- dat %>% filter(X1.1.Adult.Admissions.20.21 != 0)

dat$X1.4b.COPD.Coded.Emergency.Ward.Discharges <- ifelse(dat$X1.3.COPD.Coded.Emergency.Admissions != dat$X1.4a..COPD.Coded.Emergency.non.Ward.Discharges & 
                                                           dat$X1.4b.COPD.Coded.Emergency.Ward.Discharges == 0, NA, dat$X1.4b.COPD.Coded.Emergency.Ward.Discharges)

dat$X1.4a..COPD.Coded.Emergency.non.Ward.Discharges[dat$X1.4a..COPD.Coded.Emergency.non.Ward.Discharges == 0] <- NA

dat$X1.6.AA.Coded.Emergency.Ward.Discharges <- ifelse(dat$X1.5.AA.Coded.Emergency.Admissions != dat$X1.6a..AA.Coded.Emergency.non.Ward.Discharges & dat$X1.6.AA.Coded.Emergency.Ward.Discharges == 0, NA, dat$X1.6.AA.Coded.Emergency.Ward.Discharges)

dat$X1.6a..AA.Coded.Emergency.non.Ward.Discharges[dat$X1.6a..AA.Coded.Emergency.non.Ward.Discharges == 0] <- NA


# Alex edits - not useless column.
# can be used to add in zeros for some columns

# # Further get rid of useless columns
# dat <- dat %>%
#   select(-X1.4b.No.dedicated.respiratory.ward, -X1.6a.No.dedicated.respiratory.ward, -X1.8.No.Respiratory.Ward.Beds,
#          -X1.8a.No.Respiratory.Ward.Beds.L2, -X1.11.No.virt.beds)

# Alex edits - if resp wards are inconsistent, remove. 

table(dat$X1.8.No.Respiratory.Ward.Beds)
table(dat$X1.4b.No.dedicated.respiratory.ward)
table(dat$X1.6a.No.dedicated.respiratory.ward)
table(dat$X1.8.No.Respiratory.Ward.Beds)

table(dat$X1.6a.No.dedicated.respiratory.ward, dat$X1.8.No.Respiratory.Ward.Beds, useNA = "ifany")

# If they say that they don't have a respiratory ward but they say that they do have respiratory ward beds,
# change respiratory ward bed question to blanks. 

dat$X1.8.Respiratory.Ward.Beds[dat$X1.8.No.Respiratory.Ward.Beds == "No dedicated respiratory wards" | 
                                 dat$X1.6a.No.dedicated.respiratory.ward == "We do not have a dedicated respiratory ward"] <- NA




drop <- dat %>% filter(!is.na(X1.8.Respiratory.Ward.Beds) & X1.6a.No.dedicated.respiratory.ward == "We do not have a dedicated respiratory ward")
nrow(drop)

drop <- dat %>% filter(X1.6a.No.dedicated.respiratory.ward == "We do not have a dedicated respiratory ward")
nrow(drop)

# We need to actually do some calculations: 

dat <- dat %>% mutate(adm_per_bed = round(X1.1.Adult.Admissions.20.21/X1.7.Medical.Beds, 1),
                      resp_adm_per_bed = round(X1.2.Respiratory.Coded.Emergency.Admissions/X1.8.Respiratory.Ward.Beds, 1),
                      copd_per_1000_admission = round((X1.3.COPD.Coded.Emergency.Admissions/X1.1.Adult.Admissions.20.21)*1000, 1),
                      nat_prop_copd_admission = round((X1.4b.COPD.Coded.Emergency.Ward.Discharges/X1.3.COPD.Coded.Emergency.Admissions)*100, 1),
                      adult_asthma_per_1000_admission = round((X1.5.AA.Coded.Emergency.Admissions/X1.1.Adult.Admissions.20.21)*1000, 1),
                      nat_prop_asthma_admission = round((X1.6.AA.Coded.Emergency.Ward.Discharges/X1.5.AA.Coded.Emergency.Admissions)*100, 1),
                      L2_bed_per_1000_resp_admission = round((X1.8a.Respiratory.Ward.Beds.L2/X1.2.Respiratory.Coded.Emergency.Admissions)*1000, 1),
                      HDU_bed_per_1000_admission = round((X1.10a.HDU.Beds/X1.1.Adult.Admissions.20.21)*1000, 1),
                      RSU_bed_per_1000_admission = round((X1.9a.RSU.Beds/X1.1.Adult.Admissions.20.21)*1000, 1)) %>%
  relocate(adm_per_bed, .after = X1.1.Adult.Admissions.20.21) %>%
  relocate(resp_adm_per_bed, .after = X1.2.Respiratory.Coded.Emergency.Admissions) %>%
  relocate(copd_per_1000_admission, .after = X1.3.COPD.Coded.Emergency.Admissions) %>%
  relocate(nat_prop_copd_admission, .after = X1.4b.COPD.Coded.Emergency.Ward.Discharges) %>%
  relocate(adult_asthma_per_1000_admission, .after = X1.5.AA.Coded.Emergency.Admissions) %>%
  relocate(nat_prop_asthma_admission, .after = X1.6.AA.Coded.Emergency.Ward.Discharges) %>%
  relocate(L2_bed_per_1000_resp_admission, .after = X1.8a.Respiratory.Ward.Beds.L2) %>%
  relocate(HDU_bed_per_1000_admission, .after = X1.10a.HDU.Beds) %>%
  relocate(RSU_bed_per_1000_admission, .after = X1.9a.RSU.Beds)


# Create some functions to use with mutate for the WTE stuff


all_adult_WTE <- function(x) {round((x/dat$X1.2.Respiratory.Coded.Emergency.Admissions)*300, 1)}
just_aa_WTE <- function(x) {round((x/dat$X1.5.AA.Coded.Emergency.Admissions)*300, 1)}
just_copd_WTE <- function(x) {round((x/dat$X1.3.COPD.Coded.Emergency.Admissions)*300, 1)}



# Create all the extra columns that need to be created

dat %>% select(contains(".WTE")) %>% colnames()

dat <- dat %>%
  mutate(X2.1a.ST3..WTE_WTE_just_aa = just_aa_WTE(X2.1a.ST3..WTE), 
         .after = X2.1a.ST3..Unfilled.WTE) %>%
  mutate(X2.1a.ST3..WTE_WTE_just_copd = just_copd_WTE(X2.1a.ST3..WTE), 
         .after = X2.1a.ST3..Unfilled.WTE) %>%
  mutate(X2.1a.ST3..WTE_WTE_all_adult = all_adult_WTE(X2.1a.ST3..WTE),
         .after = X2.1a.ST3..Unfilled.WTE)

dat <- dat %>% 
  mutate(X2.1b.Respiratory.Consultants.WTE_WTE_just_aa = just_aa_WTE(X2.1b.Respiratory.Consultants.WTE),
         .after = X2.1b.Respiratory.Consultants.Unfilled.WTE) %>% 
  mutate(X2.1b.Respiratory.Consultants.WTE_WTE_just_copd = just_copd_WTE(X2.1b.Respiratory.Consultants.WTE),
         .after = X2.1b.Respiratory.Consultants.Unfilled.WTE) %>% 
  mutate(X2.1b.Respiratory.Consultants.WTE_WTE_all_adult = all_adult_WTE(X2.1b.Respiratory.Consultants.WTE), 
         .after = X2.1b.Respiratory.Consultants.Unfilled.WTE)

dat <- dat %>% 
  mutate(X2.1c.General.Respiratory.Nurse.Specialists.WTE_WTE_just_aa = just_aa_WTE(X2.1c.General.Respiratory.Nurse.Specialists.WTE),
         .after = X2.1c.General.Respiratory.Nurse.Specialists.Unfilled.WTE) %>% 
  mutate(X2.1c.General.Respiratory.Nurse.Specialists.WTE_WTE_just_copd = just_copd_WTE(X2.1c.General.Respiratory.Nurse.Specialists.WTE),
         .after = X2.1c.General.Respiratory.Nurse.Specialists.Unfilled.WTE) %>%
  mutate(X2.1c.General.Respiratory.Nurse.Specialists.WTE_WTE_all_adult = all_adult_WTE(X2.1c.General.Respiratory.Nurse.Specialists.WTE), 
         .after = X2.1c.General.Respiratory.Nurse.Specialists.Unfilled.WTE)

dat <- dat %>% 
  mutate(X2.1d.COPD.Nurse.WTE_WTE_just_copd = just_copd_WTE(X2.1d.COPD.Nurse.WTE),
         .after = X2.1d.COPD.Nurse.Unfilled.WTE) %>%
  mutate(X2.1d.COPD.Nurse.WTE_WTE_all_adult = all_adult_WTE(X2.1d.COPD.Nurse.WTE), 
         .after = X2.1d.COPD.Nurse.Unfilled.WTE)

dat <- dat %>% 
  mutate(X2.1e.Asthma.Nurse.WTE_WTE_just_aa = just_aa_WTE(X2.1e.Asthma.Nurse.WTE),
         .after = X2.1e.Asthma.Nurse.Unfilled.WTE) %>%
  mutate(X2.1e.Asthma.Nurse.WTE_WTE_all_adult = all_adult_WTE(X2.1e.Asthma.Nurse.WTE), 
         .after = X2.1e.Asthma.Nurse.Unfilled.WTE)

dat <- dat %>%
  mutate(X2.1f.Nurse.Consultant.WTE_WTE_all_adult = all_adult_WTE(X2.1f.Nurse.Consultant.WTE), 
         .after = X2.1f.Nurse.Consultant.Unfilled.WTE)

dat <- dat %>%
  mutate(X2.1g.Advanced.Nurse.Practitioner.WTE_WTE_all_adult = all_adult_WTE(X2.1g.Advanced.Nurse.Practitioner.WTE), 
         .after = X2.1g.Advanced.Nurse.Practitioner.Unfilled.WTE)

dat <- dat %>%
  mutate(X2.1h.Advanced.clinical.practitioner.WTE_WTE_all_adult = all_adult_WTE(X2.1h.Advanced.clinical.practitioner.WTE), 
         .after = X2.1h.Advanced.clinical.practitioner.unfilled.WTE)

dat <- dat %>%
  mutate(X2.1i.DieticianWTE_WTE_all_adult = all_adult_WTE(X2.1i.DieticianWTE), 
         .after = X2.1i.Dietician.unfilled.WTE)

dat <- dat %>%
  mutate(X2.1j.Exercise.physiologist.sports.instructor.WTE_WTE_all_adult = all_adult_WTE(X2.1j.Exercise.physiologist.sports.instructor.WTE), 
         .after = X2.1j.Exercise.physiologist.sports.instructor.unfilled.WTE)

dat <- dat %>%
  mutate(X2.1k.Occupational.therapist.WTE_WTE_all_adult = all_adult_WTE(X2.1k.Occupational.therapist.WTE), 
         .after = X2.1k.Occupational.therapist.unfilled.WTE)

dat <- dat %>%
  mutate(X2.1l.Pharmacist.WTE_WTE_all_adult = all_adult_WTE(X2.1l.Pharmacist.WTE), 
         .after = X2.1l.Pharmacist.unfilled.WTE)

dat <- dat %>%
  mutate(X2.1m.Physiologist.WTE_WTE_all_adult = all_adult_WTE(X2.1m.Physiologist.WTE), 
         .after = X2.1m.Physiologist.unfilled.WTE)

dat <- dat %>%
  mutate(X2.1n.Psychiatrist.WTE_WTE_all_adult = all_adult_WTE(X2.1n.Psychiatrist.WTE), 
         .after = X2.1n.Psychiatrist.unfilled.WTE)

dat <- dat %>%
  mutate(X2.1o.Psychologist.WTE_WTE_all_adult = all_adult_WTE(X2.1o.Psychologist.WTE), 
         .after = X2.1o.Psychologist.unfilled.WTE)

dat <- dat %>%
  mutate(X2.1p.Social.worker.WTE_WTE_all_adult = all_adult_WTE(X2.1p.Social.worker.WTE), 
         .after = X2.1p.Social.worker.unfilled.WTE)

dat <- dat %>%
  mutate(X2.1q.Tobacco.dependence.advisor.WTE_WTE_all_adult = all_adult_WTE(X2.1q.Tobacco.dependence.advisor.WTE), 
         .after = X2.1q.Tobacco.dependence.advisor.unfilled.WTE)




# # # # # #

# the strange RSU question:

dat$X1.9.hospital.have.a.Respiratory.Support.Unit <- "No"
dat$X1.9.hospital.have.a.Respiratory.Support.Unit[!is.na(dat$X1.9a.RSU.Beds)] <- "Yes"

dat$X1.9.hospital.have.a.Respiratory.Support.Unit <- factor(dat$X1.9.hospital.have.a.Respiratory.Support.Unit,
                                                                                        levels = c("Yes", "No"))


# Check the levels of the questions! 
# And just relevel the factors so the order is correct:

dat$X1.10.hospital.have.a.High.Dependency.Unit.s. <- factor(dat$X1.10.hospital.have.a.High.Dependency.Unit.s.,
                                                            levels = c("Yes", "No"))

dat$X3.1.Dedicated.On.Call.rota <- factor(dat$X3.1.Dedicated.On.Call.rota,
                                          levels = c("Yes", "No"))

dat$X3.2.Clinical.Lead.COPD <- factor(dat$X3.2.Clinical.Lead.COPD,
                                      levels = c("Yes", "No"))

dat$X3.3.Clinical.Lead.AA <- factor(dat$X3.3.Clinical.Lead.AA,
                                    levels = c("Yes", "No"))

dat$X4.2a.on.call.consultant.in.person.or.virtually <- factor(dat$X4.2a.on.call.consultant.in.person.or.virtually,
                                                              levels = c("Virtually only", "In person only", "Both"))      

dat$X5.1.EPR.System <- factor(dat$X5.1.EPR.System,
                              levels = c("Yes", "No"))      

dat$X5.2a.EPR.automated.entry.data.into.the.COPD...AA..audit <- factor(dat$X5.2a.EPR.automated.entry.data.into.the.COPD...AA..audit,
                                                                       levels = c("Yes", "No"))

dat$X5.3a.PR.Service.30.Days.COPD <- factor(dat$X5.3a.PR.Service.30.Days.COPD,
                                            levels = c("Yes", "No", "Not known"))

dat$X6.1.Does.your.hospital.have.a.severe.asthma.service <- factor(dat$X6.1.Does.your.hospital.have.a.severe.asthma.service,
                                                                   levels = c("Yes", "No"))

dat$X6.1a.does.your.hospital.have.access.to.severe.asthma.MDT <- factor(dat$X6.1a.does.your.hospital.have.access.to.severe.asthma.MDT,
                                                                        levels = c("Yes", "No"))

dat$X6.1b.do.you.have.a.referral.pathway.to.a.severe.asthma.service <- factor(dat$X6.1b.do.you.have.a.referral.pathway.to.a.severe.asthma.service,
                                                                              levels = c("Yes", "No"))

dat$X6.2.hospital.provide.access.to.a.Asthma.biologics.drugs <- factor(dat$X6.2.hospital.provide.access.to.a.Asthma.biologics.drugs,
                                                          levels = c("Yes", "No"))

dat$X6.4.COPD.MDT.Meetings <- factor(dat$X6.4.COPD.MDT.Meetings,
                                     levels = c("Yes for COPD only", "Yes for asthma only", "Yes for both", "No"))

dat$X6.4a.MDT.Meeting.Frequency <- factor(dat$X6.4a.MDT.Meeting.Frequency,
                                          levels = c("Weekly", "Fortnightly", "Monthly", "Quarterly", "Other"))

dat$X6.5.Development.of.Integrated.Respiratory.Services <- factor(dat$X6.5.Development.of.Integrated.Respiratory.Services,
                                                                  levels = c("Yes", "No"))

dat$X7.1.Patient.Carer.Surveys <- factor(dat$X7.1.Patient.Carer.Surveys,
                                         levels = c("Continuous (every patient)", "More than 4 times a year", "3-4 times a year", "1-2 times a year", "Less than once a year", "Never"))

dat$X7.2.Strategic.Group.Respiratory.Service <- factor(dat$X7.2.Strategic.Group.Respiratory.Service,
                                                       levels = c("Yes", "No", "Not known"))

dat$X7.2a.Patient.Representation <- factor(dat$X7.2a.Patient.Representation,
                                              levels = c("Yes", "No", "Not known"))

dat$X7.3.trust.link.with.your.integrated.care.system <- factor(dat$X7.3.trust.link.with.your.integrated.care.system,
                                                               levels = c("Yes", "No", "Not known"))



# # # # # # #

# # # # # # # 


# It's at this point, because I didn't take out the columns with 3 dots... I need to do it here. By creating this vector for use at the end.
# Have to do it here because they are in numeric format here

inspectvect <- dat %>% select(where(is.numeric)) %>% select(contains("...")) %>% colnames()


dat_hosp_level <- dat
colnames(dat_hosp_level)

# Split columns with multiple answers by ";"
# Define the variables to be split
col_to_separate <- c("X3.4.On.Site.Palliative.Care", "X3.5..tobacco.dependence.service",
                     "X3.6..tobacco.dependence.service.referred.on.discharge", "X4.1.Speciality.Triage",
                     "X4.2.On.Call.Consultant.Days", "X4.3a.ST3..MAU.Ward.Round.Days", "X4.3b.ST3..Resp.Ward.Days",
                     "X4.3c.ST3..Resp.Ward.Days.Other", "X4.4a.Respiratory.Nurse.Reviews.COPD",
                     "X4.4b.Respiratory.Nurse.Reviews.AA", "X4.5a.Physio.Review.COPD", "X4.5b.Physio.Review.AA",
                     "X5.3.pulmonary.rehabilitation.service.available.to.COPD.patients", "X6.2a.biotypes.all.that.apply",
                     "X6.3a.Service.Out.Reach", "X6.3b.Service.In.Reach", "X6.3c.Admissions.Avoidance",
                     "X6.3d.Oxygen.Assessment", "X6.3e.Medicine.Management", "X6.3f.Chronic.Disease.Management",
                     "X6.3g.Nebuliser.Service", "X6.3h.tobacco.Cessation.Advice", "X6.3i.Virtual.Wards",
                     "X6.4b.MDT.Physicians", "X6.4b.MDT.Ward.Nurses", "X6.4b.MDT.Physiotherapists", "X6.3b.MDT.Other",
                     "X8.1.Transition.Paediatric.To.Adult")

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
  relocate(`X3.4.On.Site.Palliative.Care_COPD patients`:`X4.2.On.Call.Consultant.Days_Out of hours`, .after = X3.3.Clinical.Lead.AA) %>%
  relocate(X4.3a.ST3..MAU.Ward.Round.Days_Weekdays:`X4.5b.Physio.Review.AA_No respiratory physiotherapist available`, .after = X4.2a.on.call.consultant.in.person.or.virtually) %>%
  relocate(`X5.3.pulmonary.rehabilitation.service.available.to.COPD.patients_Yes - based within the community`:X5.3.pulmonary.rehabilitation.service.available.to.COPD.patients_No, .after = X5.2a.EPR.automated.entry.data.into.the.COPD...AA..audit) %>%
  relocate(`X6.2a.biotypes.all.that.apply_Prescribe Asthma Biologics`:`X6.3i.Virtual.Wards_Service not provided`, .after = X6.2.hospital.provide.access.to.a.Asthma.biologics.drugs) %>%
  relocate(`X6.4b.MDT.Physicians_Respiratory consultant`:`X6.3b.MDT.Other_Social worker`, .after = X6.4a.Other) %>%
  relocate(`X8.1.Transition.Paediatric.To.Adult_Their GP is sent the same record`:`X8.1.Transition.Paediatric.To.Adult_The young person is given the opportunity to be seen without their parents/carers`, .after = X7.3.trust.link.with.your.integrated.care.system) %>%
  relocate(X1.10.hospital.have.a.High.Dependency.Unit.s.:HDU_bed_per_1000_admission, .after = L2_bed_per_1000_resp_admission)


dat_hosp_level <- dat_hosp_level %>% arrange(region, integrated_care_system, trust_code, trust_name, hosp_code, hosp_name) %>% #select(-country) %>%
  relocate(region, integrated_care_system, trust_code, trust_name, hosp_code, hosp_name)

#dat_hosp_level %>% select(where(is.numeric)) %>% select(contains("...")) %>% colnames()

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

# 
# for (i in 1:ncol(dat_hosp_level)) {
#   if (grepl("\\.\\.\\.", colnames(dat_hosp_level))[i] == TRUE & 
#       (is.numeric(dat_hosp_level[ , i]) == TRUE | is.logical(dat_hosp_level[ , i]) == TRUE)) {
# 
#     dat_hosp_level[[i]] <- as.character(dat_hosp_level[[i]])
#     dat_hosp_level[[i]] <- recode(dat_hosp_level[[i]], "1" = "Yes", .missing = "No")
#   
#     }
# }


table(dat_hosp_level$X6.4.COPD.MDT.Meetings)
table(dat_hosp_level$`X6.4b.MDT.Physicians_Respiratory consultant`, useNA = "ifany")


dat_hosp_level <- dat_hosp_level %>% mutate(across(c(starts_with("X6.4b"), starts_with("X6.3b")), 
                                            ~ifelse(X6.4.COPD.MDT.Meetings == "No", NA, .)))

dat_hosp_level_save <- dat_hosp_level

# Need to do some rounding.(for hospital level only)

dat_hosp_level$adm_per_bed <- round(dat_hosp_level$adm_per_bed, 1)
dat_hosp_level$adm_per_bed <- sprintf("%.1f", dat_hosp_level$adm_per_bed)

dat_hosp_level$resp_adm_per_bed <- round(dat_hosp_level$resp_adm_per_bed, 1)
dat_hosp_level$resp_adm_per_bed <- sprintf("%.1f", dat_hosp_level$resp_adm_per_bed)

dat_hosp_level$copd_per_1000_admission <- round(dat_hosp_level$copd_per_1000_admission, 1)
dat_hosp_level$copd_per_1000_admission <- sprintf("%.1f", dat_hosp_level$copd_per_1000_admission)

dat_hosp_level$nat_prop_copd_admission <- round(dat_hosp_level$nat_prop_copd_admission, 1)
dat_hosp_level$nat_prop_copd_admission <- sprintf("%.1f", dat_hosp_level$nat_prop_copd_admission)

dat_hosp_level$adult_asthma_per_1000_admission <- round(dat_hosp_level$adult_asthma_per_1000_admission, 1)
dat_hosp_level$adult_asthma_per_1000_admission <- sprintf("%.1f", dat_hosp_level$adult_asthma_per_1000_admission)

dat_hosp_level$nat_prop_asthma_admission <- round(dat_hosp_level$nat_prop_asthma_admission, 1)
dat_hosp_level$nat_prop_asthma_admission <- sprintf("%.1f", dat_hosp_level$nat_prop_asthma_admission)

dat_hosp_level$L2_bed_per_1000_resp_admission <- round(dat_hosp_level$L2_bed_per_1000_resp_admission, 1)
dat_hosp_level$L2_bed_per_1000_resp_admission <- sprintf("%.1f", dat_hosp_level$L2_bed_per_1000_resp_admission)

dat_hosp_level$HDU_bed_per_1000_admission <- round(dat_hosp_level$HDU_bed_per_1000_admission, 1)
dat_hosp_level$HDU_bed_per_1000_admission <- sprintf("%.1f", dat_hosp_level$HDU_bed_per_1000_admission)

dat_hosp_level$RSU_bed_per_1000_admission <- round(dat_hosp_level$RSU_bed_per_1000_admission, 1)
dat_hosp_level$RSU_bed_per_1000_admission <- sprintf("%.1f", dat_hosp_level$RSU_bed_per_1000_admission)

dat_hosp_level <- dat_hosp_level %>% mutate(across(starts_with("X2.1"), ~sprintf("%.1f", round(., 1))))

dat_hosp_level_output <- dat_hosp_level %>% select(-country)

write.csv(dat_hosp_level_output, "C:/Alex Harley/Audit_2023_onwards/2022-2023/AA/Analysis/Output/Hospital_level_data_COPD_AA_org_2022-23_per300.csv",
         row.names = FALSE, na = "")


# # # # # # that is it for the hospital-level data!!! # # # # # # # # #




dat_hosp_level <- dat_hosp_level_save


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

dat_hosp_level







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

# first.1 <- dat %>% select(X1.1.Adult.Admissions.20.21:X1.8a.Respiratory.Ward.Beds.L2) %>% colnames()
#                             
#                             # X1.5.Paediatric.medical.beds) %>% colnames()
# 
# 
# for (i in first.1) {
#   all <- cbind(all, sum_stats(dat, i, roundno = 0))
# }

all <- cbind(all, sum_stats_total(dat_hosp_level, "X1.1.Adult.Admissions.20.21", roundno = 0))

all <- cbind(all, sum_stats(dat_hosp_level, "adm_per_bed", roundno = 1))

all <- cbind(all, sum_stats_total(dat_hosp_level, "X1.2.Respiratory.Coded.Emergency.Admissions", roundno = 0))

all <- cbind(all, sum_stats(dat_hosp_level, "resp_adm_per_bed", roundno = 1))

all <- cbind(all, sum_stats_total(dat_hosp_level, "X1.3.COPD.Coded.Emergency.Admissions", roundno = 0))

all <- cbind(all, sum_stats(dat_hosp_level, "copd_per_1000_admission", roundno = 1))

all <- cbind(all, sum_stats_total(dat_hosp_level, "X1.4b.COPD.Coded.Emergency.Ward.Discharges", roundno = 0))

all <- cbind(all, sum_stats(dat_hosp_level, "nat_prop_copd_admission", roundno = 1))

all <- cbind(all, sum_stats_total(dat_hosp_level, "X1.5.AA.Coded.Emergency.Admissions", roundno = 0))

all <- cbind(all, sum_stats(dat_hosp_level, "adult_asthma_per_1000_admission", roundno = 1))

all <- cbind(all, sum_stats_total(dat_hosp_level, "X1.6.AA.Coded.Emergency.Ward.Discharges", roundno = 0))

all <- cbind(all, sum_stats(dat_hosp_level, "nat_prop_asthma_admission", roundno = 1))

all <- cbind(all, sum_stats_total(dat_hosp_level, "X1.7.Medical.Beds", roundno = 0))

all <- cbind(all, sum_stats_total(dat_hosp_level, "X1.8.Respiratory.Ward.Beds", roundno = 0))

all <- cbind(all, sum_stats_total(dat_hosp_level, "X1.8a.Respiratory.Ward.Beds.L2", roundno = 0))

all <- cbind(all, sum_stats(dat_hosp_level, "L2_bed_per_1000_resp_admission", roundno = 1))

all <- cbind(all, freq_stats(dat_hosp_level, "X1.10.hospital.have.a.High.Dependency.Unit.s.", remove_nos = TRUE))

all <- cbind(all, sum_stats_total(dat_hosp_level, "X1.10a.HDU.Beds", roundno = 0))

all <- cbind(all, sum_stats(dat_hosp_level, "HDU_bed_per_1000_admission", roundno = 0))



# then we carry on

dat_hosp_level <- dat_hosp_level %>%
  mutate(`X3.4.On.Site.Palliative.Care_No on-site palliative care service` = as.factor(`X3.4.On.Site.Palliative.Care_No on-site palliative care service`),
         `X3.5..tobacco.dependence.service_No tobacco dependence service available` = as.factor(`X3.5..tobacco.dependence.service_No tobacco dependence service available`),
         `X3.6..tobacco.dependence.service.referred.on.discharge_No tobacco dependence service available` = as.factor(`X3.6..tobacco.dependence.service.referred.on.discharge_No tobacco dependence service available`),
         X4.1.Speciality.Triage_Weekends = as.factor(X4.1.Speciality.Triage_Weekends),
         `X4.1.Speciality.Triage_Out of hours` = as.factor(`X4.1.Speciality.Triage_Out of hours`),
         X4.2.On.Call.Consultant.Days_Weekends = as.factor(X4.2.On.Call.Consultant.Days_Weekends),
         `X4.2.On.Call.Consultant.Days_Out of hours` = as.factor(`X4.2.On.Call.Consultant.Days_Out of hours`),
         X4.3a.ST3..MAU.Ward.Round.Days_Weekends = as.factor(X4.3a.ST3..MAU.Ward.Round.Days_Weekends),
         `X4.3a.ST3..MAU.Ward.Round.Days_Out of hours` = as.factor(`X4.3a.ST3..MAU.Ward.Round.Days_Out of hours`),
         X4.3b.ST3..Resp.Ward.Days_Weekends = as.factor(X4.3b.ST3..Resp.Ward.Days_Weekends),
         `X4.3b.ST3..Resp.Ward.Days_Out of hours` = as.factor(`X4.3b.ST3..Resp.Ward.Days_Out of hours`),
         X4.3b.ST3..Resp.Ward.Days_None = as.factor(X4.3b.ST3..Resp.Ward.Days_None),
         X4.3c.ST3..Resp.Ward.Days.Other_Weekends = as.factor(X4.3c.ST3..Resp.Ward.Days.Other_Weekends),
         `X4.3c.ST3..Resp.Ward.Days.Other_Out of hours` = as.factor(`X4.3c.ST3..Resp.Ward.Days.Other_Out of hours`),
         X4.4a.Respiratory.Nurse.Reviews.COPD_Weekends = as.factor(X4.4a.Respiratory.Nurse.Reviews.COPD_Weekends),
         `X4.4a.Respiratory.Nurse.Reviews.COPD_Out of hours` = as.factor(`X4.4a.Respiratory.Nurse.Reviews.COPD_Out of hours`),
         `X4.4a.Respiratory.Nurse.Reviews.COPD_No respiratory nurse available` = as.factor(`X4.4a.Respiratory.Nurse.Reviews.COPD_No respiratory nurse available`),
         X4.4b.Respiratory.Nurse.Reviews.AA_Weekends = as.factor(X4.4b.Respiratory.Nurse.Reviews.AA_Weekends),
         `X4.4b.Respiratory.Nurse.Reviews.AA_Out of hours` = as.factor(`X4.4b.Respiratory.Nurse.Reviews.AA_Out of hours`),
         `X4.4b.Respiratory.Nurse.Reviews.AA_No respiratory nurse available` = as.factor(`X4.4b.Respiratory.Nurse.Reviews.AA_No respiratory nurse available`),
         `X4.5a.Physio.Review.COPD_No respiratory physiotherapist available` = as.factor(`X4.5a.Physio.Review.COPD_No respiratory physiotherapist available`),
         `X5.3.pulmonary.rehabilitation.service.available.to.COPD.patients_Yes - based at this hospital` = as.factor(`X5.3.pulmonary.rehabilitation.service.available.to.COPD.patients_Yes - based at this hospital`),
         X5.3.pulmonary.rehabilitation.service.available.to.COPD.patients_No = as.factor(X5.3.pulmonary.rehabilitation.service.available.to.COPD.patients_No),
         `X6.3b.Service.In.Reach_Community team that works jointly with a hospital team` = as.factor(`X6.3b.Service.In.Reach_Community team that works jointly with a hospital team`),
         `X6.3d.Oxygen.Assessment_Service not provided` = as.factor(`X6.3d.Oxygen.Assessment_Service not provided`),
         `X6.3e.Medicine.Management_Single team that works across the community/secondary care interface` = as.factor(`X6.3e.Medicine.Management_Single team that works across the community/secondary care interface`),
         `X6.3g.Nebuliser.Service_Single team that works across the community/secondary care interface` = as.factor(`X6.3g.Nebuliser.Service_Single team that works across the community/secondary care interface`),
         `X6.3h.tobacco.Cessation.Advice_Service not provided` = as.factor(`X6.3h.tobacco.Cessation.Advice_Service not provided`),
         `X6.3i.Virtual.Wards_Hospital based team` = as.factor(`X6.3i.Virtual.Wards_Hospital based team`),
         `X6.4b.MDT.Ward.Nurses_Ward nurse` = as.factor(`X6.4b.MDT.Ward.Nurses_Ward nurse`),
         `X6.4b.MDT.Physiotherapists_Ward physiotherapist` = as.factor(`X6.4b.MDT.Physiotherapists_Ward physiotherapist`),
         `X6.3b.MDT.Other_Community occupational therapist` = as.factor(`X6.3b.MDT.Other_Community occupational therapist`),
         `X6.3b.MDT.Other_Palliative care specialist` = as.factor(`X6.3b.MDT.Other_Palliative care specialist`),
         X6.3b.MDT.Other_Psychologist = as.factor(X6.3b.MDT.Other_Psychologist),
         `X6.3b.MDT.Other_Social worker` = as.factor(`X6.3b.MDT.Other_Social worker`),
         `X6.3b.MDT.Other_Tobacco dependence counsellor` = as.factor(`X6.3b.MDT.Other_Tobacco dependence counsellor`),
         `X6.3b.MDT.Other_Ward pharmacist` = as.factor(`X6.3b.MDT.Other_Ward pharmacist`),
         `X6.3b.MDT.Other_Ward occupational therapist` = as.factor(`X6.3b.MDT.Other_Ward occupational therapist`),
         X6.3b.MDT.Other_Other = as.factor(X6.3b.MDT.Other_Other),
         `X8.1.Transition.Paediatric.To.Adult_The young person has a full record of their condition` = as.factor(`X8.1.Transition.Paediatric.To.Adult_The young person has a full record of their condition`),
         `X8.1.Transition.Paediatric.To.Adult_Their GP is sent the same record` = as.factor(`X8.1.Transition.Paediatric.To.Adult_Their GP is sent the same record`),
         `X8.1.Transition.Paediatric.To.Adult_The young person is given the opportunity to be seen without their parents/carers` = as.factor(`X8.1.Transition.Paediatric.To.Adult_The young person is given the opportunity to be seen without their parents/carers`),
         `X8.1.Transition.Paediatric.To.Adult_The young person has a named case worker to assist in signposting for them and their family` = as.factor(`X8.1.Transition.Paediatric.To.Adult_The young person has a named case worker to assist in signposting for them and their family`))


all <- cbind(all, freq_stats(dat_hosp_level, "X1.9.hospital.have.a.Respiratory.Support.Unit", remove_nos = TRUE))

# all <- cbind(all, freq_stats(dat_hosp_level, "X1.9.hospital.have.a.Respiratory.Support.Unit_Community based team", remove_nos = TRUE))
# all <- cbind(all, freq_stats(dat_hosp_level, "X1.9.hospital.have.a.Respiratory.Support.Unit_Single team that works across the community/secondary care interface", remove_nos = TRUE))
# all <- cbind(all, freq_stats(dat_hosp_level, "X1.9.hospital.have.a.Respiratory.Support.Unit_Service not provided", remove_nos = TRUE))


# carry on...

all <- cbind(all, sum_stats_total(dat_hosp_level, "X1.9a.RSU.Beds", roundno = 0))

all <- cbind(all, sum_stats_total(dat_hosp_level, "RSU_bed_per_1000_admission", roundno = 1))

all <- cbind(all, sum_stats_total(dat_hosp_level, "X1.11.How.many.virtual.ward.beds", roundno = 0))

all <- cbind(all, sum_stats_total(dat_hosp_level, "X1.11a.irtual.ward.beds.are.dedicated.to.asthma.or.COPD", roundno = 0))



head(dat_hosp_level$X2.1a.ST3..WTE)

second <- dat_hosp_level %>% select(X2.1a.ST3..WTE:X2.1q.Tobacco.dependence.advisor.WTE_WTE_all_adult) %>% colnames()


# Have to do this because slightly different things are wanted for each variable (some want totals, etc)

# for (i in 1:5) {
#   all <- cbind(all, sum_stats(dat, second[(i*4) - 3], mean = FALSE, sd = FALSE, total = TRUE, roundno = 1))
#   all <- cbind(all, sum_stats(dat, second[(i*4) - 2], mean = FALSE, sd = FALSE, total = TRUE, roundno = 1))
#   all <- cbind(all, sum_stats(dat, second[(i*4) - 1], mean = FALSE, sd = FALSE, total = FALSE, roundno = 1))
#   all <- cbind(all, sum_stats(dat, second[(i*4) - 0], mean = FALSE, sd = FALSE, total = FALSE, roundno = 1))
#   }

for (i in second) {
  all <- cbind(all, sum_stats_total(dat_hosp_level, i, mean = FALSE, sd = FALSE, total = TRUE, roundno = 1))
}

colnames(all)

all <- all %>% select(-Var1) %>% select(-ends_with("all_adult_Total"), -ends_with("just_asthma_Total"),
                      -ends_with("just_aa_Total"), -ends_with("just_copd_Total"))
                                                                


# Continue (x3.1 to x3.3)
all <- cbind(all, freq_stats(dat_hosp_level, "X3.1.Dedicated.On.Call.rota", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X3.2.Clinical.Lead.COPD", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X3.3.Clinical.Lead.AA", remove_nos = TRUE))

# third_1 <- dat %>% select(X3.1.Dedicated.On.Call.rota:X3.3.Clinical.Lead.AA) %>% colnames()

# for (i in third_1) {
#   all <- cbind(all, freq_stats(dat, i, remove_nos = TRUE))
# }

all <- cbind(all, freq_stats(dat_hosp_level, "X3.4.On.Site.Palliative.Care_COPD patients", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X3.4.On.Site.Palliative.Care_Asthma patients", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X3.4.On.Site.Palliative.Care_Neither COPD nor asthma patients", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X3.4.On.Site.Palliative.Care_No on-site palliative care service", remove_nos = TRUE))

all <- cbind(all, freq_stats(dat_hosp_level, "X3.5..tobacco.dependence.service_COPD patients", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X3.5..tobacco.dependence.service_Asthma patients", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X3.5..tobacco.dependence.service_No tobacco dependence service available", remove_nos = TRUE))

all <- cbind(all, freq_stats(dat_hosp_level, "X3.6..tobacco.dependence.service.referred.on.discharge_COPD patients", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X3.6..tobacco.dependence.service.referred.on.discharge_Asthma patients", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X3.6..tobacco.dependence.service.referred.on.discharge_No tobacco dependence service available", remove_nos = TRUE))

all <- cbind(all, freq_stats(dat_hosp_level, "X4.1.Speciality.Triage_Weekdays", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X4.1.Speciality.Triage_Weekends", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X4.1.Speciality.Triage_Out of hours", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X4.1.Speciality.Triage_No speciality triage of patients to respiratory medicine", remove_nos = TRUE))

all <- cbind(all, freq_stats(dat_hosp_level, "X4.2.On.Call.Consultant.Days_Weekdays", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X4.2.On.Call.Consultant.Days_Weekends", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X4.2.On.Call.Consultant.Days_Out of hours", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X4.2.On.Call.Consultant.Days_No on-call respiratory consultant available", remove_nos = TRUE))


all <- cbind(all, freq_stats(dat_hosp_level, "X4.2a.on.call.consultant.in.person.or.virtually", remove_nos = TRUE))


all <- cbind(all, freq_stats(dat_hosp_level, "X4.3a.ST3..MAU.Ward.Round.Days_Weekdays", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X4.3a.ST3..MAU.Ward.Round.Days_Weekends", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X4.3a.ST3..MAU.Ward.Round.Days_Out of hours", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X4.3a.ST3..MAU.Ward.Round.Days_None", remove_nos = TRUE))

all <- cbind(all, freq_stats(dat_hosp_level, "X4.3b.ST3..Resp.Ward.Days_Weekdays", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X4.3b.ST3..Resp.Ward.Days_Weekends", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X4.3b.ST3..Resp.Ward.Days_Out of hours", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X4.3b.ST3..Resp.Ward.Days_None", remove_nos = TRUE))

all <- cbind(all, freq_stats(dat_hosp_level, "X4.3c.ST3..Resp.Ward.Days.Other_Weekdays", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X4.3c.ST3..Resp.Ward.Days.Other_Weekends", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X4.3c.ST3..Resp.Ward.Days.Other_Out of hours", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X4.3c.ST3..Resp.Ward.Days.Other_None", remove_nos = TRUE))

all <- cbind(all, freq_stats(dat_hosp_level, "X4.4a.Respiratory.Nurse.Reviews.COPD_Weekdays", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X4.4a.Respiratory.Nurse.Reviews.COPD_Weekends", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X4.4a.Respiratory.Nurse.Reviews.COPD_Out of hours", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X4.4a.Respiratory.Nurse.Reviews.COPD_No respiratory nurse available", remove_nos = TRUE))

all <- cbind(all, freq_stats(dat_hosp_level, "X4.4b.Respiratory.Nurse.Reviews.AA_Weekdays", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X4.4b.Respiratory.Nurse.Reviews.AA_Weekends", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X4.4b.Respiratory.Nurse.Reviews.AA_Out of hours", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X4.4b.Respiratory.Nurse.Reviews.AA_No respiratory nurse available", remove_nos = TRUE))

all <- cbind(all, freq_stats(dat_hosp_level, "X4.5a.Physio.Review.COPD_Weekdays", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X4.5a.Physio.Review.COPD_Weekends", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X4.5a.Physio.Review.COPD_Out of hours", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X4.5a.Physio.Review.COPD_No respiratory physiotherapist available", remove_nos = TRUE))

all <- cbind(all, freq_stats(dat_hosp_level, "X4.5b.Physio.Review.AA_Weekdays", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X4.5b.Physio.Review.AA_Weekends", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X4.5b.Physio.Review.AA_Out of hours", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X4.5b.Physio.Review.AA_No respiratory physiotherapist available", remove_nos = TRUE))


all <- cbind(all, freq_stats(dat_hosp_level, "X5.1.EPR.System", remove_nos = TRUE))

all <- cbind(all, freq_stats(dat_hosp_level, "X5.2a.EPR.automated.entry.data.into.the.COPD...AA..audit", remove_nos = TRUE))


all <- cbind(all, freq_stats(dat_hosp_level, "X5.3.pulmonary.rehabilitation.service.available.to.COPD.patients_Yes - based at this hospital", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X5.3.pulmonary.rehabilitation.service.available.to.COPD.patients_Yes - based within another hospital", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X5.3.pulmonary.rehabilitation.service.available.to.COPD.patients_Yes - based within the community", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X5.3.pulmonary.rehabilitation.service.available.to.COPD.patients_No", remove_nos = TRUE))


all <- cbind(all, freq_stats(dat_hosp_level, "X5.3a.PR.Service.30.Days.COPD", remove_nos = TRUE))



# Continue (X6.1 to X6.2)
sixth <- dat_hosp_level %>% select(X6.1.Does.your.hospital.have.a.severe.asthma.service:X6.2.hospital.provide.access.to.a.Asthma.biologics.drugs) %>% colnames()

for (i in sixth) {
  all <- cbind(all, freq_stats(dat_hosp_level, i, remove_nos = TRUE))
}


all <- cbind(all, freq_stats(dat_hosp_level, "X6.2a.biotypes.all.that.apply_Prescribe Asthma Biologics", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X6.2a.biotypes.all.that.apply_Administer Asthma Biologics", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X6.2a.biotypes.all.that.apply_Monitor patients on Asthma Biologics", remove_nos = TRUE))

all <- cbind(all, freq_stats(dat_hosp_level, "X6.3a.Service.Out.Reach_Service not provided", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X6.3a.Service.Out.Reach_Hospital based team", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X6.3a.Service.Out.Reach_Hospital team that works jointly with a community team", remove_nos = TRUE))

all <- cbind(all, freq_stats(dat_hosp_level, "X6.3b.Service.In.Reach_Service not provided", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X6.3b.Service.In.Reach_Community based team", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X6.3b.Service.In.Reach_Community team that works jointly with a hospital team", remove_nos = TRUE))

all <- cbind(all, freq_stats(dat_hosp_level, "X6.3c.Admissions.Avoidance_Service not provided", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X6.3c.Admissions.Avoidance_Hospital based team", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X6.3c.Admissions.Avoidance_Community based team", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X6.3c.Admissions.Avoidance_Single team that works across the community/secondary care interface", remove_nos = TRUE))

all <- cbind(all, freq_stats(dat_hosp_level, "X6.3d.Oxygen.Assessment_Service not provided", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X6.3d.Oxygen.Assessment_Hospital based team", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X6.3d.Oxygen.Assessment_Community based team", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X6.3d.Oxygen.Assessment_Single team that works across the community/secondary care interface", remove_nos = TRUE))

all <- cbind(all, freq_stats(dat_hosp_level, "X6.3e.Medicine.Management_Service not provided", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X6.3e.Medicine.Management_Hospital based team", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X6.3e.Medicine.Management_Community based team", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X6.3e.Medicine.Management_Single team that works across the community/secondary care interface", remove_nos = TRUE))

all <- cbind(all, freq_stats(dat_hosp_level, "X6.3f.Chronic.Disease.Management_Service not provided", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X6.3f.Chronic.Disease.Management_Hospital based team", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X6.3f.Chronic.Disease.Management_Community based team", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X6.3f.Chronic.Disease.Management_Single team that works across the community/secondary care interface", remove_nos = TRUE))

all <- cbind(all, freq_stats(dat_hosp_level, "X6.3g.Nebuliser.Service_Service not provided", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X6.3g.Nebuliser.Service_Hospital based team", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X6.3g.Nebuliser.Service_Community based team", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X6.3g.Nebuliser.Service_Single team that works across the community/secondary care interface", remove_nos = TRUE))

all <- cbind(all, freq_stats(dat_hosp_level, "X6.3h.tobacco.Cessation.Advice_Service not provided", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X6.3h.tobacco.Cessation.Advice_Hospital based team", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X6.3h.tobacco.Cessation.Advice_Community based team", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X6.3h.tobacco.Cessation.Advice_Single team that works across the community/secondary care interface", remove_nos = TRUE))

all <- cbind(all, freq_stats(dat_hosp_level, "X6.3i.Virtual.Wards_Service not provided", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X6.3i.Virtual.Wards_Hospital based team", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X6.3i.Virtual.Wards_Community based team", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X6.3i.Virtual.Wards_Single team that works across the community/secondary care interface", remove_nos = TRUE))

all <- cbind(all, freq_stats(dat_hosp_level, "X6.4.COPD.MDT.Meetings", remove_nos = TRUE))

all <- cbind(all, freq_stats(dat_hosp_level, "X6.4a.MDT.Meeting.Frequency", remove_nos = TRUE))

all <- cbind(all, freq_stats(dat_hosp_level, "X6.4b.MDT.Physicians_Respiratory consultant", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X6.4b.MDT.Physicians_Other member of respiratory specialist team", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X6.4b.MDT.Physicians_General practitioner", remove_nos = TRUE))

all <- cbind(all, freq_stats(dat_hosp_level, "X6.4b.MDT.Ward.Nurses_Ward nurse", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X6.4b.MDT.Ward.Nurses_Community nurse", remove_nos = TRUE))

all <- cbind(all, freq_stats(dat_hosp_level, "X6.4b.MDT.Physiotherapists_Ward physiotherapist", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X6.4b.MDT.Physiotherapists_Community physiotherapist", remove_nos = TRUE))

all <- cbind(all, freq_stats(dat_hosp_level, "X6.3b.MDT.Other_Community pharmacist", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X6.3b.MDT.Other_Community occupational therapist", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X6.3b.MDT.Other_Palliative care specialist", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X6.3b.MDT.Other_Psychologist", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X6.3b.MDT.Other_Social worker", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X6.3b.MDT.Other_Tobacco dependence counsellor", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X6.3b.MDT.Other_Ward pharmacist", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X6.3b.MDT.Other_Ward occupational therapist", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X6.3b.MDT.Other_Other", remove_nos = TRUE))


# carry on...

seventh <- dat_hosp_level %>% select(X6.5.Development.of.Integrated.Respiratory.Services:X7.3.trust.link.with.your.integrated.care.system) %>% colnames()

for (i in seventh) {
  all <- cbind(all, freq_stats(dat_hosp_level, i))
}


all <- cbind(all, freq_stats(dat_hosp_level, "X8.1.Transition.Paediatric.To.Adult_The young person has a full record of their condition", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X8.1.Transition.Paediatric.To.Adult_Their GP is sent the same record", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X8.1.Transition.Paediatric.To.Adult_The young person is given the opportunity to be seen without their parents/carers", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X8.1.Transition.Paediatric.To.Adult_The young person has a transition plan that has been agreed with both paediatric and adult clinicians", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X8.1.Transition.Paediatric.To.Adult_The young person has a named case worker to assist in signposting for them and their family", remove_nos = TRUE))
all <- cbind(all, freq_stats(dat_hosp_level, "X8.1.Transition.Paediatric.To.Adult_We do not have any formal transition arrangements", remove_nos = TRUE))


# Special management of resp_adm_per_bed_Mean & resp_adm_per_bed_SD
overall_mean <- dat_hosp_level %>%
  summarise(overall_mean = mean(resp_adm_per_bed[is.finite(resp_adm_per_bed)], na.rm = TRUE))
mean_by_country <- dat_hosp_level %>%
  group_by(country) %>%
  summarise(mean_value = mean(resp_adm_per_bed[is.finite(resp_adm_per_bed)], na.rm = TRUE))
result <- bind_rows(
  overall_mean,
  mean_by_country %>% filter(country == "England"),
  mean_by_country %>% filter(country == "Wales")
)

overall_sd <- sd(dat_hosp_level$resp_adm_per_bed[is.finite(dat_hosp_level$resp_adm_per_bed)], na.rm = TRUE)
eng_sd <- sd(dat_hosp_level$resp_adm_per_bed[dat_hosp_level$country == "England" & is.finite(dat_hosp_level$resp_adm_per_bed)], na.rm = TRUE)
wal_sd <- sd(dat_hosp_level$resp_adm_per_bed[dat_hosp_level$country == "Wales" & is.finite(dat_hosp_level$resp_adm_per_bed)], na.rm = TRUE)
sd_summary <- data.frame(
  Country = c("National (All)", "England", "Wales"),
  resp_adm_per_bed_SD_new = c(overall_sd, eng_sd, wal_sd)
)

to_be_added <- data.frame(
  Country = c("National (All)", "England", "Wales"),
  resp_adm_per_bed_Mean_new = c(124.6, 127.2, 63.0),
  resp_adm_per_bed_SD_new = c(87.5, 88.3, 20.6)
)




# # # # The end!!!!!



all %>% select(contains(inspectvect)) %>% colnames() # & ends_with(c("No_Total_N", "No_)) 

# Use the vector we created earlier of the columns we have to drop

ncol(all)   # 938

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

ncol(all)   # 785
colnames(all)
# all <- left_join(all, to_be_added, by = "Country")
# all <- all %>%
#   relocate(resp_adm_per_bed_Mean_new, .after = X1.2.Respiratory.Coded.Emergency.Admissions_High_quart) %>%
#   relocate(resp_adm_per_bed_SD_new, .after = resp_adm_per_bed_Mean_new) %>%
#   select(-resp_adm_per_bed_Mean, -resp_adm_per_bed_SD) %>%
#   rename(resp_adm_per_bed_Mean = resp_adm_per_bed_Mean_new,
#          resp_adm_per_bed_SD = resp_adm_per_bed_SD_new)

all$resp_adm_per_bed_Mean <- to_be_added$resp_adm_per_bed_Mean_new
all$resp_adm_per_bed_SD <- to_be_added$resp_adm_per_bed_SD_new

#all <- all %>% select(!(contains(inspectvect) & ends_with(c("No_n", "No_perc")))) # %>% colnames()


write.csv(all, "C:/Alex Harley/Audit_2023_onwards/2022-2023/AA/Analysis/Output/National_level_data_COPD_AA_org_2022-23_Alex_edits_per300.csv", row.names = FALSE)



# # # # # # # Benchmarking

# Make the benchmarking variables
col_to_separate <- c("X4.2.On.Call.Consultant.Days",
                     "X8.1.Transition.Paediatric.To.Adult")

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

colnames(dat_benchmark)

table(dat_benchmark$X4.3a.ST3..MAU.Ward.Round.Days)
table(dat_benchmark$X4.3b.ST3..Resp.Ward.Days)

# KPI_1_7days_resp_specialist_advice
dat_benchmark$KPI_1_7days_resp_specialist_advice <- "No"
dat_benchmark$KPI_1_7days_resp_specialist_advice[dat_benchmark$X4.3a.ST3..MAU.Ward.Round.Days %in% 
                                                   c("Weekdays;Weekends","Weekdays;Weekends;Out of hours")] <- "Yes"
dat_benchmark$KPI_1_7days_resp_specialist_advice[dat_benchmark$X4.3b.ST3..Resp.Ward.Days %in% 
                                                   c("Weekdays;Weekends","Weekdays;Weekends;Out of hours")] <- "Yes"
dat_benchmark$KPI_1_7days_resp_specialist_advice <- factor(dat_benchmark$KPI_1_7days_resp_specialist_advice, 
                                                           levels = c("Yes", "No"))


# KPI_2_clinical_lead_copd_aa
dat_benchmark$KPI_2_clinical_lead_copd_aa <- "No"
dat_benchmark$KPI_2_clinical_lead_copd_aa[dat_benchmark$X3.2.Clinical.Lead.COPD == "Yes" &
                                            dat_benchmark$X3.3.Clinical.Lead.AA == "Yes"] <- "Yes"
dat_benchmark$KPI_2_clinical_lead_copd_aa <- factor(dat_benchmark$KPI_2_clinical_lead_copd_aa, levels = c("Yes", "No"))


# KPI_3_PR_copd_30days
dat_benchmark$KPI_3_PR_copd_30days <- "No"
dat_benchmark$KPI_3_PR_copd_30days[dat_benchmark$X5.3a.PR.Service.30.Days.COPD == "Yes"] <- "Yes"
dat_benchmark$KPI_3_PR_copd_30days <- factor(dat_benchmark$KPI_3_PR_copd_30days, levels = c("Yes", "No"))


# KPI_4_MDT_COPD

# new variable needed for 'other' that meets the benchmark for frequency

dat_benchmark$meets_weekly_kpi <- "No"
dat_benchmark$meets_weekly_kpi[dat_benchmark$X6.4a.MDT.Meeting.Frequency == "Weekly"] <- "Yes"
dat_benchmark$meets_weekly_kpi[dat_benchmark$X6.4a.Other %in% c("Asthma - fortnightly. COPD - weekly",
                                                                "3x a week - weekdays",
                                                                "COPD weekly and Asthma monthly",
                                                                "Monday-Friday",
                                                                "Twice a week, used to three times a week")] <- "Yes - Other"

dat_benchmark$KPI_4_MDT_COPD <- "No"
dat_benchmark$KPI_4_MDT_COPD[dat_benchmark$X6.4.COPD.MDT.Meetings %in% c("Yes for COPD only", "Yes for both") &
                               dat_benchmark$meets_weekly_kpi %in% c("Yes", "Yes - Other")] <- "Yes"
dat_benchmark$KPI_4_MDT_COPD <- factor(dat_benchmark$KPI_4_MDT_COPD, levels = c("Yes", "No"))


# KPI_5_transition
dat_benchmark$KPI_5_transition <- "No"
dat_benchmark$KPI_5_transition[dat_benchmark$`X8.1.Transition.Paediatric.To.Adult_The young person has a full record of their condition` == "Yes"] <- "Yes"
dat_benchmark$KPI_5_transition[dat_benchmark$`X8.1.Transition.Paediatric.To.Adult_Their GP is sent the same record` == "Yes"] <- "Yes"
dat_benchmark$KPI_5_transition[dat_benchmark$`X8.1.Transition.Paediatric.To.Adult_The young person is given the opportunity to be seen without their parents/carers` == "Yes"] <- "Yes"
dat_benchmark$KPI_5_transition[dat_benchmark$`X8.1.Transition.Paediatric.To.Adult_The young person has a transition plan that has been agreed with both paediatric and adult clinicians` == "Yes"] <- "Yes"
dat_benchmark$KPI_5_transition[dat_benchmark$`X8.1.Transition.Paediatric.To.Adult_The young person has a named case worker to assist in signposting for them and their family` == "Yes"] <- "Yes"
dat_benchmark$KPI_5_transition <- factor(dat_benchmark$KPI_5_transition, levels = c("Yes", "No"))


# KPI_6_severe_asthma_service
dat_benchmark$KPI_6_severe_asthma_service <- "No"
dat_benchmark$KPI_6_severe_asthma_service[dat_benchmark$X6.1.Does.your.hospital.have.a.severe.asthma.service == "Yes"] <- "Yes"
dat_benchmark$KPI_6_severe_asthma_service[dat_benchmark$X6.1a.does.your.hospital.have.access.to.severe.asthma.MDT == "Yes"] <- "Yes"
dat_benchmark$KPI_6_severe_asthma_service[dat_benchmark$X6.1b.do.you.have.a.referral.pathway.to.a.severe.asthma.service == "Yes"] <- "Yes"
dat_benchmark$KPI_6_severe_asthma_service <- factor(dat_benchmark$KPI_6_severe_asthma_service, levels = c("Yes", "No"))

table(dat_benchmark$KPI_6_severe_asthma_service)

# Hospital level
bmk_hosp_level <- dat_benchmark %>% select(region, integrated_care_system, trust_name, hosp_name, starts_with("KPI_"))

bmk_hosp_level

write.csv(bmk_hosp_level, "C:/Alex Harley/Audit_2023_onwards/2022-2023/AA/Analysis/Output/Hospital_level_benchmarking_COPD_AA_org_2022-23_KPI1_update.csv", row.names = FALSE)



# National level
bmk <- data.frame(Country = c("National (All)", "England", "Wales"))

KPI_vars <- dat_benchmark %>% select(starts_with("KPI_")) %>% colnames()

bmk <- cbind(bmk, 
             freq_stats(dat_benchmark, KPI_vars[1], remove_nos = TRUE, KPI = TRUE),
             freq_stats(dat_benchmark, KPI_vars[2], remove_nos = TRUE, KPI = TRUE),
             freq_stats(dat_benchmark, KPI_vars[3], remove_nos = TRUE, KPI = TRUE),
             freq_stats(dat_benchmark, KPI_vars[4], remove_nos = TRUE, KPI = TRUE),
             freq_stats(dat_benchmark, KPI_vars[5], remove_nos = TRUE, KPI = TRUE),
             freq_stats(dat_benchmark, KPI_vars[6], remove_nos = TRUE, KPI = TRUE))


# Check which column has duplicated column name
col_names <- colnames(bmk)
col_counts <- table(col_names)
print(col_counts)
# Var1?
# name of duplicated column
dup_names <- names(bmk)[duplicated(names(bmk))]
dup_count <- table(names(bmk))[names(table(names(bmk))) %in% dup_names]
dup_info <- data.frame(Column_Name = names(dup_count), Count = as.vector(dup_count))
print(dup_info)

# There are several [Var1] in [bmk]
# We want to delete them
duplicated_col <- which(duplicated(names(bmk)))
bmk <- bmk[, -duplicated_col]
bmk <- bmk %>% select(-Var1)

ncol(bmk)


write.csv(bmk, "C:/Alex Harley/Audit_2023_onwards/2022-2023/AA/Analysis/Output/National_level_benchmarking_COPD_AA_org_2022-23_KPI1_update.csv", row.names = FALSE)

