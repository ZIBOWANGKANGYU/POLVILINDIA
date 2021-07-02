# read census data 
# 2011 census population
detach(package:openxlsx)
library(xlsx)
library(ggplot2)
library(sf)
library(dplyr)
library(tmap)
library(openxlsx)
Sys.setlocale("LC_TIME", "English")
Sys.setenv(LANG = "en_US.UTF-8")
A1<-read.xlsx("population/A-1_NO_OF_VILLAGES_TOWNS_HOUSEHOLDS_POPULATION_AND_AREA.xlsx", sheetIndex = 1)
A1<-A1[4:19995, 1:15]
colnames(A1)<-c("state_code", "district_code", "sub_district_code", "area_type", "name", "residential_type", "number_villages_inhabited", "number_villages_uninhabited", "number_towns", "number_households", "population_total", "population_male", "population_female", "area", "population_density")
A1_states<-A1[A1$area_type=="STATE",]
A1_districts<-A1[A1$area_type=="DISTRICT",]
# read religion data
for (i in 0:9){
  data_religion<-read.xlsx(paste0("population/DDW0", i, "C-01 MDDS.xls"), sheetIndex = 1)
  assign(paste0("religion", i), data_religion)
} 
for (i in 10:35){
  data_religion<-read.xlsx(paste0("population/DDW", i, "C-01 MDDS.xls"), sheetIndex = 1)
  assign(paste0("religion", i), data_religion)
} 
C1<-data.frame()
religion_colnames<-c("table_name", "state_code", "district_code", "tehsil_code", "town_code", "name", "residential_type", "population_total", "population_male", "population_female", 
                     "population_Hindu_total", "population_Hindu_male", "population_Hindu_female", 
                     "population_Muslim_total", "population_Muslim_male", "population_Muslim_female", 
                     "population_Christian_total", "population_Christian_male", "population_Christian_female", 
                     "population_Sikh_total", "population_Sikh_male", "population_Sikh_female",
                     "population_Buddhist_total", "population_Buddhist_male", "population_Buddhist_female",
                     "population_Jain_total", "population_Jain_male", "population_Jain_female",
                     "population_other_total", "population_other_male", "population_other_female",
                     "population_not_stated_total", "population_not_stated_male", "population_not_stated_female")
for (i in 1:35){
  assign("religion_state", get(paste0("religion", i)))
  colnames(religion_state)<-religion_colnames
  religion_state<-religion_state[5:nrow(religion_state), is.na(religion_state[6,])==FALSE]
  assign(paste0("religion", i), religion_state)
  C1<-rbind(C1, religion_state)
}
# state population information
population_state<-C1[C1$district_code=="000"& C1$tehsil_code=="00000" & C1$town_code=="000000",]
population_state_total<-population_state[population_state$residential_type=="Total",]
# district population information
population_district<-C1[C1$district_code!="000"& C1$tehsil_code=="00000" & C1$town_code=="000000",]
population_district_total<-population_district[population_district$residential_type=="Total",]
population_district_urbanization<-population_district%>%select(c("district_code", "residential_type", "population_total"))
population_district_urbanization<-population_district_urbanization[is.na(population_district_urbanization$district_code)==FALSE,]
population_district_urbanization<-data.table::dcast(data = as.data.table(population_district_urbanization), district_code~residential_type)
population_district_urbanization$urb_rate<-population_district_urbanization$Urban%>%as.character()%>%as.numeric()/population_district_urbanization$Total%>%as.character()%>%as.numeric()

population_district_total<-merge(population_district_total, population_district_urbanization%>%select(c("district_code", "urb_rate")), by="district_code", all.x=TRUE)
# linking lynching cases to locations
# state level
A1_states$name<-as.character(A1_states$name)
A1_states$name[A1_states$name=="JAMMU & KASHMIR @&"]<-"JAMMU & KASHMIR"
lynchings_date$STATE<-as.character(lynchings_date$STATE)
lynchings_date$STATE[lynchings_date$STATE=="JAMMU AND KASHMIR"]<-"JAMMU & KASHMIR"
lynchings_date$STATE[lynchings_date$STATE=="ORISSA"]<-"ODISHA"
lynchings_date_2011<-lynchings_date
lynchings_date_2011$STATE[lynchings_date_2011$STATE=="TELANGANA"]<-"ANDHRA PRADESH"
lynchings_date_2011$STATE[lynchings_date_2011$STATE=="DELHI"]<-"NCT OF DELHI"
state_list<-unique(A1_states%>%select(state_code, name))
lynchings_date_2011<-merge(lynchings_date_2011, state_list, by.x="STATE", by.y="name", all.x = TRUE)
# district level
district_list<-unique(A1_districts%>%select(state_code, district_code, name))
colnames(district_list)[3]<-"district_name"
district_list<-merge(district_list, state_list, by="state_code", all.x = TRUE)
colnames(district_list)[4]<-"state_name"
district_list$district_name<-toupper(district_list$district_name)
lynchings_date_2011<-merge(lynchings_date_2011, district_list, by.x=c("state_code", "DISTRICT"), by.y=c("state_code", "district_name"), all.x = TRUE)
write.xlsx(lynchings_date_2011[is.na(lynchings_date_2011$district_code)==TRUE,], "population/district_modify_0.xlsx")
write.xlsx(district_list, "population/district_list.xlsx")
lynchings_date_2011_modify<-read.xlsx("population/district_modify.xlsx", sheetIndex = 1)
lynchings_date_2011_modify$district_code<-NULL
lynchings_date_2011_modify$state_name<-NULL
lynchings_date_2011_modify$NA.<-NULL
district_list$district_name[district_list$district_name=="MEWAT "]<-"MEWAT"
district_list$district_name[district_list$district_name=="PALWAL "]<-"PALWAL"
district_list$district_name[district_list$district_name=="BULANDSHAHR "]<-"BULANDSHAHR"
district_list$district_name[district_list$district_name=="KHORDHA "]<-"KHORDHA"
district_list$district_name[district_list$district_name=="HARDA "]<-"HARDA"
district_list$district_name[district_list$district_name=="KHORDHA "]<-"KHORDHA"
lynchings_date_2011_modify<-merge(lynchings_date_2011_modify, district_list, by.x=c("state_code", "DISTRICT_MODIFY"), by.y=c("state_code", "district_name"), all.x = TRUE)
lynchings_date_2011_district<-rbind(lynchings_date_2011_modify%>%select(-c("DISTRICT_MODIFY", "DISTRICT", "STATE", "STATE_MODIFY", "TOWN_MODIFY", "MODIFY_SOURCE", "state_name")), lynchings_date_2011[is.na(lynchings_date_2011$district_code)==FALSE,]%>%select(-c("DISTRICT", "STATE", "state_name")))
lynchings_date_2011_district$date_gap_state_beginning<-lynchings_date_2011_district$date_gap_beginning
lynchings_date_2011_district$date_gap_state_ending<-lynchings_date_2011_district$date_gap_ending
lynchings_date_2011_district$date_gap_state_government<-lynchings_date_2011_district$date_gap_government
lynchings_date_2011_district$winning_party_state<-lynchings_date_2011_district$winning_party
lynchings_date_2011_district$winning_party_pre_state<-lynchings_date_2011_district$winning_party_pre
lynchings_date_2011_district$date_gap_beginning<-NULL
lynchings_date_2011_district$date_gap_ending<-NULL
lynchings_date_2011_district$date_gap_government<-NULL
lynchings_date_2011_district$winning_party<-NULL
lynchings_date_2011_district$winning_party_pre<-NULL
lynchings_date_2011_district<-merge(lynchings_date_2011_district, district_list, by=c("state_code", "district_code"), all.x = TRUE)

# adding electoral district information to lynching data 
write.xlsx(lynchings_date_2011_district, "lynching_Add_electorate.xls")
write.xlsx(unique(elections_06_18%>%select(c("state_01", "district_01", "ac_nm"))), "electorate_list.xls")
lynchings_date_2011_ac<-read.xlsx("lynching_Add_electorate_added.xls", sheetIndex = 1)
lynchings_date_2011_MLA<-merge(lynchings_date_2011_district, lynchings_date_2011_ac%>%select(c("DATE", "state_code", "MLA_ELECTORATE", "SOURCE")), by.x=c("DATE", "state_code"), by.y=c("DATE", "state_code"), all = TRUE)

# linking district information to election data 
list_elections_before<-list()
list_elections_after<-list()
list_elections_before_one<-list()
list_elections_after_one<-list()
elections_06_18$State_Code_11[elections_06_18$state_01=="PUNJAB"]<-"03"
elections_06_18$State_Code_11[elections_06_18$state_01=="ASSAM"]<-"18"
elections_06_18$State_Code_11[elections_06_18$state_01=="JAMMU & KASHMIR"]<-"01"
elections_06_18$State_Code_11[elections_06_18$state_01=="UTTAR PRADESH"]<-"09"
# no incident happened between beginning and government formation
for (i in 1:nrow(lynchings_date_2011_MLA)){
  elections_list<-elections_06_18[elections_06_18$state_01==lynchings_date_2011_MLA$state_name[i] & elections_06_18$ac_nm==lynchings_date_2011_MLA$MLA_ELECTORATE[i], ]
  elections_list<-elections_list[is.na(elections_list$year)==FALSE,]
  elections_before<-elections_list[elections_list$date_begining<=lynchings_date_2011_MLA$DATE[i],]
  elections_before_one<-elections_before[which.max(elections_before$date_begining),]
  elections_before_two<-elections_before[rank(-as.numeric(elections_before$date_begining))==2,]
  elections_after<-elections_list[elections_list$date_begining>lynchings_date_2011_MLA$DATE[i],]
  elections_after_one<-elections_after[which.min(elections_after$date_begining),]
  list_elections_before[[i]]<-elections_before
  if (nrow(elections_before_one)>0) {
    list_elections_before_one[[i]]<-elections_before_one
  } else{
    list_elections_before_one[[i]]<-rep(NA, 54)
  }
  if (nrow(elections_before_two)>0) {
    list_elections_before_two[[i]]<-elections_before_two
  } else{
    list_elections_before_two[[i]]<-rep(NA, 54)
  }
  list_elections_after[[i]]<-elections_after
  if (nrow(elections_after_one)>0) {
    list_elections_after_one[[i]]<-elections_after_one
  } else{
    list_elections_after_one[[i]]<-rep(NA, 54)
  }
}
for (i in 1:nrow(lynchings_date_2011_MLA)){
  lynchings_date_2011_MLA$num_election_after[i]<-nrow(list_elections_after[[i]])
  lynchings_date_2011_MLA$num_election_before[i]<-nrow(list_elections_before[[i]])
}
elections_before_df<-as.data.frame(do.call(rbind,list_elections_before_one))
elections_before_two_df<-as.data.frame(do.call(rbind,list_elections_before_two))
elections_after_df<-as.data.frame(do.call(rbind,list_elections_after_one))
elections_before_df<-elections_before_df%>%select(c("cand_nm", "cand_sex", "cand_age", "totalvotes", "party", "date_begining", "date_ending", "date_government", "largest_party_state", "winner_state"))
colnames(elections_before_df)<-c("winner_name_before", "winner_sex_before", "winner_age_before", "winner_votes_before", "winner_party_before", "date_before_beginning", "date_before_ending", "date_before_government", "largest_party_state_before", "winner_state_before")
elections_before_two_df<-elections_before_two_df%>%select(c("cand_nm", "cand_sex", "cand_age", "totalvotes", "party", "date_begining", "date_ending", "date_government", "largest_party_state", "winner_state"))
colnames(elections_before_two_df)<-c("winner_name_before_two", "winner_sex_before_two", "winner_age_before_two", "winner_votes_before_two", "winner_party_before_two", "date_before_two_beginning", "date_before_two_ending", "date_before_two_government", "largest_party_state_before_two", "winner_state_before_two")
elections_after_df<-elections_after_df%>%select(c("cand_nm", "cand_sex", "cand_age", "totalvotes", "party", "date_begining", "date_ending", "date_government", "largest_party_state", "winner_state"))
colnames(elections_after_df)<-c("winner_name_after", "winner_sex_after", "winner_age_after", "winner_votes_after", "winner_party_after", "date_after_beginning", "date_after_ending", "date_after_government", "largest_party_state_after", "winner_state_after")
lynchings_date_electorate<-cbind(lynchings_date_2011_MLA, elections_before_df, elections_before_two_df, elections_after_df)

# reading education data 
education_0<-read.xlsx("Education/DDW-0000C-12.xlsx", sheetIndex = 1)
for (i in 1:9){
  data_education<-read.xlsx(paste0("Education/DDW-0", i, "00C-12.xls"), sheetIndex = 1)
  assign(paste0("education_", i), data_education)
} 
for (i in 10:35){
  data_education<-read.xlsx(paste0("Education/DDW-", i, "00C-12.xls"), sheetIndex = 1)
  assign(paste0("education_", i), data_education)
} 
C12<-data.frame()
education_colnames<-c("table_name", "state_code", "district_code", "name", "residential_type", "age", 
                      "population_total", "population_male", "population_female", "attending_main_total", 
                      "attending_main_male", "attending_main_female", "attending_marginal_3_6_total", 
                      "attending_marginal_3_6_male", "attending_marginal_3_6_female", "attending_marginal_3_total", 
                      "attending_marginal_3_male", "attending_marginal_3_female", "attending_none_total", 
                      "attending_none_male", "attending_none_female", "not_attending_main_total", "not_attending_main_male", 
                      "not_attending_main_female", "not_attending_marginal_3_6_total", "not_attending_marginal_3_6_male", 
                      "not_attending_marginal_3_6_female", "not_attending_marginal_3_total", "not_attending_marginal_3_male", 
                      "not_attending_marginal_3_female", "not_attending_none_total", "not_attending_none_male", 
                      "not_attending_none_female","")
for (i in 1:35){
  assign("education_state", get(paste0("education_", i)))
  colnames(education_state)<-education_colnames
  education_state<-education_state[7:nrow(education_state), 1:33]
  assign(paste0("education_", i), education_state)
  C12<-rbind(C12, education_state)
}
education_district<-C12[C12$age=="5-19" & C12$residential_type=="Total",]
education_district[,7:33] <- lapply(education_district[,7:33], function(x) as.numeric(as.character(x)))
education_district$attending_total<-education_district$attending_main_total+education_district$attending_marginal_3_6_total+education_district$attending_marginal_3_total+education_district$attending_none_total
education_district$attending_female<-education_district$attending_main_female+education_district$attending_marginal_3_6_female+education_district$attending_marginal_3_female+education_district$attending_none_female
education_district$attending_rate_total<-education_district$attending_total/education_district$population_total
education_district$attending_rate_female<-education_district$attending_female/education_district$population_female

population_district_total<-merge(population_district_total, education_district%>%select("district_code", "attending_rate_total", "attending_rate_female"), by="district_code")

# reading household data
household_0<-read.xlsx("Households/DDW-HH3711-0000.xls", sheetIndex = 1)
for (i in 1:8){
  data_household<-read.xlsx(paste0("Households/DDW-HH3711-0", i, "00.xls"), sheetIndex = 1)
  assign(paste0("household_", i), data_household)
} 
for (i in 10:18){
  data_household<-read.xlsx(paste0("Households/DDW-HH3711-", i, "00.xls"), sheetIndex = 1)
  assign(paste0("household_", i), data_household)
} 
for (i in 20:27){
  data_household<-read.xlsx(paste0("Households/DDW-HH3711-", i, "00.xls"), sheetIndex = 1)
  assign(paste0("household_", i), data_household)
} 
for (i in 29:35){
  data_household<-read.xlsx(paste0("Households/DDW-HH3711-", i, "00.xls"), sheetIndex = 1)
  assign(paste0("household_", i), data_household)
} 
household_9<-read.xlsx("Households/DDW-HH3711-0900.xlsx", sheet = 1)
household_19<-read.xlsx("Households/DDW-HH3711-1900.xlsx", sheet= 1)
household_28<-read.xlsx("Households/DDW-HH3711-2800.xlsx", sheet = 1)
HH37<-data.frame()
household_colnames<-c("table_name", "state_code", "district_code", "tesil_code", "town_code", "name", "residential_type", 
                      "main_source_drinking_water", "available_drinking_water", "total_households", 
                      "electricity_av_latrine_av", "electricity_av_latrine_nav", "electricity_nav_latrine_av", 
                      "electricity_nav_latrine_nav")
for (i in 1:35){
  assign("household_state", get(paste0("household_", i)))
  household_state<-household_state[6:nrow(household_state), 1:14]
  colnames(household_state)<-household_colnames
  assign(paste0("household_", i), household_state)
  HH37<-rbind(HH37, household_state)
}
household_district<-HH37[HH37$tesil_code=="00000" & HH37$residential_type=="Total" & (HH37$main_source_drinking_water=="All Sources" | HH37$main_source_drinking_water=="Tap water from treated source") & (HH37$available_drinking_water=="Total number of households" | HH37$available_drinking_water=="Within premises"),]
population_district_household<-data.frame(district_code=population_district_total$district_code)
population_district_household$total_household<-NA
population_district_household$equipped_household<-NA
for (i in (1:length(population_district_total$district_code))){
  population_district_household$total_household[i]<-as.character(household_district$total_households[household_district$district_code==as.character(population_district_total$district_code[i]) & is.na(household_district$district_code)==FALSE & household_district$main_source_drinking_water=="All Sources" & is.na(household_district$main_source_drinking_water)==FALSE & household_district$available_drinking_water=="Total number of households" & is.na(household_district$available_drinking_water)==FALSE])
  population_district_household$equipped_household[i]<-as.character(household_district$electricity_av_latrine_av[household_district$district_code==as.character(population_district_total$district_code[i]) & is.na(household_district$district_code)==FALSE & household_district$main_source_drinking_water=="Tap water from treated source" & is.na(household_district$main_source_drinking_water)==FALSE & household_district$available_drinking_water=="Within premises" & is.na(household_district$available_drinking_water)==FALSE])
}
population_district_household$equip_rate<-as.numeric(as.character(population_district_household$equipped_household))/as.numeric(as.character(population_district_household$total_household))
population_district_total<-merge(population_district_total, population_district_household%>%select(c("district_code", "equip_rate")), by="district_code", all.x = TRUE)

# reading caste and tribe data
caste0<-read.xlsx("castes and tribes/castes.xls", sheetIndex = 1)
caste<-caste0[caste0$District!="000" & caste0$TRU=="Total",]
caste<-caste%>%select(c("District", "TOT_P"))
colnames(caste)<-c("district_code", "SC_pop")
tribe0<-read.xlsx("castes and tribes/tribes.xls", sheetIndex = 1)
tribe<-tribe0[tribe0$District!="000" & tribe0$TRU=="Total",]
tribe<-tribe%>%select(c("District", "TOT_P"))
colnames(tribe)<-c("district_code", "ST_pop")
population_district_total<-merge(population_district_total, caste, by="district_code", all.x = TRUE)
population_district_total<-merge(population_district_total, tribe, by="district_code", all.x = TRUE)
population_district_total$SC_rate<-population_district_total$SC_pop/population_district_total$population_total
population_district_total$ST_rate<-population_district_total$ST_pop/population_district_total$population_total

# linking district information to election data: election_19
list_elections_before<-list()
list_elections_after<-list()
list_elections_before_one<-list()
list_elections_before_two<-list()
list_elections_after_one<-list()
sum(elections_19_agr$state_01%in%elections_06_18$state_01==FALSE)
elections_19_agr$state_ac<-paste0(elections_19_agr$state_01, elections_19_agr$ac_nm)
elections_06_18$state_ac<-paste0(elections_06_18$state_01, elections_06_18$ac_nm)
sum(elections_19_agr$state_ac%in%elections_06_18$state_ac==FALSE)
for (i in 1:nrow(lynchings_date_2011_MLA)){
  elections_list<-elections_19_agr[elections_19_agr$state_01==lynchings_date_2011_MLA$state_name[i] & elections_19_agr$ac_nm==lynchings_date_2011_MLA$MLA_ELECTORATE[i], ]
  elections_list<-elections_list[is.na(elections_list$year)==FALSE,]
  elections_before<-elections_list[elections_list$date_begining<=lynchings_date_2011_MLA$DATE[i],]
  elections_before_one<-elections_before[which.max(elections_before$date_begining),]
  elections_before_two<-elections_before[rank(-as.numeric(elections_before$date_begining))==2,]
  elections_after<-elections_list[elections_list$date_begining>lynchings_date_2011_MLA$DATE[i],]
  elections_after_one<-elections_after[which.min(elections_after$date_begining),]
  list_elections_before[[i]]<-elections_before
  if (nrow(elections_before_one)>0) {
    list_elections_before_one[[i]]<-elections_before_one
  } else{
    list_elections_before_one[[i]]<-rep(NA, 18)
  }
  if (nrow(elections_before_two)>0) {
    list_elections_before_two[[i]]<-elections_before_two
  } else{
    list_elections_before_two[[i]]<-rep(NA, 18)
  }
  list_elections_after[[i]]<-elections_after
  if (nrow(elections_after_one)>0) {
    list_elections_after_one[[i]]<-elections_after_one
  } else{
    list_elections_after_one[[i]]<-rep(NA, 18)
  }
}
for (i in 1:nrow(lynchings_date_2011_MLA)){
  lynchings_date_2011_MLA$num_election_after2[i]<-nrow(list_elections_after[[i]])
  lynchings_date_2011_MLA$num_election_before2[i]<-nrow(list_elections_before[[i]])
}
elections_before_df_2<-as.data.frame(do.call(rbind,list_elections_before_one))
elections_before_two_df_2<-as.data.frame(do.call(rbind,list_elections_before_two))
elections_after_df_2<-as.data.frame(do.call(rbind,list_elections_after_one))
elections_before_df_2<-elections_before_df_2%>%select(c("winning_candidate_name", "winning_party_vote", "winning_party", 
                                                        "party_2_candidate_name", "party_2_vote", "party_2", 
                                                        "date_begining", "date_ending", "date_government", "largest_party_state", "winner_state"))
colnames(elections_before_df_2)<-c("winner_name_before_2", "winner_votes_before_2", "winner_party_before_2", 
                                 "party2_name_before_2", "party2_votes_before_2", "party2_party_before_2",
                                 "date_before_2_beginning", "date_before_2_ending", "date_before_2_government", "largest_party_state_before_2", "winner_state_before_2")
elections_before_two_df_2<-elections_before_two_df_2%>%select(c("winning_candidate_name", "winning_party_vote", "winning_party", 
                                                        "party_2_candidate_name", "party_2_vote", "party_2", 
                                                        "date_begining", "date_ending", "date_government", "largest_party_state", "winner_state"))
colnames(elections_before_two_df_2)<-c("winner_name_before_two_2", "winner_votes_before_two_2", "winner_party_before_two_2", 
                                   "party2_name_before_two_2", "party2_votes_before_two_2", "party2_party_before_two_2",
                                   "date_before_two_2_beginning","date_before_two_2_ending","date_before_two_2_government", "largest_party_state_before_two_2", "winner_state_before_two_2")
elections_after_df_2<-elections_after_df_2%>%select(c("winning_candidate_name", "winning_party_vote", "winning_party", 
                                                      "party_2_candidate_name", "party_2_vote", "party_2", 
                                                      "date_begining", "date_ending", "date_government", "largest_party_state", "winner_state"))
colnames(elections_after_df_2)<-c("winner_name_after_2", "winner_votes_after_2", "winner_party_after_2", 
                                   "party2_name_after_2", "party2_votes_after_2", "party2_party_after_2",
                                   "date_after_2_beginning","date_after_2_ending","date_after_2_government", "largest_party_state_after_2", "winner_state_after_2")
lynchings_date_electorate_2<-cbind(lynchings_date_2011_MLA, elections_before_df_2, elections_before_two_df_2, elections_after_df_2)
write.xlsx(lynchings_date_electorate_2[lynchings_date_electorate_2$num_election_after2==0 & lynchings_date_electorate_2$num_election_before2==0 , ], "electorate_name_modify.xls")
write.xlsx(unique(elections_19_agr%>%select(c("state_01", "ac_nm"))), "ac_list_19.xls")

# combining them

lynchings_date_combined<-lynchings_date_electorate[, 1:29]

lynchings_date_combined$winner_name_before<-ifelse(is.na(lynchings_date_electorate_2$winner_name_before_2)==FALSE, lynchings_date_electorate_2$winner_name_before_2, lynchings_date_electorate$winner_name_before)
lynchings_date_combined$winner_votes_before<-ifelse(is.na(lynchings_date_electorate_2$winner_votes_before_2)==FALSE, lynchings_date_electorate_2$winner_votes_before_2, lynchings_date_electorate$winner_votes_before)
lynchings_date_combined$winner_party_before<-ifelse(is.na(lynchings_date_electorate_2$winner_party_before_2)==FALSE, lynchings_date_electorate_2$winner_party_before_2, lynchings_date_electorate$winner_party_before)
lynchings_date_combined$party2_name_before<-lynchings_date_electorate_2$party2_name_before_2
lynchings_date_combined$party2_votes_before<-lynchings_date_electorate_2$party2_votes_before_2
lynchings_date_combined$party2_party_before<-lynchings_date_electorate_2$party2_party_before_2

lynchings_date_combined$date_before_beginning<-ifelse(is.na(lynchings_date_electorate_2$date_before_2_beginning)==FALSE, lynchings_date_electorate_2$date_before_2_beginning, lynchings_date_electorate$date_before_beginning)
lynchings_date_combined$date_before_ending<-ifelse(is.na(lynchings_date_electorate_2$date_before_2_ending)==FALSE, lynchings_date_electorate_2$date_before_2_ending, lynchings_date_electorate$date_before_ending)
lynchings_date_combined$date_before_government<-ifelse(is.na(lynchings_date_electorate_2$date_before_2_government)==FALSE, lynchings_date_electorate_2$date_before_2_government, lynchings_date_electorate$date_before_government)

lynchings_date_combined$largest_party_state_before<-ifelse(is.na(lynchings_date_electorate_2$largest_party_state_before_2)==FALSE, lynchings_date_electorate_2$largest_party_state_before_2, lynchings_date_electorate$largest_party_state_before)
lynchings_date_combined$winner_state_before<-ifelse(is.na(lynchings_date_electorate_2$winner_state_before_2)==FALSE, lynchings_date_electorate_2$winner_state_before_2, lynchings_date_electorate$winner_state_before)

lynchings_date_combined$winner_name_before_two<-ifelse(is.na(lynchings_date_electorate_2$winner_name_before_two_2)==FALSE, lynchings_date_electorate_2$winner_name_before_two_2, lynchings_date_electorate$winner_name_before_two)
lynchings_date_combined$winner_votes_before_two<-ifelse(is.na(lynchings_date_electorate_2$winner_votes_before_two_2)==FALSE, lynchings_date_electorate_2$winner_votes_before_two_2, lynchings_date_electorate$winner_votes_before_two)
lynchings_date_combined$winner_party_before_two<-ifelse(is.na(lynchings_date_electorate_2$winner_party_before_two_2)==FALSE, lynchings_date_electorate_2$winner_party_before_two_2, lynchings_date_electorate$winner_party_before_two)
lynchings_date_combined$party2_name_before_two<-lynchings_date_electorate_2$party2_name_before_two_2
lynchings_date_combined$party2_votes_before_two<-lynchings_date_electorate_2$party2_votes_before_two_2
lynchings_date_combined$party2_party_before_two<-lynchings_date_electorate_2$party2_party_before_two_2

lynchings_date_combined$date_before_two_beginning<-ifelse(is.na(lynchings_date_electorate_2$date_before_two_2_beginning)==FALSE, lynchings_date_electorate_2$date_before_two_2_beginning, lynchings_date_electorate$date_before_two_beginning)
lynchings_date_combined$date_before_two_ending<-ifelse(is.na(lynchings_date_electorate_2$date_before_two_2_ending)==FALSE, lynchings_date_electorate_2$date_before_two_2_ending, lynchings_date_electorate$date_before_two_ending)
lynchings_date_combined$date_before_two_government<-ifelse(is.na(lynchings_date_electorate_2$date_before_two_2_government)==FALSE, lynchings_date_electorate_2$date_before_two_2_government, lynchings_date_electorate$date_before_two_government)

lynchings_date_combined$largest_party_state_before_two<-ifelse(is.na(lynchings_date_electorate_2$largest_party_state_before_two_2)==FALSE, lynchings_date_electorate_2$largest_party_state_before_two_2, lynchings_date_electorate$largest_party_state_before_two)
lynchings_date_combined$winner_state_before_two<-ifelse(is.na(lynchings_date_electorate_2$winner_state_before_two_2)==FALSE, lynchings_date_electorate_2$winner_state_before_two_2, lynchings_date_electorate$winner_state_before_two)

lynchings_date_combined$winner_name_after<-ifelse(is.na(lynchings_date_electorate_2$winner_name_after_2)==FALSE, lynchings_date_electorate_2$winner_name_after_2, lynchings_date_electorate$winner_name_after)
lynchings_date_combined$winner_votes_after<-ifelse(is.na(lynchings_date_electorate_2$winner_votes_after_2)==FALSE, lynchings_date_electorate_2$winner_votes_after_2, lynchings_date_electorate$winner_votes_after)
lynchings_date_combined$winner_party_after<-ifelse(is.na(lynchings_date_electorate_2$winner_party_after_2)==FALSE, lynchings_date_electorate_2$winner_party_after_2, lynchings_date_electorate$winner_party_after)
lynchings_date_combined$party2_name_after<-lynchings_date_electorate_2$party2_name_after_2
lynchings_date_combined$party2_votes_after<-lynchings_date_electorate_2$party2_votes_after_2
lynchings_date_combined$party2_party_after<-lynchings_date_electorate_2$party2_party_after_2

lynchings_date_combined$date_after_beginning<-ifelse(is.na(lynchings_date_electorate_2$date_after_2_beginning)==FALSE, lynchings_date_electorate_2$date_after_2_beginning, lynchings_date_electorate$date_after_beginning)
lynchings_date_combined$date_after_ending<-ifelse(is.na(lynchings_date_electorate_2$date_after_2_ending)==FALSE, lynchings_date_electorate_2$date_after_2_ending, lynchings_date_electorate$date_after_ending)
lynchings_date_combined$date_after_government<-ifelse(is.na(lynchings_date_electorate_2$date_after_2_government)==FALSE, lynchings_date_electorate_2$date_after_2_government, lynchings_date_electorate$date_after_government)

lynchings_date_combined$largest_party_state_after<-ifelse(is.na(lynchings_date_electorate_2$largest_party_state_after_2)==FALSE, lynchings_date_electorate_2$largest_party_state_after_2, lynchings_date_electorate$largest_party_state_after)
lynchings_date_combined$winner_state_after<-ifelse(is.na(lynchings_date_electorate_2$winner_state_after_2)==FALSE, lynchings_date_electorate_2$winner_state_after_2, lynchings_date_electorate$winner_state_after)

lynchings_date_combined$MLA_ELECTORATE<-as.character(lynchings_date_combined$MLA_ELECTORATE)
lynchings_date_combined$MLA_ELECTORATE[lynchings_date_combined$MLA_ELECTORATE=="PANIPATI" & is.na(lynchings_date_combined$MLA_ELECTORATE)==FALSE]<-"PANIPAT CITY"
