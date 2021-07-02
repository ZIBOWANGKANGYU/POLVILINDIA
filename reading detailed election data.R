# reading detailed election data
library(xlsx)
library(ggplot2)
library(sf)
library(dplyr)
library(tmap)
library(haven)
library(stringr)

Sys.setlocale("LC_TIME", "English")
Sys.setenv(LANG = "en_US.UTF-8")
elections_06_18<-read_dta("elections_06_18_population.dta")
elections_19<-read_dta("LA_Elections_05_2019_acpanel.dta")
elections_18<-read_dta("LA_Elections_06_2018.dta")
# verify that the district names are the same
election_district_name_code<-unique(elections_06_18%>%select(c("State_Code_11", "Dist_Code_11", "district_11")))
try<-merge(lynchings_date_2011_district, election_district_name_code, by.x=c("state_code", "district_code"), by.y=c("State_Code_11", "Dist_Code_11"))
try<-merge(try, district_list%>%select(c("district_code", "district_name")), by.x="district_code", by.y="district_code" )
try[try$district_11!=try$district_name,]
# verified
# add month and date data to election data
elections$STATE[elections$STATE=="JAMMU AND KASHMIR"]<-"JAMMU & KASHMIR"
elections$STATE[elections$STATE=="PUDUCHERRY"]<-"PONDICHERRY"
elections$STATE[elections$STATE=="UTTARAKHAND"]<-"UTTARANCHAL"
elections_06_18<-merge(elections_06_18, elections, by.x=c("year", "state_01"), by.y=c("Last.Elections.Held", "STATE"), all.x = TRUE)
elections_06_18$largest_party_state<-elections_06_18$Largest.Majority.Party
elections_06_18$government_state<-elections_06_18$Government
elections_06_18$winner_state<-elections_06_18$winner
elections_06_18$Largest.Majority.Party<-NULL
elections_06_18$Government<-NULL
elections_06_18$winner<-NULL
elections_06_18<-elections_06_18[is.na(elections_06_18$date_begining)==FALSE,]
elections_06_18$state_01<-as.character(elections_06_18$state_01)
elections_06_18$state_01[elections_06_18$state_01=="ORISSA"]<-"ODISHA"
elections_06_18$state_01[elections_06_18$state_01=="PONDICHERRY"]<-"PUDUCHERRY"
elections_06_18$state_01[elections_06_18$state_01=="DELHI"]<-"NCT OF DELHI"
elections_06_18$state_01[elections_06_18$state_01=="UTTARANCHAL"]<-"UTTARAKHAND"
elections_06_18$state_year<-paste0(elections_06_18$state_01, elections_06_18$year)

elections_19$state_year<-paste0(elections_19$state_01, elections_19$year)

# remove duplicated elections
elections_18$state_year<-paste0(elections_18$state_01, elections_18$year)
elections_18<-elections_18[elections_18$state_year%in%elections_19$state_year==FALSE,]

# aggregate candidate data to elections
elections_19_agr<-elections_19%>%group_by(year, state_01, ac_nm)%>%summarise(winning_party=ifelse(length(totalvotes)>=1, party[rank(-totalvotes) == 1], "non-existant"), party_2=ifelse(length(totalvotes)>=2, party[rank(-totalvotes) == 2], "non-existant"), 
                                                                winning_party_vote=ifelse(length(totalvotes)>=1, totalvotes[rank(-totalvotes) == 1], NA), party_2_vote=ifelse(length(totalvotes)>=2, totalvotes[rank(-totalvotes) == 2], NA), 
                                                                winning_candidate_name=ifelse(length(totalvotes)>=1, cand_nm[rank(-totalvotes) == 1], "non-existant"), party_2_candidate_name=ifelse(length(totalvotes)>=2, cand_nm[rank(-totalvotes) == 2], "non-existant"), 
                                                                electors=first(electors), turnout=first(turnout))
elections_18_agr<-elections_18%>%group_by(year, state_01, ac_nm)%>%summarise(winning_party=ifelse(length(totalvotes)>=1, party[rank(-totalvotes) == 1], "non-existant"),  party_2=ifelse(length(totalvotes)>=2, party[rank(-totalvotes) == 2], "non-existant"),
                                                                             winning_party_vote=ifelse(length(totalvotes)>=1, totalvotes[rank(-totalvotes) == 1], NA), party_2_vote=ifelse(length(totalvotes)>=2, totalvotes[rank(-totalvotes) == 2], NA), 
                                                                             winning_candidate_name=ifelse(length(totalvotes)>=1, cand_nm[rank(-totalvotes) == 1], "non-existant"), party_2_candidate_name=ifelse(length(totalvotes)>=2, cand_nm[rank(-totalvotes) == 2], "non-existant")
                                                                             )
elections_18_agr$electors<-NA
elections_18_agr$turnout<-NA
elections_19_agr<-rbind(elections_19_agr, elections_18_agr)
elections_19_agr$winning_party[elections_19_agr$winning_party=="non-existant"]<-NA
elections_19_agr$party_2[elections_19_agr$party_2=="non-existant"]<-NA
elections_19_agr$winning_candidate_name[elections_19_agr$winning_candidate_name=="non-existant"]<-NA
elections_19_agr$party_2_candidate_name[elections_19_agr$party_2_candidate_name=="non-existant"]<-NA

# add month and date data to election data
elections_19_agr<-merge(elections_19_agr, elections, by.x=c("year", "state_01"), by.y=c("Last.Elections.Held", "STATE"), all.x = TRUE)
elections_19_agr$largest_party_state<-elections_19_agr$Largest.Majority.Party
elections_19_agr$government_state<-elections_19_agr$Government
elections_19_agr$winner_state<-elections_19_agr$winner
elections_19_agr$Largest.Majority.Party<-NULL
elections_19_agr$Government<-NULL
elections_19_agr$winner<-NULL
elections_19_agr<-elections_19_agr[is.na(elections_19_agr$date_begining)==FALSE,]
elections_19_agr$state_01[elections_19_agr$state_01=="ORISSA"]<-"ODISHA"
elections_19_agr$state_01[elections_19_agr$state_01=="PONDICHERRY"]<-"PUDUCHERRY"
elections_19_agr$state_01[elections_19_agr$state_01=="DELHI"]<-"NCT OF DELHI"
elections_19_agr$state_01[elections_19_agr$state_01=="UTTARANCHAL"]<-"UTTARAKHAND"

# read in MLA electorate data
map_AC<-st_read("maps-master/maps-master/assembly-constituencies/India_AC.shp")
head(map_AC)
unique(lynchings_date_combined$state_name)
unique(map_AC$ST_NAME)
unique(lynchings_date_combined$state_name)%in%unique(map_AC$ST_NAME)
map_AC$ST_NAME<-as.character(map_AC$ST_NAME)
map_AC$ST_NAME[map_AC$ST_NAME=="DELHI"]<-"NCT OF DELHI"
map_AC$ST_NAME[map_AC$ST_NAME=="ORISSA"]<-"ODISHA"
unique(lynchings_date_combined$MLA_ELECTORATE)%in%unique(map_AC$AC_NAME)
map_AC$AC_NAME1<-gsub("(ST)", "", map_AC$AC_NAME, fixed=TRUE)
map_AC$AC_NAME1<-gsub("(SC)", "", map_AC$AC_NAME1, fixed=TRUE)
map_AC$AC_NAME1<-toupper(map_AC$AC_NAME1)
map_AC$AC_NAME1<-str_trim(map_AC$AC_NAME1)
try<-lynchings_date_combined[lynchings_date_combined$MLA_ELECTORATE%in%unique(map_AC$AC_NAME1)==FALSE,]
write.csv(unique(map_AC%>%as.data.frame()%>%select(c("ST_NAME", "DIST_NAME", "AC_NAME1"))), "AC_map_list.csv")
map_AC$AC_NAME1[map_AC$AC_NAME1=="CHAMKAUR SAHIB" & is.na(map_AC$AC_NAME1)==FALSE]<-"CHAMKAURSAHIB"
map_AC$AC_NAME1[map_AC$AC_NAME1=="SONBARSHA" & is.na(map_AC$AC_NAME1)==FALSE]<-"SONBARSA"
map_AC$AC_NAME1[map_AC$AC_NAME1=="BHUBANESWAR(MADHYA)" & is.na(map_AC$AC_NAME1)==FALSE]<-"BHUBANESWAR CENTRAL"
map_AC$AC_NAME1[map_AC$AC_NAME1=="GARHI SAMPLA-KILO" & is.na(map_AC$AC_NAME1)==FALSE]<-"GARHISAMPLA-KILOI"
map_AC$AC_NAME1[map_AC$AC_NAME1=="MORADABAD NAGAR" & is.na(map_AC$AC_NAME1)==FALSE]<-"MORADABADNAGAR"
map_AC$AC_NAME1[map_AC$AC_NAME1=="BAREILLY CANTT." & is.na(map_AC$AC_NAME1)==FALSE]<-"BAREILLYCANTT."

