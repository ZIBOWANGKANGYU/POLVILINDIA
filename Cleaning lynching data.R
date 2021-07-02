# Mark Wang, kaw129@ucsd.edu
# 11 April 2019
library(xlsx)
library(ggplot2)
library(sf)
library(dplyr)
library(tmap)
Sys.setlocale("LC_TIME", "English")
Sys.setenv(LANG = "en_US.UTF-8")
# cleaning and importing lynching data
lynchings_date<-read.xlsx(file = "lynchings.xlsx", sheetIndex = 2, encoding = "UTF-8")
colnames(lynchings_date)[16]<-"Notes"
lynchings_date<-lynchings_date[1:125,1:16]
lynchings_date$DATE<-as.Date(paste(lynchings_date$MONTH, lynchings_date$DAY,lynchings_date$YEAR, sep=" "), "%B %d %Y")
lynchings_date$STATE<-as.character(lynchings_date$STATE)
lynchings_date$STATE[lynchings_date$STATE=="TELENGANA"]<-"TELANGANA"
# cleaning and importing maps
map_Tehsil<-st_read("India_Tehsil_Boundary.shp")
map_Tehsil_df<-data.frame(map_Tehsil)%>%select(-geometry)
map_District<-st_read("India_District_Boundary.shp")
map_District_df<-data.frame(map_District)%>%select(-geometry)
map_State<-st_read("India_State_Boundary.shp")
map_State_df<-data.frame(map_State)%>%select(-geometry)
# importing election data
elections<-read.xlsx("elections.xlsx", encoding="UTF=8", sheetIndex = 2)
colnames(elections)[1]<-"STATE"
elections$STATE<-toupper(elections$STATE)
elections$date_begining<-as.Date(as.character(elections$date_begining), "%d %B %Y")
elections$date_ending<-as.Date(as.character(elections$date_ending), "%d %B %Y")
elections$date_government<-as.Date(as.character(elections$date_government), "%d %B %Y")
elections$STATE[elections$STATE=="JAMMU & KASHMIR"]<-"JAMMU AND KASHMIR"
elections$STATE[elections$STATE=="ODISHA"]<-"ORISSA"
# aggregate by month and state
lynchings_date$ARRESTS_MADE<-as.numeric(as.character(lynchings_date$ARRESTS_MADE))
lynchings_date$STATE<-as.character(lynchings_date$STATE)
lynchings_date$STATE[lynchings_date$STATE=="JAMMU & KASHMIR"]<-"JAMMU AND KASHMIR"
lynchings_date$STATE[lynchings_date$STATE=="ODISHA"]<-"ORISSA"
lynchings_date$STATE<-as.factor(as.character(lynchings_date$STATE))
# May need population data on this 
# Number of victims of important states
ggplot(lynchings_date[lynchings_date$STATE=="UTTAR PRADESH",], aes(x=DATE))+
  geom_point(aes(y=VICTIMS))+scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  labs(x="time", y="victims", title = "Cow lynching and number of victims in Uttar Pradesh")+
  geom_rect(aes(xmin=as.Date("2017-02-11"), xmax=as.Date("2017-03-08"),ymin=0, ymax=Inf, fill="BJP win"), alpha = 0.5, show.legend = TRUE)+
  scale_fill_manual("MLA election",values=c("orange"))
ggplot(lynchings_date[lynchings_date$STATE=="HARYANA",], aes(x=DATE))+
  geom_point(aes(y=VICTIMS))+scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  labs(x="time", y="victims", title = "Cow lynching and number of victims in Haryana")+
  geom_vline(aes(xintercept=as.Date("2014-10-15"), color="BJP win"), size=1.5) + 
  scale_color_manual("MLA election",values=c("orange"))
# automize ggplot
elections<-elections[order(elections$date_begining),]
elections$winner<-"BJP losing"
elections$winner[elections$Government=="NDA"]<-"BJP winning"
cols <- c("BJP winning" = "orange", "BJP losing" = "green")
cols_incumbent <- c("BJP" = "orange", "non-BJP" = "green")
# plot states with cow lynching
ggplot(lynchings_date, aes(x=DATE))+
  geom_point(aes(y=VICTIMS))+scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  labs(x="time", y="victims")+xlim(as.Date("2012-6-1"), Sys.Date())+
  geom_vline(data=elections[elections$STATE %in% lynchings_STATE$STATE,], mapping = aes(xintercept=date, color=winner), size=1.5, show.legend = TRUE)+
  scale_color_manual("MLA election",values=cols)+facet_wrap(~STATE, ncol = 3)+labs(title = "State election cycle and cow lynching victim, 2012-2019")
ggsave("state election cycle and cow lynching victim.pdf", width = 12, height = 10)
# cow lynchings relative to elections on t=0
for (i in 1:nrow(lynchings_date)){
  dif_vec<-lynchings_date$DATE[i]-elections$date_begining[elections$STATE==lynchings_date$STATE[i]]
  winner_vec<-elections$winner[elections$STATE==lynchings_date$STATE[i]]
  lynchings_date$date_gap_beginning[i]<-dif_vec[which.min(abs(dif_vec))]
  lynchings_date$winning_party[i]<-winner_vec[which.min(abs(dif_vec))]
}
for (i in 1:nrow(lynchings_date)){
  dif_vec<-lynchings_date$DATE[i]-elections$date_ending[elections$STATE==lynchings_date$STATE[i]]
  lynchings_date$date_gap_ending[i]<-dif_vec[which.min(abs(dif_vec))]
}
for (i in 1:nrow(lynchings_date)){
  dif_vec<-lynchings_date$DATE[i]-elections$date_government[elections$STATE==lynchings_date$STATE[i]]
  lynchings_date$date_gap_government[i]<-dif_vec[which.min(abs(dif_vec))]
}
lynchings_date$date_gap_government[lynchings_date$date_gap_government>lynchings_date$date_gap_beginning]<-NA
# get rid of more than 2-year gaps
ggplot()+
  geom_density(data=lynchings_date[lynchings_date$winning_party=="BJP winning",], aes(x=date_gap_beginning, color=winning_party), size=1.5, bw=60)+
  geom_density(data=lynchings_date[lynchings_date$winning_party=="BJP losing",], aes(x=date_gap_beginning, color=winning_party), size=1.5, bw=60)+labs(x="Days to MLA election", y="frequency of cow linching")+
  labs(x="Days to MLA election (beginning)", y="frequency of cow linching", title="Density of caw lynching on the elction timeline, by winner", caption = "kernal bandwidth: 60 days")+
  geom_vline(xintercept = 0, color="red", size=1.5)+
  scale_color_manual("MLA election",values=cols)+xlim(-365, 365)
ggsave("plots/summary/density of cow lynching on the election timeline, by winner1.jpg", width = 6, height = 5)

ggplot()+
  geom_density(data=lynchings_date[lynchings_date$winning_party=="BJP winning",], aes(x=date_gap_ending, color=winning_party), size=1.5, bw=60)+
  geom_density(data=lynchings_date[lynchings_date$winning_party=="BJP losing",], aes(x=date_gap_ending, color=winning_party), size=1.5, bw=60)+labs(x="Days to MLA election", y="frequency of cow linching")+
  labs(x="Days to MLA election (ending)", y="frequency of cow linching", title="Density of caw lynching on the elction timeline, by winner", caption = "kernal bandwidth: 60 days")+
  geom_vline(xintercept = 0, color="red", size=1.5)+
  scale_color_manual("MLA election",values=cols)+xlim(-365, 365)
ggsave("plots/summary/density of cow lynching on the election timeline, by winner2.jpg", width = 6, height = 5)

ggplot()+
  geom_density(data=lynchings_date[lynchings_date$winning_party=="BJP winning",], aes(x=date_gap_government, color=winning_party), size=1.5, bw=60)+
  geom_density(data=lynchings_date[lynchings_date$winning_party=="BJP losing",], aes(x=date_gap_government, color=winning_party), size=1.5, bw=60)+labs(x="Days to MLA election", y="frequency of cow linching")+
  labs(x="Days to MLA election (new government)", y="frequency of cow linching", title="Density of caw lynching on the elction timeline, by winner", caption = "kernal bandwidth: 60 days")+
  geom_vline(xintercept = 0, color="red", size=1.5)+
  scale_color_manual("MLA election",values=cols)+xlim(-365, 365)
ggsave("plots/summary/density of cow lynching on the election timeline, by winner3.jpg", width = 6, height = 5)


# get the previous winning party 
for (i in 1:nrow(lynchings_date)){
  if (lynchings_date$STATE[i]=="TELANGANA"){
  dif_vec_pre<-elections$date_begining[elections$STATE=="ANDHRA PRADESH" & elections$date_begining<lynchings_date$DATE[i]]-lynchings_date$DATE[i]
  winner_vec_pre<-elections$winner[elections$STATE=="ANDHRA PRADESH" & elections$date_begining<lynchings_date$DATE[i]]
  lynchings_date$winning_party_pre[i]<-winner_vec_pre[which.min(abs(dif_vec_pre))]
    
  }
  else{
  dif_vec_pre<-elections$date_begining[elections$STATE==lynchings_date$STATE[i] & elections$date_begining<lynchings_date$DATE[i]]-lynchings_date$DATE[i]
  winner_vec_pre<-elections$winner[elections$STATE==lynchings_date$STATE[i] & elections$date_begining<lynchings_date$DATE[i]]
  lynchings_date$winning_party_pre[i]<-winner_vec_pre[which.min(abs(dif_vec_pre))]
  }
}
lynchings_date$incumbent[lynchings_date$winning_party_pre=="BJP winning"]<-"BJP"
lynchings_date$incumbent[lynchings_date$winning_party_pre=="BJP losing"]<-"non-BJP"
