# Analysis 
library(xlsx)
library(ggplot2)
library(sf)
library(plyr)
library(dplyr)
library(tmap)
library(cobalt)
library(reshape2)
library(data.table)
library(zoo)
library(survival)
library(stargazer)
Sys.setlocale("LC_TIME", "English")
Sys.setenv(LANG = "en_US.UTF-8")
# summary statistics

# timeline
ggplot(data=lynchings_date_combined, aes(format(DATE, "%Y")))+geom_bar(stat="count")+labs(x="Year", y="Number of incidents", title="Temperal distribution of incidents\nrecorded by FactCheck.in")
ggsave("plots/summary/temporal_year.jpg")
sum(format(lynchings_date_2011_MLA$DATE, "%Y")==2017)
temporal<-data.frame(date=seq(as.Date("2012-1-1"), as.Date("2019-3-1"), by = "month"))
for (i in 1:nrow(temporal)){
  temporal$lynching_count[i]<-sum(format(lynchings_date_combined$DATE, format="%Y%m")==format(temporal$date[i], format="%Y%m"))
}
ggplot(temporal, aes(y=temporal$lynching_count, x=temporal$date))+geom_smooth(span=0.3, se = FALSE)+labs(x="time", y="number of incidents", title="Number of cow-related violence by time", caption = "span=0.3")
ggsave("plots/summary/temporal month.jpg", width = 6, height = 5)
temporal$month<-format(temporal$date, "%m")
summary(lm(formula = lynching_count~month, data = temporal))

# maps: state level
lynchings_STATE<-lynchings_date_combined%>%group_by(state_name)%>%dplyr::summarize(state_code=first(state_code), INCIDENTS=sum(INCIDENTS, na.rm=TRUE), VICTIMS=sum(VICTIMS, na.rm=TRUE), DEATHS=sum(DEATHS, na.rm=TRUE), INJURED=sum(Num_INJURED, na.rm=TRUE), ARRESTS=sum(ARRESTS_MADE, na.rm=TRUE))
map_State$NAME_1_up<-toupper(map_State$NAME_1)
map_State$NAME_1_up[map_State$NAME_1_up=="JAMMU AND KASHMIR"]<-"JAMMU & KASHMIR"
map_State$NAME_1_up[map_State$NAME_1_up=="DELHI"]<-"NCT OF DELHI"
map_State$NAME_1_up[map_State$NAME_1_up=="ORISSA"]<-"ODISHA"
lynchings_STATE<-merge(lynchings_STATE, population_state_total, by=c("state_code"), all.x = TRUE)
map_State_total<-merge(map_State,lynchings_STATE,  by.y="state_name", by.x="NAME_1_up", all.x = TRUE)
map_State_total$INCIDENTS<-as.numeric(as.character(map_State_total$INCIDENTS))
map_State_total$population_total<-as.numeric(as.character(map_State_total$population_total))
map_State_total$VICTIMS<-as.numeric(as.character(map_State_total$VICTIMS))
map_State_total$INCIDENTSPMC<-map_State_total$INCIDENTS/map_State_total$population_total*1000000
map_State_total$VICTIMSPMC<-map_State_total$VICTIMS/map_State_total$population_total*1000000
figure4_1_1<-tm_shape(map_State_total)+tm_polygons("INCIDENTS")+tm_borders(lwd=0.1, col="white")+tm_layout(title="Total numbers of cow-related violence", frame = FALSE, inner.margins=0.1, legend.position = c("LEFT", "BOTTOM"))
tmap_save(tm = figure4_1_1, filename = "plots/summary/4_1_1.jpg")
figure4_1_2<-tm_shape(map_State_total)+tm_polygons("VICTIMS")+tm_borders(lwd=0.1, col="white")+tm_layout(title="Total victims of cow-related violence", frame = FALSE, inner.margins=0.1, legend.position = c("LEFT", "BOTTOM"))
tmap_save(tm = figure4_1_2, filename = "plots/summary/4_1_2.jpg")
figure4_1_5<-tm_shape(map_State_total)+tm_polygons("INCIDENTSPMC")+tm_borders(lwd=0.1, col="white")+tm_layout(title="Numbers of cow-related violence\nper million residents", frame = FALSE, inner.margins=0.1, legend.position = c("LEFT", "BOTTOM"))
tmap_save(tm = figure4_1_5, filename = "plots/summary/4_1_5.jpg")
# maps: district level
map_District$NAME_1_up<-toupper(map_District$NAME_1)
map_District$NAME_2_up<-toupper(map_District$NAME_2)
map_District$NAME_1_up[map_District$NAME_1_up=="JAMMU AND KASHMIR"]<-"JAMMU & KASHMIR"
map_District$NAME_1_up[map_District$NAME_1_up=="DELHI"]<-"NCT OF DELHI"
map_District$NAME_1_up[map_District$NAME_1_up=="ORISSA"]<-"ODISHA"
lynchings_DISTRICT<-lynchings_date_2011_MLA%>%group_by(district_name)%>%dplyr::summarize(state_code=first(state_code), district_code=first(district_code), state_name=first(state_name), INCIDENTS=sum(INCIDENTS, na.rm=TRUE), VICTIMS=sum(VICTIMS, na.rm=TRUE), DEATHS=sum(DEATHS, na.rm=TRUE), INJURED=sum(Num_INJURED, na.rm=TRUE), ARRESTS=sum(ARRESTS_MADE, na.rm=TRUE))
lynchings_DISTRICT$district_name[lynchings_DISTRICT$district_name=="RANGAREDDY"]<-"RANGA REDDY"
lynchings_DISTRICT$state_name[lynchings_DISTRICT$district_name=="RANGAREDDY"]<-"ANDHRA PRADESH"
lynchings_DISTRICT$district_name[lynchings_DISTRICT$district_name=="NAVSARI  "]<-"NAVSARI"
lynchings_DISTRICT$district_name[lynchings_DISTRICT$district_name=="ANAND  "]<-"ANAND"
lynchings_DISTRICT$district_name[lynchings_DISTRICT$district_name=="GARHWA "]<-"GARHWA"
lynchings_DISTRICT$district_name[lynchings_DISTRICT$district_name=="JYOTIBA PHULE NAGAR"]<-"AMROHA"
lynchings_DISTRICT$district_name[lynchings_DISTRICT$district_name=="SOUTH TWENTY FOUR PARGANAS"]<-"NORTH 24 PARGANAS"
lynchings_DISTRICT$district_name[lynchings_DISTRICT$district_name=="JALPAIGURI "]<-"JALPAIGURI"
lynchings_DISTRICT<-merge(lynchings_DISTRICT, population_district_total, by=c("state_code", "district_code"), all.x = TRUE)
map_District_total<-merge(map_District,lynchings_DISTRICT,  by.y="district_name", by.x="NAME_2_up", all.x = TRUE)
map_District_total$INCIDENTSPMC<-map_District_total$INCIDENTS/map_District_total$population_total*1000000
map_District_total$VICTIMSPMC<-map_District_total$VICTIMS/map_District_total$population_total*1000000

figure4_1_3<-tm_shape(map_District_total)+tm_polygons("INCIDENTS",  palette = "Blues")+tm_borders(lwd=0.1, col="white")+tm_layout(title="Total numbers of cow-related violence", frame = FALSE, inner.margins=0.1, legend.position = c("LEFT", "BOTTOM"))
tmap_save(tm = figure4_1_3, filename = "plots/summary/4_1_3.jpg")
figure4_1_4<-tm_shape(map_District_total)+tm_polygons("VICTIMS",  palette = "Blues")+tm_borders(lwd=0.1, col="white")+tm_layout(title="Total victims of cow-related violence", frame = FALSE, inner.margins=0.1, legend.position = c("LEFT", "BOTTOM"))
tmap_save(tm = figure4_1_4, filename = "plots/summary/4_1_4.jpg")
map_District_total<-merge(map_District,lynchings_DISTRICT,  by.y="district_name", by.x="NAME_2_up", all.x = TRUE)
figure4_1_6<-tm_shape(map_District_total)+tm_polygons("INCIDENTSPMC",  palette = "Blues")+tm_borders(lwd=0.1, col="white")+tm_layout(title="Numbers of cow-related violence\nper million residents", frame = FALSE, inner.margins=0.1, legend.position = c("LEFT", "BOTTOM"))
tmap_save(tm = figure4_1_6, filename = "plots/summary/4_1_6.jpg")

# maps: constituency level
lynchings_AC<-lynchings_date_combined%>%group_by(state_name, MLA_ELECTORATE)%>%dplyr::summarize(state_code=first(state_code), district_name=first(district_name), INCIDENTS=sum(INCIDENTS, na.rm=TRUE), VICTIMS=sum(VICTIMS, na.rm=TRUE), DEATHS=sum(DEATHS, na.rm=TRUE), INJURED=sum(Num_INJURED, na.rm=TRUE), ARRESTS=sum(ARRESTS_MADE, na.rm=TRUE))
lynchings_AC$MLA_ELECTORATE%in%map_AC$AC_NAME1
map_AC_total<-merge(map_AC, lynchings_AC%>%select(-c("state_code")), by.x=c("ST_NAME", "AC_NAME"), by.y=c("state_name", "MLA_ELECTORATE"), all.x=TRUE)

figure4_1_7<-tm_shape(map_AC_total)+tm_polygons("INCIDENTS",  palette = "Reds", style="cat")+tm_borders(lwd=0.1, col="white")+tm_layout(title="Total numbers of cow-related violence", frame = FALSE, inner.margins=0.1, legend.position = c("LEFT", "BOTTOM"))
tmap_save(tm = figure4_1_7, filename = "plots/summary/4_1_7.jpg")
figure4_1_8<-tm_shape(map_AC_total)+tm_polygons("VICTIMS",  palette = "Reds", style="cat")+tm_borders(lwd=0.1, col="white")+tm_layout(title="Total victims of cow-related violence", frame = FALSE, inner.margins=0.1, legend.position = c("LEFT", "BOTTOM"))
tmap_save(tm = figure4_1_8, filename = "plots/summary/4_1_8.jpg")

st_write(map_AC_total, "maps-master/new/map_AC_combined.shp", driver="ESRI Shapefile")

# summary statistics
population_district_total<-population_district[population_district$residential_type=="Total",]
population_district_total$lynching<-population_district_total$district_code%in%lynchings_date_combined$district_code
population_district_total$population_Hindu_total<-as.numeric(as.character(population_district_total$population_Hindu_total))
population_district_total$population_total<-as.numeric(as.character(population_district_total$population_total))
population_district_total$Hindu_proportion<-population_district_total$population_Hindu_total/population_district_total$population_total
population_district_total$population_Muslim_total<-as.numeric(as.character(population_district_total$population_Muslim_total))
population_district_total$Muslim_proportion<-population_district_total$population_Muslim_total/population_district_total$population_total


t.test(population_district_total$Hindu_proportion~population_district_total$lynching)
t.test(population_district_total$Muslim_proportion~population_district_total$lynching)
t.test(population_district_total$attending_rate_total~population_district_total$lynching)
t.test(population_district_total$attending_rate_female~population_district_total$lynching)
t.test(population_district_total$equip_rate~population_district_total$lynching)
t.test(population_district_total$SC_rate~population_district_total$lynching)
t.test(population_district_total$ST_rate~population_district_total$lynching)
t.test(population_district_total$urb_rate~population_district_total$lynching)


mu <- plyr::ddply(population_district_total, "lynching", summarise, grp.mean=mean(Muslim_proportion, na.rm = TRUE))
ggplot(population_district_total, aes(x=Muslim_proportion, color=lynching)) +
  geom_density(size=1)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=lynching),
             linetype="dashed", size=1)+
  labs(x="Muslim population proportion", title="Lynching and Muslim population\ndistrict level")
ggsave("plots/summary/lynching_Muslim.jpg", width = 6, height = 5)

mu <- plyr::ddply(population_district_total, "lynching", summarise, grp.mean=mean(attending_rate_total, na.rm = TRUE))
ggplot(population_district_total, aes(x=attending_rate_total, color=lynching)) +
  geom_density(size=1)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=lynching),
             linetype="dashed", size=1)+
  labs(x="Proportion of 5-19 year-olds in school", title="lynching and education\ndistrict level")
ggsave("plots/summary/lynching_education.jpg", width = 6, height = 5)

mu <- ddply(population_district_total, "lynching", summarise, grp.mean=mean(SC_rate, na.rm = TRUE))
ggplot(population_district_total, aes(x=SC_rate, color=lynching)) +
  geom_density(size=1)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=lynching),
             linetype="dashed", size=1)+
  labs(x="Proportion of scheduled castes population", title="Lynching and caste\ndistrict level")
ggsave("plots/summary/lynching_caste.jpg", width = 6, height = 5)

mu <- ddply(population_district_total, "lynching", summarise, grp.mean=mean(urb_rate, na.rm = TRUE))
ggplot(population_district_total, aes(x=urb_rate, color=lynching)) +
  geom_density(size=1)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=lynching),
             linetype="dashed", size=1)+
  labs(x="Urbanization rate", title="lynching and urbanization\ndistrict level")
ggsave("plots/summary/lynching_urban.jpg", width = 6, height = 5)

mu <- ddply(population_district_total, "lynching", summarise, grp.mean=mean(ST_rate, na.rm = TRUE))
ggplot(population_district_total, aes(x=ST_rate, color=lynching)) +
  geom_density(size=1)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=lynching),
             linetype="dashed", size=1)+
  labs(x="Proportion of scheduled tribe population", title="Lynching and tribe\ndistrict level")
ggsave("plots/summary/lynching_tribe.jpg", width = 6, height = 5)

mu <- ddply(population_district_total, "lynching", summarise, grp.mean=mean(equip_rate, na.rm = TRUE))
ggplot(population_district_total, aes(x=equip_rate, color=lynching)) +
  geom_density(size=1)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=lynching),
             linetype="dashed", size=1)+
  labs(x="Proportion of households equipped with basic utilities", title="Lynching and household utilities\ndistrict level")
ggsave("plots/summary/lynching_utilities.jpg", width = 6, height = 5)

bal.tab(population_district_total%>%select(c("Hindu_proportion", "Muslim_proportion", "attending_rate_total", "attending_rate_female", "equip_rate")), treat=population_district_total$lynching)

# lynching by winning party at state level (not meaningful)
ggplot()+
  geom_density(data=lynchings_date_combined[lynchings_date_combined$incumbent=="BJP",], aes(x=date_gap_state_beginning, color=incumbent), size=1.5, bw=30)+
  geom_density(data=lynchings_date_combined[lynchings_date_combined$incumbent=="non-BJP",], aes(x=date_gap_state_beginning, color=incumbent), size=1.5, bw=30)+labs(x="Days to MLA election", y="frequency of cow linching")+
  labs(x="Days to beginning of MLA election", y="frequency of cow linching", title="Density of caw lynching on the elction timeline, by winner", caption = "kernal bandwidth: 30 days")+
  geom_vline(xintercept = 0, color="red", size=1.5)+
  xlim(-365*2, 365*2)+
  scale_color_manual("Incumbent party",values=cols_incumbent)+xlim(-365, 365)
ggsave("density of cow lynching on the election timeline, by incumbent party.pdf", width = 6, height = 5)
ggplot(data=lynchings_date)+geom_density(data=lynchings_date, aes(x=date_gap_beginning), size=1.5, bw=60)+labs(x="Days to MLA election", y="frequency of cow linching", title="Density of caw lynching on the elction timeline, total", caption = "kernal bandwidth: 60 days")+geom_vline(xintercept = 0, color="red", size=1.5)+xlim(-365*2, 365*2)
ggsave("density of cow lynching on the election timeline, total.pdf", width = 6, height = 5)
# lynching by winning party at electorate level
lynchings_date_combined$before_gap_beginning<-lynchings_date_combined$DATE-lynchings_date_combined$date_before_beginning
lynchings_date_combined$before_gap_ending<-lynchings_date_combined$DATE-lynchings_date_combined$date_before_ending
lynchings_date_combined$before_gap_government<-lynchings_date_combined$DATE-lynchings_date_combined$date_before_government

lynchings_date_combined$after_gap_beginning<-lynchings_date_combined$DATE-lynchings_date_combined$date_after_beginning
lynchings_date_combined$after_gap_ending<-lynchings_date_combined$DATE-lynchings_date_combined$date_after_ending
lynchings_date_combined$after_gap_government<-lynchings_date_combined$DATE-lynchings_date_combined$date_after_government

summary(as.numeric(lynchings_date_combined$after_gap_beginning-lynchings_date_combined$before_gap_beginning))

lynchings_date_combined$closest_gap_beginning[lynchings_date_combined$before_gap_beginning<=365*2 & is.na(lynchings_date_combined$before_gap_beginning)==FALSE]<-lynchings_date_combined$before_gap_beginning[lynchings_date_combined$before_gap_beginning<=365*2 & is.na(lynchings_date_combined$before_gap_beginning)==FALSE]
lynchings_date_combined$closest_gap_ending[lynchings_date_combined$before_gap_ending<=365*2 & is.na(lynchings_date_combined$before_gap_ending)==FALSE]<-lynchings_date_combined$before_gap_ending[lynchings_date_combined$before_gap_ending<=365*2 & is.na(lynchings_date_combined$before_gap_ending)==FALSE]
lynchings_date_combined$closest_gap_government[lynchings_date_combined$before_gap_government<=365*2 & is.na(lynchings_date_combined$before_gap_government)==FALSE]<-lynchings_date_combined$before_gap_government[lynchings_date_combined$before_gap_government<=365*2 & is.na(lynchings_date_combined$before_gap_government)==FALSE]

lynchings_date_combined$closest_gap_beginning[lynchings_date_combined$after_gap_beginning>=-365*2 & is.na(lynchings_date_combined$after_gap_beginning)==FALSE]<-lynchings_date_combined$after_gap_beginning[lynchings_date_combined$after_gap_beginning>=-365*2 & is.na(lynchings_date_combined$after_gap_beginning)==FALSE]
lynchings_date_combined$closest_gap_ending[lynchings_date_combined$after_gap_ending>=-365*2 & is.na(lynchings_date_combined$after_gap_ending)==FALSE]<-lynchings_date_combined$after_gap_ending[lynchings_date_combined$after_gap_ending>=-365*2 & is.na(lynchings_date_combined$after_gap_ending)==FALSE]
lynchings_date_combined$closest_gap_government[lynchings_date_combined$after_gap_government>=-365*2 & is.na(lynchings_date_combined$after_gap_government)==FALSE]<-lynchings_date_combined$after_gap_government[lynchings_date_combined$after_gap_government>=-365*2 & is.na(lynchings_date_combined$after_gap_government)==FALSE]

lynchings_date_combined$closest_winner[lynchings_date_combined$before_gap_beginning<=365*2 & is.na(lynchings_date_combined$before_gap_beginning)==FALSE]<-lynchings_date_combined$winner_party_before[lynchings_date_combined$before_gap_beginning<=365*2 & is.na(lynchings_date_combined$before_gap_beginning)==FALSE]
lynchings_date_combined$closest_winner[lynchings_date_combined$after_gap_beginning>=-365*2 & is.na(lynchings_date_combined$after_gap_beginning)==FALSE]<-lynchings_date_combined$winner_party_after[lynchings_date_combined$after_gap_beginning>=-365*2 & is.na(lynchings_date_combined$after_gap_beginning)==FALSE]

lynchings_date_combined$closest_BJP[lynchings_date_combined$closest_winner=="BJP"]<-"BJP"
lynchings_date_combined$closest_BJP[lynchings_date_combined$closest_winner!="BJP"]<-"non-BJP"

lynchings_date_combined$before_year<-as.numeric(lynchings_date_combined$before_gap_beginning)%/%365
lynchings_date_combined$after_year<-as.numeric(lynchings_date_combined$after_gap_beginning)%/%365
 
lynchings_date_combined$anchor[lynchings_date_combined$before_gap_beginning<=365*2 & is.na(lynchings_date_combined$before_gap_beginning)==FALSE]<-"before"
lynchings_date_combined$anchor[lynchings_date_combined$after_gap_beginning>=-365*2 & is.na(lynchings_date_combined$after_gap_beginning)==FALSE]<-"after"

ggplot(data=lynchings_date_combined[lynchings_date_combined$before_year<=5,], aes(x=before_year))+
  geom_bar()+
  labs(x="year to Vidhan Sabha election", y="number of cow-related violence", title="Distribution of cow-related after Vidhan Sabha elections")
ggsave("plots/summary/lynching_time_before.jpg", width = 6, height = 5)

ggplot(data=lynchings_date_combined[lynchings_date_combined$after_year>=-5,], aes(x=after_year))+
  geom_bar()+
  labs(x="year to Vidhan Sabha election", y="number of cow-related violence", title="Distribution of cow-related prior to Vidhan Sabha elections")
ggsave("plots/summary/lynching_time_after.jpg", width = 6, height = 5)

ggplot()+
  geom_density(data=lynchings_date_combined, aes(x=closest_gap_beginning), size=1.5, bw=30)+
  labs(x="Days to beginning of MLA election", y="frequency of cow linching", title="Density of cow-related violence on the elction timeline", caption = "kernal bandwidth: 30 days")+
  xlim(-400, 400)+
  geom_vline(xintercept = 0, color="red", size=1.5)
ggsave("plots/summary/lynching_time_beginning.jpg", width = 6, height = 5)

ggplot()+
  geom_density(data=lynchings_date_combined, aes(x=closest_gap_ending), size=1.5, bw=30)+
  labs(x="Days to end of MLA election", y="frequency of cow linching", title="Density of cow-related violence on the elction timeline", caption = "kernal bandwidth: 30 days")+
  xlim(-400, 400)+
  geom_vline(xintercept = 0, color="red", size=1.5)
ggsave("plots/summary/lynching_time_ending.jpg", width = 6, height = 5)

ggplot()+
  geom_density(data=lynchings_date_combined, aes(x=closest_gap_government), size=1.5, bw=30)+
  labs(x="Days to new state gov. formation", y="frequency of cow linching", title="Density of cow-related violence on the elction timeline", caption = "kernal bandwidth: 30 days")+
  xlim(-400, 400)+
  geom_vline(xintercept = 0, color="red", size=1.5)
ggsave("plots/summary/lynching_time_government.jpg", width = 6, height = 5)

ggplot()+
  geom_density(data=lynchings_date_combined[lynchings_date_combined$closest_winner=="BJP",], aes(x=closest_gap_beginning, color=closest_BJP), size=1.5, bw=30)+
  geom_density(data=lynchings_date_combined[lynchings_date_combined$closest_winner!="BJP",], aes(x=closest_gap_beginning, color=closest_BJP), size=1.5, bw=30)+
  labs(x="Days to beginning of MLA election", y="frequency of cow linching", title="Density of caw lynching on the elction timeline\nby electorate winner", caption = "kernal bandwidth: 30 days")+
  geom_vline(xintercept = 0, color="red", size=1.5)+
  scale_color_manual("Winning party",values=cols_incumbent)+xlim(-365, 365)
ggsave("plots/summary/lynching_winner_beginning.jpg", width = 6, height = 5)

ggplot()+
  geom_density(data=lynchings_date_combined[lynchings_date_combined$closest_winner=="BJP",], aes(x=closest_gap_ending, color=closest_BJP), size=1.5, bw=30)+
  geom_density(data=lynchings_date_combined[lynchings_date_combined$closest_winner!="BJP",], aes(x=closest_gap_ending, color=closest_BJP), size=1.5, bw=30)+
  labs(x="Days to end of MLA election", y="frequency of cow linching", title="Density of caw lynching on the elction timeline\nby electorate winner", caption = "kernal bandwidth: 30 days")+
  geom_vline(xintercept = 0, color="red", size=1.5)+
  scale_color_manual("Winning party",values=cols_incumbent)+xlim(-365, 365)
ggsave("plots/summary/lynching_winner_ending.jpg", width = 6, height = 5)

ggplot()+
  geom_density(data=lynchings_date_combined[lynchings_date_combined$closest_winner=="BJP",], aes(x=closest_gap_government, color=closest_BJP), size=1.5, bw=30)+
  geom_density(data=lynchings_date_combined[lynchings_date_combined$closest_winner!="BJP",], aes(x=closest_gap_government, color=closest_BJP), size=1.5, bw=30)+
  labs(x="Days to state gov. formation", y="frequency of cow linching", title="Density of caw lynching on the elction timeline\nby electorate winner", caption = "kernal bandwidth: 30 days")+
  geom_vline(xintercept = 0, color="red", size=1.5)+
  scale_color_manual("Winning party",values=cols_incumbent)+xlim(-365, 365)
ggsave("plots/summary/lynching_winner_government.jpg", width = 6, height = 5)

# four types of electorates
lynchings_date_combined$incumbent_pre<-NA
lynchings_date_combined$incumbent_pre[lynchings_date_combined$anchor=="before" & is.na(lynchings_date_combined$anchor)==FALSE]<-lynchings_date_combined$winner_party_before_two[lynchings_date_combined$anchor=="before" & is.na(lynchings_date_combined$anchor)==FALSE]
lynchings_date_combined$incumbent_pre[lynchings_date_combined$anchor=="after" & is.na(lynchings_date_combined$anchor)==FALSE]<-lynchings_date_combined$winner_party_before[lynchings_date_combined$anchor=="after" & is.na(lynchings_date_combined$anchor)==FALSE]

lynchings_date_combined$ruling_after[lynchings_date_combined$anchor=="before" & is.na(lynchings_date_combined$anchor)==FALSE]<-lynchings_date_combined$winner_party_before[lynchings_date_combined$anchor=="before" & is.na(lynchings_date_combined$anchor)==FALSE]
lynchings_date_combined$ruling_after[lynchings_date_combined$anchor=="after" & is.na(lynchings_date_combined$anchor)==FALSE]<-lynchings_date_combined$winner_party_after[lynchings_date_combined$anchor=="after" & is.na(lynchings_date_combined$anchor)==FALSE]

sum(lynchings_date_combined$incumbent_pre=="BJP" & lynchings_date_combined$ruling_after=="BJP" & lynchings_date_combined$before_gap_beginning<=365, na.rm = TRUE)
sum(lynchings_date_combined$incumbent_pre=="BJP" & lynchings_date_combined$ruling_after!="BJP" & lynchings_date_combined$before_gap_beginning<=365, na.rm = TRUE)
sum(lynchings_date_combined$incumbent_pre!="BJP" & lynchings_date_combined$ruling_after=="BJP" & lynchings_date_combined$before_gap_beginning<=365, na.rm = TRUE)
sum(lynchings_date_combined$incumbent_pre!="BJP" & lynchings_date_combined$ruling_after!="BJP" & lynchings_date_combined$before_gap_beginning<=365, na.rm = TRUE)

# get number of BJP and non-BJP seats to standardize data

electorates<-unique(rbind(unique(elections_06_18%>%select(c("state_01", "ac_nm"))), unique(elections_19_agr%>%select(c("state_01", "ac_nm")))))
electorates_list<-list()
time_series<-seq(from=as.Date("2012/6/1"), to=as.Date("2019/2/1"), by="month")
for (i in 1:nrow(electorates)){
  election_list1<-elections_06_18[elections_06_18$state_01==electorates$state_01[i] & elections_06_18$ac_nm==electorates$ac_nm[i],]%>%select(c("state_01", "ac_nm", "date", "party"))
  colnames(election_list1)<-c("state_01", "ac_nm", "date", "winning_party")
  election_list2<-elections_19_agr[elections_19_agr$state_01==electorates$state_01[i] & elections_19_agr$ac_nm==electorates$ac_nm[i],]%>%select(c("state_01", "ac_nm", "date", "winning_party"))
  elections_list<-unique(rbind(election_list1, election_list2))
  electorates_list[[i]]<-elections_list[order(elections_list$date),]
}

electorates_time<-matrix(, nrow=nrow(electorates), ncol=length(time_series))
for (i in 1:nrow(electorates_time)){
  election_list<-electorates_list[[i]]
  for (j in 1:ncol(electorates_time)){
    election_list_date<-election_list[election_list$date<=time_series[j],]
    electorates_time[i, j]<-ifelse(length(election_list_date$winning_party[which.max(election_list_date$date)])>0, election_list_date$winning_party[which.max(election_list_date$date)], NA)
  }
}
electorates_time_df<-as.data.frame(electorates_time)
colnames(electorates_time_df)<-time_series
colSums(electorates_time=="BJP", na.rm = TRUE)
elections_time<-data.frame(time=time_series)
elections_time$BJP_number<-colSums(electorates_time=="BJP", na.rm = TRUE)
elections_time$non_BJP_number<-colSums(electorates_time!="BJP", na.rm = TRUE)
elections_time$total_number<-colSums(is.na(electorates_time)==FALSE)
elections_time$BJP_prop<-elections_time$BJP_number/elections_time$total_number

ggplot(data=elections_time)+geom_line(aes(x=time, y=BJP_number), color="orange", size=2)+geom_line(aes(x=time, y=non_BJP_number), color="green", size=2)+
  ylim(0, 3500)+labs(title="Number of electorates by ruling party", y="number of electorates")
ggsave("plots/summary/election_time_1.jpg")

ggplot(data=elections_time)+geom_line(aes(x=time, y=BJP_prop), color="orange", size=2)+
  ylim(0, 0.5)+labs(title="Poropotion of BJP Vidhan Sabha seats", y="proportion")
ggsave("plots/summary/election_time_2.jpg")

lynchings_date_combined$month<-format(lynchings_date_combined$DATE, "%Y-%m")
elections_time$month<-format(elections_time$time, "%Y-%m")
lynchings_date_combined<-merge(lynchings_date_combined, elections_time%>%select(c("BJP_number", "non_BJP_number", "month")), by="month", all.x=TRUE)
lynchings_date_combined$winner_party_before_BJP[lynchings_date_combined$winner_party_before=="BJP"]<-"BJP"
lynchings_date_combined$winner_party_before_BJP[lynchings_date_combined$winner_party_before!="BJP" & is.na(lynchings_date_combined$winner_party_before)==FALSE]<-"non_BJP"
lynchings_date_combined$winner_party_recent_BJP[lynchings_date_combined$anchor=="before" & lynchings_date_combined$winner_party_before=="BJP"]<-"BJP"
lynchings_date_combined$winner_party_recent_BJP[lynchings_date_combined$anchor=="before" & lynchings_date_combined$winner_party_before!="BJP"]<-"non_BJP"
lynchings_date_combined$winner_party_recent_BJP[lynchings_date_combined$anchor=="after" & lynchings_date_combined$winner_party_after=="BJP"]<-"BJP"
lynchings_date_combined$winner_party_recent_BJP[lynchings_date_combined$anchor=="after" & lynchings_date_combined$winner_party_after!="BJP"]<-"non_BJP"


lynchings_month<-lynchings_date_combined%>%group_by(month, winner_party_before_BJP)%>%summarise(total_incidents=n(), total_victims=sum(VICTIMS, na.rm=TRUE))        
lynchings_month<-lynchings_month[is.na(lynchings_month$winner_party_before_BJP)==FALSE,]
colnames(lynchings_month)<-c("month", "party", "total_incidents", "total_victims")
elections_time_BJP<-elections_time%>%select(c("month", "BJP_number"))
colnames(elections_time_BJP)<-c("month", "constituency_number")
elections_time_BJP$party<-"BJP"
elections_time_non_BJP<-elections_time%>%select(c("month", "non_BJP_number"))
colnames(elections_time_non_BJP)<-c("month", "constituency_number")
elections_time_non_BJP$party<-"non_BJP"
elections_time_combined<-rbind(elections_time_BJP, elections_time_non_BJP)
elections_time_combined<-merge(elections_time_combined, lynchings_month, by=c("month", "party"), all.x=TRUE)
elections_time_combined$VD<-elections_time_combined$total_incidents/elections_time_combined$constituency_number
elections_time_combined$VD[is.na(elections_time_combined$VD)==TRUE]<-0
elections_time_combined$time<-as.Date(paste0(elections_time_combined$month, "-01"), "%Y-%m-%d")

breaks<-time_series[3*(1:27)]
violence_binned<-data.frame(start=breaks[1:26], end=breaks[2:27])

for (i in 1:26){
  violence_binned$VD_BJP[i]<-sum(elections_time_combined$VD[elections_time_combined$party=="BJP"& elections_time_combined$time>=violence_binned$start[i] & elections_time_combined$time<violence_binned$end[i]], na.rm = TRUE)/3
  violence_binned$VD_non_BJP[i]<-sum(elections_time_combined$VD[elections_time_combined$party=="non_BJP"& elections_time_combined$time>=violence_binned$start[i] & elections_time_combined$time<violence_binned$end[i]], na.rm = TRUE)/3
  }
violence_binned$mid<-violence_binned$start+floor(violence_binned$start-violence_binned$end)/2
violence_binned<-reshape(violence_binned, direction="long", varying=c("VD_BJP", "VD_non_BJP"), v.names = "VD", idvar = c("start", "end", "mid"), timevar = "party", times = c("BJP", "non_BJP"))

cols_incumbent <- c("BJP" = "orange", "non_BJP" = "green")
ggplot(data=violence_binned, aes(x=mid, y=VD, fill=party)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_manual("ruling party",values=cols_incumbent)+
  labs(x="time", y="number of incidents per constituency per month", title="Prevalence of cow-related violence\nin BJP- and non-BJP-ruled constituencies\n(three-month average)")
ggsave("plots/analyses/stand_party2.jpg")

cols_incumbent <- c("BJP" = "orange", "non_BJP" = "green")
ggplot(elections_time_combined, aes(x=time, y=VD, color=party))+
  geom_point(data=elections_time_combined[elections_time_combined$VD>0,])+
  geom_smooth()+
  labs(x="time", y="number of incidents per constituency per month", title="prevalence of cow-related violence\nin BJP- and non-BJP-ruled constituencies")+
  scale_color_manual("ruling party",values=cols_incumbent)
 ggsave("plots/analyses/stand_party.jpg")

lynchings_date_combined<-merge(lynchings_date_combined, elections_time_combined%>%select("month", "party", "constituency_number"), by.x=c("month", "winner_party_recent_BJP"), by.y=c("month", "party"), all.x=TRUE)
lynchings_date_combined$closest_gap_month_beginning<-lynchings_date_combined$closest_gap_beginning%/%30
lynchings_date_combined$closest_gap_month_ending<-lynchings_date_combined$closest_gap_ending%/%30
lynchings_date_combined$closest_gap_month_government<-lynchings_date_combined$closest_gap_government%/%30

lynchings_date_combined_elections1<-data.frame(month_gap=c(-24:24, -24:24)
                                                ,winner_party=c(rep("BJP", 49), rep("non_BJP", 49)))
lynchings_date_combined_elections2_beginning<-lynchings_date_combined%>%group_by(closest_gap_month_beginning, winner_party_recent_BJP)%>%summarise(sum_VD=sum(1/constituency_number, na.rm=TRUE))
lynchings_date_combined_elections2_ending<-lynchings_date_combined%>%group_by(closest_gap_month_ending, winner_party_recent_BJP)%>%summarise(sum_VD=sum(1/constituency_number, na.rm=TRUE))
lynchings_date_combined_elections2_government<-lynchings_date_combined%>%group_by(closest_gap_month_government, winner_party_recent_BJP)%>%summarise(sum_VD=sum(1/constituency_number, na.rm=TRUE))

lynchings_date_combined_elections<-merge(lynchings_date_combined_elections1, lynchings_date_combined_elections2_beginning, by.x=c("month_gap", "winner_party"), by.y=c("closest_gap_month_beginning", "winner_party_recent_BJP"), all.x = TRUE)
lynchings_date_combined_elections$sum_VD_beginning<-lynchings_date_combined_elections$sum_VD
lynchings_date_combined_elections$sum_VD<-NULL

lynchings_date_combined_elections<-merge(lynchings_date_combined_elections, lynchings_date_combined_elections2_ending, by.x=c("month_gap", "winner_party"), by.y=c("closest_gap_month_ending", "winner_party_recent_BJP"), all.x = TRUE)
lynchings_date_combined_elections$sum_VD_ending<-lynchings_date_combined_elections$sum_VD
lynchings_date_combined_elections$sum_VD<-NULL

lynchings_date_combined_elections<-merge(lynchings_date_combined_elections, lynchings_date_combined_elections2_government, by.x=c("month_gap", "winner_party"), by.y=c("closest_gap_month_government", "winner_party_recent_BJP"), all.x = TRUE)
lynchings_date_combined_elections$sum_VD_government<-lynchings_date_combined_elections$sum_VD
lynchings_date_combined_elections$sum_VD<-NULL

lynchings_date_combined_elections$sum_VD_beginning[is.na(lynchings_date_combined_elections$sum_VD_beginning)==TRUE]<-0
lynchings_date_combined_elections$sum_VD_ending[is.na(lynchings_date_combined_elections$sum_VD_ending)==TRUE]<-0
lynchings_date_combined_elections$sum_VD_government[is.na(lynchings_date_combined_elections$sum_VD_government)==TRUE]<-0


ggplot(data=lynchings_date_combined_elections, aes(x=month_gap, y=sum_VD_beginning, color=winner_party))+
  geom_point(data=lynchings_date_combined_elections[lynchings_date_combined_elections$sum_VD_beginning>0,])+
  geom_smooth(data=lynchings_date_combined_elections, span=1)+
  ylim(0, 0.002)+xlim(-22, 22)+
  labs(x="month to beginning of election", y="number of incidents per constituency per month", title="Prevalence of cow-related violence\nin BJP- and non-BJP-ruled constituencies")+
  scale_color_manual("winning party in election",values=cols_incumbent)+geom_vline(xintercept=0, color="red")
ggsave("plots/analyses/stand_party3_0_beginning.jpg", width =6 )

ggplot(data=lynchings_date_combined_elections, aes(x=month_gap, y=sum_VD_ending, color=winner_party))+
  geom_point(data=lynchings_date_combined_elections[lynchings_date_combined_elections$sum_VD_ending>0,])+
  geom_smooth(data=lynchings_date_combined_elections, span=1)+
  ylim(0, 0.002)+xlim(-22, 22)+
  labs(x="month to end of election", y="number of incidents per constituency per month", title="Prevalence of cow-related violence\nin BJP- and non-BJP-ruled constituencies")+
  scale_color_manual("winning party in election",values=cols_incumbent)+geom_vline(xintercept=0, color="red")
ggsave("plots/analyses/stand_party3_0_ending.jpg", width =6 )

ggplot(data=lynchings_date_combined_elections, aes(x=month_gap, y=sum_VD_government, color=winner_party))+
  geom_point(data=lynchings_date_combined_elections[lynchings_date_combined_elections$sum_VD_government>0,])+
  geom_smooth(data=lynchings_date_combined_elections, span=1)+
  ylim(0, 0.002)+xlim(-22, 22)+
  labs(x="month to end of election", y="number of incidents per constituency per month", title="Prevalence of cow-related violence\nin BJP- and non-BJP-ruled constituencies")+
  scale_color_manual("winning party in election",values=cols_incumbent)+geom_vline(xintercept=0, color="red")
ggsave("plots/analyses/stand_party3_0_government.jpg", width =6 )

ggplot(data=lynchings_date_combined_elections, aes(x=month_gap, y=sum_VD_beginning, color=winner_party))+
  geom_point(data=lynchings_date_combined_elections[lynchings_date_combined_elections$sum_VD_beginning>0,])+
  geom_smooth(data=lynchings_date_combined_elections[lynchings_date_combined_elections$month_gap<0,], span=1)+
  geom_smooth(data=lynchings_date_combined_elections[lynchings_date_combined_elections$month_gap>0,], span=1)+
  ylim(0, 0.002)+xlim(-20, 22)+
  labs(x="month to beginning of election", y="number of incidents per constituency per month", title="Prevalence of cow-related violence\nin BJP- and non-BJP-ruled constituencies")+
  scale_color_manual("winning party in election",values=cols_incumbent)+geom_vline(xintercept=0, color="red")
ggsave("plots/analyses/stand_party3_beginning.jpg", width =6 )

ggplot(data=lynchings_date_combined_elections, aes(x=month_gap, y=sum_VD_ending, color=winner_party))+
  geom_point(data=lynchings_date_combined_elections[lynchings_date_combined_elections$sum_VD_ending>0,])+
  geom_smooth(data=lynchings_date_combined_elections[lynchings_date_combined_elections$month_gap<0,], span=1)+
  geom_smooth(data=lynchings_date_combined_elections[lynchings_date_combined_elections$month_gap>0,], span=1)+
  ylim(0, 0.002)+xlim(-20, 22)+
  labs(x="month to end of election", y="number of incidents per constituency per month", title="Prevalence of cow-related violence\nin BJP- and non-BJP-ruled constituencies")+
  scale_color_manual("winning party in election",values=cols_incumbent)+geom_vline(xintercept=0, color="red")
ggsave("plots/analyses/stand_party3_ending.jpg", width =6 )

ggplot(data=lynchings_date_combined_elections, aes(x=month_gap, y=sum_VD_government, color=winner_party))+
  geom_point(data=lynchings_date_combined_elections[lynchings_date_combined_elections$sum_VD_government>0,])+
  geom_smooth(data=lynchings_date_combined_elections[lynchings_date_combined_elections$month_gap<0,], span=1)+
  geom_smooth(data=lynchings_date_combined_elections[lynchings_date_combined_elections$month_gap>0,], span=1)+
  ylim(0, 0.002)+xlim(-20, 22)+
  labs(x="month to government formation", y="number of incidents per constituency per month", title="Prevalence of cow-related violence\nin BJP- and non-BJP-ruled constituencies")+
  scale_color_manual("winning party in election",values=cols_incumbent)+geom_vline(xintercept=0, color="red")
ggsave("plots/analyses/stand_party3_government.jpg", width =6 )

lynchings_date_combined$BJP_relative_before<-NA
lynchings_date_combined$BJP_relative_before[lynchings_date_combined$winner_party_before=="BJP" & is.na(lynchings_date_combined$winner_party_before)==FALSE]<-(lynchings_date_combined$winner_votes_before/lynchings_date_combined$party2_votes_before)[lynchings_date_combined$winner_party_before=="BJP"& is.na(lynchings_date_combined$winner_party_before)==FALSE]
lynchings_date_combined$BJP_relative_before[lynchings_date_combined$party2_party_before=="BJP" & is.na(lynchings_date_combined$party2_party_before)==FALSE]<-(lynchings_date_combined$party2_votes_before/lynchings_date_combined$winner_votes_before)[lynchings_date_combined$party2_party_before=="BJP" & is.na(lynchings_date_combined$party2_party_before)==FALSE]
lynchings_date_combined$close_before<-NA
lynchings_date_combined$close_before[log(lynchings_date_combined$BJP_relative_before)>-0.2 & log(lynchings_date_combined$BJP_relative_before)<0.2]<-TRUE
lynchings_date_combined$close_before[log(lynchings_date_combined$BJP_relative_before)<=-0.2 | log(lynchings_date_combined$BJP_relative_before)>=0.2]<-FALSE

lynchings_date_combined$BJP_relative_after<-NA
lynchings_date_combined$BJP_relative_after[lynchings_date_combined$winner_party_after=="BJP" & is.na(lynchings_date_combined$winner_party_after)==FALSE]<-(lynchings_date_combined$winner_votes_after/lynchings_date_combined$party2_votes_after)[lynchings_date_combined$winner_party_after=="BJP"& is.na(lynchings_date_combined$winner_party_after)==FALSE]
lynchings_date_combined$BJP_relative_after[lynchings_date_combined$party2_party_after=="BJP" & is.na(lynchings_date_combined$party2_party_after)==FALSE]<-(lynchings_date_combined$party2_votes_after/lynchings_date_combined$winner_votes_after)[lynchings_date_combined$party2_party_after=="BJP" & is.na(lynchings_date_combined$party2_party_after)==FALSE]
lynchings_date_combined$close_after<-NA
lynchings_date_combined$close_after[log(lynchings_date_combined$BJP_relative_after)>-0.2 & log(lynchings_date_combined$BJP_relative_after)<0.2]<-TRUE
lynchings_date_combined$close_after[log(lynchings_date_combined$BJP_relative_after)<=-0.2 | log(lynchings_date_combined$BJP_relative_after)>=0.2]<-FALSE

lynchings_date_combined$close_recent[lynchings_date_combined$anchor=="before" & is.na(lynchings_date_combined$anchor)==FALSE]<-lynchings_date_combined$close_before[lynchings_date_combined$anchor=="before" & is.na(lynchings_date_combined$anchor)==FALSE]
lynchings_date_combined$close_recent[lynchings_date_combined$anchor=="after" & is.na(lynchings_date_combined$anchor)==FALSE]<-lynchings_date_combined$close_after[lynchings_date_combined$anchor=="after" & is.na(lynchings_date_combined$anchor)==FALSE]

ggplot(data=violence_binned, aes(x=mid, y=VD, fill=party)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_manual("ruling party",values=cols_incumbent)+
  labs(x="time", y="number of incidents per constituency per month", title="prevalence of cow-related violence\nin BJP- and non-BJP-ruled constituencies\n(three-month average)")
ggsave("plots/analyses/stand_party2.jpg")

lynchings_date_combined_elections2_close_beginning<-lynchings_date_combined[lynchings_date_combined$close_recent==TRUE,]%>%group_by(closest_gap_month_beginning, winner_party_recent_BJP)%>%summarise(sum_VD_beginning=sum(1/constituency_number, na.rm=TRUE))
lynchings_date_combined_elections2_close_ending<-lynchings_date_combined[lynchings_date_combined$close_recent==TRUE,]%>%group_by(closest_gap_month_ending, winner_party_recent_BJP)%>%summarise(sum_VD_ending=sum(1/constituency_number, na.rm=TRUE))
lynchings_date_combined_elections2_close_government<-lynchings_date_combined[lynchings_date_combined$close_recent==TRUE,]%>%group_by(closest_gap_month_government, winner_party_recent_BJP)%>%summarise(sum_VD_government=sum(1/constituency_number, na.rm=TRUE))

lynchings_date_combined_elections_close<-merge(lynchings_date_combined_elections1, lynchings_date_combined_elections2_close_beginning, by.x=c("month_gap", "winner_party"), by.y=c("closest_gap_month_beginning", "winner_party_recent_BJP"), all.x = TRUE)
lynchings_date_combined_elections_close<-merge(lynchings_date_combined_elections_close, lynchings_date_combined_elections2_close_ending, by.x=c("month_gap", "winner_party"), by.y=c("closest_gap_month_ending", "winner_party_recent_BJP"), all.x = TRUE)
lynchings_date_combined_elections_close<-merge(lynchings_date_combined_elections_close, lynchings_date_combined_elections2_close_government, by.x=c("month_gap", "winner_party"), by.y=c("closest_gap_month_government", "winner_party_recent_BJP"), all.x = TRUE)

lynchings_date_combined_elections_close$sum_VD_beginning[is.na(lynchings_date_combined_elections_close$sum_VD_beginning)==TRUE]<-0
lynchings_date_combined_elections_close$sum_VD_ending[is.na(lynchings_date_combined_elections_close$sum_VD_ending)==TRUE]<-0
lynchings_date_combined_elections_close$sum_VD_government[is.na(lynchings_date_combined_elections_close$sum_VD_government)==TRUE]<-0

lynchings_date_combined_elections2_non_close_beginning<-lynchings_date_combined[lynchings_date_combined$close_recent==FALSE,]%>%group_by(closest_gap_month_beginning, winner_party_recent_BJP)%>%summarise(sum_VD_beginning=sum(1/constituency_number, na.rm=TRUE))
lynchings_date_combined_elections2_non_close_ending<-lynchings_date_combined[lynchings_date_combined$close_recent==FALSE,]%>%group_by(closest_gap_month_ending, winner_party_recent_BJP)%>%summarise(sum_VD_ending=sum(1/constituency_number, na.rm=TRUE))
lynchings_date_combined_elections2_non_close_government<-lynchings_date_combined[lynchings_date_combined$close_recent==FALSE,]%>%group_by(closest_gap_month_government, winner_party_recent_BJP)%>%summarise(sum_VD_government=sum(1/constituency_number, na.rm=TRUE))

lynchings_date_combined_elections_non_close<-merge(lynchings_date_combined_elections1, lynchings_date_combined_elections2_non_close_beginning, by.x=c("month_gap", "winner_party"), by.y=c("closest_gap_month_beginning", "winner_party_recent_BJP"), all.x = TRUE)
lynchings_date_combined_elections_non_close<-merge(lynchings_date_combined_elections_non_close, lynchings_date_combined_elections2_non_close_ending, by.x=c("month_gap", "winner_party"), by.y=c("closest_gap_month_ending", "winner_party_recent_BJP"), all.x = TRUE)
lynchings_date_combined_elections_non_close<-merge(lynchings_date_combined_elections_non_close, lynchings_date_combined_elections2_non_close_government, by.x=c("month_gap", "winner_party"), by.y=c("closest_gap_month_government", "winner_party_recent_BJP"), all.x = TRUE)

lynchings_date_combined_elections_non_close$sum_VD_beginning[is.na(lynchings_date_combined_elections_non_close$sum_VD_beginning)==TRUE]<-0
lynchings_date_combined_elections_non_close$sum_VD_ending[is.na(lynchings_date_combined_elections_non_close$sum_VD_ending)==TRUE]<-0
lynchings_date_combined_elections_non_close$sum_VD_government[is.na(lynchings_date_combined_elections_non_close$sum_VD_government)==TRUE]<-0


lynchings_date_combined_elections2_pre_BJP_beginning<-lynchings_date_combined[(lynchings_date_combined$anchor=="before" & lynchings_date_combined$winner_party_before_two=="BJP" & is.na(lynchings_date_combined$anchor)==FALSE & is.na(lynchings_date_combined$winner_party_before_two)==FALSE) | (lynchings_date_combined$anchor=="after" & lynchings_date_combined$winner_party_before=="BJP" & is.na(lynchings_date_combined$anchor)==FALSE & is.na(lynchings_date_combined$winner_party_before)==FALSE),]%>%group_by(closest_gap_month_beginning, winner_party_recent_BJP)%>%summarise(sum_VD_beginning=sum(1/constituency_number, na.rm=TRUE))
lynchings_date_combined_elections2_pre_BJP_ending<-lynchings_date_combined[(lynchings_date_combined$anchor=="before" & lynchings_date_combined$winner_party_before_two=="BJP" & is.na(lynchings_date_combined$anchor)==FALSE & is.na(lynchings_date_combined$winner_party_before_two)==FALSE) | (lynchings_date_combined$anchor=="after" & lynchings_date_combined$winner_party_before=="BJP" & is.na(lynchings_date_combined$anchor)==FALSE & is.na(lynchings_date_combined$winner_party_before)==FALSE),]%>%group_by(closest_gap_month_ending, winner_party_recent_BJP)%>%summarise(sum_VD_ending=sum(1/constituency_number, na.rm=TRUE))
lynchings_date_combined_elections2_pre_BJP_government<-lynchings_date_combined[(lynchings_date_combined$anchor=="before" & lynchings_date_combined$winner_party_before_two=="BJP" & is.na(lynchings_date_combined$anchor)==FALSE & is.na(lynchings_date_combined$winner_party_before_two)==FALSE) | (lynchings_date_combined$anchor=="after" & lynchings_date_combined$winner_party_before=="BJP" & is.na(lynchings_date_combined$anchor)==FALSE & is.na(lynchings_date_combined$winner_party_before)==FALSE),]%>%group_by(closest_gap_month_government, winner_party_recent_BJP)%>%summarise(sum_VD_government=sum(1/constituency_number, na.rm=TRUE))

lynchings_date_combined_elections_pre_BJP<-merge(lynchings_date_combined_elections1, lynchings_date_combined_elections2_pre_BJP_beginning, by.x=c("month_gap", "winner_party"), by.y=c("closest_gap_month_beginning", "winner_party_recent_BJP"), all.x = TRUE)
lynchings_date_combined_elections_pre_BJP<-merge(lynchings_date_combined_elections_pre_BJP, lynchings_date_combined_elections2_pre_BJP_ending, by.x=c("month_gap", "winner_party"), by.y=c("closest_gap_month_ending", "winner_party_recent_BJP"), all.x = TRUE)
lynchings_date_combined_elections_pre_BJP<-merge(lynchings_date_combined_elections_pre_BJP, lynchings_date_combined_elections2_pre_BJP_government, by.x=c("month_gap", "winner_party"), by.y=c("closest_gap_month_government", "winner_party_recent_BJP"), all.x = TRUE)

lynchings_date_combined_elections_pre_BJP$sum_VD_beginning[is.na(lynchings_date_combined_elections_pre_BJP$sum_VD_beginning)==TRUE]<-0
lynchings_date_combined_elections_pre_BJP$sum_VD_ending[is.na(lynchings_date_combined_elections_pre_BJP$sum_VD_ending)==TRUE]<-0
lynchings_date_combined_elections_pre_BJP$sum_VD_government[is.na(lynchings_date_combined_elections_pre_BJP$sum_VD_government)==TRUE]<-0

lynchings_date_combined_elections2_pre_non_BJP_beginning<-lynchings_date_combined[(lynchings_date_combined$anchor=="before" & lynchings_date_combined$winner_party_before_two!="BJP" & is.na(lynchings_date_combined$anchor)==FALSE & is.na(lynchings_date_combined$winner_party_before_two)==FALSE) | (lynchings_date_combined$anchor=="after" & lynchings_date_combined$winner_party_before!="BJP" & is.na(lynchings_date_combined$anchor)==FALSE & is.na(lynchings_date_combined$winner_party_before)==FALSE),]%>%group_by(closest_gap_month_beginning, winner_party_recent_BJP)%>%summarise(sum_VD_beginning=sum(1/constituency_number, na.rm=TRUE))
lynchings_date_combined_elections2_pre_non_BJP_ending<-lynchings_date_combined[(lynchings_date_combined$anchor=="before" & lynchings_date_combined$winner_party_before_two!="BJP" & is.na(lynchings_date_combined$anchor)==FALSE & is.na(lynchings_date_combined$winner_party_before_two)==FALSE) | (lynchings_date_combined$anchor=="after" & lynchings_date_combined$winner_party_before!="BJP" & is.na(lynchings_date_combined$anchor)==FALSE & is.na(lynchings_date_combined$winner_party_before)==FALSE),]%>%group_by(closest_gap_month_ending, winner_party_recent_BJP)%>%summarise(sum_VD_ending=sum(1/constituency_number, na.rm=TRUE))
lynchings_date_combined_elections2_pre_non_BJP_government<-lynchings_date_combined[(lynchings_date_combined$anchor=="before" & lynchings_date_combined$winner_party_before_two!="BJP" & is.na(lynchings_date_combined$anchor)==FALSE & is.na(lynchings_date_combined$winner_party_before_two)==FALSE) | (lynchings_date_combined$anchor=="after" & lynchings_date_combined$winner_party_before!="BJP" & is.na(lynchings_date_combined$anchor)==FALSE & is.na(lynchings_date_combined$winner_party_before)==FALSE),]%>%group_by(closest_gap_month_government, winner_party_recent_BJP)%>%summarise(sum_VD_government=sum(1/constituency_number, na.rm=TRUE))

lynchings_date_combined_elections_pre_non_BJP<-merge(lynchings_date_combined_elections1, lynchings_date_combined_elections2_pre_non_BJP_beginning, by.x=c("month_gap", "winner_party"), by.y=c("closest_gap_month_beginning", "winner_party_recent_BJP"), all.x = TRUE)
lynchings_date_combined_elections_pre_non_BJP<-merge(lynchings_date_combined_elections_pre_non_BJP, lynchings_date_combined_elections2_pre_non_BJP_ending, by.x=c("month_gap", "winner_party"), by.y=c("closest_gap_month_ending", "winner_party_recent_BJP"), all.x = TRUE)
lynchings_date_combined_elections_pre_non_BJP<-merge(lynchings_date_combined_elections_pre_non_BJP, lynchings_date_combined_elections2_pre_non_BJP_government, by.x=c("month_gap", "winner_party"), by.y=c("closest_gap_month_government", "winner_party_recent_BJP"), all.x = TRUE)

lynchings_date_combined_elections_pre_non_BJP$sum_VD_beginning[is.na(lynchings_date_combined_elections_pre_non_BJP$sum_VD_beginning)==TRUE]<-0
lynchings_date_combined_elections_pre_non_BJP$sum_VD_ending[is.na(lynchings_date_combined_elections_pre_non_BJP$sum_VD_ending)==TRUE]<-0
lynchings_date_combined_elections_pre_non_BJP$sum_VD_government[is.na(lynchings_date_combined_elections_pre_non_BJP$sum_VD_government)==TRUE]<-0

ggplot(data=lynchings_date_combined_elections_close, aes(x=month_gap, y=sum_VD_beginning, color=winner_party))+
  geom_point(data=lynchings_date_combined_elections_close[lynchings_date_combined_elections_close$sum_VD_beginning>0,])+
  geom_smooth(data=lynchings_date_combined_elections_close[lynchings_date_combined_elections_close$month_gap<0,], span=2)+
  geom_smooth(data=lynchings_date_combined_elections_close[lynchings_date_combined_elections_close$month_gap>0,], span=2)+
  ylim(0, 0.002)+xlim(-22, 22)+
  labs(x="month to beginning of election", y="number of incidents per constituency per month", title="Prevalence of cow-related violence (close election)\nin BJP- and non-BJP-ruled constituencies")+
  scale_color_manual("winning party in election",values=cols_incumbent)+geom_vline(xintercept=0, color="red")
ggsave("plots/analyses/stand_party3_1_beginning.jpg", width =6 )

ggplot(data=lynchings_date_combined_elections_close, aes(x=month_gap, y=sum_VD_ending, color=winner_party))+
  geom_point(data=lynchings_date_combined_elections_close[lynchings_date_combined_elections_close$sum_VD_ending>0,])+
  geom_smooth(data=lynchings_date_combined_elections_close[lynchings_date_combined_elections_close$month_gap<0,], span=2)+
  geom_smooth(data=lynchings_date_combined_elections_close[lynchings_date_combined_elections_close$month_gap>0,], span=2)+
  ylim(0, 0.002)+xlim(-22, 22)+
  labs(x="month to end of election", y="number of incidents per constituency per month", title="Prevalence of cow-related violence (close election)\nin BJP- and non-BJP-ruled constituencies")+
  scale_color_manual("winning party in election",values=cols_incumbent)+geom_vline(xintercept=0, color="red")
ggsave("plots/analyses/stand_party3_1_ending.jpg", width =6 )

ggplot(data=lynchings_date_combined_elections_close, aes(x=month_gap, y=sum_VD_government, color=winner_party))+
  geom_point(data=lynchings_date_combined_elections_close[lynchings_date_combined_elections_close$sum_VD_government>0,])+
  geom_smooth(data=lynchings_date_combined_elections_close[lynchings_date_combined_elections_close$month_gap<0,], span=2)+
  geom_smooth(data=lynchings_date_combined_elections_close[lynchings_date_combined_elections_close$month_gap>0,], span=2)+
  ylim(0, 0.002)+xlim(-22, 22)+
  labs(x="month to government formation", y="number of incidents per constituency per month", title="Prevalence of cow-related violence (close election)\nin BJP- and non-BJP-ruled constituencies")+
  scale_color_manual("winning party in election",values=cols_incumbent)+geom_vline(xintercept=0, color="red")
ggsave("plots/analyses/stand_party3_1_government.jpg", width =6 )

ggplot(data=lynchings_date_combined_elections_non_close, aes(x=month_gap, y=sum_VD_beginning, color=winner_party))+
  geom_point(data=lynchings_date_combined_elections_non_close[lynchings_date_combined_elections_non_close$sum_VD_beginning>0,])+
  geom_smooth(data=lynchings_date_combined_elections_non_close[lynchings_date_combined_elections_non_close$month_gap<0,], span=2)+
  geom_smooth(data=lynchings_date_combined_elections_non_close[lynchings_date_combined_elections_non_close$month_gap>0,], span=2)+
  ylim(0, 0.002)+xlim(-22, 22)+
  labs(x="month to beginning of election", y="number of incidents per constituency per month", title="Prevalence of cow-related violence (non-close election)\nin BJP- and non-BJP-ruled constituencies")+
  scale_color_manual("winning party in election",values=cols_incumbent)+geom_vline(xintercept=0, color="red")
ggsave("plots/analyses/stand_party3_2_beginning.jpg", width =6 )

ggplot(data=lynchings_date_combined_elections_non_close, aes(x=month_gap, y=sum_VD_ending, color=winner_party))+
  geom_point(data=lynchings_date_combined_elections_non_close[lynchings_date_combined_elections_non_close$sum_VD_ending>0,])+
  geom_smooth(data=lynchings_date_combined_elections_non_close[lynchings_date_combined_elections_non_close$month_gap<0,], span=2)+
  geom_smooth(data=lynchings_date_combined_elections_non_close[lynchings_date_combined_elections_non_close$month_gap>0,], span=2)+
  ylim(0, 0.002)+xlim(-22, 22)+
  labs(x="month to end of election", y="number of incidents per constituency per month", title="Prevalence of cow-related violence (non-close election)\nin BJP- and non-BJP-ruled constituencies")+
  scale_color_manual("winning party in election",values=cols_incumbent)+geom_vline(xintercept=0, color="red")
ggsave("plots/analyses/stand_party3_2_ending.jpg", width =6 )

ggplot(data=lynchings_date_combined_elections_non_close, aes(x=month_gap, y=sum_VD_government, color=winner_party))+
  geom_point(data=lynchings_date_combined_elections_non_close[lynchings_date_combined_elections_non_close$sum_VD_government>0,])+
  geom_smooth(data=lynchings_date_combined_elections_non_close[lynchings_date_combined_elections_non_close$month_gap<0,], span=2)+
  geom_smooth(data=lynchings_date_combined_elections_non_close[lynchings_date_combined_elections_non_close$month_gap>0,], span=2)+
  ylim(0, 0.002)+xlim(-22, 22)+
  labs(x="month to formation of government", y="number of incidents per constituency per month", title="Prevalence of cow-related violence (non-close election)\nin BJP- and non-BJP-ruled constituencies")+
  scale_color_manual("winning party in election",values=cols_incumbent)+geom_vline(xintercept=0, color="red")
ggsave("plots/analyses/stand_party3_2_government.jpg", width =6 )

ggplot(data=lynchings_date_combined_elections_pre_BJP, aes(x=month_gap, y=sum_VD_beginning, color=winner_party))+
  geom_point(data=lynchings_date_combined_elections_pre_BJP[lynchings_date_combined_elections_pre_BJP$sum_VD_beginning>0,])+
  geom_smooth(data=lynchings_date_combined_elections_pre_BJP[lynchings_date_combined_elections_pre_BJP$month_gap<0,], span=2)+
  geom_smooth(data=lynchings_date_combined_elections_pre_BJP[lynchings_date_combined_elections_pre_BJP$month_gap>0,], span=2)+
  ylim(0, 0.002)+xlim(-20, 22)+
  labs(x="month to beginning of election", y="number of incidents per constituency per month", title="Prevalence of cow-related violence (BJP incumbent)\nin BJP- and non-BJP-ruled constituencies")+
  scale_color_manual("winning party in election",values=cols_incumbent)+geom_vline(xintercept=0, color="red")
ggsave("plots/analyses/stand_party3_3_beginning.jpg", width =6 )

ggplot(data=lynchings_date_combined_elections_pre_BJP, aes(x=month_gap, y=sum_VD_ending, color=winner_party))+
  geom_point(data=lynchings_date_combined_elections_pre_BJP[lynchings_date_combined_elections_pre_BJP$sum_VD_ending>0,])+
  geom_smooth(data=lynchings_date_combined_elections_pre_BJP[lynchings_date_combined_elections_pre_BJP$month_gap<0,], span=2)+
  geom_smooth(data=lynchings_date_combined_elections_pre_BJP[lynchings_date_combined_elections_pre_BJP$month_gap>0,], span=2)+
  ylim(0, 0.002)+xlim(-20, 22)+
  labs(x="month to end of election", y="number of incidents per constituency per month", title="Prevalence of cow-related violence (BJP incumbent)\nin BJP- and non-BJP-ruled constituencies")+
  scale_color_manual("winning party in election",values=cols_incumbent)+geom_vline(xintercept=0, color="red")
ggsave("plots/analyses/stand_party3_3_ending.jpg", width =6 )

ggplot(data=lynchings_date_combined_elections_pre_BJP, aes(x=month_gap, y=sum_VD_government, color=winner_party))+
  geom_point(data=lynchings_date_combined_elections_pre_BJP[lynchings_date_combined_elections_pre_BJP$sum_VD_government>0,])+
  geom_smooth(data=lynchings_date_combined_elections_pre_BJP[lynchings_date_combined_elections_pre_BJP$month_gap<0,], span=2)+
  geom_smooth(data=lynchings_date_combined_elections_pre_BJP[lynchings_date_combined_elections_pre_BJP$month_gap>0,], span=2)+
  ylim(0, 0.002)+xlim(-20, 22)+
  labs(x="month to government formation", y="number of incidents per constituency per month", title="Prevalence of cow-related violence (BJP incumbent)\nin BJP- and non-BJP-ruled constituencies")+
  scale_color_manual("winning party in election",values=cols_incumbent)+geom_vline(xintercept=0, color="red")
ggsave("plots/analyses/stand_party3_3_government.jpg", width =6 )

ggplot(data=lynchings_date_combined_elections_pre_non_BJP, aes(x=month_gap, y=sum_VD_beginning, color=winner_party))+
  geom_point(data=lynchings_date_combined_elections_pre_non_BJP[lynchings_date_combined_elections_pre_non_BJP$sum_VD_beginning>0,])+
  geom_smooth(data=lynchings_date_combined_elections_pre_non_BJP[lynchings_date_combined_elections_pre_non_BJP$month_gap<0,], span=2)+
  geom_smooth(data=lynchings_date_combined_elections_pre_non_BJP[lynchings_date_combined_elections_pre_non_BJP$month_gap>0,], span=2)+
  ylim(0, 0.002)+xlim(-20, 22)+
  labs(x="month to beginning of election", y="number of incidents per constituency per month", title="Prevalence of cow-related violence (non-BJP incumbent)\nin BJP- and non-BJP-ruled constituencies")+
  scale_color_manual("winning party in election",values=cols_incumbent)+geom_vline(xintercept=0, color="red")
ggsave("plots/analyses/stand_party3_4_beginning.jpg", width =6 )

ggplot(data=lynchings_date_combined_elections_pre_non_BJP, aes(x=month_gap, y=sum_VD_ending, color=winner_party))+
  geom_point(data=lynchings_date_combined_elections_pre_non_BJP[lynchings_date_combined_elections_pre_non_BJP$sum_VD_ending>0,])+
  geom_smooth(data=lynchings_date_combined_elections_pre_non_BJP[lynchings_date_combined_elections_pre_non_BJP$month_gap<0,], span=2)+
  geom_smooth(data=lynchings_date_combined_elections_pre_non_BJP[lynchings_date_combined_elections_pre_non_BJP$month_gap>0,], span=2)+
  ylim(0, 0.002)+xlim(-20, 22)+
  labs(x="month to end of election", y="number of incidents per constituency per month", title="Prevalence of cow-related violence (non-BJP incumbent)\nin BJP- and non-BJP-ruled constituencies")+
  scale_color_manual("winning party in election",values=cols_incumbent)+geom_vline(xintercept=0, color="red")
ggsave("plots/analyses/stand_party3_4_ending.jpg", width =6 )

ggplot(data=lynchings_date_combined_elections_pre_non_BJP, aes(x=month_gap, y=sum_VD_government, color=winner_party))+
  geom_point(data=lynchings_date_combined_elections_pre_non_BJP[lynchings_date_combined_elections_pre_non_BJP$sum_VD_government>0,])+
  geom_smooth(data=lynchings_date_combined_elections_pre_non_BJP[lynchings_date_combined_elections_pre_non_BJP$month_gap<0,], span=2)+
  geom_smooth(data=lynchings_date_combined_elections_pre_non_BJP[lynchings_date_combined_elections_pre_non_BJP$month_gap>0,], span=2)+
  ylim(0, 0.002)+xlim(-20, 22)+
  labs(x="month to government formation", y="number of incidents per constituency per month", title="Prevalence of cow-related violence (non-BJP incumbent)\nin BJP- and non-BJP-ruled constituencies")+
  scale_color_manual("winning party in election",values=cols_incumbent)+geom_vline(xintercept=0, color="red")
ggsave("plots/analyses/stand_party3_4_government.jpg", width =6 )

# regression discontinuity design

# linking all elections to incidents (using elections_19_agr)

elections_19_agr$BJP_relative[elections_19_agr$winning_party=="BJP" & is.na(elections_19_agr$winning_party)==FALSE]<-(elections_19_agr$winning_party_vote/elections_19_agr$party_2_vote)[elections_19_agr$winning_party=="BJP" & is.na(elections_19_agr$winning_party)==FALSE]
elections_19_agr$BJP_relative[elections_19_agr$party_2=="BJP" & is.na(elections_19_agr$party_2)==FALSE]<-(elections_19_agr$party_2_vote/elections_19_agr$winning_party_vote)[elections_19_agr$party_2=="BJP" & is.na(elections_19_agr$party_2)==FALSE]
elections_RD<-elections_19_agr[elections_19_agr$year>=2010,]
duplicated(elections%>%select(c("STATE", "Last.Elections.Held")))
for (i in 1:nrow(elections_RD)){
  elections_after<-elections_RD[elections_RD$year>=elections_RD$year[i] & elections_RD$state_01==elections_RD$state_01[i] & elections_RD$ac_nm==elections_RD$ac_nm[i],]
  if (nrow(elections_after)==1){
    elections_RD$num_cycle_beginning[i]<-sum(lynchings_date_combined$DATE>=elections_after$date_begining[rank(elections_after$date_begining)==1] & lynchings_date_combined$DATE<=elections_after$date_begining[rank(elections_after$date_begining)==1]+365*5 & lynchings_date_combined$state_name==elections_after$state_01 & lynchings_date_combined$MLA_ELECTORATE==elections_after$ac_nm, na.rm = TRUE)
    elections_RD$num_cycle_ending[i]<-sum(lynchings_date_combined$DATE>=elections_after$date_ending[rank(elections_after$date_ending)==1] & lynchings_date_combined$DATE<=elections_after$date_ending[rank(elections_after$date_ending)==1]+365*5 & lynchings_date_combined$state_name==elections_after$state_01 & lynchings_date_combined$MLA_ELECTORATE==elections_after$ac_nm, na.rm = TRUE)
    elections_RD$num_cycle_government[i]<-sum(lynchings_date_combined$DATE>=elections_after$date_government[rank(elections_after$date_government)==1] & lynchings_date_combined$DATE<=elections_after$date_government[rank(elections_after$date_government)==1]+365*5 & lynchings_date_combined$state_name==elections_after$state_01 & lynchings_date_combined$MLA_ELECTORATE==elections_after$ac_nm, na.rm = TRUE)
    elections_RD$num_year_beginning[i]<-sum(lynchings_date_combined$DATE>=elections_after$date_begining[rank(elections_after$date_begining)==1] & lynchings_date_combined$DATE<elections_after$date_begining[rank(elections_after$date_begining)==1]+365 & lynchings_date_combined$state_name==elections_after$state_01 & lynchings_date_combined$MLA_ELECTORATE==elections_after$ac_nm, na.rm = TRUE)
    elections_RD$num_year_ending[i]<-sum(lynchings_date_combined$DATE>=elections_after$date_ending[rank(elections_after$date_ending)==1] & lynchings_date_combined$DATE<elections_after$date_ending[rank(elections_after$date_ending)==1]+365 & lynchings_date_combined$state_name==elections_after$state_01 & lynchings_date_combined$MLA_ELECTORATE==elections_after$ac_nm, na.rm = TRUE)
    elections_RD$num_year_government[i]<-sum(lynchings_date_combined$DATE>=elections_after$date_government[rank(elections_after$date_government)==1] & lynchings_date_combined$DATE<elections_after$date_government[rank(elections_after$date_government)==1]+365 & lynchings_date_combined$state_name==elections_after$state_01 & lynchings_date_combined$MLA_ELECTORATE==elections_after$ac_nm, na.rm = TRUE)
  }else{
    elections_RD$num_cycle_beginning[i]<-sum(elections_after$date_begining[rank(elections_after$date_begining)==1]<= lynchings_date_combined$DATE & lynchings_date_combined$DATE < elections_after$date_begining[rank(elections_after$date_begining)==2] & lynchings_date_combined$state_name==elections_after$state_01 & lynchings_date_combined$MLA_ELECTORATE==elections_after$ac_nm, na.rm = TRUE)
    elections_RD$num_cycle_ending[i]<-sum(elections_after$date_ending[rank(elections_after$date_ending)==1]<= lynchings_date_combined$DATE & lynchings_date_combined$DATE < elections_after$date_ending[rank(elections_after$date_ending)==2] & lynchings_date_combined$state_name==elections_after$state_01 & lynchings_date_combined$MLA_ELECTORATE==elections_after$ac_nm, na.rm = TRUE)
    elections_RD$num_cycle_government[i]<-sum(elections_after$date_government[rank(elections_after$date_government)==1]<= lynchings_date_combined$DATE & lynchings_date_combined$DATE < elections_after$date_government[rank(elections_after$date_government)==2] & lynchings_date_combined$state_name==elections_after$state_01 & lynchings_date_combined$MLA_ELECTORATE==elections_after$ac_nm, na.rm = TRUE)
    elections_RD$num_year_beginning[i]<-sum(elections_after$date_begining[rank(elections_after$date_begining)==1]<= lynchings_date_combined$DATE & lynchings_date_combined$DATE<elections_after$date_begining[rank(elections_after$date_begining)==1]+365 & lynchings_date_combined$DATE < elections_after$date_beginning[rank(elections_after$date_beginning)==2] & lynchings_date_combined$state_name==elections_after$state_01 & lynchings_date_combined$MLA_ELECTORATE==elections_after$ac_nm, na.rm = TRUE)
    elections_RD$num_year_ending[i]<-sum(elections_after$date_ending[rank(elections_after$date_ending)==1]<= lynchings_date_combined$DATE & lynchings_date_combined$DATE<elections_after$date_ending[rank(elections_after$date_ending)==1]+365 & lynchings_date_combined$DATE < elections_after$date_ending[rank(elections_after$date_ending)==2] & lynchings_date_combined$state_name==elections_after$state_01 & lynchings_date_combined$MLA_ELECTORATE==elections_after$ac_nm, na.rm = TRUE)
    elections_RD$num_year_government[i]<-sum(elections_after$date_government[rank(elections_after$date_government)==1]<= lynchings_date_combined$DATE & lynchings_date_combined$DATE<elections_after$date_government[rank(elections_after$date_government)==1]+365 & lynchings_date_combined$DATE < elections_after$date_government[rank(elections_after$date_government)==2] & lynchings_date_combined$state_name==elections_after$state_01 & lynchings_date_combined$MLA_ELECTORATE==elections_after$ac_nm, na.rm = TRUE)
  }
}

ggplot(data=elections_RD)+geom_density(aes(x=log(BJP_relative)))+xlim(-2, 2)+labs(x="ln(VS)", y="density", title="Distribution of log relative BJP vote share")
ggsave("plots/analyses/VS_distribution.jpg", width =6 )

ggplot(data=elections_RD[is.na(elections_RD$BJP_relative)==FALSE,], aes(x=date_begining, y=log(BJP_relative)))+geom_point()+geom_smooth()+labs(x="beginning of election", y="VS", title="Temporal pattern of VS")
ggsave("plots/analyses/VS_temporal.jpg", width =6 )       
breaks<-c(-0.05*(20:1), 0.05*(0:20))
voteshare_binned<-data.frame(start=breaks[1:40], end=breaks[2:41])

for (i in 1:40){
  voteshare_binned$mean_cycle_beginning[i]<-mean(elections_RD$num_cycle_beginning[log(elections_RD$BJP_relative)>=voteshare_binned$start[i] & log(elections_RD$BJP_relative)<voteshare_binned$end[i]], na.rm=TRUE)
  voteshare_binned$mean_cycle_ending[i]<-mean(elections_RD$num_cycle_ending[log(elections_RD$BJP_relative)>=voteshare_binned$start[i] & log(elections_RD$BJP_relative)<voteshare_binned$end[i]], na.rm=TRUE)
  voteshare_binned$mean_cycle_government[i]<-mean(elections_RD$num_cycle_government[log(elections_RD$BJP_relative)>=voteshare_binned$start[i] & log(elections_RD$BJP_relative)<voteshare_binned$end[i]], na.rm=TRUE)
  voteshare_binned$mean_year_beginning[i]<-mean(elections_RD$num_year_beginning[log(elections_RD$BJP_relative)>=voteshare_binned$start[i] & log(elections_RD$BJP_relative)<voteshare_binned$end[i]], na.rm = TRUE)
  voteshare_binned$mean_year_ending[i]<-mean(elections_RD$num_year_ending[log(elections_RD$BJP_relative)>=voteshare_binned$start[i] & log(elections_RD$BJP_relative)<voteshare_binned$end[i]], na.rm = TRUE)
  voteshare_binned$mean_year_government[i]<-mean(elections_RD$num_year_government[log(elections_RD$BJP_relative)>=voteshare_binned$start[i] & log(elections_RD$BJP_relative)<voteshare_binned$end[i]], na.rm = TRUE)
  }

voteshare_binned$mid=(voteshare_binned$start+voteshare_binned$end)/2


ggplot(data=elections_RD, aes(x=log(BJP_relative), y=num_cycle_beginning))+
  geom_smooth(data=elections_RD[elections_RD$BJP_relative<1 & elections_RD$BJP_relative>0.7,], method = "loess")+
  geom_smooth(data=elections_RD[elections_RD$BJP_relative>1 & elections_RD$BJP_relative<1.4,], method = "loess")+
  geom_vline(xintercept = 0, color="red", size=1)+
  xlim(-0.2, 0.2)+
  labs(x= "log relative BJP vote share", y= "number of incidents in election cycle (beginning of election)", title="regression discontinuity:\nrelative BJP vote share and cow-related violence")
ggsave("plots/analyses/RD1_beginning.jpg", width =6 )

ggplot(data=elections_RD, aes(x=log(BJP_relative), y=num_cycle_ending))+
  geom_smooth(data=elections_RD[elections_RD$BJP_relative<1 & elections_RD$BJP_relative>0.7,], method = "loess")+
  geom_smooth(data=elections_RD[elections_RD$BJP_relative>1 & elections_RD$BJP_relative<1.4,], method = "loess")+
  geom_vline(xintercept = 0, color="red", size=1)+
  xlim(-0.2, 0.2)+
  labs(x= "log relative BJP vote share", y= "number of incidents in election cycle (end of election)", title="regression discontinuity:\nrelative BJP vote share and cow-related violence")
ggsave("plots/analyses/RD1_ending.jpg", width =6 )

ggplot(data=elections_RD, aes(x=log(BJP_relative), y=num_cycle_government))+
  geom_smooth(data=elections_RD[elections_RD$BJP_relative<1 & elections_RD$BJP_relative>0.7,], method = "loess")+
  geom_smooth(data=elections_RD[elections_RD$BJP_relative>1 & elections_RD$BJP_relative<1.4,], method = "loess")+
  geom_vline(xintercept = 0, color="red", size=1)+
  xlim(-0.2, 0.2)+
  labs(x= "log relative BJP vote share", y= "number of incidents in election cycle (government formation)", title="regression discontinuity:\nrelative BJP vote share and cow-related violence")
ggsave("plots/analyses/RD1_government.jpg", width =6 )

ggplot(data=elections_RD, aes(x=log(BJP_relative), y=num_year_beginning))+
  geom_smooth(data=elections_RD[elections_RD$BJP_relative<1 & elections_RD$BJP_relative>0.7,], method = "loess")+
  geom_smooth(data=elections_RD[elections_RD$BJP_relative>1 & elections_RD$BJP_relative<1.4,], method = "loess")+
  geom_vline(xintercept = 0, color="red", size=1)+
  labs(x= "log relative BJP vote share", y= "number of incidents in following year (beginning of election)", title="regression discontinuity:\nrelative BJP vote share and cow-related violence")
ggsave("plots/analyses/RD2_beginning.jpg", width =6 )

ggplot(data=elections_RD, aes(x=log(BJP_relative), y=num_year_ending))+
  geom_smooth(data=elections_RD[elections_RD$BJP_relative<1 & elections_RD$BJP_relative>0.7,], method = "loess")+
  geom_smooth(data=elections_RD[elections_RD$BJP_relative>1 & elections_RD$BJP_relative<1.4,], method = "loess")+
  geom_vline(xintercept = 0, color="red", size=1)+
  labs(x= "log relative BJP vote share", y= "number of incidents in following year (end of election)", title="regression discontinuity:\nrelative BJP vote share and cow-related violence")
ggsave("plots/analyses/RD2_ending.jpg", width =6 )

ggplot(data=elections_RD, aes(x=log(BJP_relative), y=num_year_government))+
  geom_smooth(data=elections_RD[elections_RD$BJP_relative<1 & elections_RD$BJP_relative>0.7,], method = "loess")+
  geom_smooth(data=elections_RD[elections_RD$BJP_relative>1 & elections_RD$BJP_relative<1.4,], method = "loess")+
  geom_vline(xintercept = 0, color="red", size=1)+
  labs(x= "log relative BJP vote share", y= "number of incidents in following year (end of election)", title="regression discontinuity:\nrelative BJP vote share and cow-related violence")
ggsave("plots/analyses/RD2_government.jpg", width =6 )

ggplot(data=voteshare_binned[voteshare_binned$mid>=-0.5 & voteshare_binned$mid<0.6,])+
  geom_density(data=elections_RD, aes(x=log(BJP_relative), color="density"), size=1, alpha=0.1)+
  geom_line(aes(x=mid, y=20*mean_cycle_beginning, color="number of incidents"), size=1)+
  geom_vline(xintercept = 0, color="red", size=1)+
  xlim(-0.5, 0.6)+labs(x= "log relative BJP vote share", y= "number of incidents in election cycle", title="regression discontinuity:\nrelative BJP vote share and cow-related violence", caption = "binned average, binsize=0.05")+
  scale_y_continuous("density distrbution of vote-share", sec.axis = sec_axis(~ . * 0.05, name = "number of incidents in election cycle"))+
  scale_color_manual(values = c("yellow","blue"))
ggsave("plots/analyses/RD3.jpg", width =7 )


ggplot(data=voteshare_binned[voteshare_binned$mid>=-0.5 & voteshare_binned$mid<0.6,])+
  geom_density(data=elections_RD, aes(x=log(BJP_relative), color="density"), size=1, alpha=0.1)+
  geom_line(aes(x=mid, y=20*mean_cycle_beginning, color="number of incidents"), size=1)+
  geom_vline(xintercept = 0, color="red", size=1)+
  xlim(-0.5, 0.6)+labs(x= "log relative BJP vote share", y= "number of incidents in cycle (beginning of election)", title="regression discontinuity:\nrelative BJP vote share and cow-related violence", caption = "binned average, binsize=0.05")+
  scale_y_continuous("density distrbution of vote-share", sec.axis = sec_axis(~ . * 0.05, name = "number of incidents in cycle (beginning of election)"))+
  scale_color_manual(values = c("yellow","blue"))
ggsave("plots/analyses/RD3_beginning.jpg", width =7 )


ggplot(data=voteshare_binned[voteshare_binned$mid>=-0.5 & voteshare_binned$mid<0.6,])+
  geom_density(data=elections_RD, aes(x=log(BJP_relative), color="density"), size=1, alpha=0.1)+
  geom_line(aes(x=mid, y=20*mean_cycle_ending, color="number of incidents"), size=1)+
  geom_vline(xintercept = 0, color="red", size=1)+
  xlim(-0.5, 0.6)+labs(x= "log relative BJP vote share", y= "number of incidents cycle (end of election)", title="regression discontinuity:\nrelative BJP vote share and cow-related violence", caption = "binned average, binsize=0.05")+
  scale_y_continuous("density distrbution of vote-share", sec.axis = sec_axis(~ . * 0.05, name = "number of incidents in cycle (end of election)"))+
  scale_color_manual(values = c("yellow","blue"))
ggsave("plots/analyses/RD3_ending.jpg", width =7 )


ggplot(data=voteshare_binned[voteshare_binned$mid>=-0.5 & voteshare_binned$mid<0.6,])+
  geom_density(data=elections_RD, aes(x=log(BJP_relative), color="density"), size=1, alpha=0.1)+
  geom_line(aes(x=mid, y=20*mean_cycle_government, color="number of incidents"), size=1)+
  geom_vline(xintercept = 0, color="red", size=1)+
  xlim(-0.5, 0.6)+labs(x= "log relative BJP vote share", y= "number of incidents cycle (government formation)", title="regression discontinuity:\nrelative BJP vote share and cow-related violence", caption = "binned average, binsize=0.05")+
  scale_y_continuous("density distrbution of vote-share", sec.axis = sec_axis(~ . * 0.05, name = "number of incidents in cycle (government formation)"))+
  scale_color_manual(values = c("yellow","blue"))
ggsave("plots/analyses/RD3_government.jpg", width =7 )


ggplot(data=voteshare_binned[voteshare_binned$mid>=-0.5 & voteshare_binned$mid<0.6,])+
  geom_density(data=elections_RD, aes(x=log(BJP_relative), color="density"), size=1, alpha=0.1)+
  geom_line(aes(x=mid, y=20*mean_year_beginning, color="number of incidents"), size=1)+
  geom_vline(xintercept = 0, color="red", size=1)+
  xlim(-0.5, 0.6)+labs(x= "log relative BJP vote share", y= "number of incidents in following year (beginning of election)", title="regression discontinuity:\nrelative BJP vote share and cow-related violence", caption = "binned average, binsize=0.05")+
  scale_y_continuous("density distrbution of vote-share", sec.axis = sec_axis(~ . * 0.05, name = "number of incidents in following year (beginning of election)"))+
  scale_color_manual(values = c("yellow","blue"))
ggsave("plots/analyses/RD4_beginning.jpg", width =7 )

ggplot(data=voteshare_binned[voteshare_binned$mid>=-0.5 & voteshare_binned$mid<0.6,])+
  geom_density(data=elections_RD, aes(x=log(BJP_relative), color="density"), size=1, alpha=0.1)+
  geom_line(aes(x=mid, y=20*mean_year_ending, color="number of incidents"), size=1)+
  geom_vline(xintercept = 0, color="red", size=1)+
  xlim(-0.5, 0.6)+labs(x= "log relative BJP vote share", y= "number of incidents in following year (end of election)", title="regression discontinuity:\nrelative BJP vote share and cow-related violence", caption = "binned average, binsize=0.05")+
  scale_y_continuous("density distrbution of vote-share", sec.axis = sec_axis(~ . * 0.05, name = "number of incidents in following year (end of election)"))+
  scale_color_manual(values = c("yellow","blue"))
ggsave("plots/analyses/RD4_ending.jpg", width =7 )


ggplot(data=voteshare_binned[voteshare_binned$mid>=-0.5 & voteshare_binned$mid<0.6,])+
  geom_density(data=elections_RD, aes(x=log(BJP_relative), color="density"), size=1, alpha=0.1)+
  geom_line(aes(x=mid, y=20*mean_year_government, color="number of incidents"), size=1)+
  geom_vline(xintercept = 0, color="red", size=1)+
  xlim(-0.5, 0.6)+labs(x= "log relative BJP vote share", y= "number of incidents in following year (formation of government)", title="regression discontinuity:\nrelative BJP vote share and cow-related violence", caption = "binned average, binsize=0.05")+
  scale_y_continuous("density distrbution of vote-share", sec.axis = sec_axis(~ . * 0.05, name = "number of incidents in following year (formation of government)"))+
  scale_color_manual(values = c("yellow","blue"))
ggsave("plots/analyses/RD4_government.jpg", width =7 )

# regressions
elections_RD$BJP_winning<-elections_RD$winning_party=="BJP"
elections_RD$occur_year_beginning<-elections_RD$num_year_beginning>0
elections_RD$occur_cycle_beginning<-elections_RD$num_cycle_beginning>0
elections_RD$occur_year_ending<-elections_RD$num_year_ending>0
elections_RD$occur_cycle_ending<-elections_RD$num_cycle_ending>0
elections_RD$occur_year_government<-elections_RD$num_year_government>0
elections_RD$occur_cycle_government<-elections_RD$num_cycle_government>0
class(elections_RD$num_year_beginning)
elections_RD$year<-as.factor(elections_RD$year)
elections_RD$ac_nm<-as.factor(elections_RD$ac_nm)
elections_RD$VS<-log(elections_RD$BJP_relative)

model1<-glm(occur_year_beginning ~ poly(VS, 2) + BJP_winning , data=elections_RD[abs(elections_RD$VS)<=0.2 & is.na(elections_RD$VS)==FALSE,], family = binomial)
model2<-glm(occur_year_beginning ~ poly(VS, 2) + year + BJP_winning , data=elections_RD[abs(elections_RD$VS)<=0.2 & is.na(elections_RD$VS)==FALSE,], family = binomial)
model3<-glm(occur_cycle_beginning ~ poly(VS, 2) + BJP_winning , data=elections_RD[abs(elections_RD$VS)<=0.2 & is.na(elections_RD$VS)==FALSE,], family = binomial)
model4<-glm(occur_cycle_beginning ~ poly(VS, 2) + year + BJP_winning , data=elections_RD[abs(elections_RD$VS)<=0.2 & is.na(elections_RD$VS)==FALSE,], family = binomial)
stargazer(model1, model2, model3, model4, 
          keep=c(1, 2,3, 11, 12), 
          covariate.labels = c("VS", "VS squared", "", "BJP winning", "Constant"), 
          add.lines = list(c("Year fixed effect", "Yes", "No", "Yes", "No")),
          dep.var.labels = c("Incidents in following year", "Incidents in following cycle"),
          type="html", out="plots/analyses/regression1.html")
model1_1<-glm(occur_year_ending ~ poly(VS, 2) + BJP_winning , data=elections_RD[abs(elections_RD$VS)<=0.2 & is.na(elections_RD$VS)==FALSE,], family = binomial)
model2_1<-glm(occur_year_ending ~ poly(VS, 2) + year + BJP_winning , data=elections_RD[abs(elections_RD$VS)<=0.2 & is.na(elections_RD$VS)==FALSE,], family = binomial)
model3_1<-glm(occur_cycle_ending ~ poly(VS, 2) + BJP_winning , data=elections_RD[abs(elections_RD$VS)<=0.2 & is.na(elections_RD$VS)==FALSE,], family = binomial)
model4_1<-glm(occur_cycle_ending ~ poly(VS, 2) + year + BJP_winning , data=elections_RD[abs(elections_RD$VS)<=0.2 & is.na(elections_RD$VS)==FALSE,], family = binomial)
stargazer(model1_1, model2_1, model3_1, model4_1, 
          keep=c(1, 2,3, 11, 12), 
          covariate.labels = c("VS", "VS squared", "", "BJP winning", "Constant"), 
          add.lines = list(c("Year fixed effect", "Yes", "No", "Yes", "No")),
          dep.var.labels = c("Incidents in following year", "Incidents in following cycle"),
          title="robust check: end of election",
          type="html", out="plots/analyses/regression2.html")
model1_2<-glm(occur_year_government ~ poly(VS, 2) + BJP_winning , data=elections_RD[abs(elections_RD$VS)<=0.2 & is.na(elections_RD$VS)==FALSE,], family = binomial)
model2_2<-glm(occur_year_government ~ poly(VS, 2) + year + BJP_winning , data=elections_RD[abs(elections_RD$VS)<=0.2 & is.na(elections_RD$VS)==FALSE,], family = binomial)
model3_2<-glm(occur_cycle_government ~ poly(VS, 2) + BJP_winning , data=elections_RD[abs(elections_RD$VS)<=0.2 & is.na(elections_RD$VS)==FALSE,], family = binomial)
model4_2<-glm(occur_cycle_government ~ poly(VS, 2) + year + BJP_winning , data=elections_RD[abs(elections_RD$VS)<=0.2 & is.na(elections_RD$VS)==FALSE,], family = binomial)
stargazer(model1_2, model2_2, model3_2, model4_2, 
          keep=c(1, 2,3, 11, 12), 
          covariate.labels = c("VS", "VS squared", "", "BJP winning", "Constant"), 
          add.lines = list(c("Year fixed effect", "Yes", "No", "Yes", "No")),
          dep.var.labels = c("Incidents in following year", "Incidents in following cycle"),
          title="robust check: formation of government",
          type="html", out="plots/analyses/regression3.html")
