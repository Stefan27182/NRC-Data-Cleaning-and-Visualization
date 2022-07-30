
# The R code below asks the user to import the files saved by the python code, 
# and then the code uses those files to generate three plots using ggplot2. 




#-------------------------------------------------------------#

library(tidyverse) # Activates ggplot2, dplyr, tidyr, readr, purr, tibble, stringr, forcats

options(max.print = 200)

library(maps)


library(tcltk2)

#-------------------------------------------------------------#


msgBox <- tkmessageBox(title = "Load the appropriate Agreement State csv next",
                       message = "Load the appropriate Agreement State csv next", icon = "info", type = "ok")


Agreement_State <- read.csv(file.choose()) # load relevant file


msgBox <- tkmessageBox(title = "Load the appropriate Power Reactor csv next",
                       message = "Load the appropriate Power Reactor csv next", icon = "info", type = "ok")


Power_Reactor <- read.csv(file.choose()) # load relevant file


msgBox <- tkmessageBox(title = "Load the appropriate Fuel Cycle Facility csv next",
                       message = "Load the appropriate Fuel Cycle Facility csv next", icon = "info", type = "ok")


Fuel_Cycle_Facility <- read.csv(file.choose()) # load relevant file


msgBox <- tkmessageBox(title = "Load the appropriate Part21 csv next",
                       message = "Load the appropriate Part21 csv next", icon = "info", type = "ok")

Part21 <- read.csv(file.choose()) # load relevant file



# ----- start Agreement State dataframe instantiation and column naming----- #

AS_df <- data.frame(Agreement_State$Region.No, Agreement_State$Event.Desc, Agreement_State$Site.Name, Agreement_State$City.Name, Agreement_State$State.Cd, Agreement_State$Licensee.Name)
colnames(AS_df) <- c("Region", "Event Desc", "Site Name", "City", "State", "Licensee Name")


# ----- end Agreement State dataframe instantiation and column naming----- #


# ----- start Power Reactor event dataframe instantiation and colulmn naming----- #

PR_df <- data.frame(Power_Reactor$Region.No, Power_Reactor$Event.Desc, Power_Reactor$Site.Name , Power_Reactor$City.Name, Power_Reactor$State.Cd, Power_Reactor$Licensee.Name)
colnames(PR_df) <- c("Region", "Event Desc", "Site Name", "City", "State", "Licensee Name")


# ----- end Power Reactor dataframe instantiation and column naming----- #


# ----- start Fuel Cycle Facility dataframe instantiation and column naming ----- #

FCF_df <- data.frame(Fuel_Cycle_Facility$Region.No, Fuel_Cycle_Facility$Event.Desc, Fuel_Cycle_Facility$Site.Name, Fuel_Cycle_Facility$City.Name, Fuel_Cycle_Facility$State.Cd, Fuel_Cycle_Facility$Licensee.Name)
colnames(FCF_df) <- c("Region", "Event Desc", "Site Name", "City", "State", "Licensee Name")


# ----- end Fuel Cycle Facility dataframe instantiation and column naming ----- #


# ----- start Part21 dataframe instantiation and column naming ----- #

P21_df <- data.frame(Part21$Region.No, Part21$Event.Desc, Part21$Site.Name, Part21$City.Name, Part21$State.Cd, Part21$Licensee.Name)
colnames(P21_df) <- c("Region", "Event Desc", "Site Name", "City", "State", "Licensee Name")


# ----- end Part21 dataframe instantiation and column naming----- #


# ----- start joining dataframe's ----- #

join_1 <- full_join(AS_df, FCF_df)

join_2 <- full_join(PR_df, P21_df)

final_join <- full_join(join_1, join_2)

# ----- end joining dataframe's ----- #

# ----- start dataframe tidying ----- #

final_join$`Site Name` <- as.character(final_join$`Site Name`)

final_join$`Licensee Name` <- as.character(final_join$`Licensee Name`)

for(x in 1:nrow(final_join))
{
  if(final_join[x, 'Site Name'] == "")
  {
    final_join[x, 'Site Name'] <- final_join[x, 'Licensee Name']
    
  } # end if
  
} # end for

for(x in 1:nrow(final_join))
{
  if(final_join[x, 'Licensee Name'] == "")
  {
    final_join[x, 'Licensee Name'] <- final_join[x, 'Site Name']
    
  } # end if
  
} # end for

final_join <- final_join[,1:5]
rownames(final_join) <- c(1:nrow(final_join))
final_join$City <- as.character(final_join$City)
final_join$State <- as.character(final_join$State)

# ----- end dataframe tidying ----- #


# ----- finding cities that are in the dataframe at least twice ----- #

fj <- final_join %>% group_by(City) %>% filter(n()>1)

#----- end filter ----- #


# ----- binding naming and arranging ----- #

bnd <- cbind(fj$Region, fj$`Event Desc`, fj$`Site Name`, fj$City, fj$State)
bnd <- as.data.frame(bnd)
colnames(bnd) <- c("Region", "Event Desc", "Site Name", "City", "State")
dplyr::arrange(bnd, bnd$`City`)

# ----- end binding, ... -----#


# ----- using table function to count frequency of each city from >1 filter ----- #

tbl <- table(fj$City)

tbl_df <- as.data.frame(tbl)
colnames(tbl_df) <- c("City", "Report Frequency")
tbl_df$City <- as.character(tbl_df$City)


# ----- remove duplicates ----- #

fj <- fj[!duplicated(fj$City), ]


multi_reports <- cbind(fj, tbl_df$`Report Frequency`)

multi_reports_df <- as.data.frame(multi_reports)

colors <- data.frame("colors" = 1:nrow(multi_reports_df))

multi_reports_df <- cbind(multi_reports_df, colors)

colnames(multi_reports_df) <- c(as.list(colnames(fj)),"Report Frequency", "Color")


# ----- this for loop is used to create data points to aid in color determination ----- #

for(x in 1:nrow(multi_reports_df))
{
  
  mrdf_max  <- round(max(multi_reports_df$`Report Frequency`), 0)
  
  mrdf_min  <- round(min(multi_reports_df$`Report Frequency`), 0)
  
  mrdf_mdl  <- ((round((mrdf_max-mrdf_min), 0) / 2) + mrdf_min)
  
  mrdf_tq   <- ((round((mrdf_max-mrdf_mdl), 0) / 2 + mrdf_mdl))
  
  mrdf_sq   <- ((round((mrdf_mdl-mrdf_min), 0) / 2 + mrdf_min))
  
} # end for


# ----- this for is used for color determination ----- #

for( x in 1:nrow(multi_reports_df)) 
{
  if(multi_reports_df$`Report Frequency`[x] >= mrdf_min && multi_reports_df$`Report Frequency`[x] < mrdf_sq)
  {
    multi_reports_df$Color[x] <- "#283593" #Blue
    
  } # end if
  if(multi_reports_df$`Report Frequency`[x] >= mrdf_sq && multi_reports_df$`Report Frequency`[x] < mrdf_tq)
  {
    multi_reports_df$Color[x] <- "#FFEB3B" #Yellow
    
  } # end if
  if(multi_reports_df$`Report Frequency`[x] >= mrdf_tq)
  {
    multi_reports_df$Color[x] <- "#F44336" #Red
    
  } # end if
  
} # end for


# ----- creating nrc region vectors ------ #

nrc_region_1 <- c("Maine", "Connecticut", "Maryland", "Massachusetts", "Vermont", "New Hampshire", "New Jersey", "New York",  "Pennsylvania", "Rhode Island")

nrc_region_2 <- c("Alabama", "Florida", "Georgia", "North Carolina", "South Carolina", "Tennessee", "Virginia", "West Virginia", "Kentucky")

nrc_region_3 <- c("Illinois", "Iowa", "Michigan", "Minnesota", "Ohio", "Wisconsin", "Indiana", "Missouri")

nrc_region_4 <- c("Utah" ,"North Dakota", "South Dakota", "Montana", "Idaho", "Alask","Wyoming","Arizona", "Arkansas", "Oklahoma", "California", "Colorado", "Kansas", "Nevada", "Louisiana", "Mississippi", "Nebraska", "Texas", "Washington", "Oregon", "New Mexico")

# ----- converting vectors to dataframes ----- #

nrc_region_1_df <- as.data.frame(nrc_region_1)
nrc_region_2_df <- as.data.frame(nrc_region_2)
nrc_region_3_df <- as.data.frame(nrc_region_3)
nrc_region_4_df <- as.data.frame(nrc_region_4)

# ----- adding region number to dataframes ----- #

nrc_region_1_df <- cbind(nrc_region_1_df, as.integer(1))
nrc_region_2_df <- cbind(nrc_region_2_df, as.integer(2))
nrc_region_3_df <- cbind(nrc_region_3_df, as.integer(3))
nrc_region_4_df <- cbind(nrc_region_4_df, as.integer(4))

# ----- housekeeping ----- #

colnames(nrc_region_1_df) <- c("State Name", "Region")
colnames(nrc_region_2_df) <- c("State Name", "Region")
colnames(nrc_region_3_df) <- c("State Name", "Region")
colnames(nrc_region_4_df) <- c("State Name", "Region")

nrc_region_1_df$`State Name` <- as.character(nrc_region_1_df$`State Name`)
nrc_region_2_df$`State Name` <- as.character(nrc_region_2_df$`State Name`)
nrc_region_3_df$`State Name` <- as.character(nrc_region_3_df$`State Name`)
nrc_region_4_df$`State Name` <- as.character(nrc_region_4_df$`State Name`)

# ----- joining dataframes ----- #

nrc_reg_join_1 <- full_join(nrc_region_1_df, nrc_region_2_df)

nrc_reg_join_2 <- full_join(nrc_region_3_df, nrc_region_4_df)

nrc_reg_final_join <- full_join(nrc_reg_join_1, nrc_reg_join_2)

nrc_reg_final_join$`State Name` <- str_to_lower(nrc_reg_final_join$`State Name`)

full_join(fj, nrc_reg_final_join, by = c("State" = "State Name")) 

# ----- end joins ------ #


# ----- Setting map data ----- #

mp_dta <-  map_data("state")

# ----- joining map data and the above join ----- #

state_nrc_inner_join <- inner_join(mp_dta, nrc_reg_final_join, by = c("region" = "State Name"))

new_bnd <- cbind((as.character(multi_reports_df$City)), multi_reports_df)

library(readr)

State_and_two_letter_State_code_ <- read_delim("State and two letter State code .txt", 
                                               delim = "|", escape_double = FALSE, col_names = FALSE, 
                                               col_types = cols(X3 = col_skip()), trim_ws = TRUE)



colnames(State_and_two_letter_State_code_) <- c("State", "State code")

for(x in 1:nrow(new_bnd))
{
  holder_var <- match(new_bnd$State[x], State_and_two_letter_State_code_$`State code`)
  new_bnd$State[x] <- State_and_two_letter_State_code_$`State`[holder_var]
}

new_bnd$State <- tolower(new_bnd$State)

city_st_freq <- data.frame(x = nrow(new_bnd), y = 5)

city_st_freq[1:nrow(new_bnd),3:6] <- new_bnd[1:nrow(new_bnd),4:7]

colnames(city_st_freq) <- c("lat", "long", "Site Name", "City", "State", "Frequency")

rownames(city_st_freq) <- c(1:nrow(city_st_freq))

tmp_city <- data.frame()
city_st_freq$City

lat_long <- read_csv("raw_githubusercontent_com__kelvins__US-Cities-Database__main__csv__us_cities.csv")

for(x in 1:nrow(city_st_freq))
{
  
  tmp_city <- grep(tolower(city_st_freq$City[x]), tolower(lat_long$CITY))
  
  for(y in 1:length(tmp_city))
  {
    if(tolower(lat_long$STATE_NAME[tmp_city[y]]) == tolower(city_st_freq$State[x]))
    {
      city_st_freq$lat[x]  <- lat_long$LATITUDE[tmp_city[y]]
      city_st_freq$long[x] <- lat_long$LONGITUDE[tmp_city[y]]
      
    }
  }
  
} # end for

label_str <- data.frame()

x<-1
for(x in 1:nrow(city_st_freq))
{
  label_str[x,1] <- paste(city_st_freq[x,3], ",\n", city_st_freq[x,4], ",\n", str_to_title(city_st_freq[x,5]), ",", city_st_freq[x,6], "\n")
  
}

city_st_freq <- cbind(city_st_freq, label_str)

colnames(city_st_freq)[ncol(city_st_freq)] <- c("Site Name, City, State, and Frequency")


# ---------------------------- PLOTS ---------------------------- # 


# ----- start Event description frequency count faceted by Event description ------ #

ggplot(data=final_join, aes(`Region`, fill = `Event Desc`)) +
  geom_bar() +
  xlab("Region") +
  ylab("Reporting Frequency") +
  facet_wrap(~ `Event Desc`) +
  ggtitle("NRC reporting events by Region")

# ----- end Event description frequency count faceted by Event description ------ #



# ----- the following plot shows the counts and sites for sites that have more than two reports in the reporting time-frame ------ #

ggplot(data = multi_reports_df, aes(`Report Frequency`, fct_rev(`Site Name`), fill = `Color`)) +
  geom_bar(stat = "identity") +
  labs(title = "NRC reporting sites which reported more than\none incident in the reporting time-frame.\n") +
  theme(plot.title.position = "plot") +
  theme(plot.title = element_text(size=16, color = "black", face="bold", family = "Tahoma")) +
  theme(axis.title.x = element_text(size=12, color="black", face="bold", angle=0)) +
  theme(axis.title.y = element_text(size=12, color="black", face="bold", angle=90)) +
  theme(legend.title = element_text(size=12, color="purple", face="bold", angle=0)) +
  scale_fill_identity() +
  labs(x = "Report Count", y = "Site Name", fill = "Report Count")

# ----- the previous plot shows the counts and sites for sites that have more than two reports in the reporting time-frame ------ #



# ----- the plot below is a map with regions in different colors, and site location and counts shown ----- #
ggplot(data=state_nrc_inner_join, aes(x=long, y=lat)) +
  xlab("")+
  ylab("")+
  geom_polygon(aes(group=group), colour="black", fill=ifelse(state_nrc_inner_join$Region == 2, "red", ifelse(state_nrc_inner_join$Region == 3, "yellow", ifelse(state_nrc_inner_join$Region == 4, "purple", "blue")))) +
  geom_point(data=city_st_freq, aes(x=long, y=lat, colour = `Site Name, City, State, and Frequency`, fill = `Site Name, City, State, and Frequency`), size = 5)
# ----- this plot above  is a map with regions in different colors, and site location and counts shown ----- #

# ----- FINIS ---- #