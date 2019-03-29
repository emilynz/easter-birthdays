## File for working out when your birthday would coincide with Easter.
## Emily Harvey 29/3/2019
## Could do with tidying up, but seems to work...


# User inputs -------------------------------------------------------------


# set your birthday and whether you only want to know during Easter itself (modeSTRICT=TRUE) 
# or whether a birthday on the Thursday before or Tuesday after would count too

DOB <- as.Date("01-04-1980", format = c("%d-%m-%Y")) 

modeSTRICT = FALSE


# Set up and  Calculations ------------------------------------------------------------


library(tidyverse)
library(lubridate)

#load data
easter_data <- read_csv('Easterdates.csv')


# transform date into the right form
EasterSunday <- easter_data %>%
  mutate(EasterSundayDate=paste(Day,Month,Year, sep='-')) %>%
  mutate(EasterSundayDate=as.Date(EasterSundayDate, format =c("%d-%m-%Y"))) %>%
  arrange(EasterSundayDate) %>%
  select(EasterSundayDate) 

# in order to compare dates we need to strip out the year 
# it will default to this year (as it needs a year...?), but as long as they are all the same that's fine - no issues with leap years in Mar/Apr dates

easter_calcs <- EasterSunday %>%
  mutate(BirthDate = DOB) %>%
  mutate(EasterSundayDay = as.Date(paste(day(EasterSundayDate),month(EasterSundayDate),sep='-'), format =c("%d-%m")),
         BirthDay = as.Date(paste(day(BirthDate),month(BirthDate),sep='-'), format =c("%d-%m")))


# calculate if the selected birthday is during Easter or not
if(modeSTRICT){
  easter_calcs <- easter_calcs %>%
  mutate(Timing = case_when(
    (BirthDay < EasterSundayDay+2) & (BirthDay > EasterSundayDay-3) & (year(EasterSundayDate)< 2019) ~ "Birthday during Easter - past",
    (BirthDay < EasterSundayDay+2) & (BirthDay > EasterSundayDay-3)  ~ "Birthday during Easter - future",
    TRUE ~ "No"
  ))
}else{
  easter_calcs <- easter_calcs %>%
    mutate(Timing = case_when(
      (BirthDay < EasterSundayDay+3) & (BirthDay > EasterSundayDay-4) & (year(EasterSundayDate)< 2019) ~ "Birthday during Easter - past",
      (BirthDay < EasterSundayDay+3) & (BirthDay > EasterSundayDay-4)  ~ "Birthday during Easter - future",
      TRUE ~ "No"
    ))
}

# looking at the pattern over full dataset -----------

# add variable for which century
easter_calcs <- easter_calcs%>%
  mutate(century=paste0(str_trunc(year(EasterSundayDate),2,"right",ellipsis=""),"00"))


#plot for each century
ggplot(easter_calcs, aes(EasterSundayDay, ..count..)) +
  geom_bar(aes(fill=Timing))+
  scale_fill_manual(values=c("#8225CA", "#C210CE","#E6E6FA"))+
  facet_wrap(~century)+
  theme_bw()



# for the 100 years from DOB (optimistic about life span...?) -----------

#check if Easter was before or after birthdate in year of birth

startYear <- EasterSunday %>%
  mutate(BirthDate = DOB) %>%
  filter(year(EasterSundayDate)==year(DOB)) %>%
  mutate(startYear = ifelse( (EasterSundayDate+ifelse(modeSTRICT,1,2))<BirthDate, year(BirthDate)+1,year(BirthDate) )) %>%
  pull(startYear)

# create the subset for the 100 easters after birth date
lifetime <- easter_calcs %>%
  filter(year(EasterSundayDate)>(startYear-1) & year(EasterSundayDate)<(startYear+100)) %>%
  mutate(Year = year(EasterSundayDate))

# create the text for the labels on the bar plot
labels <- lifetime %>%
  filter(Timing != "No") %>%
  group_by(EasterSundayDay) %>%
  summarise(years = list(Year))


# plot the figure
p<- ggplot(lifetime, aes(EasterSundayDay, ..count.., position = "stack")) +
  geom_bar(aes(fill=Timing))+
  scale_fill_manual(values=c("#C210CE", "#8225CA", "#E6E6FA"))+
  theme_minimal()+
  scale_y_discrete(name ="Number of occurrences", 
                   limits=c(0:5), expand = c(0,0))+
  scale_x_date(name ="Date of Easter Sunday")+
  ggtitle("Timing of Easter Sunday during the century after example birthdate")+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.x = element_line())


for (day in seq(nrow(labels))) {
  p <- p + annotate("text",x=labels$EasterSundayDay[day], y=0.3, label=paste(unlist(labels$years[day]), collapse = "                "), 
               vjust = 0.3, hjust = 0, angle = 90, colour="white")
}

p








