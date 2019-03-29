## File for working out when ANZAC Day would be in the week after Easter.
## Emily Harvey 29/3/2019
## Could do with tidying up, but seems to work...


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
anzac_calcs <- EasterSunday %>%
  mutate(EasterSundayDay = as.Date(paste(day(EasterSundayDate),month(EasterSundayDate),sep='-'), format =c("%d-%m")),
         ANZACDay = as.Date("25-04", format =c("%d-%m")))

# calculate when ANZAC is during or the week after Easter
anzac_calcs <- anzac_calcs %>%
  mutate(Timing = case_when(
    (ANZACDay < EasterSundayDay+2) & (ANZACDay > EasterSundayDay-3) ~ "ANZAC during Easter",
    (EasterSundayDay > as.Date("19-04", format =c("%d-%m")))  ~ "ANZAC in the week after Easter",
    TRUE ~ "No"
  ) )

# looking at the pattern over full dataset -----------

# add variable for which century
anzac_calcs <- anzac_calcs %>%
  mutate(century=paste0(str_trunc(year(EasterSundayDate),2,"right",ellipsis=""),"00"))

#plot for each century
ggplot(anzac_calcs, aes(EasterSundayDay, ..count..)) +
  geom_bar(aes(fill=Timing))+
  scale_fill_manual(values=c("#9E2109", "#EF1E0C","#C9BDBC"))+
  facet_wrap(~century)+
  theme_bw()

# looking at just the next 100 years -----------

#create the subset (could change to > or < 100 manually?)
century <- anzac_calcs %>%
  mutate(Year = year(EasterSundayDate))%>%
  filter(Year >(year(EasterSundayDay)-1) & Year<(year(EasterSundayDay)+100))

# create the text for the labels on the bar plot
labels <- century %>%
  filter(Timing != "No") %>%
  group_by(EasterSundayDay) %>%
  summarise(years = list(Year))


p <- ggplot(century, aes(EasterSundayDay, ..count..)) +
  geom_bar(aes(fill=Timing))+
  scale_fill_manual(values=c("#9E2109", "#EF1E0C","#C9BDBC"))+
  theme_minimal()+
  scale_y_discrete(name ="Number of occurrences", 
                   limits=c(0:6), expand = c(0,0))+
  scale_x_date(name ="Date of Easter Sunday")+
  ggtitle("Timing of Easter in relation to ANZAC Day in the next 100 years")+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.x = element_line())

for (day in seq(nrow(labels))) {
  p <- p + annotate("text",x=labels$EasterSundayDay[day], y=0.25, label=paste(unlist(labels$years[day]), collapse = "            "), 
                    vjust = 0.3, hjust = 0, angle = 90, colour="white")
}

p
