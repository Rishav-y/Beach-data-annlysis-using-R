# loading packages ----

library(tidyverse)
library(here)
library(skimr)
library(janitor)


# loading data ----

beaches <- read.csv(here("data", "sydneybeaches.csv"))


# looking at data ----

glimpse(beaches)

head(beaches)

# both used to get a summary of the data ----

summary(beaches)

skim(beaches)

# Cleaning Data ----

cleanbeaches <- rename(beaches, beachbugs = Enterococci..cfu.100ml.)

write.csv(cleanbeaches, "cleanbeaches.csv")


clean_beaches <- cleanbeaches %>% 
  separate(Date, c("Day","Month","Year"), remove = FALSE) %>% 
  mutate(logbeachbugs = log(beachbugs)) %>% 
  mutate(buggier = beachbugs > mean(beachbugs, na.rm = TRUE))

# Top 10 Highest recorded Bug Levels

worst_bug_levels <- clean_beaches %>% 
  arrange(- beachbugs) %>% 
  slice(1:10)


# Beach wise analysis for bugs

Beachwise_data <- clean_beaches %>% 
  group_by(Site) %>% 
  summarise( meanbugs = mean(beachbugs, na.rm = TRUE),
             maxbugs = max(beachbugs, na.rm = TRUE)) %>% 
  arrange(-meanbugs)

meanbeachbugs= mean(cleanbeaches$beachbugs, na.rm = TRUE)

# which counncil is not able to reduce the bugs

Council_Data <- clean_beaches %>% 
  group_by(Council) %>% 
  summarise(meanbugs = mean(beachbugs, na.rm = TRUE))


# Plotting the Graphss

## Bug levels by year

clean_beaches %>% 
  ggplot(aes(x=Year, y = beachbugs, color = Year))+
  geom_jitter()+
  labs(title = "Bug levels across Years")



## bug levels by site

clean_beaches %>% 
  ggplot(aes(x = Site, y = beachbugs, color = Year))+
  geom_jitter()+
  coord_flip()+
  labs(title ="Bugs by site")

## bug levels by site individual graphs

clean_beaches %>% 
  ggplot(aes(x = Year, y = beachbugs, color = Year))+
  geom_jitter()+
  facet_wrap(~ Site)

clean_beaches %>% 
  ggplot(aes(x = Year, y = beachbugs, color = Site))+
  geom_jitter()+
  facet_wrap(~ Site)

##Council reports

clean_beaches %>%
  ggplot(aes(x = Year, y = beachbugs, color = Council))+
  geom_jitter()+
  facet_wrap(~ Site)+
  labs(title = "council handling the beaches over")

## plotting meanbeachbugs

clean_beaches %>% 
  group_by(Site, Year) %>% 
  summarise(meanbugs = mean(beachbugs)) %>% 
  ggplot(aes(x= Year, y = meanbugs))+
  geom_col()+
  facet_wrap(~Site)+
  labs(title = "Yearly mean beachbugs across years")
  







