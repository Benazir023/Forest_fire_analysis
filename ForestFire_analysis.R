# Setting up

library(tidyverse)

# Import data

forestfires <- read_csv("D:/BENA/Data Analytics/Dataquest/Project3_ForestFires/forestfires.csv")

head(forestfires)
glimpse(forestfires)
skimr::skim_without_charts(forestfires)
View(forestfires)
colnames(forestfires)

# The column names stand for....

# Each row represents a fire incident, its location & the weather variables associated with it. How each variable is related to the fire itself?



# Data processing
# Rearrange the month & day columns to their inherent orders

forestfires %>%
  pull(month) %>%
  unique

forestfires %>%
  pull(day) %>%
  unique

# Converting day & month into categorical variables, to make sure they're ordered correctly

forestfires <- forestfires %>%
  mutate(
    month_cat = factor(month, levels = c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"))
  )

forestfires <- forestfires %>%
  mutate(
    day_cat = factor(day, levels = c("sun","mon","tue","wed","thu","fri","sat"))
  )
# Alternatively

month_order <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
day_order <- c("sun","mon","tue","wed","thu","fri","sat")
 
forestfires <- forestfires %>%
   mutate(
     month = factor(month, levels = month_order),
     day = factor(day, levels = day_order)
   )

# When do most fires happen? summary by month & day

fires_per_month <- forestfires %>%
  group_by(month_cat) %>%
  summarise(total_fires = n())

forestfires %>%
ggplot(aes(x = month_cat)) +
  geom_bar() +        #viz1. aug has the highest number of fire incidents followed by sep
  labs(
    title = "Number of forest fires per month",
    y = "fire_incidents_count"
    )

# Alternatively

fires_per_month %>%
  ggplot(aes(x = month_cat, y = total_fires)) +
  geom_col() +
  labs(
    title = "Number of forest fires per month",
    y = "fire_incidents_count"
  )


fires_per_day <- forestfires %>%
  group_by(day_cat) %>%
  summarise(total_fires = n())

forestfires %>%
  ggplot(aes(x = day_cat)) +
  geom_bar() +
  labs(title = "Forest fires per day") +
  theme(panel.background = element_rect(fill = "lightblue"))

# Alternatively

fires_per_day %>%                        # sun had the highest number of fire incidents followed by fri. day with lowest number of incidents was wed
  ggplot(aes(x=day_cat, y=total_fires)) +
  geom_col() +                  
  labs(
    title = "Forest fires per day",
    y = "fire_incidents_count"
  )

# Plotting Other Variables Against Time
# Focus is on months since the variations are also brought about by seasons

forestfires_long <- forestfires %>%                   #transforms the data from wide to long format
  pivot_longer(
    cols = c(FFMC,DMC,DC,ISI,temp,RH,wind,rain),
    names_to = "weather_variable",
    values_to = "quantity"
  )

forestfires_long %>%
  ggplot(aes(x = month_cat, y = quantity)) +
  geom_boxplot() +
  facet_wrap(vars(weather_variable), scales = "free_y") +
  labs(
    title = "weather variable changes per month",
    y = "quantity",
    x = "month"
  )
 
# DC, DMC, FFMC, RH, temp & wind have high values in Aug & Sep

# Examining Forest Fire Severity
# the area column will be used as a proxy, based on the assumption that the more severe a fire is the larger the area that will be burnt

forestfires_long %>%
  ggplot(aes(x=quantity, y=area)) +
  geom_point() +
  facet_wrap(vars(weather_variable), scales = "free_x") +
  labs(
    title = "Relationship between area & weather_variables",
    x = "quantity of variable",
    y = "burnt area (in hectares)"
  )

# When the wind speed, temperatures, DC, FFMC are high, the area burnt tends to also be larger
# When RH, & rain are low the fires seem to be more severe & have a wide coverage

# Outlier Problems

forestfires_long_filtered <- forestfires_long %>%
  filter(area != 0 & area < 746)

# Check the max value

summary <- forestfires_long_filtered %>%
  summarise(
    maximum = max(area),
    minimum = min(area)
  )

forestfires_long_filtered1 <- forestfires_long_filtered %>%
  filter(area > 0.09 & area < 80)   #constitute 94% of the population after filtering in code above  

forestfires_long_filtered %>%
  filter(area > 0.09 & area < 80) %>%
  ggplot(aes(x=quantity, y=area)) +
  geom_point() +
  facet_wrap(vars(weather_variable), scales = "free_x") +
  labs(
    title = "Relationship between area & weather_variables",
    x = "quantity of variable",
    y = "burnt area (in hectares)"
  )

forestfires_long_filtered2 <- forestfires_long_filtered %>%
  filter(area > 80)

forestfires_long_filtered %>%
  filter(area > 80) %>%
  ggplot(aes(x=quantity, y=area)) +
  geom_point() +
  facet_wrap(vars(weather_variable), scales = "free_x") +
  labs(
    title = "Relationship between area & weather_variables",
    x = "quantity of variable",
    y = "burnt area (in hectares)"
  )
# It's hard  to describe relationship for most of the weather variables. However based on 94% of the population after filtering, the area increased the longer it took the place to receive rain. There also seems to be a positive relationship between FFMC & area
# Find out relationship between area & the other weather variables***
# Investigate the zeros. Are there forest fires that don't burn any land?
# Insight gathered from the analysis...

#Testing for emergency-fix github

