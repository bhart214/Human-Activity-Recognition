### EDA: Getting a Feel for the Data

library(tidyverse)

ha <- read_delim("dataset-har-PUC-Rio-ugulino.csv", delim = ";")
glimpse(ha)

### Cleaning

# that's definitely not meters! ...and the bmi appears to be affected as well
ha <- ha %>% 
  rename(height_cm = how_tall_in_meters)
ha <- ha %>% 
  mutate(body_mass_index = body_mass_index/10)

# change class/user/gender to factor variable
ha$class <- as.factor(ha$class)
ha$user <- as.factor(ha$user)
ha$gender <- as.factor(ha$gender)


# remove row with NA value
ha <- na.omit(ha)

# write to r dataset for future use
write_rds(ha, "human_activity.rds")


### Graphs
ggplot(ha, aes(x = class)) +
  geom_bar()

ha %>% 
  gather(key = sensor_axis, value = sensor_reading, x1:z4) %>% 
  ggplot(aes(sensor_reading, fill = sensor_axis)) +
  geom_density(alpha = 0.3) +
  facet_grid(class ~ .) +
  scale_x_continuous(limits = c(-250, 250))

ha %>% 
  filter(class == "walking") %>% 
  gather(key = sensor_axis, value = sensor_reading, x1:z4) %>% 
  ggplot(aes(sensor_reading, fill = sensor_axis)) +
  geom_density(alpha = 0.3) +
  facet_grid(user ~ .) +
  scale_x_continuous(limits = c(-250, 250))


