library(tidyverse)
library(dplyr)
library(ggplot2)
library("maps")

income_df <- read.csv("data/kaggle_income.csv")
crime_df <- read.csv("data/Criminal_Justice_Data_Book.csv")


# Data preparation (year == 2016, state == WA)
# Crime:
crime_2016_df <- crime_df %>%
  filter(year == 2016, 
         county != "state") %>%
  select(year, 
         county,
         contains("POP") | contains("JDP") | contains("PDP") |
           contains("ARN") | contains("JRN") | contains("APS")) %>%
  mutate(county = tolower(county),
         jail_rate = JDP_TOTALADP / POP_TOTAL,
         total_arrest = ARN_TOTAL + JRN_TOTAL)

crime_2016_total <- crime_2016_df %>%
  select(county, contains("TOTAL"), total_arrest)



# Income:
county_names <- unique(income_df$County)

income_df <- income_df %>%
  mutate(County = tolower(str_remove(County, " County")))

# data clean-up
# got rid of data with value 0 (sum_w == 0)
# got rid of wierd outliers (replaced Median == 300000 to NA)

wa_income_df <- income_df %>%
  filter(State_ab == "WA", sum_w > 0.0) %>%
  select(State_Name, State_ab, County, Type, Mean, Median, Stdev, sum_w)

wa_income_df[wa_income_df$Median == 300000, "Median"] <- NA

modified_income_df <- wa_income_df %>%
  group_by(County) %>%
  summarize(Mean = mean(Mean),
         Median = mean(Median, na.rm = T),
         Stdev = mean(Stdev),
         sum_w = mean(sum_w))

# No median value for mason, so I used mean instead

modified_income_df[modified_income_df$County=="mason", "Median"] <-
  modified_income_df %>%
  filter(County == "mason") %>%
  pull(Mean)

  

# Section 2.2 #1
# crime dataframe summary stats for the state of Washington 

crime_df <- filter(crime_df, county != "STATE")

mean_F_0_11 <- mean(crime_df$POP_F_0TO11)
mean_F_12_17 <- mean(crime_df$POP_F_12TO17)
mean_F_18_39 <- mean(crime_df$POP_F_18TO39)
mean_F_40UP <- mean(crime_df$POP_F_40UP)
mean_F_total <- mean(crime_df$POP_F_TOTAL)

mean_M_0_11 <- mean(crime_df$POP_M_0TO11)
mean_M_12_17 <- mean(crime_df$POP_M_12TO17)
mean_M_18_39 <- mean(crime_df$POP_M_18TO39)
mean_M_40UP <- mean(crime_df$POP_M_40UP)
mean_M_total <- mean(crime_df$POP_M_TOTAL)

county_num_median_total_F <- crime_df %>% 
  filter(POP_F_TOTAL == median(POP_F_TOTAL)) %>%
  pull(POP_F_TOTAL)

county_median_total_F <- crime_df %>% 
  filter(POP_F_TOTAL == median(POP_F_TOTAL)) %>%
  pull(county)

county_num_median_total_M <- crime_df %>% 
  filter(POP_M_TOTAL == median(POP_M_TOTAL)) %>%
  pull(POP_M_TOTAL)

county_median_total_M <- crime_df %>% 
  filter(POP_M_TOTAL == median(POP_M_TOTAL)) %>%
  pull(county)

max_county_F <- crime_df %>% 
  filter(POP_F_TOTAL == max(POP_F_TOTAL)) %>%
  pull(POP_F_TOTAL)

max_county_name_F <- crime_df %>% 
  filter(POP_F_TOTAL == max(POP_F_TOTAL)) %>%
  pull(county)

max_county_M <- crime_df %>% 
  filter(POP_M_TOTAL == max(POP_M_TOTAL)) %>%
  pull(POP_M_TOTAL)

max_county_name_M <- crime_df %>% 
  filter(POP_M_TOTAL == max(POP_M_TOTAL)) %>%
  pull(county)

min_county_F <- crime_df %>% 
  filter(POP_F_TOTAL == min(POP_F_TOTAL)) %>%
  pull(POP_F_TOTAL)

min_county_name_F <- crime_df %>% 
  filter(POP_F_TOTAL == min(POP_F_TOTAL)) %>%
  pull(county)

min_county_M <- crime_df %>% 
  filter(POP_M_TOTAL == min(POP_M_TOTAL)) %>%
  pull(POP_M_TOTAL)

min_county_name_M <- crime_df %>% 
  filter(POP_M_TOTAL == min(POP_M_TOTAL)) %>%
  pull(county)

# Income dataframe summary stats (USA by counties)

income_df <- mutate(income_df, county_state = paste(County,State_Name, sep = ", "))

mean_national_income <- mean(income_df$Mean)

median_national_income <- median(income_df$Median)

max_county_income <- income_df %>% 
  filter(Mean == max(Mean)) %>% 
  pull(county_state)

min_county_income <- income_df %>% 
  filter(Mean == min(Mean)) %>% 
  pull(county_state)

WA_counties_income <- income_df %>% 
  filter(State_Name == "Washington") %>% 
  group_by(County) %>% 
  summarize(Mean = mean(Mean), Median = median(Median))

WA_state_income <- income_df %>% 
  filter(State_Name == "Washington") %>%
  summarize(Mean = mean(Mean), Median = median(Median))

# Section 2.1 #2
# gets a sample of each dataframe

# crime in 2016:

crime_2016_sample <- slice_max(crime_2016_df, jail_rate, n = 10)

# income (from modified dataset):

income_sample <- modified_income_df

# Section 2.2 #2

# Graph/Plot #1
wa_income_data <- income_df %>% filter(State_Name == "Washington") %>% 
  group_by(County) %>% 
  summarise(Mean = round(mean(Mean))) %>% 
  unique() %>% 
  mutate(County = tolower(County))

shape_data <- map_data("county") %>% 
  filter(region == "washington")

wa_income_data_geo <- right_join(wa_income_data, 
                                 shape_data, 
                                 by = c("County" = "subregion"))

income_wa_plot <- ggplot(data = wa_income_data_geo) + 
  geom_polygon(mapping = aes(x=long, y = lat, group = group,fill = Mean)) + 
  coord_map() +
  scale_fill_distiller(palette = "YlGn", direction=1) +  
  labs(title = "Average Income across Washington", x = "", y= "", fill = "Average Income")

# Graph/Plot #2
# This graph have a outlier with a negative jail ratio

shape_jail_data <- left_join(shape_data, 
                             crime_2016_df,
                             by = c("subregion" = "county"))

jail_wa_plot <- ggplot(data = shape_jail_data) + 
  geom_polygon(mapping = aes(x=long, y = lat, group = group,fill = jail_rate)) + 
  coord_map() +
  scale_fill_distiller(palette = "YlGn", direction=1) +  
  labs(title = "Average Jail ratio across Washington", 
       x = "", 
       y= "", 
       fill = "Average Jail ratio",
       caption = paste("Average jail ratio is calculated by dividing average \n",
                       "daily jail population by the total population of the county \n",
                       "For example, 0.002 means, on average, 0.2% of \n",
                       "the population in a county is in jail daily. \n",
                       "A negative value means no data."))

# Graph/Plot #3
top10_jailrate_county<- slice_max(crime_2016_df, jail_rate, n = 10) %>%
  arrange(jail_rate)

top10_jailrate_plot <- top10_jailrate_county %>%
  mutate(county = factor(str_to_title(county),
                        levels = str_to_title(rev(top10_jailrate_county$county)))) %>%
  ggplot() + 
    geom_col(mapping = aes(x = county, y = jail_rate)) +
    labs(title = "Top 10 Washington Counties with Highest Jail Rate", 
         x = "", 
         y= "Average Daily Jail Rate")

# Graph/Plot #4
top_income_counties <- wa_income_data %>%  slice_max(Mean, n = 10) %>% arrange(Mean)
top_income_counties <- top_income_counties %>% mutate(County = factor(County, levels = County))
top_income_counties <- top_income_counties %>% mutate(Mean = factor(Mean, levels = unique(Mean)))
top_incomes_plot <- ggplot(data = top_income_counties) + geom_col(mapping = aes(x=County, y = Mean), position="dodge") + coord_flip() +  
  labs(title = "Counties with largest average incomes", x = "County Name", y = "Average Income")

# Graph/Plot #5
jail_density_plot <- ggplot(data=crime_2016_df, mapping = aes(x=jail_rate)) +
  geom_density() +
  geom_rug() +
  labs(
    title = "WA Jail Rate Density Plot",
    x = "Jail Rate"
  )

# Graph/Plot #6
income_density_plot <- ggplot(data=wa_income_data, mapping = aes(x = Mean)) +
  geom_density() +
  geom_rug() +
  labs(
    title = "WA Mean Income Density Plot",
    x = "Mean Income"
  )

# Section 2.2 #3 
# Outliers

# Section 3 
# Specific Question Analyses

# 1.Are people of different economic status punished by law differently? 

time_inc_df <- crime_2016_df %>% 
  left_join(modified_income_df, by = c("county" = "County")) %>% 
  select(county, APS_AVG_SENT, Median) %>% 
  na.omit()

max_sentence_data <- time_inc_df %>% 
  filter(APS_AVG_SENT == max(APS_AVG_SENT)) %>% 
  select(county, Median)

min_sentence_data <- time_inc_df %>% 
  filter(APS_AVG_SENT == min(APS_AVG_SENT)) %>% 
  select(county, Median)

max_income_data <- time_inc_df %>% 
  filter(Median == max(Median)) %>% 
  select(county, APS_AVG_SENT)

min_income_data <- time_inc_df %>% 
  filter(Median == min(Median)) %>% 
  select(county, APS_AVG_SENT)

avg_sent_med_income_plot <- ggplot(time_inc_df, aes(x = APS_AVG_SENT, y = Median)) + 
  geom_point(color = 'blue') +
  labs(
    title = "Median Incomes vs Average Sentence Time in Counties",
    x = "Average Sentence Time in Years",
    y = "Median Income"
  )

# 2.How are the rates of people arrested and the rates of people who actually 
#  are sentenced and go to prison affected by economic status? 

county_name_by_income_median <- modified_income_df %>%
  arrange(Median) %>%
  pull(County)

arrest_vs_prison_data <- crime_2016_total %>%
  select(county, POP_TOTAL, PDP_TOTAL ,ARN_TOTAL, JRN_TOTAL, total_arrest) %>%
  filter(county %in% county_name_by_income_median) %>%
  mutate(arrest_rate = total_arrest/POP_TOTAL,
         prison_rate = PDP_TOTAL/POP_TOTAL,
         prison_to_arrest_rate = PDP_TOTAL/total_arrest,
         income_median = modified_income_df$Median,
         income_mean = modified_income_df$Mean,
         income_sd = modified_income_df$Stdev)

# plot
# should see warnings: removed 2 rows containing missing values

arrest_vs_prison_by_median_plot <- ggplot(data = arrest_vs_prison_data,
                                   mapping = aes(x = income_median, 
                                                 y = prison_to_arrest_rate)) +
  geom_point() +
  geom_smooth(method="lm") +
  labs(
    title = "WA county prison to arrest rate by income median",
    x = "Income Median",
    y = "prison-arrest rate"
  )




# 3.How does the economic status of residents in a given county correlate with
#  the incarceration rate?

county_income_incarceration <-wa_income_data_geo %>% left_join(shape_jail_data, by = c("County" = "subregion")) %>% 
  select(County, Mean, jail_rate) %>% unique()


# 4.How does the type of crime committed relate to the geographic location of
#  the crime and the economic status of the area? 
  
# wrangling counties with mean incomes below $50,000/yr

county_crime_under_50k <- mutate(wa_income_data, county = tolower(County)) %>%
  left_join(crime_2016_df, by = "county") %>%
  filter(Mean < 50000) %>%
  select(County, starts_with("ARN"))

county_crime_under_50k_proportion <- county_crime_under_50k %>%
  summarize(County,
            arson_proportion = ARN_ARSON / ARN_TOTAL,
            assault_proportion = ARN_ASSAULT / ARN_TOTAL,
            bribery_proportion = ARN_BRIBERY / ARN_TOTAL,
            burglary_proportion = ARN_BURGLARY / ARN_TOTAL,
            destruction_property_proportion = ARN_DESTPROP / ARN_TOTAL,
            drug_violation_proportion = ARN_DRUGVIOL / ARN_TOTAL,
            extortion_proportion = ARN_EXTORTION / ARN_TOTAL,
            forgery_proportion = ARN_FORGERY / ARN_TOTAL,
            forced_sex_proportion = ARN_FSEX / ARN_TOTAL,
            gambling_violation_proportion = ARN_GAMBVIOL / ARN_TOTAL,
            group_b_proportion = ARN_GROUP_B / ARN_TOTAL,
            human_traffcking_proportion = ARN_HTRFFCKNG / ARN_TOTAL,
            kidnap_proportion = ARN_KIDNAP / ARN_TOTAL,
            manslaughter_proportion = ARN_MNSLGHTR / ARN_TOTAL,
            murder_proportion = ARN_MURDER / ARN_TOTAL,
            non_forced_sex_proportion = ARN_NFSEX / ARN_TOTAL,
            porn_proportion = ARN_PORN / ARN_TOTAL,
            prostitution_proportion = ARN_PROST / ARN_TOTAL,
            robbery_proportion = ARN_ROBBERY / ARN_TOTAL,
            theft_proportion = ARN_THEFT / ARN_TOTAL,
            violation_no_contact_proportion = ARN_VIOLNCO / ARN_TOTAL,
            weapons_violation_proportion = ARN_WEAPVIOL / ARN_TOTAL) %>%
  select_if(~any(. > 0.02))

# wrangling counties with mean incomes above $75,000/yr

county_crime_over_75k <- mutate(wa_income_data, county = tolower(County)) %>%
  left_join(crime_2016_df, by = "county") %>%
  filter(Mean > 75000) %>%
  select(County, starts_with("ARN"))

county_crime_over_75k_proportion <- county_crime_over_75k %>%
  summarize(County,
            arson_proportion = ARN_ARSON / ARN_TOTAL,
            assault_proportion = ARN_ASSAULT / ARN_TOTAL,
            bribery_proportion = ARN_BRIBERY / ARN_TOTAL,
            burglary_proportion = ARN_BURGLARY / ARN_TOTAL,
            destruction_property_proportion = ARN_DESTPROP / ARN_TOTAL,
            drug_violation_proportion = ARN_DRUGVIOL / ARN_TOTAL,
            extortion_proportion = ARN_EXTORTION / ARN_TOTAL,
            forgery_proportion = ARN_FORGERY / ARN_TOTAL,
            forced_sex_proportion = ARN_FSEX / ARN_TOTAL,
            gambling_violation_proportion = ARN_GAMBVIOL / ARN_TOTAL,
            group_b_proportion = ARN_GROUP_B / ARN_TOTAL,
            human_traffcking_proportion = ARN_HTRFFCKNG / ARN_TOTAL,
            kidnap_proportion = ARN_KIDNAP / ARN_TOTAL,
            manslaughter_proportion = ARN_MNSLGHTR / ARN_TOTAL,
            murder_proportion = ARN_MURDER / ARN_TOTAL,
            non_forced_sex_proportion = ARN_NFSEX / ARN_TOTAL,
            porn_proportion = ARN_PORN / ARN_TOTAL,
            prostitution_proportion = ARN_PROST / ARN_TOTAL,
            robbery_proportion = ARN_ROBBERY / ARN_TOTAL,
            theft_proportion = ARN_THEFT / ARN_TOTAL,
            violation_no_contact_proportion = ARN_VIOLNCO / ARN_TOTAL,
            weapons_violation_proportion = ARN_WEAPVIOL / ARN_TOTAL) %>%
  select_if(~any(. > 0.02))

# plotting counties below $50k/yr

lowest_incomes_long <- pivot_longer(county_crime_under_50k_proportion,
                                    cols = ends_with("proportion"),
                                    names_to = "crime_type"
)

lowest_incomes_plot <- ggplot(data = lowest_incomes_long) +
  geom_point(
    mapping = aes(x = County, y = value, color = crime_type),
  ) +
  labs(
    title = "WA Type of Crime Proportion for Counties with Mean Incomes below $50k/yr",
    x = "County",
    y = "Proportion of Crime"
  )

# plotting counties above $75k/yr

highest_incomes_long <- pivot_longer(county_crime_over_75k_proportion,
                                     cols = ends_with("proportion"),
                                     names_to = "crime_type"
)

highest_incomes_plot <- ggplot(data = highest_incomes_long) +
  geom_point(
    mapping = aes(x = County, y = value, color = crime_type),
  ) +
  labs(
    title = "WA Type of Crime Proportion for Counties with Mean Incomes above $75k/yr",
    x = "County",
    y = "Proportion of Crime"
  )


