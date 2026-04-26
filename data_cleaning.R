# Data manipulation and cleaning
library(tidyverse)    # Includes dplyr, ggplot2, tidyr, readr, forcats, etc.
library(lubridate)    # Handling dates

# Correlation and visualization
library(GGally)       # Correlation and pair plots
library(corrplot)     # Base R style correlation plots
library(ggcorrplot)   # ggplot2-style correlation plots
library(patchwork)    # Combine ggplot2 charts


# 데이터 로드
bike.raw <- read.csv("seoulbike.csv", fileEncoding = "Latin1", check.names = F)

# r 파일의 object 따로 저장하였다.
save.image(file = "truc_objects.RData")
load("truc_objects.RData")

##################################################################################################
## 1. DATA PREPROCESSING
##################################################################################################
# TASK_1A. 날짜 데이터 처리. Create a new column called "dat_type" that categorizes a date into a weekend 
# or a weekday.
##################################################################################################
# Create two dataframes to join with the original "Date" column in bike.raw
# + 2017 dataframe
cal_2017 <- tibble(
  id = 1:365,
  Date = as.Date("2017-01-01") + id  # Start from Jan 1, 2017 to Jan 1, 2018
)
cal_2017 <- cal_2017 %>%
  mutate(Dat_Type = ifelse(wday(Date) %in% c(1, 7), "weekend", "weekday"),
         Date = format(Date, "%d/%m/%Y")) %>% 
  as.data.frame()
# only select December because in bike.raw "Date" column only includes Dec, 2017
cal_2017 <- cal_2017[(nrow(cal_2017)-31):(nrow(cal_2017)), -c(1)]
has_rownames(cal_2017) # TRUE
cal_2017 <- remove_rownames(cal_2017) # Reset index 

# + 2018 dataframe
cal_2018 <- tibble(
  id = 1:365,
  Date = as.Date("2018-01-01") + id  # Start from Jan 2, 2018 to November 30, 2018
)
cal_2018 <- cal_2018 %>%
  mutate(Dat_Type = ifelse(wday(Date) %in% c(1, 7), "weekend", "weekday"),
         Date = format(Date, "%d/%m/%Y")) %>% 
  as.data.frame()
# only select January to November because in bike.raw "Date" column only includes Jan~Nov, 2018
cal_2018 <- cal_2018[1:333, -c(1)]
has_rownames(cal_2018) # TRUE
cal_2018 <- remove_rownames(cal_2018) # Reset index 

# merge 2017_calendar and 2018_calendar
dim(cal_2017)
dim(cal_2018)
date_category.df <- bind_rows(cal_2017, cal_2018)
dim(date_category.df)

# left join 
bike <- bike.raw %>%
  left_join(date_category.df, by = "Date")



##################################################################################################
# TASK_1B. 컬럼명 변환 및 결측지 처리: Filter out Functioning Day = "No", as on those days Rented Bike 
# Count = 0
##################################################################################################
colnames(bike) <- c('date', 'bike_count', 'hour', 'temp', 'humid', 'wind', 
                           'visibility', 'dew_temp', 'radiation', 'rainfall',
                           'snowfall', 'season', 'holiday', 'function_binary', 'dat_type'
)

# Remove NA values included in the function_binary column (시스템이 작동하지 않은 날)
bike <- bike %>% 
  filter(function_binary == "Yes")
dim(bike)

# remove function_binary column
bike <- bike[,-c(14)]

##################################################################################################
# TASK_1C. time_in_day 변수 생성: 시간대 나누기
##################################################################################################
#
#  time_in_day    |       time_hour 
# -----------------------------------------
#    dusk         |     0시부터 - 6시까지
#   morning       |    7시부터 ~ 13시까지
#   evening       |    14시부터 ~ 18시까지
#   night         |    19시부터 ~ 23시까지
assign_list <- c("dawn", "early morning", "morning", "afternoon", "evening", "night")

bike <- bike %>%
  mutate(time_in_day = case_when(
    hour >= 0  & hour <= 3  ~ assign_list[1],  
    hour >= 4  & hour <= 7 ~ assign_list[2],  
    hour >= 8 & hour <= 11 ~ assign_list[3],  
    hour >= 12 & hour <= 15 ~ assign_list[4],
    hour >= 16 & hour <= 19 ~ assign_list[5],
    hour >= 20 & hour <= 23 ~ assign_list[6] 
  ))
table(bike$time_in_day)



##################################################################################################
# TASK_1D. 범주형 변수 인코딩: season, holiday
##################################################################################################
bike$season <- as.factor(bike$season) # Autumn, Spring, Summer, Winter
table(bike$season)
bike$holiday <- as.factor(bike$holiday) # Holiday, No Holiday
table(bike$holiday)
bike$hour <- as.factor(bike$hour) # 0시,1시,2시,...,23시
table(bike$hour)
bike$dat_type <- as.factor(bike$dat_type) # weekend, weekday
table(bike$dat_type)
bike$time_in_day <- as.factor(bike$time_in_day)
table(bike$time_in_day)


##################################################################################################
# TASK_1E. 이상치 및 결측치 처리
##################################################################################################
# 결측치 있는지 확인하기
colSums(is.na(bike)) # No NAs are detected

# 결측치 확인하기 위해 박스프롯 그리기 
# 시각화
weather_columns <- bike[, c('temp', 'humid', 'wind', 'visibility', 'dew_temp', 'radiation', 'rainfall', 'snowfall')]
ggpairs(weather_columns)

par(mfrow = c(2, 4))  # 2 rows, 4 columns of plots
boxplot(bike$temp, main = "Temperature")
boxplot(bike$humid, main = "Humidity")
boxplot(bike$wind, main = "Wind Speed")
boxplot(bike$visibility, main = "Visibility")
boxplot(bike$dew_temp, main = "Dew Temp")
boxplot(bike$radiation, main = "Radiation")
boxplot(bike$rainfall, main = "Rainfall")
boxplot(bike$snowfall, main = "Snowfall")


## Scatter Plot for Visualizing Relationships between Bike Count and Weather
corr_table <- bike %>% 
  select(bike_count, temp, humid, wind, visibility, dew_temp, radiation, rainfall, snowfall)

p1 <- ggplot(corr_table, aes(x = temp, y = bike_count)) + 
  geom_point(alpha = .1) + 
  stat_smooth(method = lm) + 
  ggtitle("Corr between Bike Count and Temp")

p2 <- ggplot(corr_table, aes(x = humid, y = bike_count)) + 
  geom_point(alpha = .1) + 
  stat_smooth(method = lm) + 
  ggtitle("Corr between Bike Count and Humidity")

p3 <- ggplot(corr_table, aes(x = wind, y = bike_count)) + 
  geom_point(alpha = .1) + 
  stat_smooth(method = lm) + 
  ggtitle("Corr between Bike Count and Wind")

p4 <- ggplot(corr_table, aes(x = visibility, y = bike_count)) + 
  geom_point(alpha = .1) + 
  stat_smooth(method = lm) + 
  ggtitle("Corr between Bike Count and Visibility")

p5 <- ggplot(corr_table, aes(x = dew_temp, y = bike_count)) + 
  geom_point(alpha = .1) + 
  stat_smooth(method = lm) + 
  ggtitle("Corr between Bike Count and Dew Temp")

p6 <- ggplot(corr_table, aes(x = radiation, y = bike_count)) + 
  geom_point(alpha = .1) + 
  stat_smooth(method = lm) + 
  ggtitle("Corr between Bike Count and Radiation")

p7 <- ggplot(corr_table, aes(x = rainfall, y = bike_count)) + 
  geom_point(alpha = .1) + 
  stat_smooth(method = lm) + 
  ggtitle("Corr between Bike Count and Rainfall")

p8 <- ggplot(corr_table, aes(x = snowfall, y = bike_count)) + 
  geom_point(alpha = .1) + 
  stat_smooth(method = lm) + 
  ggtitle("Corr between Bike Count and Snowfall")


# Combine all plots into a grid (2 rows x 4 columns for readability)
(p1 + p2 + p3 + p4) / (p5 + p6 + p7 + p8)





##################################################################################################
# TASK_1G. Snapshot of the dataset and Visualization (Pie charts, Box Plot, Correlation)
##################################################################################################
str(bike)
summary(bike)



##################################################################################################
# TASK_1H. Snapshot of the dataset and Visualization (Pie charts, Box Plot, Correlation)
##################################################################################################
# VISUALIZATION

# Correlation matrix for all columns in weather_columns
weather_cor <- cor(weather_columns)
round(weather_cor, digits = 2)
corrplot(weather_cor)




# VIZ_1. 날씨 변수들 사이 상관관계
#################################
# Subset numerical variables
df_numeric <- bike[, c('temp', 'humid', 'wind', 
                     'visibility', 'dew_temp', 'radiation', 
                     'rainfall', 'snowfall')]


# VIZ_2. Total Bike Rentals by Time of Day
#################################

#  Total Bike Rentals by Time of Day
table0 <- bike %>%
  group_by(time_in_day) %>%
  summarise(count = sum(bike_count))

ggplot(table0, aes(x = fct_reorder(time_in_day, count, .desc = TRUE), y = count)) +
  geom_col(fill = "#9fcae2ff", colour = "black") +
  geom_text(aes(label = comma(count)), vjust = -0.5) +
  labs(
    title = "Total Bike Rentals by Time of Day",
    x = "Time of Day",
    y = "Total Bike Count"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  
    axis.text.x = element_text(size = 16, face = "bold")
  )


# VIZ_3. Bike count by hour
#################################

# Bike count by hour
table1 <- bike %>%
  group_by(hour) %>%
  summarise(count = sum(bike_count))

ggplot(table1, aes(x = factor(hour), y = count, fill = count)) +
  geom_col() +
  scale_fill_gradient(
    low = "#a6cee3",  # light blue
    high = "#1f78b4"  # dark blue
  ) +
  geom_hline(yintercept = 200000, color = "red", size = 0.5) +
  geom_hline(yintercept = 400000, color = "red", size = 0.5) +
  labs(
    title = "Total Bike Rentals by Hour",
    x = "Hour",
    y = "Total Bike Count"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  
  ) +
  scale_y_continuous(
    labels = scales::comma) +
  coord_cartesian(ylim = c(min(table1$count), max(table1$count))) +
  theme(panel.grid = element_blank()) 


# VIZ_4. Bike count by date
#################################
bike$date <- as.Date(bike$date)
ggplot(bike, aes(x = date, y = bike_count)) +
  geom_line(color = "#a6cee3", linewidth = 1) +  # Apply custom color
  labs(title = "Daily Bike Rental Trend Over Time") +
  
  # Optional: Improve aesthetics
  theme_minimal() +  # Clean background
  theme(plot.title = element_text(hjust = 0.5))  # Center title



# VIZ_5. Bike count by season
#################################
# Bike Count by Season
table2 <- bike %>% 
  group_by(season) %>% 
  summarise(count = sum(bike_count)) %>% 
  arrange(desc(season)) %>%  # Optional: arrange by season or count
  mutate(
    prop = count / sum(count) * 100,  # Calculate percentages
    label = sprintf("%.1f%%", prop)
  ) %>% 
  arrange(desc(prop))

# Pie chart 
ggplot(table2, aes(x = "", y = prop, fill = reorder(season,prop))) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +  # Remove background/grid
  
  # Custom color palette (change as needed)
  scale_fill_manual(
    values = c(
    "Autumn" = "#a6cee3",  
    "Spring" = "#EDA394",  
    "Summer" = "#FF6B6B",  
    "Winter" = "#457DED"),
    name = "Season"
    ) +
  guides(fill = guide_legend(reverse = TRUE)) +
  
  # Percentage displayed
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),  # Auto-center labels
    color = "black",
    fontface = "bold"
  ) +
  
  labs(title = "Distribution by Season") +
  
  # Center the title
  theme(
    plot.title = element_text(
      hjust = 0.5,
      vjust = 0.5,
      size = 16,
      face = "bold"
    ),
    
    # Legend position
    legend.position = "right",
    
    # Legend title formatting
    legend.title = element_text(
      size = 12,
      face = "bold"
    )
  )




# VIZ_6. Bike by holiday
#################################

# Bike Count by Holiday
table3 <- bike %>% 
  group_by(holiday) %>% 
  summarise(cout = sum(bike_count))
table3
# Prepare data with percentages
table3 <- table3 %>%
  mutate(
    prop = cout / sum(cout) * 100,
    label = sprintf("%.1f%%", prop)  # Format as percentage
  )

# Create  pie chart
ggplot(table3, aes(x = "", y = prop, fill = holiday)) +
  geom_col(width = 1, color = "white", linewidth = 0.5) + 
  
  # Percentage labels inside slices
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),  # Auto-center labels
    color = "black",
    fontface = "bold"
  ) +
  
  # Color
  scale_fill_manual(
    values = c("No Holiday" = "#a6cee3", "Holiday" = "#FF6B6B"),
    name = ""  # Empty legend title
  ) +
  
  # Theme
  labs(title = "Bike Rentals by Holiday Status") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    legend.position = "right", 
    legend.text = element_text(size = 10)
  )


# VIZ 7. Bike by weekend or weekday
#################################
# Bike by weekend or weekday
table3 <- bike %>% 
  group_by(dat_type) %>% 
  summarise(count = sum(bike_count)) %>%
  arrange(desc(count)) %>%
  mutate(dat_type = factor(dat_type, levels = dat_type))

# Change to proportion table
table3 <- table3 %>%
  mutate(
    prop = count / sum(count) * 100,
    label = sprintf("%.1f%%", prop)  # Format as percentage
  ) 
table3

# Create pie chart (largest wedge first)
ggplot(table3, aes(x = "", y = prop, fill = reorder(dat_type, prop))) +
  geom_col(width = 1, color = "white", linewidth = 0.5) + 
  
  # Percentage labels inside slices
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),
    color = "black",
    fontface = "bold"
  ) +
  # Color 
  scale_fill_manual(
    values = c("weekend" = "#FF6B6B","weekday" = "#a6cee3"),
    name = ""
  ) +
  # Theme
  labs(title = "Bike Rentals by Day Type") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    legend.position = "right", 
    legend.text = element_text(size = 10)
  ) +
  # Legend order
  guides(fill = guide_legend(reverse = TRUE)) +
  coord_polar("y", start = 0)

# VIZ 8. Bike by time of the day
#################################
# First I have to make sure table5 is correct
table5 <- bike %>% 
  group_by(time_in_day) %>% 
  summarise(count = sum(bike_count))
table5

slices <- table5$count  # Get the count column as vector
lbls <- table5$time_in_day

# Calculate percentages
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, " (", pct, "%)", sep="")  # Format labels
pie(slices, 
    labels = lbls, 
    main = "Bike Rentals by Time in the Day",
    clockwise = TRUE,  
    init.angle = 90)   


bike_normalized <- bike_normalized[,-c()]

# 프로젝트 마무리하기: 데이터 bike_cleaned 이름으로 저장장
library(readr)
write_csv(bike, "bike_cleaned.csv")

