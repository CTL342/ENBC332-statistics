# Christopher A. Lee
# ENBC 332
# Final Project
# 12/03/2025

### Dataset Import
# Link to dataset: https://www.kaggle.com/datasets/programmerrdai/cancer/data?select=total-cancer-deaths-by-type.csv
install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("countrycode")
library(readxl)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(countrycode)
file_path <- '/Users/christopherlee/Desktop/UMD/2025 Fall/ENBC 332/R Workspace/ENBC 332 - Statistics Project/cancerData/'

ori_data <- read_excel(paste(file_path, 'total-cancer-deaths-by-type.xlsx', sep=""), sheet="total-cancer-deaths-by-type")
gdp_data <- read_excel(paste(file_path, '2019 - Country Information.xlsx', sep=""), sheet="gross-domestic-product")
pop_data <- read_excel(paste(file_path, '2019 - Country Information.xlsx', sep=""), sheet="population")


### Data Preparation
print("--- Stratified Sampling From Datasets ---")
set.seed(100)
df_gdp <- data.frame(gdp_data, check.names=FALSE)
df_gdp_high <- df_gdp[df_gdp$GDP >= 1000000, ]
df_gdp_low <- df_gdp[df_gdp$GDP < 1000000, ]

# 17 Samples taken from low gdp dataframe because there were only 17 countries within the high gdp dataframe
df_gdp_low_new <- sample(1:length(df_gdp_low$Country), 17, replace=FALSE)
df_gdp_new <- rbind(df_gdp_high, df_gdp_low[df_gdp_low_new, ])

# Deaths only from the year of 2019 is taken out and analyzed
df <- data.frame(ori_data, check.names = FALSE)
df_2019 <- df[df$Year == 2019, ]
death_cols <- grep("^Deaths", names(df), value = TRUE)
df_2019$Total_Deaths <- rowSums(df_2019[, death_cols], na.rm = TRUE)
total_deaths_by_entity <- df_2019[, c("Entity", "Total_Deaths")]
names(total_deaths_by_entity)[names(total_deaths_by_entity) == "Entity"] <- "CountryName"

print("--- Data Cleaning and Merging ---")
df_gdp_clean <- data.frame(gdp_data, check.names=FALSE)
names(df_gdp_clean)[names(df_gdp_clean) == "Country"] <- "CountryName"

df_pop_clean <- data.frame(pop_data, check.names=FALSE)
names(df_pop_clean)[names(df_pop_clean) == "Country"] <- "CountryName"

# Brackets removed from GDP data
df_gdp_clean$CountryName <- gsub("\\[.*\\]", "", df_gdp_clean$CountryName)
df_gdp_clean$CountryName <- trimws(df_gdp_clean$CountryName)

# "non-country" regions in dataset are removed
filter_list <- c("World", "Asia", "Africa", "Europe", "North America", 
                 "South America", "Oceania", "European Union",
                 "High-income countries", "Low-income countries",
                 "Upper-middle-income countries", "Lower-middle-income countries")

df_deaths_clean <- total_deaths_by_entity %>% filter(!CountryName %in% filter_list)
df_gdp_clean    <- df_gdp_clean    %>% filter(!CountryName %in% filter_list)
df_pop_clean    <- df_pop_clean    %>% filter(!CountryName %in% filter_list)

# Fixed two known name mismatches
df_pop_clean$CountryName[df_pop_clean$CountryName == "Russian Federation"] <- "Russia"
df_pop_clean$CountryName[df_pop_clean$CountryName == "Korea, South"] <- "South Korea"

# Clean data is merged
df_econ <- merge(df_gdp_clean, df_pop_clean, by = "CountryName")
df_final_2019 <- merge(df_econ, df_deaths_clean, by = "CountryName")

# Any rows with incomplete data are removed
df_final_2019 <- df_final_2019[complete.cases(df_final_2019) & 
                                 df_final_2019$Population > 0 & 
                                 df_final_2019$GDP > 0, ]

print("--- Rate Calculations ---")
df_final_2019$GDP_per_Capita <- (df_final_2019$GDP * 1000000) / df_final_2019$Population
df_final_2019$Death_Rate_per_100k <- (df_final_2019$Total_Deaths / df_final_2019$Population) * 100000

print("--- Analysis-Ready Dataframe (Head) ---")
head(df_final_2019)


### Data analysis
# Demographic visualization for cancer death distribution by continent
df_final_2019$Continent <- countrycode(sourcevar = df_final_2019$CountryName,
                                       origin = "country.name",
                                       destination = "continent")
continent_summary <- df_final_2019 %>%
  group_by(Continent) %>%
  summarise(Total_Deaths = sum(Total_Deaths, na.rm = TRUE))
ggplot(continent_summary, aes(x=reorder(Continent, -Total_Deaths), y=Total_Deaths, fill=Continent)) +
  geom_bar(stat="identity") +
  labs(title="Total Cancer Deaths by Continent (2019)",
       x="Continent", y="Total Deaths") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)

# Proof that the original distribution was skewed and the log transformation ensured a normal distribution
print("--- Histograms ---")
p1 <- ggplot(df_final_2019, aes(x=GDP_per_Capita)) +
  geom_histogram(bins=30, fill="skyblue", color="black") +
  labs(title="Distribution of GDP per Capita", x="GDP per Capita (USD)", y="Count") +
  theme_minimal()
p2 <- ggplot(df_final_2019, aes(x=log(GDP_per_Capita))) +
  geom_histogram(bins=30, fill="salmon", color="black") +
  labs(title="Distribution of Log(GDP)", x="Log(GDP per Capita)", y="Count") +
  theme_minimal()
grid.arrange(p1, p2, ncol=2)

print("--- Pearson Correlation Test ---")
cor_test_result <- cor.test(df_final_2019$GDP_per_Capita, df_final_2019$Death_Rate_per_100k)
print(cor_test_result)

log_model <- lm(Death_Rate_per_100k ~ log(GDP_per_Capita), data = df_final_2019)

print("--- Log-Linear Regression Model Summary ---")
summary(log_model)

plot(log(df_final_2019$GDP_per_Capita), df_final_2019$Death_Rate_per_100k,
     main = "Cancer Death Rate vs. GDP per Capita (2019)",
     xlab = "Log(GDP per Capita)",
     ylab = "Cancer Deaths per 100,000 People",
     pch = 19,
     col = "blue",
     cex = 1.2)

abline(log_model, col = "red", lwd = 2)
grid()
legend("topright", 
       legend = c("Country Data", "Regression Line"), 
       col = c("blue", "red"), 
       pch = c(19, NA), 
       lty = c(NA, 1), 
       lwd = c(NA, 2))

print("--- Finalized GDP Data ---")
print(gdp_data)
print("--- Finalized Population Data ---")
print(pop_data)