##### 1.0 Data ----------------------------------------------------------------------------------------------------------------------
### 1.1 Read data
# Data from directory
data <- readRDS(file = "06. Data/02. Rekodning/03. results/immigrant_recode.rds") %>% 
  as_tibble()

##### 2.0 Cleaning ------------------------------------------------------------------------------------------------------------------
### 2.1 Remove speeders (maybe not relevant)
# Get the time stamps right and a time variable
data <- data %>% 
  mutate(ActualSurveyStartTime = ymd_hms(ActualSurveyStartTime),
         ActualSurveyEndTime = ymd_hms(ActualSurveyEndTime),
         time = ActualSurveyEndTime - ActualSurveyStartTime)

# Remove respodent who use less than 3 minutes
data <- data %>% 
  filter(time > 3)

### 2.2 Remove duplicates
data <- data %>% 
  distinct(., Id, 
           .keep_all = TRUE)

### 2.3 Remove test interviews
data <- data %>% 
  filter(!grepl(pattern = "test", 
                x = Id, 
                ignore.case = TRUE))

data <- data %>% 
  filter(!grepl(pattern = "xxx", 
                x = Id, 
                ignore.case = TRUE))

data <- data %>% 
  filter(!grepl(pattern = "\\[", 
                x = Id, 
                ignore.case = FALSE))

### 2.4 We have 1016 complete interviews, so we remove the last 16
data <- data %>% 
  arrange(ActualSurveyEndTime) %>% 
  slice(1:1000)

##### 3.0 Save to folder -------------------------------------------------------------------------------------------------------------
saveRDS(object = data,
        file = "06. Data/03. Datarens/03. results/immigrant_clean.rds")
