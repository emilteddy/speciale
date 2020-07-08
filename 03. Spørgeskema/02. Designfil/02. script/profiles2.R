# Reset session
rm(list=ls())

# Set seed
set.seed(310893)

# Read packages
library(pacman)
pacman::p_load(dplyr, tidyverse, ggplot2, haven, stargazer, xlsx, psych, sjstats, 
               lubridate, xlsx, foreign, broom, stringr, purrr, jsonlite, data.table, beepr)

# Set working directory
setwd(paste("C:/Users/", Sys.info()[7], "/Dropbox/", sep = ""))

# Load themes and colors 
source("epinion_style.R")
source("epinion_color.R")

##### 1.0 Features and levels -------------------------------------------------------------------------------------------------
### 1.1 Define feature and number of levels
# Feature: Køn
# Levels: 1) Mand 2) Kvinde
a <- 1:2

# Feature: Alder
# Levels: 1) 25 år 2) 31 år 3) 37 år 4) 43 år 5) 55 år
b <- 1:5

# Feature: Uddannelse
# Levels: 1) Ingen formel uddannelse 2) Grundskole 3) Gymnasial uddannelse 4) Erhvervsfaglig uddannelse 5) Kort videregående uddannelse 6) Lang videregående uddannelse
c <- 1:6

# Feature: Tidligere beskæftigelse
#Levels: 1) Arbejdsløs 2) Tjener 3) Kok 4) Mekaniker 5) Butiksassistent 6) Salgskonsulent 7) Regnskabsmedarbejder 8) It-medarbejder 9) Analytiker 10) Ingeniør 11) Læge 
d <- 1:11

# Feature: Enkelskkundskaber
# Levels: 1) Taler ikke engelsk 2) Forstår engelsk 3) Kan føre en samtale på engelsk 4) Taler flydende engelsk
e <- 1:4

# Feature: Oprindelsesland
# Levels: 1) Tyskland 2) Polen 3) Tyrkiet 4) Kina 5) Somalia 6) Syrien 7) Irak
f <- 1:7

# Feature: Religion
# Levels: 1) Ikke troende 2) Kristen 3) Muslim
g <- 1:3

### 1.2 Expand grid to get all possible combinations of profiles
all <- expand.grid(a,
                   b, 
                   c,
                   d,
                   e,
                   f,
                   g)

##### 2.0 Impossible profiles ------------------------------------------------------------------------------------------------
### 2.1 Set a default of "Possible"
all$Profile <- "Possible"

### 2.2 Uddannelse og beskæftigelse
# Can't have ingen formel uddannelse and be regnskabsmedarbejder
all$Profile <- ifelse(all$Var3==1 & all$Var4==7, "Impossible", all$Profile)

# Can't have ingen formel uddannelse and be it-medarbejder
all$Profile <- ifelse(all$Var3==1 & all$Var4==8, "Impossible", all$Profile)

# Can't have ingen formel uddannelse and be analytiker
all$Profile <- ifelse(all$Var3==1 & all$Var4==9, "Impossible", all$Profile)

# Can't have ingen formel uddannelse and be ingeniør
all$Profile <- ifelse(all$Var3==1 & all$Var4==10, "Impossible", all$Profile)

# Can't have ingen formel uddannelse and be læge
all$Profile <- ifelse(all$Var3==1 & all$Var4==11, "Impossible", all$Profile)

# Can't have grundskole and be ingeniør
all$Profile <- ifelse(all$Var3==2 & all$Var4==10, "Impossible", all$Profile)

# Can't have grundskole and be læge
all$Profile <- ifelse(all$Var3==2 & all$Var4==11, "Impossible", all$Profile)

# Can't have gymnasial uddannelse and be ingeniør
all$Profile <- ifelse(all$Var3==3 & all$Var4==10, "Impossible", all$Profile)

# Can't have gymnasial uddannelse and be læge
all$Profile <- ifelse(all$Var3==3 & all$Var4==11, "Impossible", all$Profile)

# Can't have erhvervsuddannelse and be ingeniør
all$Profile <- ifelse(all$Var3==4 & all$Var4==10, "Impossible", all$Profile)

# Can't have erhvervsuddannelse and be læge
all$Profile <- ifelse(all$Var3==4 & all$Var4==11, "Impossible", all$Profile)

# Can't have kort videregående uddannelse and be ingeniør
all$Profile <- ifelse(all$Var3==5 & all$Var4==10, "Impossible", all$Profile)

# Can't have kort videregående uddannelse and be læge
all$Profile <- ifelse(all$Var3==5 & all$Var4==11, "Impossible", all$Profile)

### 2.4 Select only possible profiles
clean <- all %>% 
  filter(Profile=="Possible") %>% 
  select(-Profile)

##### 3.0 Make conjoint tasks ---------------------------------------------------------------------------------------------------------------
### 3.1 Randomly draw five tasks with two profiles in each 
# Sawtooth only allow a sample of 999
tasks <- bind_rows(replicate(n = 999, 
                             bind_rows(replicate(n = 5, 
                                                 expr = sample_n(tbl = clean,
                                                                 size = 2,
                                                                 replace = TRUE), 
                                                 simplify = FALSE), 
                                       .id = "Task"), 
                             simplify = FALSE), 
                   .id = "Version")

# Create a variable indicating profile placement
tasks$Concept <- as.character(1:2)

# Ensure the right placement of variables
tasks <- tasks %>% 
  select(Version, Task, Concept, starts_with("Var")) %>% 
  mutate(Id = as.character(1:nrow(.)))

##### 4.0 Finish design file preperations ---------------------------------------------------------------------------------------------------
### 4.1 Make vector with labels 
labels <- paste(paste("Attr",
                      sprintf(fmt = "%02d",
                              1:7)),
                c("Køn",
                  "Alder",
                  "Uddannelse",
                  "Tidligere beskæftigelse",
                  "Engelskkundskaber",
                  "Oprindelsesland",
                  "Religion"),
                sep = " - ")

### 4.2 Smash the labels on the sample
names(tasks)[4:10] <- labels

##### 5.0 Save to folder -------------------------------------------------------------------------------------------------------------------
write.xlsx(x = tasks,
           file = "03. Spørgeskema/02. Designfil/03. results/designfile (part 2).xlsx",
           sheetName = "Design file (part 2)",
           row.names = FALSE)