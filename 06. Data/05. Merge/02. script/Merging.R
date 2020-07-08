##### 1.0 Data ----------------------------------------------------------------------------------------------------------------------
### 1.1 Survey data
survey <- readRDS(file = "06. Data/03. Datarens/03. results/immigrant_clean.rds") %>% 
  as_tibble()

### 1.2 Design files
# Conjoint experiment prior to the priming
pre_design <- openxlsx::read.xlsx(xlsxFile = "03. Spørgeskema/02. Designfil/03. results/designfile (part 1).xlsx")

# Conjoint experiment after the priming
post_design <- openxlsx::read.xlsx(xlsxFile = "03. Spørgeskema/02. Designfil/03. results/designfile (part 2).xlsx")

### 1.3 Output from the conjoint experiment
# Conjoint experiment prior to the priming
pre_output <- read_csv(file = "06. Data/05. Merge/01. data/10090_CBC_PRE_data.csv")

#Conjoint experiment after the priming
post_output <- read_csv(file = "06. Data/05. Merge/01. data/10090_CBC_POST_data.csv")

##### 2.0 Make design files ready for merge ----------------------------------------------------------------------------------------
### 2.1 Add labels
# Design file 1
pre_design <- pre_design %>% 
  mutate(køn = factor(`Attr.01.-.Køn`,
                      levels = c(1, 2),
                      labels = c("Mand",
                                 "Kvinde")),
         alder = factor(`Attr.02.-.Alder`,
                        levels = c(1, 2, 3, 4, 5),
                        labels = c("25 år", 
                                   "31 år",
                                   "37 år",
                                   "43 år",
                                   "55 år")),
         uddannelse = factor(`Attr.03.-.Uddannelse`,
                             levels = c(1, 2, 3, 4, 5, 6),
                             labels = c("Ingen formel uddannelse",
                                        "Grundskole",
                                        "Gymnasial uddannelse",
                                        "Erhvervsfaglig uddannelse",
                                        "Kort videregående uddannelse",
                                        "Lang videregående uddannelse")),
         beskæftigelse = factor(`Attr.04.-.Tidligere.beskæftigelse`,
                                levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
                                labels = c("Arbejdsløs",
                                           "Tjener",
                                           "Kok",
                                           "Mekaniker",
                                           "Butiksassistent",
                                           "Salgskonsulent",
                                           "Regnskabsmedarbejder",
                                           "It-medarbejder",
                                           "Analytiker",
                                           "Ingeniør",
                                           "Læge")),
         sprog = factor(`Attr.05.-.Engelskkundskaber`,
                        levels = c(1, 2, 3, 4),
                        labels = c("Taler og forstår ikke engelsk",
                                   "Forstår, men taler ikke engelsk",
                                   "Kan føre en samtale på engelsk",
                                   "Taler flydende engelsk")),
         land = factor(`Attr.06.-.Oprindelsesland`,
                       levels = c(1, 2, 3, 4, 5, 6, 7),
                       labels = c("Polen",
                                  "Tyskland",
                                  "Somalia",
                                  "Kina",
                                  "Syrien",
                                  "Tyrkiet",
                                  "Irak")),
         religion = factor(`Attr.07.-.Religion`,
                           levels = c(1, 2, 3),
                           labels = c("Ikke troende",
                                      "Muslim",
                                      "Kristen"))) %>% 
  select(-Id, -starts_with("Attr"))

# Design file 2
post_design <- post_design %>% 
  mutate(køn = factor(`Attr.01.-.Køn`,
                      levels = c(1, 2),
                      labels = c("Mand",
                                 "Kvinde")),
         alder = factor(`Attr.02.-.Alder`,
                        levels = c(1, 2, 3, 4, 5),
                        labels = c("25 år", 
                                   "31 år",
                                   "37 år",
                                   "43 år",
                                   "55 år")),
         uddannelse = factor(`Attr.03.-.Uddannelse`,
                             levels = c(1, 2, 3, 4, 5, 6),
                             labels = c("Ingen formel uddannelse",
                                        "Grundskole",
                                        "Gymnasial uddannelse",
                                        "Erhvervsfaglig uddannelse",
                                        "Kort videregående uddannelse",
                                        "Lang videregående uddannelse")),
         beskæftigelse = factor(`Attr.04.-.Tidligere.beskæftigelse`,
                                levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
                                labels = c("Arbejdsløs",
                                           "Tjener",
                                           "Kok",
                                           "Mekaniker",
                                           "Butiksassistent",
                                           "Salgskonsulent",
                                           "Regnskabsmedarbejder",
                                           "It-medarbejder",
                                           "Analytiker",
                                           "Ingeniør",
                                           "Læge")),
         sprog = factor(`Attr.05.-.Engelskkundskaber`,
                        levels = c(1, 2, 3, 4),
                        labels = c("Taler og forstår ikke engelsk",
                                   "Forstår, men taler ikke engelsk",
                                   "Kan føre en samtale på engelsk",
                                   "Taler flydende engelsk")),
         land = factor(`Attr.06.-.Oprindelsesland`,
                       levels = c(1, 2, 3, 4, 5, 6, 7),
                       labels = c("Polen",
                                  "Tyskland",
                                  "Somalia",
                                  "Kina",
                                  "Syrien",
                                  "Tyrkiet",
                                  "Irak")),
         religion = factor(`Attr.07.-.Religion`,
                           levels = c(1, 2, 3),
                           labels = c("Ikke troende",
                                      "Muslim",
                                      "Kristen"))) %>% 
  select(-Id, -starts_with("Attr"))

### 2.2 Make an unique indicator for each profile
# Deisgn file 1
pre_design <- pre_design %>% 
  mutate(pre_indicator = paste(Version, Task, Concept, sep = "-")) %>% 
  select(pre_indicator, køn, alder, uddannelse, beskæftigelse, sprog, land, religion)

# Deisgn file 2
post_design <- post_design %>% 
  mutate(post_indicator = paste(Version, Task, Concept, sep = "-")) %>% 
  select(post_indicator, køn, alder, uddannelse, beskæftigelse, sprog, land, religion)

##### 3.0 Make the output from the conjoint experiment ready for merge ------------------------------------------------------------------
### 3.1 Remove duplicates
# CJ 1
pre_output <- pre_output %>%
  drop_na(sys_CBCVersion_CBCStudies) %>% 
  arrange( desc(sys_StartTime) ) %>%
  distinct(Respondent_Serial, 
           .keep_all = TRUE)

# CJ 2
post_output <- post_output %>%
  drop_na(sys_CBCVersion_CBCStudies) %>% 
  arrange( desc(sys_StartTime) ) %>%
  distinct(Respondent_Serial, 
           .keep_all = TRUE)

### 3.2 Select variables needed
# CJ 1
pre_output <- pre_output %>% 
  select(Respondent_Serial, sys_CBCVersion_CBCStudies, starts_with("CBCStudies_Random"), ends_with("r1"), ends_with("r2"))

# CJ 2
post_output <- post_output %>% 
  select(Respondent_Serial, sys_CBCVersion_CBCStudies, starts_with("CBCStudies_Random"), ends_with("r1"), ends_with("r2"))

### 3.3 Subset output on id's from the survey
# Get id's from the survey
id <- as.character(str_trim(survey$Respondent_Serial))

# CJ 1
pre_output <- pre_output %>% 
  mutate(Respondent_Serial = as.character(Respondent_Serial)) %>% 
  filter(Respondent_Serial %in% id)

# CJ 2
post_output <- post_output %>% 
  mutate(Respondent_Serial = as.character(Respondent_Serial)) %>% 
  filter(Respondent_Serial %in% id)

### 3.4 Make profile choice and rating 
# CJ 1
pre_output_choice <- pre_output %>% 
  mutate(profil_1 = ifelse(CBCStudies_Random1==2, 0, 1),
         profil_2 = ifelse(CBCStudies_Random1!=2, 0, 1),
         profil_3 = ifelse(CBCStudies_Random2==2, 0, 1),
         profil_4 = ifelse(CBCStudies_Random2!=2, 0, 1),
         profil_5 = ifelse(CBCStudies_Random3==2, 0, 1),
         profil_6 = ifelse(CBCStudies_Random3!=2, 0, 1),
         profil_7 = ifelse(CBCStudies_Random4==2, 0, 1),
         profil_8 = ifelse(CBCStudies_Random4!=2, 0, 1),
         profil_9 = ifelse(CBCStudies_Random5==2, 0, 1),
         profil_10 = ifelse(CBCStudies_Random5!=2, 0, 1)) %>% 
  select(Respondent_Serial, sys_CBCVersion_CBCStudies, starts_with("profil"))

pre_output_choice <- pre_output_choice %>% 
  pivot_longer(cols = -c("Respondent_Serial", "sys_CBCVersion_CBCStudies"),
               names_to = "profiler",
               values_to = "choice") %>% 
  arrange(Respondent_Serial) %>% 
  as_tibble()

pre_output_rating <- pre_output %>% 
  mutate(profil_1 = s1grid1_r1,
         profil_2 = s1grid1_r2,
         profil_3 = s2grid1_r1,
         profil_4 = s2grid1_r2,
         profil_5 = s3grid1_r1,
         profil_6 = s3grid1_r2,
         profil_7 = s4grid1_r1,
         profil_8 = s4grid1_r2,
         profil_9 = s5grid1_r1,
         profil_10 = s5grid1_r2) %>% 
  select(Respondent_Serial, sys_CBCVersion_CBCStudies, starts_with("profil"))

pre_output_rating <- pre_output_rating %>% 
  pivot_longer(cols = -c("Respondent_Serial", "sys_CBCVersion_CBCStudies"),
               names_to = "profiler",
               values_to = "rating") %>% 
  arrange(Respondent_Serial) %>% 
  as_tibble()

pre_output_combine <- left_join(pre_output_choice, pre_output_rating, by = c("Respondent_Serial", "sys_CBCVersion_CBCStudies", "profiler"))

# CJ 2
post_output_choice <- post_output %>% 
  mutate(profil_11 = ifelse(CBCStudies_Random1==2, 0, 1),
         profil_12 = ifelse(CBCStudies_Random1!=2, 0, 1),
         profil_13 = ifelse(CBCStudies_Random2==2, 0, 1),
         profil_14 = ifelse(CBCStudies_Random2!=2, 0, 1),
         profil_15 = ifelse(CBCStudies_Random3==2, 0, 1),
         profil_16 = ifelse(CBCStudies_Random3!=2, 0, 1),
         profil_17 = ifelse(CBCStudies_Random4==2, 0, 1),
         profil_18 = ifelse(CBCStudies_Random4!=2, 0, 1),
         profil_19 = ifelse(CBCStudies_Random5==2, 0, 1),
         profil_20 = ifelse(CBCStudies_Random5!=2, 0, 1)) %>% 
  select(Respondent_Serial, sys_CBCVersion_CBCStudies, starts_with("profil"))

post_output_choice <- post_output_choice %>% 
  pivot_longer(cols = -c("Respondent_Serial", "sys_CBCVersion_CBCStudies"),
               names_to = "profiler",
               values_to = "choice") %>% 
  arrange(Respondent_Serial) %>% 
  as_tibble()

post_output_rating <- post_output %>% 
  mutate(profil_11 = s1grid1_r1,
         profil_12 = s1grid1_r2,
         profil_13 = s2grid1_r1,
         profil_14 = s2grid1_r2,
         profil_15 = s3grid1_r1,
         profil_16 = s3grid1_r2,
         profil_17 = s4grid1_r1,
         profil_18 = s4grid1_r2,
         profil_19 = s5grid1_r1,
         profil_20 = s5grid1_r2) %>% 
  select(Respondent_Serial, sys_CBCVersion_CBCStudies, starts_with("profil"))

post_output_rating <- post_output_rating %>% 
  pivot_longer(cols = -c("Respondent_Serial", "sys_CBCVersion_CBCStudies"),
               names_to = "profiler",
               values_to = "rating") %>% 
  arrange(Respondent_Serial) %>% 
  as_tibble()

post_output_combine <- left_join(post_output_choice, post_output_rating, by = c("Respondent_Serial", "sys_CBCVersion_CBCStudies", "profiler"))

### 3.5 Make Task and Concept variable and make the unique indicator out of it
# CJ 1
pre_output_combine <- pre_output_combine %>% 
  mutate(Task = case_when(profiler=="profil_1" | profiler=="profil_2" ~ 1,
                          profiler=="profil_3" | profiler=="profil_4" ~ 2,
                          profiler=="profil_5" | profiler=="profil_6" ~ 3,
                          profiler=="profil_7" | profiler=="profil_8" ~ 4,
                          profiler=="profil_9" | profiler=="profil_10" ~ 5),
         Concept = ifelse(profiler=="profil_2" | profiler=="profil_4" | profiler=="profil_6" | profiler=="profil_8" | profiler=="profil_10",
                          2,
                          1),
         pre_indicator = paste(sys_CBCVersion_CBCStudies, Task, Concept, sep = "-")) %>% 
  select(Respondent_Serial, pre_indicator, profiler, choice, rating)

# CJ 2
post_output_combine <- post_output_combine %>% 
  mutate(Task = case_when(profiler=="profil_11" | profiler=="profil_12" ~ 1,
                          profiler=="profil_13" | profiler=="profil_14" ~ 2,
                          profiler=="profil_15" | profiler=="profil_16" ~ 3,
                          profiler=="profil_17" | profiler=="profil_18" ~ 4,
                          profiler=="profil_19" | profiler=="profil_20" ~ 5),
         Concept = ifelse(profiler=="profil_12" | profiler=="profil_14" | profiler=="profil_16" | profiler=="profil_18" | profiler=="profil_20",
                          2,
                          1),
         post_indicator = paste(sys_CBCVersion_CBCStudies, Task, Concept, sep = "-")) %>% 
  select(Respondent_Serial, post_indicator, profiler, choice, rating)

##### 4.0 Merge with design file -----------------------------------------------------------------------------------------------------------------
### 4.1 Merge pre
# Merge output and design plus transform the unique indicator to a priming indicator
pre_conjoint <- pre_output_combine %>% 
  left_join(., pre_design, by = "pre_indicator") %>% 
  rename(priming = pre_indicator) %>% 
  mutate(priming = 0)

### 4.2 Merge post
# Merge output and design plus transform the unique indicator to a priming indicator
post_conjoint <- post_output_combine %>% 
  left_join(., post_design, by = "post_indicator") %>% 
  rename(priming = post_indicator) %>% 
  mutate(priming = 1)

##### 5.0 Merge the conjoint experiment before priming with the one after ------------------------------------------------------------------------
### 5.1 Stack the two dataset and arrange by id 
conjoint <- pre_conjoint %>% 
  bind_rows(., post_conjoint) %>% 
  arrange(Respondent_Serial)

##### 6.0 Merge the conjoint experiment with survey data -----------------------------------------------------------------------------------------
### 6.1 Make survey data ready
# Select meta data in survey
survey <- survey %>% 
  mutate(Respondent_Serial = as.character(Respondent_Serial))

# Change names of variables such that you know it is meta data
names(survey)[2:17] <- paste("resp", names(survey)[2:17], sep = "_")

### 6.2 Merge conjoint and survey
conjoint <- left_join(conjoint, survey, by = "Respondent_Serial")

##### 7.0 Save to folder -------------------------------------------------------------------------------------------------------------------------
saveRDS(object = conjoint,
        file = "06. Data/05. Merge/03. results/immigrant_final.rds")