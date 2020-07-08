##### 1.0 Data ----------------------------------------------------------------------------------------------------------------------
### 1.1 Read data
# Data from directory
data <- read_sav("06. Data/01. Rådata/P10090_51272.sav") %>% 
  as_tibble()

# Select only completed interviews
data <- data %>% 
  filter(SurveyStatus==2)

# Delete variables we do not need
data <- data %>% 
  select(-starts_with("Respondent_O")) %>% 
  select(-starts_with("DataCollection_")) %>% 
  select(-starts_with("DataCleaning_")) %>% 
  select(-starts_with("UserAgent_")) %>% 
  select(-starts_with("TimeTracking")) %>%
  select(-SurveyStatus_errorDescription, -PanelName_tps, -LastQuestionStartTime, -LastQuestionEndTime, -TimeZone)

### 1.2 Read scheme for regions
region <- xlsx::read.xlsx(file = "06. Data/03. Datarens/01. data/postnr_til_regioner.xlsx",
                          sheetIndex = 1,
                          encoding = "UTF-8") %>% 
  drop_na() %>% 
  as_tibble()

##### 2.0 Recode variables -----------------------------------------------------------------------------------------------------------------
### 2.1 Get regions
data <- left_join(data, region, by = c("Q2b_l1" = "postnr"))

### 2.2 Make new variables from survey variables
data <- data %>% 
  mutate(køn = case_when(Q1==1 ~ "Mand",
                         Q1==2 ~ "Kvinde"),
         alder = case_when(Q2_l1>=18 & Q2_l1<=34 ~ "18-34 år",
                           Q2_l1>=35 & Q2_l1<=55 ~ "35-55 år",
                           Q2_l1>55 ~ "56 år eller derover"),
         køn_alder = paste(køn, alder, sep = " "),
         postnummer = as.character(Q2b_l1),
         uddannelse = factor(Q3,
                             levels = c(1, 2, 3, 4, 5, 6, 7, 8),
                             labels = c("Grundskole (folkeskole, privatskole, efterskole)",
                                        "Gymnasie, HF, studenterkursus",
                                        "Erhvervsgymnasium (HHX eller HTX)",
                                        "Erhvervsfaglig (f.eks. tømrer, frisør, kontorassistent)",
                                        "Kort videregående (f.eks. datamatiker, laborant)",
                                        "Mellemlang videregående (f.eks. teknikumingeniør, lærer)",
                                        "Bachelor (f.eks. 1. del af en lang videregående uddannelse)",
                                        "Lang videregående (f.eks. gymnasielærer, økonom, jurist)")),
         beskæftigelse = case_when(Q4==1 ~ "Lønmodtager",
                                   Q4==2 ~ "Selvstændig erhvervsdrivende",
                                   Q4==3 ~ "Medarbejdende ægtefælle",
                                   Q4==4 ~ "Værnepligt",
                                   Q4==5 ~ "Arbejdsløs",
                                   Q4==6 ~ "Hjemmearbejdende uden erhvervsarbejde",
                                   Q4==7 ~ "Pensionist (pga. alder)",
                                   Q4==8 ~ "Førtidspensionist",
                                   Q4==9 ~ "Efterløns- eller fleksydelsesmodtager",
                                   Q4==10 ~ "Langtidssyg",
                                   Q4==11 ~ "Skoleelev, studerende, kursist",
                                   Q4==12 ~ "Ude af erhverv i øvrigt"),
         parti = case_when(Q5==1 ~ "Socialdemokratiet",
                           Q5==2 ~ "Radikale Venstre",
                           Q5==3 ~ "Konservative Folkeparti",
                           Q5==4 ~ "Nye Borgerlige",
                           Q5==5 ~ "Klaus Riskær Pedersen",
                           Q5==6 ~ "Socialistisk Folkeparti",
                           Q5==7 ~ "Liberal Alliance",
                           Q5==8 ~ "Kristendemokraterne",
                           Q5==9 ~ "Dansk Folkeparti",
                           Q5==10 ~ "Stram Kurs",
                           Q5==11 ~ "Venstre",
                           Q5==12 ~ "Enhedslisten",
                           Q5==13 ~ "Alternativet",
                           Q5==14 ~ "Stemte blankt",
                           Q5==15 ~ "Stemte ikke",
                           Q5==16 ~ "Havde ikke stemmeret",
                           Q5==17 ~ "Vil ikke oplyse"),
         prime = case_when(random_prime==1 ~ "Økonomisk prime",
                           random_prime==2 ~ "Kulturelt prime"),
         prime_svar = ifelse(!is.na(economic), economic, cultural),
         prime_svar = factor(prime_svar, 
                             levels = c(1, 2, 3, 4),
                             labels = c("I høj grad", "I nogen grad", "I mindre grad", "Slet ikke")),
         manipulationstjek = case_when(Q6==1 ~ "Konsekvenser for dansk kultur, normer og værdier",
                                       Q6==2 ~ "Konsekvenser for Danmarks økonomi, velfærdssamfund og arbejdsmarked",
                                       Q6==3 ~ "Ingen af ovenstående"),
         region = case_when(region==1081 ~ "Nordjylland",
                            region==1082 ~ "Midtjylland",
                            region==1083 ~ "Syddanmark",
                            region==1084 ~ "Hovedstaden",
                            region==1085 ~ "Sjælland"),
         alder_num = Q2_l1) %>% 
  select(Respondent_Serial, Id, ActualSurveyStartTime, ActualSurveyEndTime, køn, alder, køn_alder, alder_num, postnummer, uddannelse,
         beskæftigelse, parti, prime, prime_svar, manipulationstjek, region)

##### 3.0 Save to folder -----------------------------------------------------------------------------------------------------------------
saveRDS(object = data,
        file = "06. Data/02. Rekodning/03. results/immigrant_recode.rds")
