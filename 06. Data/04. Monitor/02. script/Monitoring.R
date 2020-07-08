##### 1.0 Data ----------------------------------------------------------------------------------------------------------------------
### 1.1 Data to monitor
data <- readRDS(file = "06. Data/03. Datarens/03. results/immigrant_clean.rds") %>% 
  as_tibble()

### 1.2 Population matrices
# Global 
pop_matrix <- readRDS(file = "06. Data/04. Monitor/01. data/populationsmatricer_global.rds")

# Extract Denmark
population <- pop_matrix[["Danmark"]]

##### 2.0 Monitor variables ---------------------------------------------------------------------------------------------------------
### 2.1 Select variables
# Population
variable <- c("køn_alder",
              "region")

# Sample
group <- c(quo(køn_alder),
           quo(region))

### 2.2 Loop 
# Make count variable
n <- 0

# Delete existing file
wd <- "06. Data/04. Monitor/03. results/immigrant_monitor.xlsx"

if (file.exists(wd)) {
  file.remove(wd)
}

# Do loop
for (i in variable) {
  target <- population[[i]]
  names(target) <- c("Strata", "Population")
  n <- n + 1
  
  temp <- data %>% 
    group_by(!!group[[n]]) %>% 
    tally() %>% 
    transmute(Strata = !!group[[n]],
              Sample = n/sum(n))
  
  contain <- left_join(target, temp, by = "Strata")
  
  result <- contain %>% 
    transmute(Strata = Strata,
              Population = Population,
              Sample = Sample,
              Difference = Sample - Population)
  
  xlsx::write.xlsx(x = result,
                   file = "06. Data/04. Monitor/03. results/immigrant_monitor.xlsx",
                   sheetName = paste0(i),
                   append = TRUE)
}

##### 2.0 Distributions for report -----------------------------------------------------------------------------------------------------
### 2.1 Gender
# Make dataframe
df <- data %>% 
  group_by(køn) %>% 
  tally() %>% 
  mutate(andel = (n / sum(n))*100,
         feature = "Køn")

# Visualise result
plot <- df %>% 
  ggplot(., aes(x = køn,
                y = andel)) +
  geom_col(fill = "black") +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, 100)) +
  xlab("") +
  ylab("Procent") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") + 
  coord_flip() +
  add_hygge()

# Save results
ggsave("06. Data/04. Monitor/03. results/køn.eps", 
       plot = plot, 
       width = 7, 
       height = 5, 
       dpi = 320)

### 2.2 Age
# Make dataframe
df <- data %>% 
  group_by(alder) %>% 
  tally() %>% 
  mutate(andel = (n / sum(n))*100,
         feature = "Alder")

# Visualise result
plot <- df %>% 
  ggplot(., aes(x = reorder(alder, desc(alder)),
                y = andel)) +
  geom_col(fill = "black") +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, 100)) +
  xlab("") +
  ylab("Procent") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") + 
  coord_flip() +
  add_hygge()

# Save results
ggsave("06. Data/04. Monitor/03. results/alder.eps", 
       plot = plot, 
       width = 7, 
       height = 5, 
       dpi = 320)

### 2.3 Education
# Make dataframe
df <- data %>% 
  mutate(uddannelse = case_when(uddannelse=="Grundskole (folkeskole, privatskole, efterskole)" ~ "Grundskole",
                                uddannelse=="Gymnasie, HF, studenterkursus" ~ "Gymnasium",
                                uddannelse=="Erhvervsgymnasium (HHX eller HTX)" ~ "Gymnasium",
                                uddannelse=="Erhvervsfaglig (f.eks. tømrer, frisør, kontorassistent)" ~ "Erhvervsfaglig",
                                uddannelse=="Kort videregående (f.eks. datamatiker, laborant)" ~ "KVU",
                                uddannelse=="Mellemlang videregående (f.eks. teknikumingeniør, lærer)" ~ "MVU",
                                uddannelse=="Bachelor (f.eks. 1. del af en lang videregående uddannelse)" ~ "LVU",
                                uddannelse=="Lang videregående (f.eks. gymnasielærer, økonom, jurist)" ~ "LVU")) %>% 
  group_by(uddannelse) %>% 
  tally() %>% 
  mutate(andel = (n / sum(n))*100,
         feature = "Uddannelse") %>% 
  ungroup() %>% 
  mutate(uddannelse = factor(uddannelse,
                             levels = c("Grundskole",
                                        "Gymnasium",
                                        "Erhvervsfaglig",
                                        "KVU",
                                        "MVU",
                                        "LVU")))
# Visualise result
plot <- df %>% 
  ggplot(., aes(x = reorder(uddannelse, desc(uddannelse)),
                y = andel)) +
  geom_col(fill = "black") +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, 50)) +
  xlab("") +
  ylab("Procent") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") + 
  coord_flip() +
  add_hygge()

# Save results
ggsave("06. Data/04. Monitor/03. results/uddannelse.eps", 
       plot = plot, 
       width = 7, 
       height = 5, 
       dpi = 320)

### 2.4 Occupation
# Make dataframe
df <- data %>% 
  mutate(beskæftigelse = case_when(beskæftigelse=="Arbejdsløs" ~ "Arbejdsløs",
                                   beskæftigelse=="Lønmodtager" ~ "Lønmodtager",
                                   beskæftigelse=="Selvstændig erhvervsdrivende" ~ "Selvstændig",
                                   TRUE ~ "Ude af erhverv")) %>% 
  group_by(beskæftigelse) %>% 
  tally() %>% 
  mutate(andel = (n / sum(n))*100,
         feature = "Beskæftigelse")

# Visualise result
plot <- df %>% 
  ggplot(., aes(x = reorder(beskæftigelse, -andel),
                y = andel)) +
  geom_col(fill = "black") +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, 50)) +
  xlab("") +
  ylab("Procent") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") + 
  coord_flip() +
  add_hygge()

# Save results
ggsave("06. Data/04. Monitor/03. results/beskæftigelse.eps", 
       plot = plot, 
       width = 7, 
       height = 5, 
       dpi = 320)

### 2.6 Party
# Make dataframe
df <- data %>% 
  group_by(parti) %>% 
  tally() %>% 
  mutate(andel = (n / sum(n))*100,
         feature = "Parti")

# Visualise result
plot <- df %>% 
  ggplot(., aes(x = reorder(parti, -andel),
                y = andel)) +
  geom_col(fill = "black") +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, 50)) +
  xlab("") +
  ylab("Procent") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") + 
  coord_flip() +
  add_hygge()

# Save results
ggsave("06. Data/04. Monitor/03. results/parti.eps", 
       plot = plot, 
       width = 7, 
       height = 5, 
       dpi = 320)

### 2.7 Region
# Make dataframe
df <- data %>% 
  filter(!is.na(region)) %>% 
  group_by(region) %>% 
  tally() %>% 
  mutate(andel = (n / sum(n))*100,
         feature = "Region")

# Visualise result
plot <- df %>% 
  ggplot(., aes(x = reorder(region, -andel),
                y = andel)) +
  geom_col(fill = "black") +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, 50)) +
  xlab("") +
  ylab("Procent") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") + 
  coord_flip() +
  add_hygge()

# Save results
ggsave("06. Data/04. Monitor/03. results/region.eps", 
       plot = plot, 
       width = 7, 
       height = 5, 
       dpi = 320)