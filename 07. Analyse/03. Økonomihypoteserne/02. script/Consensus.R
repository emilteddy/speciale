##### 1.0 Data ------------------------------------------------------------------------------------------------------------------------------
### 1.1 Read data in
data <- readRDS("06. Data/05. Merge/03. results/immigrant_final.rds")

### 1.2 Select only data from before the priming
conjoint <- data %>% 
  filter(priming==0)

### 1.3 Put labels on profile characteristics
attr(conjoint$køn, "label") <- "Køn"
attr(conjoint$alder, "label") <- "Alder"
attr(conjoint$uddannelse, "label") <- "Uddannelse"
attr(conjoint$beskæftigelse, "label") <- "Tidligere beskæftigelse"
attr(conjoint$sprog, "label") <- "Engelskkundskaber"
attr(conjoint$land, "label") <- "Oprindelsesland"
attr(conjoint$religion, "label") <- "Religion"

##### 2.0 Examining education ---------------------------------------------------------------------------------------------------------------
### 2.1 Fit marginal means
mm <- cj(data = conjoint,
           formula = choice ~ køn + alder + uddannelse + beskæftigelse + sprog + land + religion + uddannelse:beskæftigelse,
           id = ~Respondent_Serial,
           estimate = "mm",
           h0 = 0.5)

### 2.3 Visualise results
plot <- mm %>%  
  filter(feature=="Uddannelse") %>% 
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)))) +
  geom_point(fill = "black",
             size = 2) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 height = 0.2,
                 color = "black") +
  geom_vline(xintercept = 0.5,
             linetype = "longdash",
             color = "black") +
  xlab("Marginal means for Pr(støtte til indvandrer)") +
  ylab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") + 
  add_hygge()

# Create LaTeX output
output <- mm %>% 
  filter(feature=="Uddannelse")

output <- kable(x = output,
                format = "latex",
                digits = 2)

# Save results
write_file(x = output,
           path = "07. Analyse/03. Økonomihypoteserne/03. results/uddannelse.txt")


ggsave("07. Analyse/03. Økonomihypoteserne/03. results/uddannelse.eps", 
       plot = plot, 
       width = 7, 
       height = 5, 
       dpi = 320)

### 2.5 Fit AMCE
amce <- cj(data = conjoint,
           formula = choice ~ køn + alder + uddannelse + beskæftigelse + sprog + land + religion + uddannelse:beskæftigelse,
           id = ~Respondent_Serial,
           estimate = "amce")

### 2.6 Visualise results
plot <- amce %>%  
  filter(feature=="Uddannelse") %>% 
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)))) +
  geom_point(fill = "black",
             size = 2) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 height = 0.2,
                 color = "black") +
  geom_vline(xintercept = 0.0,
             linetype = "longdash",
             color = "black") +
  xlab("AMCE for Pr(støtte til indvandrer)") +
  ylab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") + 
  add_hygge()

# Create LaTeX output
output <- amce %>% 
  filter(feature=="Uddannelse")

output <- kable(x = output,
                format = "latex",
                digits = 2)

# Save results
write_file(x = output,
           path = "07. Analyse/03. Økonomihypoteserne/03. results/ingenformeluddannelse.txt")


ggsave("07. Analyse/03. Økonomihypoteserne/03. results/ingenformeluddannelse.eps", 
       plot = plot, 
       width = 7, 
       height = 5, 
       dpi = 320)

### 2.7 Make erhvervsfaglig uddannelse baseline
# Recode
conjoint <- conjoint %>% 
  mutate(Uddannelse = factor(uddannelse,
                             levels = c("Erhvervsfaglig uddannelse",
                                        "Kort videregående uddannelse",
                                        "Lang videregående uddannelse",
                                        "Ingen formel uddannelse",
                                        "Grundskole",
                                        "Gymnasial uddannelse")))

# Label
attr(conjoint$Uddannelse, "label") <- "Uddannelse"

### 2.8 Fit model again
amce <- cj(data = conjoint,
           formula = choice ~ køn + alder + Uddannelse + beskæftigelse + sprog + land + religion + Uddannelse:beskæftigelse,
           id = ~Respondent_Serial,
           estimate = "amce")

### 2.9 Visualise general results
plot <- amce %>%  
  filter(feature=="Uddannelse" & level!="Ingen formel uddannelse" & level!="Grundskole" & level!="Gymnasial uddannelse") %>% 
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)))) +
  geom_point(fill = "black",
             size = 2) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 height = 0.2,
                 color = "black") +
  geom_vline(xintercept = 0.0,
             linetype = "longdash",
             color = "black") +
  xlab("AMCE for Pr(støtte til indvandrer)") +
  ylab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") + 
  add_hygge()

# Create LaTeX output
output <- amce %>% 
  filter(feature=="Uddannelse" & level!="Ingen formel uddannelse" & level!="Grundskole" & level!="Gymnasial uddannelse")

output <- kable(x = output,
                format = "latex",
                digits = 2)

# Save results
write_file(x = output,
           path = "07. Analyse/03. Økonomihypoteserne/03. results/erhvervsfaglig.txt")


ggsave("07. Analyse/03. Økonomihypoteserne/03. results/erhvervsfaglig.eps", 
       plot = plot, 
       width = 7, 
       height = 5, 
       dpi = 320)

##### 3.0 Education as a function of the respondent's own education ---------------------------------------------------------------------
### 3.1 Recode education variable
conjoint <- conjoint %>% 
  mutate(resp_uddannelse2 = case_when(resp_uddannelse=="Grundskole (folkeskole, privatskole, efterskole)" ~ "Grundskoleuddannelse",
                                      resp_uddannelse=="Gymnasie, HF, studenterkursus" ~ "Gymnasial uddannelse",
                                      resp_uddannelse=="Erhvervsgymnasium (HHX eller HTX)" ~ "Gymnasial uddannelse",
                                      resp_uddannelse=="Erhvervsfaglig (f.eks. tømrer, frisør, kontorassistent)" ~ "Erhvervsfaglig uddannelse",
                                      resp_uddannelse=="Kort videregående (f.eks. datamatiker, laborant)" ~ "Kort videregående uddannelse",
                                      resp_uddannelse=="Mellemlang videregående (f.eks. teknikumingeniør, lærer)" ~ "Mellemlang videregående uddannelse",
                                      resp_uddannelse=="Bachelor (f.eks. 1. del af en lang videregående uddannelse)" ~ "Lang videregående uddannelse",
                                      resp_uddannelse=="Lang videregående (f.eks. gymnasielærer, økonom, jurist)" ~ "Lang videregående uddannelse"))

### 3.1 ANOVA
anova <- cj_anova(data = conjoint,
                  formula = choice ~ køn + alder + uddannelse + beskæftigelse + sprog + land + religion + uddannelse:beskæftigelse,
                  by = ~resp_uddannelse2,
                  id = ~Respondent_Serial)

### 3.2 Fit model
mm <- cj(data = conjoint,
         formula = choice ~ køn + alder + uddannelse + beskæftigelse + sprog + land + religion + uddannelse:beskæftigelse,
         by = ~resp_uddannelse2,
         id = ~Respondent_Serial,
         estimate = "mm",
         h0 = 0.5)

### 3.3 Visualise results
plot <- mm %>% 
  filter(feature=="Uddannelse") %>% 
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)),
                shape = resp_uddannelse2)) +
  geom_point(position = position_dodge(width = 0.5),
             size = 2) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 position = position_dodge(width = 0.5),
                 height = 0.5) +
  geom_vline(xintercept = 0.5,
             linetype = "longdash",
             color = "black") +
  xlab("Marginal means for Pr(støtte til indvandrer)") +
  ylab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") + 
  add_hygge() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  guides(shape = guide_legend(nrow = 3,
                              byrow = TRUE)) +
  scale_shape_discrete(breaks = c("Grundskoleuddannelse",
                                  "Gymnasial uddannelse",
                                  "Erhvervsfaglig uddannelse",
                                  "Kort videregående uddannelse",
                                  "Mellemlang videregående uddannelse",
                                  "Lang videregående uddannelse"))
# Create LaTeX output
output <- mm %>% 
  filter(feature=="Uddannelse")

output <- kable(x = output,
                format = "latex",
                digits = 2)

# Save results
write_file(x = output,
           path = "07. Analyse/03. Økonomihypoteserne/03. results/uddannelse_x_uddannelse.txt")


ggsave("07. Analyse/03. Økonomihypoteserne/03. results/uddannelse_x_uddannelse.eps", 
       plot = plot, 
       width = 7, 
       height = 5, 
       dpi = 320)

### 3.3 Estimate AMCE 
amce <- cj(data = conjoint,
           formula = choice ~ køn + alder + uddannelse + beskæftigelse + sprog + land + religion + uddannelse:beskæftigelse,
           by = ~resp_uddannelse2,
           id = ~Respondent_Serial,
           estimate = "amce")

### 3.4 Visualise results
plot <- amce %>% 
  filter(feature=="Uddannelse") %>% 
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)),
                shape = resp_uddannelse2)) +
  geom_point(position = position_dodge(width = 0.5),
             size = 2) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 position = position_dodge(width = 0.5),
                 height = 0.5) +
  geom_vline(xintercept = 0.0,
             linetype = "longdash",
             color = "black") +
  xlab("AMCE for Pr(støtte til indvandrer)") +
  ylab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") + 
  xlim(-0.2, 0.4) +
  add_hygge() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  guides(shape = guide_legend(nrow = 3,
                              byrow = TRUE)) +
  scale_shape_discrete(breaks = c("Grundskoleuddannelse",
                                  "Gymnasial uddannelse",
                                  "Erhvervsfaglig uddannelse",
                                  "Kort videregående uddannelse",
                                  "Mellemlang videregående uddannelse",
                                  "Lang videregående uddannelse"))

# Create LaTeX output
output <- amce %>% 
  filter(feature=="Uddannelse")

output <- kable(x = output,
                format = "latex",
                digits = 2)

# Save results
write_file(x = output,
           path = "07. Analyse/03. Økonomihypoteserne/03. results/uddannelse_x_uddannelse_amce.txt")


ggsave("07. Analyse/03. Økonomihypoteserne/03. results/uddannelse_x_uddannelse_amce.eps", 
       plot = plot, 
       width = 7, 
       height = 5, 
       dpi = 320)

### 3.5 Estimate difference
# Select data
df <- conjoint %>% 
  filter(resp_uddannelse2=="Grundskoleuddannelse" | resp_uddannelse2=="Lang videregående uddannelse") %>% 
  mutate(udd = ifelse(resp_uddannelse2=="Grundskoleuddannelse", 0, 1))

# Labels
attr(df$køn, "label") <- "Køn"
attr(df$alder, "label") <- "Alder"
attr(df$uddannelse, "label") <- "Uddannelse"
attr(df$beskæftigelse, "label") <- "Tidligere beskæftigelse"
attr(df$sprog, "label") <- "Engelskkundskaber"
attr(df$land, "label") <- "Oprindelsesland"
attr(df$religion, "label") <- "Religion"

# Fit model
diff <- mm_diffs(data = df,
                 formula = choice ~ køn + alder + uddannelse + beskæftigelse + sprog + land + religion + uddannelse:beskæftigelse,
                 by = ~udd,
                 id = ~Respondent_Serial,
                 alpha = 0.975)

# Visualise results
plot <- diff %>%
  filter(feature=="Uddannelse") %>%
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)))) +
  geom_point(fill = "black",
             size = 2) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 height = 0.2,
                 color = "black") +
  geom_vline(xintercept = 0,
             linetype = "longdash",
             color = "black") +
  xlab("Forskel i marginal means mellem\n Pr(Lang videregående uddannelse|støtte til indvandrer) og\n Pr(Grundskole|støtte til indvandrer)") +
  ylab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") + 
  add_hygge()

# Create LaTeX output
output <- diff %>% 
  filter(feature=="Uddannelse")

output <- kable(x = output,
                format = "latex",
                digits = 2)

# Save results
write_file(x = output,
           path = "07. Analyse/03. Økonomihypoteserne/03. results/mm_diff_uddannelse.txt")


ggsave("07. Analyse/03. Økonomihypoteserne/03. results/mm_diff_uddannelse.eps", 
       plot = plot, 
       width = 7, 
       height = 5, 
       dpi = 320)

##### 4.0 Examining previous occupation -------------------------------------------------------------------------------------------------
### 4.1 Order characteristics
# Recode
conjoint <- conjoint %>% 
  mutate(Beskæftigelse = factor(beskæftigelse,
                                levels = c("Arbejdsløs",
                                           "Butiksassistent",
                                           "Salgskonsulent",
                                           "Tjener",
                                           "Mekaniker",
                                           "Kok",
                                           "Regnskabsmedarbejder",
                                           "Analytiker",
                                           "It-medarbejder",
                                           "Ingeniør",
                                           "Læge")))

# Label
attr(conjoint$Beskæftigelse, "label") <- "Tidligere beskæftigelse"

### 3.2 Fit model
mm <- cj(data = conjoint,
         formula = choice ~ køn + alder + uddannelse + Beskæftigelse + sprog + land + religion + uddannelse:Beskæftigelse,
         id = ~Respondent_Serial,
         estimate = "mm",
         h0 = 0.5)

### 5.3 Visualise general results
plot <- mm %>%  
  filter(feature=="Tidligere beskæftigelse") %>% 
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)))) +
  geom_point(fill = "black",
             size = 2) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 height = 0.2,
                 color = "black") +
  geom_vline(xintercept = 0.5,
             linetype = "longdash",
             color = "black") +
  xlab("Marginal means for Pr(støtte til indvandrer)") +
  ylab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") + 
  add_hygge()

# Create LaTeX output
output <- mm %>% 
  filter(feature=="Tidligere beskæftigelse")

output <- kable(x = output,
                format = "latex",
                digits = 2)

# Save results
write_file(x = output,
           path = "07. Analyse/03. Økonomihypoteserne/03. results/beskæftigelse.txt")


ggsave("07. Analyse/03. Økonomihypoteserne/03. results/beskæftigelse.eps", 
       plot = plot, 
       width = 7, 
       height = 5, 
       dpi = 320)

### 5.5 Estimate AMCE
amce <- cj(data = conjoint,
           formula = choice ~ køn + alder + uddannelse + Beskæftigelse + sprog + land + religion + uddannelse:Beskæftigelse,
           id = ~Respondent_Serial,
           estimate = "amce")

### 5.6 Visualise lower bound <= 50 %
plot <- amce %>%  
  filter(feature=="Tidligere beskæftigelse") %>% 
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)))) +
  geom_point(fill = "black",
             size = 2) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 height = 0.2,
                 color = "black") +
  geom_vline(xintercept = 0.0,
             linetype = "longdash",
             color = "black") +
  xlab("AMCE for Pr(støtte til indvandrer)") +
  ylab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") + 
  add_hygge()

# Create LaTeX output
output <- amce %>% 
  filter(feature=="Tidligere beskæftigelse")

output <- kable(x = output,
                format = "latex",
                digits = 2)

# Save results
write_file(x = output,
           path = "07. Analyse/03. Økonomihypoteserne/03. results/beskæftigelse_amce.txt")


ggsave("07. Analyse/03. Økonomihypoteserne/03. results/beskæftigelse_amce.eps", 
       plot = plot, 
       width = 7, 
       height = 5, 
       dpi = 320)
