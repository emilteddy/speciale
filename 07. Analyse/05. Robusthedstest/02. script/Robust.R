##### 1.0 Data ------------------------------------------------------------------------------------------------------------------------------
### 1.1 Read data in
data <- readRDS("06. Data/05. Merge/03. results/immigrant_final.rds")

### 1.2 Data from before the priming
# Select
conjoint <- data %>% 
  filter(priming==0)

# Put labels on profile characteristics
attr(conjoint$køn, "label") <- "Køn"
attr(conjoint$alder, "label") <- "Alder"
attr(conjoint$uddannelse, "label") <- "Uddannelse"
attr(conjoint$beskæftigelse, "label") <- "Tidligere beskæftigelse"
attr(conjoint$sprog, "label") <- "Engelskkundskaber"
attr(conjoint$land, "label") <- "Oprindelsesland"
attr(conjoint$religion, "label") <- "Religion"

### 1.3 Data from after priming
# Select
priming <- data %>% 
  filter(priming==1)

# Put labels on profile characteristics
attr(priming$køn, "label") <- "Køn"
attr(priming$alder, "label") <- "Alder"
attr(priming$uddannelse, "label") <- "Uddannelse"
attr(priming$beskæftigelse, "label") <- "Tidligere beskæftigelse"
attr(priming$sprog, "label") <- "Engelskkundskaber"
attr(priming$land, "label") <- "Oprindelsesland"
attr(priming$religion, "label") <- "Religion"

##### 2.0 Rating outcome --------------------------------------------------------------------------------------------------------------------
### 2.1 Fit model
mm <- cj(data = conjoint,
         formula = rating ~ køn + alder + uddannelse + beskæftigelse + sprog + land + religion + uddannelse:beskæftigelse,
         id = ~Respondent_Serial,
         estimate = "mm",
         h0 = mean(conjoint$rating))

### 2.2 Visualise results
plot <- mm %>%  
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)))) +
  geom_point(fill = "black") +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 height = 0.5,
                 color = "black") +
  geom_vline(xintercept = mean(conjoint$rating),
             linetype = "longdash",
             color = "black") +
  xlab("Marginal means for støtte til indvandrer på skala fra 0-10") +
  ylab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") + 
  add_hygge()

# Create LaTeX output
output <- kable(x = mm,
                format = "latex",
                digits = 2)

# Save results
write_file(x = output,
           path = "07. Analyse/05. Robusthedstest/03. results/rating.txt")


ggsave("07. Analyse/05. Robusthedstest/03. results/rating.eps", 
       plot = plot, 
       width = 8, 
       height = 12, 
       dpi = 320)

##### 3.0 Task number -----------------------------------------------------------------------------------------------------------------------
### 3.1 Make task number indicator
conjoint <- conjoint %>% 
  mutate(conjointnr = case_when(profiler=="profil_1" | profiler=="profil_2" ~ "Opgave nr. 1",
                                profiler=="profil_3" | profiler=="profil_4" ~ "Opgave nr. 2",
                                profiler=="profil_5" | profiler=="profil_6" ~ "Opgave nr. 3",
                                profiler=="profil_7" | profiler=="profil_8" ~ "Opgave nr. 4",
                                profiler=="profil_9" | profiler=="profil_10" ~ "Opgave nr. 5"))

### 3.2 ANOVA
anova <- cj_anova(data = conjoint,
                  formula = choice ~ køn + alder + uddannelse + beskæftigelse + sprog + land + religion + uddannelse:beskæftigelse,
                  id = ~Respondent_Serial,
                  by = ~conjointnr)

### 3.3 Fit model
mm <- cj(data = conjoint,
         formula = choice ~ køn + alder + uddannelse + beskæftigelse + sprog + land + religion + uddannelse:beskæftigelse,
         id = ~Respondent_Serial,
         by = ~conjointnr,
         estimate = "mm",
         h0 = 0.5)

### 3.4 Visualise results
plot <- mm %>% 
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)),
                shape = conjointnr)) +
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
  xlim(0.2, 1.0) +
  add_hygge() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  guides(shape = guide_legend(nrow = 2,
                              byrow = TRUE)) +
  scale_shape_discrete(breaks = c("Opgave nr. 1",
                                  "Opgave nr. 2",
                                  "Opgave nr. 3",
                                  "Opgave nr. 4",
                                  "Opgave nr. 5"))

# Create LaTeX output
output <- mm %>%
  select(statistic,
         outcome,
         level,
         estimate,
         std.error,
         z,
         p,
         BY)

output <- kable(x = output,
                format = "latex",
                digits = 2)

# Save results
write_file(x = output,
           path = "07. Analyse/05. Robusthedstest/03. results/conjointnr.txt")


ggsave("07. Analyse/05. Robusthedstest/03. results/conjointnr.eps", 
       plot = plot, 
       width = 8, 
       height = 12, 
       dpi = 320)

##### 4.0 Profile number ------------------------------------------------------------------------------------------------------------------
### 4.1 Make profile number indicator
conjoint <- conjoint %>% 
  mutate(profilnr = case_when(profiler=="profil_1" | profiler=="profil_3" | profiler=="profil_5" | profiler=="profil_7" | profiler=="profil_9" ~ "Profil nr. 1",
                              profiler=="profil_2" | profiler=="profil_4" | profiler=="profil_6" | profiler=="profil_8" | profiler=="profil_10" ~ "Profil nr. 2"))

### 4.2 ANOVA
anova <- cj_anova(data = conjoint,
                  formula = choice ~ køn + alder + uddannelse + beskæftigelse + sprog + land + religion + uddannelse:beskæftigelse,
                  id = ~Respondent_Serial,
                  by = ~profilnr)

### 4.3 Fit model
mm <- cj(data = conjoint,
         formula = choice ~ køn + alder + uddannelse + beskæftigelse + sprog + land + religion + uddannelse:beskæftigelse,
         id = ~Respondent_Serial,
         by = ~profilnr,
         estimate = "mm",
         h0 = 0.5)

# Visualise results
plot <- mm %>% 
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)),
                shape = profilnr)) +
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
  xlim(0.2, 1.0) +
  add_hygge() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  guides(shape = guide_legend(nrow = 1,
                              byrow = TRUE)) +
  scale_shape_discrete(breaks = c("Profil nr. 1",
                                  "Profil nr. 2"))

# Create LaTeX output
output <- mm %>%
  select(statistic,
         outcome,
         level,
         estimate,
         std.error,
         z,
         p,
         BY)

output <- kable(x = output,
                format = "latex",
                digits = 2)

# Save results
write_file(x = output,
           path = "07. Analyse/05. Robusthedstest/03. results/profilnr.txt")


ggsave("07. Analyse/05. Robusthedstest/03. results/profilnr.eps", 
       plot = plot, 
       width = 8, 
       height = 12, 
       dpi = 320)

##### 5.0 Manipulation check --------------------------------------------------------------------------------------------------------------
### 5.1 Make a indicator of whether respondents noticed the information
priming <- priming %>% 
  mutate(resp_manipulationstjek_korrekt = ifelse(resp_prime=="Økonomisk prime" & resp_manipulationstjek=="Konsekvenser for Danmarks økonomi, velfærdssamfund og arbejdsmarked" |
                                                   resp_prime=="Kulturelt prime" & resp_manipulationstjek=="Konsekvenser for dansk kultur, normer og værdier", 
                                                 "Korrekt",
                                                 "Ikke korrekt"),
         resp_eksperiment = ifelse(resp_prime=="Økonomisk prime", "Økonomisk trussel", "Kulturel trussel"))

### Select data
# Economic
øko <- priming %>%
  filter(resp_eksperiment=="Økonomisk trussel")

# Cultural
kul <- priming %>%
  filter(resp_eksperiment=="Kulturel trussel")

### 5.3 Labels
# Economic
attr(øko$køn, "label") <- "Køn"
attr(øko$alder, "label") <- "Alder"
attr(øko$uddannelse, "label") <- "Uddannelse"
attr(øko$beskæftigelse, "label") <- "Tidligere beskæftigelse"
attr(øko$sprog, "label") <- "Engelskkundskaber"
attr(øko$land, "label") <- "Oprindelsesland"
attr(øko$religion, "label") <- "Religion"

# Cultural
attr(kul$køn, "label") <- "Køn"
attr(kul$alder, "label") <- "Alder"
attr(kul$uddannelse, "label") <- "Uddannelse"
attr(kul$beskæftigelse, "label") <- "Tidligere beskæftigelse"
attr(kul$sprog, "label") <- "Engelskkundskaber"
attr(kul$land, "label") <- "Oprindelsesland"
attr(kul$religion, "label") <- "Religion"

### 5.4 Fit model
# Economic
anova <- cj_anova(data = øko,
                  formula = choice ~ køn + alder + uddannelse + beskæftigelse + sprog + land + religion + uddannelse:beskæftigelse,
                  by = ~resp_manipulationstjek_korrekt,
                  id = ~Respondent_Serial)

diff1 <- mm_diffs(data = øko,
                  formula = choice ~ køn + alder + uddannelse + beskæftigelse + sprog + land + religion + uddannelse:beskæftigelse,
                  by = ~resp_manipulationstjek_korrekt,
                  id = ~Respondent_Serial,
                  alpha = 0.975)

# Cultural
anova <- cj_anova(data = kul,
                  formula = choice ~ køn + alder + uddannelse + beskæftigelse + sprog + land + religion + uddannelse:beskæftigelse,
                  by = ~resp_manipulationstjek_korrekt,
                  id = ~Respondent_Serial)

diff2 <- mm_diffs(data = kul,
                  formula = choice ~ køn + alder + uddannelse + beskæftigelse + sprog + land + religion + uddannelse:beskæftigelse,
                  by = ~resp_manipulationstjek_korrekt,
                  id = ~Respondent_Serial,
                  alpha = 0.975)

### 5.5 Make indicator
# Economic
diff1 <- diff1 %>% 
  mutate(resp_eksperiment = "Økonomisk trussel")

# Cultural
diff2 <- diff2 %>% 
  mutate(resp_eksperiment = "Kulturel trussel")

### 5.5 Merge
diff <- rbind(diff1, diff2)

# Visualise results
plot <- diff %>%  
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)),
                shape = resp_eksperiment)) +
  geom_point(position = position_dodge(width = 0.5),
             size = 2) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 position = position_dodge(width = 0.5),
                 height = 0.5) +
  geom_vline(xintercept = 0,
             linetype = "longdash",
             color = "black") +
  xlab("Forskel i marginal means for Pr(korrekt svar|støtte til indvandrer)\n og Pr(forkert svar|støtte til indvandrer)") +
  ylab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") + 
  xlim(-0.4, 0.4) +
  add_hygge() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  guides(shape = guide_legend(nrow = 1,
                              byrow = TRUE)) +
  scale_shape_discrete(breaks = c("Økonomisk trussel",
                                  "Kulturel trussel"))

# Create LaTeX output
diff <- diff %>%
  select(statistic,
         outcome,
         level,
         estimate,
         std.error,
         z,
         p,
         BY,
         resp_eksperiment)

output <- kable(x = diff,
                format = "latex",
                digits = 2)

# Save results
write_file(x = output,
           path = "07. Analyse/05. Robusthedstest/03. results/manipulationstjek_rigtig.txt")


ggsave("07. Analyse/05. Robusthedstest/03. results/manipulationstjek_rigtig.eps", 
       plot = plot, 
       width = 8, 
       height = 12, 
       dpi = 320)


### 5.2 Select data
# Correct answers
korrekt <- priming %>% 
  filter(resp_manipulationstjek_korrekt=="Korrekt")

# Incorrect answers
ikkekorrekt <- priming %>% 
  filter(resp_manipulationstjek_korrekt=="Ikke korrekt")

### 5.3 Labels
# Correct answers
attr(korrekt$køn, "label") <- "Køn"
attr(korrekt$alder, "label") <- "Alder"
attr(korrekt$uddannelse, "label") <- "Uddannelse"
attr(korrekt$beskæftigelse, "label") <- "Tidligere beskæftigelse"
attr(korrekt$sprog, "label") <- "Engelskkundskaber"
attr(korrekt$land, "label") <- "Oprindelsesland"
attr(korrekt$religion, "label") <- "Religion"

# Incorrect answers
attr(ikkekorrekt$køn, "label") <- "Køn"
attr(ikkekorrekt$alder, "label") <- "Alder"
attr(ikkekorrekt$uddannelse, "label") <- "Uddannelse"
attr(ikkekorrekt$beskæftigelse, "label") <- "Tidligere beskæftigelse"
attr(ikkekorrekt$sprog, "label") <- "Engelskkundskaber"
attr(ikkekorrekt$land, "label") <- "Oprindelsesland"
attr(ikkekorrekt$religion, "label") <- "Religion"

### 5.4 Fit model
# Correct answers
diff1 <- mm_diffs(data = korrekt,
                  formula = choice ~ køn + alder + uddannelse + beskæftigelse + sprog + land + religion + uddannelse:beskæftigelse,
                  by = ~resp_eksperiment,
                  id = ~Respondent_Serial,
                  alpha = 0.975)

# Incorrect answers
diff2 <- mm_diffs(data = ikkekorrekt,
                  formula = choice ~ køn + alder + uddannelse + beskæftigelse + sprog + land + religion + uddannelse:beskæftigelse,
                  by = ~resp_eksperiment,
                  id = ~Respondent_Serial,
                  alpha = 0.975)

### 5.5 Make indicator
# Correct
diff1 <- diff1 %>% 
  mutate(manipulation = "Korrekt")

# Incorrect
diff2 <- diff2 %>% 
  mutate(manipulation = "Ikke korrekt")

### 5.5 Merge
diff <- rbind(diff1, diff2)

# Visualise results
plot <- diff %>%  
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)),
                shape = manipulation)) +
  geom_point(position = position_dodge(width = 0.5),
             size = 2) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 position = position_dodge(width = 0.5),
                 height = 0.5) +
  geom_vline(xintercept = 0,
             linetype = "longdash",
             color = "black") +
  xlab("Forskel i marginal means for Pr(økonomisk trussel|støtte til indvandrer)\n og Pr(kulturel trussel|støtte til indvandrer)") +
  ylab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") + 
  xlim(-0.4, 0.4) +
  add_hygge() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  guides(shape = guide_legend(nrow = 1,
                              byrow = TRUE)) +
  scale_shape_discrete(breaks = c("Korrekt",
                                  "Ikke korrekt"))

# Create LaTeX output
output <- kable(x = mm,
                format = "latex",
                digits = 2)

# Save results
write_file(x = output,
           path = "07. Analyse/05. Robusthedstest/03. results/manipulationstjek.txt")


ggsave("07. Analyse/05. Robusthedstest/03. results/manipulationstjek.eps", 
       plot = plot, 
       width = 8, 
       height = 12, 
       dpi = 320)

##### 6.0 Balance check ----------------------------------------------------------------------------------------------------------------
### 6.1 Gender
# Create indicator
conjoint <- conjoint %>%
  mutate(resp_køn_num = ifelse(resp_køn=="Kvinde", 1, 0)) 

# Fit model
mm <- cj(data = conjoint,
         formula = resp_køn_num ~ køn + alder + uddannelse + beskæftigelse + sprog + land + religion + uddannelse:beskæftigelse,
         id = ~Respondent_Serial,
         estimate = "mm",
         h0 = mean(conjoint$resp_køn_num))

### 2.2 Visualise results
plot1 <- mm %>%  
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)))) +
  geom_point(fill = "black") +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 height = 0.5,
                 color = "black") +
  geom_vline(xintercept = mean(conjoint$resp_køn_num),
             linetype = "longdash",
             color = "black") +
  xlab("Marginal means for Pr(kvinde)") +
  ylab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") + 
  xlim(0.4, 0.7) +
  add_hygge()

# Create LaTeX output
output <- kable(x = mm,
                format = "latex",
                digits = 2)

# Save results
write_file(x = output,
           path = "07. Analyse/05. Robusthedstest/03. results/balance_køn.txt")


ggsave("07. Analyse/05. Robusthedstest/03. results/balance_køn.eps", 
       plot = plot1, 
       width = 8, 
       height = 12, 
       dpi = 320)

### 6.2 Age
# Fit model
mm <- cj(data = conjoint,
         formula = resp_alder_num ~ køn + alder + uddannelse + beskæftigelse + sprog + land + religion + uddannelse:beskæftigelse,
         id = ~Respondent_Serial,
         estimate = "mm",
         h0 = mean(conjoint$resp_alder_num))

# Visualise results
plot2 <- mm %>%  
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)))) +
  geom_point(fill = "black") +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 height = 0.5,
                 color = "black") +
  geom_vline(xintercept = mean(conjoint$resp_alder_num),
             linetype = "longdash",
             color = "black") +
  xlab("Marginal means for gennemsnitlig alder") +
  ylab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") + 
  xlim(40, 55) +
  add_hygge()

# Create LaTeX output
output <- kable(x = mm,
                format = "latex",
                digits = 2)

# Save results
write_file(x = output,
           path = "07. Analyse/05. Robusthedstest/03. results/balance_alder.txt")


ggsave("07. Analyse/05. Robusthedstest/03. results/balance_alder.eps", 
       plot = plot2, 
       width = 8, 
       height = 12, 
       dpi = 320)

### 6.3 Education
# Create indicator
conjoint <- conjoint %>%
  mutate(resp_uddannelse_num = ifelse(resp_uddannelse=="Kort videregående (f.eks. datamatiker, laborant)" |
                                        resp_uddannelse=="Mellemlang videregående (f.eks. teknikumingeniør, lærer)" |
                                        resp_uddannelse=="Bachelor (f.eks. 1. del af en lang videregående uddannelse)" |
                                        resp_uddannelse=="Lang videregående (f.eks. gymnasielærer, økonom, jurist)", 1, 0)) 

# Fit model
mm <- cj(data = conjoint,
         formula = resp_uddannelse_num ~ køn + alder + uddannelse + beskæftigelse + sprog + land + religion + uddannelse:beskæftigelse,
         id = ~Respondent_Serial,
         estimate = "mm",
         h0 = mean(conjoint$resp_uddannelse_num))

# Visualise results
plot3 <- mm %>%  
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)))) +
  geom_point(fill = "black") +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 height = 0.5,
                 color = "black") +
  geom_vline(xintercept = mean(conjoint$resp_uddannelse_num),
             linetype = "longdash",
             color = "black") +
  xlab("Marginal means for Pr(videregående uddannelse)") +
  ylab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") + 
  xlim(0.35, 0.65) +
  add_hygge()

# Create LaTeX output
output <- kable(x = mm,
                format = "latex",
                digits = 2)

# Save results
write_file(x = output,
           path = "07. Analyse/05. Robusthedstest/03. results/balance_uddannelse.txt")


ggsave("07. Analyse/05. Robusthedstest/03. results/balance_uddannelse.eps", 
       plot = plot3, 
       width = 8, 
       height = 12, 
       dpi = 320)

# Combine into one
plot <- grid.arrange(plot1, plot2, plot3, nrow = 1)

ggsave("07. Analyse/05. Robusthedstest/03. results/balancetest.eps", 
       plot = plot, 
       width = 18, 
       height = 12, 
       dpi = 320)

### 6.4 Priming experiment
# Select data
prime <- data %>% 
  filter(priming==1) %>% 
  mutate(resp_eksperiment = ifelse(resp_prime=="Økonomisk prime", "Økonomisk trussel", "Kulturel trussel")) %>%
  select(Respondent_Serial, resp_køn, resp_alder_num, resp_uddannelse, resp_eksperiment) %>%
  distinct()

# Make dataframe
df <- data.frame(resp_eksperiment = c("Kulturel trussel",
                                      "Økonomisk trussel"))

# Gender
# Create indicator
prime <- prime %>%
  mutate(resp_køn_num = ifelse(resp_køn=="Kvinde", 1, 0)) 

# Fit model
model <- lm(formula = resp_køn_num ~ resp_eksperiment, data = prime)

# Predict
gender <- data.frame(resp_eksperiment = c("Kulturel trussel",
                                          "Økonomisk trussel"),
                     predict(object = model,
                             newdata = df,
                             interval = "confidence",
                             type = "response"),
                     feature = "Eksperimentel situation"
)

# Visualise results
plot1 <- gender %>%  
  ggplot(., aes(x = fit, 
                y = resp_eksperiment)) +
  geom_point(fill = "black") +
  geom_errorbarh(aes(xmin = lwr, 
                     xmax = upr),
                 height = 0.2,
                 color = "black") +
  geom_vline(xintercept = mean(prime$resp_køn_num),
             linetype = "longdash",
             color = "black") +
  xlab("Estimeret Pr(kvinde)") +
  ylab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") + 
  xlim(0.4, 0.7) +
  add_hygge()

# Create LaTeX output
output <- kable(x = gender,
                format = "latex",
                digits = 2)

# Save results
write_file(x = output,
           path = "07. Analyse/05. Robusthedstest/03. results/balance_køn_prime.txt")


ggsave("07. Analyse/05. Robusthedstest/03. results/balance_køn_prime.eps", 
       plot = plot1, 
       width = 7, 
       height = 5, 
       dpi = 320)

# Age
# Fit model
model <- lm(formula = resp_alder_num ~ resp_eksperiment, data = prime)

# Predict
age <- data.frame(resp_eksperiment = c("Kulturel trussel",
                                       "Økonomisk trussel"),
                  predict(object = model,
                          newdata = df,
                          interval = "confidence",
                          type = "response"),
                  feature = "Eksperimentel situation"
)

# Visualise results
plot2 <- age %>%  
  ggplot(., aes(x = fit, 
                y = resp_eksperiment)) +
  geom_point(fill = "black") +
  geom_errorbarh(aes(xmin = lwr, 
                     xmax = upr),
                 height = 0.2,
                 color = "black") +
  geom_vline(xintercept = mean(prime$resp_alder_num),
             linetype = "longdash",
             color = "black") +
  xlab("Estimeret gennemsnitlig alder") +
  ylab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") + 
  xlim(40, 55) +
  add_hygge()

# Create LaTeX output
output <- kable(x = age,
                format = "latex",
                digits = 2)

# Save results
write_file(x = output,
           path = "07. Analyse/05. Robusthedstest/03. results/balance_alder_prime.txt")


ggsave("07. Analyse/05. Robusthedstest/03. results/balance_alder_prime.eps", 
       plot = plot2, 
       width = 7, 
       height = 5, 
       dpi = 320)

# Education
# Create indicator
prime <- prime %>%
  mutate(resp_uddannelse_num = ifelse(resp_uddannelse=="Kort videregående (f.eks. datamatiker, laborant)" |
                                        resp_uddannelse=="Mellemlang videregående (f.eks. teknikumingeniør, lærer)" |
                                        resp_uddannelse=="Bachelor (f.eks. 1. del af en lang videregående uddannelse)" |
                                        resp_uddannelse=="Lang videregående (f.eks. gymnasielærer, økonom, jurist)", 1, 0))  

# Fit model
model <- lm(formula = resp_uddannelse_num ~ resp_eksperiment, data = prime)

# Predict
edu <- data.frame(resp_eksperiment = c("Kulturel trussel",
                                       "Økonomisk trussel"),
                  predict(object = model,
                          newdata = df,
                          interval = "confidence",
                          type = "response"),
                  feature = "Eksperimentel situation"
)

# Visualise results
plot3 <- edu %>%  
  ggplot(., aes(x = fit, 
                y = resp_eksperiment)) +
  geom_point(fill = "black") +
  geom_errorbarh(aes(xmin = lwr, 
                     xmax = upr),
                 height = 0.2,
                 color = "black") +
  geom_vline(xintercept = mean(prime$resp_uddannelse_num),
             linetype = "longdash",
             color = "black") +
  xlab("Estimeret Pr(videregående uddannelse)") +
  ylab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") + 
  xlim(0.40, 0.60) +
  add_hygge()

# Create LaTeX output
output <- kable(x = edu,
                format = "latex",
                digits = 2)

# Save results
write_file(x = output,
           path = "07. Analyse/05. Robusthedstest/03. results/balance_uddannelse_prime.txt")


ggsave("07. Analyse/05. Robusthedstest/03. results/balance_uddannelse_prime.eps", 
       plot = plot3, 
       width = 7, 
       height = 5, 
       dpi = 320)

# Combine into one
plot <- grid.arrange(plot1, plot2, plot3, nrow = 1)

ggsave("07. Analyse/05. Robusthedstest/03. results/balancetest_prime.eps", 
       plot = plot, 
       width = 15, 
       height = 5, 
       dpi = 320)

##### 7.0 Distributions ----------------------------------------------------------------------------------------------------------------
### 7.1 Make data frame
# Køn
køn <- data %>%
  group_by(priming, køn) %>%
  tally() %>%
  mutate(andel = (n / (nrow(data)/2))*100,
         feature = "Køn",
         level = as.factor(køn)) %>%
  select(-køn)

# Alder
alder <- data %>%
  group_by(priming, alder) %>%
  tally() %>%
  mutate(andel = (n / (nrow(data)/2))*100,
         feature = "Alder",
         level = as.factor(alder)) %>%
  select(-alder)

# Uddannelse
uddannelse <- data %>%
  group_by(priming, uddannelse) %>%
  tally() %>%
  mutate(andel = (n / (nrow(data)/2))*100,
         feature = "Uddannelse",
         level = as.factor(uddannelse)) %>%
  select(-uddannelse)

# Tidligere beskæftigelse
beskæftigelse <- data %>%
  group_by(priming, beskæftigelse) %>%
  tally() %>%
  mutate(andel = (n / (nrow(data)/2))*100,
         feature = "Tidligere beskæftigelse",
         level = as.factor(beskæftigelse)) %>%
  select(-beskæftigelse)

# Engelskkundskaber
sprog <- data %>%
  group_by(priming, sprog) %>%
  tally() %>%
  mutate(andel = (n / (nrow(data)/2))*100,
         feature = "Engelskkundskaber",
         level = as.factor(sprog)) %>%
  select(-sprog)

# Oprindelsesland
land <- data %>%
  group_by(priming, land) %>%
  tally() %>%
  mutate(andel = (n / (nrow(data)/2))*100,
         feature = "Oprindelsesland",
         level = as.factor(land)) %>%
  select(-land)

# Religion
religion <- data %>%
  group_by(priming, religion) %>%
  tally() %>%
  mutate(andel = (n / (nrow(data)/2))*100,
         feature = "Religion",
         level = as.factor(religion)) %>%
  select(-religion)

# Combine into one
dist <- bind_rows(køn, alder, uddannelse, beskæftigelse, sprog, land, religion)

dist <- dist %>%
  mutate(feature = factor(feature,
                          levels = c("Køn",
                                     "Alder",
                                     "Uddannelse",
                                     "Tidligere beskæftigelse",
                                     "Engelskkundskaber",
                                     "Oprindelsesland",
                                     "Religion")),
         level = factor(level,
                        levels = c("Mand",
                                   "Kvinde",
                                   "25 år", 
                                   "31 år",
                                   "37 år",
                                   "43 år",
                                   "55 år",
                                   "Ingen formel uddannelse",
                                   "Grundskole",
                                   "Gymnasial uddannelse",
                                   "Erhvervsfaglig uddannelse",
                                   "Kort videregående uddannelse",
                                   "Lang videregående uddannelse",
                                   "Arbejdsløs",
                                   "Tjener",
                                   "Kok",
                                   "Mekaniker",
                                   "Butiksassistent",
                                   "Salgskonsulent",
                                   "Regnskabsmedarbejder",
                                   "It-medarbejder",
                                   "Analytiker",
                                   "Ingeniør",
                                   "Læge",
                                   "Taler og forstår ikke engelsk",
                                   "Forstår, men taler ikke engelsk",
                                   "Kan føre en samtale på engelsk",
                                   "Taler flydende engelsk",
                                   "Polen",
                                   "Tyskland",
                                   "Somalia",
                                   "Kina",
                                   "Syrien",
                                   "Tyrkiet",
                                   "Irak",
                                   "Ikke troende",
                                   "Muslim",
                                   "Kristen")))

### 7.2 Visualise results
# Before priming
plot1 <- dist %>%
  filter(priming==0) %>%
  ggplot(., aes(x = andel,
                y = reorder(level, desc(level)))) +
  geom_col(fill = "black") +
  scale_x_continuous(expand = c(0, 0), 
                     limits = c(0, 75)) +
  ylab("") +
  xlab("Procent") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") + 
  ggtitle("Inden primingeksperiment (n = 10.000)") +
  add_hygge()

# After priming
plot2 <- dist %>%
  filter(priming==1) %>%
  ggplot(., aes(x = andel,
                y = reorder(level, desc(level)))) +
  geom_col(fill = "black") +
  scale_x_continuous(expand = c(0, 0), 
                     limits = c(0, 75)) +
  ylab("") +
  xlab("Procent") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") + 
  ggtitle("Efter primingeksperiment (n = 10.000)") +
  add_hygge()

# Combine into one plot
plot <- grid.arrange(plot1, plot2, nrow = 1)

# Create LaTeX output
output <- kable(x = dist,
                format = "latex",
                digits = 2)

# Save results
write_file(x = output,
           path = "07. Analyse/05. Robusthedstest/03. results/fordeling_conjoint.txt")


ggsave("07. Analyse/05. Robusthedstest/03. results/fordeling_conjoint.eps", 
       plot = plot, 
       width = 12, 
       height = 12, 
       dpi = 320)

### 7.3 Primingeksperiment
# Create variable
data <- data %>% 
  mutate(resp_eksperiment = case_when(priming==0 ~ "Kontrolmåling",
                                      priming==1 & resp_prime=="Kulturelt prime" ~ "Kulturel trussel",
                                      priming==1 & resp_prime=="Økonomisk prime" ~ "Økonomisk trussel"),
         resp_eksperiment = factor(resp_eksperiment,
                                   levels = c("Kontrolmåling",
                                              "Økonomisk trussel",
                                              "Kulturel trussel")))

# Make data frame
dist <- data %>%
  group_by(resp_eksperiment) %>%
  tally() %>%
  mutate(andel = (n / sum(n))*100,
         feature = "Primingeksperiment")

# Visualise
plot <- dist %>%
  ggplot(., aes(x = andel,
                y = reorder(resp_eksperiment, desc(resp_eksperiment)))) +
  geom_col(fill = "black") +
  scale_x_continuous(expand = c(0, 0), 
                     limits = c(0, 100)) +
  ylab("") +
  xlab("Procent") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") +
  add_hygge()

# Create LaTeX output
output <- kable(x = dist,
                format = "latex",
                digits = 2)

# Save results
write_file(x = output,
           path = "07. Analyse/05. Robusthedstest/03. results/fordeling_priming.txt")


ggsave("07. Analyse/05. Robusthedstest/03. results/fordeling_priming.eps", 
       plot = plot, 
       width = 7, 
       height = 5, 
       dpi = 320)