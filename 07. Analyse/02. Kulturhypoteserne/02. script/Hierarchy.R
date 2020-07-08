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

##### 2.0 Examination of country of origin --------------------------------------------------------------------------------------------------
### 2.1 Make Germany as baseline
# Recode
conjoint <- conjoint %>% 
  mutate(Land = factor(land,
                       levels = c("Tyskland",
                                  "Polen",
                                  "Kina",
                                  "Syrien",
                                  "Tyrkiet",
                                  "Irak",
                                  "Somalia")))

# Label
attr(conjoint$Land, "label") <- "Oprindelsesland"

### 2.2 Fit model
mm <- cj(data = conjoint, 
         formula = choice ~ køn + alder + uddannelse + beskæftigelse + sprog + Land + religion + uddannelse:beskæftigelse,
         id = ~Respondent_Serial,
         estimate = "mm",
         h0 = 0.5)

### 2.3 Visualise results
plot <- mm %>%  
  filter(feature=="Oprindelsesland") %>% 
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
  filter(feature=="Oprindelsesland")

output <- kable(x = output,
                format = "latex",
                digits = 2)

# Save results
write_file(x = output,
           path = "07. Analyse/02. Kulturhypoteserne/03. results/land.txt")


ggsave("07. Analyse/02. Kulturhypoteserne/03. results/land.eps", 
       plot = plot, 
       width = 7, 
       height = 5, 
       dpi = 320)

### 2.4 Estimate AMCE 
amce <- cj(data = conjoint, 
           formula = choice ~ køn + alder + uddannelse + beskæftigelse + sprog + Land + religion + uddannelse:beskæftigelse,
           id = ~Respondent_Serial,
           estimate = "amce")

### 2.3 Visualise results
plot <- amce %>%  
  filter(feature=="Oprindelsesland") %>% 
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
  filter(feature=="Oprindelsesland")

output <- kable(x = output,
                format = "latex",
                digits = 2)

# Save results
write_file(x = output,
           path = "07. Analyse/02. Kulturhypoteserne/03. results/land1.txt")


ggsave("07. Analyse/02. Kulturhypoteserne/03. results/land1.eps", 
       plot = plot, 
       width = 7, 
       height = 5, 
       dpi = 320)

##### 3.0 Examination of religion ---------------------------------------------------------------------------------------------------------
### 3.1 Order characteristics
# Recode 
conjoint <- conjoint %>% 
  mutate(Religion = factor(religion,
                           levels = c("Ikke troende",
                                      "Kristen",
                                      "Muslim")))

# Label
attr(conjoint$Religion, "label") <- "Religion"

### 3.2 Fit model
mm <- cj(data = conjoint, 
         formula = choice ~ køn + alder + uddannelse + beskæftigelse + sprog + land + Religion + uddannelse:beskæftigelse,
         id = ~Respondent_Serial,
         estimate = "mm",
         h0 = 0.5)

### 3.3 Visualise results
plot <- mm %>%  
  filter(feature=="Religion") %>% 
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)))) +
  geom_point(fill = "black",
             size = 2) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 height = 0.1,
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
  filter(feature=="Religion")

output <- kable(x = output,
                format = "latex",
                digits = 2)

# Save results
write_file(x = output,
           path = "07. Analyse/02. Kulturhypoteserne/03. results/religion.txt")


ggsave("07. Analyse/02. Kulturhypoteserne/03. results/religion.eps", 
       plot = plot, 
       width = 7, 
       height = 5, 
       dpi = 320)

### 3.4 Estimate AMCE
amce <- cj(data = conjoint, 
           formula = choice ~ køn + alder + uddannelse + beskæftigelse + sprog + land + Religion + uddannelse:beskæftigelse,
           id = ~Respondent_Serial,
           estimate = "amce")

### 3.5 Visualise results
plot <- amce %>%  
  filter(feature=="Religion") %>% 
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)))) +
  geom_point(fill = "black",
             size = 2) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 height = 0.1,
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
  filter(feature=="Religion")

output <- kable(x = output,
                format = "latex",
                digits = 2)

# Save results
write_file(x = output,
           path = "07. Analyse/02. Kulturhypoteserne/03. results/religion1.txt")


ggsave("07. Analyse/02. Kulturhypoteserne/03. results/religion1.eps", 
       plot = plot, 
       width = 7, 
       height = 5, 
       dpi = 320)

##### 4.0 Interaction between country and religion ----------------------------------------------------------------------------------------
anova <- cj_anova(data = conjoint,
                  formula = choice ~ køn + alder + uddannelse + beskæftigelse + sprog + Land + uddannelse:beskæftigelse,
                  by = ~Religion,
                  id = ~Respondent_Serial)

### 4.1 Fit model
mm <- cj(data = conjoint,
         formula = choice ~ køn + alder + uddannelse + beskæftigelse + sprog + Land + uddannelse:beskæftigelse,
         by = ~Religion,
         id = ~Respondent_Serial,
         estimate = "mm",
         h0 = 0.5)

### 4.2 Visualise results
plot <- mm %>% 
  filter(feature=="Oprindelsesland") %>% 
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)),
                shape = Religion)) +
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
        legend.title = element_blank())

# Create LaTeX output
output <- mm %>% 
  filter(feature=="Oprindelsesland")

output <- kable(x = output,
                format = "latex",
                digits = 2)

# Save results
write_file(x = output,
           path = "07. Analyse/02. Kulturhypoteserne/03. results/land_x_religion.txt")


ggsave("07. Analyse/02. Kulturhypoteserne/03. results/land_x_religion.eps", 
       plot = plot, 
       width = 7, 
       height = 5, 
       dpi = 320)

### 4.3 Estimate difference in marginal means 
# Select data for difference between ikke troende and muslim 
df <- conjoint %>% 
  filter(religion!="Kristen") %>% 
  mutate(Religion = ifelse(religion=="Ikke troende", 0, 1))

# Labels
attr(df$køn, "label") <- "Køn"
attr(df$alder, "label") <- "Alder"
attr(df$uddannelse, "label") <- "Uddannelse"
attr(df$beskæftigelse, "label") <- "Tidligere beskæftigelse"
attr(df$sprog, "label") <- "Engelskkundskaber"
attr(df$Land, "label") <- "Oprindelsesland"
attr(df$religion, "label") <- "Religion"

# Fit model
diff1 <- mm_diffs(data = df,
                  formula = choice ~ køn + alder + uddannelse + beskæftigelse + sprog + Land + uddannelse:beskæftigelse,
                  by = ~Religion,
                  id = ~Respondent_Serial,
                  alpha = 0.975)

# Make indicator
diff1 <- diff1 %>% 
  mutate(Religion = "Ikke troende vs. muslim")

# Select data for difference between ikke troende and kristne
df <- conjoint %>% 
  filter(religion!="Muslim") %>% 
  mutate(Religion = ifelse(religion=="Ikke troende", 0, 1))

# Labels
attr(df$køn, "label") <- "Køn"
attr(df$alder, "label") <- "Alder"
attr(df$uddannelse, "label") <- "Uddannelse"
attr(df$beskæftigelse, "label") <- "Tidligere beskæftigelse"
attr(df$sprog, "label") <- "Engelskkundskaber"
attr(df$Land, "label") <- "Oprindelsesland"
attr(df$religion, "label") <- "Religion"

# Fit model
diff2 <- mm_diffs(data = df,
                  formula = choice ~ køn + alder + uddannelse + beskæftigelse + sprog + Land + uddannelse:beskæftigelse,
                  by = ~Religion,
                  id = ~Respondent_Serial,
                  alpha = 0.975)

# Make indicator
diff2 <- diff2 %>% 
  mutate(Religion = "Ikke troende vs. kristen")

# Merge
diff <- rbind(diff1, diff2)

# Visualise results
plot <- diff %>%  
  filter(feature=="Oprindelsesland") %>% 
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)),
                shape = Religion)) +
  geom_point(position = position_dodge(width = 0.5),
             size = 2) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 position = position_dodge(width = 0.5),
                 height = 0.5) +
  geom_vline(xintercept = 0.0,
             linetype = "longdash",
             color = "black") +
  xlab("Forskel i marginal means for Pr(støtte til indvandrer)") +
  ylab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") + 
  add_hygge() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  guides(color = guide_legend(nrow = 1,
                              byrow = TRUE)) +
  scale_color_discrete(breaks = c("Ikke troende vs. kristen",
                                  "Ikke troende vs. muslim"))

# Create LaTeX output
output <- diff %>% 
  filter(feature=="Oprindelsesland")

output <- kable(x = output,
                format = "latex",
                digits = 2)

# Save results
write_file(x = output,
           path = "07. Analyse/02. Kulturhypoteserne/03. results/ikketroende_religion.txt")


ggsave("07. Analyse/02. Kulturhypoteserne/03. results/ikketroende_religion.eps", 
       plot = plot, 
       width = 7, 
       height = 5, 
       dpi = 320)
