##### 1.0 Data ------------------------------------------------------------------------------------------------------------------------------
### 1.1 Read data in
data <- readRDS("06. Data/05. Merge/03. results/immigrant_final.rds")

### 1.2 Make a experimental group variabel
data <- data %>% 
  mutate(resp_eksperiment = case_when(priming==0 ~ "Kontrolmåling",
                                      priming==1 & resp_prime=="Kulturelt prime" ~ "Kulturel trussel",
                                      priming==1 & resp_prime=="Økonomisk prime" ~ "Økonomisk trussel"),
         resp_eksperiment = factor(resp_eksperiment,
                                   levels = c("Kontrolmåling",
                                              "Økonomisk trussel",
                                              "Kulturel trussel")))

### 1.3 Put labels on profile characteristics
attr(data$køn, "label") <- "Køn"
attr(data$alder, "label") <- "Alder"
attr(data$uddannelse, "label") <- "Uddannelse"
attr(data$beskæftigelse, "label") <- "Tidligere beskæftigelse"
attr(data$sprog, "label") <- "Engelskkundskaber"
attr(data$land, "label") <- "Oprindelsesland"
attr(data$religion, "label") <- "Religion"

##### 2.0 Overview --------------------------------------------------------------------------------------------------------------------------
### 2.1 Plain view
anova <- cj_anova(data = data,
                  formula = choice ~ køn + alder + uddannelse + beskæftigelse + sprog + land + religion + uddannelse:beskæftigelse,
                  by = ~resp_eksperiment,
                  id = ~Respondent_Serial)

# Fit model
mm <- cj(data = data,
         formula = choice ~ køn + alder + uddannelse + beskæftigelse + sprog + land + religion + uddannelse:beskæftigelse,
         by = ~resp_eksperiment,
         id = ~Respondent_Serial,
         estimate = "mm",
         h0 = 0.5)

# Visualise results
plot <- mm %>% 
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)),
                shape = resp_eksperiment)) +
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
  guides(shape = guide_legend(nrow = 1,
                              byrow = TRUE)) +
  scale_shape_discrete(breaks = c("Kontrolmåling",
                                  "Økonomisk trussel",
                                  "Kulturel trussel"))

# Create LaTeX output
mm <- mm %>% 
  select(statistic,
         level, 
         estimate, 
         std.error,
         z,
         p,
         resp_eksperiment)

output <- kable(x = mm,
                format = "latex",
                digits = 2)

# Save results
write_file(x = output,
           path = "07. Analyse/04. Priminghypotesen/03. results/overview_all.txt")


ggsave("07. Analyse/04. Priminghypotesen/03. results/overview_all.eps", 
       plot = plot, 
       width = 8, 
       height = 12, 
       dpi = 320)

### 2.2 Within the economic group 
# Select data
eco <- data %>% 
  filter(resp_prime=="Økonomisk prime") %>% 
  mutate(resp_eksperiment = ifelse(priming==1, "Økonomisk trussel", "Kontrolmåling"))

# Labels
attr(eco$køn, "label") <- "Køn"
attr(eco$alder, "label") <- "Alder"
attr(eco$uddannelse, "label") <- "Uddannelse"
attr(eco$beskæftigelse, "label") <- "Tidligere beskæftigelse"
attr(eco$sprog, "label") <- "Engelskkundskaber"
attr(eco$land, "label") <- "Oprindelsesland"
attr(eco$religion, "label") <- "Religion"

# ANOVA
anova <- cj_anova(data = eco,
                  formula = choice ~ køn + alder + uddannelse + beskæftigelse + sprog + land + religion + uddannelse:beskæftigelse,
                  by = ~resp_eksperiment,
                  id = ~Respondent_Serial)

# Fit model
diff <- mm_diffs(data = eco,
                 formula = choice ~ køn + alder + uddannelse + beskæftigelse + sprog + land + religion + uddannelse:beskæftigelse,
                 by = ~resp_eksperiment,
                 id = ~Respondent_Serial,
                 alpha = 0.975)

# Visualise results
plot <- diff %>%
  filter(feature=="Uddannelse" | feature=="Tidligere beskæftigelse") %>% 
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)))) +
  geom_point(fill = "black") +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 height = 0.3,
                 color = "black") +
  geom_vline(xintercept = 0,
             linetype = "longdash",
             color = "black") +
  xlab("Forskel i marginal means for Pr(økonomisk trussel|støtte til indvandrer)\n og Pr(kontrolmåling|støtte til indvandrer)") +
  ylab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") +
  xlim(-0.3, 0.3) +
  add_hygge()


# Create LaTeX output
output <- diff %>% 
  filter(feature=="Uddannelse" | feature=="Tidligere beskæftigelse") %>% 
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
           path = "07. Analyse/04. Priminghypotesen/03. results/within_economic.txt")


ggsave("07. Analyse/04. Priminghypotesen/03. results/within_economic.eps", 
       plot = plot, 
       width = 7, 
       height = 8, 
       dpi = 320)

### 2.3 Within the cultural group
# Select data
cul <- data %>% 
  filter(resp_prime=="Kulturelt prime") %>% 
  mutate(resp_eksperiment = ifelse(priming==1, "Kulturel trussel", "Kontrolmåling"))

# Labels
attr(cul$køn, "label") <- "Køn"
attr(cul$alder, "label") <- "Alder"
attr(cul$uddannelse, "label") <- "Uddannelse"
attr(cul$beskæftigelse, "label") <- "Tidligere beskæftigelse"
attr(cul$sprog, "label") <- "Engelskkundskaber"
attr(cul$land, "label") <- "Oprindelsesland"
attr(cul$religion, "label") <- "Religion"

# ANOVA 
anova <- cj_anova(data = cul,
                  formula = choice ~ køn + alder + uddannelse + beskæftigelse + sprog + land + religion + uddannelse:beskæftigelse,
                  by = ~resp_eksperiment,
                  id = ~Respondent_Serial)

# Fit model
diff <- mm_diffs(data = cul,
                 formula = choice ~ køn + alder + uddannelse + beskæftigelse + sprog + land + religion + uddannelse:beskæftigelse,
                 by = ~resp_eksperiment,
                 id = ~Respondent_Serial,
                 alpha = 0.975)

# Visualise results
plot <- diff %>%  
  filter(feature=="Oprindelsesland" | feature=="Religion") %>% 
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)))) +
  geom_point(fill = "black") +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 height = 0.2,
                 color = "black") +
  geom_vline(xintercept = 0,
             linetype = "longdash",
             color = "black") +
  xlab("Forskel i marginal means for Pr(kulturel trussel|støtte til indvandrer)\n og Pr(kontrolmåling|støtte til indvandrer)") +
  ylab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") + 
  xlim(-0.3, 0.3) +
  add_hygge()

# Create LaTeX output
output <- diff %>% 
  filter(feature=="Oprindelsesland" | feature=="Religion") %>% 
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
           path = "07. Analyse/04. Priminghypotesen/03. results/within_cultural.txt")


ggsave("07. Analyse/04. Priminghypotesen/03. results/within_cultural.eps", 
       plot = plot, 
       width = 7, 
       height = 8, 
       dpi = 320)

### 2.4 Between the economic group and the cultural group
# Select data
prime <- data %>% 
  filter(priming==1) %>% 
  mutate(resp_eksperiment = ifelse(resp_prime=="Økonomisk prime", "Økonomisk trussel", "Kulturel trussel"))

# Labels
attr(prime$køn, "label") <- "Køn"
attr(prime$alder, "label") <- "Alder"
attr(prime$uddannelse, "label") <- "Uddannelse"
attr(prime$beskæftigelse, "label") <- "Tidligere beskæftigelse"
attr(prime$sprog, "label") <- "Engelskkundskaber"
attr(prime$land, "label") <- "Oprindelsesland"
attr(prime$religion, "label") <- "Religion"

# ANOVA
anova <- cj_anova(data = prime,
                  formula = choice ~ køn + alder + uddannelse + beskæftigelse + sprog + land + religion + uddannelse:beskæftigelse,
                  by = ~resp_eksperiment,
                  id = ~Respondent_Serial)

# Fit model
diff <- mm_diffs(data = prime,
                 formula = choice ~ køn + alder + uddannelse + beskæftigelse + sprog + land + religion + uddannelse:beskæftigelse,
                 by = ~resp_eksperiment,
                 id = ~Respondent_Serial,
                 alpha = 0.975)

# Visualise results
plot <- diff %>%  
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)))) +
  geom_point(fill = "black") +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 height = 0.5,
                 color = "black") +
  geom_vline(xintercept = 0,
             linetype = "longdash",
             color = "black") +
  xlab("Forskel i marginal means for Pr(økonomisk trussel|støtte til indvandrer)\n og Pr(kulturel trussel|støtte til indvandrer)") +
  ylab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") + 
  xlim(-0.3, 0.3) +
  add_hygge()

# Create LaTeX output
diff <- diff %>% 
  select(statistic,
         outcome,
         level,
         estimate,
         std.error,
         z,
         p,
         BY)

output <- kable(x = diff,
                format = "latex",
                digits = 2)

# Save results
write_file(x = output,
           path = "07. Analyse/04. Priminghypotesen/03. results/between_groups.txt")


ggsave("07. Analyse/04. Priminghypotesen/03. results/between_groups.eps", 
       plot = plot, 
       width = 8, 
       height = 12, 
       dpi = 320)

##### 3.0 Dependent on agreement of the priming question --------------------------------------------------------------------------------
### 3.1 Within the economic group
# Select data
ihøjgrad <- eco %>% 
  filter(resp_prime_svar=="I høj grad")

inogengrad <- eco %>% 
  filter(resp_prime_svar=="I nogen grad")

imindregrad <- eco %>% 
  filter(resp_prime_svar=="I mindre grad" | resp_prime_svar=="Slet ikke")

# Labels
attr(ihøjgrad$køn, "label") <- "Køn"
attr(ihøjgrad$alder, "label") <- "Alder"
attr(ihøjgrad$uddannelse, "label") <- "Uddannelse"
attr(ihøjgrad$beskæftigelse, "label") <- "Tidligere beskæftigelse"
attr(ihøjgrad$sprog, "label") <- "Engelskkundskaber"
attr(ihøjgrad$land, "label") <- "Oprindelsesland"
attr(ihøjgrad$religion, "label") <- "Religion"

attr(inogengrad$køn, "label") <- "Køn"
attr(inogengrad$alder, "label") <- "Alder"
attr(inogengrad$uddannelse, "label") <- "Uddannelse"
attr(inogengrad$beskæftigelse, "label") <- "Tidligere beskæftigelse"
attr(inogengrad$sprog, "label") <- "Engelskkundskaber"
attr(inogengrad$land, "label") <- "Oprindelsesland"
attr(inogengrad$religion, "label") <- "Religion"

attr(imindregrad$køn, "label") <- "Køn"
attr(imindregrad$alder, "label") <- "Alder"
attr(imindregrad$uddannelse, "label") <- "Uddannelse"
attr(imindregrad$beskæftigelse, "label") <- "Tidligere beskæftigelse"
attr(imindregrad$sprog, "label") <- "Engelskkundskaber"
attr(imindregrad$land, "label") <- "Oprindelsesland"
attr(imindregrad$religion, "label") <- "Religion"

# Fit models
m1 <- mm_diffs(data = ihøjgrad,
               formula = choice ~ køn + alder + uddannelse + beskæftigelse + sprog + land + religion + uddannelse:beskæftigelse,
               by = ~resp_eksperiment,
               id = ~Respondent_Serial,
               alpha = 0.975)

m2 <- mm_diffs(data = inogengrad,
               formula = choice ~ køn + alder + uddannelse + beskæftigelse + sprog + land + religion + uddannelse:beskæftigelse,
               by = ~resp_eksperiment,
               id = ~Respondent_Serial,
               alpha = 0.975)

m3 <- mm_diffs(data = imindregrad,
               formula = choice ~ køn + alder + uddannelse + beskæftigelse + sprog + land + religion + uddannelse:beskæftigelse,
               by = ~resp_eksperiment,
               id = ~Respondent_Serial,
               alpha = 0.975)

# Make indicator
m1 <- m1 %>% 
  mutate(resp_eksperiment = "I høj grad")

m2 <- m2 %>% 
  mutate(resp_eksperiment = "I nogen grad")

m3 <- m3 %>% 
  mutate(resp_eksperiment = "I mindre grad eller slet ikke")

# Merge results
diff <- rbind(m1, m2, m3)

# Visualise results
plot <- diff %>% 
  filter(feature=="Uddannelse" | feature=="Tidligere beskæftigelse") %>% 
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
  xlab("Forskel i marginal means for Pr(økonomisk trussel|støtte til indvandrer)\n og Pr(Kontrolmåling|støtte til indvandrer)") +
  ylab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") + 
  xlim(-0.6, 0.6) +
  add_hygge() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  guides(shape = guide_legend(nrow = 1,
                              byrow = TRUE)) +
  scale_shape_discrete(breaks = c("I høj grad",
                                  "I nogen grad",
                                  "I mindre grad eller slet ikke"))

# Create LaTeX output
output <- diff %>% 
  filter(feature=="Uddannelse" | feature=="Tidligere beskæftigelse") %>% 
  select(statistic,
         outcome,
         level,
         estimate,
         std.error,
         z,
         p,
         BY,
         resp_eksperiment)

output <- kable(x = output,
                format = "latex",
                digits = 2)

# Save results
write_file(x = output,
           path = "07. Analyse/04. Priminghypotesen/03. results/within_economic_agree.txt")


ggsave("07. Analyse/04. Priminghypotesen/03. results/within_economic_agree.eps", 
       plot = plot, 
       width = 7, 
       height = 8, 
       dpi = 320)

### 3.2 Within the cultural group
# Select data
ihøjgrad <- cul %>% 
  filter(resp_prime_svar=="I høj grad")

inogengrad <- cul %>% 
  filter(resp_prime_svar=="I nogen grad")

imindregrad <- cul %>% 
  filter(resp_prime_svar=="I mindre grad" | resp_prime_svar=="Slet ikke")

# Labels
attr(ihøjgrad$køn, "label") <- "Køn"
attr(ihøjgrad$alder, "label") <- "Alder"
attr(ihøjgrad$uddannelse, "label") <- "Uddannelse"
attr(ihøjgrad$beskæftigelse, "label") <- "Tidligere beskæftigelse"
attr(ihøjgrad$sprog, "label") <- "Engelskkundskaber"
attr(ihøjgrad$land, "label") <- "Oprindelsesland"
attr(ihøjgrad$religion, "label") <- "Religion"

attr(inogengrad$køn, "label") <- "Køn"
attr(inogengrad$alder, "label") <- "Alder"
attr(inogengrad$uddannelse, "label") <- "Uddannelse"
attr(inogengrad$beskæftigelse, "label") <- "Tidligere beskæftigelse"
attr(inogengrad$sprog, "label") <- "Engelskkundskaber"
attr(inogengrad$land, "label") <- "Oprindelsesland"
attr(inogengrad$religion, "label") <- "Religion"

attr(imindregrad$køn, "label") <- "Køn"
attr(imindregrad$alder, "label") <- "Alder"
attr(imindregrad$uddannelse, "label") <- "Uddannelse"
attr(imindregrad$beskæftigelse, "label") <- "Tidligere beskæftigelse"
attr(imindregrad$sprog, "label") <- "Engelskkundskaber"
attr(imindregrad$land, "label") <- "Oprindelsesland"
attr(imindregrad$religion, "label") <- "Religion"

# Fit models
m1 <- mm_diffs(data = ihøjgrad,
               formula = choice ~ køn + alder + uddannelse + beskæftigelse + sprog + land + religion + uddannelse:beskæftigelse,
               by = ~resp_eksperiment,
               id = ~Respondent_Serial,
               alpha = 0.975)

m2 <- mm_diffs(data = inogengrad,
               formula = choice ~ køn + alder + uddannelse + beskæftigelse + sprog + land + religion + uddannelse:beskæftigelse,
               by = ~resp_eksperiment,
               id = ~Respondent_Serial,
               alpha = 0.975)

m3 <- mm_diffs(data = imindregrad,
               formula = choice ~ køn + alder + uddannelse + beskæftigelse + sprog + land + religion + uddannelse:beskæftigelse,
               by = ~resp_eksperiment,
               id = ~Respondent_Serial,
               alpha = 0.975)

# Make indicator
m1 <- m1 %>% 
  mutate(resp_eksperiment = "I høj grad")

m2 <- m2 %>% 
  mutate(resp_eksperiment = "I nogen grad")

m3 <- m3 %>% 
  mutate(resp_eksperiment = "I mindre grad eller slet ikke")

# Merge results
diff <- rbind(m1, m2, m3)

# Visualise results
plot <- diff %>% 
  filter(feature=="Oprindelsesland" | feature=="Religion") %>% 
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
  xlab("Forskel i marginal means for Pr(kulturel trussel|støtte til indvandrer)\n og Pr(kontrolmåling|støtte til indvandrer)") +
  ylab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") + 
  xlim(-0.6, 0.6) +
  add_hygge() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  guides(shape = guide_legend(nrow = 1,
                              byrow = TRUE)) +
  scale_shape_discrete(breaks = c("I høj grad",
                                  "I nogen grad",
                                  "I mindre grad eller slet ikke"))

# Create LaTeX output
output <- diff %>% 
  filter(feature=="Oprindelsesland" | feature=="Religion") %>% 
  select(statistic,
         outcome,
         level,
         estimate,
         std.error,
         z,
         p,
         BY,
         resp_eksperiment)

output <- kable(x = output,
                format = "latex",
                digits = 2)

# Save results
write_file(x = output,
           path = "07. Analyse/04. Priminghypotesen/03. results/within_cultural_agree.txt")


ggsave("07. Analyse/04. Priminghypotesen/03. results/within_cultural_agree.eps", 
       plot = plot, 
       width = 7, 
       height = 8, 
       dpi = 320)

##### 4.0 Discussion -------------------------------------------------------------------------------------------------------------------
### 4.1 Economic threat
eco <- eco %>% 
  mutate(resp_prime_svar1 = case_when(resp_prime_svar=="I høj grad" ~ "I høj grad",
                                     resp_prime_svar=="I nogen grad" ~ "I nogen grad",
                                     resp_prime_svar=="I mindre grad" | resp_prime_svar=="Slet ikke" ~ "I mindre grad eller slet ikke"))
# Fit model
mm <- cj(data = eco,
         formula = choice ~ køn + alder + uddannelse + beskæftigelse + sprog + land + religion + uddannelse:beskæftigelse,
         by = ~resp_prime_svar1,
         id = ~Respondent_Serial,
         estimate = "mm",
         h0 = 0.5)

# Visualise results
plot1 <- mm %>% 
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)),
                shape = resp_prime_svar1)) +
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
  ggtitle("Økonomisk trussel\n") +
  xlim(0.0, 1.0) +
  add_hygge() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  guides(color = guide_legend(nrow = 1,
                              byrow = TRUE)) +
  scale_color_discrete(breaks = c("I høj grad",
                                  "I nogen grad",
                                  "I mindre grad eller slet ikke"))

### 4.2 Cultural treat
cul <- cul %>% 
  mutate(resp_prime_svar1 = case_when(resp_prime_svar=="I høj grad" ~ "I høj grad",
                                      resp_prime_svar=="I nogen grad" ~ "I nogen grad",
                                      resp_prime_svar=="I mindre grad" | resp_prime_svar=="Slet ikke" ~ "I mindre grad eller slet ikke"))

# Fit model
mm <- cj(data = cul,
         formula = choice ~ køn + alder + uddannelse + beskæftigelse + sprog + land + religion + uddannelse:beskæftigelse,
         by = ~resp_prime_svar1,
         id = ~Respondent_Serial,
         estimate = "mm",
         h0 = 0.5)

# Visualise results
plot2 <- mm %>% 
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)),
                shape = resp_prime_svar1)) +
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
  ggtitle("Kulturel trussel\n") +
  xlim(0.0, 1.0) +
  add_hygge() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  guides(shape = guide_legend(nrow = 1,
                              byrow = TRUE)) +
  scale_shape_discrete(breaks = c("I høj grad",
                                  "I nogen grad",
                                  "I mindre grad eller slet ikke"))


### 4.3 Arrange
plot <- grid.arrange(plot1, plot2, nrow = 1)

# Save to folder
ggsave("07. Analyse/04. Priminghypotesen/03. results/discussion.eps", 
       plot = plot, 
       width = 12, 
       height = 12, 
       dpi = 320)
