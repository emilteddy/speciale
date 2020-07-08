##### 1.0 Data ------------------------------------------------------------------------------------------------------------------------------
### 1.1 Read data in
data <- readRDS("06. Data/05. Merge/03. results/immigrant_final.rds")

### 1.2 Select only data from before the priming
conjoint <- data %>% 
  filter(priming==0)

### 1.3 Check out profile characteristics
# Without restrictions on them
table(conjoint$køn)
table(conjoint$alder)
table(conjoint$sprog)
table(conjoint$land)
table(conjoint$religion)

# With restrictions
table(conjoint$uddannelse, conjoint$beskæftigelse)

### 1.4 Put labels on profile characteristics
attr(conjoint$køn, "label") <- "Køn"
attr(conjoint$alder, "label") <- "Alder"
attr(conjoint$uddannelse, "label") <- "Uddannelse"
attr(conjoint$beskæftigelse, "label") <- "Tidligere beskæftigelse"
attr(conjoint$sprog, "label") <- "Engelskkundskaber"
attr(conjoint$land, "label") <- "Oprindelsesland"
attr(conjoint$religion, "label") <- "Religion"

##### 2.0 Overview of effects ---------------------------------------------------------------------------------------------------------------
### 2.1 Marginal means
# Fit model
mm <- cj(data = conjoint,
         formula = choice ~ køn + alder + uddannelse + beskæftigelse + sprog + land + religion + uddannelse:beskæftigelse,
         id = ~Respondent_Serial,
         estimate = "mm",
         h0 = 0.5)

# Visualise results
plot <- mm %>%  
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)))) +
  geom_point(fill = "black") +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 height = 0.5,
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
mm <- mm %>% 
  select(level, 
         estimate, 
         std.error,
         z,
         p)

output <- kable(x = mm,
                format = "latex",
                digits = 2)

# Save results
write_file(x = output,
           path = "07. Analyse/01. Conjointeksperiment/03. results/overview_mm.txt")


ggsave("07. Analyse/01. Conjointeksperiment/03. results/overview_mm.eps", 
       plot = plot, 
       width = 8, 
       height = 12, 
       dpi = 320)

##### 3.0 Estimate most and least likely profiles to be selected -------------------------------------------------------------------------
### 3.1 Fit model
model <- lm_robust(formula = choice ~ køn + alder + uddannelse + beskæftigelse + sprog + land + religion + uddannelse:beskæftigelse,
                   data = conjoint,
                   clusters = Respondent_Serial)

### 3.3 Predict probability for every profile
predicted <- bind_cols(conjoint, 
                       as.data.frame(predict(object = model, 
                                             newdata = conjoint,
                                             se.fit = TRUE,
                                             interval = "confidence")))

### 3.4 select percentiles
# Make a vector of percentiles to be selected
percentiles <- quantile(predicted$fit.fit, c(0.01, 0.25, 0.50, 0.75, 0.99))

# Calculate distance to percentile 
predicted <- predicted %>% 
  mutate(dist1 = abs(percentiles[1] - fit.fit),
         dist25 = abs(percentiles[2] - fit.fit),
         dist50 = abs(percentiles[3] - fit.fit),
         dist75 = abs(percentiles[4] - fit.fit),
         dist99 = abs(percentiles[5] - fit.fit))

# select profiles with the minimum distances to percentiles
profiles <- predicted %>% 
  filter(dist1==min(dist1) | dist25==min(dist25) | dist50==min(dist50) | dist75==min(dist75) | dist99==min(dist99)) %>% 
  select(køn, alder, uddannelse, beskæftigelse, sprog, land, religion, fit.fit, fit.lwr, fit.upr, se.fit) %>% 
  distinct() %>% 
  arrange(fit.fit) %>% 
  mutate(percentiles = c(1, 25, 50, 75, 99),
         feature = "Profiltyper")

# Correct confidence interval such that it can't go below zero
profiles <- profiles %>% 
  mutate(fit.lwr = ifelse(fit.lwr<0, 0.00, fit.lwr))

### 3.5 Add text label to profiles
# Add label as a variable
profiles <- profiles %>% 
  mutate(label = paste(paste(køn),
                       paste(alder),
                       paste(uddannelse),
                       paste(beskæftigelse),
                       paste(sprog),
                       paste(land),
                       paste(religion),
                       sep = "\n"))

# Draw text
text1 <- textGrob(label = profiles$label[1], 
                  gp = gpar(fontsize = 7))

text25 <- textGrob(label = profiles$label[2], 
                   gp = gpar(fontsize = 7))

text50 <- textGrob(label = profiles$label[3], 
                   gp = gpar(fontsize = 7))

text75 <- textGrob(label = profiles$label[4], 
                   gp = gpar(fontsize = 7))

text99 <- textGrob(label = profiles$label[5], 
                   gp = gpar(fontsize = 7))

### 3.6 Visualise results
plot <- profiles %>% 
  ggplot(., aes(x = percentiles, 
                y = fit.fit)) +
  geom_point(fill = "black",
             size = 2) +
  geom_errorbar(aes(ymin = fit.lwr,
                    ymax = fit.upr),
                 width = 2,
                 color = "black") +
  scale_x_continuous("Percentiler", 
                     breaks = c(1, 25, 50, 75, 99),
                     expand = c(0.05, 0.2)) +
  scale_y_continuous("Estimeret Pr(støtte til indvandrer))",
                     breaks = c(0.0, 0.2, 0.4, 0.6, 0.8)) +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") +
  annotation_custom(text1, 
                    xmin = 1,
                    xmax = 1,
                    ymin = -0.25,
                    ymax = -0.25) + 
  annotation_custom(text25, 
                    xmin = 25,
                    xmax = 25,
                    ymin = -0.25,
                    ymax = -0.25) + 
  annotation_custom(text50,
                    xmin = 50, 
                    xmax = 50,
                    ymin = -0.25,
                    ymax = -0.25) + 
  annotation_custom(text75,
                    xmin = 75,
                    xmax = 75,
                    ymin = -0.25,
                    ymax = -0.25) + 
  annotation_custom(text99,
                    xmin = 99,
                    xmax = 99,
                    ymin = -0.25,
                    ymax = -0.25) +
  coord_cartesian(clip = "off") +
  add_hygge() +
  theme(plot.margin = unit(c(1, 2, 8, 1), "lines"))

# Create LaTeX output
output <- profiles %>% 
  select(percentiles,
         køn,
         alder,
         uddannelse,
         beskæftigelse,
         sprog,
         land,
         religion,
         fit.fit,
         se.fit)

output <- kable(x = output,
                format = "latex",
                digits = 2)

# Save results
write_file(x = output,
           path = "07. Analyse/01. Conjointeksperiment/03. results/profiles.txt")

ggsave("07. Analyse/01. Conjointeksperiment/03. results/profiles.eps", 
       plot = plot, 
       width = 9, 
       height = 7, 
       dpi = 320)
