# ================ Plotting Portion ==================

# --------------Gamma Plot Construction--------------
library(ggplot2)

df_gamma <- read.table('~/Desktop/df_gamma_merged.csv', sep = ",", header = T)
df_gamma[df_gamma==0] <- NA
df_gamma <- na.omit(df_gamma)

df_gamma$Genus <- factor(df_gamma$Genus, levels = df_gamma$Genus[order(df_gamma$Average, decreasing = FALSE)])

GammaPlot <- ggplot(data = df_gamma, aes(x=Average, y=Genus)) + 
  geom_bar(stat="identity", fill="gray67") +  
  geom_errorbar(aes(xmax=Average+(1.96*STDEV), xmin= ifelse(Average-(1.96*STDEV) < 0, 0, Average - STDEV)))

GammaPlot <- GammaPlot + labs(y = "Genus", x = "Average Count of Genus Found in Gammaproteobacteria (n = 17)")

# --------------Acid Plot Construction--------------
library(ggplot2)

df_acid <- read.table('~/Desktop/df_acid_merged.csv', sep = ",", header = T)
df_acid[df_acid==0] <- NA
df_acid <- na.omit(df_acid)

df_acid$Genus <- factor(df_acid$Genus, levels = df_acid$Genus[order(df_acid$Average, decreasing = FALSE)])

AcidPlot <- ggplot(data = df_acid, aes(x=Average, y=Genus)) + 
  geom_bar(stat="identity", fill="gray67")

AcidPlot <- AcidPlot + labs(y = "Genus", x = "Average Count of Genus Found in Acidithiobacillia (n = 1)")

#----------Alpha Plot Construction-----------------
library(ggplot2)

df_alpha <- read.table('~/Desktop/df_alpha_merged.csv', sep = ",", header = T)
df_alpha[df_alpha==0] <- NA
df_alpha <- na.omit(df_alpha)

df_alpha$Genus <- factor(df_alpha$Genus, levels = df_alpha$Genus[order(df_alpha$Average, decreasing = FALSE)])

AlphaPlot <- ggplot(data = df_alpha, aes(x= Average, y= Genus)) + 
  geom_bar(stat="identity", fill = "gray67") + 
  geom_errorbar(aes(xmax=Average+(1.96*STDEV), xmin= ifelse(Average-(1.96*STDEV) < 0, 0, Average - STDEV)))

AlphaPlot <- AlphaPlot + labs(y = "Genus", x = "Average Count of Genus Found in Alphaproteobacteria (n = 20)")

#----------Delta Plot Construction-----------------
library(ggplot2)

df_delta <- read.table('~/Desktop/df_delta_merged.csv', sep = ",", header = T)
df_delta[df_delta==0] <- NA
df_delta <- na.omit(df_delta)

df_delta$Genus <- factor(df_delta$Genus, levels = df_delta$Genus[order(df_delta$Average, decreasing = FALSE)])

DeltaPlot <- ggplot(data = df_delta, aes(x= Average, y= Genus)) + 
  geom_bar(stat="identity", fill = "gray67")
  
DeltaPlot <- DeltaPlot + labs(y = "Genus", x = "Average Count of Genus Found in Deltaproteobacteria (n = 1)")

#-----------------Plotting Comparisons-----------------
library(ggpubr)
library("cowplot")

# Alpha x Gamma Plot Comparison (Yes SD)
plot_grid(AlphaPlot + rremove("y.grid"), GammaPlot + rremove("y.grid"), 
          labels = c("A", "B"),
          ncol = 2, nrow = 1)

# Alpha x Delta Plot Comparison 
plot_grid(AlphaPlot + rremove("y.grid"), DeltaPlot + rremove("y.grid"), 
          labels = c("A", "B"),
          ncol = 2, nrow = 1)

# Gamma x Delta Plot Comparison
plot_grid(GammaPlot + rremove("y.grid"), DeltaPlot + rremove("y.grid"), 
          labels = c("A", "B"),
          ncol = 2, nrow = 1)

# ==================== Statistical Analyse Portion =====================

# Statistical Analyse for Combined Table (Finding Significant Difference Between Own Groups)
df_combined <- read.table('~/Desktop/stats/combined.csv', sep =",",header = T)

# Checking for Normal Distribution (Assumptions)
model_combined <- lm(genus_values ~ treatment, data = df_combined)
residuals <- residuals(model_combined)
hist(residuals)
qqnorm(residuals)
qqline(residuals, col = "blue")
fitted <- fitted(model_combined)
plot(residuals ~ fitted)

# SQRT, does not work 
SQRT_model_combined <-lm(genus_values ~ treatment, data = df_combined)
SQRT_residuals <- residuals(SQRT_model_combined)
hist(SQRT_residuals)
qqnorm(SQRT_residuals)
qqline(SQRT_residuals, col = "blue")
SQRT_fitted <- fitted(SQRT_model_combined)
plot(SQRT_residuals ~ SQRT_fitted)

# Power, does not work 
power_combined_model <- lm((genus_values^2) ~ treatment, data = df_combined)
power_residuals <- residuals(power_combined_model)
hist(power_residuals)
qqnorm(power_residuals)
qqline(power_residuals)
power_fitted <- fitted(power_combined_model)
plot(power_residuals ~ power_fitted)

# Reciprocal, does not work 
reciprocal_model <- lm((1/(genus_values+1)) ~ treatment, data = df_combined)
reciprocal_resdiuals <- residuals(reciprocal_model)
hist(reciprocal_resdiuals)
qqnorm(reciprocal_resdiuals)
qqline(reciprocal_resdiuals)
reciprocal_fitted <- fitted(reciprocal_model)
plot(reciprocal_resdiuals ~ reciprocal_fitted)

# Log Does not work 
log_model <- lm(log(genus_values+1) ~ treatment, data = df_combined)
log_resdiuals <- residuals(log_model)
hist(log_resdiuals)
qqnorm(log_resdiuals)
qqline(log_resdiuals)
log_fitted <- fitted(log_model)
plot(log_resdiuals ~ log_fitted)

# Not Normal after data transformation as well (Krustal Wallis Test Appropriate)
kruskal.test(genus_values ~ treatment, data = df_combined) # Yes SD
# Post Hoc test for Kruskal Wallis Test
pairwise.wilcox.test(df_combined$genus_values, df_combined$treatment, p.adjust.method = "bonferroni", exact = FALSE)
# ------------------------------------------------

# Statistical Analyse for Eukaryote Table (Own Groups and Eukaryote Table)
df_eukaryote <- read.table('~/Desktop/stats/eukaryote.csv', sep =",",header = T)

# Checking for Normal Distribution (Assumptions)
model_eukaryote <- lm(genus_values ~ treatment, data = df_eukaryote)
residuals_eukaryote <- residuals(model_eukaryote)
hist(residuals_eukaryote)
qqnorm(residuals_eukaryote)
qqline(residuals_eukaryote, col = "blue")
fitted_eukaryote <- fitted(model_eukaryote)
plot(residuals_eukaryote ~ fitted_eukaryote)

# SQRT, does not work 
SQRT_model_combined <-lm(genus_values ~ treatment, data = df_eukaryote)
SQRT_residuals <- residuals(SQRT_model_combined)
hist(SQRT_residuals)
qqnorm(SQRT_residuals)
qqline(SQRT_residuals, col = "blue")
SQRT_fitted <- fitted(SQRT_model_combined)
plot(SQRT_residuals ~ SQRT_fitted)

# Power, does not work 
power_combined_model <- lm((genus_values^2) ~ treatment, data = df_eukaryote)
power_residuals <- residuals(power_combined_model)
hist(power_residuals)
qqnorm(power_residuals)
qqline(power_residuals)
power_fitted <- fitted(power_combined_model)
plot(power_residuals ~ power_fitted)

# Reciprocal, does not work 
reciprocal_model <- lm((1/(genus_values+1)) ~ treatment, data = df_eukaryote)
reciprocal_resdiuals <- residuals(reciprocal_model)
hist(reciprocal_resdiuals)
qqnorm(reciprocal_resdiuals)
qqline(reciprocal_resdiuals)
reciprocal_fitted <- fitted(reciprocal_model)
plot(reciprocal_resdiuals ~ reciprocal_fitted)

df_eukaryote[df_eukaryote == 0] <- NA
df_eukaryote_log <- na.omit(df_eukaryote)

# Log does not work
log_model <- lm(log(genus_values) ~ treatment, data = df_eukaryote_log)
log_resdiuals <- residuals(log_model)
hist(log_resdiuals)
qqnorm(log_resdiuals)
qqline(log_resdiuals)
log_fitted <- fitted(log_model)
plot(log_resdiuals ~ log_fitted)

# Not Normal after data transformation as well (Krustal Wallis Test Appropriate)
kruskal.test(genus_values ~ treatment, data = df_eukaryote) # No SD
# Post Hoc test for Kruskal Wallis Test
pairwise.wilcox.test(df_eukaryote$genus_values, df_eukaryote$treatment, p.adjust.method = "bonferroni", exact = FALSE)

# ------------------------------------------------

# Statistical Analyse for Eukaryote Table (Alpha and Delta Only Comparison)
df_eukaryote_copy <- read.table('~/Desktop/stats/eukaryote_copy.csv', sep =",",header = T)

# Checking for Normal Distribution (Assumptions)
model_eukaryote_copy <- lm(genus_values ~ treatment, data = df_eukaryote_copy)
residuals_eukaryote_copy <- residuals(model_eukaryote_copy)
hist(residuals_eukaryote_copy)
qqnorm(residuals_eukaryote_copy)
qqline(residuals_eukaryote_copy, col = "blue")
fitted_eukaryote_copy <- fitted(model_eukaryote_copy)
plot(residuals_eukaryote_copy ~ fitted_eukaryote_copy)

# SQRT, does not work 
SQRT_model_combined <-lm(genus_values ~ treatment, data = df_eukaryote_copy)
SQRT_residuals <- residuals(SQRT_model_combined)
hist(SQRT_residuals)
qqnorm(SQRT_residuals)
qqline(SQRT_residuals, col = "blue")
SQRT_fitted <- fitted(SQRT_model_combined)
plot(SQRT_residuals ~ SQRT_fitted)

# Power, does not work 
power_combined_model <- lm((genus_values^2) ~ treatment, data = df_eukaryote_copy)
power_residuals <- residuals(power_combined_model)
hist(power_residuals)
qqnorm(power_residuals)
qqline(power_residuals)
power_fitted <- fitted(power_combined_model)
plot(power_residuals ~ power_fitted)

# Reciprocal, does not work 
reciprocal_model <- lm((1/(genus_values+1)) ~ treatment, data = df_eukaryote_copy)
reciprocal_resdiuals <- residuals(reciprocal_model)
hist(reciprocal_resdiuals)
qqnorm(reciprocal_resdiuals)
qqline(reciprocal_resdiuals)
reciprocal_fitted <- fitted(reciprocal_model)
plot(reciprocal_resdiuals ~ reciprocal_fitted)

# Log, does not work
log_model <- lm(log(genus_values+1) ~ treatment, data = df_eukaryote_copy)
log_resdiuals <- residuals(log_model)
hist(log_resdiuals)
qqnorm(log_resdiuals)
qqline(log_resdiuals)
log_fitted <- fitted(log_model)
plot(log_resdiuals ~ log_fitted)

# Not Normal after data transformation as well (Krustal Wallis Test Appropriate)
kruskal.test(genus_values ~ treatment, data = df_eukaryote_copy) # no SD
# Post Hoc test for Kruskal Wallis Test
pairwise.wilcox.test(df_eukaryote_copy$genus_values, df_eukaryote_copy$treatment, p.adjust.method = "bonferroni", exact = FALSE)

# -------------------------- FIN ---------------------


