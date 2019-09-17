# Migration pattern of Yellow-throated Buntings revealed by isotope-based geographic assignment

# Autumn data for oxygen
library(lme4)
fall1_o <- read.csv ("C:/fall_o.csv")
head(fall1_o)
options(na.action = "na.fail")

# Removing the complex random effect to solve the singular fit or overfitting error (Matuschek et al. 2017)
# Matuschek H, Kliegl R, Vasishth S, Baayen H, Bates D. Balancing type I error and power in linear mixed models. Journal of Memory and Language, 94:305?€?315, 2017.
fallout_o <- lmer (o ~ date + sex + (1 | id) + (1 + date | site), data=fall1_o)
fallout_o <- lmer (o ~ date + sex + (1 + date | site), data=fall1_o)
fallout_o <- lmer (o ~ date + sex + (1 | id) + (1 | site), data=fall1_o)
fallout_o <- lmer (o ~ date + sex + (1 | site), data=fall1_o)
fallout_o <- lmer (o ~ date + sex + (1 | id), data=fall1_o)
summary(fallout_o)

# Autumn data for oxygen: model selection (backward elimination)
library(lmerTest) 
step_fo <- step(fallout_o)
step_fo 
final_fo <- get_model(step_fo)
anova(final_fo)
summary(final_fo)

# Autumn data for oxygen: likelihood ratio test 
final_fo
fallout_o2 <- lmer (o ~ date + (1 | id), data=fall1_o, REML=FALSE)
fallout_o3 <- lmer (o ~ 1+ (1 | id), data=fall1_o, REML=FALSE)
anova(fallout_o2, fallout_o3)

# Autumn data for oxygen: final model summary
fallout_fo <- lmer (o ~ date + (1 | id) , data=fall1_o)
fallout_fo
summary(fallout_fo)
