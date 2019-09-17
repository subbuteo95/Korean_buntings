# Migration pattern of Yellow-throated Buntings revealed by isotope-based geographic assignment

# Autumn data for hydrogen
library(lme4)
fall1 <- read.csv ("C:/fall_h.csv")
head(fall1)
options(na.action = "na.fail")

# Removing the complex random effect to solve the singular fit or overfitting error (Matuschek et al. 2017)
# Matuschek H, Kliegl R, Vasishth S, Baayen H, Bates D. Balancing type I error and power in linear mixed models. Journal of Memory and Language, 94:305?315, 2017.
fallout1 <- lmer (h ~ date + sex + (1 | id) + (1 + date | site), data=fall1)
fallout1 <- lmer (h ~ date + sex + (1 | id) + (1 | site), data=fall1)
fallout1 <- lmer (h ~ date + sex + (1 + date | site), data=fall1)
fallout1 <- lmer (h ~ date + sex + (1 | site), data=fall1)
fallout1 <- lmer (h ~ date + sex + (1 | id), data=fall1)
summary(fallout1)

# Autumn data for hydrogen: model selection (backward elimination)
library(lmerTest) 
step_f <- step(fallout1)
step_f 
final_f <- get_model(step_f)
anova(final_f)
summary(final_f)

# Autumn data for hydrogen: likelihood ratio test 
final_f 
fallout2 <- lmer (h ~ date + (1 | id), data=fall1, REML=FALSE)
fallout3 <- lmer (h ~ 1+ (1 | id), data=fall1, REML=FALSE)
anova(fallout2, fallout3)

# Autumn data for hydrogen: final model summary
fallout_f <- lmer (h ~ date + (1 | id), data=fall1)
fallout_f
summary(fallout_f)
