#### Power Analysis ####
# Place-based resentment
# Date: 10.11.2022
# R version 4.2.0
# Platform: x86_64-w64-mingw32/x64 (64-bit)

library(DeclareDesign) # Version 0.26.0
library(DesignLibrary) # Version 0.1.6
library(lme4) # Version 1.1-25
library(ggplot2) # Version 3.3.2

# Power analysis

resent <- multi_arm_designer(N = 1000, m_arms = 4, outcome_means = c(0,0.25,0.5,1),
                             outcome_sds =  c(1,1,1,1),
                             conditions = c("control", "small", "medium", "big"))

diagnosis <- diagnose_design(resent, sims = 500)
diagnosis

pooled.report <- data.frame(EstimatorLabel = diagnosis$diagnosands_df$inquiry,
                           power = diagnosis$diagnosands_df$power,
                           mean_estimate = diagnosis$diagnosands_df$mean_estimate)

ggplot(data=pooled.report, aes(x=EstimatorLabel, y=power, fill = EstimatorLabel)) +
  geom_bar(stat="identity") + ggtitle("Multi-Arm Vignette Experiment (N=1000)") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

# Simulate data

resent.dat <- draw_data(resent)

# Analysis

lm0 <- lm(Y ~ Z, data = resent.dat)
summary(lm0)


