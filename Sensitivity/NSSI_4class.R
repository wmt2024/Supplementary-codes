library(haven)
library(dplyr)
library(tidyr)
library(car)
library(broom)

df <- read_sav("R_ana.sav") %>%
  mutate(
    NSSI    = as.factor(NSSI),
    gender  = as.factor(gender),
    grade   = as.factor(grade),
    profile = as.factor(profile),
    age     = as.numeric(age),
    BDI     = as.numeric(BDI),
    SAI     = as.numeric(SAI)
  ) %>%
  drop_na(NSSI, age, grade, gender, BDI, SAI, profile)

df$profile <- relevel(df$profile, ref = "4")

model1 <- glm(NSSI ~ age + grade + gender, data = df, family = binomial)
model2 <- glm(NSSI ~ age + grade + gender + BDI + SAI, data = df, family = binomial)
model3 <- glm(NSSI ~ age + grade + gender + BDI + SAI + profile, data = df, family = binomial)

extract_results <- function(model, adjust_FDR = FALSE) {
  res <- tidy(model) %>%
    mutate(
      OR = exp(estimate),
      lower_CI = exp(estimate - 1.96 * std.error),
      upper_CI = exp(estimate + 1.96 * std.error)
    ) %>%
    select(term, estimate, std.error, OR, lower_CI, upper_CI, p.value)
  
  if (adjust_FDR) {
    res <- res %>%
      mutate(p_FDR = p.adjust(p.value, method = "BH"))
  }
  
  return(res)
}

cat("\nMODEL 1 RESULTS\n")
print(extract_results(model1))

cat("\nMODEL 2 RESULTS\n")
print(extract_results(model2))

cat("\nMODEL 3 RESULTS (FINAL MODEL) WITH BH-FDR\n")
print(extract_results(model3, adjust_FDR = TRUE))

cat("\nMODEL COMPARISON (Δχ²)\n")
anova_out <- anova(model1, model2, model3, test = "Chisq")
print(anova_out)

cat("\nVIF VALUES\n")
print(vif(model3))
