library(haven)
library(dplyr)
library(tidyr)
library(car)
library(broom)

df <- read_sav("R_ana.sav") %>% 
  mutate(
    NSSI   = as.factor(NSSI),  
    gender = as.factor(gender),
    grade  = as.factor(grade),
    class  = as.factor(class),
    age    = as.numeric(age),
    BDI    = as.numeric(BDI),
    TAI    = as.numeric(TAI)
  ) %>%
  drop_na(NSSI, age, grade, gender, BDI, TAI, class)   

#-----------------------------
# Models
#-----------------------------
model1 <- glm(NSSI ~ age + grade + gender,    
              data = df, family = binomial)

model2 <- glm(NSSI ~ age + grade + gender + BDI + TAI,   
              data = df, family = binomial)

model3 <- glm(NSSI ~ age + grade + gender + BDI + TAI + class,  
              data = df, family = binomial)

#-----------------------------
# Extract function
#-----------------------------
extract_results <- function(model) {
  tidy(model) %>%
    mutate(
      OR = exp(estimate),
      lower_CI = exp(estimate - 1.96 * std.error),
      upper_CI = exp(estimate + 1.96 * std.error)
    ) %>%
    select(term, estimate, std.error, OR, lower_CI, upper_CI, p.value)
}

#-----------------------------
# Outputs
#-----------------------------
cat("\nMODEL 1 RESULTS\n")
print(extract_results(model1))

cat("\nMODEL 2 RESULTS\n")
print(extract_results(model2))

cat("\nMODEL 3 RESULTS\n")
results_model3 <- extract_results(model3)
print(results_model3)

cat("\nMODEL 3 BH-FDR Corrected p-values\n")
results_model3 <- results_model3 %>%
  mutate(p_FDR_BH = p.adjust(p.value, method = "BH"))
print(results_model3)

cat("\nHierarchical Model Comparison (Δχ²)\n")
anova_out <- anova(model1, model2, model3, test = "Chisq")
print(anova_out)

cat("\nVIF Values (Model 3)\n")
print(vif(model3))
