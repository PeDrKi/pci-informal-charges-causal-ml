library(plm)
library(grf)

df <- read.csv("pci_panel_final.csv", stringsAsFactors = FALSE)
cat("N =", nrow(df), "\n\n")

# ===== TREATMENT MỚI: CSTP3 (Tính minh bạch) thay cho CSTP5 =====
# Controls giờ là: CSTP1,2,4,5,6,8,9,10 (loại CSTP3=treatment, CSTP7=nguồn outcome)

# ---- Panel FE ----
pdata <- pdata.frame(df, index = c("province", "year"))
controls <- paste0("CSTP", c(1,2,4,5,6,8,9,10))
formula_fe <- as.formula(paste("attitude_positive ~ CSTP3 +", paste(controls, collapse = " + ")))
model_fe <- plm(formula_fe, data = pdata, model = "within", effect = "twoways")
cat("=== PANEL FE (Treatment = CSTP3) ===\n")
print(summary(model_fe))

# ---- Causal Forest ----
Y <- df$attitude_positive
W <- df$CSTP3
covariate_names <- c(paste0("CSTP", c(1,2,4,5,6,8,9,10)), "year")
X <- as.matrix(df[, covariate_names])

set.seed(42)
cf <- causal_forest(X, Y, W, num.trees = 2000)

cat("\n=== CAUSAL FOREST (Treatment = CSTP3) ===\n")
cat("ATE:\n"); print(average_treatment_effect(cf))
cat("\nTest calibration:\n"); print(test_calibration(cf))
cat("\nVariable importance:\n")
vi <- variable_importance(cf); names(vi) <- covariate_names
print(sort(vi, decreasing = TRUE))
