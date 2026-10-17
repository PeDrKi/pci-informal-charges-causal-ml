library(grf)

df <- read.csv("pci_panel_placebo.csv", stringsAsFactors = FALSE)
cat("N (placebo) =", nrow(df), "\n\n")

demean_2way <- function(df, var) {
  prov_mean <- ave(df[[var]], df$province, FUN = mean)
  year_mean <- ave(df[[var]], df$year, FUN = mean)
  grand_mean <- mean(df[[var]])
  df[[var]] - prov_mean - year_mean + grand_mean
}

covariate_names <- c(paste0("CSTP", c(1,2,3,4,6,8,9,10)))
Y_dm <- demean_2way(df, "attitude_positive_lag")   # outcome NĂM TRƯỚC
W_dm <- demean_2way(df, "CSTP5")                     # treatment NĂM NAY
X_dm <- sapply(covariate_names, function(v) demean_2way(df, v))

set.seed(42)
n <- nrow(df)
K <- 5
folds <- sample(rep(1:K, length.out = n))

Y_resid <- numeric(n)
W_resid <- numeric(n)

for (k in 1:K) {
  train_idx <- which(folds != k)
  test_idx  <- which(folds == k)
  rf_Y <- regression_forest(X_dm[train_idx, ], Y_dm[train_idx], num.trees = 1000)
  Y_resid[test_idx] <- Y_dm[test_idx] - predict(rf_Y, X_dm[test_idx, ])$predictions
  rf_W <- regression_forest(X_dm[train_idx, ], W_dm[train_idx], num.trees = 1000)
  W_resid[test_idx] <- W_dm[test_idx] - predict(rf_W, X_dm[test_idx, ])$predictions
}

dml_fit <- lm(Y_resid ~ W_resid)
cat("=== PLACEBO TEST cho FE-DML: CSTP5(t) -> Outcome(t-1), đã khử FE ===\n")
print(summary(dml_fit))

ate <- coef(dml_fit)["W_resid"]
p <- summary(dml_fit)$coefficients["W_resid","Pr(>|t|)"]
cat("\nATE placebo (FE-DML):", round(ate,4), "| p-value:", round(p,4), "\n")
if (p > 0.05) {
  cat(">>> ĐẠT: không tìm thấy tác động giả có ý nghĩa thống kê (p > 0.05)\n")
} else {
  cat(">>> KHÔNG ĐẠT: vẫn tìm thấy tác động giả có ý nghĩa thống kê\n")
}
