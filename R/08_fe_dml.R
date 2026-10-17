library(grf)

df <- read.csv("pci_panel_final.csv", stringsAsFactors = FALSE)

# ===== BƯỚC 1: KHỬ HIỆU ỨNG CỐ ĐỊNH (within transformation 2 chiều) =====
# Với mỗi biến Z: Z_demeaned = Z - mean(Z|tỉnh) - mean(Z|năm) + mean(Z toàn mẫu)
demean_2way <- function(df, var) {
  prov_mean <- ave(df[[var]], df$province, FUN = mean)
  year_mean <- ave(df[[var]], df$year, FUN = mean)
  grand_mean <- mean(df[[var]])
  df[[var]] - prov_mean - year_mean + grand_mean
}

covariate_names <- c(paste0("CSTP", c(1,2,3,4,6,8,9,10)))
Y_dm <- demean_2way(df, "attitude_positive")
W_dm <- demean_2way(df, "CSTP5")
X_dm <- sapply(covariate_names, function(v) demean_2way(df, v))

cat("Đã khử hiệu ứng cố định theo tỉnh + năm cho Y, W, X\n\n")

# ===== BƯỚC 2: DOUBLE MACHINE LEARNING (partialling-out, Chernozhukov et al. 2018) =====
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
  Y_pred <- predict(rf_Y, X_dm[test_idx, ])$predictions
  Y_resid[test_idx] <- Y_dm[test_idx] - Y_pred

  rf_W <- regression_forest(X_dm[train_idx, ], W_dm[train_idx], num.trees = 1000)
  W_pred <- predict(rf_W, X_dm[test_idx, ])$predictions
  W_resid[test_idx] <- W_dm[test_idx] - W_pred
}

dml_fit <- lm(Y_resid ~ W_resid)
cat("=== KẾT QUẢ FE-DML (partialling-out, đã khử hiệu ứng cố định) ===\n")
print(summary(dml_fit))

ate_dml <- coef(dml_fit)["W_resid"]
se_dml <- summary(dml_fit)$coefficients["W_resid", "Std. Error"]
cat("\nATE (FE-DML):", round(ate_dml, 4), "| SE:", round(se_dml, 4),
    "| p-value:", round(summary(dml_fit)$coefficients["W_resid","Pr(>|t|)"], 4), "\n")

saveRDS(list(ate=ate_dml, se=se_dml, folds=folds), "fe_dml_result.rds")
