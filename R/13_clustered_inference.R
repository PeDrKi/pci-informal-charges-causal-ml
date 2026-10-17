# =============================================================
# BƯỚC 13: SUY LUẬN THEO CỤM TỈNH (sửa các điểm chưa đồng đều ở bước 01, 02, 07, 08, 09)
# Đặt file này cùng thư mục với pci_panel_final.csv và pci_panel_placebo.csv
# (chạy 07_placebo_test.R trước nếu chưa có pci_panel_placebo.csv)
# install.packages(c("plm", "grf", "sandwich", "lmtest"))
# =============================================================
library(plm); library(grf); library(sandwich); library(lmtest)

df  <- read.csv("pci_panel_final.csv",  stringsAsFactors = FALSE)
dfp <- read.csv("pci_panel_placebo.csv", stringsAsFactors = FALSE)
controls <- paste0("CSTP", c(1, 2, 3, 4, 6, 8, 9, 10))

demean_2way <- function(d, var) {
  d[[var]] - ave(d[[var]], d$province) - ave(d[[var]], d$year) + mean(d[[var]])
}

# ---------- (A) Panel FE với SE theo cụm tỉnh ----------
fe_cluster <- function(d, yvar) {
  f <- as.formula(paste(yvar, "~ CSTP5 +", paste(controls, collapse = " + ")))
  m <- plm(f, data = pdata.frame(d, index = c("province", "year")),
           model = "within", effect = "twoways")
  list(model = m,
       ct = coeftest(m, vcov = vcovHC(m, type = "HC1", cluster = "group"))["CSTP5", ])
}
cat("=== (A) Panel FE: SE theo cụm tỉnh (CSTP5) ===\n")
print(fe_cluster(df, "attitude_positive")$ct)
cat("\n--- Placebo Panel FE (cụm tỉnh) ---\n")
print(fe_cluster(dfp, "attitude_positive_lag")$ct)

# ---------- (B) Causal Forest tiêu chuẩn có clusters ----------
cf_cluster <- function(d, yvar) {
  X <- as.matrix(d[, c(controls, "year")])
  set.seed(42)
  cf <- causal_forest(X, d[[yvar]], d$CSTP5, num.trees = 2000,
                      clusters = as.integer(factor(d$province)))
  cat("ATE:\n"); print(average_treatment_effect(cf))
  cat("Calibration:\n"); print(test_calibration(cf))
}
cat("\n=== (B) Causal Forest (clusters = tỉnh) ===\n"); cf_cluster(df, "attitude_positive")
cat("\n--- Placebo Causal Forest (clusters = tỉnh) ---\n"); cf_cluster(dfp, "attitude_positive_lag")

# ---------- (C) FE-DML: fold chia THEO TỈNH, SE theo cụm tỉnh ----------
fe_dml <- function(d, yvar, K = 5, seed = 42) {
  Ydm <- demean_2way(d, yvar); Wdm <- demean_2way(d, "CSTP5")
  Xdm <- sapply(controls, function(v) demean_2way(d, v))
  set.seed(seed)
  provs <- unique(d$province)
  fold_of_prov <- setNames(sample(rep(1:K, length.out = length(provs))), provs)
  folds <- fold_of_prov[d$province]
  Yr <- Wr <- numeric(nrow(d))
  for (k in 1:K) {
    tr <- which(folds != k); te <- which(folds == k)
    Yr[te] <- Ydm[te] - predict(regression_forest(Xdm[tr, ], Ydm[tr], num.trees = 1000), Xdm[te, ])$predictions
    Wr[te] <- Wdm[te] - predict(regression_forest(Xdm[tr, ], Wdm[tr], num.trees = 1000), Xdm[te, ])$predictions
  }
  fit <- lm(Yr ~ Wr)
  coeftest(fit, vcov = vcovCL(fit, cluster = d$province, type = "HC1"))["Wr", ]
}
cat("\n=== (C) FE-DML (fold theo tỉnh, SE theo cụm tỉnh) ===\n")
print(fe_dml(df, "attitude_positive"))
cat("\n--- Placebo FE-DML ---\n")
print(fe_dml(dfp, "attitude_positive_lag"))

# ---------- (D) Độ ổn định: lặp lại FE-DML với nhiều seed chia fold ----------
cat("\n=== (D) FE-DML qua 20 seed khác nhau (ước lượng điểm và p-value) ===\n")
res <- t(sapply(1:20, function(s) fe_dml(df, "attitude_positive", seed = s)[c("Estimate", "Pr(>|t|)")]))
print(summary(res))
