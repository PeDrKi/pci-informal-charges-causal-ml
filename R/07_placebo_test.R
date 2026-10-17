library(plm)
library(grf)

df <- read.csv("pci_panel_final.csv", stringsAsFactors = FALSE)

# ===== Xây dựng bộ dữ liệu placebo: ghép CSTP5 năm t với outcome năm (t-1) =====
df_sorted <- df[order(df$province, df$year), ]
placebo_rows <- list()
for (prov in unique(df_sorted$province)) {
  grp <- df_sorted[df_sorted$province == prov, ]
  grp <- grp[order(grp$year), ]
  if (nrow(grp) < 2) next
  for (i in 2:nrow(grp)) {
    row_t <- grp[i, ]
    row_tm1 <- grp[i - 1, ]
    if (row_t$year - row_tm1$year != 1) next
    rec <- row_t[, c("province", "year", paste0("CSTP", 1:10), "region")]
    rec$attitude_positive_lag <- row_tm1$attitude_positive
    placebo_rows[[length(placebo_rows) + 1]] <- rec
  }
}
placebo_df <- do.call(rbind, placebo_rows)
write.csv(placebo_df, "pci_panel_placebo.csv", row.names = FALSE)
cat("Placebo dataset:", nrow(placebo_df), "quan sát,", length(unique(placebo_df$province)), "tỉnh\n\n")

controls <- paste0("CSTP", c(1,2,3,4,6,8,9,10))

# ---- PLACEBO Panel FE ----
pdata <- pdata.frame(placebo_df, index = c("province", "year"))
f <- as.formula(paste("attitude_positive_lag ~ CSTP5 +", paste(controls, collapse = " + ")))
model_fe <- plm(f, data = pdata, model = "within", effect = "twoways")
cat("=== PLACEBO Panel FE: CSTP5(t) -> Outcome(t-1) ===\n")
print(summary(model_fe)$coefficients["CSTP5", ])

# ---- PLACEBO Causal Forest (tiêu chuẩn, chưa khử FE) ----
Y <- placebo_df$attitude_positive_lag
W <- placebo_df$CSTP5
covariate_names <- c(controls, "year")
X <- as.matrix(placebo_df[, covariate_names])

set.seed(42)
cf <- causal_forest(X, Y, W, num.trees = 2000)
cat("\n=== PLACEBO Causal Forest: CSTP5(t) -> Outcome(t-1) ===\n")
cat("ATE:\n"); print(average_treatment_effect(cf))
cat("\nTest calibration:\n"); print(test_calibration(cf))
