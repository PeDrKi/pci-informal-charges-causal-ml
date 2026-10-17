library(plm)

df <- read.csv("pci_panel_final.csv", stringsAsFactors = FALSE)
controls <- paste0("CSTP", c(1,2,3,4,6,8,9,10))
f <- as.formula(paste("attitude_positive ~ CSTP5 +", paste(controls, collapse=" + ")))
provinces <- unique(df$province)

# ===== 1. LEAVE-ONE-PROVINCE-OUT =====
cat("=== Leave-one-province-out (63 lần lặp) ===\n")
loo_coefs <- numeric(length(provinces))
for (i in seq_along(provinces)) {
  df_loo <- df[df$province != provinces[i], ]
  pdata_loo <- pdata.frame(df_loo, index = c("province","year"))
  fit <- tryCatch(plm(f, data=pdata_loo, model="within", effect="twoways"), error=function(e) NULL)
  loo_coefs[i] <- if (!is.null(fit)) coef(fit)["CSTP5"] else NA
}
cat("Hệ số CSTP5 khi loại từng tỉnh - Min:", round(min(loo_coefs,na.rm=T),4), 
    "| Max:", round(max(loo_coefs,na.rm=T),4), 
    "| Mean:", round(mean(loo_coefs,na.rm=T),4), "\n")
worst_idx <- which.min(loo_coefs)
cat("Tỉnh khi loại ra cho hệ số THẤP NHẤT:", provinces[worst_idx], "->", round(loo_coefs[worst_idx],4), "\n\n")

# ===== 2. MANUAL CLUSTER (PAIRS) BOOTSTRAP cho hệ số CSTP5 =====
cat("=== Cluster Bootstrap theo tỉnh (B=500) cho hệ số CSTP5 ===\n")
set.seed(2024)
B <- 500
boot_coefs <- numeric(B)
for (b in 1:B) {
  sampled <- sample(provinces, length(provinces), replace = TRUE)
  # tạo id duy nhất để tránh trùng tên tỉnh khi ghép (panel cần index duy nhất)
  boot_df <- do.call(rbind, lapply(seq_along(sampled), function(k) {
    sub <- df[df$province == sampled[k], ]
    sub$province_boot <- paste0(sub$province, "_", k)
    sub
  }))
  pdata_b <- pdata.frame(boot_df, index = c("province_boot","year"))
  fit_b <- tryCatch(plm(f, data=pdata_b, model="within", effect="twoways"), error=function(e) NULL)
  boot_coefs[b] <- if (!is.null(fit_b)) coef(fit_b)["CSTP5"] else NA
}
boot_coefs <- boot_coefs[!is.na(boot_coefs)]
cat("Số lần bootstrap thành công:", length(boot_coefs), "/", B, "\n")
ci_lower <- quantile(boot_coefs, 0.025)
ci_upper <- quantile(boot_coefs, 0.975)
cat("Cluster Bootstrap 95% CI cho CSTP5:", round(ci_lower,4), "đến", round(ci_upper,4), "\n")
boot_se <- sd(boot_coefs)
cat("Cluster Bootstrap SE:", round(boot_se,4), "(so với SE gốc 0.0101)\n\n")

saveRDS(list(loo_coefs=loo_coefs, provinces=provinces, boot_coefs=boot_coefs, 
             ci_lower=ci_lower, ci_upper=ci_upper, boot_se=boot_se), "new_robustness_results.rds")
