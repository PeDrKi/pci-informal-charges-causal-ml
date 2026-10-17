library(grf)

df <- read.csv("pci_panel_final.csv", stringsAsFactors = FALSE)

demean_2way <- function(df, var) {
  prov_mean <- ave(df[[var]], df$province, FUN = mean)
  year_mean <- ave(df[[var]], df$year, FUN = mean)
  grand_mean <- mean(df[[var]])
  df[[var]] - prov_mean - year_mean + grand_mean
}

covariate_names <- c(paste0("CSTP", c(1,2,3,4,6,8,9,10)))
provinces <- unique(df$province)
n_prov <- length(provinces)

B <- 200  # số lần lặp bootstrap
cat("Bắt đầu block bootstrap (theo tỉnh) với B =", B, "lần lặp...\n")

boot_results <- matrix(NA, nrow = B, ncol = n_prov)
colnames(boot_results) <- provinces

set.seed(123)

for (b in 1:B) {
  # block bootstrap: chọn lại NGẪU NHIÊN 63 tỉnh (có lặp), giữ nguyên toàn bộ
  # dữ liệu 7 năm của mỗi tỉnh được chọn -> tôn trọng cấu trúc panel
  sampled_provinces <- sample(provinces, n_prov, replace = TRUE)
  boot_df <- do.call(rbind, lapply(sampled_provinces, function(p) df[df$province == p, ]))

  Y_dm_b <- demean_2way(boot_df, "attitude_positive")
  W_dm_b <- demean_2way(boot_df, "CSTP5")
  X_dm_b <- sapply(covariate_names, function(v) demean_2way(boot_df, v))

  cf_b <- causal_forest(X_dm_b, Y_dm_b, W_dm_b, num.trees = 500)
  boot_df$CATE_b <- as.vector(cf_b$predictions)

  prov_cate_b <- aggregate(CATE_b ~ province, data = boot_df, FUN = mean)
  for (i in 1:nrow(prov_cate_b)) {
    boot_results[b, prov_cate_b$province[i]] <- prov_cate_b$CATE_b[i]
  }
  if (b %% 50 == 0) cat("  Đã xong", b, "/", B, "lần lặp\n")
}

# Tính khoảng tin cậy 95% (percentile method) cho từng tỉnh
ci_result <- data.frame(
  province = provinces,
  mean_cate = colMeans(boot_results, na.rm = TRUE),
  ci_lower = apply(boot_results, 2, quantile, probs = 0.025, na.rm = TRUE),
  ci_upper = apply(boot_results, 2, quantile, probs = 0.975, na.rm = TRUE)
)
ci_result$significant <- ci_result$ci_lower > 0 | ci_result$ci_upper < 0
ci_result <- ci_result[order(-ci_result$mean_cate), ]

cat("=== Số tỉnh có CATE khác 0 có ý nghĩa thống kê (CI 95% không chứa 0) ===\n")
cat(sum(ci_result$significant), "/", nrow(ci_result), "tỉnh\n\n")
print(head(ci_result, 10))

write.csv(ci_result, "bootstrap_cate_ci.csv", row.names = FALSE)
cat("\nĐã lưu bootstrap_cate_ci.csv\n")
