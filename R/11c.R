library(grf)

df <- read.csv("pci_panel_final.csv", stringsAsFactors = FALSE)

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

set.seed(42)
cf_fe <- causal_forest(X_dm, Y_dm, W_dm, num.trees = 2000, clusters = as.integer(factor(df$province)))
df$CATE_FE <- as.vector(cf_fe$predictions)

# ===== MÔ PHỎNG CHÍNH SÁCH =====
# Lấy lát cắt năm 2024 (hiện trạng gần nhất) làm nền mô phỏng
df_2024 <- df[df$year == 2024, c("province", "CSTP5", "attitude_positive", "CATE_FE")]
median_cstp5_2024 <- median(df_2024$CSTP5)
cat("Trung vị CSTP5 năm 2024:", round(median_cstp5_2024, 4), "\n")
cat("Số tỉnh dưới trung vị:", sum(df_2024$CSTP5 < median_cstp5_2024), "/", nrow(df_2024), "\n\n")

# Với mỗi tỉnh dưới trung vị: giả định CSTP5 được nâng lên bằng trung vị
df_2024$below_median <- df_2024$CSTP5 < median_cstp5_2024
df_2024$gap_to_median <- pmax(0, median_cstp5_2024 - df_2024$CSTP5)
df_2024$expected_gain <- df_2024$CATE_FE * df_2024$gap_to_median
df_2024$simulated_outcome <- df_2024$attitude_positive + df_2024$expected_gain

cat("=== KẾT QUẢ MÔ PHỎNG ===\n")
sim_summary <- df_2024[df_2024$below_median, c("province","CSTP5","gap_to_median","CATE_FE","attitude_positive","expected_gain","simulated_outcome")]
sim_summary <- sim_summary[order(-sim_summary$expected_gain), ]
print(head(sim_summary, 10))

cat("\nTổng số tỉnh được mô phỏng nâng CSTP5:", sum(df_2024$below_median), "\n")
cat("Mức tăng trung bình dự kiến (trong nhóm được nâng):", round(mean(df_2024$expected_gain[df_2024$below_median]), 4), "\n")
cat("Thái độ tích cực TB hiện tại (toàn mẫu 2024):", round(mean(df_2024$attitude_positive), 4), "\n")

df_2024$outcome_after <- ifelse(df_2024$below_median, df_2024$simulated_outcome, df_2024$attitude_positive)
cat("Thái độ tích cực TB SAU mô phỏng (toàn mẫu 2024):", round(mean(df_2024$outcome_after), 4), "\n")
cat("Mức tăng tổng thể toàn quốc:", round(mean(df_2024$outcome_after) - mean(df_2024$attitude_positive), 4),
    "(", round(100*(mean(df_2024$outcome_after) - mean(df_2024$attitude_positive))/mean(df_2024$attitude_positive), 2), "%)\n")

write.csv(sim_summary, "policy_simulation_result.csv", row.names = FALSE)
