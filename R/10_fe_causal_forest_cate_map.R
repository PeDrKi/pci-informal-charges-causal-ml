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
# Causal Forest TRÊN DỮ LIỆU ĐÃ KHỬ HIỆU ỨNG CỐ ĐỊNH -> vừa linh hoạt (ML)
# vừa đã kiểm soát nhiễu cấp tỉnh (đã qua placebo test ở dạng partialling-out)
cf_fe <- causal_forest(X_dm, Y_dm, W_dm, num.trees = 2000)

cat("=== ATE (Causal Forest trên dữ liệu đã khử FE) ===\n")
print(average_treatment_effect(cf_fe))

cat("\n=== Test Calibration ===\n")
print(test_calibration(cf_fe))

df$CATE_FE <- as.vector(cf_fe$predictions)

cat("\n=== CATE trung bình theo VÙNG ===\n")
region_cate <- aggregate(CATE_FE ~ region, data = df, FUN = mean)
region_cate <- region_cate[order(-region_cate$CATE_FE), ]
print(region_cate)

cate_by_province <- aggregate(CATE_FE ~ province, data = df, FUN = mean)
write.csv(cate_by_province, "cate_fe_by_province.csv", row.names = FALSE)
write.csv(region_cate, "cate_fe_by_region.csv", row.names = FALSE)
cat("\nĐã lưu CATE theo tỉnh và theo vùng\n")
