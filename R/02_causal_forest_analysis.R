# =============================================================
# BƯỚC 2: CAUSAL FOREST (phương pháp Causal Machine Learning)
# =============================================================
# Cài đặt package (chỉ 1 lần, trên máy bạn dùng CRAN bình thường):
#   install.packages("grf")
# Nếu install.packages("grf") báo lỗi thiếu "DiceKriging", chạy thêm:
#   install.packages("DiceKriging")
# rồi install.packages("grf") lại.

library(grf)

# --- Đọc dữ liệu ---
df <- read.csv("pci_panel_final.csv", stringsAsFactors = FALSE)

# --- Khai báo Y (outcome), W (treatment), X (covariates) ---
Y <- df$attitude_positive
W <- df$CSTP5  # grf hỗ trợ treatment liên tục, không cần nhị phân hóa

covariate_names <- c(paste0("CSTP", c(1, 2, 3, 4, 6, 8, 9, 10)), "year")
X <- as.matrix(df[, covariate_names])

# --- Chạy Causal Forest ---
set.seed(42)  # để kết quả tái lập được - LUÔN đặt seed khi viết báo cáo
cf <- causal_forest(X, Y, W, num.trees = 2000)

# --- 1. Average Treatment Effect (so sánh với Panel FE ở bước 1) ---
cat("=== Average Treatment Effect (ATE) ===\n")
ate <- average_treatment_effect(cf)
print(ate)

# --- 2. Kiểm định heterogeneity có ý nghĩa thống kê không ---
cat("\n=== Test Calibration (kiểm định heterogeneity) ===\n")
calib <- test_calibration(cf)
print(calib)
cat("\nDiễn giải: nếu 'differential.forest.prediction' có ý nghĩa thống kê\n")
cat("(p < 0.05), nghĩa là có bằng chứng heterogeneity thật sự giữa các tỉnh.\n")

# --- 3. Variable importance: yếu tố nào ảnh hưởng độ lớn hiệu ứng ---
cat("\n=== Variable Importance ===\n")
vi <- variable_importance(cf)
names(vi) <- covariate_names
print(sort(vi, decreasing = TRUE))

# --- 4. Phân phối CATE theo từng tỉnh-năm ---
df$CATE <- as.vector(cf$predictions)  # ép về vector (cf$predictions vốn là ma trận Nx1)
cat("\n=== Phân phối CATE (tau.hat) ===\n")
print(summary(df$CATE))

# --- 5. Top 10 tỉnh hưởng lợi nhiều nhất / ít nhất (trung bình theo tỉnh) ---
cate_by_province <- aggregate(CATE ~ province, data = df, FUN = mean)
cate_by_province <- cate_by_province[order(-cate_by_province$CATE), ]

cat("\n=== Top 10 tỉnh có CATE cao nhất (hưởng lợi nhiều nhất) ===\n")
print(head(cate_by_province, 10))

cat("\n=== Top 10 tỉnh có CATE thấp nhất (hưởng lợi ít nhất) ===\n")
print(tail(cate_by_province, 10))

# --- Lưu toàn bộ kết quả ---
write.csv(df[, c("province", "year", "CATE")], "output_cate_by_province_year.csv", row.names = FALSE)
write.csv(cate_by_province, "output_cate_ranking_by_province.csv", row.names = FALSE)

sink("output_causal_forest_results.txt")
cat("=== ATE ===\n"); print(ate)
cat("\n=== Test Calibration ===\n"); print(calib)
cat("\n=== Variable Importance ===\n"); print(sort(vi, decreasing = TRUE))
sink()

cat("\nĐã lưu:\n")
cat(" - output_cate_by_province_year.csv (CATE từng quan sát)\n")
cat(" - output_cate_ranking_by_province.csv (xếp hạng CATE theo tỉnh)\n")
cat(" - output_causal_forest_results.txt (tóm tắt để đưa vào bài báo)\n")
