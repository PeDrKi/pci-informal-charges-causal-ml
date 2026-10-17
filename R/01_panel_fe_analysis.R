# =============================================================
# BƯỚC 1: PANEL FIXED EFFECTS (baseline truyền thống)
# =============================================================
# Trước khi chạy lần đầu trên máy bạn, cài 2 package sau (chỉ cần 1 lần):
#   install.packages("plm")
# (Trên máy bạn cài trực tiếp qua CRAN sẽ chạy được ngay - không gặp
#  rắc rối như môi trường sandbox của Claude vì máy bạn không bị chặn domain)

library(plm)

# --- Đọc dữ liệu ---
df <- read.csv("pci_panel_final.csv", stringsAsFactors = FALSE)
cat("Số dòng:", nrow(df), "| Số tỉnh:", length(unique(df$province)),
    "| Số năm:", length(unique(df$year)), "\n\n")

# --- Khai báo cấu trúc panel: tỉnh = đơn vị, năm = thời gian ---
pdata <- pdata.frame(df, index = c("province", "year"))

# --- Biến kiểm soát: 8 chỉ số CSTP còn lại
#     (loại CSTP5 vì là treatment, loại CSTP7 vì outcome là thành phần của nó) ---
controls <- paste0("CSTP", c(1, 2, 3, 4, 6, 8, 9, 10))
formula_fe <- as.formula(
  paste("attitude_positive ~ CSTP5 +", paste(controls, collapse = " + "))
)

# --- Mô hình Fixed Effects hai chiều (kiểm soát cả tỉnh và năm) ---
model_fe <- plm(formula_fe, data = pdata, model = "within", effect = "twoways")

cat("=== KẾT QUẢ PANEL FIXED EFFECTS (Two-way: tỉnh + năm) ===\n")
print(summary(model_fe))

# --- Lưu kết quả ra file text để đưa vào bài báo ---
sink("output_panel_fe_results.txt")
print(summary(model_fe))
sink()

cat("\nĐã lưu kết quả vào output_panel_fe_results.txt\n")
cat("Diễn giải: hệ số CSTP5 là ước lượng ATE theo phương pháp truyền thống.\n")
cat("So sánh con số này với ATE từ Causal Forest ở bước 2.\n")
