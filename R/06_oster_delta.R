df <- read.csv("pci_panel_final.csv", stringsAsFactors = FALSE)
controls <- paste0("CSTP", c(1,2,3,4,6,8,9,10))

# Mô hình RESTRICTED: chỉ treatment + fixed effects (không covariates)
f_restricted <- as.formula("attitude_positive ~ CSTP5 + factor(province) + factor(year)")
m_restricted <- lm(f_restricted, data = df)
beta_restricted <- coef(m_restricted)["CSTP5"]
r2_restricted <- summary(m_restricted)$r.squared

# Mô hình FULL: treatment + covariates + fixed effects
f_full <- as.formula(paste("attitude_positive ~ CSTP5 +", paste(controls, collapse=" + "), "+ factor(province) + factor(year)"))
m_full <- lm(f_full, data = df)
beta_full <- coef(m_full)["CSTP5"]
r2_full <- summary(m_full)$r.squared

cat("Beta (restricted, không covariates):", round(beta_restricted, 4), "| R2:", round(r2_restricted, 4), "\n")
cat("Beta (full, có covariates):", round(beta_full, 4), "| R2:", round(r2_full, 4), "\n\n")

# Công thức Oster (2019): delta = [beta_full / (beta_restricted - beta_full)] * [(Rmax - R2_full) / (R2_full - R2_restricted)]
# Rmax theo quy ước chuẩn của Oster: 1.3 x R2_full
Rmax <- 1.3 * r2_full
delta <- (beta_full / (beta_restricted - beta_full)) * ((Rmax - r2_full) / (r2_full - r2_restricted))

cat("Rmax (1.3 x R2_full):", round(Rmax, 4), "\n")
cat("Oster's delta:", round(delta, 3), "\n\n")

if (abs(delta) > 1) {
  cat(">>> delta > 1: kết quả TƯƠNG ĐỐI VỮNG trước nhiễu không quan sát được\n")
  cat(">>> (mức độ chọn lọc từ biến không quan sát cần LỚN HƠN mức độ chọn lọc từ biến đã quan sát mới đủ để triệt tiêu kết quả)\n")
} else {
  cat(">>> delta < 1: kết quả NHẠY CẢM với nhiễu không quan sát được\n")
}
