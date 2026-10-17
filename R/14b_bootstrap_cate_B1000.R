# BƯỚC 14b: Bootstrap theo tỉnh cho CATE, B = 1000, huấn luyện lại toàn bộ quy trình mỗi vòng.
# Mỗi vòng: (i) rút 63 tỉnh có hoàn lại, gán nhãn tỉnh duy nhất cho bản sao; (ii) khử FE hai chiều trên mẫu bootstrap;
# (iii) huấn luyện causal_forest (500 cây); (iv) DỰ ĐOÁN CATE tại đặc điểm X (đã khử FE, tính trên dữ liệu gốc) của cả 63 tỉnh
# -> tỉnh không được rút vẫn có dự đoán. CATE tỉnh = trung bình 7 năm. CI percentile 95% riêng lẻ + dải sup-t đồng thời.
library(grf)
df <- read.csv("pci_panel_final.csv", stringsAsFactors = FALSE); df <- df[order(df$province, df$year), ]
controls <- paste0("CSTP", c(1,2,3,4,6,8,9,10))
dm2 <- function(d, v) d[[v]] - ave(d[[v]], d$province) - ave(d[[v]], d$year) + mean(d[[v]])
X0 <- sapply(controls, function(v) dm2(df, v))
provs <- unique(df$province); np <- length(provs); B <- 1000
idx <- split(seq_len(nrow(df)), factor(df$province, levels = provs))
res <- matrix(NA, B, np, dimnames = list(NULL, provs)); ate_b <- numeric(B)
set.seed(20260101)
for (b in 1:B) {
  s <- sample(seq_len(np), np, replace = TRUE)
  rows <- unlist(idx[s]); bd <- df[rows, ]
  bd$province <- rep(paste0("p", seq_along(s)), times = lengths(idx[s]))   # nhãn duy nhất cho bản sao
  Yb <- dm2(bd, "attitude_positive"); Wb <- dm2(bd, "CSTP5"); Xb <- sapply(controls, function(v) dm2(bd, v))
  cf <- causal_forest(Xb, Yb, Wb, num.trees = 500, num.threads = 2)
  pr <- predict(cf, X0)$predictions
  res[b, ] <- tapply(pr, factor(df$province, levels = provs), mean)
  ate_b[b] <- mean(pr)
  if (b %% 100 == 0) cat("done", b, "/", B, "\n")
}
est <- colMeans(res); sdv <- apply(res, 2, sd)
ci <- data.frame(province = provs, mean_cate = est, sd = sdv,
                 lo = apply(res, 2, quantile, .025), hi = apply(res, 2, quantile, .975))
ci$sig_indiv <- ci$lo > 0 | ci$hi < 0
tmax <- apply(abs(sweep(sweep(res, 2, est), 2, sdv, "/")), 1, max); crit <- quantile(tmax, .95)
ci$lo_simul <- est - crit * sdv; ci$hi_simul <- est + crit * sdv
ci$sig_simul <- ci$lo_simul > 0 | ci$hi_simul < 0
cat("sig riêng lẻ:", sum(ci$sig_indiv), "/", np, "\nsup-t critical:", round(crit, 3), "; sig đồng thời:", sum(ci$sig_simul), "/", np, "\n")
cat("SD CATE giữa các tỉnh (điểm):", round(sd(est), 4), "; SD bootstrap trung bình mỗi tỉnh:", round(mean(sdv), 4), "\n")
cat("CATE trung bình bootstrap (ATE) mean/sd:", round(mean(ate_b), 4), round(sd(ate_b), 4), "\n")
# kiểm tra dị biệt: phương sai giữa tỉnh so với nhiễu bootstrap
cat("Tỉ lệ SD_giữa_tỉnh / SD_bootstrap_trung_bình:", round(sd(est) / mean(sdv), 3), "\n")
cat("Khoảng CATE (min, max):", round(range(est), 4), "\n")
write.csv(ci, "bootstrap_cate_ci_B1000.csv", row.names = FALSE)
