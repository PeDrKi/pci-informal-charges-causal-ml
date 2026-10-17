# BƯỚC 20: (A) CSTP5 hài hòa (9 chỉ tiêu chung 2018-2024); (B) CSTP5 trễ một năm (giảm sai số mẫu cùng nguồn khảo sát).
library(sandwich); library(lmtest)
df <- read.csv("panel_with_indicators.csv", stringsAsFactors = FALSE); df <- df[order(df$province, df$year), ]
controls <- paste0("CSTP", c(1,2,3,4,6,8,9,10)); ind <- c("i1_peers_pay","i2_result","i3_harass","i4_acceptable","i5_inspect","i6_over10pct","i7_land","i8_bidcomm","i9_court")
dm2 <- function(d, v) d[[v]] - ave(d[[v]], d$province) - ave(d[[v]], d$year) + mean(d[[v]])
zy <- function(x, yr) ave(x, yr, FUN = function(v) (v - mean(v)) / sd(v))
# hướng của từng chỉ tiêu theo tương quan trong năm với CSTP5 gốc (điểm cao = chi phí thấp)
sg <- sapply(ind, function(v) sign(mean(sapply(split(df, df$year), function(d) cor(d[[v]], d$CSTP5)))))
cat("Hướng chỉ tiêu (+1 = cao hơn là tốt hơn):\n"); print(sg)
Z <- sapply(ind, function(v) zy(df[[v]], df$year)) %*% diag(sg)
df$H9 <- rowMeans(Z); df$H8 <- rowMeans(Z[, -2])    # H8 bỏ chỉ tiêu 'kết quả sau khi trả'
cat("Tương quan H9 với CSTP5 gốc:", round(cor(df$H9, df$CSTP5), 3), " | trong năm (TB):", round(mean(sapply(split(df, df$year), function(d) cor(d$H9, d$CSTP5))), 3), "\n")
fe <- function(d, w, y = "attitude_positive") {
  Y <- dm2(d, y); W <- dm2(d, w); X <- sapply(controls, function(v) dm2(d, v)); g <- as.integer(factor(d$province))
  fit <- lm(Y ~ W + X - 1); ct <- coeftest(fit, vcov = vcovCL(fit, cluster = g, type = "HC1"))["W", ]
  sdw <- sd(d[[w]]); c(beta = ct[[1]], se = ct[[2]], p = ct[[4]], beta_perSD = ct[[1]] * sdw, se_perSD = ct[[2]] * sdw, N = nrow(d))
}
wild <- function(d, w, B = 4999, seed = 1) {
  Y <- dm2(d, "attitude_positive"); W <- dm2(d, w); X <- sapply(controls, function(v) dm2(d, v)); g <- as.integer(factor(d$province))
  ts <- function(y) { fit <- lm(y ~ W + X - 1); coeftest(fit, vcov = vcovCL(fit, cluster = g, type = "HC1"))["W", 3] }
  obs <- ts(Y); f0 <- lm(Y ~ X - 1); y0 <- fitted(f0); u0 <- resid(f0); set.seed(seed)
  tb <- replicate(B, { eta <- sample(c(-1, 1), max(g), TRUE)[g]; ts(y0 + u0 * eta) }); mean(abs(tb) >= abs(obs))
}
lagv <- function(d, v) ave(d[[v]], d$province, FUN = function(x) c(NA, head(x, -1)))
df$CSTP5_l1 <- lagv(df, "CSTP5"); df$H9_l1 <- lagv(df, "H9")
res <- list()
res[["1. CSTP5 gốc, 2018-2024 (cơ sở)"]] <- c(fe(df, "CSTP5"), pw = wild(df, "CSTP5"))
res[["2. CSTP5 hài hòa H9 (9 chỉ tiêu chung)"]] <- c(fe(df, "H9"), pw = wild(df, "H9"))
res[["3. H8 (bỏ chỉ tiêu 'kết quả sau khi trả')"]] <- c(fe(df, "H8"), pw = wild(df, "H8"))
d2 <- df[df$year >= 2019, ]
res[["4. CSTP5 gốc, 2019-2024 (cùng mẫu với trễ)"]] <- c(fe(d2, "CSTP5"), pw = wild(d2, "CSTP5"))
res[["5. CSTP5 gốc trễ một năm (2019-2024)"]] <- c(fe(d2, "CSTP5_l1"), pw = wild(d2, "CSTP5_l1"))
res[["6. H9 trễ một năm (2019-2024)"]] <- c(fe(d2, "H9_l1"), pw = wild(d2, "H9_l1"))
# bỏ từng chỉ tiêu (leave-one-indicator-out) cho H9
loo <- sapply(seq_along(ind), function(k) { d <- df; d$Hk <- rowMeans(Z[, -k]); fe(d, "Hk")[c("beta_perSD", "p")] })
colnames(loo) <- ind; cat("\nLeave-one-indicator-out (H9): beta/SD và p\n"); print(round(loo, 4))
# hệ số theo giai đoạn cho H9 (đo lường hài hòa nên so sánh hợp lý hơn)
for (pr in list(2018:2021, 2022:2024)) { d <- df[df$year %in% pr, ]; cat("H9 giai đoạn", range(pr), ":", round(fe(d, "H9")[c("beta_perSD","se_perSD","p")], 4), "\n") }
out <- do.call(rbind, res); print(round(out, 4))
write.csv(out, "out20_table.csv")
