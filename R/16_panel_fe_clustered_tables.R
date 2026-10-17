# BƯỚC 16: Bảng 4 (Panel FE đầy đủ, SE theo cụm tỉnh) và Bảng 8 (độ vững Panel FE) trong bài.
library(plm); library(sandwich); library(lmtest)
df <- read.csv("pci_panel_final.csv", stringsAsFactors = FALSE)
controls <- paste0("CSTP", c(1,2,3,4,6,8,9,10))
f <- as.formula(paste("attitude_positive ~ CSTP5 +", paste(controls, collapse = " + ")))
fit <- function(d) plm(f, data = pdata.frame(d, index = c("province", "year")), model = "within", effect = "twoways")
ct <- function(m) coeftest(m, vcov = vcovHC(m, type = "HC1", cluster = "group"))

cat("=== Bảng 4: Panel FE hai chiều, SE cluster-robust theo tỉnh (HC1) ===\n")
m <- fit(df); print(round(ct(m), 4)); s <- summary(m)
cat("N =", nobs(m), "| R2 =", round(s$r.squared["rsq"], 3), "| F =", round(s$fstatistic$statistic, 2), "(p =", format.pval(s$fstatistic$p.value, 3), ")\n")

cat("\n=== Bảng 8 (1): loại Hà Nội và TP.HCM ===\n")
m2 <- fit(df[!df$province %in% c("Hà Nội", "TP.HCM"), ]); cat("N =", nobs(m2), "\n"); print(round(ct(m2)["CSTP5", ], 4))

cat("\n=== Bảng 8 (2): leave-one-province-out (63 lần) ===\n")
provs <- unique(df$province)
loo <- sapply(provs, function(p) coef(fit(df[df$province != p, ]))["CSTP5"])
cat("β min/max/mean:", round(min(loo), 4), round(max(loo), 4), round(mean(loo), 4), "| tỉnh làm β thấp nhất:", provs[which.min(loo)], "\n")

cat("\n=== Bảng 8 (3): cluster (pairs) bootstrap theo tỉnh, B = 500, seed 2024 ===\n")
set.seed(2024); B <- 500; bc <- rep(NA_real_, B)
for (b in 1:B) {
  s_ <- sample(provs, length(provs), replace = TRUE)
  bd <- do.call(rbind, lapply(seq_along(s_), function(k) { x <- df[df$province == s_[k], ]; x$province <- paste0(x$province, "_", k); x }))
  r <- tryCatch(coef(fit(bd))["CSTP5"], error = function(e) NA); bc[b] <- r
}
bc <- bc[!is.na(bc)]
cat("thành công:", length(bc), "| SE =", round(sd(bc), 4), "| CI 95% percentile: [", round(quantile(bc, .025), 4), ",", round(quantile(bc, .975), 4), "]\n")

cat("\n=== Bảng 8 (4): Oster's δ (Rmax = 1,3 x R2 mô hình đầy đủ có biến giả tỉnh và năm) ===\n")
mr <- lm(attitude_positive ~ CSTP5 + factor(province) + factor(year), data = df)
mf <- lm(as.formula(paste("attitude_positive ~ CSTP5 +", paste(controls, collapse = "+"), "+ factor(province) + factor(year)")), data = df)
br <- coef(mr)["CSTP5"]; bf <- coef(mf)["CSTP5"]; r2r <- summary(mr)$r.squared; r2f <- summary(mf)$r.squared; rmax <- 1.3 * r2f
cat("β restricted:", round(br, 4), "R2:", round(r2r, 4), "| β full:", round(bf, 4), "R2:", round(r2f, 4), "| Rmax:", round(rmax, 4), "\n")
cat("δ =", round((bf / (br - bf)) * ((rmax - r2f) / (r2f - r2r)), 3), "\n")
