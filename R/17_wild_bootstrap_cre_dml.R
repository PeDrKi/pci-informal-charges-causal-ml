# BƯỚC 17: (A) wild cluster bootstrap-t cho Panel FE (63 cụm); (B) CRE-DML: DML với đặc trưng Mundlak thay cho khử trung bình thủ công.
library(grf); library(sandwich); library(lmtest)
df <- read.csv("pci_panel_final.csv", stringsAsFactors = FALSE); df <- df[order(df$province, df$year), ]
controls <- paste0("CSTP", c(1,2,3,4,6,8,9,10))
dm2 <- function(d, v) d[[v]] - ave(d[[v]], d$province) - ave(d[[v]], d$year) + mean(d[[v]])

## (A) Wild cluster bootstrap-t (Cameron-Gelbach-Miller 2008), Rademacher, null beta = 0 áp đặt. Dùng FWL: sau khử FE hai chiều.
Y <- dm2(df, "attitude_positive"); W <- dm2(df, "CSTP5"); X <- sapply(controls, function(v) dm2(df, v)); g <- as.integer(factor(df$province))
tstat <- function(y, w, Xc, cl) { fit <- lm(y ~ w + Xc); coeftest(fit, vcov = vcovCL(fit, cluster = cl, type = "HC1"))["w", c(1, 3)] }
obs <- tstat(Y, W, X, g); cat("Panel FE (trên dữ liệu đã khử FE): beta =", round(obs[1], 4), " t =", round(obs[2], 3), "\n")
fit0 <- lm(Y ~ X); y0 <- fitted(fit0); u0 <- resid(fit0)           # mô hình ràng buộc beta = 0
set.seed(1); B <- 9999; tb <- numeric(B); G <- max(g)
for (b in 1:B) { eta <- sample(c(-1, 1), G, replace = TRUE)[g]; tb[b] <- tstat(y0 + u0 * eta, W, X, g)[2] }
cat("Wild cluster bootstrap-t p-value (2 phía, B = 9999):", round(mean(abs(tb) >= abs(obs[2])), 4), "\n")
# khoảng tin cậy bootstrap-t đảo ngược: dùng phân vị |t*| từ bootstrap không ràng buộc (pairs-t xấp xỉ)
crit <- quantile(abs(tb), 0.95); se <- obs[1] / obs[2]
cat("CI 95% theo phân vị |t*| của wild bootstrap (xấp xỉ): [", round(obs[1] - crit * abs(se), 4), ",", round(obs[1] + crit * abs(se), 4), "]; giá trị tới hạn", round(crit, 3), "\n")

## (B) CRE-DML: nuisance dùng Z = (X_it, trung bình tỉnh của X, trung bình tỉnh của W, năm), không khử trung bình
Z <- cbind(as.matrix(df[, controls]), sapply(controls, function(v) ave(df[[v]], df$province)), Wbar = ave(df$CSTP5, df$province), year = df$year)
colnames(Z)[9:16] <- paste0(controls, "_bar")
cre <- function(seed, ntree = 1000, yv = df$attitude_positive) {
  set.seed(seed); u <- unique(df$province); f <- setNames(sample(rep(1:5, length.out = length(u))), u)[df$province]
  Yr <- Wr <- numeric(nrow(df))
  for (k in 1:5) { tr <- which(f != k); te <- which(f == k)
    Yr[te] <- yv[te] - predict(regression_forest(Z[tr, ], yv[tr], num.trees = ntree), Z[te, ])$predictions
    Wr[te] <- df$CSTP5[te] - predict(regression_forest(Z[tr, ], df$CSTP5[tr], num.trees = ntree), Z[te, ])$predictions }
  fit <- lm(Yr ~ Wr); coeftest(fit, vcov = vcovCL(fit, cluster = df$province, type = "HC1"))["Wr", c(1, 2, 4)]
}
cat("\nCRE-DML (Mundlak), seed 42:\n"); print(round(cre(42), 4))
r <- t(sapply(1:20, cre)); cat("20 seed: hệ số min/median/max:", round(range(r[,1]), 4), round(median(r[,1]), 4), "; p min/median/max:", round(range(r[,3]), 3), round(median(r[,3]), 3), "\n")
cat("\nPlacebo CRE-DML (outcome năm t-1 với CSTP5 năm t):\n")
dfp <- read.csv("pci_panel_placebo.csv", stringsAsFactors = FALSE); dfp <- dfp[order(dfp$province, dfp$year), ]
Zp <- cbind(as.matrix(dfp[, controls]), sapply(controls, function(v) ave(dfp[[v]], dfp$province)), Wbar = ave(dfp$CSTP5, dfp$province), year = dfp$year)
set.seed(42); u <- unique(dfp$province); f <- setNames(sample(rep(1:5, length.out = length(u))), u)[dfp$province]; Yr <- Wr <- numeric(nrow(dfp))
for (k in 1:5) { tr <- which(f != k); te <- which(f == k)
  Yr[te] <- dfp$attitude_positive_lag[te] - predict(regression_forest(Zp[tr, ], dfp$attitude_positive_lag[tr], num.trees = 1000), Zp[te, ])$predictions
  Wr[te] <- dfp$CSTP5[te] - predict(regression_forest(Zp[tr, ], dfp$CSTP5[tr], num.trees = 1000), Zp[te, ])$predictions }
fit <- lm(Yr ~ Wr); print(round(coeftest(fit, vcov = vcovCL(fit, cluster = dfp$province, type = "HC1"))["Wr", c(1, 2, 4)], 4))
