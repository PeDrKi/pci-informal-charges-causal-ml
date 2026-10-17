# BƯỚC 15: kiểm định trực tiếp chênh lệch giữa hai giai đoạn (treatment x giai đoạn), cùng mẫu, SE theo cụm tỉnh.
library(grf); library(plm); library(sandwich); library(lmtest)
df <- read.csv("pci_panel_final.csv", stringsAsFactors = FALSE); df <- df[order(df$province, df$year), ]
controls <- paste0("CSTP", c(1,2,3,4,6,8,9,10)); df$late <- as.numeric(df$year >= 2022)
dm2 <- function(d, v) d[[v]] - ave(d[[v]], d$province) - ave(d[[v]], d$year) + mean(d[[v]])
# (a) Panel FE với tương tác
df$W_late <- df$CSTP5 * df$late
m <- plm(as.formula(paste("attitude_positive ~ CSTP5 + W_late +", paste(controls, collapse="+"))), data = pdata.frame(df, index = c("province","year")), model = "within", effect = "twoways")
cat("== Panel FE: CSTP5 và CSTP5 x (2022-2024) ==\n"); print(coeftest(m, vcov = vcovHC(m, type = "HC1", cluster = "group"))[c("CSTP5","W_late"), ])
# (b) FE-DML: phần dư Y, W (rừng, fold theo tỉnh) rồi hồi quy Yr ~ Wr + Wr:late
Y <- dm2(df, "attitude_positive"); W <- dm2(df, "CSTP5"); X <- sapply(controls, function(v) dm2(df, v))
res <- function(seed) {
  set.seed(seed); u <- unique(df$province); f <- setNames(sample(rep(1:5, length.out = length(u))), u)[df$province]
  Yr <- Wr <- numeric(nrow(df))
  for (k in 1:5) { tr <- which(f != k); te <- which(f == k)
    Yr[te] <- Y[te] - predict(regression_forest(X[tr,], Y[tr], num.trees = 1000), X[te,])$predictions
    Wr[te] <- W[te] - predict(regression_forest(X[tr,], W[tr], num.trees = 1000), X[te,])$predictions }
  fit <- lm(Yr ~ Wr + I(Wr * df$late)); coeftest(fit, vcov = vcovCL(fit, cluster = df$province, type = "HC1"))[2:3, c(1,2,4)]
}
cat("\n== FE-DML tương tác theo seed (hàng 1: hệ số Wr = giai đoạn đầu; hàng 2: chênh lệch giai đoạn sau) ==\n")
r <- lapply(1:20, res)
est <- sapply(r, function(z) z[,1]); p <- sapply(r, function(z) z[,3]); se <- sapply(r, function(z) z[,2])
cat("Hệ số giai đoạn đầu: median", round(median(est[1,]),4), "range", round(range(est[1,]),4), "\n")
cat("Chênh lệch giai đoạn sau: median", round(median(est[2,]),4), "range", round(range(est[2,]),4), "; SE median", round(median(se[2,]),4), "; p median", round(median(p[2,]),3), "range", round(range(p[2,]),3), "\n")
