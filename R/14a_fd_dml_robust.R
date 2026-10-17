# BƯỚC 14a: FE-DML (khử trung bình) vs FD-DML (sai phân bậc nhất), placebo nhiều lag,
# độ vững trực tiếp cho FE-DML, PCI theo năm.
library(grf); library(plm); library(sandwich); library(lmtest); library(glmnet)
df <- read.csv("pci_panel_final.csv", stringsAsFactors = FALSE)
df <- df[order(df$province, df$year), ]
controls <- paste0("CSTP", c(1,2,3,4,6,8,9,10))
dm2 <- function(d, v) d[[v]] - ave(d[[v]], d$province) - ave(d[[v]], d$year) + mean(d[[v]])

# ---- learner generic: trả về phần dư cross-fit; fold theo tỉnh ----
resid_cf <- function(target, X, folds, learner = "rf", ntree = 1000) {
  r <- numeric(length(target))
  for (k in sort(unique(folds))) {
    tr <- which(folds != k); te <- which(folds == k)
    pred <- switch(learner,
      rf = predict(regression_forest(X[tr, , drop=FALSE], target[tr], num.trees = ntree), X[te, , drop=FALSE])$predictions,
      lm = { f <- lm(target[tr] ~ ., data = as.data.frame(X[tr, , drop=FALSE])); as.numeric(predict(f, as.data.frame(X[te, , drop=FALSE]))) },
      enet = { g <- cv.glmnet(X[tr, , drop=FALSE], target[tr], alpha = 0.5); as.numeric(predict(g, X[te, , drop=FALSE], s = "lambda.min")) },
      gbm = { g <- gbm::gbm(y ~ ., data = data.frame(y = target[tr], X[tr, , drop=FALSE]), distribution = "gaussian",
                            n.trees = 300, interaction.depth = 2, shrinkage = 0.05, bag.fraction = 0.7, verbose = FALSE)
              as.numeric(predict(g, data.frame(X[te, , drop=FALSE]), n.trees = 300)) })
    r[te] <- target[te] - pred
  }
  r
}
make_folds <- function(prov, K, seed) {
  set.seed(seed); u <- unique(prov)
  setNames(sample(rep(1:K, length.out = length(u))), u)[as.character(prov)]
}
final <- function(Yr, Wr, prov) {
  fit <- lm(Yr ~ Wr)
  coeftest(fit, vcov = vcovCL(fit, cluster = prov, type = "HC1"))["Wr", c(1, 2, 4)]
}
fe_dml <- function(d, yvar = "attitude_positive", ctrl = controls, K = 5, seed = 42, learner = "rf") {
  Y <- dm2(d, yvar); W <- dm2(d, "CSTP5"); X <- sapply(ctrl, function(v) dm2(d, v))
  f <- make_folds(d$province, K, seed)
  final(resid_cf(Y, X, f, learner), resid_cf(W, X, f, learner), d$province)
}
# FD: sai phân trong tỉnh, rồi khử trung bình theo năm (hiệu ứng năm)
fd_data <- function(d, yvar = "attitude_positive", ctrl = controls, lag = 1) {
  d <- d[order(d$province, d$year), ]
  dl <- function(v) ave(d[[v]], d$province, FUN = function(z) c(rep(NA, lag), diff(z, lag = lag)))
  out <- data.frame(province = d$province, year = d$year, Y = dl(yvar), W = dl("CSTP5"))
  for (v in ctrl) out[[v]] <- dl(v)
  out <- out[complete.cases(out), ]
  for (v in c("Y", "W", ctrl)) out[[v]] <- out[[v]] - ave(out[[v]], out$year)
  out
}
fd_dml <- function(d, ctrl = controls, K = 5, seed = 42, learner = "rf", yvar = "attitude_positive") {
  o <- fd_data(d, yvar, ctrl); X <- as.matrix(o[, ctrl, drop = FALSE])
  f <- make_folds(o$province, K, seed)
  c(final(resid_cf(o$Y, X, f, learner), resid_cf(o$W, X, f, learner), o$province), n = nrow(o))
}
fd_ols <- function(d, ctrl = controls) {
  o <- fd_data(d, ctrl = ctrl); fit <- lm(as.formula(paste("Y ~ W +", paste(ctrl, collapse = "+"))), data = o)
  c(coeftest(fit, vcov = vcovCL(fit, cluster = o$province, type = "HC1"))["W", c(1, 2, 4)], n = nrow(o))
}
pr <- function(lbl, x) cat(sprintf("%-46s est=%8.4f  se=%7.4f  p=%6.3f%s\n", lbl, x[1], x[2], x[3], if (length(x) > 3) paste0("  n=", x[4]) else ""))

cat("##### 1. ƯỚC LƯỢNG CHÍNH: FE-DML (demeaning) vs FD-DML (sai phân) #####\n")
pr("FE-DML rf (demean), K=5, seed 42", fe_dml(df))
pr("FD-DML rf (sai phân + khử năm), K=5", fd_dml(df))
pr("FD-OLS (sai phân + khử năm, tuyến tính)", fd_ols(df))
fd20 <- t(sapply(1:20, function(s) fd_dml(df, seed = s)[1:3]))
cat("FD-DML qua 20 seed: est min/med/max =", round(range(fd20[,1]),4), round(median(fd20[,1]),4),
    "; p min/med/max =", round(range(fd20[,3]),3), round(median(fd20[,3]),3), "\n")
fe20 <- t(sapply(1:20, function(s) fe_dml(df, seed = s)[1:3]))
cat("FE-DML qua 20 seed: est min/med/max =", round(range(fe20[,1]),4), round(median(fe20[,1]),4),
    "; p min/med/max =", round(range(fe20[,3]),3), round(median(fe20[,3]),3), "\n")

cat("\n##### 2. PLACEBO NHIỀU LAG: CSTP5(t) với outcome(t-k), k=1,2,3 #####\n")
mk_placebo <- function(d, k) {
  d <- d[order(d$province, d$year), ]
  d$Ylag <- ave(d$attitude_positive, d$province, FUN = function(z) c(rep(NA, k), head(z, -k)))
  d[!is.na(d$Ylag), ]
}
for (k in 1:3) {
  dk <- mk_placebo(df, k)
  m <- plm(as.formula(paste("Ylag ~ CSTP5 +", paste(controls, collapse = "+"))), data = pdata.frame(dk, index = c("province", "year")), model = "within", effect = "twoways")
  ct <- coeftest(m, vcov = vcovHC(m, type = "HC1", cluster = "group"))["CSTP5", c(1, 2, 4)]
  pr(sprintf("Panel FE placebo lag %d (n=%d)", k, nrow(dk)), c(ct, n = nrow(dk)))
  pr(sprintf("FE-DML placebo lag %d", k), c(fe_dml(dk, "Ylag"), n = nrow(dk)))
}
cat("-- Placebo ở dạng sai phân: ΔCSTP5(t) với ΔY(t-1) --\n")
d1 <- df[order(df$province, df$year), ]
d1$Ylag1 <- ave(d1$attitude_positive, d1$province, FUN = function(z) c(NA, head(z, -1)))
d1 <- d1[!is.na(d1$Ylag1), ]
pr("FD-DML placebo (ΔW_t vs ΔY_{t-1})", fd_dml(d1, yvar = "Ylag1"))
pr("FD-OLS placebo", { o <- fd_data(d1, yvar = "Ylag1"); fit <- lm(as.formula(paste("Y ~ W +", paste(controls, collapse="+"))), data = o); c(coeftest(fit, vcov = vcovCL(fit, cluster = o$province, type = "HC1"))["W", c(1,2,4)], n = nrow(o)) })
cat("-- Lead/lag treatment: Y(t) ~ CSTP5(t) + CSTP5(t-1) (FE-DML trên mẫu có lag) --\n")
dd <- df[order(df$province, df$year), ]
dd$W1 <- ave(dd$CSTP5, dd$province, FUN = function(z) c(NA, head(z, -1)))
dd <- dd[!is.na(dd$W1), ]
Y <- dm2(dd, "attitude_positive"); W <- dm2(dd, "CSTP5"); W1 <- dm2(dd, "W1"); X <- sapply(controls, function(v) dm2(dd, v))
f <- make_folds(dd$province, 5, 42)
Yr <- resid_cf(Y, X, f); Wr <- resid_cf(W, X, f); W1r <- resid_cf(W1, X, f)
fit <- lm(Yr ~ Wr + W1r); print(round(coeftest(fit, vcov = vcovCL(fit, cluster = dd$province, type = "HC1")), 4))

cat("\n##### 3. ĐỘ VỮNG TRỰC TIẾP CHO FE-DML (và FD-DML) #####\n")
big2 <- c("Thành phố Hà Nội", "Thành phố Hồ Chí Minh", "Hà Nội", "TP. Hồ Chí Minh", "TP.HCM", "Hồ Chí Minh")
cat("Tên tỉnh có Hà Nội/HCM:", grep("Hà Nội|Hồ Chí Minh|HCM", unique(df$province), value = TRUE), "\n")
sel <- !grepl("Hà Nội|Hồ Chí Minh|HCM", df$province)
pr("FE-DML loại Hà Nội & TP.HCM", fe_dml(df[sel, ]))
pr("FD-DML loại Hà Nội & TP.HCM", fd_dml(df[sel, ]))
for (lr in c("lm", "enet", "gbm")) { pr(paste("FE-DML learner =", lr), fe_dml(df, learner = lr)); pr(paste("FD-DML learner =", lr), fd_dml(df, learner = lr)) }
pr("FE-DML K=10", fe_dml(df, K = 10)); pr("FD-DML K=10", fd_dml(df, K = 10))
pr("FE-DML K=3", fe_dml(df, K = 3))
cat("-- Bỏ từng biến kiểm soát (FE-DML) --\n")
for (v in controls) pr(paste("FE-DML bỏ", v), fe_dml(df, ctrl = setdiff(controls, v)))
pr("FE-DML không biến kiểm soát nào (chỉ phần dư trên 1 cột hằng)", { Y <- dm2(df, "attitude_positive"); W <- dm2(df, "CSTP5"); final(Y - mean(Y), W - mean(W), df$province) })
cat("-- Theo giai đoạn --\n")
pr("FE-DML 2018-2021", fe_dml(df[df$year <= 2021, ])); pr("FE-DML 2022-2024", fe_dml(df[df$year >= 2022, ]))
pr("FD-DML 2018-2021", fd_dml(df[df$year <= 2021, ])); pr("FD-DML 2022-2024", fd_dml(df[df$year >= 2022, ]))
cat("-- Bỏ từng năm --\n")
for (y in sort(unique(df$year))) pr(paste("FE-DML bỏ năm", y), fe_dml(df[df$year != y, ]))
cat("-- Leave-one-province-out (FE-DML, 63 lần) --\n")
provs <- unique(df$province)
lo <- t(sapply(provs, function(p) fe_dml(df[df$province != p, ], seed = 1)))
cat("Ước lượng min/max:", round(range(lo[,1]), 4), "; p max:", round(max(lo[,3]), 3), "; số lần p<0.05:", sum(lo[,3] < 0.05), "/63\n")
cat("Tỉnh làm ước lượng thấp nhất/cao nhất:", provs[which.min(lo[,1])], provs[which.max(lo[,1])], "\n")

cat("\n##### 4. CSTP5 VÀ OUTCOME THEO NĂM (tính so sánh PCI) #####\n")
tab <- do.call(rbind, lapply(split(df, df$year), function(g) data.frame(year = g$year[1], n = nrow(g),
  CSTP5_mean = mean(g$CSTP5), CSTP5_sd = sd(g$CSTP5), CSTP5_p10 = quantile(g$CSTP5, .1), CSTP5_med = median(g$CSTP5), CSTP5_p90 = quantile(g$CSTP5, .9),
  Y_mean = mean(g$attitude_positive), Y_sd = sd(g$attitude_positive))))
print(round(tab, 3), row.names = FALSE)
cat("-- Chuẩn hóa z-score CSTP5 và outcome trong từng năm (loại bỏ khác biệt thang đo giữa năm) --\n")
dz <- df; zs <- function(v) ave(dz[[v]], dz$year, FUN = function(z) (z - mean(z)) / sd(z))
for (v in c("attitude_positive", "CSTP5", controls)) dz[[v]] <- zs(v)
pr("FE-DML với biến z-score theo năm (hệ số theo SD)", fe_dml(dz))
cat("-- CSTP5 sd within-tỉnh theo năm: tương quan CSTP5 giữa các năm liền kề --\n")
w <- reshape(df[, c("province", "year", "CSTP5")], idvar = "province", timevar = "year", direction = "wide")
print(round(cor(w[, -1], use = "pair")[cbind(1:6, 2:7)], 3))
cat("Tự tương quan lag-1 trong tỉnh (sau khử FE) của CSTP5:", round(cor(dm2(dd, "CSTP5"), dm2(dd, "W1")), 3), "\n")
