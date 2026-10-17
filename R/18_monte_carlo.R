# BƯỚC 18: Monte Carlo gần cấu trúc dữ liệu (63 tỉnh x 7 năm, treatment liên tục, FE tương quan với treatment, nhiễu phi tuyến).
# Ước lượng so sánh: Panel FE, FE-DML (khử trung bình), FD-DML (sai phân), CRE-DML (Mundlak), Causal Forest tiêu chuẩn.
# Chỉ số: bias, RMSE, coverage CI 95%, độ dài CI. Tham số thật theta = 0,025 (APE).
library(grf); library(sandwich); library(lmtest); library(parallel)
df0 <- read.csv("pci_panel_final.csv", stringsAsFactors = FALSE); df0 <- df0[order(df0$province, df0$year), ]
controls <- paste0("CSTP", c(1,2,3,4,6,8,9,10)); Xs <- scale(as.matrix(df0[, controls])); n <- nrow(df0)
prov <- as.integer(factor(df0$province)); yr <- as.integer(factor(df0$year)); theta <- 0.025
dm2 <- function(v) v - ave(v, prov) - ave(v, yr) + mean(v)
folds_of <- function(K = 5) sample(rep(1:K, length.out = max(prov)))[prov]
cl_fit <- function(Yr, Wr) { fit <- lm(Yr ~ Wr); ct <- coeftest(fit, vcov = vcovCL(fit, cluster = prov_cur, type = "HC1"))["Wr", ]; c(est = ct[1], se = ct[2]) }
rf_resid <- function(target, Z, f, nt) { r <- numeric(length(target)); for (k in unique(f)) { tr <- f != k
  r[!tr] <- target[!tr] - predict(regression_forest(Z[tr, , drop = FALSE], target[tr], num.trees = nt), Z[!tr, , drop = FALSE])$predictions }; r }
gen <- function(het) {
  a <- rnorm(max(prov), 0, 0.07)[prov]; l <- rnorm(max(yr), 0, 0.05)[yr]
  W <- 6.7 + 0.3 * (Xs[,1] - 0.5 * Xs[,2] + 0.2 * Xs[,3]^2) + 7 * a + 0.3 * rnorm(max(yr))[yr] + rnorm(n, 0, 0.35)
  g <- 0.03 * (sin(Xs[,1]) + 0.5 * Xs[,2]^2 + Xs[,3] * Xs[,4])
  tau <- theta * (1 + if (het) 0.8 * Xs[,1] else 0)
  Y <- 0.5 + tau * (W - 6.7) + g + a + l + rnorm(n, 0, 0.05)
  list(Y = Y, W = W)
}
one <- function(rep, het, nt = 300) {
  set.seed(1000 * het + rep); d <- gen(het); Y <- d$Y; W <- d$W; prov_cur <<- prov; out <- list()
  ## Panel FE (OLS hai chiều, SE cụm)
  Yd <- dm2(Y); Wd <- dm2(W); Xd <- apply(Xs, 2, dm2); fit <- lm(Yd ~ Wd + Xd); ct <- coeftest(fit, vcov = vcovCL(fit, cluster = prov, type = "HC1"))["Wd", ]
  out$PanelFE <- c(ct[1], ct[2])
  ## FE-DML
  f <- folds_of(); out$FE_DML <- cl_fit(rf_resid(Yd, Xd, f, nt), rf_resid(Wd, Xd, f, nt))
  ## FD-DML (bỏ năm đầu; khử trung bình theo năm)
  ok <- yr > 1; dl <- function(v) ave(v, prov, FUN = function(z) c(NA, diff(z)))
  Yf <- dl(Y); Wf <- dl(W); Xf <- apply(Xs, 2, dl); Yf <- Yf[ok]; Wf <- Wf[ok]; Xf <- Xf[ok, ]; yy <- yr[ok]
  Yf <- Yf - ave(Yf, yy); Wf <- Wf - ave(Wf, yy); Xf <- apply(Xf, 2, function(v) v - ave(v, yy)); prov_cur <<- prov[ok]
  ff <- sample(rep(1:5, length.out = max(prov)))[prov[ok]]; out$FD_DML <- cl_fit(rf_resid(Yf, Xf, ff, nt), rf_resid(Wf, Xf, ff, nt))
  ## CRE-DML
  prov_cur <<- prov; Z <- cbind(Xs, apply(Xs, 2, function(v) ave(v, prov)), Wbar = ave(W, prov), yr)
  out$CRE_DML <- cl_fit(rf_resid(Y, Z, f, nt), rf_resid(W, Z, f, nt))
  ## Causal Forest tiêu chuẩn (có clusters)
  cf <- causal_forest(cbind(Xs, yr), Y, W, num.trees = 500, clusters = prov, num.threads = 1); a <- average_treatment_effect(cf); out$CF <- c(a[1], a[2])
  unlist(out)
}
run <- function(het, R) { res <- mclapply(1:R, function(r) tryCatch(one(r, het), error = function(e) NULL), mc.cores = 2); do.call(rbind, Filter(Negate(is.null), res)) }
summ <- function(M) { ms <- c("PanelFE", "FE_DML", "FD_DML", "CRE_DML", "CF")
  t(sapply(seq_along(ms), function(j) { e <- M[, 2 * j - 1]; s <- M[, 2 * j]; c(bias = mean(e) - theta, RMSE = sqrt(mean((e - theta)^2)), coverage = mean(abs(e - theta) <= qt(0.975, 62) * s), CI_len = mean(2 * qt(0.975, 62) * s), reps = length(e)) })) -> r; rownames(r) <- ms; round(r, 4) }
R <- as.integer(Sys.getenv("MC_REPS", "150"))
cat("=== Kịch bản 1: tác động đồng nhất (theta = 0,025), R =", R, "===\n"); M1 <- run(0, R); print(summ(M1))
cat("\n=== Kịch bản 2: tác động dị biệt theo X1 (APE = 0,025), R =", R, "===\n"); M2 <- run(1, R); print(summ(M2))
saveRDS(list(M1 = M1, M2 = M2), "mc_results.rds")
