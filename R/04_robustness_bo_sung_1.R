library(plm)
library(grf)

df <- read.csv("pci_panel_final.csv", stringsAsFactors = FALSE)
controls <- paste0("CSTP", c(1,2,3,4,6,8,9,10))

# ===== 1. LOẠI HÀ NỘI VÀ TP.HCM (2 tỉnh có thể là outlier do quy mô đặc biệt) =====
df_excl <- df[!(df$province %in% c("Hà Nội", "TP.HCM")), ]
pdata_excl <- pdata.frame(df_excl, index = c("province","year"))
f <- as.formula(paste("attitude_positive ~ CSTP5 +", paste(controls, collapse=" + ")))
fe_excl <- plm(f, data=pdata_excl, model="within", effect="twoways")
cat("=== Loại Hà Nội, TP.HCM (N=", nrow(df_excl), ") ===\n")
print(summary(fe_excl)$coefficients["CSTP5",])

# ===== 2. WILD CLUSTER BOOTSTRAP (thay cho cluster-robust SE thường, phù hợp hơn khi ít cluster) =====
# Cài package nếu cần
if (!requireNamespace("fwildclusterboot", quietly=TRUE)) {
  install.packages("fwildclusterboot", repos="https://cloud.r-project.org")
}
