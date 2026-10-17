# Chạy theo thứ tự. Đặt working directory = thư mục này. Cần: plm, grf (>= 2.6), sandwich, lmtest, glmnet, gbm.
# Bước 00 chỉ cần khi bắt đầu từ các tệp Excel PCI thô (xem docs/pci_provenance_template.csv); dữ liệu đã xử lý nằm ở data_processed/.
setwd("results")                      # các script đọc pci_panel_final.csv trong thư mục hiện hành
file.copy(list.files("../data_processed", full.names = TRUE), ".", overwrite = TRUE)
for (f in c("13_clustered_inference.R",

            "16_panel_fe_clustered_tables.R", "17_wild_bootstrap_cre_dml.R", "14a_fd_dml_robust.R", "15_interaction_period.R",
            "14b_bootstrap_cate_B1000.R",
            "18_monte_carlo.R"))   # 18 chạy ~15 phút   # 14b chạy lâu (~10-20 phút/2 lõi)
  source(file.path("../R", f), echo = FALSE)
