# =============================================================
# BƯỚC 0: XỬ LÝ DỮ LIỆU THÔ - GHÉP FILE PCI THÀNH PANEL SẠCH
# =============================================================
# Phiên bản dùng cho Google Colab, đọc trực tiếp từ Google Drive.
# Không cần đổi tên file - script tự nhận diện năm (2018-2024) có
# trong tên file, dù tên file gốc dài và có mã hash phía trước.
#
# Cài đặt package (chỉ 1 lần):
#   install.packages("readxl")
#   install.packages("stringi")

library(readxl)
library(stringi)

# --- Thiết lập locale UTF-8: BẮT BUỘC để R đọc đúng tên sheet/tên tỉnh tiếng Việt có dấu ---
tryCatch(Sys.setlocale("LC_ALL", "C.UTF-8"), warning = function(w) {
  tryCatch(Sys.setlocale("LC_ALL", "en_US.UTF-8"), warning = function(w2) {
    message("Cảnh báo: không đặt được locale UTF-8, có thể lỗi khi đọc tên tiếng Việt")
  })
})

# ---- 0. MOUNT GOOGLE DRIVE (chỉ áp dụng khi chạy trên Google Colab) ----
# Cách mount: bấm biểu tượng thư mục (Files) ở thanh bên trái Colab ->
# bấm biểu tượng ổ đĩa "Mount Drive" -> cấp quyền truy cập Google tài khoản.
# Việc này áp dụng chung cho toàn bộ notebook (không phân biệt R hay Python),
# nên chỉ cần làm 1 lần trước khi chạy script này.

DATA_DIR <- "/content/drive/MyDrive/PCI_P/data"

if (!dir.exists(DATA_DIR)) {
  stop(paste0(
    "Không tìm thấy thư mục: ", DATA_DIR, "\n",
    "-> Kiểm tra lại: (1) đã Mount Drive chưa, (2) đường dẫn có đúng không.\n",
    "Nếu chạy trên máy cá nhân (không phải Colab), sửa DATA_DIR thành đường dẫn thật trên máy bạn."
  ))
}

# ---- 1. Tự động quét toàn bộ file .xlsx trong thư mục và nhận diện năm ----
all_files <- list.files(DATA_DIR, pattern = "\\.xlsx$", full.names = TRUE)
cat("Tìm thấy", length(all_files), "file .xlsx trong", DATA_DIR, "\n")

extract_year <- function(fn) {
  # tìm chuỗi số đứng riêng lẻ (không dính số khác trước/sau) trong khoảng 2018-2024
  m <- regmatches(fn, regexpr("(?<!\\d)20(1[8-9]|2[0-4])(?!\\d)", fn, perl = TRUE))
  if (length(m) == 0) NA_character_ else m
}

files <- list()
for (fp in all_files) {
  yr <- extract_year(basename(fp))
  if (is.na(yr)) {
    message("CẢNH BÁO: không nhận diện được năm từ tên file, bỏ qua: ", basename(fp))
    next
  }
  if (!is.null(files[[yr]])) {
    stop(paste0("Phát hiện 2 file cùng năm ", yr, " - kiểm tra lại thư mục data, chỉ nên có đúng 1 file/năm."))
  }
  files[[yr]] <- fp
}

cat("Đã nhận diện", length(files), "năm dữ liệu:", paste(sort(names(files)), collapse = ", "), "\n\n")
if (length(files) == 0) stop("Không nhận diện được năm nào từ tên file - kiểm tra lại tên file trong thư mục data.")

BLACKLIST <- c("cao nhất", "thấp nhất", "trung vị", "thứ hạng", "trung bình", "tối đa", "tối thiểu")

# Kiểm tra tên tỉnh có CHỨA từ khóa blacklist hay không (không phải khớp chính xác)
# -- SỬA LỖI: bản đầu tiên dùng khớp chính xác (%in%) đã bỏ sót dòng như
#    "Thứ hạng của tỉnh/thành phố" (chỉ chặn được "thứ hạng" đứng một mình)
is_blacklisted <- function(name) {
  name_low <- tolower(name)
  any(sapply(BLACKLIST, function(kw) grepl(kw, name_low, fixed = TRUE)))
}

# ---- 2. Hàm chuẩn hóa Unicode (khắc phục lỗi NFC/NFD ở tên tỉnh có dấu) ----
norm_text <- function(x) {
  stri_trans_nfc(trimws(as.character(x)))
}

# ---- 3. Hàm tìm vị trí cột CSTP1..CSTP10 dựa trên tên cột (không dựa vị trí cố định) ----
get_cstp_col_map <- function(header_row) {
  map <- list()
  for (i in seq_along(header_row)) {
    h <- header_row[i]
    if (!is.na(h)) {
      h_norm <- norm_text(h)
      m <- regmatches(h_norm, regexpr("^CSTP\\s*[0-9]+", h_norm))
      if (length(m) > 0) {
        num <- as.integer(gsub("[^0-9]", "", m))
        map[[as.character(num)]] <- i
      }
    }
  }
  map
}

# =============================================================
# PHẦN A: GHÉP SHEET "Tổng hợp" QUA 5 NĂM -> ĐIỂM 10 CHỈ SỐ CON + VÙNG
# =============================================================
panel_list <- list()
region_map <- list()

for (yr in names(files)) {
  fp <- files[[yr]]
  raw <- suppressMessages(read_excel(fp, sheet = "Tổng hợp", col_names = FALSE))
  header <- as.character(raw[1, ])
  cstp_map <- get_cstp_col_map(header)

  region_idx <- NA
  for (i in seq_along(header)) {
    if (!is.na(header[i]) && grepl("Vùng", norm_text(header[i]))) region_idx <- i
  }

  data_rows <- raw[-1, ]
  for (r in seq_len(nrow(data_rows))) {
    province_raw <- data_rows[[1]][r]
    if (is.na(province_raw)) next
    province <- norm_text(province_raw)
    if (is_blacklisted(province) || province == "") next

    rec <- list(province = province, year = as.integer(yr))
    for (k in 1:10) {
      col_i <- cstp_map[[as.character(k)]]
      rec[[paste0("CSTP", k)]] <- if (!is.null(col_i)) as.numeric(data_rows[[col_i]][r]) else NA
    }
    if (!is.na(region_idx)) {
      region_val <- norm_text(data_rows[[region_idx]][r])
      rec$region <- region_val
      if (region_val != "" && !is.na(region_val)) region_map[[province]] <- region_val
    } else {
      rec$region <- NA
    }
    panel_list[[length(panel_list) + 1]] <- as.data.frame(rec, stringsAsFactors = FALSE)
  }
}

panel <- do.call(rbind, panel_list)

# điền vùng miền còn thiếu bằng cách tra từ các năm khác có sẵn
for (i in seq_len(nrow(panel))) {
  if (is.na(panel$region[i]) || panel$region[i] == "") {
    p <- panel$province[i]
    if (!is.null(region_map[[p]])) panel$region[i] <- region_map[[p]]
  }
}

# loại bỏ dòng trùng lặp hoàn toàn (lỗi có sẵn trong 1 số file Excel gốc)
panel <- panel[!duplicated(panel[, c("province", "year")]), ]

cat("=== Panel cơ sở (Tổng hợp) ===\n")
cat("Số dòng:", nrow(panel), "| Số tỉnh:", length(unique(panel$province)), "\n")

# =============================================================
# PHẦN B: TRÍCH BIẾN OUTCOME TỪ SHEET "Tính Năng động"
# =============================================================
outcome_list <- list()

for (yr in names(files)) {
  fp <- files[[yr]]
  raw <- suppressMessages(read_excel(fp, sheet = "Tính Năng động", col_names = FALSE))
  header <- as.character(raw[1, ])

  outcome_idx <- NA
  for (i in seq_along(header)) {
    if (!is.na(header[i])) {
      h_low <- tolower(norm_text(header[i]))
      if (grepl("thái độ", h_low) && grepl("tích cực", h_low)) outcome_idx <- i
    }
  }
  if (is.na(outcome_idx)) stop(paste("Không tìm thấy cột outcome ở năm", yr, "- kiểm tra lại tên cột trong file Excel"))

  data_rows <- raw[-1, ]
  for (r in seq_len(nrow(data_rows))) {
    province_raw <- data_rows[[1]][r]
    if (is.na(province_raw)) next
    province <- norm_text(province_raw)
    if (is_blacklisted(province) || province == "") next
    outcome_list[[length(outcome_list) + 1]] <- data.frame(
      province = province,
      year = as.integer(yr),
      attitude_positive = as.numeric(data_rows[[outcome_idx]][r]),
      stringsAsFactors = FALSE
    )
  }
}

outcome_df <- do.call(rbind, outcome_list)
outcome_df <- outcome_df[!duplicated(outcome_df[, c("province", "year")]), ]

# =============================================================
# PHẦN C: GHÉP A + B, KIỂM ĐỊNH CHẤT LƯỢNG, XUẤT FILE
# =============================================================
final <- merge(panel, outcome_df, by = c("province", "year"), all.x = TRUE)

cat("\n=== KIỂM ĐỊNH CHẤT LƯỢNG DỮ LIỆU ===\n")
n_years <- length(files)
cat("Tổng số dòng:", nrow(final), sprintf("(kỳ vọng: 63 tỉnh x %d năm = %d)\n", n_years, 63 * n_years))
cat("Số tỉnh duy nhất:", length(unique(final$province)), "(kỳ vọng: 63)\n")
cat("Số dòng thiếu outcome:", sum(is.na(final$attitude_positive)), "(kỳ vọng: 0)\n")
for (k in 1:10) {
  col <- paste0("CSTP", k)
  bad <- sum(final[[col]] < 0 | final[[col]] > 10, na.rm = TRUE)
  if (bad > 0) cat("CẢNH BÁO:", col, "có", bad, "giá trị ngoài khoảng [0,10]\n")
}

write.csv(final, "pci_panel_final.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("\nĐã lưu: pci_panel_final.csv\n")
