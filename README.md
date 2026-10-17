# Chi phí không chính thức (PCI-CSTP5) và đánh giá chính quyền tỉnh: Panel FE, FE-DML, FD-DML, Causal Forest

Mã và dữ liệu đã xử lý đi kèm bài báo gửi Tạp chí Kinh tế – Luật và Ngân hàng.

## Dữ liệu
- `data_processed/pci_panel_final.csv`: bảng cân bằng 63 tỉnh × 2018–2024 (441 quan sát) lập từ các tệp PCI công khai (VCCI & USAID, https://pcivietnam.vn). Outcome: `attitude_positive` (tỷ lệ doanh nghiệp đánh giá thái độ chính quyền tỉnh là tích cực); treatment: `CSTP5`; kiểm soát: CSTP1–4, 6, 8–10.
- `data_processed/pci_panel_placebo.csv`: cùng dữ liệu, ghép CSTP5(t) với outcome(t−1).
- Dữ liệu thô không được phân phối lại; tải trực tiếp từ nguồn. `docs/pci_provenance_template.csv` là bảng đối chiếu nguồn theo năm, **cần điền** (tệp, câu hỏi, mẫu số, cách tính CSTP5, thay đổi đo lường).

## Môi trường
R ≥ 4.3; `plm`, `grf` 2.6.1, `sandwich`, `lmtest`, `glmnet`, `gbm`. Nên dùng `renv::init()` rồi `renv::snapshot()` để khóa phiên bản (chưa tạo `renv.lock`).

## Thứ tự và ánh xạ script → kết quả trong bài
Các script `00`–`12` là bản gốc (suy luận **không** theo cụm tỉnh). Các số trong bài dùng suy luận theo cụm tỉnh, được tính bằng các script sau:

| Script | Nội dung | Bảng/mục trong bài |
|---|---|---|
| `00_xu_ly_du_lieu.R` | ghép tệp PCI thô thành panel | Mục 3.4 |
| `13_clustered_inference.R` | Panel FE (SE cụm), Causal Forest (clusters), FE-DML (fold theo tỉnh, SE cụm), placebo một độ trễ | Bảng 4 (hệ số CSTP5), 5, 6 |
| `14a_fd_dml_robust.R` | FD-DML, placebo nhiều độ trễ, độ vững riêng cho FE-DML, thống kê PCI theo năm | Bảng 7, 9; Mục 3.4 |
| `14b_bootstrap_cate_B1000.R` | bootstrap CATE B = 1.000, dải sup-t | Mục 4.4 |
| `15_interaction_period.R` | kiểm định trực tiếp chênh lệch giữa hai giai đoạn | Mục 4.5 |
| `02c.R`, `03c.R`, `10c.R`, `11c.R` | biến thể có `clusters` của 02, 03, 10, 11 (CF tiêu chuẩn, treatment CSTP3, FE-Causal Forest theo vùng, minh họa quy mô) | Mục 4.2, 4.4, 4.5 |
| `16_panel_fe_clustered_tables.R` | Bảng 4 đầy đủ và Bảng 7 (Panel FE) | Bảng 4, 7 |
| `17_wild_bootstrap_cre_dml.R` | wild cluster bootstrap-t (B = 9.999) và CRE-DML | Bảng 7, 8 |
| `18_monte_carlo.R` | Monte Carlo 200 lần lặp (biến `MC_REPS`; ~15 phút/2 lõi) | Bảng 9 |
| `01`, `04`–`07`, `08`–`09`, `12` | bản gốc không theo cụm (Panel FE đầy đủ hệ số, loại HN/HCM, LOPO, cluster bootstrap, Oster δ, FE-DML gốc, bootstrap B = 200) | tham khảo |

`16_panel_fe_clustered_tables.R` tái tạo Bảng 4 (Panel FE đầy đủ, SE theo cụm tỉnh) và Bảng 7 (loại HN/HCM, LOPO, cluster bootstrap B = 500, Oster δ); kết quả khớp bài (`results/out16.txt`).

`results/out13.txt`, `out14a.txt`, `out14b.txt`, `out16.txt`, `out17.txt`, `out18.txt` và `bootstrap_cate_ci_B1000.csv` là kết quả chạy đã dùng trong bài.

## Seed và thời gian chạy
Cấu hình chính: seed 42 cho chia fold và rừng (13, 14a); 14a lặp 20 seed (1–20) để báo cáo độ ổn định; 14b dùng seed 20260101. Ước lượng điểm của rừng thay đổi nhẹ giữa các lần chạy/máy (ví dụ 0,0255 ở bản 13 và 0,0257 ở 14a); bài dùng 14a làm cấu hình chính cho FE-DML. 14a ~8 phút, 14b ~15–20 phút trên 2 lõi.

## Hạn chế đã biết
FE-DML dùng khử hiệu ứng cố định thủ công, chưa phải thủ tục DML-panel đầy đủ (Clarke & Polselli, 2026); Monte Carlo chỉ dùng một thiết kế hiệu ứng cố định cộng gộp do tác giả chọn, không phải chứng minh tổng quát. Mã 08–12 là bản gốc, chưa dùng suy luận theo cụm.
# pci-informal-charges-causal-ml
