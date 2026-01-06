# Histogram yield plot 
# Author: Janine
# Date: 14 Sep

# ---- Histogram of yield_kg_ha_delineated ----
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)   
})

# 1) Load data
df_raw <- read_csv("UQ Cocoa R/2_Data/farms_with_aez_shade_densities.csv", show_col_types = FALSE)

# 2) Check the column exists
if (!"yield_kg_ha_delineated" %in% names(df_raw)) {
  stop("Column 'yield_kg_ha_delineated' not found.\nAvailable columns:\n",
       paste(names(df_raw), collapse = ", "))
}

# 3) Clean to a numeric vector (drops non-numeric/NA)
df <- df_raw %>%
  transmute(yield = suppressWarnings(as.numeric(yield_kg_ha_delineated))) %>%
  filter(is.finite(yield))

n_all <- nrow(df_raw)
n_used <- nrow(df)
n_drop <- n_all - n_used

# 4) Ensure output directory exists
dir.create("output", showWarnings = FALSE)

# 5) BASE R hist() version (as requested)
png("output/hist_yield_base.png", width = 1200, height = 900, res = 150)
par(mar = c(4.5, 4.5, 3.5, 1.5))
h <- hist(
  df$yield,
  breaks = "FD",              # Freedman–Diaconis rule (sensible default)
  main = "Histogram of Yield (kg/ha)",
  xlab  = "Yield (kg/ha)",
  ylab  = "Frequency",
  col   = "grey85",
  border= "grey30"
)
abline(v = mean(df$yield),   lwd = 2, lty = 1, col = "black")
abline(v = median(df$yield), lwd = 2, lty = 2, col = "black")
legend("topright",
       legend = c("Mean", "Median"),
       lwd = 2, lty = c(1, 2), bty = "n")
mtext(sprintf("N used: %d of %d (dropped: %d)", n_used, n_all, n_drop),
      side = 3, line = 0.2, cex = 0.9)
dev.off()

cat("Saved base R histogram to: ", normalizePath("output/hist_yield_base.png"), "\n")

# 6) OPTIONAL: ggplot2 grayscale version
p <- ggplot(df, aes(x = yield)) +
  geom_histogram(bins = 30, fill = "grey80", colour = "grey30") +
  geom_vline(aes(xintercept = mean(yield)), linetype = 1) +
  geom_vline(aes(xintercept = median(yield)), linetype = 2) +
  labs(title = "Histogram of Yield (kg/ha)",
       x = "Yield (kg/ha)", y = "Count",
       subtitle = sprintf("N used: %d of %d (dropped: %d)", n_used, n_all, n_drop)) +
  theme_minimal(base_size = 13)

ggsave("output/hist_yield_ggplot_bw.png", p, width = 10, height = 7, dpi = 150)
cat("Saved ggplot histogram to: ", normalizePath("output/hist_yield_ggplot_bw.png"), "\n")

# ====== end =====
# ====== adjust x axis =====
# ---- Histogram of yield_kg_ha_delineated (x-axis 0..30,000) + drop reasons ----
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(ggplot2)
})

# 1) Load
df_raw <- read_csv("UQ Cocoa R/2_Data/farms_with_aez_shade_densities.csv", show_col_types = FALSE) %>%
  mutate(.row_id = row_number())  # keep original row numbers for auditing

if (!"yield_kg_ha_delineated" %in% names(df_raw)) {
  stop("Column 'yield_kg_ha_delineated' not found.\nAvailable columns:\n",
       paste(names(df_raw), collapse = ", "))
}

# 2) Parse and diagnose drop reasons
#    We: (a) coerce to character, (b) trim, (c) coerce to numeric, (d) label reasons
tmp <- df_raw %>%
  mutate(
    yield_raw_chr = trimws(as.character(yield_kg_ha_delineated)),
    yield_num     = suppressWarnings(as.numeric(yield_raw_chr)),
    drop_reason = case_when(
      # Explicit missing/blank
      is.na(yield_kg_ha_delineated) | yield_raw_chr %in% c("", "NA", "N/A", "NaN", "na", "n/a") ~ "missing",
      # Non-numeric text (coercion produced NA but the original wasn't missing)
      !is.na(yield_raw_chr) & is.na(yield_num) ~ "non-numeric text",
      is.infinite(yield_num) ~ "infinite",
      is.nan(yield_num) ~ "NaN",
      # Negative yields (keep or drop?—common to drop; we mark as 'negative value')
      yield_num < 0 ~ "negative value",
      TRUE ~ "kept"
    )
  )

# What we keep for analysis (only valid finite numbers)
df <- tmp %>% filter(drop_reason == "kept") %>% transmute(yield = yield_num)

n_all   <- nrow(tmp)
n_used  <- nrow(df)
n_drop  <- n_all - n_used

# 3) Report why rows were dropped
reason_counts <- tmp %>%
  filter(drop_reason != "kept") %>%
  count(drop_reason, sort = TRUE)

cat("\n--- Row usage summary ---\n")
cat(sprintf("Total rows: %d\nUsed in histogram: %d\nDropped: %d\n\n", n_all, n_used, n_drop))
if (n_drop > 0) {
  cat("Reasons for drop:\n")
  print(reason_counts)
} else {
  cat("No rows were dropped.\n")
}

# Save the dropped rows for inspection
if (n_drop > 0) {
  dir.create("output", showWarnings = FALSE)
  dropped_detail <- tmp %>%
    filter(drop_reason != "kept") %>%
    select(.row_id, yield_original = yield_raw_chr, drop_reason)
  write.csv(dropped_detail, "output/dropped_yield_rows.csv", row.names = FALSE)
  cat("Wrote details of dropped rows to: ", normalizePath("output/dropped_yield_rows.csv"), "\n")
}

# 4) Ensure output dir
dir.create("output", showWarnings = FALSE)

# 5) Base R hist() with x-axis capped at 0..30,000
png("output/hist_yield_base_xmax30000.png", width = 1200, height = 900, res = 150)
par(mar = c(4.5, 4.5, 3.5, 1.5))
h <- hist(
  df$yield,
  breaks = "FD",
  main = "Histogram of Yield 2022 (kg/ha)",
  xlab  = "Yield (kg/ha)",
  ylab  = "Frequency",
  col   = "grey85",
  border= "grey30",
  xlim  = c(0, 30000)   # <- requested axis range
)
abline(v = mean(df$yield),   lwd = 2, lty = 1, col = "black")
abline(v = median(df$yield), lwd = 2, lty = 2, col = "black")
legend("topright", legend = c("Mean", "Median"), lwd = 2, lty = c(1, 2), bty = "n")
mtext(sprintf("N used: %d of %d (dropped: %d)", n_used, n_all, n_drop),
      side = 3, line = 0.2, cex = 0.9)
dev.off()
cat("Saved base R histogram to: ", normalizePath("output/hist_yield_base_xmax30000.png"), "\n")

# 6) ggplot version (keeps all data but only shows 0..30,000)
p <- ggplot(df, aes(x = yield)) +
  geom_histogram(bins = 30, fill = "grey80", colour = "grey30") +
  geom_vline(aes(xintercept = mean(yield)), linetype = 1) +
  geom_vline(aes(xintercept = median(yield)), linetype = 2) +
  coord_cartesian(xlim = c(0, 30000)) +   # <- show 0..30,000 without dropping data
  labs(
    title = "Histogram of Yield 2022 (kg/ha)",
    x = "Yield (kg/ha)", y = "Count",
    subtitle = sprintf("N used: %d of %d (dropped: %d)", n_used, n_all, n_drop)
  ) +
  theme_minimal(base_size = 13)

ggsave("output/hist_yield_ggplot_xmax30000.png", p, width = 10, height = 7, dpi = 150)
cat("Saved ggplot histogram to: ", normalizePath("output/hist_yield_ggplot_xmax30000.png"), "\n")

# ==== End =====
# ==== generate 2021 =====
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(ggplot2)
})

# 1) Load
df_raw <- read_csv("UQ Cocoa R/2_Data/farms_with_aez_shade_densities.csv", show_col_types = FALSE) %>%
  mutate(.row_id = row_number())  # keep original row numbers for auditing

if (!"yield_kg_ha_2021" %in% names(df_raw)) {
  stop("Column 'yield_kg_ha_2021' not found.\nAvailable columns:\n",
       paste(names(df_raw), collapse = ", "))
}

# 2) Parse and diagnose drop reasons
#    We: (a) coerce to character, (b) trim, (c) coerce to numeric, (d) label reasons
tmp <- df_raw %>%
  mutate(
    yield_raw_chr = trimws(as.character(yield_kg_ha_2021)),
    yield_num     = suppressWarnings(as.numeric(yield_raw_chr)),
    drop_reason = case_when(
      # Explicit missing/blank
      is.na(yield_kg_ha_2021) | yield_raw_chr %in% c("", "NA", "N/A", "NaN", "na", "n/a") ~ "missing",
      # Non-numeric text (coercion produced NA but the original wasn't missing)
      !is.na(yield_raw_chr) & is.na(yield_num) ~ "non-numeric text",
      is.infinite(yield_num) ~ "infinite",
      is.nan(yield_num) ~ "NaN",
      # Negative yields (keep or drop?—common to drop; we mark as 'negative value')
      yield_num < 0 ~ "negative value",
      TRUE ~ "kept"
    )
  )

# What we keep for analysis (only valid finite numbers)
df <- tmp %>% filter(drop_reason == "kept") %>% transmute(yield = yield_num)

n_all   <- nrow(tmp)
n_used  <- nrow(df)
n_drop  <- n_all - n_used

# 3) Report why rows were dropped
reason_counts <- tmp %>%
  filter(drop_reason != "kept") %>%
  count(drop_reason, sort = TRUE)

cat("\n--- Row usage summary ---\n")
cat(sprintf("Total rows: %d\nUsed in histogram: %d\nDropped: %d\n\n", n_all, n_used, n_drop))
if (n_drop > 0) {
  cat("Reasons for drop:\n")
  print(reason_counts)
} else {
  cat("No rows were dropped.\n")
}

# Save the dropped rows for inspection
if (n_drop > 0) {
  dir.create("output", showWarnings = FALSE)
  dropped_detail <- tmp %>%
    filter(drop_reason != "kept") %>%
    select(.row_id, yield_original = yield_raw_chr, drop_reason)
  write.csv(dropped_detail, "output/dropped_yield_rows.csv", row.names = FALSE)
  cat("Wrote details of dropped rows to: ", normalizePath("output/dropped_yield_rows.csv"), "\n")
}

# 4) Ensure output dir
dir.create("output", showWarnings = FALSE)

# 5) Base R hist() with x-axis capped at 0..30,000
png("output/hist_yield_base_2021.png", width = 1200, height = 900, res = 150)
par(mar = c(4.5, 4.5, 3.5, 1.5))
h <- hist(
  df$yield,
  breaks = "FD",
  main = "Histogram of Yield 2021 (kg/ha)",
  xlab  = "Yield (kg/ha)",
  ylab  = "Frequency",
  col   = "grey85",
  border= "grey30",
  xlim  = c(0, 30000)   # <- requested axis range
)
abline(v = mean(df$yield),   lwd = 2, lty = 1, col = "black")
abline(v = median(df$yield), lwd = 2, lty = 2, col = "black")
legend("topright", legend = c("Mean", "Median"), lwd = 2, lty = c(1, 2), bty = "n")
mtext(sprintf("N used: %d of %d (dropped: %d)", n_used, n_all, n_drop),
      side = 3, line = 0.2, cex = 0.9)
dev.off()
cat("Saved base R histogram to: ", normalizePath("output/hist_yield_base_2021.png"), "\n")

# 6) ggplot version (keeps all data but only shows 0..30,000)
p <- ggplot(df, aes(x = yield)) +
  geom_histogram(bins = 30, fill = "grey80", colour = "grey30") +
  geom_vline(aes(xintercept = mean(yield)), linetype = 1) +
  geom_vline(aes(xintercept = median(yield)), linetype = 2) +
  coord_cartesian(xlim = c(0, 30000)) +   # <- show 0..30,000 without dropping data
  labs(
    title = "Histogram of Yield 2021 (kg/ha)",
    x = "Yield (kg/ha)", y = "Count",
    subtitle = sprintf("N used: %d of %d (dropped: %d)", n_used, n_all, n_drop)
  ) +
  theme_minimal(base_size = 13)

ggsave("output/hist_yield_ggplot_2021.png", p, width = 10, height = 7, dpi = 150)
cat("Saved ggplot histogram to: ", normalizePath("output/hist_yield_ggplot_2021.png"), "\n")

#===== end =====
# ==== generate histogram yield 2020 =====
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(ggplot2)
})

# 1) Load
df_raw <- read_csv("UQ Cocoa R/2_Data/farms_with_aez_shade_densities.csv", show_col_types = FALSE) %>%
  mutate(.row_id = row_number())  # keep original row numbers for auditing

if (!"yield_kg_ha_2020" %in% names(df_raw)) {
  stop("Column 'yield_kg_ha_2020' not found.\nAvailable columns:\n",
       paste(names(df_raw), collapse = ", "))
}

# 2) Parse and diagnose drop reasons
#    We: (a) coerce to character, (b) trim, (c) coerce to numeric, (d) label reasons
tmp <- df_raw %>%
  mutate(
    yield_raw_chr = trimws(as.character(yield_kg_ha_2021)),
    yield_num     = suppressWarnings(as.numeric(yield_raw_chr)),
    drop_reason = case_when(
      # Explicit missing/blank
      is.na(yield_kg_ha_2021) | yield_raw_chr %in% c("", "NA", "N/A", "NaN", "na", "n/a") ~ "missing",
      # Non-numeric text (coercion produced NA but the original wasn't missing)
      !is.na(yield_raw_chr) & is.na(yield_num) ~ "non-numeric text",
      is.infinite(yield_num) ~ "infinite",
      is.nan(yield_num) ~ "NaN",
      # Negative yields (keep or drop?—common to drop; we mark as 'negative value')
      yield_num < 0 ~ "negative value",
      TRUE ~ "kept"
    )
  )

# What we keep for analysis (only valid finite numbers)
df <- tmp %>% filter(drop_reason == "kept") %>% transmute(yield = yield_num)

n_all   <- nrow(tmp)
n_used  <- nrow(df)
n_drop  <- n_all - n_used

# 3) Report why rows were dropped
reason_counts <- tmp %>%
  filter(drop_reason != "kept") %>%
  count(drop_reason, sort = TRUE)

cat("\n--- Row usage summary ---\n")
cat(sprintf("Total rows: %d\nUsed in histogram: %d\nDropped: %d\n\n", n_all, n_used, n_drop))
if (n_drop > 0) {
  cat("Reasons for drop:\n")
  print(reason_counts)
} else {
  cat("No rows were dropped.\n")
}

# Save the dropped rows for inspection
if (n_drop > 0) {
  dir.create("output", showWarnings = FALSE)
  dropped_detail <- tmp %>%
    filter(drop_reason != "kept") %>%
    select(.row_id, yield_original = yield_raw_chr, drop_reason)
  write.csv(dropped_detail, "output/dropped_yield_rows.csv", row.names = FALSE)
  cat("Wrote details of dropped rows to: ", normalizePath("output/dropped_yield_rows.csv"), "\n")
}

# 4) Ensure output dir
dir.create("output", showWarnings = FALSE)

# 5) Base R hist() with x-axis capped at 0..30,000
png("output/hist_yield_base_2020.png", width = 1200, height = 900, res = 150)
par(mar = c(4.5, 4.5, 3.5, 1.5))
h <- hist(
  df$yield,
  breaks = "FD",
  main = "Histogram of Yield 2020 (kg/ha)",
  xlab  = "Yield (kg/ha)",
  ylab  = "Frequency",
  col   = "grey85",
  border= "grey30",
  xlim  = c(0, 30000)   # <- requested axis range
)
abline(v = mean(df$yield),   lwd = 2, lty = 1, col = "black")
abline(v = median(df$yield), lwd = 2, lty = 2, col = "black")
legend("topright", legend = c("Mean", "Median"), lwd = 2, lty = c(1, 2), bty = "n")
mtext(sprintf("N used: %d of %d (dropped: %d)", n_used, n_all, n_drop),
      side = 3, line = 0.2, cex = 0.9)
dev.off()
cat("Saved base R histogram to: ", normalizePath("output/hist_yield_base_2020.png"), "\n")

# 6) ggplot version (keeps all data but only shows 0..30,000)
p <- ggplot(df, aes(x = yield)) +
  geom_histogram(bins = 30, fill = "grey80", colour = "grey30") +
  geom_vline(aes(xintercept = mean(yield)), linetype = 1) +
  geom_vline(aes(xintercept = median(yield)), linetype = 2) +
  coord_cartesian(xlim = c(0, 30000)) +   # <- show 0..30,000 without dropping data
  labs(
    title = "Histogram of Yield 2020 (kg/ha)",
    x = "Yield (kg/ha)", y = "Count",
    subtitle = sprintf("N used: %d of %d (dropped: %d)", n_used, n_all, n_drop)
  ) +
  theme_minimal(base_size = 13)

ggsave("output/hist_yield_ggplot_2020.png", p, width = 10, height = 7, dpi = 150)
cat("Saved ggplot histogram to: ", normalizePath("output/hist_yield_ggplot_2020.png"), "\n")

# ===== end ====
# ==== adjust to the attainable yield in low and high inputs 
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

# ---------- SETTINGS ----------
LOW_MAX   <- 1109   # low-input cutoff (inclusive)
HIGH_CAP  <- 2647   # high-input & global cap (inclusive)
BINWIDTH  <- 100    # change for finer/coarser bins
# -----------------------------

# 1) Load (keep original row id + raw text for audit)
df_raw <- read_csv("UQ Cocoa R/2_Data/farms_with_aez_shade_densities.csv", show_col_types = FALSE) %>%
  mutate(.row_id = dplyr::row_number())
stopifnot("yield_kg_ha_delineated" %in% names(df_raw))

# 2) Parse numeric, keep finite & non-negative
df_num <- df_raw %>%
  transmute(
    .row_id,
    yield_original = trimws(as.character(yield_kg_ha_delineated)),
    yield = suppressWarnings(as.numeric(yield_kg_ha_delineated))
  ) %>%
  filter(is.finite(yield), yield >= 0)

# 3) ---- CSV of ALL values > 2647 (outside thresholds) ----
dir.create("output", showWarnings = FALSE)
df_over_1109 <- df_num %>% filter(yield > HIGH_CAP)
write.csv(df_over_1109,
          "output/yield_over_1109_rows.csv",
          row.names = FALSE)

# 4) Data for plotting (capped at ≤ 2647)
df_cap <- df_num %>% filter(yield <= HIGH_CAP)

# Groups (OVERLAPPING by design: low ≤ 1109; high ≤ 2647)
df_low  <- df_cap %>% filter(yield <= LOW_MAX)
df_high <- df_cap

# 5) Shared breaks covering 0..5,000 + key edges
brks <- sort(unique(c(seq(0, 5000, by = BINWIDTH), LOW_MAX, HIGH_CAP)))

# 6) Bin with common breaks (left-closed; include lowest)
h_low  <- if (nrow(df_low)  > 0) hist(df_low$yield,  breaks = brks, plot = FALSE,
                                      right = FALSE, include.lowest = TRUE) else NULL
h_high <- if (nrow(df_high) > 0) hist(df_high$yield, breaks = brks, plot = FALSE,
                                      right = FALSE, include.lowest = TRUE) else NULL

# y-scale from data
ymax <- max(c(if (!is.null(h_low)) h_low$counts else 0,
              if (!is.null(h_high)) h_high$counts else 0), na.rm = TRUE)
ylim <- c(0, ceiling(ymax * 1.08))

# 7) Draw combined overlapped histogram
png("output/hist_yield_combined_overlap.png", width = 1300, height = 900, res = 150)
par(mar = c(4.5, 5, 3.6, 1.5))

plot(0, 0, type = "n",
     xlab = "Yield (kg/ha)",
     ylab = "Frequency",
     main = sprintf("Attainable Yield (Overlap): Low-input (≤%d) & High-input (≤%d) kg/ha", LOW_MAX, HIGH_CAP),
     xlim = c(0, 5000),
     ylim = ylim)

draw_hist_rects <- function(h, fill, border) {
  if (is.null(h)) return(invisible(NULL))
  for (i in seq_along(h$counts)) {
    if (h$counts[i] > 0) {
      rect(h$breaks[i], 0, h$breaks[i + 1], h$counts[i],
           col = fill, border = border)
    }
  }
}

# HIGH first (semi-opaque), then LOW with **transparent fill** (border only)
draw_hist_rects(h_high, fill = adjustcolor("lightpink", alpha.f = 0.6), border = "grey40")
draw_hist_rects(h_low,  fill = NA,                                 border = "#404040")  # transparent

# Guides
abline(v = LOW_MAX,  lty = 3, lwd = 2)
abline(v = HIGH_CAP, lty = 3, lwd = 2)

legend("topright",
       fill   = c(NA, adjustcolor("lightgreen", alpha.f = 0.6)),
       border = c("#404040", "grey40"),
       legend = c(sprintf("Low-input (≤ %d) — transparent fill", LOW_MAX),
                  sprintf("High-input (≤ %d)", HIGH_CAP)),
       bty = "n", cex = 0.95)

dev.off()

cat("Saved plot: ", normalizePath("output/hist_yield_combined_overlap.png"), "\n")
cat("Saved rows > 1109: ", normalizePath("output/yield_over_1109_rows.csv"), "\n")

# ======== end ========
# adjust attainable yield high vs low ====
# ---- Packages ----
library(tidyverse)

# ---- Thresholds (edit here if needed) ----
thr_high <- 2647
thr_low  <- 1109

# ---- Read & prep ----
df0 <- read.csv("UQ Cocoa R/2_Data/yield_high_vs_low.csv", check.names = FALSE)

# In case your 2022 column is named "yield_kg_ha_2022" instead of "..._delineated"
col_2022 <- if ("yield_kg_ha_delineated" %in% names(df0)) {
  "yield_kg_ha_delineated"
} else if ("yield_kg_ha_2022" %in% names(df0)) {
  "yield_kg_ha_2022"
} else {
  stop("Couldn't find 2022 yield column (expect 'yield_kg_ha_delineated' or 'yield_kg_ha_2022').")
}

need <- c("yield_kg_ha_2020", "yield_kg_ha_2021", col_2022)
missing <- setdiff(need, names(df0))
if (length(missing)) stop("Missing column(s): ", paste(missing, collapse = ", "))

numify <- function(x) {
  if (is.numeric(x)) return(x)
  x <- gsub("[^0-9eE+\\-\\.]", "", as.character(x))  # strip units/commas/% if any
  suppressWarnings(as.numeric(x))
}

df_long <- df0 %>%
  transmute(`2020` = numify(.data[["yield_kg_ha_2020"]]),
            `2021` = numify(.data[["yield_kg_ha_2021"]]),
            `2022` = numify(.data[[col_2022]])) %>%
  pivot_longer(everything(), names_to = "year", values_to = "yield_kg_ha") %>%
  filter(is.finite(yield_kg_ha), yield_kg_ha > 0) %>%                # keep only positive, finite
  mutate(category = case_when(                                     # disjoint buckets
    yield_kg_ha <= thr_low            ~ "Low-input attainable",
    yield_kg_ha <= thr_high           ~ "High-input attainable",
    TRUE                              ~ "Excluded"
  )) %>%
  filter(category != "Excluded") %>%
  mutate(
    category = factor(category, levels = c("High-input attainable", "Low-input attainable")),
    year = factor(year, levels = c("2020", "2021", "2022"))
  )

# ---- Plot ----
p <- ggplot(df_long, aes(x = category, y = yield_kg_ha, fill = year,
                         group = interaction(category, year))) +
  geom_boxplot(position = position_dodge(width = 0.75),
               width = 0.6, outlier.shape = 16, outlier.alpha = 0.4) +
  scale_x_discrete(labels = c("High-input\nattainable", "Low-input\nattainable")) +
  labs(x = NULL, y = "Cocoa yield (kg/ha)", fill = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top",
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())

print(p)

# ---- (Optional) Save & quick counts ----
ggsave("box_yield_high_vs_low.png", p, width = 8, height = 4, dpi = 300)

# How many observations used per box (handy for reporting)
df_long %>% count(category, year, name = "n")

# ==== end =====
# adjust turqoise blue low input, orange high input ===============
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

# ---------- SETTINGS ----------
LOW_MAX   <- 1109   # low-input cutoff (inclusive)
HIGH_CAP  <- 2647   # high-input upper cap (inclusive)
BINWIDTH  <- 100

# Fills (with light transparency so overlaps are visible)
low_fill  <- adjustcolor("#0077BE", alpha.f = 0.45)  # sea blue
high_fill <- adjustcolor("#FF4500", alpha.f = 0.60)  # red-orange
border_col <- "black"
border_lwd <- 0.8
# -----------------------------

# 1) Load
df_raw <- read_csv("UQ Cocoa R/2_Data/farms_with_aez_shade_densities.csv",
                   show_col_types = FALSE) %>%
  mutate(.row_id = dplyr::row_number())
stopifnot("yield_kg_ha_delineated" %in% names(df_raw))

# 2) Parse numeric, keep finite & non-negative
df_num <- df_raw %>%
  transmute(
    .row_id,
    yield_original = trimws(as.character(yield_kg_ha_delineated)),
    yield = suppressWarnings(as.numeric(yield_kg_ha_delineated))
  ) %>%
  filter(is.finite(yield), yield >= 0)

# 3) CSV of ALL values > 2647 (outside thresholds)
dir.create("output", showWarnings = FALSE)
df_over_2647 <- df_num %>% filter(yield > HIGH_CAP)
write.csv(df_over_2647, "output/yield_over_2647_rows.csv", row.names = FALSE)

# 4) Data for plotting (≤ 2647)
df_cap <- df_num %>% filter(yield <= HIGH_CAP)

# Disjoint buckets
df_low  <- df_cap %>% filter(yield <= LOW_MAX)                     # 0–1109
df_high <- df_cap %>% filter(yield > LOW_MAX, yield <= HIGH_CAP)   # 1109–2647

# 5) Shared breaks 0..5000 + edges
brks <- sort(unique(c(seq(0, 5000, by = BINWIDTH), LOW_MAX, HIGH_CAP)))

# 6) Bin (left-closed; include lowest)
h_low  <- if (nrow(df_low)  > 0) hist(df_low$yield,  breaks = brks, plot = FALSE,
                                      right = FALSE, include.lowest = TRUE) else NULL
h_high <- if (nrow(df_high) > 0) hist(df_high$yield, breaks = brks, plot = FALSE,
                                      right = FALSE, include.lowest = TRUE) else NULL

# Y-scale
ymax <- max(c(if (!is.null(h_low)) h_low$counts else 0,
              if (!is.null(h_high)) h_high$counts else 0), na.rm = TRUE)
ylim <- c(0, ceiling(ymax * 1.08))

# ---- Helper to draw one histogram with borders ----
draw_hist_rects <- function(h, fill, border = border_col, lwd = border_lwd) {
  if (is.null(h)) return(invisible(NULL))
  for (i in seq_along(h$counts)) {
    if (h$counts[i] > 0) {
      rect(h$breaks[i], 0, h$breaks[i + 1], h$counts[i],
           col = fill, border = border, lwd = lwd)
    }
  }
}

# 7) Draw combined overlapped histogram
png("output/hist_yield_combined_overlap.png", width = 1300, height = 900, res = 150)
par(mar = c(4.5, 5, 3.6, 1.5))

plot(0, 0, type = "n",
     xlab = "Yield (kg/ha)",
     ylab = "Frequency",
     main = sprintf("Attainable Yield: Low input vs High input", LOW_MAX, LOW_MAX, HIGH_CAP),
     xlim = c(0, 5000),
     ylim = ylim)

# High behind, Low on top
draw_hist_rects(h_high, fill = high_fill)
draw_hist_rects(h_low,  fill = low_fill)

# Guides
abline(v = LOW_MAX,  lty = 3, lwd = 2)
abline(v = HIGH_CAP, lty = 3, lwd = 2)

legend("topright",
       fill   = c(low_fill, high_fill),
       border = border_col,
       legend = c(sprintf("Low-input (0–%d)", LOW_MAX),
                  sprintf("High-input (%d–%d)", LOW_MAX, HIGH_CAP)),
       bty = "n", cex = 0.95)

dev.off()

cat("Saved plot: ", normalizePath("output/hist_yield_combined_overlap.png"), "\n")
cat("Saved rows > 2647: ", normalizePath("output/yield_over_2647_rows.csv"), "\n")

# ============= end ==================
# adding random factor and log(yield)
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

# ---------- SETTINGS ----------
LOW_MAX   <- 1109   # low-input cutoff (inclusive)
HIGH_CAP  <- 2647   # high-input upper cap (inclusive)
BINWIDTH  <- 100

# Fills (with light transparency so overlaps are visible)
low_fill   <- adjustcolor("#0077BE", alpha.f = 0.45)  # sea blue
high_fill  <- adjustcolor("#FF4500", alpha.f = 0.60)  # red-orange
border_col <- "black"
border_lwd <- 0.8

# Parameters for skewed log plot
set.seed(123)                 # reproducible random skew
SKEW_SHAPE <- 2               # gamma shape (controls skew)
SKEW_RATE  <- 3               # gamma rate  (controls spread)
BINWIDTH_LOG <- 0.15          # bin width on the log-skewed scale
# -----------------------------

# 1) Load
df_raw <- read_csv("UQ Cocoa R/2_Data/farms_with_aez_shade_densities.csv",
                   show_col_types = FALSE) %>%
  mutate(.row_id = dplyr::row_number())
stopifnot("yield_kg_ha_delineated" %in% names(df_raw))

# 2) Parse numeric, keep finite & non-negative
df_num <- df_raw %>%
  transmute(
    .row_id,
    yield_original = trimws(as.character(yield_kg_ha_delineated)),
    yield = suppressWarnings(as.numeric(yield_kg_ha_delineated))
  ) %>%
  filter(is.finite(yield), yield >= 0)

# ---- (NEW) Build log-transformed & skew-jittered series ---------------------
# log1p keeps zeros valid; center so distribution crosses 0;
# subtract a positive gamma deviate to introduce a left tail (negative values)
eps <- rgamma(n = nrow(df_num), shape = SKEW_SHAPE, rate = SKEW_RATE)
df_num <- df_num %>%
  mutate(
    log1p_yield     = log1p(yield),
    log1p_centered  = log1p_yield - median(log1p_yield, na.rm = TRUE),
    log1p_skew      = log1p_centered - eps
  )

# 3) CSV of ALL values > 2647 (outside thresholds)
dir.create("output", showWarnings = FALSE)
df_over_2647 <- df_num %>% filter(yield > HIGH_CAP)
write.csv(df_over_2647, "output/yield_over_2647_rows.csv", row.names = FALSE)

# 4) Data for plotting (≤ 2647)
df_cap <- df_num %>% filter(yield <= HIGH_CAP)

# Disjoint buckets
df_low  <- df_cap %>% filter(yield <= LOW_MAX)                     # 0–1109
df_high <- df_cap %>% filter(yield > LOW_MAX, yield <= HIGH_CAP)   # 1109–2647

# 5) Shared breaks 0..5000 + edges
brks <- sort(unique(c(seq(0, 5000, by = BINWIDTH), LOW_MAX, HIGH_CAP)))

# 6) Bin (left-closed; include lowest)
h_low  <- if (nrow(df_low)  > 0) hist(df_low$yield,  breaks = brks, plot = FALSE,
                                      right = FALSE, include.lowest = TRUE) else NULL
h_high <- if (nrow(df_high) > 0) hist(df_high$yield, breaks = brks, plot = FALSE,
                                      right = FALSE, include.lowest = TRUE) else NULL

# Y-scale
ymax <- max(c(if (!is.null(h_low)) h_low$counts else 0,
              if (!is.null(h_high)) h_high$counts else 0), na.rm = TRUE)
ylim <- c(0, ceiling(ymax * 1.08))

# ---- Helper to draw one histogram with borders ----
draw_hist_rects <- function(h, fill, border = border_col, lwd = border_lwd) {
  if (is.null(h)) return(invisible(NULL))
  for (i in seq_along(h$counts)) {
    if (h$counts[i] > 0) {
      rect(h$breaks[i], 0, h$breaks[i + 1], h$counts[i],
           col = fill, border = border, lwd = lwd)
    }
  }
}

# 7) Draw combined overlapped histogram (ORIGINAL)
png("output/hist_yield_combined_overlap.png", width = 1300, height = 900, res = 150)
par(mar = c(4.5, 5, 3.6, 1.5))

plot(0, 0, type = "n",
     xlab = "Yield (kg/ha)",
     ylab = "Frequency",
     main = "Attainable Yield: Low input vs High input",
     xlim = c(0, 5000),
     ylim = ylim)

# High behind, Low on top
draw_hist_rects(h_high, fill = high_fill)
draw_hist_rects(h_low,  fill = low_fill)

# Guides
abline(v = LOW_MAX,  lty = 3, lwd = 2)
abline(v = HIGH_CAP, lty = 3, lwd = 2)

legend("topright",
       fill   = c(low_fill, high_fill),
       border = border_col,
       legend = c(sprintf("Low-input (0–%d)", LOW_MAX),
                  sprintf("High-input (%d–%d)", LOW_MAX, HIGH_CAP)),
       bty = "n", cex = 0.95)

dev.off()

# 8) (NEW) Skewed log-yield histogram with density LINE overlaid -------------
# Choose breaks so the x-axis extends into negatives (“starting from minus”)
rng <- range(df_num$log1p_skew, na.rm = TRUE)
lo  <- floor(rng[1] / BINWIDTH_LOG) * BINWIDTH_LOG
hi  <- ceiling(rng[2] / BINWIDTH_LOG) * BINWIDTH_LOG
brks_log <- seq(lo, hi, by = BINWIDTH_LOG)

png("output/hist_log1p_yield_skewed.png", width = 1300, height = 900, res = 150)
par(mar = c(4.5, 5, 3.6, 1.5))

# freq=FALSE so the density curve aligns with the histogram
hist(df_num$log1p_skew,
     breaks = brks_log, freq = FALSE,
     col = adjustcolor("grey25", alpha.f = 0.50),
     border = border_col, lwd = border_lwd,
     main = "Skewed log-yield histogram (log1p, centered, gamma-skew jitter)",
     xlab = "log1p(yield) centered + skew (can be < 0)",
     ylab = "Density")

# (NEW) The line showing the skewed histogram shape
dens <- density(df_num$log1p_skew, na.rm = TRUE)
lines(dens, lwd = 2)          # <-- requested line overlay
abline(v = 0, lty = 3, lwd = 1.5)  # reference at 0

mtext(sprintf("Skew params: Gamma(shape=%g, rate=%g); bin=%.2f",
              SKEW_SHAPE, SKEW_RATE, BINWIDTH_LOG), side = 3, line = 0.5, cex = 0.9)

dev.off()

cat("Saved plot: ", normalizePath("output/hist_yield_combined_overlap.png"), "\n")
cat("Saved rows > 2647: ", normalizePath("output/yield_over_2647_rows.csv"), "\n")
cat("Saved plot: ", normalizePath("output/hist_log1p_yield_skewed.png"), "\n")

# ========= comparison ======
# --- Differentiate LOW (0–1109) vs HIGH (1109–2647) on the skewed log plot ---

# Reuse your existing LOW_MAX, HIGH_CAP, SKEW_SHAPE, SKEW_RATE, BINWIDTH_LOG,
# low_fill, high_fill, border_col, border_lwd

# Build (or reuse) the skewed series
# --- Skewed log-yield by band, EXCLUDING zero yields from the plot ---

# Reuse your constants: LOW_MAX, HIGH_CAP, SKEW_SHAPE, SKEW_RATE, BINWIDTH_LOG,
# low_fill, high_fill, border_col, border_lwd

# 1) Keep only positive yields for plotting (drop 0s), and cap at HIGH_CAP
df_graph <- df_num %>%
  dplyr::filter(is.finite(yield), yield > 0, yield <= HIGH_CAP)

# 2) Build the skewed (centered log1p minus gamma) values
log1p_y <- log1p(df_graph$yield)
center  <- median(log1p_y, na.rm = TRUE)

set.seed(123)  # reproducible skew
eps <- rgamma(n = length(log1p_y), shape = SKEW_SHAPE, rate = SKEW_RATE)
log1p_skew <- (log1p_y - center) - eps

# 3) Label bands (now strictly >0 on the low end)
band <- dplyr::case_when(
  df_graph$yield > 0 & df_graph$yield <= LOW_MAX          ~ "Low input (>0–1109)",
  df_graph$yield > LOW_MAX & df_graph$yield <= HIGH_CAP    ~ "High input (1109–2647)",
  TRUE ~ NA_character_
)

# 4) Shared breaks on the skewed axis
rng <- range(log1p_skew, na.rm = TRUE)
lo  <- floor(rng[1] / BINWIDTH_LOG) * BINWIDTH_LOG
hi  <- ceiling(rng[2] / BINWIDTH_LOG) * BINWIDTH_LOG
brks_log <- seq(lo, hi, by = BINWIDTH_LOG)

# Split values
low_vals  <- log1p_skew[band == "Low input (>0–1109)"]
high_vals <- log1p_skew[band == "High input (1109–2647)"]

# Avoid empty vectors gracefully
h_or_null <- function(x) if (length(x)) hist(x, breaks = brks_log, plot = FALSE, freq = FALSE, right = FALSE) else NULL
d_or_null <- function(x) if (length(x) > 1) density(x) else NULL

h_low  <- h_or_null(low_vals)
h_high <- h_or_null(high_vals)
d_low  <- d_or_null(low_vals)
d_high <- d_or_null(high_vals)

ymax <- max(
  if (!is.null(h_low))  h_low$density  else 0,
  if (!is.null(h_high)) h_high$density else 0,
  if (!is.null(d_low))  d_low$y        else 0,
  if (!is.null(d_high)) d_high$y       else 0,
  na.rm = TRUE
)
ylim <- c(0, 1.08 * ymax)

# Helper to draw rectangles using density heights
draw_hist_rects_density <- function(h, fill, border = border_col, lwd = border_lwd) {
  if (is.null(h)) return(invisible(NULL))
  for (i in seq_along(h$density)) {
    if (h$density[i] > 0) {
      rect(h$breaks[i], 0, h$breaks[i + 1], h$density[i],
           col = fill, border = border, lwd = lwd)
    }
  }
}

dir.create("output", showWarnings = FALSE)
png("output/hist_log1p_yield_skewed_by_band_nozeros.png", width = 1300, height = 900, res = 150)
par(mar = c(4.5, 5, 3.6, 1.5))

plot(0, 0, type = "n",
     xlab = "log1p(yield) centered + skew (zeros excluded)",
     ylab = "Density",
     main = "Skewed log-yield by input band (no zeros; ≤2647 only)",
     xlim = c(lo, hi), ylim = ylim)

# Draw HIGH behind, LOW on top
draw_hist_rects_density(h_high, fill = high_fill)
draw_hist_rects_density(h_low,  fill = low_fill)

# Optional density lines
if (!is.null(d_high)) lines(d_high, lwd = 2)          # high: solid
if (!is.null(d_low))  lines(d_low,  lwd = 2, lty = 2) # low: dashed

abline(v = 0, lty = 3, lwd = 1.5)

legend("topright",
       fill   = c(low_fill, high_fill),
       border = border_col,
       lty    = c(2, 1),
       legend = c("Low input (>0–1109)", "High input (1109–2647)"),
       bty = "n", cex = 0.95)

dev.off()

cat("Saved plot: ", normalizePath("output/hist_log1p_yield_skewed_by_band_nozeros.png"), "\n")

# ======

library(tidyverse)

# ---- Thresholds ----
thr_low  <- 1109
thr_high <- 2647

low_col  <- "#0077BE"   # low (blue)
high_col <- "#FF4500"   # high (orange)

# ---- Read & prep ----
df0 <- read.csv("UQ Cocoa R/2_Data/shade_yield_densities_high_input.csv", check.names = FALSE)

col_2022 <- if ("yield_kg_ha_delineated" %in% names(df0)) {
  "yield_kg_ha_delineated"
} else if ("yield_kg_ha_2022" %in% names(df0)) {
  "yield_kg_ha_2022"
} else {
  stop("Couldn't find 2022 yield column (expect 'yield_kg_ha_delineated' or 'yield_kg_ha_2022').")
}

need <- c("yield_kg_ha_2020", "yield_kg_ha_2021", col_2022)
missing <- setdiff(need, names(df0))
if (length(missing)) stop("Missing column(s): ", paste(missing, collapse = ", "))

numify <- function(x) {
  if (is.numeric(x)) return(x)
  x <- gsub("[^0-9eE+\\-\\.]", "", as.character(x))
  suppressWarnings(as.numeric(x))
}

# Long format, drop zeros, keep only ≤ thr_high; tag low/high
df_long <- df0 %>%
  transmute(`2020` = numify(.data[["yield_kg_ha_2020"]]),
            `2021` = numify(.data[["yield_kg_ha_2021"]]),
            `2022` = numify(.data[[col_2022]])) %>%
  pivot_longer(everything(), names_to = "year", values_to = "yield_kg_ha") %>%
  filter(is.finite(yield_kg_ha), yield_kg_ha > 0) %>% 
  mutate(category = case_when(
    yield_kg_ha <= thr_low  ~ "Low (0–1109]",
    yield_kg_ha <= thr_high ~ "High (1109–2647]",
    TRUE                    ~ "Excluded"
  )) %>%
  filter(category != "Excluded") %>%
  mutate(
    year = factor(year, levels = c("2020","2021","2022")),
    category = factor(category, levels = c("Low (0–1109]","High (1109–2647]"))
  )

dir.create("output", showWarnings = FALSE)

# ---- Summarise: mean + SE + n ----
df_summary <- df_long %>%
  group_by(year, category) %>%
  summarise(
    mean_yield = mean(yield_kg_ha, na.rm = TRUE),
    se = sd(yield_kg_ha, na.rm = TRUE) / sqrt(n()),
    n = n(),
    .groups = "drop"
  )

# ---- Plot A: per year ----
p_year <- ggplot(df_summary, aes(x = year, y = mean_yield, fill = category)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(ymin = mean_yield - se, ymax = mean_yield + se),
                position = position_dodge(width = 0.7), width = 0.2) +
  geom_text(aes(label = paste0("n=", n)),
            position = position_dodge(width = 0.7),
            vjust = -0.8, size = 3.5) +
  scale_fill_manual(values = c("Low (0–1109]" = low_col,
                               "High (1109–2647]" = high_col)) +
  labs(x = NULL, y = "Average cocoa yield (kg/ha)", fill = NULL,
       title = "Average yield by year and input band (zeros excluded; ≤2647)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top",
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())

ggsave("output/avg_yield_low_vs_high_by_year.png",
       p_year, width = 8.5, height = 4.8, dpi = 300)

# ---- Plot B: all years pooled ----
df_summary_all <- df_long %>%
  group_by(category) %>%
  summarise(
    mean_yield = mean(yield_kg_ha, na.rm = TRUE),
    se = sd(yield_kg_ha, na.rm = TRUE) / sqrt(n()),
    n = n(),
    .groups = "drop"
  )

p_all <- ggplot(df_summary_all, aes(x = category, y = mean_yield, fill = category)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = mean_yield - se, ymax = mean_yield + se), width = 0.2) +
  geom_text(aes(label = paste0("n=", n)), vjust = -0.8, size = 3.5) +
  scale_fill_manual(values = c("Low (0–1109]" = low_col,
                               "High (1109–2647]" = high_col), guide = "none") +
  scale_x_discrete(labels = c("Low (0–1109]" = "Low\n(0–1109]",
                              "High (1109–2647]" = "High\n(1109–2647]")) +
  labs(x = NULL, y = "Average cocoa yield (kg/ha)",
       title = "Average yield by input band (zeros excluded; ≤2647; all years)") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())

ggsave("output/avg_yield_low_vs_high_overall.png",
       p_all, width = 5.5, height = 4.8, dpi = 300)
# =====

library(tidyverse)

# ---- Thresholds ----
thr_low  <- 1109
thr_high <- 2647

low_col  <- "#0077BE"
high_col <- "#FF4500"

# ---- Read & prep ----
df0 <- read.csv("UQ Cocoa R/2_Data/shade_yield_densities_high_input.csv", check.names = FALSE)

col_2022 <- if ("yield_kg_ha_delineated" %in% names(df0)) {
  "yield_kg_ha_delineated"
} else if ("yield_kg_ha_2022" %in% names(df0)) {
  "yield_kg_ha_2022"
} else {
  stop("Couldn't find 2022 yield column (expect 'yield_kg_ha_delineated' or 'yield_kg_ha_2022').")
}

numify <- function(x) {
  if (is.numeric(x)) return(x)
  x <- gsub("[^0-9eE+\\-\\.]", "", as.character(x))
  suppressWarnings(as.numeric(x))
}

# ---- Long format ----
df_long <- df0 %>%
  transmute(`2020` = numify(.data[["yield_kg_ha_2020"]]),
            `2021` = numify(.data[["yield_kg_ha_2021"]]),
            `2022` = numify(.data[[col_2022]])) %>%
  pivot_longer(everything(), names_to = "year", values_to = "yield_kg_ha") %>%
  filter(is.finite(yield_kg_ha), yield_kg_ha > 0, yield_kg_ha <= thr_high) %>%
  mutate(category = case_when(
    yield_kg_ha <= thr_low  ~ "Low input (0–1109]",
    yield_kg_ha >  thr_low  ~ "High input (1109–2647]"
  ))

dir.create("output", showWarnings = FALSE)

# ---- Histogram + smooth lines (no density fill) ----
p_hist <- ggplot(df_long, aes(x = yield_kg_ha, fill = category)) +
  geom_histogram(aes(y = after_stat(density)), 
                 position = "identity", bins = 30, alpha = 0.6, color = "white") +
  # smooth black lines only (no fill)
  geom_density(data = subset(df_long, category == "Low input (0–1109]"),
               aes(y = after_stat(density)), color = "black", size = 1, linetype = "dashed") +
  geom_density(data = subset(df_long, category == "High input (1109–2647]"),
               aes(y = after_stat(density)), color = "black", size = 1, linetype = "solid") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "black", size = 1) +
  scale_fill_manual(values = c("Low input (0–1109]" = low_col,
                               "High input (1109–2647]" = high_col)) +
  labs(x = "Cocoa yield (kg/ha)", y = "Density",
       title = "Yield distribution by input band (zeros excluded; ≤2647)",
       fill = NULL) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggsave("output/hist_yield_low_vs_high_lines.png",
       p_hist, width = 8, height = 5, dpi = 300)





# =====






# ======= end ========
suppressPackageStartupMessages({ library(readr); library(dplyr) })
dir.create("output", showWarnings = FALSE)

# Reuse df_num if it exists; otherwise load & clean quickly
if (!exists("df_num")) {
  df_raw <- read_csv("UQ Cocoa R/2_Data/farms_with_aez_shade_densities.csv",
                     show_col_types = FALSE)
  stopifnot("yield_kg_ha_delineated" %in% names(df_raw))
  df_num <- df_raw %>%
    transmute(yield = suppressWarnings(as.numeric(yield_kg_ha_delineated))) %>%
    filter(is.finite(yield), yield >= 0)
}

# Constants (reuse yours if already defined)
if (!exists("LOW_MAX"))  LOW_MAX  <- 1109
if (!exists("HIGH_CAP")) HIGH_CAP <- 2647

# =========================
# A) Percentages for the GRAPHED subset (zeros excluded; ≤2647)
# =========================
df_graphed <- df_num %>% filter(yield > 0, yield <= HIGH_CAP)

share_graphed <- df_graphed %>%
  transmute(band = case_when(
    yield <= LOW_MAX ~ "Low input (>0–1109]",
    yield > LOW_MAX & yield <= HIGH_CAP ~ "High input (1109–2647]"
  )) %>%
  count(band, name = "n") %>%
  mutate(total = sum(n), pct = 100 * n / total) %>%
  arrange(match(band, c("Low input (>0–1109]", "High input (1109–2647]")))

write.csv(share_graphed, "output/low_high_share_graphed.csv", row.names = FALSE)
cat("\n--- Shares for GRAPhed subset (zeros excluded; ≤2647) ---\n")
print(share_graphed, row.names = FALSE)

# =========================
# B) Overall breakdown (including zeros and >2647), for context
# =========================
share_overall <- df_num %>%
  transmute(band = case_when(
    yield == 0                         ~ "Zero",
    yield > 0 & yield <= LOW_MAX       ~ "Low input (>0–1109]",
    yield > LOW_MAX & yield <= HIGH_CAP~ "High input (1109–2647]",
    yield > HIGH_CAP                   ~ ">2647"
  )) %>%
  count(band, name = "n") %>%
  mutate(total = sum(n), pct = 100 * n / total) %>%
  arrange(match(band, c("Zero","Low input (>0–1109]","High input (1109–2647]"," >2647")))

write.csv(share_overall, "output/low_high_share_overall.csv", row.names = FALSE)
cat("\n--- Overall shares (including zeros and >2647) ---\n")
print(share_overall, row.names = FALSE)

# ====== recalculating box plots with period yields ======
# ---- Packages ----
library(tidyverse)

# ---- Thresholds ----
thr_low  <- 1109
thr_high <- 2647

# Colours (semi-transparent like your histograms)
low_col  <- grDevices::adjustcolor("#0077BE", alpha.f = 0.45)  # low
high_col <- grDevices::adjustcolor("#FF4500", alpha.f = 0.60)  # high

# ---- Read & prep ----
df0 <- read.csv("UQ Cocoa R/2_Data/shade_yield_densities_high_input.csv", check.names = FALSE)

col_2022 <- if ("yield_kg_ha_delineated" %in% names(df0)) {
  "yield_kg_ha_delineated"
} else if ("yield_kg_ha_2022" %in% names(df0)) {
  "yield_kg_ha_2022"
} else {
  stop("Couldn't find 2022 yield column (expect 'yield_kg_ha_delineated' or 'yield_kg_ha_2022').")
}

need <- c("yield_kg_ha_2020", "yield_kg_ha_2021", col_2022)
missing <- setdiff(need, names(df0))
if (length(missing)) stop("Missing column(s): ", paste(missing, collapse = ", "))

numify <- function(x) {
  if (is.numeric(x)) return(x)
  x <- gsub("[^0-9eE+\\-\\.]", "", as.character(x))
  suppressWarnings(as.numeric(x))
}

# Long frame, drop zeros, keep only <= thr_high; tag low/high bands
df_long <- df0 %>%
  transmute(`2020` = numify(.data[["yield_kg_ha_2020"]]),
            `2021` = numify(.data[["yield_kg_ha_2021"]]),
            `2022` = numify(.data[[col_2022]])) %>%
  pivot_longer(everything(), names_to = "year", values_to = "yield_kg_ha") %>%
  filter(is.finite(yield_kg_ha), yield_kg_ha > 0) %>%                   # EXCLUDE zeros
  mutate(category = case_when(
    yield_kg_ha <= thr_low              ~ "Low (0–1109]",
    yield_kg_ha <= thr_high             ~ "High (1109–2647]",
    TRUE                                ~ "Excluded"
  )) %>%
  filter(category != "Excluded") %>%
  mutate(
    year = factor(year, levels = c("2020","2021","2022")),
    category = factor(category, levels = c("Low (0–1109]","High (1109–2647]"))
  )

dir.create("output", showWarnings = FALSE)

# ---- Counts per year×band (for legend/reporting) ----
counts <- df_long %>% count(year, category, name = "n")
write.csv(counts, "output/box_counts_nozeros.csv", row.names = FALSE)
print(counts)

# ---- Plot A: two boxes per year (Low vs High) ----
dodge <- position_dodge(width = 0.7)

p_year <- ggplot(df_long, aes(x = year, y = yield_kg_ha, fill = category)) +
  geom_boxplot(position = dodge, width = 0.6, outlier.shape = 16, outlier.alpha = 0.35) +
  scale_fill_manual(values = c("Low (0–1109]" = low_col,
                               "High (1109–2647]" = high_col)) +
  labs(x = NULL, y = "Cocoa yield (kg/ha)", fill = NULL,
       title = "Yield by year and input band (zeros excluded; ≤ 2647)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top",
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())

# add n above boxes
y_max <- df_long %>% group_by(year) %>% summarise(y = max(yield_kg_ha, na.rm = TRUE)*1.03, .groups="drop")
lab_df <- counts %>% left_join(y_max, by = "year")
p_year <- p_year +
  geom_text(data = lab_df,
            aes(x = year, y = y, label = paste0("n=", n), group = category),
            position = dodge, size = 3)

ggsave("output/box_yield_low_vs_high_by_year_nozeros.png",
       p_year, width = 8.5, height = 4.8, dpi = 300)

# ---- Plot B: overall Low vs High (all years pooled) ----
p_all <- ggplot(df_long, aes(x = category, y = yield_kg_ha, fill = category)) +
  geom_boxplot(width = 0.6, outlier.shape = 16, outlier.alpha = 0.35) +
  scale_fill_manual(values = c("Low (0–1109]" = low_col,
                               "High (1109–2647]" = high_col), guide = "none") +
  scale_x_discrete(labels = c("Low (0–1109]" = "Low\n(0–1109]",
                              "High (1109–2647]" = "High\n(1109–2647]")) +
  labs(x = NULL, y = "Cocoa yield (kg/ha)",
       title = "Yield by input band (zeros excluded; ≤ 2647; all years)") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())

ggsave("output/box_yield_low_vs_high_overall_nozeros.png",
       p_all, width = 5.5, height = 4.8, dpi = 300)
# ===== end =====

# ==== adjust ===
library(tidyverse)

# ---- Thresholds ----
thr_low  <- 1109
thr_high <- 2647

low_col  <- "#0077BE"   # low (blue)
high_col <- "#FF4500"   # high (orange)

# ---- Read & prep ----
df0 <- read.csv("UQ Cocoa R/2_Data/shade_yield_densities_high_input.csv", check.names = FALSE)

col_2022 <- if ("yield_kg_ha_delineated" %in% names(df0)) {
  "yield_kg_ha_delineated"
} else if ("yield_kg_ha_2022" %in% names(df0)) {
  "yield_kg_ha_2022"
} else {
  stop("Couldn't find 2022 yield column (expect 'yield_kg_ha_delineated' or 'yield_kg_ha_2022').")
}

need <- c("yield_kg_ha_2020", "yield_kg_ha_2021", col_2022)
missing <- setdiff(need, names(df0))
if (length(missing)) stop("Missing column(s): ", paste(missing, collapse = ", "))

numify <- function(x) {
  if (is.numeric(x)) return(x)
  x <- gsub("[^0-9eE+\\-\\.]", "", as.character(x))
  suppressWarnings(as.numeric(x))
}

# Long frame, drop zeros, keep only <= thr_high; tag low/high bands
df_long <- df0 %>%
  transmute(`2020` = numify(.data[["yield_kg_ha_2020"]]),
            `2021` = numify(.data[["yield_kg_ha_2021"]]),
            `2022` = numify(.data[[col_2022]])) %>%
  pivot_longer(everything(), names_to = "year", values_to = "yield_kg_ha") %>%
  filter(is.finite(yield_kg_ha), yield_kg_ha > 0) %>% 
  mutate(category = case_when(
    yield_kg_ha <= thr_low  ~ "Low (0–1109]",
    yield_kg_ha <= thr_high ~ "High (1109–2647]",
    TRUE                    ~ "Excluded"
  )) %>%
  filter(category != "Excluded") %>%
  mutate(
    year = factor(year, levels = c("2020","2021","2022")),
    category = factor(category, levels = c("Low (0–1109]","High (1109–2647]"))
  )

dir.create("output", showWarnings = FALSE)

# ---- Compute averages ----
df_summary <- df_long %>%
  group_by(year, category) %>%
  summarise(
    mean_yield = mean(yield_kg_ha, na.rm = TRUE),
    se = sd(yield_kg_ha, na.rm = TRUE) / sqrt(n()),
    n = n(),
    .groups = "drop"
  )

# ---- Plot A: Average yield per year × band ----
p_year <- ggplot(df_summary, aes(x = year, y = mean_yield, fill = category)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(ymin = mean_yield - se, ymax = mean_yield + se),
                position = position_dodge(width = 0.7), width = 0.2) +
  geom_text(aes(label = paste0("n=", n)),
            position = position_dodge(width = 0.7),
            vjust = -0.8, size = 3.5) +
  scale_fill_manual(values = c("Low (0–1109]" = low_col,
                               "High (1109–2647]" = high_col)) +
  labs(x = NULL, y = "Average cocoa yield (kg/ha)", fill = NULL,
       title = "Average yield by year and input band (zeros excluded; ≤2647)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top",
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())

ggsave("output/avg_yield_low_vs_high_by_year.png",
       p_year, width = 8.5, height = 4.8, dpi = 300)

# ---- Plot B: Overall average (all years pooled) ----
df_summary_all <- df_long %>%
  group_by(category) %>%
  summarise(
    mean_yield = mean(yield_kg_ha, na.rm = TRUE),
    se = sd(yield_kg_ha, na.rm = TRUE) / sqrt(n()),
    n = n(),
    .groups = "drop"
  )

p_all <- ggplot(df_summary_all, aes(x = category, y = mean_yield, fill = category)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = mean_yield - se, ymax = mean_yield + se), width = 0.2) +
  geom_text(aes(label = paste0("n=", n)), vjust = -0.8, size = 3.5) +
  scale_fill_manual(values = c("Low (0–1109]" = low_col,
                               "High (1109–2647]" = high_col), guide = "none") +
  scale_x_discrete(labels = c("Low (0–1109]" = "Low\n(0–1109]",
                              "High (1109–2647]" = "High\n(1109–2647]")) +
  labs(x = NULL, y = "Average cocoa yield (kg/ha)",
       title = "Average yield by input band (zeros excluded; ≤2647; all years)") +
  theme_minimal(base_size = 10) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())

ggsave("output/avg_yield_low_vs_high_overall.png",
       p_all, width = 5.5, height = 4.8, dpi = 300)

# ======
library(tidyverse)

# ---- Thresholds ----
thr_low  <- 1109
thr_high <- 2647

low_col  <- "#0077BE"   # low (blue)
high_col <- "#FF4500"   # high (orange)

# ---- Read & prep ----
df0 <- read.csv("UQ Cocoa R/2_Data/shade_yield_densities_high_input.csv", check.names = FALSE)

col_2022 <- if ("yield_kg_ha_delineated" %in% names(df0)) {
  "yield_kg_ha_delineated"
} else if ("yield_kg_ha_2022" %in% names(df0)) {
  "yield_kg_ha_2022"
} else {
  stop("Couldn't find 2022 yield column (expect 'yield_kg_ha_delineated' or 'yield_kg_ha_2022').")
}

need <- c("yield_kg_ha_2020", "yield_kg_ha_2021", col_2022)
missing <- setdiff(need, names(df0))
if (length(missing)) stop("Missing column(s): ", paste(missing, collapse = ", "))

numify <- function(x) {
  if (is.numeric(x)) return(x)
  x <- gsub("[^0-9eE+\\-\\.]", "", as.character(x))
  suppressWarnings(as.numeric(x))
}

# Long format, drop zeros, keep only ≤ thr_high; tag low/high
df_long <- df0 %>%
  transmute(`2020` = numify(.data[["yield_kg_ha_2020"]]),
            `2021` = numify(.data[["yield_kg_ha_2021"]]),
            `2022` = numify(.data[[col_2022]])) %>%
  pivot_longer(everything(), names_to = "year", values_to = "yield_kg_ha") %>%
  filter(is.finite(yield_kg_ha), yield_kg_ha > 0) %>% 
  mutate(category = case_when(
    yield_kg_ha <= thr_low  ~ "Low (0–1109]",
    yield_kg_ha <= thr_high ~ "High (1109–2647]",
    TRUE                    ~ "Excluded"
  )) %>%
  filter(category != "Excluded") %>%
  mutate(
    year = factor(year, levels = c("2020","2021","2022")),
    category = factor(category, levels = c("Low (0–1109]","High (1109–2647]"))
  )

dir.create("output", showWarnings = FALSE)

# ---- Summarise: mean + SE + n ----
df_summary <- df_long %>%
  group_by(year, category) %>%
  summarise(
    mean_yield = mean(yield_kg_ha, na.rm = TRUE),
    se = sd(yield_kg_ha, na.rm = TRUE) / sqrt(n()),
    n = n(),
    .groups = "drop"
  )

# ---- Plot A: per year ----
p_year <- ggplot(df_summary, aes(x = year, y = mean_yield, fill = category)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(ymin = mean_yield - se, ymax = mean_yield + se),
                position = position_dodge(width = 0.7), width = 0.2) +
  geom_text(aes(label = paste0("n=", n)),
            position = position_dodge(width = 0.7),
            vjust = -0.8, size = 3.5) +
  scale_fill_manual(values = c("Low (0–1109]" = low_col,
                               "High (1109–2647]" = high_col)) +
  labs(x = NULL, y = "Average cocoa yield (kg/ha)", fill = NULL,
       title = "Average yield by year and input band (zeros excluded; ≤2647)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top",
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())

ggsave("output/avg_yield_low_vs_high_by_year.png",
       p_year, width = 8.5, height = 4.8, dpi = 300)

# ---- Plot B: all years pooled ----
df_summary_all <- df_long %>%
  group_by(category) %>%
  summarise(
    mean_yield = mean(yield_kg_ha, na.rm = TRUE),
    se = sd(yield_kg_ha, na.rm = TRUE) / sqrt(n()),
    n = n(),
    .groups = "drop"
  )

p_all <- ggplot(df_summary_all, aes(x = category, y = mean_yield, fill = category)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = mean_yield - se, ymax = mean_yield + se), width = 0.2) +
  geom_text(aes(label = paste0("n=", n)), vjust = -0.8, size = 3.5) +
  scale_fill_manual(values = c("Low (0–1109]" = low_col,
                               "High (1109–2647]" = high_col), guide = "none") +
  scale_x_discrete(labels = c("Low (0–1109]" = "Low\n(0–1109]",
                              "High (1109–2647]" = "High\n(1109–2647]")) +
  labs(x = NULL, y = "Average cocoa yield (kg/ha)",
       title = "Average yield by input band (zeros excluded; ≤2647; all years)") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())

ggsave("output/avg_yield_low_vs_high_overall.png",
       p_all, width = 5.5, height = 4.8, dpi = 300)

# ==============
# === 1) Keep only positive yields for plotting (drop 0s), and cap at HIGH_CAP ===
df_graph <- df_num %>%
  dplyr::filter(is.finite(yield), yield > 0, yield <= HIGH_CAP)

# === 2) Build the skewed (centered log1p minus gamma) values ===
log1p_y <- log1p(df_graph$yield)
center  <- median(log1p_y, na.rm = TRUE)

set.seed(123)
eps <- rgamma(n = length(log1p_y), shape = SKEW_SHAPE, rate = SKEW_RATE)
log1p_skew <- (log1p_y - center) - eps

# === 3) Label bands ===
band <- dplyr::case_when(
  df_graph$yield > 0 & df_graph$yield <= LOW_MAX       ~ "Low input (>0–1109)",
  df_graph$yield > LOW_MAX & df_graph$yield <= HIGH_CAP ~ "High input (1109–2647)",
  TRUE ~ NA_character_
)

# === 4) Shared breaks ===
rng <- range(log1p_skew, na.rm = TRUE)
lo  <- floor(rng[1] / BINWIDTH_LOG) * BINWIDTH_LOG
hi  <- ceiling(rng[2] / BINWIDTH_LOG) * BINWIDTH_LOG
brks_log <- seq(lo, hi, by = BINWIDTH_LOG)

# Split
low_vals  <- log1p_skew[band == "Low input (>0–1109)"]
high_vals <- log1p_skew[band == "High input (1109–2647)"]

# Histograms
h_low  <- hist(low_vals,  breaks = brks_log, plot = FALSE, freq = FALSE, right = FALSE)
h_high <- hist(high_vals, breaks = brks_log, plot = FALSE, freq = FALSE, right = FALSE)

# === 5) Merge densities so overlapping bins only show the dominant color ===
merged <- tibble(
  xleft  = head(h_low$breaks, -1),
  xright = tail(h_low$breaks, -1),
  low    = h_low$density,
  high   = h_high$density
) %>%
  mutate(
    draw_color = case_when(
      low > high ~ "low",
      high > low ~ "high",
      TRUE ~ "none"
    ),
    height = pmax(low, high)
  )

# === 6) Plot ===
ymax <- max(merged$height, na.rm = TRUE)
ylim <- c(0, 1.08 * ymax)

dir.create("output", showWarnings = FALSE)
png("output/hist_log1p_yield_skewed_no_overlap.png", width = 1300, height = 900, res = 150)
par(mar = c(4.5, 5, 3.6, 1.5))

plot(0, 0, type = "n",
     xlab = "log1p(yield) centered + skew (zeros excluded)",
     ylab = "Density",
     main = "Skewed histogram based on low-input and high-input attainable yield",
     xlim = c(lo, hi), ylim = ylim)

# Draw only dominant color per bin
for (i in seq_len(nrow(merged))) {
  if (merged$draw_color[i] == "low") {
    rect(merged$xleft[i], 0, merged$xright[i], merged$low[i], col = low_fill, border = border_col)
  } else if (merged$draw_color[i] == "high") {
    rect(merged$xleft[i], 0, merged$xright[i], merged$high[i], col = high_fill, border = border_col)
  }
}

# Density lines
d_low  <- density(low_vals)
d_high <- density(high_vals)
lines(d_low,  lty = 2, lwd = 2)
lines(d_high, lwd = 2)

abline(v = 0, lty = 3, lwd = 1.5)

legend("topright",
       fill   = c(low_fill, high_fill),
       border = border_col,
       lty    = c(2, 1),
       legend = c("Low input (>0–1109)", "High input (1109–2647)"),
       bty = "n", cex = 0.95)

dev.off()

cat("Saved plot: ", normalizePath("output/hist_log1p_yield_skewed_no_overlap.png"), "\n")

# adjust
# --- SETUP ---
library(dplyr)

df <- read.csv("UQ Cocoa R/2_Data/Ecom_yield_management_shade.csv")

HIGH_CAP  <- 2647
LOW_CAP   <- 1109
SKEW_SHAPE <- 2
SKEW_RATE  <- 2

if (!dir.exists("output")) dir.create("output")

plot_skewed_histogram_continuous <- function(data, var_name, year_label, file_name) {
  
# 1. Filter data
  df_clean <- data %>%
    filter(!is.na(.data[[var_name]]),
           .data[[var_name]] > 0,
           .data[[var_name]] <= HIGH_CAP)
  
  if (nrow(df_clean) == 0) return(NULL)
  
# 2. Common skew transformation for all data
  log_y <- log1p(df_clean[[var_name]])
  center <- median(log_y, na.rm = TRUE)
  set.seed(123)
  eps <- rgamma(n = length(log_y), shape = SKEW_SHAPE, rate = SKEW_RATE)
  df_clean$log_skew <- (log_y - center) - eps
  
# 3. Separate into exclusive bands based on raw yield
  low_vals  <- df_clean %>% filter(.data[[var_name]] <= LOW_CAP) %>% pull(log_skew)
  high_vals <- df_clean %>% filter(.data[[var_name]] >  LOW_CAP & .data[[var_name]] <= HIGH_CAP) %>% pull(log_skew)
  
# 4. Create small x gaps between them to prevent overlap in bins
  gap <- 0.01
  low_vals_adj  <- low_vals  - gap
  high_vals_adj <- high_vals + gap
  
  # 5. Density and limits
  d_low  <- density(low_vals_adj)
  d_high <- density(high_vals_adj)
  x_min  <- min(df_clean$log_skew) - 0.5
  x_max  <- max(df_clean$log_skew) + 0.5
  y_max  <- max(c(d_low$y, d_high$y)) * 1.1
  
  # 6. Colors
  low_fill  <- rgb(30/255,144/255,255/255,0.7)
  high_fill <- rgb(255/255,140/255,0,0.7)
  border_col <- "white"
  
  # 7. Plot
  png(file.path("output", file_name), width=8, height=6, units="in", res=300)
  
  hist(low_vals_adj,
       breaks=25, freq=FALSE,
       col=low_fill, border=border_col,
       xlim=c(x_min,x_max), ylim=c(0,y_max),
       xlab="log1p(yield) centered + skew (zeros excluded)",
       ylab="Density",
       main=paste("Skewed histogram based on low- and high-input attainable yield —", year_label),
       cex.main=1.05, cex.lab=1)
  
  hist(high_vals_adj,
       breaks=25, freq=FALSE,
       col=high_fill, border=border_col,
       add=TRUE)
  
# Add density lines
  lines(d_low,  lty=2, lwd=2)
  lines(d_high, lwd=2)
  
# Reference lines
  abline(v=0, lty=3, lwd=1.3)
  
  legend("topright",
         fill   = c(low_fill, high_fill),
         border = border_col,
         lty    = c(2,1),
         legend = c("Low input (>0–1109)", "High input (1109–2647)"),
         bty="n", cex=0.95)
  
  dev.off()
}

# --- Run for both years ---
plot_skewed_histogram_continuous(df, "yield_2020_21_kg", "2020/21", "histogram_yield_2020_21_continuous.png")
plot_skewed_histogram_continuous(df, "yield_2021_22_kg", "2021/22", "histogram_yield_2021_22_continuous.png")