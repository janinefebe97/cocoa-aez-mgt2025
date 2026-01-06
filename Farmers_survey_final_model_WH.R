## Title: Farm surveys all variables model test (1.3) - log.transform
# Author: Wilma
# Date: 23 Oct
#this dataset looks at the yield data from the interviews what is missing is to trace back the steps that Janine took to get to that csv file and clearly document what data was excluded. Also look at other management variable to make sure we did not accidentally exclude a variable that did have interesting differences. 


###########################################################
# LIBRARIES
############################################################
library(tidyverse)      # for data manipulation + read_csv
library(broom)          # for tidy model summaries
library(modelsummary)   # for comparison tables
library(patchwork)

############################################################
# 0) LOAD & CLEAN DATA
############################################################

# Load your data
df <- read_csv("UQ Cocoa R/2_Data/cocoa_yield_all_models.csv")
names(df)

# rename and clean the data like this:
df_clean <- df %>%
  filter(
    yield_sampling_year > 0,      # remove zero-yield farms (they are likely abandoned)
  ) %>%
  mutate(
    aez_zone = factor(aez_zone),
    shade_cover = as.numeric(shade_cover),
    yield_sampling_year = as.numeric(yield_sampling_year),
    sampling_year = factor(sampling_year),
    cocoa_tree_density_ha = as.numeric(cocoa_tree_density_ha),
    farm_age = as.numeric(farm_age),
    granular_fertilizer = as.factor(granular_fertilizer),
    removal_of_chupons  = as.factor(removal_of_chupons),
    pruning_of_mistletoe = as.factor(pruning_of_mistletoe),
    sanitary_pruning   = as.factor(sanitary_pruning),
    pesticides          = as.factor(pesticides),
    fungicides          = as.factor(fungicides)
  ) %>%
  drop_na(yield_sampling_year, shade_cover, aez_zone, sampling_year) %>%
  mutate(log_yield = log(yield_sampling_year))

str(df_clean)

# ============================================================
# FIGURE 1 — Cocoa yield distribution 
# ============================================================

par(mfrow=c(1,1))
# Define histogram breaks every 250 kg
breaks_seq <- seq(0, 3000, by = 250)

# Compute the histogram object (no plot yet)
h <- hist(
  df_clean$yield_sampling_year,
  breaks = breaks_seq,
  plot = FALSE
)

# Adjust plot margins
par(mar = c(5, 5, 4, 2) + 0.1)

# Transparent firebrick colour
bar_col <- adjustcolor("dodgerblue4", alpha.f = 0.6)  # 0.6 = 60% opacity

# Plot histogram
plot(
  h,
  col = bar_col,
  border = "white",
  main = "Distribution of cocoa yields across farms",
  xlab = "Cocoa yield (kg ha⁻¹)",
  ylab = "Number of farms",
  ylim = c(0, max(h$counts) * 1.2),
  axes = FALSE,
  frame.plot = FALSE
)

# Custom axes touching at zero
axis(1, pos = 0, tck = -0.02)
axis(2, pos = 0, las = 1, tck = -0.02)
# grid(nx = NA, ny = NULL, col = "grey90", lty = "dotted")

# Reference lines
abline(v = 1109, col = "dodgerblue3", lwd = 2, lty = 2)
abline(v = 2647, col = "dodgerblue3", lwd = 2, lty = 2)

# Labels beside lines
text(
  x = 1109 - 100, y = 60,
  labels = "Low-input attainable",
  srt = 90, col = "dodgerblue3", adj = c(0, 0.5), cex = 0.8
)
text(
  x = 2647 - 100, y = 60,
  labels = "High-input attainable",
  srt = 90, col = "dodgerblue3", adj = c(0, 0.5), cex = 0.8
)


#================================================================
# 1) Data vector
# Data vector
vals <- df_clean$yield_sampling_year
vals <- vals[is.finite(vals) & !is.na(vals)]

# --- Stats ---
mu    <- mean(vals)
med   <- median(vals)
sigma <- sd(vals)

# --- Histogram setup ---
breaks_seq <- seq(0, 3000, by = 250)
h <- hist(vals, breaks = breaks_seq, plot = FALSE)

# --- Normal curve (scaled to counts) ---
binw    <- diff(h$breaks)[1]
x_curve <- seq(min(h$breaks), max(h$breaks), length.out = 400)
y_curve <- dnorm(x_curve, mean = mu, sd = sigma) * length(vals) * binw

# --- Graphics setup ---
par(mfrow = c(1,1))
par(mar = c(5, 5, 4, 2) + 0.1)

bar_col <- adjustcolor("dodgerblue4", alpha.f = 0.6)

# --- Plot histogram ---
plot(
  h,
  col = bar_col,
  border = "white",
  main = "Distribution of cocoa yields across farms",
  xlab = expression(bold("Cocoa yield (kg ha"^{-1}*")")),
  ylab = expression(bold("Number of farms")),
  ylim = c(0, 1.2 * max(c(h$counts, y_curve))),
  axes = FALSE,
  frame.plot = FALSE,
  xaxs = "i"
)

# --- Axes ---
axis(1, pos = 0, tck = -0.02)
axis(2, pos = 0, las = 1, tck = -0.02)

# --- Threshold lines ---
low_thr  <- 1109
high_thr <- 2647
abline(v = low_thr,  col = "dodgerblue3", lwd = 2, lty = 2)
abline(v = high_thr, col = "dodgerblue3", lwd = 2, lty = 2)
text(low_thr  - 100, 60, "Low-input attainable",  srt = 90, col = "dodgerblue3", adj = c(0, 0.5), cex = 0.8)
text(high_thr - 100, 60, "High-input attainable", srt = 90, col = "dodgerblue3", adj = c(0, 0.5), cex = 0.8)

# --- Mean / Median ---
abline(v = mu,  col = "red",  lwd = 2)
abline(v = med, col = "navy", lwd = 2)

# --- Legend between the vertical lines ---
x_mid <- (low_thr + high_thr) / 2
y_top <- 0.95 * max(c(h$counts, y_curve))

legend(
  x = x_mid, y = y_top,
  legend = c(
    bquote(bold("Mean = ")*.(round(mu,0))*" kg ha"^{-1}),
    bquote(bold("Median = ")*.(round(med,0))*" kg ha"^{-1})
  ),
  lwd = c(2, 2),
  col = c("red", "navy"),
  bty = "n",
  xjust = 0.5, yjust = 1,
  cex = 0.9
)


# =====================================================
# HISTOGRAM + MEAN YIELD BY SAMPLING YEAR
# =====================================================

library(dplyr)

# --- Clean data first (example filter consistent with your earlier workflow) ---
# Assuming df_clean already exists and includes:
#   yield_2021_22_kg (numeric)
#   sampling_year (1 or 2)
# Replace 'yield_sampling_year' in your code with the correct yield column

# 1️⃣ Calculate mean yield by sampling year
mean_by_year <- df_clean %>%
  filter(!is.na(yield_sampling_year),
         !is.na(sampling_year),
         yield_sampling_year > 0,
         yield_sampling_year <= 2647) %>%
  group_by(sampling_year) %>%
  summarise(
    Mean_Yield = round(mean(yield_sampling_year, na.rm = TRUE), 2),
    SD_Yield = round(sd(yield_sampling_year, na.rm = TRUE), 2),
    N = n()
  )

print(mean_by_year)
cat("\nMean yield by sampling year (kg/ha):\n")
print(mean_by_year)

# --- OPTIONAL: Save to CSV if desired ---
if(!dir.exists("output")) dir.create("output")
write.csv(mean_by_year, "output/mean_yield_by_sampling_year.csv", row.names = FALSE)


# =====================================================
# COUNT: Yield interval with the highest number of farms
# =====================================================

library(dplyr)

# Assuming df_clean already exists and contains yield_2021_22_kg > 0 & ≤ 2647
# -----------------------------------------------------

# 1️⃣ Define bin width and breaks (same as histogram)
breaks_seq <- seq(0, 3000, by = 250)

# 2️⃣ Assign each farm to a yield interval
df_bins <- df_clean %>%
  mutate(
    yield_bin = cut(
      yield_sampling_year,
      breaks = breaks_seq,
      include.lowest = TRUE,
      right = FALSE
    )
  )

# 3️⃣ Count number of farms per interval
yield_bin_summary <- df_bins %>%
  group_by(yield_bin) %>%
  summarise(
    Farms = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(Farms))

# 4️⃣ Identify the bin with the maximum number of farms
top_bin <- yield_bin_summary[1, ]

# 5️⃣ Print results
cat("Yield interval with the highest number of farms:\n")
print(top_bin)

cat("\nFull yield distribution summary:\n")
print(yield_bin_summary)

# Optional: Save to output
if(!dir.exists("output")) dir.create("output")
write.csv(yield_bin_summary, "output/yield_bin_summary.csv", row.names = FALSE)


############################################################
# 2) COMPLETE MODEL WITH COVER, AEZ and FARM MANAGEMENT VARIABLES
############################################################
# Subsets
df_30 <- df_clean %>% filter(shade_cover <= 0.3)
df_40 <- df_clean %>% filter(shade_cover <= 0.4)
#df_clean                # all data (no cutoff)

# ============================================================
# Compare full model across shade thresholds
# ============================================================

# Fit same model structure to each subset
m_30 <- lm(
  log_yield ~ poly(shade_cover, 2) * aez_zone +
    poly(cocoa_tree_density_ha, 2) +
    farm_age + granular_fertilizer + removal_of_chupons +
    pruning_of_mistletoe + sanitary_pruning +
    pesticides + fungicides + sampling_year,
  data = df_30
)

# Fit same model structure to each subset
m_30_linear <- lm(
  log_yield ~ shade_cover * aez_zone +
    poly(cocoa_tree_density_ha, 2) +
    farm_age + granular_fertilizer + removal_of_chupons +
    pruning_of_mistletoe + sanitary_pruning +
    pesticides + fungicides + sampling_year,
  data = df_30
)


m_40 <- update(m_30, data = df_40)
m_full <- update(m_30, data = df_clean)

# Compare AICs
aic_table2 <- AIC(m_30, m_40, m_full) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Model")
print(aic_table2)

# Optional formatted summary
modelsummary(
  list("≤ 30 %" = m_30, "≤ 40 %" = m_40, "Full dataset" = m_full),
  gof_omit = "IC|Log|Adj|F$"
)


#check model assumptions
par(mfrow=c(2,2))
plot(m_30)

#model results
anova(m_30)
summary(m_30)


# Simple summary of proportions -------------------------------------------

# Total number of farms
n_total <- nrow(df_clean)

# Farms below each threshold
n_30 <- sum(df_clean$shade_cover <= 0.3, na.rm = TRUE)
n_40 <- sum(df_clean$shade_cover <= 0.4, na.rm = TRUE)

# Combine into a quick summary table
shade_summary <- tibble(
  Threshold = c("≤ 30%", "≤ 40%"),
  Farms = c(n_30, n_40),
  Percent_of_total = round(c(n_30, n_40) / n_total * 100, 1)
)

print(shade_summary)

# Farms with ≤ 30 % and ≤ 40 % shade cover accounted for 91.1 %, and 96.2 % of all observations (n=316), respectively, confirming that farms exceeding 30 % shade were rare in the sample.

# ============================================================
# FIGURE 2 — Shade–yield and AEZ (log-scale fit, axis relabelled)
# ============================================================

library(ggplot2)
library(dplyr)
library(patchwork)
library(scales)

# ---- Predictions (stay on log scale) ----
shade_seq <- seq(0, 0.3, length.out = 200)
newdat_shade <- data.frame(
  shade_cover = shade_seq,
  aez_zone = levels(df_clean$aez_zone)[1],
  cocoa_tree_density_ha = mean(df_clean$cocoa_tree_density_ha, na.rm = TRUE),
  farm_age = mean(df_clean$farm_age, na.rm = TRUE),
  granular_fertilizer = factor(1, levels = levels(df_clean$granular_fertilizer)),
  removal_of_chupons = factor(1, levels = levels(df_clean$removal_of_chupons)),
  pruning_of_mistletoe = factor(1, levels = levels(df_clean$pruning_of_mistletoe)),
  sanitary_pruning = factor(1, levels = levels(df_clean$sanitary_pruning)),
  pesticides = factor(1, levels = levels(df_clean$pesticides)),
  fungicides = factor(1, levels = levels(df_clean$fungicides)),
  sampling_year = levels(df_clean$sampling_year)[1]
)

pred_s <- predict(m_30, newdata = newdat_shade, se.fit = TRUE)
newdat_shade <- newdat_shade %>%
  mutate(fit = pred_s$fit,
         lwr = pred_s$fit - 1.96 * pred_s$se.fit,
         upr = pred_s$fit + 1.96 * pred_s$se.fit)

# ---- (a) Shade–yield ----
p1a <- ggplot() +
  geom_point(
    data = df_clean %>% filter(shade_cover <= 0.3),
    aes(x = shade_cover * 100, y = log_yield),
    alpha = 0.4, color = "dodgerblue3", size = 2
  ) +
  geom_ribbon(
    data = newdat_shade %>% filter(shade_cover <= 0.3),
    aes(x = shade_cover * 100, ymin = lwr, ymax = upr),
    fill = "grey50", alpha = 0.25
  ) +
  geom_line(
    data = newdat_shade %>% filter(shade_cover <= 0.3),
    aes(x = shade_cover * 100, y = fit),
    color = "grey50", linewidth = 1.1
  ) +
  scale_x_continuous(
    name = "Shade cover (%)",
    limits = c(0, 30),
    breaks = seq(0, 30, 10)
  ) +
  scale_y_continuous(
    name = expression(bold("Cocoa yield (kg ha"^{-1}*")")),
    labels = function(x) scales::comma(round(exp(x))),
    breaks = log(c(250, 500, 1000, 2000, 3000))
  ) +
  labs(title = "a) Yield–shade relationship") +
  theme_minimal(base_size = 13) +
  theme(
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0),
    panel.grid = element_blank()  # remove all gridlines
  )

# ---- (b) AEZ ----
df_plot_aez <- df_clean %>%
  filter(shade_cover <= 0.3) %>%
  drop_na(aez_zone)

p1b <- ggplot(df_plot_aez, aes(x = aez_zone, y = log_yield)) +
  geom_boxplot(outlier.shape = NA, fill = "grey90", color = "grey40") +
  geom_jitter(width = 0.15, alpha = 0.5, size = 2, color = "dodgerblue3") +
  annotate("text", x = 2.5, y = max(df_plot_aez$log_yield, na.rm = TRUE) + 0.2,
           label = "", fontface = "bold") +
  scale_y_continuous(
    name = expression(bold("Cocoa yield (kg ha"^{-1}*")")),
    labels = function(x) scales::comma(round(exp(x))),
    breaks = log(c(250, 500, 1000, 2000, 3000))
  ) +
  labs(x = "Agro-ecological zone", title = "b) Yield across AEZ") +
  theme_minimal(base_size = 13) +
  theme(
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0),
    panel.grid = element_blank()  # remove gridlines
  )

# Combine both plots
fig1 <- p1a | p1b
fig1


# ============================================================
# FIGURE 3 — Cocoa density + management (log-scale fit, labelled as kg ha⁻¹)
# ============================================================

df_plot <- df_clean %>%
  filter(shade_cover <= 0.3) %>%
  mutate(
    pruning_of_mistletoe = factor(pruning_of_mistletoe, labels = c("No", "Yes")),
    pesticides           = factor(pesticides,           labels = c("No", "Yes")),
    fungicides           = factor(fungicides,           labels = c("No", "Yes"))
  )

# ---- (a) Density ----
dens_seq <- seq(min(df_clean$cocoa_tree_density_ha, na.rm = TRUE),
                max(df_clean$cocoa_tree_density_ha, na.rm = TRUE),
                length.out = 200)
newdat_dens <- newdat_shade[rep(1, 200), ]
newdat_dens$cocoa_tree_density_ha <- dens_seq

pred_d <- predict(m_30, newdata = newdat_dens, se.fit = TRUE)
newdat_dens <- newdat_dens %>%
  mutate(fit = pred_d$fit,
         lwr = pred_d$fit - 1.96 * pred_d$se.fit,
         upr = pred_d$fit + 1.96 * pred_d$se.fit)

p2a <- ggplot() +
  geom_point(data = df_clean,
             aes(x = cocoa_tree_density_ha, y = log_yield),
             alpha = 0.4, color = "dodgerblue3", size = 2) +
  geom_ribbon(data = newdat_dens,
              aes(x = cocoa_tree_density_ha, ymin = lwr, ymax = upr),
              fill = "grey50", alpha = 0.25) +
  geom_line(data = newdat_dens,
            aes(x = cocoa_tree_density_ha, y = fit),
            color = "grey50", linewidth = 1.1) +
  scale_x_continuous(
    name = expression(bold("Cocoa density (trees ha"^{-1}*")")),
    labels = comma
  ) +
  scale_y_continuous(
    name = expression(bold("Cocoa yield (kg ha"^{-1}*")")),
    labels = function(x) comma(round(exp(x))),
    breaks = log(c(250, 500, 1000, 2000, 3000))
  ) +
  labs(title = "a) Cocoa density") +
  theme_minimal(base_size = 13) +
  theme(
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0),
    panel.grid = element_blank()
  )

# ---- Helper for boxplots ----
plot_box <- function(var, title, sig_label = NULL) {
  df_var <- df_plot %>% drop_na({{var}})
  ymax <- max(df_var$log_yield, na.rm = TRUE)
  
  p <- ggplot(df_var, aes_string(x = var, y = "log_yield")) +
    geom_boxplot(outlier.shape = NA, fill = "grey90", color = "grey40") +
    geom_jitter(width = 0.15, alpha = 0.5, size = 2, color = "dodgerblue3") +
    scale_y_continuous(
      name = expression(bold("Cocoa yield (kg ha"^{-1}*")")),
      labels = function(x) comma(round(exp(x))),
      breaks = log(c(250, 500, 1000, 2000, 3000))
    ) +
    labs(x = "Farmers survey", title = title) +
    theme_minimal(base_size = 13) +
    theme(
      axis.title.x = element_text(face = "bold"),
      axis.title.y = element_text(face = "bold"),
      plot.title = element_text(face = "bold", hjust = 0.5),
      panel.grid = element_blank()
    )
  
  if (!is.null(sig_label)) {
    p <- p + annotate("text", x = 1.5, y = ymax + 0.2,
                      label = sig_label, size = 5, fontface = "bold")
  }
  p
}

# ---- (b–d) Boxplots ----
p2b <- plot_box("pesticides", "b) Pesticide use", "*")
p2c <- plot_box("fungicides", "c) Fungicide use", "·")
p2d <- plot_box("pruning_of_mistletoe", "d) Mistletoe pruning", "·")

# ---- Combine all ----
fig2 <- (p2a | p2b | p2c | p2d)
fig2



# few checks on the U shaped relationship which is biologically impossible:

# ============================================================
# Check the shade–yield relationship on the log scale
# ============================================================

# 1. Create prediction data (use the same AEZ and year levels as before)
newdat_log <- expand.grid(
  shade_cover = seq(0, 0.4, length.out = 100),       # 0–40% for smooth curve
  aez_zone = factor(3, levels = levels(df_30$aez_zone)),  # e.g. AEZ 3 (semi-deciduous)
  cocoa_tree_density_ha = mean(df_30$cocoa_tree_density_ha, na.rm = TRUE),
  farm_age = mean(df_30$farm_age, na.rm = TRUE),
  granular_fertilizer = factor(1, levels = levels(df_30$granular_fertilizer)),
  removal_of_chupons = factor(1, levels = levels(df_30$removal_of_chupons)),
  pruning_of_mistletoe = factor(1, levels = levels(df_30$pruning_of_mistletoe)),
  sanitary_pruning = factor(1, levels = levels(df_30$sanitary_pruning)),
  pesticides = factor(1, levels = levels(df_30$pesticides)),
  fungicides = factor(1, levels = levels(df_30$fungicides)),
  sampling_year = levels(df_30$sampling_year)[1]      # pick first year
)

# 2. Predict on the log scale (no back-transformation)
pred_log <- predict(m_30, newdata = newdat_log, se.fit = TRUE)

newdat_log$fit_log <- pred_log$fit
newdat_log$lwr_log <- pred_log$fit - 1.96 * pred_log$se.fit
newdat_log$upr_log <- pred_log$fit + 1.96 * pred_log$se.fit

# 3. Plot: log_yield scale (no exponentiation)
library(ggplot2)

ggplot() +
  geom_point(data = df_30, aes(x = shade_cover * 100, y = log_yield),
             alpha = 0.4, color = "grey50", size = 2) +
  geom_ribbon(data = newdat_log,
              aes(x = shade_cover * 100, ymin = lwr_log, ymax = upr_log),
              fill = "dodgerblue3", alpha = 0.2) +
  geom_line(data = newdat_log,
            aes(x = shade_cover * 100, y = fit_log),
            color = "dodgerblue3", linewidth = 1.2) +
  coord_cartesian(xlim = c(0, 30))+
  labs(x = "Shade cover (%)",
       y = "Log-transformed cocoa yield",
       title = "Shade–yield relationship (log scale, no back-transformation)") +
  theme_minimal(base_size = 13) +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold"))




# Linear version: no quadratic term for shade
m_30_linear <- lm(
  log_yield ~ shade_cover * aez_zone + 
    poly(cocoa_tree_density_ha, 2) + 
    farm_age + granular_fertilizer + removal_of_chupons +
    pruning_of_mistletoe + sanitary_pruning +
    pesticides + fungicides + sampling_year,
  data = df_30
)

anova(m_30_linear, m_30) # significant. , adding the quadratic term imporves the model. 


# ============================================================
# Supplementary Figure S1 — Shade–yield curves by AEZ (≤30% shade)
# ============================================================

library(ggplot2)
library(dplyr)
library(scales)

# ---- Prediction data on log scale ----
shade_seq <- seq(0, 0.3, length.out = 200)
newdat_AEZ <- expand.grid(
  shade_cover = shade_seq,
  aez_zone = levels(df_clean$aez_zone),
  cocoa_tree_density_ha = mean(df_clean$cocoa_tree_density_ha, na.rm = TRUE),
  farm_age = mean(df_clean$farm_age, na.rm = TRUE),
  granular_fertilizer = factor(1, levels = levels(df_clean$granular_fertilizer)),
  removal_of_chupons = factor(1, levels = levels(df_clean$removal_of_chupons)),
  pruning_of_mistletoe = factor(1, levels = levels(df_clean$pruning_of_mistletoe)),
  sanitary_pruning = factor(1, levels = levels(df_clean$sanitary_pruning)),
  pesticides = factor(1, levels = levels(df_clean$pesticides)),
  fungicides = factor(1, levels = levels(df_clean$fungicides)),
  sampling_year = levels(df_clean$sampling_year)[1]
)

pred_AEZ <- predict(m_30, newdata = newdat_AEZ, se.fit = TRUE)
newdat_AEZ <- newdat_AEZ %>%
  mutate(
    fit = pred_AEZ$fit,
    lwr = pred_AEZ$fit - 1.96 * pred_AEZ$se.fit,
    upr = pred_AEZ$fit + 1.96 * pred_AEZ$se.fit
  )

# ---- Plot (log scale internally, yield-labelled externally) ----
pS1 <- ggplot() +
  geom_point(
    data = df_clean %>% filter(shade_cover <= 0.3),
    aes(x = shade_cover * 100, y = log_yield),
    alpha = 0.25, color = "dodgerblue3", size = 2
  ) +
  geom_ribbon(
    data = newdat_AEZ %>% filter(shade_cover <= 0.3),
    aes(x = shade_cover * 100, ymin = lwr, ymax = upr, fill = aez_zone),
    alpha = 0.15
  ) +
  geom_line(
    data = newdat_AEZ %>% filter(shade_cover <= 0.3),
    aes(x = shade_cover * 100, y = fit, color = aez_zone),
    linewidth = 1.2
  ) +
  scale_color_brewer(
    palette = "Dark2",
    name = expression(bold("Agro-ecological zones"))
  ) +
  scale_fill_brewer(
    palette = "Dark2",
    name = expression(bold("Agro-ecological zones"))
  ) +
  scale_y_continuous(
    name = expression(bold("Cocoa yield (kg ha"^{-1}*")")),
    labels = function(x) comma(round(exp(x))),
    breaks = log(c(250, 500, 1000, 2000, 3000)),
    limits = c(log(200), log(3000))
  ) +
  coord_cartesian(xlim = c(0, 30)) +
  labs(
    x = "Shade cover (%)",
    title = ""
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    panel.grid = element_blank()
  )

pS1


# ============================================================
# Supplementary Figure S2 — Non-significant management variables
# ============================================================

df_plot_ns <- df_clean %>%
  filter(shade_cover <= 0.3) %>%
  drop_na(granular_fertilizer, removal_of_chupons, sanitary_pruning) %>%
  mutate(
    granular_fertilizer = factor(granular_fertilizer, labels = c("No", "Yes")),
    removal_of_chupons  = factor(removal_of_chupons,  labels = c("No", "Yes")),
    sanitary_pruning    = factor(sanitary_pruning,    labels = c("No", "Yes"))
  )

# ---- Helper function (log scale fit, axis as yield) ----
plot_box_ns <- function(var, title) {
  df_var <- df_plot_ns %>% drop_na({{var}})
  ymax <- max(df_var$log_yield, na.rm = TRUE)
  
  ggplot(df_var, aes_string(x = var, y = "log_yield")) +
    geom_boxplot(outlier.shape = NA, fill = "grey90", color = "grey40") +
    geom_jitter(width = 0.15, alpha = 0.4, size = 2, color = "dodgerblue3") +
    annotate("text", x = 1.5, y = ymax + 0.15, label = "ns",
             size = 5, fontface = "bold", color = "grey30") +
    scale_y_continuous(
      name = expression(bold("Cocoa yield (kg ha"^{-1}*")")),
      labels = function(x) comma(round(exp(x))),
      breaks = log(c(250, 500, 1000, 2000, 3000)),
      limits = c(log(200), log(3000))
    ) +
    labs(
      x = "Farmers survey",
      title = title
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid = element_blank(),                     # remove gridlines
      axis.title.y = element_text(face = "bold"),       # bold Y-axis
      axis.title.x = element_text(face = "bold"),       # bold X-axis
      plot.title   = element_text(face = "bold", hjust = 0.5)
    )
}

# ---- Combine panels ----
pS2a <- plot_box_ns("granular_fertilizer", "a) Granular Fertiliser")
pS2b <- plot_box_ns("removal_of_chupons",  "b) Removal of Chupons")
pS2c <- plot_box_ns("sanitary_pruning",    "c) Sanitary Pruning")

(pS2a | pS2b | pS2c)

# =================== Count percentage =====
library(dplyr)

df %>%
  count(pesticides) %>%                              # Count each category
  mutate(percentage = n / sum(n) * 100)              # Calculate percentage


mean(df$cocoa_tree_density_ha < 1300, na.rm = TRUE) * 100

library(dplyr)

df %>%
  mutate(density_group = cut(
    cocoa_tree_density_ha,
    breaks = c(0, 500, 1000, 1500),     # choose your thresholds
    labels = c("Low (<500)", "Medium (500–1000)", "Optimum (1000–1500)")
  )) %>%
  count(density_group) %>%
  mutate(percentage = n / sum(n) * 100)

#========== adding farm age =====
# ---- Prepare dataset ----
df_plot_ns <- df_clean %>%
  filter(shade_cover <= 0.3) %>%
  drop_na(granular_fertilizer, removal_of_chupons, sanitary_pruning, farm_age) %>%
  mutate(
    granular_fertilizer = factor(granular_fertilizer, labels = c("No", "Yes")),
    removal_of_chupons  = factor(removal_of_chupons,  labels = c("No", "Yes")),
    sanitary_pruning    = factor(sanitary_pruning,    labels = c("No", "Yes"))
  )

# ---- Helper function (log scale fit, axis as yield) ----
plot_box_ns <- function(var, title) {
  df_var <- df_plot_ns %>% drop_na({{var}})
  ymax <- max(df_var$log_yield, na.rm = TRUE)
  
  ggplot(df_var, aes_string(x = var, y = "log_yield")) +
    geom_boxplot(outlier.shape = NA, fill = "grey90", color = "grey40") +
    geom_jitter(width = 0.15, alpha = 0.4, size = 2, color = "dodgerblue3") +
    annotate("text", x = 1.5, y = ymax + 0.15, label = "ns",
             size = 5, fontface = "bold", color = "grey30") +
    scale_y_continuous(
      name = expression(bold("Cocoa yield (kg ha"^{-1}*")")),
      labels = function(x) comma(round(exp(x))),
      breaks = log(c(250, 500, 1000, 2000, 3000)),
      limits = c(log(200), log(3000))
    ) +
    labs(
      x = expression(bold("Farmers survey")),
      title = title
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid = element_blank(),                     # remove gridlines
      axis.title.y = element_text(face = "bold"),       # bold Y-axis
      axis.title.x = element_text(face = "bold"),       # bold X-axis
      plot.title   = element_text(face = "bold", hjust = 0.5)
    )
}

# ---- Combine panels ----
pS2a <- plot_box_ns("granular_fertilizer", "a) Granular Fertiliser")
pS2b <- plot_box_ns("removal_of_chupons",  "b) Removal of Chupons")
pS2c <- plot_box_ns("sanitary_pruning",    "c) Sanitary Pruning")

# ---- Farm age (numeric variable handled separately) ----
pS2d <- ggplot(df_plot_ns, aes(x = farm_age, y = log_yield)) +
  geom_point(alpha = 0.4, color = "dodgerblue3", size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "grey30", fill = "grey40", alpha = 0.25) +
  scale_y_continuous(
    name = expression(bold("Cocoa yield (kg ha"^{-1}*")")),
    labels = function(x) comma(round(exp(x))),
    breaks = log(c(250, 500, 1000, 2000, 3000)),
    limits = c(log(200), log(3000))
  ) +
  labs(
    x = expression(bold("Farm age")),
    title = "d) Farm age"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    axis.title.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    plot.title   = element_text(face = "bold", hjust = 0.5)
  )

# ---- Combine all panels ----
(pS2a | pS2b | pS2c | pS2d)

