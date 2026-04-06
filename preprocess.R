library(data.table)
library(sf)
library(stringr)
library(ragg)
library(ggplot2)
library(dplyr)
library(e1071)

# -------- Load and clean metadata --------
meta_new <- fread("metadata_Jan_2025.csv")
meta_new$Population <- tolower(meta_new$Population)
meta_new$Code[is.na(meta_new$Code)] <- "CAO"
meta_new$lat[meta_new$Code == "CAO"] <- 21.13292
meta_new$long[meta_new$Code == "CAO"] <- 70.78436

# Create lookup tables
tribe_lookup <- list(
  IE_Tribe = unique(meta_new$Code[meta_new$Linguistic_Group == "IE" & meta_new$Tribe == "Yes"]),
  IE_NonTribe = unique(meta_new$Code[meta_new$Linguistic_Group == "IE" & meta_new$Tribe == "No"]),
  DR_NonTribe = unique(meta_new$Code[meta_new$Linguistic_Group == "DR" & meta_new$Tribe == "No"]),
  DR_Tribe = unique(meta_new$Code[meta_new$Linguistic_Group == "DR" & meta_new$Tribe == "Yes"]),
  AA_Tribe = unique(meta_new$Code[meta_new$Linguistic_Group == "AA" & meta_new$Tribe == "Yes"]),
  TB_Tribe = unique(meta_new$Code[meta_new$Linguistic_Group == "TB" & meta_new$Tribe == "Yes"]),
  TB_NonTribe = unique(meta_new$Code[meta_new$Linguistic_Group == "TB" & meta_new$Tribe == "No"])
)

# -------- Load and clean main data --------
df <- fread("GI_AllSamples_17777_November4_freeze_coded.txt", sep = "~", header = TRUE)

df$gender[df$gender=="male"] = "Male"
df$gender[df$gender=="female"] = "Female"

df$WBC_Total_White_Blood_Cell_Count = as.numeric(df$WBC_Total_White_Blood_Cell_Count)
df$anthropometry.sys_bp = as.numeric(df$anthropometry.sys_bp)
df$anthropometry.dia_bp = as.numeric(df$anthropometry.dia_bp)
df$Platelet_Count = as.numeric(df$Platelet_Count)

names(df)[names(df)=="FBS_Fasting_Blood_Glucose"] = "Fasting_Blood_Glucose"
names(df)[names(df)=="HbA1C_Glycosylated_Haemoglobin"] = "Glycosylated_Haemoglobin"
names(df)[names(df)=="AST_SGOT"] = "Aspartate_Aminotransferase"
names(df)[names(df)=="ALT_SGPT"] = "Alanine_Aminotransferase"
names(df)[names(df)=="HB_Haemoglobin"] = "Haemoglobin"
names(df)[names(df)=="RBC_Red_Blood_Cell_Count"] = "Red_Blood_Cell_Count"
names(df)[names(df)=="MCH_Mean_Corpuscular_Hb"] = "Mean_Corpuscular_Haemoglobin"
names(df)[names(df)=="WBC_Total_White_Blood_Cell_Count"] = "White_Blood_Cell_Count"
names(df)[names(df)=="anthropometry.sys_bp"] = "Systolic_Blood_Pressure"
names(df)[names(df)=="anthropometry.dia_bp"] = "Diastolic_Blood_Pressure"
names(df)[names(df)=="anthropometry.head_cir"] = "Head_Circumference"
names(df)[names(df)=="anthropometry.height"] = "Height"
names(df)[names(df)=="anthropometry.weight"] = "Weight"
names(df)[names(df)=="anthropometry.waist_cir"] = "Waist_Circumference"
names(df)[names(df)=="anthropometry.hip_cir"] = "Hip_Circumference"
names(df)[names(df)=="anthropometry.body_fat"] = "Body_Fat_Percentage"
names(df)[names(df)=="anthropometry.glucose_mg_dl"] = "Blood_Glucose_Glucometer"
names(df)[names(df)=="RBS"] = "Random_Blood_Glucose"
names(df)[names(df)=="Neutrophils"] = "Neutrophils_Percentage"
names(df)[names(df)=="Lymphocytes"] = "Lymphocytes_Percentage"
names(df)[names(df)=="Eosinophils"] = "Eosinophils_Percentage"
names(df)[names(df)=="Basophils"] = "Basophils_Percentage"
names(df)[names(df)=="Monocytes"] = "Monocytes_Percentage"

df$HDL[df$HDL==0] = NA
df$blood_draw.Blood_draw_fasting[!(df$blood_draw.Blood_draw_fasting) %in% c("yes", "no")] = NA
df$blood_draw.Blood_draw_fasting[df$blood_draw.Blood_draw_fasting == "yes"] = "fasting"
df$blood_draw.Blood_draw_fasting[df$blood_draw.Blood_draw_fasting == "no"] = "non-fasting"

df$name_dob_1.state[df$name_dob_1.state=="andra_pradesh"] = "Andhra Pradesh"
df$name_dob_1.state[df$name_dob_1.state=="uttar_pradesh"] = "Uttar Pradesh"
df$name_dob_1.state[df$name_dob_1.state=="himachal_pradesh"] = "Himachal Pradesh"
df$name_dob_1.state[df$name_dob_1.state=="jammu_kashmir"] = "Jammu and Kashmir"
df$name_dob_1.state[df$name_dob_1.state=="madya_pradesh"] = "Madhya Pradesh"
df$name_dob_1.state[df$name_dob_1.state=="tamil_nadu"] = "Tamil Nadu"
df$name_dob_1.state[df$name_dob_1.state=="west_bengal"] = "West Bengal"
df$name_dob_1.state[df$name_dob_1.state=="new delhi"] = "Delhi"
df$name_dob_1.state[df$name_dob_1.state=="delhi"] = "Delhi"
df$name_dob_1.state = str_to_title(df$name_dob_1.state)

setDT(df)

# Create T_NT mapping
df[, T_NT := ethnicity_mapping]
for (group_name in names(tribe_lookup)) {
  df[ethnicity_mapping %in% tribe_lookup[[group_name]], T_NT := group_name]
}

# Add metadata
meta_dt <- setDT(meta_new)
setkey(meta_dt, Code)
setkey(df, ethnicity_mapping)

df[meta_dt, `:=`(
  biogeographic_region = i.Physiography,
  population_size = i.Demography
), on = .(ethnicity_mapping = Code)]

# -------- Filter columns --------
biomarkers <- c("Fasting_Blood_Glucose","Glycosylated_Haemoglobin","Urea","Creatinine",
                "Total_Bilirubin","Aspartate_Aminotransferase", "Alanine_Aminotransferase","Alkaline_Phosphatase","Cholesterol",
                "Triglycerides","HDL","LDL","Haemoglobin","Red_Blood_Cell_Count",
                "Mean_Corpuscular_Haemoglobin","White_Blood_Cell_Count","Neutrophils_Percentage","Lymphocytes_Percentage",
                "Eosinophils_Percentage","Monocytes_Percentage","Basophils_Percentage","Platelet_Count","Direct_Bilirubin",
                "Indirect_Bilirubin","Albumin","Protein","Random_Blood_Glucose", "BMI", "age",
                "Systolic_Blood_Pressure", "Diastolic_Blood_Pressure","Head_Circumference",
                "Height","Weight","Waist_Circumference",
                "Hip_Circumference","Body_Fat_Percentage","Blood_Glucose_Glucometer")

df$history_illness.medication_currently_status[df$history_illness.medication_currently_status==""] = NA
df$ancestry = sub("_.*", "", df$ethnicity_mapping)

core_cols <- c(
  "LocalID", "ethnicity_mapping", "name_dob_1.state", "gender", "center", "age",
  "history_illness.medication_currently_status",
  "blood_draw.Blood_draw_fasting",
  "T_NT", "ancestry",
  biomarkers
)

core_cols <- intersect(core_cols, names(df))
df <- df[, ..core_cols]

factor_cols <- c(
  "ethnicity_mapping","T_NT","gender","center","name_dob_1.state",
  "ancestry","blood_draw.Blood_draw_fasting","history_illness.medication_currently_status"
)
factor_cols <- intersect(factor_cols, names(df))
for (v in factor_cols) df[, (v) := as.factor(get(v))]

# -------- Z-SCORE OUTLIER REMOVAL (SKEWNESS-AWARE) --------
cat("\n========== Z-Score Outlier Removal ==========\n")

# Get numeric columns, excluding 'age'
numeric_column_names <- setdiff(
  names(df)[sapply(df, is.numeric)],
  "age"
)

# Track filtering statistics
filtering_stats <- data.frame(
  variable          = character(),
  total_values      = integer(),
  kept_values       = integer(),
  removed_z         = integer(),
  skewness          = numeric(),
  rule              = character(),
  stringsAsFactors  = FALSE
)

for (var_name in numeric_column_names) {
  # Pull and clean
  data_vector <- as.numeric(df[[var_name]])
  
  # Guard: drop non-finite (keep NA for assignment)
  finite_idx <- is.finite(data_vector)
  data_vector_clean <- data_vector[finite_idx]
  data_vector_clean <- data_vector_clean[!is.na(data_vector_clean)]
  
  # Skip: constant/empty or too few points for moments
  if (length(unique(data_vector_clean)) <= 1 || length(data_vector_clean) < 3) {
    cat("Skipping", var_name, "- constant/too few values\n")
    filtering_stats <- rbind(filtering_stats, data.frame(
      variable     = var_name,
      total_values = sum(!is.na(data_vector)),
      kept_values  = sum(!is.na(data_vector)),
      removed_z    = 0,
      skewness     = NA_real_,
      rule         = "skip",
      stringsAsFactors = FALSE
    ))
    next
  }
  
  # Moments
  mean_val <- mean(data_vector_clean)
  sd_val   <- sd(data_vector_clean)
  sk       <- suppressWarnings(e1071::skewness(data_vector_clean, type = 3))
  
  # Guard: non-finite sd or skewness
  if (!is.finite(sd_val) || sd_val == 0) {
    cat("Skipping", var_name, "- sd not finite or zero\n")
    filtering_stats <- rbind(filtering_stats, data.frame(
      variable     = var_name,
      total_values = sum(!is.na(data_vector)),
      kept_values  = sum(!is.na(data_vector)),
      removed_z    = 0,
      skewness     = sk,
      rule         = "skip",
      stringsAsFactors = FALSE
    ))
    next
  }
  if (!is.finite(sk)) sk <- 0  # Default to "symmetric" if undefined
  
  # Z-score outlier mask
  z_outliers <- rep(FALSE, length(data_vector))
  rule_used  <- "two_tails"
  
  if (abs(sk) <= 1) {
    # Symmetric / mildly skewed → both tails
    z_outliers <- !is.na(data_vector) & (
      data_vector < mean_val - 3 * sd_val |
        data_vector > mean_val + 3 * sd_val
    )
  } else if (sk > 1) {
    # Right-skewed → upper tail only
    z_outliers <- !is.na(data_vector) & (data_vector > mean_val + 3 * sd_val)
    rule_used  <- "upper_tail"
  } else { # sk < -1
    # Left-skewed → lower tail only
    z_outliers <- !is.na(data_vector) & (data_vector < mean_val - 3 * sd_val)
    rule_used  <- "lower_tail"
  }
  
  # Apply filter
  df[[var_name]][z_outliers] <- NA_real_
  z_removed <- sum(z_outliers, na.rm = TRUE)
  total_cnt <- sum(!is.na(data_vector))
  kept_cnt  <- total_cnt - z_removed
  
  filtering_stats <- rbind(filtering_stats, data.frame(
    variable     = var_name,
    total_values = total_cnt,
    kept_values  = kept_cnt,
    removed_z    = z_removed,
    skewness     = sk,
    rule         = rule_used,
    stringsAsFactors = FALSE
  ))
  
  if (z_removed > 0) {
    cat("Variable:", var_name,
        "| Skewness:", round(sk, 3),
        "| Rule:", rule_used,
        "| Z-score removed:", z_removed, "\n")
  }
}

# Save filtering statistics
write.csv(filtering_stats, "outlier_removal_stats.csv", row.names = FALSE)
cat("\nOutlier removal complete. Statistics saved to 'outlier_removal_stats.csv'\n")
cat("=============================================\n\n")

# -------- Coordinates --------
coords_dt <- unique(meta_new[, .(
  ethnicity_mapping = Code,
  lat  = as.numeric(lat),
  long = as.numeric(long)
)])
setkey(coords_dt, ethnicity_mapping)

# -------- Map boundaries --------
shp_bg <- read_sf("Survey-of-India-Index-Maps/IndiaBoundary/IndiaBoundary.shp") |>
  st_make_valid() |>
  st_transform(4326) |>
  st_simplify(dTolerance = 0.10, preserveTopology = TRUE) |>
  st_union() |>
  st_cast("MULTILINESTRING")

bg0 <- as.data.table(st_coordinates(shp_bg))
setnames(bg0, c("X", "Y"), c("long", "lat"))

grp_cols <- intersect(names(bg0), c("L1","L2","L3"))
bg0[, seg := do.call(paste, c(.SD, sep=".")), .SDcols = grp_cols]
bg0 <- bg0[, .SD[seq(1, .N, by = 8)], by = seg]
bg_coords <- bg0[, .(long = c(long, NA_real_), lat = c(lat, NA_real_)), by = seg][, seg := NULL]

# -------- Pre-compute landing page data --------
linguistic_counts <- df[, .N, by = ancestry][order(-N)]

sample_location_counts <- df[, .N, by = ethnicity_mapping]
sample_location_counts <- coords_dt[sample_location_counts, on = "ethnicity_mapping"]
sample_location_counts <- sample_location_counts[!is.na(lat) & !is.na(long)]
sample_location_counts[, alpha := 0.3 + 0.7 * (N - min(N)) / (max(N) - min(N))]
sample_location_counts[, hover := paste0("Ethnicity: ", ethnicity_mapping, "<br>Sample Count: ", N)]
sample_location_counts[, marker_size := pmax(3, sqrt(N) * 1.5)]

corr_preview_data <- df[!is.na(BMI) & !is.na(HDL), .(BMI, HDL)]
corr_coefficient <- cor(corr_preview_data$BMI, corr_preview_data$HDL, use = "complete.obs")

# ---- Landing summary metrics (precompute) ----
male   <- sum(df$gender == "Male", na.rm = TRUE)
female <- sum(df$gender == "Female", na.rm = TRUE)

landing_metrics <- list(
  total_samples     = nrow(df),
  total_samples_fmt = formatC(nrow(df), format = "d", big.mark = ","),
  avg_age           = round(mean(df$age, na.rm = TRUE), 1),
  avg_age_fmt       = paste0(round(mean(df$age, na.rm = TRUE), 1), " years"),
  male              = male,
  female            = female,
  gender_ratio_fmt  = if (female > 0) paste0(round(male / female, 2), ":1") else "N/A",
  ethnicities       = length(unique(df$ethnicity_mapping[!is.na(df$ethnicity_mapping)]))
)

saveRDS(landing_metrics, "landing_metrics.rds", compress = FALSE)

# -------- Save everything --------
cat("Saving processed data...\n")
saveRDS(df, "df_clean.rds", compress = FALSE)
saveRDS(coords_dt, "coords_clean.rds", compress = FALSE)
saveRDS(bg_coords, "bg_coords_clean.rds", compress = FALSE)

cat("Done! Files created:\n")
cat("- df_clean.rds\n- coords_clean.rds\n- bg_coords_clean.rds\n- landing_metrics.rds\n- outlier_removal_stats.csv\n")

# -------- Generate landing page PNGs (static assets) --------
dir.create("www/landing", recursive = TRUE, showWarnings = FALSE)

ver <- format(Sys.time(), "%Y%m%d%H%M%S")
writeLines(ver, "www/landing/VERSION.txt")

alpha_from_counts <- function(n, lo = 0.25, hi = 0.90) {
  rng <- range(n, na.rm = TRUE)
  if (!is.finite(rng[1]) || !is.finite(rng[2]) || rng[1] == rng[2]) {
    return(rep(hi, length(n)))
  }
  lo + (hi - lo) * (n - rng[1]) / (rng[2] - rng[1])
}

# 1) Age histogram
ragg::agg_png("www/landing/age_distribution.png", width = 900, height = 520, res = 120, background = "white")
print(
  ggplot(df, aes(x = age)) +
    geom_histogram(binwidth = 5, boundary = 0, fill = "#1b4965", color = "white") +
    scale_x_continuous(breaks = seq(0, 100, 10)) +
    theme_minimal(base_size = 16) +
    labs(x = "Age (years)", y = "Count")
)
dev.off()

# 2) Geographic sample distribution (overview map)
sample_location_counts <- df[, .N, by = ethnicity_mapping]
sample_location_counts <- coords_dt[sample_location_counts, on = "ethnicity_mapping"]
sample_location_counts <- sample_location_counts[!is.na(lat) & !is.na(long)]
sample_location_counts[, alpha := alpha_from_counts(N, lo = 0.25, hi = 0.90)]
sample_location_counts[, size := pmin(7, pmax(2.5, sqrt(N) * 0.35))]

ragg::agg_png("www/landing/sample_map.png", width = 900, height = 700, res = 120, background = "white")
print(
  ggplot() +
    geom_path(data = bg_coords, aes(x = long, y = lat), color = "grey80", linewidth = 0.35, na.rm = TRUE) +
    geom_point(data = sample_location_counts, aes(x = long, y = lat, alpha = alpha, size = size),
               color = "#1b4965") +
    scale_alpha_identity() +
    scale_size_identity() +
    coord_equal() +
    theme_void(base_size = 16) +
    theme(plot.margin = margin(5, 5, 5, 5))
)
dev.off()

# 3) Correlation preview (BMI vs HDL)
cp <- df[!is.na(BMI) & !is.na(HDL), .(BMI, HDL)]
corr_coefficient <- cor(cp$BMI, cp$HDL, use = "complete.obs")

ragg::agg_png("www/landing/correlation_preview.png", width = 900, height = 700, res = 120, background = "white")
print(
  ggplot(cp, aes(x = BMI, y = HDL)) +
    geom_bin2d(bins = 60) +
    scale_fill_gradient(low = "#b8e0ff", high = "#1b4965") +
    geom_smooth(method = "lm", se = TRUE, color = "#FF6B6B", alpha = 0.2, linewidth = 1) +
    theme_minimal(base_size = 16) +
    theme(legend.position = "right") +
    labs(
      x = "BMI (kg/m²)",
      y = "HDL (mg/dL)",
      fill = "Count",
      subtitle = sprintf("r = %.3f", corr_coefficient)
    )
)
dev.off()

cat("Landing PNGs written to www/landing/ (version ", ver, ")\n", sep = "")
