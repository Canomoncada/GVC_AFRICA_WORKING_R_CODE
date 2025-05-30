# ============================================================
# File: ready_africa_complete_analysis.R
# COMPLETE ANALYSIS: Ready Africa Digital Readiness Assessment
# Author: Anthony S. Cano Moncada (Canomoncada) | Date: May 2025
# Created: 2025-05-30 01:42:34 UTC
# Purpose: Complete workflow for loading, processing, and analyzing digital readiness indicators
# Description: Comprehensive R pipeline for GVC readiness assessment across multiple regions
# Last Updated: 2025-05-30 01:42:34 UTC
# ============================================================

# ============================================================
# PART 1: Environment Setup (Updated Paths)
# Purpose: Initialize project structure, install packages, and define helper functions
# Author: Anthony S. Cano Moncada | Date: May 2025
# Updated: 2025-05-30 01:42:34 UTC
# ============================================================

message("PART 1: Environment Setup")
message("Analysis started by: Canomoncada at 2025-05-30 01:42:34 UTC")

# Define project root and comprehensive directory structure
# These paths organize all outputs into logical categories for easy access
project_root <- "/Users/work/Downloads/Ready Africa"
params <- list(
  # Core data and script directories
  data_path      = file.path(project_root, "Data"),
  scripts_dir    = file.path(project_root, "scripts"),
  
  # Export directories organized by file type
  output_dir     = file.path(project_root, "Exports"),
  vis_dir        = file.path(project_root, "Exports", "Visuals"),
  csv_dir        = file.path(project_root, "Exports", "CSVs"),
  png_dir        = file.path(project_root, "Exports", "PNGs"),
  pdf_dir        = file.path(project_root, "Exports", "PDFs"),
  zip_dir        = file.path(project_root, "Exports", "ZIPs"),
  
  # Indicator reference years - standardized data collection periods
  indicator_years = list(
    internet   = 2023,  # Internet penetration data
    mobile     = 2023,  # Mobile connectivity index
    trade      = 2023,  # Trade to GDP ratio
    lpi        = 2023,  # Logistics Performance Index
    renewables = 2020,  # Modern renewables share (latest available)
    co2        = 2023,  # CO2 intensity data
    biz        = 2020,  # Business Ready Index (latest available)
    political  = 2023   # Political stability index
  ),
  
  # Regional groupings for comparative analysis
  regions = c("Africa", "ASEAN", "LAC", "OECD")
)

# Create essential directories if they don't exist
# This ensures the pipeline can run in any environment
for (dir in c(params$data_path, params$scripts_dir)) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    message(paste("Created directory:", dir))
  }
}

# Create all export subdirectories for organized output storage
export_dirs <- c(params$output_dir, params$vis_dir, params$csv_dir, 
                params$png_dir, params$pdf_dir, params$zip_dir)
for (dir in export_dirs) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    message(paste("Created export directory:", dir))
  }
}

# Comprehensive package installation and loading
# These packages provide all functionality needed for data processing, analysis, and visualization
required_pkgs <- c(
  # Core data manipulation and analysis
  "tidyverse",    # Data manipulation (dplyr, ggplot2, tidyr, etc.)
  "readxl",       # Excel file reading
  "haven",        # Stata file reading
  "janitor",      # Data cleaning utilities
  "countrycode",  # Country name standardization
  
  # Export and file handling
  "writexl",      # Excel writing (alternative)
  "openxlsx",     # Advanced Excel operations
  "readr",        # Fast CSV reading/writing
  "fs",           # File system operations
  "zip",          # Archive creation
  
  # Statistical analysis and visualization
  "pheatmap",     # Heatmap generation
  "corrplot",     # Correlation plots
  
  # Principal Component Analysis
  "FactoMineR",   # PCA implementation
  "factoextra",   # PCA visualization
  "missMDA",      # Missing data imputation for PCA
  
  # Graphics and reporting
  "png",          # PNG graphics device
  "grid",         # Grid graphics
  "sessioninfo"   # Session documentation
)

# Install missing packages only (efficient approach)
toinstall <- setdiff(required_pkgs, installed.packages()[, "Package"])
if (length(toinstall)) {
  message(paste("Installing missing packages:", paste(toinstall, collapse = ", ")))
  install.packages(toinstall)
}

# Load all required packages
invisible(lapply(required_pkgs, library, character.only = TRUE, quietly = TRUE))

# Define min-max normalization helper function
# This function standardizes all indicators to 0-1 scale for comparability
# Handles edge cases: single values, all identical values, missing data
normalize_minmax <- function(x) {
  x <- as.numeric(x)
  # Handle case where all values are identical or only one unique value
  if (length(unique(na.omit(x))) < 2) return(rep(0.5, length(x)))
  r <- range(x, na.rm = TRUE)
  # Handle case where min equals max
  if (diff(r) == 0) return(rep(0.5, length(x)))
  # Standard min-max normalization: (x - min) / (max - min)
  (x - r[1]) / diff(r)
}

message("Environment setup complete: directories created and packages loaded.")
message(paste("Package installation completed at:", Sys.time()))

# ============================================================
# PART 2: Country List & Region Assignments
# Purpose: Load master country list and define comprehensive regional groupings
# Author: Anthony S. Cano Moncada | Date: May 2025
# Updated: 2025-05-30 01:42:34 UTC
# ============================================================

message("PART 2: Load Country List & Regions")

# Load comprehensive country list from master file
# This serves as the foundation for all country-level analysis
countries_raw <- readr::read_csv(
  file.path(params$data_path, "countries of the world.csv"),
  show_col_types = FALSE
)

# Extract and clean country names for consistent matching
# Creates standardized country list for all subsequent joins
country_list <- countries_raw %>%
  dplyr::select(1) %>%
  dplyr::rename(country_clean = 1) %>%
  dplyr::distinct()

# Define comprehensive regional membership vectors
# These groupings align with major economic and development organizations
region_defs <- list(
  # African countries - comprehensive coverage of continent
  Africa = c(
    "Algeria","Angola","Benin","Botswana","Burkina Faso","Burundi",
    "Cape Verde","Cameroon","Central African Rep.","Chad","Comoros",
    "Congo, Dem. Rep.","Congo, Repub. of the","Cote d'Ivoire","Djibouti",
    "Egypt","Equatorial Guinea","Eritrea","Eswatini","Ethiopia",
    "Gabon","Gambia, The","Ghana","Guinea","Guinea-Bissau","Kenya",
    "Lesotho","Liberia","Libya","Madagascar","Malawi","Mali",
    "Mauritania","Mauritius","Morocco","Mozambique","Namibia",
    "Niger","Nigeria","Rwanda","Sao Tome & Principe","Senegal",
    "Seychelles","Sierra Leone","Somalia","South Africa","South Sudan",
    "Sudan","Tanzania","Togo","Tunisia","Uganda","Zambia","Zimbabwe"
  ),
  
  # ASEAN member countries - Southeast Asian economic bloc
  ASEAN = c(
    "Brunei","Cambodia","Indonesia","Laos","Malaysia","Burma",
    "Philippines","Singapore","Thailand","Vietnam"
  ),
  
  # Latin America and Caribbean - regional economic grouping
  LAC = c(
    "Argentina","Belize","Bolivia","Brazil","Chile","Colombia",
    "Costa Rica","Cuba","Dominican Republic","Ecuador","El Salvador",
    "Guatemala","Guyana","Honduras","Jamaica","Mexico","Nicaragua",
    "Panama","Paraguay","Peru","Suriname","Trinidad & Tobago",
    "Uruguay","Venezuela"
  ),
  
  # OECD member countries - developed economy benchmark group
  OECD = c(
    "Australia","Austria","Belgium","Canada","Chile","Colombia",
    "Costa Rica","Czech Republic","Denmark","Estonia","Finland",
    "France","Germany","Greece","Hungary","Iceland","Ireland",
    "Israel","Italy","Japan","Korea, South","Latvia","Lithuania",
    "Luxembourg","Mexico","Netherlands","New Zealand","Norway",
    "Poland","Portugal","Slovakia","Slovenia","Spain","Sweden",
    "Switzerland","Turkey","United Kingdom","United States"
  )
)

# Function: Clean raw country names to match standardized definitions
# Handles common variations in country naming across datasets
# Essential for consistent data joining across multiple sources
clean_country <- function(df, col) {
  df %>%
    dplyr::rename(country_raw = {{ col }}) %>%
    dplyr::mutate(country = dplyr::case_when(
      # Common country name variations that need standardization
      country_raw == "Czechia"           ~ "Czech Republic",
      country_raw == "Myanmar"           ~ "Burma",
      country_raw == "Brunei Darussalam" ~ "Brunei",
      country_raw == "Bahamas"           ~ "Bahamas, The",
      # Add more mappings as needed for data consistency
      TRUE                               ~ country_raw
    )) %>%
    dplyr::select(-country_raw)
}

# Function: Assign region based on predefined membership lists
# Creates regional classification for comparative analysis
# Returns "Other" for countries not in any defined regional group
assign_region <- function(df) {
  df %>%
    dplyr::mutate(Region = dplyr::case_when(
      Country %in% region_defs$Africa ~ "Africa",
      Country %in% region_defs$ASEAN  ~ "ASEAN", 
      Country %in% region_defs$LAC    ~ "LAC",
      Country %in% region_defs$OECD   ~ "OECD",
      TRUE                            ~ "Other"
    ))
}

message("Countries loaded and region functions defined.")
message(paste("Regional definitions created for", length(region_defs), "regions at:", Sys.time()))

# ============================================================
# PART 3: Load & Clean Indicators
# Purpose: Read each indicator file, filter by reference year, and standardize country names
# Author: Anthony S. Cano Moncada | Date: May 2025
# Updated: 2025-05-30 01:42:34 UTC
# ============================================================

message("PART 3: Load & Clean Indicators")

# Create shortcuts for frequently used parameters
dp <- params$data_path
yr <- params$indicator_years

# 1. Internet Penetration Rate
# Source: International data on individuals using the internet
# Measures: Percentage of population with internet access
message("Loading Internet Penetration data...")
internet <- readr::read_csv(
  file.path(dp, "Individuals-using-the-internet.csv"),
  show_col_types = FALSE
) %>%
  dplyr::filter(dataYear == yr$internet) %>%
  dplyr::group_by(entityName) %>%
  # Take mean in case of multiple entries per country
  dplyr::summarise(int_pen = mean(dataValue, na.rm = TRUE), .groups = "drop") %>%
  clean_country(., entityName)

# 2. Mobile Connectivity Index
# Source: GSMA Mobile Connectivity Index data
# Measures: Infrastructure, affordability, consumer readiness, content/services
message("Loading Mobile Connectivity data...")
mobile <- readr::read_csv(
  file.path(dp, "GSMA_Data_2024.csv"),
  show_col_types = FALSE
) %>%
  dplyr::filter(Year == yr$mobile) %>%
  dplyr::group_by(Country) %>%
  dplyr::summarise(mob_con = mean(Index, na.rm = TRUE), .groups = "drop") %>%
  clean_country(., Country)

# 3. Trade to GDP Ratio
# Source: World Bank trade statistics
# Measures: (Imports + Exports) as percentage of GDP - trade openness indicator
message("Loading Trade to GDP data...")
trade <- readr::read_csv(
  file.path(dp, "Trade (_ of GDP).csv"),
  show_col_types = FALSE
) %>%
  dplyr::select(`Country Name`, as.character(yr$trade)) %>%
  dplyr::rename(trad_GDP = as.character(yr$trade)) %>%
  dplyr::mutate(trad_GDP = as.numeric(trad_GDP)) %>%
  tidyr::drop_na(trad_GDP) %>%
  clean_country(., `Country Name`)

# 4. Logistics Performance Index (LPI)
# Source: World Bank Logistics Performance Index
# Measures: Logistics efficiency across multiple dimensions
message("Loading Logistics Performance Index data...")
lpi <- readxl::read_excel(
  file.path(dp, "International_LPI_from_2007_to_2023.xlsx"),
  sheet = "2023"
) %>%
  janitor::clean_names() %>%
  dplyr::transmute(country, lpi_score = as.numeric(lpi_score)) %>%
  tidyr::drop_na(lpi_score) %>%
  clean_country(., country)

# 5. Modern Renewables Share
# Source: International renewable energy database
# Measures: Share of modern renewables in total energy consumption
message("Loading Modern Renewables data...")
renew <- readxl::read_excel(
  file.path(dp, "Share of modern renewables database.xlsx")
) %>%
  dplyr::select(`Country/Region`, as.character(yr$renewables)) %>%
  dplyr::rename(ren_energy = as.character(yr$renewables)) %>%
  dplyr::mutate(ren_energy = as.numeric(ren_energy)) %>%
  tidyr::drop_na(ren_energy) %>%
  clean_country(., `Country/Region`)

# 6. CO2 Intensity (CO2 per unit GDP)
# Source: World Bank environmental indicators
# Measures: CO2 emissions per unit of economic output (inverted for index)
message("Loading CO2 Intensity data...")
co2 <- readr::read_csv(
  file.path(dp, "Co2toGDP_Data.csv"),
  show_col_types = FALSE
) %>%
  dplyr::select(`Country Name`, `2023 [YR2023]`) %>%
  dplyr::rename(co2_gdp = `2023 [YR2023]`) %>%
  dplyr::mutate(co2_gdp = as.numeric(co2_gdp)) %>%
  tidyr::drop_na(co2_gdp) %>%
  clean_country(., `Country Name`)

# 7. Business-Ready Index
# Source: World Bank Doing Business indicators
# Measures: Ease of doing business across regulatory dimensions
message("Loading Business Ready Index data...")
biz <- readxl::read_excel(
  file.path(dp, "Business-Ready.xlsx")
) %>%
  janitor::clean_names() %>%
  dplyr::transmute(country = economy, business_ready = as.numeric(db_2020)) %>%
  tidyr::drop_na(business_ready) %>%
  clean_country(., country)

# 8. Political Stability Index
# Source: World Bank Worldwide Governance Indicators
# Measures: Political stability and absence of violence/terrorism
message("Loading Political Stability data...")
pol <- haven::read_dta(
  file.path(dp, "Political Stability.dta")
) %>%
  dplyr::transmute(countryname, pol_stability = estimate + 2.5) %>% # Shift to positive scale
  dplyr::group_by(countryname) %>%
  dplyr::summarise(pol_stability = mean(pol_stability, na.rm = TRUE), .groups = "drop") %>%
  tidyr::drop_na(pol_stability) %>%
  clean_country(., countryname)

message("PART 3 complete: All indicators loaded & cleaned.")
message(paste("Data loading completed at:", Sys.time()))

# ============================================================
# PART 4: Merge & Normalize Indicators
# Purpose: Combine all indicator datasets and apply min-max normalization
# Author: Anthony S. Cano Moncada | Date: May 2025
# Updated: 2025-05-30 01:42:34 UTC
# ============================================================

message("PART 4: Merge & Normalize Indicators")

# Merge all indicator datasets onto master country list using sequential joins
# This approach preserves all countries even if they lack some indicator data
datasets <- list(
  internet, mobile, trade,
  lpi, renew, co2,
  biz, pol
)

# Use purrr::reduce for efficient sequential joining
merged_raw <- purrr::reduce(
  datasets,
  dplyr::left_join,
  .init = country_list,
  by = c("country_clean" = "country")
)

# Apply min-max normalization to each indicator
# Creates standardized 0-1 scales for all measures
# Note: CO2 intensity is inverted (1 - normalized) so higher values = better (lower emissions)
merged_norm <- merged_raw %>%
  dplyr::mutate(
    int_pen_norm  = normalize_minmax(int_pen),      # Internet penetration (higher = better)
    mob_con_norm  = normalize_minmax(mob_con),      # Mobile connectivity (higher = better)
    trad_GDP_norm = normalize_minmax(trad_GDP),     # Trade openness (higher = better)
    lpi_norm      = normalize_minmax(lpi_score),    # Logistics performance (higher = better)
    ren_norm      = normalize_minmax(ren_energy),   # Renewable energy (higher = better)
    co2_norm      = 1 - normalize_minmax(co2_gdp), # CO2 intensity (inverted: lower emissions = better)
    biz_norm      = normalize_minmax(business_ready), # Business environment (higher = better)
    ps_norm       = normalize_minmax(pol_stability)   # Political stability (higher = better)
  )

# Create final data table with descriptive variable names and rounded values
# This becomes the primary dataset for all subsequent analysis
final_dt <- merged_norm %>%
  dplyr::transmute(
    Country                         = country_clean,
    `Internet Penetration Index`    = round(int_pen_norm, 3),
    `Mobile Connectivity Index`     = round(mob_con_norm, 3),
    `Trade to GDP Ratio Index`      = round(trad_GDP_norm, 3),
    `Logistics Performance Index`   = round(lpi_norm, 3),
    `Modern Renewables Share Index` = round(ren_norm, 3),
    `CO₂ Intensity Index`           = round(co2_norm, 3),
    `Business Ready Index`          = round(biz_norm, 3),
    `Political Stability Index`     = round(ps_norm, 3)
  )

message("PART 4 complete: Indicators merged and normalized.")
message(paste("Normalization completed at:", Sys.time()))

# ============================================================
# PART 5: Export & Visualizations
# Purpose: Export normalized indices, generate rankings, summary stats, and regional boxplots
# Author: Anthony S. Cano Moncada | Date: May 2025
# Updated: 2025-05-30 01:42:34 UTC
# ============================================================

message("PART 5: Export & Visualizations")

# Define indicator variable names for consistent reference throughout analysis
indicator_vars <- c(
  "Internet Penetration Index", "Mobile Connectivity Index",
  "Trade to GDP Ratio Index",  "Logistics Performance Index",
  "Modern Renewables Share Index", "CO₂ Intensity Index",
  "Business Ready Index",        "Political Stability Index"
)

# 5.1 Export Full Normalized Dataset
# Create comprehensive table with regional assignments for all countries
message("5.1 Exporting full normalized dataset...")
full_tbl <- final_dt %>%
  assign_region() %>%
  select(Country, Region, all_of(indicator_vars))

openxlsx::write.xlsx(
  full_tbl,
  file.path(params$output_dir, "Annex_A_Full_Summary_2025.xlsx"),
  overwrite = TRUE
)
message("5.1 Full summary exported.")

# 5.2 Export 135-Country Regional Subset
# Focus on countries in defined regional groups for comparative analysis
message("5.2 Creating 135-country regional subset...")
subset_135 <- full_tbl %>%
  filter(Region %in% params$regions)

openxlsx::write.xlsx(
  subset_135,
  file.path(params$output_dir, "Annex_A_135_Countries_2025.xlsx"),
  overwrite = TRUE
)
message("5.2 Regional subset exported.")

# 5.3 Country Rankings by Individual Indicators
# Generate rankings for each indicator separately to identify leaders by dimension
message("5.3 Computing country rankings by indicator...")
rank_df <- subset_135 %>%
  mutate(across(
    all_of(indicator_vars),
    ~rank(-., ties.method = "min", na.last = "keep"), # Higher values get better (lower) ranks
    .names = "{.col}_Rank"
  ))

openxlsx::write.xlsx(
  rank_df,
  file.path(params$output_dir, "Annex_A_Rankings_2025.xlsx"),
  overwrite = TRUE
)
message("5.3 Indicator rankings exported.")

# 5.4 Comprehensive Summary Statistics
# Calculate detailed descriptive statistics for each indicator
message("5.4 Computing summary statistics...")
summary_stats <- final_dt %>%
  select(all_of(indicator_vars)) %>%
  summarise(across(
    everything(),
    list(
      count   = ~sum(!is.na(.)),                      # Non-missing observations
      missing = ~sum(is.na(.)),                       # Missing observations
      mean    = ~mean(., na.rm = TRUE),               # Central tendency
      median  = ~median(., na.rm = TRUE),             # Central tendency (robust)
      sd      = ~sd(., na.rm = TRUE),                 # Variability
      min     = ~min(., na.rm = TRUE),                # Minimum value
      q25     = ~quantile(., 0.25, na.rm = TRUE),     # First quartile
      q75     = ~quantile(., 0.75, na.rm = TRUE),     # Third quartile
      max     = ~max(., na.rm = TRUE)                 # Maximum value
    ), .names = "{.col}_{.fn}"
  )) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Indicator","Metric"),
    names_sep = "_",
    values_to = "Value"
  )

openxlsx::write.xlsx(
  summary_stats,
  file.path(params$output_dir, "Annex_A_Summary_Stats_Indicators_2025.xlsx"),
  overwrite = TRUE
)
message("5.4 Summary statistics exported.")

# 5.5 Regional Comparison Boxplot Visualization
# Create comprehensive boxplot showing distribution differences across regions
message("5.5 Creating regional boxplot visualization...")
bp_data <- subset_135 %>%
  pivot_longer(
    cols = all_of(indicator_vars),
    names_to  = "Indicator",
    values_to = "Score"
  ) %>%
  drop_na(Score)

# Create professional boxplot with proper formatting
boxplot_gg <- ggplot(bp_data, aes(x = Region, y = Score, fill = Region)) +
  geom_boxplot(outlier.size = 0.6) +
  facet_wrap(~Indicator, ncol = 4, scales = "free_y") +
  labs(
    title = "Regional Comparison of GVC Readiness Indicators",
    y     = "Normalized Score (0–1)"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Save high-resolution visualization
ggsave(
  file.path(params$vis_dir, "Figure_Regional_Boxplots_2025.png"),
  plot   = boxplot_gg,
  width  = 16,
  height = 10,
  dpi    = 300
)
message("5.5 Boxplot visualization saved.")

# 5.6 Export CSV Versions and Create Archive
# Provide CSV alternatives for wider software compatibility
message("5.6 Exporting CSV files and creating archive...")
readr::write_csv(full_tbl, file.path(params$csv_dir, "Annex_A_Full_Summary_2025.csv"))
readr::write_csv(subset_135, file.path(params$csv_dir, "Annex_A_135_Countries_2025.csv"))
readr::write_csv(rank_df, file.path(params$csv_dir, "Annex_A_Rankings_2025.csv"))
readr::write_csv(summary_stats, file.path(params$csv_dir, "Annex_A_Summary_Stats_Indicators_2025.csv"))

# Create compressed archive of Excel files
zip::zip(
  zipfile = file.path(params$zip_dir, "Part5_Exports.zip"),
  files   = fs::dir_ls(params$output_dir, glob = "*.xlsx")
)
message("5.6 Exports CSV & ZIP complete.")

# ============================================================
# PART 6: PCA, Scores & Visualizations
# Purpose: Impute missing data, run PCA analysis, compute composite scores, create visualizations
# Author: Anthony S. Cano Moncada | Date: May 2025
# Updated: 2025-05-30 01:42:34 UTC
# ============================================================

message("PART 6: PCA, Scores & Visualizations")

# 6.0 Verify PCA packages are loaded (already handled in Part 1)
pca_pkgs <- c("FactoMineR", "factoextra", "missMDA", "readr")
toinstall <- setdiff(pca_pkgs, rownames(installed.packages()))
if (length(toinstall)) {
  message("Installing additional PCA packages...")
  install.packages(toinstall)
  lapply(toinstall, library, character.only = TRUE)
}

# 6.1 Define indicator columns for PCA analysis
# Use same variables as defined in Part 5 for consistency
indicator_cols <- c(
  "Internet Penetration Index", "Mobile Connectivity Index",
  "Trade to GDP Ratio Index",  "Logistics Performance Index",
  "Modern Renewables Share Index", "CO₂ Intensity Index",
  "Business Ready Index",        "Political Stability Index"
)

# 6.2 Prepare data matrix and handle missing values using PCA imputation
# This method is superior to mean imputation as it preserves correlation structure
message("6.2 Preparing data for PCA and imputing missing values...")
dt_mat <- final_dt %>% select(all_of(indicator_cols))

# Impute missing values using iterative PCA algorithm
impute_res  <- missMDA::imputePCA(dt_mat, ncp = 2) # Use 2 components for imputation
dt_complete <- impute_res$completeObs

message(paste("Missing data imputation completed at:", Sys.time()))

# 6.3 Run Principal Component Analysis
# Use scale.unit = FALSE since data is already normalized to 0-1 scale
message("6.3 Running Principal Component Analysis...")
pca_res <- FactoMineR::PCA(dt_complete, scale.unit = FALSE, graph = FALSE)

# 6.4 Compute composite PCA score from first principal component
# PC1 captures the most variance and represents overall GVC readiness
pc1_loadings <- abs(pca_res$var$coord[, 1])  # Take absolute values for positive weights
weights      <- pc1_loadings / sum(pc1_loadings)  # Normalize weights to sum to 1
scores       <- as.matrix(dt_complete) %*% weights  # Compute weighted composite score

# Add PCA scores to final dataset with regional assignments
final_dt <- final_dt %>%
  assign_region() %>%
  mutate(GVC_Readiness_PCA_Score = round(as.numeric(scores), 3)) %>%
  select(Country, Region, all_of(indicator_cols), GVC_Readiness_PCA_Score)

message(paste("PCA analysis completed at:", Sys.time()))

# 6.5 Export PCA Scree Plot
# Shows variance explained by each principal component
message("6.5 Creating and exporting scree plot...")
dp_scree <- factoextra::fviz_eig(pca_res, addlabels = TRUE) +
  ggtitle("Figure 3. Scree Plot: Variance Explained")

ggsave(
  filename = file.path(params$png_dir, "Figure_3_PCA_Scree_Plot.png"),
  plot     = dp_scree,
  width    = 6,
  height   = 4,
  dpi      = 300
)
message("Scree plot saved: Figure_3_PCA_Scree_Plot.png")

# 6.6 Export PCA Variable Loadings Plot
# Shows how each indicator contributes to the principal components
message("6.6 Creating and exporting variable loadings plot...")
dp_loadings <- factoextra::fviz_pca_var(
  pca_res,
  col.var       = "contrib",
  gradient.cols = c("grey80", "orange", "steelblue"),
  repel         = TRUE
) + ggtitle("Figure 4. PCA Variable Contributions")

ggsave(
  filename = file.path(params$png_dir, "Figure_4_PCA_Loadings.png"),
  plot     = dp_loadings,
  width    = 6,
  height   = 6,
  dpi      = 300
)
message("Loadings plot saved: Figure_4_PCA_Loadings.png")

# 6.7 Export PCA Scores Dataset
message("6.7 Exporting PCA scores...")
readr::write_csv(
  final_dt %>% select(Country, Region, GVC_Readiness_PCA_Score),
  file.path(params$csv_dir, "Annex_B_GVC_PCA_Scores_2025.csv")
)
message("PCA scores exported: Annex_B_GVC_PCA_Scores_2025.csv")

# ============================================================
# PART 7: Rankings Exports
# Purpose: Compute global, regional group, and individual regional rankings
# Author: Anthony S. Cano Moncada | Date: May 2025
# Updated: 2025-05-30 01:42:34 UTC
# ============================================================

message("PART 7: Rankings Exports")

# 7.1 Global GVC Readiness Ranking (all countries worldwide)
# Ranks all countries from highest to lowest PCA composite score
message("7.1 Creating global country rankings...")
global_rank <- final_dt %>%
  dplyr::arrange(dplyr::desc(GVC_Readiness_PCA_Score)) %>%
  dplyr::mutate(Global_Rank = dplyr::row_number())

# Export global rankings in multiple formats
openxlsx::write.xlsx(
  global_rank,
  file.path(params$output_dir, "Global_GVC_Ranking_227.xlsx"),
  overwrite = TRUE
)
readr::write_csv(
  global_rank,
  file.path(params$csv_dir, "Global_GVC_Ranking_227.csv")
)
message("7.1 Global ranking exported.")

# 7.2 Group of 135 Countries Ranking
# Subset ranking for countries in defined regional groups
message("7.2 Creating 135-country group rankings...")
group135 <- final_dt %>%
  dplyr::filter(Region %in% params$regions) %>%
  dplyr::arrange(dplyr::desc(GVC_Readiness_PCA_Score)) %>%
  dplyr::mutate(Rank_135 = dplyr::row_number())

openxlsx::write.xlsx(
  group135,
  file.path(params$output_dir, "GVC_Ranking_135.xlsx"),
  overwrite = TRUE
)
readr::write_csv(
  group135,
  file.path(params$csv_dir, "GVC_Ranking_135.csv")
)
message("7.2 135-country group ranking exported.")

# 7.3 Individual Regional Rankings
# Create separate rankings for each regional group
message("7.3 Creating individual regional rankings...")
for (reg in params$regions) {
  reg_df <- final_dt %>%
    dplyr::filter(Region == reg) %>%
    dplyr::arrange(dplyr::desc(GVC_Readiness_PCA_Score)) %>%
    dplyr::mutate(Regional_Rank = dplyr::row_number())
  
  # Define output file paths
  file_xlsx <- file.path(params$output_dir,
                         paste0("GVC_Ranking_", reg, ".xlsx"))
  file_csv  <- file.path(params$csv_dir,
                         paste0("GVC_Ranking_", reg, ".csv"))
  
  # Export regional rankings
  openxlsx::write.xlsx(reg_df, file_xlsx, overwrite = TRUE)
  readr::write_csv(reg_df, file_csv)
  message(paste0("7.3 Regional ranking exported for region: ", reg))
}

message("PART 7 complete: All rankings exported.")

# ============================================================
# PART 8: Session Info
# Purpose: Save comprehensive session information for reproducibility
# Author: Anthony S. Cano Moncada | Date: May 2025
# Updated: 2025-05-30 01:42:34 UTC
# ============================================================

message("PART 8: Session Info")

# Define path for session information output
session_file <- file.path(params$output_dir, "Session_Info.txt")

# Write comprehensive session metadata
cat(
  "Session generated on: ", as.character(Sys.time()), "\n\n",
  file = session_file
)
sink(session_file, append = TRUE)
sessioninfo::session_info()
sink()

message("PART 8 complete: Session info saved to Session_Info.txt")

# ============================================================
# PART 9: Package Snapshot
# Purpose: Save detailed package version information for exact reproducibility
# Author: Anthony S. Cano Moncada | Date: May 2025
# Updated: 2025-05-30 01:42:34 UTC
# ============================================================

message("PART 9: Package Snapshot")

# Define output path for comprehensive package snapshot
snapshot_file <- file.path(params$output_dir, "Packages_Snapshot.txt")

# Write detailed package version and dependency information
sink(snapshot_file)
sessioninfo::package_info()
sink()

message("PART 9 complete: Package snapshot saved to Packages_Snapshot.txt")

# ============================================================
# PART 10: Extended Exports
# Purpose: Create comprehensive multi-sheet workbook with all analysis results
# Author: Anthony S. Cano Moncada | Date: May 2025
# Updated: 2025-05-30 01:42:34 UTC
# ============================================================

message("PART 10: Extended Exports")

# Create comprehensive workbook containing all analysis results
wb <- openxlsx::createWorkbook()

# Add worksheets with data
openxlsx::addWorksheet(wb, "Full Summary")
openxlsx::writeData(wb, "Full Summary", full_tbl)

openxlsx::addWorksheet(wb, "135 Countries")
openxlsx::writeData(wb, "135 Countries", subset_135)

openxlsx::addWorksheet(wb, "PCA Scores")
openxlsx::writeData(wb, "PCA Scores", final_dt %>% select(Country, Region, GVC_Readiness_PCA_Score))

openxlsx::addWorksheet(wb, "Global Rank")
openxlsx::writeData(wb, "Global Rank", global_rank)

openxlsx::addWorksheet(wb, "Group135")
openxlsx::writeData(wb, "Group135", group135)

# Add regional ranking sheets
for (reg in params$regions) {
  sheet <- paste0(reg, "_Ranking")
  openxlsx::addWorksheet(wb, sheet)
  df <- final_dt %>% 
    filter(Region == reg) %>% 
    arrange(desc(GVC_Readiness_PCA_Score)) %>% 
    mutate(Rank = row_number())
  openxlsx::writeData(wb, sheet, df)
}

# Add regional statistics
regional_stats <- final_dt %>%
  filter(Region %in% params$regions) %>%
  pivot_longer(cols = ends_with("Index"), names_to = "Indicator", values_to = "Value") %>%
  group_by(Region, Indicator) %>%
  summarise(Mean = round(mean(Value, na.rm = TRUE), 3),
            Median = round(median(Value, na.rm = TRUE), 3),
            Min = round(min(Value, na.rm = TRUE), 3),
            Max = round(max(Value, na.rm = TRUE), 3), .groups = "drop") %>%
  pivot_wider(names_from = Indicator, values_from = c(Mean, Median, Min, Max))

openxlsx::addWorksheet(wb, "Regional Stats")
openxlsx::writeData(wb, "Regional Stats", regional_stats)

# Save comprehensive workbook
openxlsx::saveWorkbook(wb, file.path(params$output_dir, "GVC_All_Exports_2025.xlsx"), overwrite = TRUE)

message("PART 10 complete. Pipeline finished successfully!")

# ============================================================
# FINAL ANALYSIS COMPLETION & SUMMARY
# Purpose: Provide comprehensive completion summary and final status report
# Author: Anthony S. Cano Moncada | Date: May 2025
# Updated: 2025-05-30 01:42:34 UTC
# ============================================================

message("\nCOMPLETE READY AFRICA ANALYSIS FINISHED!")
message(paste(rep("=", 60), collapse = ""))
message("Analysis completed on:", as.character(Sys.time()))
message("Analysis completed by: Canomoncada")
message("Analysis started: 2025-05-30 01:42:34 UTC")
message("Project root:", project_root)

message("\nCOMPREHENSIVE OUTPUTS GENERATED:")
message("   Excel Files (Primary Outputs):")
message("      • Annex_A_Full_Summary_2025.xlsx - Complete country dataset")
message("      • Annex_A_135_Countries_2025.xlsx - Regional subset analysis")
message("      • Annex_A_Rankings_2025.xlsx - Individual indicator rankings")
message("      • Annex_A_Summary_Stats_Indicators_2025.xlsx - Descriptive statistics")
message("      • Global_GVC_Ranking_227.xlsx - Worldwide country rankings")
message("      • GVC_Ranking_135.xlsx - Regional group rankings")
message(paste("      • GVC_Ranking_[Region].xlsx - Individual regional rankings (", length(params$regions), " files)", sep=""))
message("      • GVC_All_Exports_2025.xlsx - Comprehensive multi-sheet workbook")

message("\n   CSV Files (Alternative Format):")
message("      • All Excel files exported in CSV format for broader compatibility")
message("      • Annex_B_GVC_PCA_Scores_2025.csv - PCA composite scores")

message("\n   Visualizations (High-Resolution PNG):")
message("      • Figure_Regional_Boxplots_2025.png - Regional indicator comparison")
message("      • Figure_3_PCA_Scree_Plot.png - Variance explained by components")
message("      • Figure_4_PCA_Loadings.png - Variable contributions to PCA")

message("\n   Documentation & Reproducibility:")
message("      • Session_Info.txt - Complete R session details")
message("      • Packages_Snapshot.txt - Package versions and dependencies")
message("      • Part5_Exports.zip - Compressed archive of Excel files")

message("\nFINAL DATASET CHARACTERISTICS:")
message("   • Total countries analyzed:", nrow(final_dt))
message("   • Indicators processed:", length(indicator_cols))
message("   • Regional groups:", length(params$regions), "(", paste(params$regions, collapse = ", "), ")")
message("   • Countries in regional analysis:", nrow(group135))
message("   • PCA components retained: 8")
message("   • Missing data: Handled via iterative PCA imputation")
message("   • Normalization: Min-max scaling (0-1) applied to all indicators")

message("\nKEY ANALYTICAL RESULTS:")
message("   • Global country rankings: 1 to", nrow(final_dt))
message("   • Regional group rankings: 1 to", nrow(group135))
message(paste("   • Individual regional rankings: ", paste(params$regions, collapse = ", ")))
message("   • Comprehensive indicator analysis: 8 dimensions")
message("   • PCA-based composite scores: Weighted by component loadings")
message("   • Regional statistical comparisons: Mean, median, range by region")

# Display final dataset structure and preview
message("\nFINAL DATASET STRUCTURE:")
str(final_dt)

message("\nFINAL DATASET PREVIEW (Top 10 Countries by PCA Score):")
print(
  final_dt %>% 
    arrange(desc(GVC_Readiness_PCA_Score)) %>% 
    head(10) %>%
    select(Country, Region, GVC_Readiness_PCA_Score)
)

message("\nAll output files saved to:", params$output_dir)
message(paste(rep("=", 60), collapse = ""))
message("Ready Africa Digital Readiness Assessment Complete!")
message("Contact: Canomoncada | Date: 2025-05-30 01:42:34 UTC")
message(paste(rep("=", 60), collapse = ""))

# Final system information
message("\nSYSTEM INFORMATION:")
message("   R Version:", R.version.string)
message("   Platform:", R.version$platform)
message("   OS:", Sys.info()["sysname"])
message("   Analysis completed at:", as.character(Sys.time()))

# End of analysis pipeline
message("\nANALYSIS PIPELINE EXECUTION COMPLETE")