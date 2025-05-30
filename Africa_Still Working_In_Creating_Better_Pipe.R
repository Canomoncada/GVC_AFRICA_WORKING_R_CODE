# =====================================================================
# AFRICA GVC READINESS - COMPREHENSIVE ANALYSIS PIPELINE - Creating Better Pipe
# Author: Anthony S. Cano Moncada
# Version: 3.0 - Professional Production Release
# Date: 2025-05-30
# User: Canomoncada
# =====================================================================

# SECTION 1: PIPELINE INITIALIZATION AND SETUP
# =====================================================================

#' Initialize GVC Analysis Pipeline
#' @description Complete setup with all required packages and configurations
#' @param project_root Character path to project directory
#' @param create_backup Logical whether to backup existing outputs
#' @return Configuration object for pipeline execution

initialize_gvc_pipeline <- function(project_root = "~/Downloads/Ready Africa",
                                   create_backup = TRUE) {
  
  start_time <- Sys.time()
  
  # Essential packages for initialization
  essential_pkgs <- c("here", "fs")
  for (pkg in essential_pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg, quiet = TRUE)
    }
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  }
  
  cat("Africa GVC Readiness Pipeline v4.0\n")
  cat("Initializing comprehensive analysis pipeline...\n")
  cat("User: Canomoncada | Time:", as.character(Sys.time()), "\n")
  
  # Enhanced directory structure
  dirs <- list(
    root         = project_root,
    data         = file.path(project_root, "Data"),
    exports      = file.path(project_root, "Exports"),
    enhanced     = file.path(project_root, "Exports", "Enhanced_Exports"),
    visuals      = file.path(project_root, "Exports", "Enhanced_Exports", "Visuals"),
    excel        = file.path(project_root, "Exports", "Enhanced_Exports", "Excel"),
    csv          = file.path(project_root, "Exports", "CSVs"),
    boxplots     = file.path(project_root, "Exports", "Enhanced_Exports", "Visuals", "Boxplots"),
    ridges       = file.path(project_root, "Exports", "Enhanced_Exports", "Visuals", "Ridges"),
    heatmaps     = file.path(project_root, "Exports", "Enhanced_Exports", "Visuals", "Heatmaps"),
    pca          = file.path(project_root, "Exports", "Enhanced_Exports", "Visuals", "PCA"),
    correlation  = file.path(project_root, "Exports", "Enhanced_Exports", "Visuals", "Correlation"),
    scatterplots = file.path(project_root, "Exports", "Enhanced_Exports", "Visuals", "ScatterPairs"),
    radar        = file.path(project_root, "Exports", "Enhanced_Exports", "Visuals", "RadarCharts"),
    violin       = file.path(project_root, "Exports", "Enhanced_Exports", "Visuals", "Violin"),
    regional     = file.path(project_root, "Exports", "Enhanced_Exports", "Visuals", "Regional_Bars"),
    logs         = file.path(project_root, "Logs"),
    backup       = file.path(project_root, "Backup", format(Sys.time(), "%Y%m%d_%H%M%S"))
  )
  
  # Create backup if requested
  if (create_backup && dir.exists(dirs$exports)) {
    cat("Creating backup of existing outputs...\n")
    dir.create(dirs$backup, recursive = TRUE, showWarnings = FALSE)
    if (length(list.files(dirs$exports, recursive = TRUE)) > 0) {
      file.copy(list.files(dirs$exports, full.names = TRUE, recursive = TRUE), 
                dirs$backup, recursive = TRUE, overwrite = TRUE)
      cat("Backup created:", dirs$backup, "\n")
    }
  }
  
  # Create all directories
  for (dir_path in dirs) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  }
  cat("Directory structure created:", length(dirs), "directories\n")
  
  # Comprehensive package management
  required_packages <- c(
    # Core data manipulation
    "tidyverse", "dplyr", "readr", "readxl", "openxlsx", "janitor",
    
    # Statistical analysis
    "FactoMineR", "factoextra", "corrplot", "broom", "psych",
    
    # Visualization - basic
    "ggplot2", "patchwork", "ggrepel", "scales", "RColorBrewer", "viridis",
    
    # Visualization - advanced
    "ggridges", "ggbeeswarm", "GGally", "fmsb", "pheatmap", "ggsci",
    
    # Utilities
    "zip"
  )
  
  # Install missing packages
  missing_pkgs <- setdiff(required_packages, rownames(installed.packages()))
  
  if (length(missing_pkgs) > 0) {
    cat("Installing", length(missing_pkgs), "missing packages...\n")
    install.packages(missing_pkgs, dependencies = TRUE, quiet = TRUE)
  }
  
  # Load packages with error handling
  for (pkg in required_packages) {
    tryCatch({
      suppressPackageStartupMessages(library(pkg, character.only = TRUE))
    }, error = function(e) {
      cat("Warning: Failed to load", pkg, "\n")
    })
  }
  
  # Global options
  options(
    scipen = 999,
    dplyr.summarise.inform = FALSE,
    readr.show_col_types = FALSE,
    warn = 1,
    stringsAsFactors = FALSE
  )
  
  # Enhanced ggplot2 theme
  theme_gvc <- function(base_size = 11) {
    theme_minimal(base_size = base_size) +
      theme(
        plot.title = element_text(size = rel(1.4), face = "bold", margin = margin(b = 20)),
        plot.subtitle = element_text(size = rel(1.1), color = "grey60", margin = margin(b = 20)),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey95", size = 0.3),
        legend.position = "bottom",
        strip.text = element_text(face = "bold", size = rel(1.1)),
        axis.title = element_text(size = rel(1.1))
      )
  }
  
  theme_set(theme_gvc())
  
  # Color palettes
  colors <- list(
    region_colors = c(
      "Africa" = "#2E8B57", "ASEAN" = "#4682B4", 
      "LAC" = "#CD853F", "OECD" = "#8B4513", "Other" = "#696969"
    ),
    blue_palette = colorRampPalette(brewer.pal(9, "Blues")),
    nature = ggsci::pal_npg("nrc")(10)
  )
  
  # Configuration object
  config <- list(
    timestamp = start_time,
    user = "Canomoncada",
    version = "4.0",
    dirs = dirs,
    colors = colors,
    indicators = c(
      "Internet Penetration Index", "Mobile Connectivity Index",
      "Trade to GDP Ratio Index", "Logistics Performance Index", 
      "Modern Renewables Share Index", "CO₂ Intensity Index",
      "Business Ready Index", "Political Stability Index"
    ),
    indicator_groups = list(
      technology = c("Internet Penetration Index", "Mobile Connectivity Index"),
      trade_logistics = c("Trade to GDP Ratio Index", "Logistics Performance Index"),
      sustainability = c("Modern Renewables Share Index", "CO₂ Intensity Index"),
      governance = c("Business Ready Index", "Political Stability Index")
    )
  )
  
  # Save configuration
  saveRDS(config, file.path(dirs$logs, "pipeline_config.rds"))
  
  cat("Pipeline initialization completed in", 
      round(difftime(Sys.time(), start_time, units = 'secs'), 2), "seconds\n")
  
  return(config)
}

# SECTION 2: DATA LOADING AND PREPARATION
# =====================================================================

#' Load and prepare GVC data
#' @param config Configuration object from initialization
#' @param data_file Path to main data file
#' @return Cleaned and prepared data frame

load_gvc_data <- function(config, data_file = NULL) {
  
  cat("Loading and preparing GVC data...\n")
  
  # Default data file paths
  if (is.null(data_file)) {
    possible_files <- c(
      file.path(config$dirs$data, "Annex_A_135_Countries_2025.csv"),
      file.path(config$dirs$data, "Final_Normalized_Indicators_2025.csv"),
      file.path(config$dirs$root, "Data Load", "Annex_A_135_Countries_2025.csv")
    )
    
    data_file <- Find(file.exists, possible_files)
    
    if (is.null(data_file)) {
      stop("No valid data file found. Please specify data_file parameter.")
    }
  }
  
  # Load data
  df <- read_csv(data_file, show_col_types = FALSE)
  
  # Clean column names
  df <- janitor::clean_names(df)
  
  # Standardize column names if needed
  expected_cols <- c("country", "region", config$indicators)
  expected_cols_clean <- janitor::make_clean_names(expected_cols)
  
  # Check for required columns
  missing_cols <- setdiff(expected_cols_clean, names(df))
  if (length(missing_cols) > 0) {
    cat("Warning: Missing columns:", paste(missing_cols, collapse = ", "), "\n")
  }
  
  # Ensure numeric indicators
  indicator_cols <- intersect(janitor::make_clean_names(config$indicators), names(df))
  df[indicator_cols] <- lapply(df[indicator_cols], as.numeric)
  
  # Remove rows with all NA values for indicators
  df <- df[rowSums(!is.na(df[indicator_cols])) > 0, ]
  
  cat("Data loaded successfully:", nrow(df), "countries,", length(indicator_cols), "indicators\n")
  
  return(df)
}

# SECTION 3: PCA ANALYSIS
# =====================================================================

#' Perform comprehensive PCA analysis
#' @param df Data frame with GVC indicators
#' @param config Configuration object
#' @return List containing PCA results and visualizations

run_pca_analysis <- function(df, config) {
  
  cat("Running PCA analysis...\n")
  
  # Prepare data for PCA
  indicator_cols <- intersect(janitor::make_clean_names(config$indicators), names(df))
  pca_data <- df[complete.cases(df[indicator_cols]), ]
  
  if (nrow(pca_data) < 10) {
    stop("Insufficient complete cases for PCA analysis")
  }
  
  # Perform PCA
  res_pca <- PCA(pca_data[indicator_cols], graph = FALSE, scale.unit = TRUE)
  
  # Combine PCA results with metadata
  pca_coords <- as.data.frame(res_pca$ind$coord)
  pca_df <- bind_cols(
    pca_data %>% select(any_of(c("country", "region"))), 
    pca_coords
  )
  
  # PCA Scatterplot
  pca_plot <- ggplot(pca_df, aes(x = Dim.1, y = Dim.2, color = region)) +
    geom_point(size = 3, alpha = 0.9) +
    geom_text_repel(aes(label = country), size = 2.5, max.overlaps = 20) +
    scale_color_manual(values = config$colors$region_colors) +
    labs(
      title = "GVC Readiness PCA Component Space",
      x = "Principal Component 1 (PC1)",
      y = "Principal Component 2 (PC2)",
      color = "Region"
    ) +
    theme_gvc()
  
  ggsave(
    filename = file.path(config$dirs$pca, "PCA_Component_Scatter.png"),
    plot = pca_plot,
    width = 10, height = 6, dpi = 300
  )
  
  # Scree Plot
  scree_plot <- fviz_eig(res_pca, addlabels = TRUE, barfill = "#4E79A7", barcolor = "black") +
    labs(
      title = "Explained Variance by PCA Dimensions",
      x = "Principal Components",
      y = "Percentage of Variance"
    ) +
    theme_gvc()
  
  ggsave(
    filename = file.path(config$dirs$pca, "PCA_Scree_Plot.png"),
    plot = scree_plot,
    width = 8, height = 5, dpi = 300
  )
  
  # Variable Contributions
  contrib_plot <- fviz_pca_var(
    res_pca,
    col.var = "contrib",
    gradient.cols = c("lightblue", "blue", "darkblue"),
    repel = TRUE
  ) +
    labs(title = "Indicator Contributions to Principal Components") +
    theme_gvc()
  
  ggsave(
    filename = file.path(config$dirs$pca, "PCA_Loadings_Contribution.png"),
    plot = contrib_plot,
    width = 8, height = 5, dpi = 300
  )
  
  cat("PCA analysis completed\n")
  
  return(list(res_pca = res_pca, pca_df = pca_df))
}

# SECTION 4: PCA RANKINGS
# =====================================================================

#' Generate PCA-based rankings
#' @param pca_df Data frame with PCA coordinates
#' @param config Configuration object
#' @return Data frame with global rankings

generate_pca_rankings <- function(pca_df, config) {
  
  cat("Generating PCA rankings...\n")
  
  # Global ranking by PC1
  global_ranking <- pca_df %>%
    arrange(desc(Dim.1)) %>%
    mutate(Global_Rank = row_number())
  
  # Regional rankings
  regional_rankings <- global_ranking %>%
    group_by(region) %>%
    arrange(desc(Dim.1)) %>%
    mutate(Regional_Rank = row_number()) %>%
    ungroup()
  
  # Export to Excel
  wb <- createWorkbook()
  
  addWorksheet(wb, "Global Rankings")
  writeData(wb, "Global Rankings", global_ranking)
  
  # Add regional sheets
  for (reg in unique(regional_rankings$region)) {
    if (!is.na(reg)) {
      reg_data <- regional_rankings %>% filter(region == reg)
      addWorksheet(wb, reg)
      writeData(wb, reg, reg_data)
    }
  }
  
  saveWorkbook(wb, file.path(config$dirs$excel, "PCA_Global_Regional_Rankings.xlsx"), 
               overwrite = TRUE)
  
  cat("PCA rankings generated\n")
  
  return(global_ranking)
}

# SECTION 5: CORRELATION ANALYSIS
# =====================================================================

#' Generate correlation heatmaps
#' @param df Data frame with GVC indicators
#' @param config Configuration object

generate_correlation_heatmap <- function(df, config) {
  
  cat("Generating correlation heatmaps...\n")
  
  # Prepare correlation matrix
  indicator_cols <- intersect(janitor::make_clean_names(config$indicators), names(df))
  cor_data <- df[indicator_cols] %>% 
    select_if(is.numeric) %>%
    drop_na()
  
  if (ncol(cor_data) < 2) {
    cat("Warning: Insufficient numeric columns for correlation analysis\n")
    return(NULL)
  }
  
  cor_matrix <- cor(cor_data, use = "pairwise.complete.obs")
  
  # Standard correlation plot
  col_palette <- config$colors$blue_palette(200)
  
  png(file.path(config$dirs$correlation, "Correlation_Heatmap_GVC_Indicators.png"), 
      width = 1200, height = 1000, res = 150)
  corrplot(cor_matrix,
           method = "color",
           col = col_palette,
           type = "upper",
           order = "hclust",
           addCoef.col = "black",
           tl.col = "black",
           tl.srt = 45,
           number.cex = 0.7,
           title = "Correlation Heatmap of GVC Readiness Indicators",
           mar = c(0, 0, 2, 0))
  dev.off()
  
  # Clustered heatmap
  pheatmap(
    mat = cor_matrix,
    color = config$colors$blue_palette(100),
    display_numbers = TRUE,
    number_color = "black",
    main = "Correlation & Clustering of GVC Indicators",
    fontsize = 11,
    cluster_rows = TRUE,
    cluster_cols = TRUE,
    filename = file.path(config$dirs$correlation, "Correlation_Clustering_Heatmap.png"),
    width = 8,
    height = 6
  )
  
  cat("Correlation heatmaps generated\n")
}

# SECTION 6: VIOLIN PLOTS
# =====================================================================

#' Generate violin plots for all indicators
#' @param df Data frame with GVC indicators
#' @param config Configuration object

generate_violin_plots <- function(df, config) {
  
  cat("Generating violin plots...\n")
  
  indicator_cols <- intersect(janitor::make_clean_names(config$indicators), names(df))
  
  for (indicator in indicator_cols) {
    if (sum(!is.na(df[[indicator]])) < 5) next  # Skip if too few observations
    
    plot <- ggplot(df, aes(x = region, y = .data[[indicator]], fill = region)) +
      geom_violin(trim = FALSE, alpha = 0.6, color = "gray30") +
      geom_beeswarm(color = "blue", size = 1.8, alpha = 0.7) +
      scale_fill_manual(values = config$colors$region_colors) +
      labs(
        title = paste("Distribution of", indicator, "by Region"),
        x = NULL,
        y = indicator
      ) +
      theme_gvc() +
      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 20, hjust = 1)
      )
    
    ggsave(
      filename = file.path(config$dirs$violin, paste0("Violin_", gsub(" ", "_", indicator), ".png")),
      plot = plot,
      width = 8, height = 6, dpi = 300
    )
  }
  
  cat("Violin plots generated\n")
}

# SECTION 7: PILLAR-BASED HEATMAPS
# =====================================================================

#' Generate heatmaps by indicator pillars
#' @param df Data frame with GVC indicators
#' @param config Configuration object

generate_pillar_heatmaps <- function(df, config) {
  
  cat("Generating pillar heatmaps...\n")
  
  blue_palette <- config$colors$blue_palette(100)
  
  # Helper function for creating heatmaps
  plot_heatmap <- function(data, indicators, title, filename) {
    indicator_cols <- intersect(janitor::make_clean_names(indicators), names(data))
    
    if (length(indicator_cols) == 0) {
      cat("Warning: No valid indicators for", title, "\n")
      return(NULL)
    }
    
    mat <- data %>%
      select(any_of(c("country", indicator_cols))) %>%
      drop_na() %>%
      column_to_rownames(var = names(.)[1]) %>%
      as.matrix()
    
    if (nrow(mat) < 2 || ncol(mat) < 1) {
      cat("Warning: Insufficient data for", title, "\n")
      return(NULL)
    }
    
    pheatmap(mat,
             cluster_rows = TRUE,
             cluster_cols = ifelse(ncol(mat) > 1, TRUE, FALSE),
             color = blue_palette,
             fontsize_row = 7,
             main = title,
             angle_col = 45,
             filename = filename,
             width = 9, height = 8)
  }
  
  # Generate heatmaps for each pillar
  for (pillar_name in names(config$indicator_groups)) {
    indicators <- config$indicator_groups[[pillar_name]]
    title <- paste(tools::toTitleCase(gsub("_", " ", pillar_name)), "Readiness Heatmap")
    filename <- file.path(config$dirs$heatmaps, paste0(tools::toTitleCase(pillar_name), "_Heatmap.png"))
    
    plot_heatmap(df, indicators, title, filename)
  }
  
  cat("Pillar heatmaps generated\n")
}

# SECTION 8: BOXPLOTS
# =====================================================================

#' Generate comprehensive boxplots
#' @param df Data frame with GVC indicators
#' @param config Configuration object

generate_boxplots <- function(df, config) {
  
  cat("Generating boxplots...\n")
  
  indicator_cols <- intersect(janitor::make_clean_names(config$indicators), names(df))
  
  # Individual boxplots
  for (var in indicator_cols) {
    if (sum(!is.na(df[[var]])) < 5) next
    
    p <- ggplot(df, aes(x = region, y = .data[[var]], fill = region)) +
      geom_boxplot(color = "black", outlier.color = "gray40", width = 0.7, alpha = 0.85) +
      scale_fill_manual(values = config$colors$region_colors) +
      labs(
        title = paste(var, "by Region"),
        x = NULL,
        y = var
      ) +
      theme_gvc() +
      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 30, hjust = 1)
      )
    
    filename <- paste0(gsub(" ", "_", var), "_Boxplot.png")
    ggsave(file.path(config$dirs$boxplots, filename), p, width = 8, height = 5.5, dpi = 300)
  }
  
  # Composite boxplot
  df_long <- df %>%
    select(any_of(c("country", "region", indicator_cols))) %>%
    pivot_longer(cols = all_of(indicator_cols), names_to = "indicator", values_to = "value") %>%
    filter(!is.na(value))
  
  if (nrow(df_long) > 0) {
    composite_plot <- ggplot(df_long, aes(x = region, y = value, fill = region)) +
      geom_boxplot(outlier.shape = NA, alpha = 0.9) +
      geom_jitter(width = 0.2, alpha = 0.3, color = "gray30") +
      facet_wrap(~ indicator, scales = "free_y", ncol = 2) +
      scale_fill_manual(values = config$colors$region_colors) +
      labs(
        title = "GVC Indicator Distributions by Region",
        x = NULL,
        y = "Value"
      ) +
      theme_gvc() +
      theme(
        legend.position = "none",
        strip.text = element_text(face = "bold", size = 11)
      )
    
    ggsave(
      file.path(config$dirs$boxplots, "Composite_GVC_Boxplots.png"),
      composite_plot,
      width = 12, height = 10, dpi = 300
    )
  }
  
  cat("Boxplots generated\n")
}

# SECTION 9: REGIONAL AVERAGE BARPLOTS
# =====================================================================

#' Generate regional average barplots
#' @param df Data frame with GVC indicators
#' @param config Configuration object

generate_regional_barplots <- function(df, config) {
  
  cat("Generating regional average barplots...\n")
  
  indicator_cols <- intersect(janitor::make_clean_names(config$indicators), names(df))
  
  # Calculate regional means
  regional_means <- df %>%
    select(any_of(c("region", indicator_cols))) %>%
    group_by(region) %>%
    summarise(across(everything(), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
    pivot_longer(-region, names_to = "indicator", values_to = "score") %>%
    filter(!is.na(score), !is.na(region))
  
  if (nrow(regional_means) == 0) {
    cat("Warning: No data available for regional barplots\n")
    return(NULL)
  }
  
  # Generate individual barplots
  for (ind in unique(regional_means$indicator)) {
    plot_data <- regional_means %>% filter(indicator == ind)
    
    if (nrow(plot_data) < 2) next
    
    p <- plot_data %>%
      ggplot(aes(x = reorder(region, score), y = score, fill = region)) +
      geom_col(show.legend = FALSE, alpha = 0.85) +
      scale_fill_manual(values = config$colors$region_colors) +
      coord_flip() +
      labs(
        title = paste("Regional Average:", ind),
        x = NULL,
        y = "Average Score"
      ) +
      theme_gvc()
    
    filename <- paste0(gsub(" ", "_", ind), "_Regional_Barplot.png")
    ggsave(file.path(config$dirs$regional, filename), p, width = 8, height = 5, dpi = 300)
  }
  
  cat("Regional average barplots generated\n")
}

# SECTION 10: PAIRWISE SCATTERPLOTS
# =====================================================================

#' Generate pairwise scatterplots
#' @param df Data frame with GVC indicators
#' @param config Configuration object

generate_pairwise_scatterplots <- function(df, config) {
  
  cat("Generating pairwise scatterplots...\n")
  
  indicator_cols <- intersect(janitor::make_clean_names(config$indicators), names(df))
  
  # Select subset of indicators for pairwise plotting
  selected_vars <- head(indicator_cols, 6)  # Limit to 6 to avoid overcrowding
  
  # Prepare data
  df_subset <- df %>%
    select(any_of(c("country", "region", selected_vars))) %>%
    filter(!is.na(region)) %>%
    drop_na()
  
  if (nrow(df_subset) < 10 || length(selected_vars) < 2) {
    cat("Warning: Insufficient data for pairwise scatterplots\n")
    return(NULL)
  }
  
  # Create pairwise plot
  tryCatch({
    png(file.path(config$dirs$scatterplots, "GVC_Pairwise_Scatterplots_by_Region.png"), 
        width = 1800, height = 1600, res = 200)
    
    pairs_plot <- GGally::ggpairs(
      data = df_subset,
      columns = which(names(df_subset) %in% selected_vars),
      mapping = aes(color = region),
      lower = list(continuous = wrap("smooth", method = "lm", se = FALSE, alpha = 0.3)),
      upper = list(continuous = wrap("cor", size = 3)),
      title = "Pairwise GVC Indicator Relationships by Region"
    )
    
    print(pairs_plot)
    dev.off()
    
  }, error = function(e) {
    cat("Warning: Could not generate pairwise scatterplots:", e$message, "\n")
    if (dev.cur() > 1) dev.off()
  })
  
  cat("Pairwise scatterplots generated\n")
}

# SECTION 11: RADAR CHARTS
# =====================================================================

#' Generate radar charts for selected countries
#' @param df Data frame with GVC indicators
#' @param config Configuration object

generate_radar_charts <- function(df, config) {
  
  cat("Generating radar charts...\n")
  
  indicator_cols <- intersect(janitor::make_clean_names(config$indicators), names(df))
  
  # Normalize indicators to 0-1 scale
  df_scaled <- df %>%
    mutate(across(all_of(indicator_cols), ~scales::rescale(.x, to = c(0, 1), na.rm = TRUE))) %>%
    select(any_of(c("country", "region", indicator_cols)))
  
  # Radar chart function
  plot_radar <- function(country_name) {
    df_country <- df_scaled %>% filter(country == country_name)
    if (nrow(df_country) == 0) return(NULL)
    
    values <- df_country %>% select(all_of(indicator_cols)) %>% as.numeric()
    if (any(is.na(values))) {
      cat("Warning: Skipping", country_name, "due to missing values\n")
      return(NULL)
    }
    
    radar_data <- rbind(
      rep(1, length(values)),  # Max line
      rep(0, length(values)),  # Min line
      values
    )
    colnames(radar_data) <- indicator_cols
    rownames(radar_data) <- c("Max", "Min", country_name)
    
    file_path <- file.path(config$dirs$radar, paste0("Radar_", gsub(" ", "_", country_name), ".png"))
    
    tryCatch({
      png(file_path, width = 800, height = 800)
      fmsb::radarchart(
        radar_data,
        axistype = 1,
        pcol = "blue",
        pfcol = scales::alpha("blue", 0.35),
        plwd = 2,
        title = paste("GVC Readiness Profile:", country_name),
        cglcol = "gray80",
        cglty = 1,
        axislabcol = "black",
        vlcex = 0.85
      )
      dev.off()
    }, error = function(e) {
      cat("Warning: Could not create radar chart for", country_name, "\n")
      if (dev.cur() > 1) dev.off()
    })
  }
  
  # Generate radar charts for example countries
  example_countries <- c("Kenya", "South Africa", "Nigeria", "Ghana", "Rwanda")
  available_countries <- intersect(example_countries, df_scaled$country)
  
  for (country in available_countries) {
    plot_radar(country)
  }
  
  cat("Radar charts generated\n")
}

# SECTION 12: RANKING HEATMAP
# =====================================================================

#' Generate ranking heatmap
#' @param df Data frame with GVC indicators
#' @param config Configuration object

generate_ranking_heatmap <- function(df, config) {
  
  cat("Generating ranking heatmap...\n")
  
  indicator_cols <- intersect(janitor::make_clean_names(config$indicators), names(df))
  
  # Create rankings
  df_ranks <- df %>%
    select(any_of(c("country", indicator_cols))) %>%
    mutate(across(all_of(indicator_cols), 
                  ~rank(-.x, ties.method = "min", na.last = "keep"))) %>%
    drop_na()
  
  if (nrow(df_ranks) < 5) {
    cat("Warning: Insufficient data for ranking heatmap\n")
    return(NULL)
  }
  
  # Prepare matrix
  rank_matrix <- df_ranks %>%
    column_to_rownames("country") %>%
    as.matrix()
  
  # Create heatmap
  tryCatch({
    pheatmap(
      mat = rank_matrix,
      cluster_rows = TRUE,
      cluster_cols = FALSE,
      color = rev(config$colors$blue_palette(100)),
      main = "Country Rankings Across GVC Indicators (1 = Best)",
      angle_col = 45,
      fontsize_row = 7,
      fontsize_col = 10,
      filename = file.path(config$dirs$heatmaps, "GVC_Indicator_Rankings_Heatmap.png"),
      width = 9,
      height = 10
    )
  }, error = function(e) {
    cat("Warning: Could not generate ranking heatmap:", e$message, "\n")
  })
  
  cat("Ranking heatmap generated\n")
}

# SECTION 13: EXCEL EXPORTS
# =====================================================================

#' Export analysis results to Excel
#' @param df Original data frame
#' @param pca_df PCA results data frame
#' @param config Configuration object

export_to_excel <- function(df, pca_df, config) {
  
  cat("Exporting results to Excel...\n")
  
  # Main results workbook
  wb <- createWorkbook()
  
  # Add datasets
  addWorksheet(wb, "Original_Data")
  writeData(wb, "Original_Data", df)
  
  if (!is.null(pca_df)) {
    addWorksheet(wb, "PCA_Coordinates")
    writeData(wb, "PCA_Coordinates", pca_df)
  }
  
  # Add summary statistics
  indicator_cols <- intersect(janitor::make_clean_names(config$indicators), names(df))
  
  if (length(indicator_cols) > 0) {
    summary_stats <- df %>%
      select(all_of(indicator_cols)) %>%
      summarise(across(everything(), list(
        mean = ~mean(.x, na.rm = TRUE),
        median = ~median(.x, na.rm = TRUE),
        sd = ~sd(.x, na.rm = TRUE),
        min = ~min(.x, na.rm = TRUE),
        max = ~max(.x, na.rm = TRUE)
      ))) %>%
      pivot_longer(everything(), names_to = "indicator_stat", values_to = "value") %>%
      separate(indicator_stat, into = c("indicator", "statistic"), sep = "_(?=[^_]+$)") %>%
      pivot_wider(names_from = statistic, values_from = value)
    
    addWorksheet(wb, "Summary_Statistics")
    writeData(wb, "Summary_Statistics", summary_stats)
  }
  
  saveWorkbook(wb, file.path(config$dirs$excel, "GVC_Analysis_Results.xlsx"), overwrite = TRUE)
  
  cat("Excel exports completed\n")
}

# SECTION 14: MASTER EXECUTION FUNCTION
# =====================================================================

#' Run complete GVC analysis pipeline
#' @param project_root Character path to project directory
#' @param data_file Optional path to specific data file
#' @param create_backup Logical whether to backup existing outputs
#' @return List containing all analysis results

run_complete_gvc_analysis <- function(project_root = "~/Downloads/Ready Africa",
                                     data_file = NULL,
                                     create_backup = TRUE) {
  
  cat("=================================================================\n")
  cat("STARTING COMPREHENSIVE AFRICA GVC READINESS ANALYSIS PIPELINE\n")
  cat("=================================================================\n")
  
  # Initialize pipeline
  config <- initialize_gvc_pipeline(project_root, create_backup)
  
  # Load data
  tryCatch({
    df <- load_gvc_data(config, data_file)
  }, error = function(e) {
    cat("Error loading data:", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(df)) {
    cat("Analysis terminated due to data loading error\n")
    return(NULL)
  }
  
  # Initialize results list
  results <- list(
    config = config,
    data = df,
    pca_results = NULL
  )
  
  # Run analysis modules
  analysis_modules <- list(
    "PCA Analysis" = function() {
      pca_results <- run_pca_analysis(df, config)
      if (!is.null(pca_results)) {
        generate_pca_rankings(pca_results$pca_df, config)
        results$pca_results <<- pca_results
      }
      return(pca_results)
    },
    "Correlation Analysis" = function() generate_correlation_heatmap(df, config),
    "Violin Plots" = function() generate_violin_plots(df, config),
    "Pillar Heatmaps" = function() generate_pillar_heatmaps(df, config),
    "Boxplots" = function() generate_boxplots(df, config),
    "Regional Barplots" = function() generate_regional_barplots(df, config),
    "Pairwise Scatterplots" = function() generate_pairwise_scatterplots(df, config),
    "Radar Charts" = function() generate_radar_charts(df, config),
    "Ranking Heatmap" = function() generate_ranking_heatmap(df, config),
    "Excel Export" = function() export_to_excel(df, results$pca_results$pca_df, config)
  )
  
  # Execute each module with error handling
  for (module_name in names(analysis_modules)) {
    cat("\n--- Running:", module_name, "---\n")
    tryCatch({
      analysis_modules[[module_name]]()
      cat("✓", module_name, "completed successfully\n")
    }, error = function(e) {
      cat("✗", module_name, "failed:", e$message, "\n")
    })
  }
  
  # Create summary report
  create_analysis_summary(config)
  
  cat("\n=================================================================\n")
  cat("AFRICA GVC READINESS ANALYSIS PIPELINE COMPLETED\n")
  cat("Results saved to:", config$dirs$enhanced, "\n")
  cat("=================================================================\n")
  
  return(results)
}

# SECTION 15: SUMMARY REPORT
# =====================================================================

#' Create analysis summary report
#' @param config Configuration object

create_analysis_summary <- function(config) {
  
  # Count generated files
  visual_dirs <- c(config$dirs$boxplots, config$dirs$heatmaps, config$dirs$pca,
                   config$dirs$correlation, config$dirs$violin, config$dirs$radar,
                   config$dirs$regional, config$dirs$scatterplots)
  
  file_counts <- map_int(visual_dirs, ~length(list.files(.x, pattern = "\\.(png|jpg|jpeg)$")))
  names(file_counts) <- basename(visual_dirs)
  
  excel_files <- length(list.files(config$dirs$excel, pattern = "\\.xlsx$"))
  
  # Create summary
  summary_text <- paste0(
    "AFRICA GVC READINESS ANALYSIS SUMMARY\n",
    "=====================================\n",
    "Timestamp: ", Sys.time(), "\n",
    "User: ", config$user, "\n",
    "Version: ", config$version, "\n\n",
    "OUTPUTS GENERATED:\n",
    paste(names(file_counts), ":", file_counts, "files", collapse = "\n"), "\n",
    "Excel files: ", excel_files, "\n\n",
    "TOTAL VISUALIZATIONS: ", sum(file_counts), "\n",
    "ANALYSIS COMPLETED SUCCESSFULLY\n"
  )
  
  # Save summary
  writeLines(summary_text, file.path(config$dirs$logs, "analysis_summary.txt"))
  cat(summary_text)
}

# FINAL USAGE EXAMPLE
# =====================================================================

# To run the complete analysis:
# results <- run_complete_gvc_analysis()

# To run with specific parameters:
# results <- run_complete_gvc_analysis(
#   project_root = "~/Downloads/Ready Africa",
#   data_file = "~/Downloads/Ready Africa/Data/your_data_file.csv",
#   create_backup = TRUE
# )
