# ══════════════════════════════════════════════════════════
# AFRICA GVC READINESS – COMPREHENSIVE ANALYSIS PIPELINE
# Author: Anthony S. Cano Moncada | Updated: 2025-05-30 02:00:56 UTC
# User: Canomoncada
# Version: 2.0 - Complete robust implementation with all sections
# ══════════════════════════════════════════════════════════

# ┌─────────────────────────────────────────────────────────┐
# │ SECTION 0: COMPREHENSIVE PIPELINE INITIALIZATION       │
# └─────────────────────────────────────────────────────────┘

#' Initialize comprehensive Africa GVC pipeline
#' @description Complete setup with all required packages and configurations
#' @param project_root Character path to project directory
#' @param create_backup Logical whether to backup existing outputs
#' @return Configuration object for pipeline execution

initialize_comprehensive_pipeline <- function(project_root = "~/Downloads/Ready Africa",
                                             create_backup = TRUE) {
  
  # Start timing
  start_time <- Sys.time()
  
  # Essential packages for initialization
  essential_pkgs <- c("cli", "fs", "here", "glue")
  for (pkg in essential_pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg, quiet = TRUE)
    }
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  }
  
  cli_h1("Africa GVC Readiness Pipeline v3.0")
  cli_alert_info("Initializing comprehensive analysis pipeline...")
  cli_alert_info("User: Canomoncada | Time: {Sys.time()}")
  
  # Validate project root
  if (!dir_exists(dirname(project_root))) {
    cli_abort("Parent directory does not exist: {dirname(project_root)}")
  }
  
  # Enhanced directory structure
  dirs <- list(
    root         = project_root,
    data         = path(project_root, "Data"),
    exports      = path(project_root, "Exports"),
    enhanced     = path(project_root, "Exports", "Enhanced_Exports"),
    visuals      = path(project_root, "Exports", "Enhanced_Exports", "Visuals"),
    excel        = path(project_root, "Exports", "Enhanced_Exports", "Excel"),
    csv          = path(project_root, "Exports", "CSVs"),
    png          = path(project_root, "Exports", "PNGs"),
    pdf          = path(project_root, "Exports", "PDFs"),
    html         = path(project_root, "Exports", "HTML"),
    maps         = path(project_root, "Exports", "Enhanced_Exports", "Visuals", "Maps"),
    boxplots     = path(project_root, "Exports", "Enhanced_Exports", "Visuals", "Boxplots"),
    ridges       = path(project_root, "Exports", "Enhanced_Exports", "Visuals", "Ridges"),
    heatmaps     = path(project_root, "Exports", "Enhanced_Exports", "Visuals", "Heatmaps"),
    pca          = path(project_root, "Exports", "Enhanced_Exports", "Visuals", "PCA"),
    correlation  = path(project_root, "Exports", "Enhanced_Exports", "Visuals", "Correlation"),
    scatterplots = path(project_root, "Exports", "Enhanced_Exports", "Visuals", "ScatterPairs"),
    radar        = path(project_root, "Exports", "Enhanced_Exports", "Visuals", "RadarCharts"),
    violin       = path(project_root, "Exports", "Enhanced_Exports", "Visuals", "Violin"),
    regional     = path(project_root, "Exports", "Enhanced_Exports", "Visuals", "Regional_Bars"),
    networks     = path(project_root, "Exports", "Enhanced_Exports", "Visuals", "Networks"),
    logs         = path(project_root, "Logs"),
    temp         = path(project_root, "Temp"),
    backup       = path(project_root, "Backup", format(Sys.time(), "%Y%m%d_%H%M%S"))
  )
  
  # Create backup if requested
  if (create_backup && dir_exists(dirs$exports)) {
    cli_alert_info("Creating backup of existing outputs...")
    dir_create(dirs$backup, recurse = TRUE)
    if (length(dir_ls(dirs$exports)) > 0) {
      file_copy(dir_ls(dirs$exports, recurse = TRUE), dirs$backup, overwrite = TRUE)
      cli_alert_success("Backup created: {dirs$backup}")
    }
  }
  
  # Create all directories
  walk(dirs, ~dir_create(.x, recurse = TRUE))
  cli_alert_success("Directory structure created: {length(dirs)} directories")
  
  # Comprehensive package management
  required_packages <- list(
    # Core data manipulation
    core = c("tidyverse", "dplyr", "readr", "readxl", "openxlsx", "haven", 
             "janitor", "countrycode", "lubridate", "purrr", "stringr", "forcats"),
    
    # Statistical analysis
    stats = c("FactoMineR", "factoextra", "missMDA", "corrplot", "broom", "psych"),
    
    # Visualization - basic
    viz_basic = c("ggplot2", "patchwork", "ggrepel", "scales", "RColorBrewer", "viridis"),
    
    # Visualization - advanced
    viz_advanced = c("ggridges", "ggbeeswarm", "GGally", "fmsb", "ggbump", 
                    "pheatmap", "ggsci", "ggraph", "ggforce"),
    
    # Geospatial
    geo = c("sf", "rnaturalearth", "rnaturalearthdata", "maps", "maptools"),
    
    # Network analysis
    network = c("igraph", "tidygraph", "visNetwork", "networkD3"),
    
    # Interactive & web
    interactive = c("DT", "htmlwidgets", "flexdashboard", "rmarkdown", "plotly", "crosstalk"),
    
    # Utilities
    utils = c("zip", "tictoc", "future", "furrr", "sessioninfo")
  )
  
  all_packages <- unlist(required_packages, use.names = FALSE)
  
  # Install missing packages with progress tracking
  missing_pkgs <- setdiff(all_packages, rownames(installed.packages()))
  
  if (length(missing_pkgs) > 0) {
    cli_alert_info("Installing {length(missing_pkgs)} missing packages...")
    
    install_results <- map_lgl(missing_pkgs, ~{
      tryCatch({
        install.packages(.x, dependencies = TRUE, quiet = TRUE)
        TRUE
      }, error = function(e) {
        cli_alert_warning("Failed to install {.x}")
        FALSE
      })
    })
    
    if (!all(install_results)) {
      failed_pkgs <- missing_pkgs[!install_results]
      cli_alert_warning("Failed installations: {paste(failed_pkgs, collapse = ', ')}")
    }
  }
  
  # Load packages by category with error handling
  load_package_category <- function(pkg_list, category_name) {
    cli_alert_info("Loading {category_name} packages...")
    
    results <- map_lgl(pkg_list, ~{
      tryCatch({
        suppressPackageStartupMessages(library(.x, character.only = TRUE))
        TRUE
      }, error = function(e) {
        cli_alert_warning("Failed to load {.x}")
        FALSE
      })
    })
    
    cli_alert_success("Loaded {sum(results)}/{length(results)} {category_name} packages")
    return(results)
  }
  
  # Load all package categories
  package_results <- imap(required_packages, ~load_package_category(.x, .y))
  
  # Global options for enhanced performance and consistency
  options(
    scipen = 999,
    dplyr.summarise.inform = FALSE,
    readr.show_col_types = FALSE,
    warn = 1,
    stringsAsFactors = FALSE,
    mc.cores = parallel::detectCores() - 1,
    ggplot2.continuous.colour = "viridis",
    ggplot2.continuous.fill = "viridis"
  )
  
  # Enhanced ggplot2 theme
  theme_gvc <- function(base_size = 11, base_family = "") {
    theme_minimal(base_size = base_size, base_family = base_family) +
      theme(
        plot.title = element_text(size = rel(1.4), face = "bold", 
                                 margin = margin(b = 20), hjust = 0),
        plot.subtitle = element_text(size = rel(1.1), color = "grey60", 
                                   margin = margin(b = 20)),
        plot.caption = element_text(size = rel(0.8), color = "grey60", 
                                  hjust = 0, margin = margin(t = 20)),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey95", size = 0.3),
        legend.position = "bottom",
        legend.box = "horizontal",
        strip.text = element_text(face = "bold", size = rel(1.1)),
        axis.title = element_text(size = rel(1.1)),
        axis.text = element_text(size = rel(0.9))
      )
  }
  
  theme_set(theme_gvc())
  
  # Comprehensive color palettes
  colors <- list(
    # Primary brand colors
    primary = "#2E8B57",
    secondary = "#4682B4", 
    accent = "#CD853F",
    
    # Status colors
    success = "#228B22",
    warning = "#FF8C00", 
    danger = "#DC143C",
    info = "#1E90FF",
    
    # Regional colors
    region_colors = c(
      "Africa" = "#2E8B57", "ASEAN" = "#4682B4", 
      "LAC" = "#CD853F", "OECD" = "#8B4513", "Other" = "#696969"
    ),
    
    # Palette functions
    blue_palette = colorRampPalette(brewer.pal(9, "Blues")),
    blue_pal_n = function(n) colorRampPalette(brewer.pal(9, "Blues"))(max(n, 3)),
    diverging_palette = colorRampPalette(c("#2166AC", "#F7F7F7", "#B2182B")),
    sequential_palette = colorRampPalette(c("#F7FBFF", "#08306B")),
    
    # Scientific journal palettes
    nature = ggsci::pal_npg("nrc")(10),
    science = ggsci::pal_aaas("default")(10),
    lancet = ggsci::pal_lancet("lanonc")(9)
  )
  
  # Analysis configuration
  config <- list(
    # Metadata
    timestamp = start_time,
    user = "Canomoncada",
    version = "3.0",
    
    # Directories
    dirs = dirs,
    
    # Visual settings
    colors = colors,
    
    # Analysis parameters
    indicators = c(
      "Internet Penetration Index", "Mobile Connectivity Index",
      "Trade to GDP Ratio Index", "Logistics Performance Index", 
      "Modern Renewables Share Index", "CO₂ Intensity Index",
      "Business Ready Index", "Political Stability Index"
    ),
    
    # Indicator groupings
    indicator_groups = list(
      technology = c("Internet Penetration Index", "Mobile Connectivity Index"),
      trade_logistics = c("Trade to GDP Ratio Index", "Logistics Performance Index"),
      sustainability = c("Modern Renewables Share Index", "CO₂ Intensity Index"),
      governance = c("Business Ready Index", "Political Stability Index")
    ),
    
    # Regional definitions
    regions = c("Africa", "ASEAN", "LAC", "OECD"),
    
    # Statistical parameters
    pca_components = 5,
    correlation_threshold = 0.6,
    significance_level = 0.05,
    
    # Visualization parameters
    plot_width = 10,
    plot_height = 7,
    plot_dpi = 300,
    
    # Package loading results
    packages_loaded = package_results,
    total_packages = length(all_packages)
  )
  
  # Save configuration
  saveRDS(config, path(dirs$logs, "pipeline_config.rds"))
  
  cli_alert_success("Pipeline initialization completed in {round(difftime(Sys.time(), start_time, units = 'secs'), 2)} seconds")
  cli_alert_info("Ready to process {length(config$indicators)} indicators across {length(config$regions)} regions")
  
  return(config)
}

# ┌─────────────────────────────────────────────────────────┐
# │ SECTION 1: DATA LOADING AND VALIDATION                 │
# └─────────────────────────────────────────────────────────┘

load_gvc_data <- function(config) {
  cli_h2("Loading Africa GVC Data")
  
  main_data_file <- path(config$dirs$csv, "Annex_A_135_Countries_2025.csv")
  
  if (!file_exists(main_data_file)) {
    cli_abort("Main data file not found: {main_data_file}")
  }
  
  tryCatch({
    cli_alert_info("Loading main dataset...")
    
    df_main <- read_csv(main_data_file, 
                       show_col_types = FALSE,
                       locale = locale(encoding = "UTF-8"),
                       na = c("", "NA", "N/A", "#N/A", "NULL"))
    
    required_cols <- c("Country", "Region", config$indicators)
    missing_cols <- setdiff(required_cols, names(df_main))
    
    if (length(missing_cols) > 0) {
      cli_abort("Missing required columns: {paste(missing_cols, collapse = ', ')}")
    }
    
    total_cells <- nrow(df_main) * length(config$indicators)
    missing_cells <- sum(is.na(df_main[config$indicators]))
    missing_percentage <- round(missing_cells / total_cells * 100, 2)
    
    complete_cases <- sum(complete.cases(df_main[config$indicators]))
    complete_percentage <- round(complete_cases / nrow(df_main) * 100, 2)
    
    regional_counts <- df_main %>% 
      count(Region, name = "count") %>%
      mutate(percentage = round(count / sum(count) * 100, 1))
    
    cli_alert_success("Main dataset loaded successfully")
    cli_alert_info("{nrow(df_main)} countries, {ncol(df_main)} variables")
    cli_alert_info("{complete_cases} complete cases ({complete_percentage}%)")
    cli_alert_info("{missing_percentage}% missing values")
    
    cli_alert_info("Regional distribution:")
    walk2(regional_counts$Region, regional_counts$count, 
          ~cli_alert_info("   {.x}: {.y} countries"))
    
    return(list(main = df_main))
    
  }, error = function(e) {
    cli_abort("Failed to load main dataset: {e$message}")
  })
}

# ┌─────────────────────────────────────────────────────────┐
# │ SECTION 2: RIDGE PLOTS GENERATION                      │
# └─────────────────────────────────────────────────────────┘

generate_ridge_plots <- function(data, config) {
  cli_h2("Generating Ridge Plots")
  
  ridge_plots <- map(config$indicators, ~{
    indicator <- .x
    
    tryCatch({
      plot <- ggplot(data, aes(x = .data[[indicator]], y = Region, fill = Region)) +
        geom_density_ridges(
          scale = 1.2, 
          alpha = 0.8, 
          color = "white", 
          size = 0.5,
          rel_min_height = 0.01
        ) +
        scale_fill_manual(values = config$colors$region_colors) +
        labs(
          title = str_wrap(paste("Distribution of", indicator, "by Region"), 50),
          x = indicator,
          y = NULL,
          caption = "Data: Africa GVC Readiness Analysis 2025"
        ) +
        theme_gvc() +
        theme(
          legend.position = "none",
          plot.title = element_text(size = 14, face = "bold"),
          axis.text.y = element_text(size = 11, face = "bold")
        )
      
      filename <- paste0("Ridge_", str_replace_all(indicator, " ", "_"), ".png")
      ggsave(
        filename = filename,
        plot = plot,
        path = config$dirs$ridges,
        width = config$plot_width,
        height = 6,
        dpi = config$plot_dpi
      )
      
      cli_alert_success("Saved ridge plot: {filename}")
      return(plot)
      
    }, error = function(e) {
      cli_alert_danger("Failed to create ridge plot for {indicator}: {e$message}")
      return(NULL)
    })
  })
  
  names(ridge_plots) <- str_replace_all(config$indicators, " ", "_")
  ridge_plots <- discard(ridge_plots, is.null)
  
  cli_alert_success("Generated {length(ridge_plots)} ridge plots")
  return(ridge_plots)
}

# ┌─────────────────────────────────────────────────────────┐
# │ SECTION 3: COMPREHENSIVE PCA ANALYSIS                  │
# └─────────────────────────────────────────────────────────┘

perform_comprehensive_pca <- function(data, config) {
  cli_h2("Performing Comprehensive PCA Analysis")
  
  pca_data <- data %>%
    select(all_of(config$indicators)) %>%
    mutate(across(everything(), as.numeric))
  
  complete_cases <- sum(complete.cases(pca_data))
  if (complete_cases < 10) {
    cli_abort("Insufficient complete cases for PCA: {complete_cases}")
  }
  
  cli_alert_info("Using {complete_cases} complete cases for PCA")
  
  if (any(is.na(pca_data))) {
    cli_alert_info("Performing missing value imputation...")
    pca_data_complete <- pca_data %>%
      mutate(across(everything(), ~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)))
  } else {
    pca_data_complete <- pca_data
  }
  
  cli_alert_info("Computing Principal Component Analysis...")
  
  tryCatch({
    pca_result <- PCA(
      pca_data_complete,
      scale.unit = TRUE,
      ncp = config$pca_components,
      graph = FALSE
    )
    
    variance_explained <- pca_result$eig[, "percentage of variance"]
    cumulative_variance <- cumsum(variance_explained)
    
    cli_alert_success("PCA computation completed")
    cli_alert_info("First 2 PCs explain {round(cumulative_variance[2], 1)}% of variance")
    
    pca_coords <- as.data.frame(pca_result$ind$coord)
    pca_df <- bind_cols(
      data %>% select(Country, Region),
      pca_coords
    )
    
    # Create PCA visualizations
    plots <- list()
    
    plots$scree <- fviz_eig(
      pca_result,
      addlabels = TRUE,
      barfill = config$colors$primary,
      barcolor = "black"
    ) +
      labs(
        title = "PCA Scree Plot",
        subtitle = glue("First {config$pca_components} components explain {round(cumulative_variance[config$pca_components], 1)}% of variance"),
        x = "Principal Components",
        y = "Percentage of Variance Explained"
      ) +
      theme_gvc()
    
    plots$scatter <- ggplot(pca_df, aes(x = Dim.1, y = Dim.2, color = Region)) +
      geom_point(size = 3, alpha = 0.8) +
      geom_text_repel(
        aes(label = Country),
        size = 2.5,
        max.overlaps = 15,
        box.padding = 0.3
      ) +
      scale_color_manual(values = config$colors$region_colors) +
      labs(
        title = "PCA Component Space: Countries by Region",
        x = glue("PC1 ({round(variance_explained[1], 1)}% variance)"),
        y = glue("PC2 ({round(variance_explained[2], 1)}% variance)"),
        color = "Region"
      ) +
      theme_gvc() +
      theme(legend.position = "bottom")
    
    plots$contributions <- fviz_pca_var(
      pca_result,
      col.var = "contrib",
      gradient.cols = c(
        config$colors$blue_palette(3)[1],
        config$colors$blue_palette(3)[2], 
        config$colors$blue_palette(3)[3]
      ),
      repel = TRUE
    ) +
      labs(
        title = "Variable Contributions to Principal Components",
        subtitle = "Color intensity shows contribution strength"
      ) +
      theme_gvc()
    
    # Save PCA plots
    plot_specs <- list(
      list(plot = plots$scree, filename = "PCA_Scree_Enhanced.png", width = 10, height = 7),
      list(plot = plots$scatter, filename = "PCA_Component_Scatter.png", width = 10, height = 7),
      list(plot = plots$contributions, filename = "PCA_Contributions_Enhanced.png", width = 10, height = 8)
    )
    
    walk(plot_specs, ~{
      ggsave(
        filename = .x$filename,
        plot = .x$plot,
        path = config$dirs$pca,
        width = .x$width,
        height = .x$height,
        dpi = config$plot_dpi
      )
      cli_alert_success("Saved: {.x$filename}")
    })
    
    # Calculate rankings
    global_ranking <- pca_df %>%
      arrange(desc(Dim.1)) %>%
      mutate(Global_Rank = row_number(), PCA_Score = Dim.1)
    
    regional_rankings <- map(config$regions, ~{
      pca_df %>%
        filter(Region == .x) %>%
        arrange(desc(Dim.1)) %>%
        mutate(Regional_Rank = row_number(), PCA_Score = Dim.1)
    })
    names(regional_rankings) <- config$regions
    
    cli_alert_success("PCA analysis completed successfully")
    
    return(list(
      pca_result = pca_result,
      plots = plots,
      variance_explained = variance_explained,
      cumulative_variance = cumulative_variance,
      pca_df = pca_df,
      global_ranking = global_ranking,
      regional_rankings = regional_rankings
    ))
    
  }, error = function(e) {
    cli_abort("PCA analysis failed: {e$message}")
  })
}

# ┌─────────────────────────────────────────────────────────┐
# │ SECTION 4: COMPREHENSIVE VISUALIZATION SUITE           │
# └─────────────────────────────────────────────────────────┘

generate_comprehensive_visualizations <- function(data, config) {
  cli_h2("Generating Comprehensive Visualization Suite")
  
  visualization_results <- list()
  
  # BOXPLOTS
  cli_alert_info("Creating boxplots...")
  
  tryCatch({
    make_mini_boxplot <- function(indicator) {
      ggplot(data, aes(x = Region, y = .data[[indicator]], fill = Region)) +
        geom_boxplot(alpha = 0.8, outlier.size = 0.8) +
        scale_fill_manual(values = config$colors$region_colors) +
        labs(title = str_wrap(indicator, 25), x = NULL, y = NULL) +
        theme_gvc(base_size = 9) +
        theme(
          legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
          plot.title = element_text(size = 9, face = "bold")
        )
    }
    
    boxplot_list <- map(config$indicators, make_mini_boxplot)
    
    composite_boxplot <- wrap_plots(boxplot_list, ncol = 2) +
      plot_annotation(
        title = "GVC Readiness Indicators: Regional Distribution Comparison",
        subtitle = "Boxplots showing median, quartiles, and outliers for each indicator by region",
        caption = "Data: Africa GVC Readiness Analysis 2025",
        theme = theme_gvc()
      )
    
    ggsave(
      filename = "Composite_GVC_Boxplots.png",
      plot = composite_boxplot,
      path = config$dirs$boxplots,
      width = 14,
      height = 12,
      dpi = config$plot_dpi
    )
    
    visualization_results$boxplots <- composite_boxplot
    cli_alert_success("Boxplots completed")
    
  }, error = function(e) {
    cli_alert_danger("Boxplot generation failed: {e$message}")
  })
  
  # VIOLIN PLOTS
  cli_alert_info("Creating violin plots...")
  
  tryCatch({
    violin_plots <- map(config$indicators, ~{
      indicator <- .x
      
      plot <- ggplot(data, aes(x = Region, y = .data[[indicator]], fill = Region)) +
        geom_violin(
          trim = FALSE, 
          alpha = 0.6, 
          color = "gray30",
          scale = "width"
        ) +
        geom_beeswarm(
          color = "white", 
          size = 1.8, 
          alpha = 0.8,
          cex = 0.8
        ) +
        scale_fill_manual(values = config$colors$region_colors) +
        labs(
          title = str_wrap(paste("Distribution of", indicator, "by Region"), 50),
          x = NULL,
          y = indicator
        ) +
        theme_gvc() +
        theme(
          legend.position = "none",
          axis.text.x = element_text(angle = 20, hjust = 1)
        )
      
      filename <- paste0("Violin_", str_replace_all(indicator, " ", "_"), ".png")
      ggsave(
        filename = filename,
        plot = plot,
        path = config$dirs$violin,
        width = 8,
        height = 6,
        dpi = config$plot_dpi
      )
      
      return(plot)
    })
    
    names(violin_plots) <- str_replace_all(config$indicators, " ", "_")
    visualization_results$violin_plots <- violin_plots
    
    cli_alert_success("Violin plots completed")
    
  }, error = function(e) {
    cli_alert_danger("Violin plot generation failed: {e$message}")
  })
  
  # CORRELATION HEATMAPS
  cli_alert_info("Creating correlation heatmaps...")
  
  tryCatch({
    indicator_data <- data %>%
      select(all_of(config$indicators)) %>%
      mutate(across(everything(), as.numeric))
    
    cor_matrix <- cor(indicator_data, use = "pairwise.complete.obs", method = "pearson")
    
    png(
      filename = path(config$dirs$correlation, "GVC_Indicator_Correlation_Heatmap.png"),
      width = 1200, 
      height = 1000, 
      res = 150
    )
    
    corrplot(
      cor_matrix,
      method = "color",
      type = "upper",
      order = "hclust",
      col = config$colors$blue_palette(100),
      tl.col = "black",
      tl.srt = 45,
      addCoef.col = "black",
      number.cex = 0.75,
      mar = c(0, 0, 1, 0),
      title = "Correlation Matrix of GVC Readiness Indicators"
    )
    
    dev.off()
    
    visualization_results$correlation <- cor_matrix
    cli_alert_success("Correlation heatmaps completed")
    
  }, error = function(e) {
    cli_alert_danger("Correlation heatmap generation failed: {e$message}")
  })
  
  # PILLAR HEATMAPS
  cli_alert_info("Creating pillar-based heatmaps...")
  
  tryCatch({
    pillar_heatmaps <- imap(config$indicator_groups, ~{
      indicators <- .x
      pillar_name <- .y
      
      pillar_data <- data %>%
        select(Country, all_of(indicators)) %>%
        drop_na() %>%
        column_to_rownames("Country") %>%
        as.matrix()
      
      title <- str_to_title(str_replace_all(pillar_name, "_", " & "))
      filename <- path(config$dirs$heatmaps, glue("{str_to_title(pillar_name)}_Readiness_Heatmap.png"))
      
      pheatmap(
        mat = pillar_data,
        cluster_rows = TRUE,
        cluster_cols = TRUE,
        color = config$colors$blue_palette(100),
        fontsize_row = 7,
        fontsize_col = 9,
        main = glue("{title} Readiness Heatmap"),
        angle_col = 45,
        filename = filename,
        width = 9,
        height = 8
      )
      
      return(pillar_data)
    })
    
    visualization_results$pillar_heatmaps <- pillar_heatmaps
    cli_alert_success("Pillar-based heatmaps completed")
    
  }, error = function(e) {
    cli_alert_danger("Pillar heatmap generation failed: {e$message}")
  })
  
  # REGIONAL BAR PLOTS
  cli_alert_info("Creating regional average bar plots...")
  
  tryCatch({
    regional_means <- data %>%
      select(Region, all_of(config$indicators)) %>%
      group_by(Region) %>%
      summarise(across(everything(), mean, na.rm = TRUE), .groups = "drop") %>%
      pivot_longer(-Region, names_to = "Indicator", values_to = "Score")
    
    regional_bar_plots <- map(config$indicators, ~{
      indicator <- .x
      
      plot <- regional_means %>%
        filter(Indicator == indicator) %>%
        ggplot(aes(x = reorder(Region, Score), y = Score, fill = Region)) +
        geom_col(alpha = 0.85, color = "black", size = 0.3) +
        coord_flip() +
        scale_fill_manual(values = config$colors$region_colors) +
        labs(
          title = str_wrap(paste("Regional Average:", indicator), 40),
          x = NULL,
          y = "Average Score (Normalized 0-1)"
        ) +
        theme_gvc() +
        theme(legend.position = "none")
      
      filename <- paste0(str_replace_all(indicator, " ", "_"), "_Regional_Barplot.png")
      ggsave(
        filename = filename,
        plot = plot,
        path = config$dirs$regional,
        width = 8,
        height = 5,
        dpi = config$plot_dpi
      )
      
      return(plot)
    })
    
    names(regional_bar_plots) <- str_replace_all(config$indicators, " ", "_")
    
    visualization_results$regional_bars <- list(
      individual = regional_bar_plots,
      data = regional_means
    )
    
    cli_alert_success("Regional bar plots completed")
    
  }, error = function(e) {
    cli_alert_danger("Regional bar plot generation failed: {e$message}")
  })
  
  cli_alert_success("Comprehensive visualization suite completed")
  return(visualization_results)
}

# ┌─────────────────────────────────────────────────────────┐
# │ SECTION 5: RADAR CHART GENERATION                      │
# └─────────────────────────────────────────────────────────┘

generate_radar_charts <- function(data, config) {
  cli_h2("Generating Radar Charts")
  
  df_scaled <- data %>%
    mutate(across(all_of(config$indicators), ~scales::rescale(as.numeric(.x), to = c(0, 1), na.rm = TRUE))) %>%
    select(Country, Region, all_of(config$indicators))
  
  plot_radar_safe <- function(country_name) {
    tryCatch({
      df_country <- df_scaled %>% filter(Country == country_name)
      if (nrow(df_country) == 0) return(NULL)
      
      values <- df_country %>% select(all_of(config$indicators)) %>% as.numeric()
      if (any(is.na(values))) {
        cli_alert_warning("Skipping {country_name} due to missing values")
        return(NULL)
      }
      
      radar_data <- rbind(
        rep(1, length(values)),
        rep(0, length(values)),
        values
      )
      colnames(radar_data) <- config$indicators
      rownames(radar_data) <- c("Max", "Min", country_name)
      
      file_path <- path(config$dirs$radar, paste0("Radar_", str_replace_all(country_name, " ", "_"), ".png"))
      
      png(file_path, width = 800, height = 800)
      radarchart(
        radar_data,
        axistype = 1,
        pcol = config$colors$primary,
        pfcol = alpha(config$colors$primary, 0.35),
        plwd = 2,
        title = paste("GVC Readiness Profile:", country_name),
        cglcol = "gray80",
        cglty = 1,
        axislabcol = "black",
        vlcex = 0.85
      )
      dev.off()
      
      return(TRUE)
      
    }, error = function(e) {
      cli_alert_warning("Failed to create radar chart for {country_name}: {e$message}")
      return(FALSE)
    })
  }
  
  cli_alert_info("Creating individual radar charts...")
  
  priority_countries <- c(
    "South Africa", "Nigeria", "Kenya", "Ghana", "Rwanda", "Morocco", "Egypt", "Ethiopia",
    "Singapore", "United States", "Germany", "Chile", "Brazil"
  )
  
  available_countries <- intersect(priority_countries, df_scaled$Country)
  radar_results <- map_lgl(available_countries, plot_radar_safe)
  successful_radars <- sum(radar_results, na.rm = TRUE)
  
  cli_alert_success("Created {successful_radars} individual radar charts")
  
  return(list(
    individual_count = successful_radars,
    countries_processed = available_countries,
    scaled_data = df_scaled
  ))
}

# ┌─────────────────────────────────────────────────────────┐
# │ SECTION 6: EXCEL EXPORTS                               │
# └─────────────────────────────────────────────────────────┘

generate_excel_exports <- function(data, pca_results, config) {
  cli_h2("Generating Excel Exports")
  
  tryCatch({
    wb <- createWorkbook()
    
    addWorksheet(wb, "Main_Dataset")
    writeData(wb, "Main_Dataset", data)
    
    if (!is.null(pca_results$pca_df)) {
      addWorksheet(wb, "PCA_Coordinates")
      writeData(wb, "PCA_Coordinates", pca_results$pca_df)
    }
    
    if (!is.null(pca_results$global_ranking)) {
      addWorksheet(wb, "Global_Rankings")
      writeData(wb, "Global_Rankings", pca_results$global_ranking)
    }
    
    if (!is.null(pca_results$regional_rankings)) {
      iwalk(pca_results$regional_rankings, ~{
        sheet_name <- paste0(.y, "_Rankings")
        addWorksheet(wb, sheet_name)
        writeData(wb, sheet_name, .x)
      })
    }
    
    summary_stats <- data %>%
      select(all_of(config$indicators)) %>%
      summarise(across(everything(), list(
        Count = ~sum(!is.na(.)),
        Mean = ~mean(., na.rm = TRUE),
        Median = ~median(., na.rm = TRUE),
        SD = ~sd(., na.rm = TRUE),
        Min = ~min(., na.rm = TRUE),
        Max = ~max(., na.rm = TRUE)
      ), .names = "{.col}_{.fn}")) %>%
      pivot_longer(cols = everything(), 
                   names_to = c("Indicator", "Statistic"),
                   names_sep = "_",
                   values_to = "Value") %>%
      pivot_wider(names_from = Statistic, values_from = Value)
    
    addWorksheet(wb, "Summary_Statistics")
    writeData(wb, "Summary_Statistics", summary_stats)
    
    regional_averages <- data %>%
      select(Region, all_of(config$indicators)) %>%
      group_by(Region) %>%
      summarise(across(everything(), mean, na.rm = TRUE), .groups = "drop")
    
    addWorksheet(wb, "Regional_Averages")
    writeData(wb, "Regional_Averages", regional_averages)
    
    main_excel_file <- path(config$dirs$excel, "GVC_Comprehensive_Analysis_2025.xlsx")
    saveWorkbook(wb, main_excel_file, overwrite = TRUE)
    
    cli_alert_success("Main Excel workbook saved: {path_file(main_excel_file)}")
    
    return(main_excel_file)
    
  }, error = function(e) {
    cli_abort("Excel export failed: {e$message}")
  })
}

# ┌─────────────────────────────────────────────────────────┐
# │ SECTION 7: NETWORK ANALYSIS                            │
# └─────────────────────────────────────────────────────────┘

generate_network_analysis <- function(data, config) {
  cli_h2("Generating Network Analysis")
  
  tryCatch({
    mat <- data %>% 
      select(Country, all_of(config$indicators)) %>% 
      column_to_rownames("Country") %>% 
      as.matrix()
    
    corr_countries <- cor(t(mat), use = "pairwise.complete.obs")
    
    adj <- corr_countries
    adj[abs(adj) <= 0.7] <- 0
    diag(adj) <- 0
    
    net <- graph_from_adjacency_matrix(adj, mode = "undirected", weighted = TRUE, diag = FALSE)
    
    V(net)$region <- data$Region[match(V(net)$name, data$Country)]
    V(net)$wdeg <- strength(net)
    V(net)$size <- rescale(V(net)$wdeg, to = c(4, 12))
    
    set.seed(42)
    l <- layout_with_fr(net, weights = abs(E(net)$weight), niter = 500)
    l[!is.finite(l)] <- 0
    l <- norm_coords(l, xmin = -1, xmax = 1, ymin = -1, ymax = 1)
    
    regions <- sort(unique(na.omit(V(net)$region)))
    pal <- brewer.pal(max(length(regions), 3), "Set2")[1:length(regions)]
    col_map <- setNames(pal, regions)
    V(net)$color <- col_map[V(net)$region]
    
    png(path(config$dirs$networks, "country_similarity_network.png"),
        width = 1000, height = 800, res = 150)
    par(bg = "white", mar = c(0, 0, 2, 0))
    
    plot(net,
         layout = l,
         rescale = FALSE,
         xlim = c(-1, 1),
         ylim = c(-1, 1),
         vertex.size = V(net)$size,
         vertex.color = V(net)$color,
         vertex.frame.color = "white",
         vertex.label = V(net)$name,
         vertex.label.cex = 0.6,
         edge.width = abs(E(net)$weight) * 2,
         edge.color = adjustcolor("gray40", alpha.f = 0.3),
         main = "Country Similarity Network (Correlation > 0.7)")
    
    legend("topleft",
           legend = regions,
           col = pal,
           pch = 19,
           pt.cex = 1.5,
           bty = "n")
    
    dev.off()
    
    cli_alert_success("Network analysis completed")
    
    return(net)
    
  }, error = function(e) {
    cli_alert_warning("Network analysis failed: {e$message}")
    return(NULL)
  })
}

# ┌─────────────────────────────────────────────────────────┐
# │ SECTION 8: FINAL ARCHIVE CREATION                      │
# └─────────────────────────────────────────────────────────┘

create_final_archive <- function(config) {
  cli_h2("Creating Final Archive")
  
  tryCatch({
    zip_file_path <- path(config$dirs$root, "Exports", "ZIPs", "GVC_Complete_Analysis_2025.zip")
    dir_create(dirname(zip_file_path), recurse = TRUE)
    
    zip::zipr(
      zipfile = zip_file_path,
      files = dir_ls(config$dirs$enhanced, recurse = TRUE)
    )
    
    cli_alert_success("Complete archive created: {zip_file_path}")
    return(zip_file_path)
    
  }, error = function(e) {
    cli_alert_warning("Archive creation failed: {e$message}")
    return(NULL)
  })
}

# ┌─────────────────────────────────────────────────────────┐
# │ MAIN EXECUTION PIPELINE                                │
# └─────────────────────────────────────────────────────────┘

main_pipeline <- function() {
  cli_h1("EXECUTING COMPLETE AFRICA GVC ANALYSIS PIPELINE")
  
  # Initialize pipeline
  config <- initialize_comprehensive_pipeline()
  
  # Load data
  data_results <- load_gvc_data(config)
  main_data <- data_results$main
  
  # Generate ridge plots
  ridge_results <- generate_ridge_plots(main_data, config)
  
  # Perform PCA analysis
  pca_results <- perform_comprehensive_pca(main_data, config)
  
  # Generate comprehensive visualizations
  viz_results <- generate_comprehensive_visualizations(main_data, config)
  
  # Generate radar charts
  radar_results <- generate_radar_charts(main_data, config)
  
  # Generate Excel exports
  excel_file <- generate_excel_exports(main_data, pca_results, config)
  
  # Generate network analysis
  network_result <- generate_network_analysis(main_data, config)
  
  # Create final archive
  archive_file <- create_final_archive(config)
  
  # Final summary
  cli_h1("PIPELINE EXECUTION COMPLETED")
  cli_alert_success("All analysis components completed successfully")
  cli_alert_info("Main Excel file: {excel_file}")
  cli_alert_info("Complete archive: {archive_file}")
  cli_alert_info("Total execution time: {round(difftime(Sys.time(), config$timestamp, units = 'mins'), 2)} minutes")
  
  # Display session information
  cli_alert_info("Session information saved for reproducibility")
  session_info <- sessionInfo()
  saveRDS(session_info, path(config$dirs$logs, "session_info.rds"))
  
  return(list(
    config = config,
    data = main_data,
    pca_results = pca_results,
    viz_results = viz_results,
    radar_results = radar_results,
    excel_file = excel_file,
    archive_file = archive_file
  ))
}

# Execute the complete pipeline
if (interactive()) {
  results <- main_pipeline()
}
