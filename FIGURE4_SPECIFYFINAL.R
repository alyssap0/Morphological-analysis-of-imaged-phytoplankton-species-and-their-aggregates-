# Load required packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(gtable)
library(grid)
library(gridExtra)
library(latex2exp)
library(plyr)
library(reshape2)
library(scales)
library(ggpubr)
library(patchwork)
library(FSA)
library(multcompView)
library(tibble)
library(viridis)
library(rstatix)

setwd("C:/Users/Cashies/OneDrive - University of Tasmania/AP_THESIS/Data")

# Read CSV file
all_data <- read.csv("agg_data.csv", header = TRUE)

all_data <- all_data %>%
  mutate(Porosity_correct = Total_Hole_Area_mm2 / Area_mm2)

# Selecting variables of interest and excluding dots
dat <- all_data %>%
  select(Species, Equivalent_Spherical_Diameter_mm, Perimeter_mm, Area_mm2, Circularity, Aspect_Ratio, Porosity_correct) %>%
  filter(Equivalent_Spherical_Diameter_mm >= 0.5,
         Area_mm2 > 0, Perimeter_mm > 0) %>% 
  mutate(
    Species = as.factor(Species),
    Porosity_percent = Porosity_correct * 100,
    Wobble = Perimeter_mm / Equivalent_Spherical_Diameter_mm,
    Fractal_Dimension = 2 * log10(Perimeter_mm) / log10(Area_mm2)
  ) %>%
  filter(Species %in% c("Chaetoceros sp.", "Ditylum sp.", "Melosira sp.", "Skeletonema sp.", "Thalassionema sp."))

# Regression-derived FD by species
fd_by_species <- dat %>%
  group_by(Species) %>%
  do({
    model <- lm(log10(Perimeter_mm) ~ log10(Area_mm2), data = .)
    slope <- coef(model)["log10(Area_mm2)"]
    data.frame(Fractal_Dimension = 2 * slope)
  })

dat$Fractal_Dimension <- 2 * log10(dat$Perimeter_mm) / log10(dat$Area_mm2)

# Reshape the data to long format
dat_long <- melt(dat, id.vars = "Species")

#####ADD SIGNIFICANCE LETTERS 

metrics_to_test <- c(
  "Equivalent_Spherical_Diameter_mm",
  "Perimeter_mm",
  "Area_mm2",
  "Circularity",
  "Aspect_Ratio",
  "Porosity_Percent",
  "Wobbliness",
  "Fractal_Dimension"
)

letters_grad <- expand.grid( SpeciesPct = factor(speciespct_levels, levels = speciespct_levels), variable = metrics_to_test, stringsAsFactors = FALSE ) 

# MANUAL ENTER OF ESD LETTERS 

letters_grad$Letter[ letters_grad$variable == "Equivalent_Spherical_Diameter_mm" & letters_grad$SpeciesPct == "Chaetoceros sp." ] <- "a" 
letters_grad$Letter[ letters_grad$variable == "Equivalent_Spherical_Diameter_mm" & letters_grad$SpeciesPct == "Ditylum sp." ] <- "b" 
letters_grad$Letter[ letters_grad$variable == "Equivalent_Spherical_Diameter_mm" & letters_grad$SpeciesPct == "Melosira sp." ] <- "c" 
letters_grad$Letter[ letters_grad$variable == "Equivalent_Spherical_Diameter_mm" & letters_grad$SpeciesPct == "Skeletonema sp." ] <- "a" 
letters_grad$Letter[ letters_grad$variable == "Equivalent_Spherical_Diameter_mm" & letters_grad$SpeciesPct == "Thalassionema sp." ] <- "d" 

# MANUAL ENTER OF PERIMETER LETTERS 

letters_grad$Letter[ letters_grad$variable == "Perimeter_mm" & letters_grad$SpeciesPct == "Chaetoceros sp." ] <- "a" 
letters_grad$Letter[ letters_grad$variable == "Perimeter_mm" & letters_grad$SpeciesPct == "Ditylum sp." ] <- "b" 
letters_grad$Letter[ letters_grad$variable == "Perimeter_mm" & letters_grad$SpeciesPct == "Melosira sp." ] <- "c" 
letters_grad$Letter[ letters_grad$variable == "Perimeter_mm" & letters_grad$SpeciesPct == "Skeletonema sp." ] <- "d" 
letters_grad$Letter[ letters_grad$variable == "Perimeter_mm" & letters_grad$SpeciesPct == "Thalassionema sp." ] <- "e" 

# MANUAL ENTER OF AREA LETTERS 

letters_grad$Letter[ letters_grad$variable == "Area_mm2" & letters_grad$SpeciesPct == "Chaetoceros sp." ] <- "a" 
letters_grad$Letter[ letters_grad$variable == "Area_mm2" & letters_grad$SpeciesPct == "Ditylum sp." ] <- "b" 
letters_grad$Letter[ letters_grad$variable == "Area_mm2" & letters_grad$SpeciesPct == "Melosira sp." ] <- "c" 
letters_grad$Letter[ letters_grad$variable == "Area_mm2" & letters_grad$SpeciesPct == "Skeletonema sp." ] <- "a" 
letters_grad$Letter[ letters_grad$variable == "Area_mm2" & letters_grad$SpeciesPct == "Thalassionema sp." ] <- "d" 

# MANUAL ENTER OF Circularity LETTERS 

letters_grad$Letter[ letters_grad$variable == "Circularity" & letters_grad$SpeciesPct == "Chaetoceros sp." ] <- "a" 
letters_grad$Letter[ letters_grad$variable == "Circularity" & letters_grad$SpeciesPct == "Ditylum sp." ] <- "b" 
letters_grad$Letter[ letters_grad$variable == "Circularity" & letters_grad$SpeciesPct == "Melosira sp." ] <- "c" 
letters_grad$Letter[ letters_grad$variable == "Circularity" & letters_grad$SpeciesPct == "Skeletonema sp." ] <- "d" 
letters_grad$Letter[ letters_grad$variable == "Circularity" & letters_grad$SpeciesPct == "Thalassionema sp." ] <- "e" 

# MANUAL ENTER OF Aspect Ratio LETTERS 

letters_grad$Letter[ letters_grad$variable == "Aspect_Ratio" & letters_grad$SpeciesPct == "Chaetoceros sp." ] <- "a" 
letters_grad$Letter[ letters_grad$variable == "Aspect_Ratio" & letters_grad$SpeciesPct == "Ditylum sp." ] <- "b" 
letters_grad$Letter[ letters_grad$variable == "Aspect_Ratio" & letters_grad$SpeciesPct == "Melosira sp." ] <- "c" 
letters_grad$Letter[ letters_grad$variable == "Aspect_Ratio" & letters_grad$SpeciesPct == "Skeletonema sp." ] <- "d" 
letters_grad$Letter[ letters_grad$variable == "Aspect_Ratio" & letters_grad$SpeciesPct == "Thalassionema sp." ] <- "e" 

# MANUAL ENTER OF Porosity LETTERS 

letters_grad$Letter[ letters_grad$variable == "Porosity_percent" & letters_grad$SpeciesPct == "Chaetoceros sp." ] <- "a" 
letters_grad$Letter[ letters_grad$variable == "Porosity_percent" & letters_grad$SpeciesPct == "Ditylum sp." ] <- "b" 
letters_grad$Letter[ letters_grad$variable == "Porosity_percent" & letters_grad$SpeciesPct == "Melosira sp." ] <- "b" 
letters_grad$Letter[ letters_grad$variable == "Porosity_percent" & letters_grad$SpeciesPct == "Skeletonema sp." ] <- "c" 
letters_grad$Letter[ letters_grad$variable == "Porosity_percent" & letters_grad$SpeciesPct == "Thalassionema sp." ] <- "d" 

# MANUAL ENTER OF Wobble LETTERS 

letters_grad$Letter[ letters_grad$variable == "Wobble" & letters_grad$SpeciesPct == "Chaetoceros sp." ] <- "a" 
letters_grad$Letter[ letters_grad$variable == "Wobble" & letters_grad$SpeciesPct == "Ditylum sp." ] <- "b" 
letters_grad$Letter[ letters_grad$variable == "Wobble" & letters_grad$SpeciesPct == "Melosira sp." ] <- "c" 
letters_grad$Letter[ letters_grad$variable == "Wobble" & letters_grad$SpeciesPct == "Skeletonema sp." ] <- "d" 
letters_grad$Letter[ letters_grad$variable == "Wobble" & letters_grad$SpeciesPct == "Thalassionema sp." ] <- "e" 

# MANUAL ENTER OF Fractal Dimension LETTERS 

letters_grad$Letter[ letters_grad$variable == "Fractal_Dimension" & letters_grad$SpeciesPct == "Chaetoceros sp." ] <- "a" 
letters_grad$Letter[ letters_grad$variable == "Fractal_Dimension" & letters_grad$SpeciesPct == "Ditylum sp." ] <- "b" 
letters_grad$Letter[ letters_grad$variable == "Fractal_Dimension" & letters_grad$SpeciesPct == "Melosira sp." ] <- "b" 
letters_grad$Letter[ letters_grad$variable == "Fractal_Dimension" & letters_grad$SpeciesPct == "Skeletonema sp." ] <- "c" 
letters_grad$Letter[ letters_grad$variable == "Fractal_Dimension" & letters_grad$SpeciesPct == "Thalassionema sp." ] <- "d" 

## 5. UPDATED FUNCTION TO ADD LETTERS
## ============================================================
add_letters_above <- function(p, dsub, metric, x_offset = 0) {
  
  df <- letters_grad[letters_grad$variable == metric, ]
  if (nrow(df) == 0) return(p)
  
  df$x <- as.numeric(df$SpeciesPct) + x_offset
  
  p +
    annotate(
      "text",
      x = df$x,
      y = Inf,           # always at top of panel
      vjust = -0.6,      # raise above panel
      label = df$Letter,
      size = 5.5,
      fontface = "bold",
      color = "black"
    ) +
    coord_cartesian(clip = "off")
}


vir_fun <- col_numeric(viridis(256, option="D"), domain=c(0,100))

create_violin2 <- function(dat_long, metric, y_label,
                           plot_tag="(a)", show_x_axis=TRUE,
                           ymax_limit = NULL, 
                           x_offset = 0) {
  
  dsub <- dat_long %>% filter(variable == metric)
  
  # Calculate the actual data range for positioning letters
  data_max <- max(dsub$value, na.rm = TRUE)
  
  ref <- dsub %>%
    filter(SpeciesPct %in% c("0%", "100%")) %>%
    group_by(SpeciesPct) %>%
    summarise(median_value = median(value), .groups="drop") %>%
    mutate(RefColor = SpeciesPct)
  
  p <- ggplot(dsub, aes(x = SpeciesPct, y = value, fill = Percent_numeric)) +
    geom_violin(scale="width", trim=TRUE, color="black", alpha=0.85) +
    stat_summary(fun=median, geom="crossbar", width=0.4)
  
  if (nrow(ref) > 0) {
    p <- p + geom_hline(
      data = ref,
      aes(yintercept = median_value, color = RefColor),
      linetype = "dashed",
      linewidth = 0.9
    )
  }
  
  p <- p +
    scale_fill_viridis_c(option="D", name="Treatment",
                         breaks=c(0,100),
                         labels=c(species0_label, species100_label)) +
    scale_color_manual(name="Reference",
                       values=c("0%"=vir_fun(0),"100%"=vir_fun(100)))
  
  # --- SET Y-AXIS LIMITS (this clips violins properly) ---
  if (!is.null(ymax_limit)) {
    p <- p + scale_y_continuous(limits = c(0, ymax_limit), expand = c(0, 0))
    # Add letters based on the VISIBLE range (ymax_limit)
    letter_df <- letters_grad[letters_grad$variable == metric, ]
    if (nrow(letter_df) > 0) {
      letter_df$x <- as.numeric(letter_df$SpeciesPct) + x_offset
      letter_df$y <- ymax_limit  # Position at top of visible range
      
      p <- p + geom_text(
        data = letter_df,
        aes(x = x, y = y, label = Letter),
        inherit.aes = FALSE,
        vjust = -0.6,
        size = 5.5,
        fontface = "bold",
        color = "black"
      )
    }
  }
  
  p <- p +
    labs(x=NULL, y=y_label, tag=plot_tag) +
    coord_cartesian(clip = "off") +
    theme_bw() +
    theme(
      legend.position="right",
      axis.text.x = if(show_x_axis) element_text(angle=45,hjust=1,size=16,color="black")
      else element_blank(),
      axis.text.y = element_text(size=16,color="black"),
      text        = element_text(size=18, family="Arial"),
      panel.grid  = element_blank(),
      plot.margin = margin(t=35, r=10, b=10, l=10)
    )
  
  return(p)
}
# ─────────────────────────────────────────────
# UPDATED Violin-plot function with proper clipping
# ─────────────────────────────────────────────
create_violinplot <- function(dat, metric, y_label, y_limits = NULL, tag = "",
                              show_x_labels = FALSE, letters_df = NULL, letter_size = 6,
                              add_points = FALSE, point_alpha = 0.35, point_size = 1.7) {
  dat_sub <- dat %>% filter(variable == metric)
  
  p <- ggplot(dat_sub, aes(x = Species, y = value, fill = Species)) +
    geom_violin(trim = FALSE, scale = "width", alpha = 0.8, color = "black") +
    # optional raw points
    { if (add_points) geom_jitter(width = 0.15, alpha = point_alpha, size = point_size) } +
    # median tick mark
    stat_summary(fun = median, geom = "point", shape = 95, size = 8, color = "black") +
    scale_fill_viridis_d(option = "D") +
    labs(y = y_label, x = NULL, tag = tag) +
    theme_bw() +
    theme(
      legend.position = "none",
      axis.text.x = if (show_x_labels) element_text(angle = 45, hjust = 1, vjust = 1, color = "black", size = 18) else element_blank(),
      axis.ticks.x = if (show_x_labels) element_line() else element_blank(),
      axis.text.y = element_text(color = "black"),
      axis.line   = element_line(color = "black"),
      text        = element_text(size = 20, color = "black"),
      panel.grid  = element_blank(),
      plot.tag    = element_text(face = "bold", size = 16),
      plot.margin = margin(t = 30, r = 10, b = 10, l = 10)  # Extra top margin for letters
    )
  
  # --- SET Y-AXIS LIMITS (this clips violins properly) ---
  if (!is.null(ymax_limit)) {
    p <- p + scale_y_continuous(limits = c(0, ymax_limit), expand = c(0, 0))
    # Add letters based on the VISIBLE range (ymax_limit)
    letter_df <- letters_grad[letters_grad$variable == metric, ]
    if (nrow(letter_df) > 0) {
      letter_df$x <- as.numeric(letter_df$SpeciesPct) + x_offset
      letter_df$y <- ymax_limit  # Position at top of visible range
      
      p <- p + geom_text(
        data = letter_df,
        aes(x = x, y = y, label = Letter),
        inherit.aes = FALSE,
        vjust = -0.6,
        size = 5.5,
        fontface = "bold",
        color = "black"
      )
    }
  }
      p <- p +
        geom_text(
          data = letters_df,
          aes(x = Species, y = ymax, label = Letter),
          inherit.aes = FALSE,
          vjust = -0.6,
          size = letter_size,
          fontface = "bold",
          color = "black"
        ) +
        coord_cartesian(clip = "off")
      
      return(p)
    }
  }
  
  # If no letters, just apply coord_cartesian for consistency
  p <- p + coord_cartesian(clip = "off")
  p
}

# UPDATED Scatterplot function with matching letter positioning
create_scatterplot <- function(fd_data, y_label, y_limits = NULL, tag = "",
                               show_x_labels = FALSE, letters_df = NULL,
                               letter_size = 6) {
  
  p <- ggplot(fd_data, aes(x = Species, y = Fractal_Dimension, color = Species)) +
    geom_point(size = 5) +
    scale_color_viridis_d(option = "D") +
    labs(y = y_label, x = NULL, tag = tag) +
    theme_bw() +
    theme(
      legend.position = "none",
      axis.text.x = if (show_x_labels) element_text(angle = 45, hjust = 1, vjust = 1,
                                                    color = "black", size = 18) else element_blank(),
      axis.ticks.x = if (show_x_labels) element_line() else element_blank(),
      axis.text.y = element_text(color = "black"),
      axis.line   = element_line(color = "black"),
      text        = element_text(size = 20, color = "black"),
      panel.grid  = element_blank(),
      plot.tag    = element_text(face = "bold", size = 16),
      plot.margin = margin(t = 30, r = 10, b = 10, l = 10)  # Extra top margin for letters
    )
  
  # Apply y-limits if specified
  if (!is.null(y_limits)) {
    ymin <- y_limits[1]
    ymax <- y_limits[2]
    p <- p + scale_y_continuous(limits = c(ymin, ymax), expand = c(0, 0))
  } else {
    ymin <- min(fd_data$Fractal_Dimension, na.rm = TRUE)
    ymax <- max(fd_data$Fractal_Dimension, na.rm = TRUE)
  }
  
  # Add letters if significant
  if (!is.null(letters_df)) {
    letters_sub <- letters_df %>%
      filter(variable == "Fractal_Dimension") %>%
      mutate(Species = factor(Species, levels = levels(fd_data$Species)))
    
    if (nrow(letters_sub) > 0) {
      # Position letters at the top of the y-limit range
      p <- p +
        geom_text(
          data = letters_df,
          aes(x = Species, y = ymax, label = Letter),
          inherit.aes = FALSE,
          vjust = -0.6,
          size = letter_size,
          fontface = "bold",
          color = "black"
        ) +
        coord_cartesian(clip = "off")
    }
  }
    
      p <- p +
        labs(x=NULL, y=y_label, tag=plot_tag) +
        coord_cartesian(clip = "off") +
        theme_bw() +
        theme(
          legend.position="right",
          axis.text.x = if(show_x_axis) element_text(angle=45,hjust=1,size=16,color="black")
          else element_blank(),
          axis.text.y = element_text(size=16,color="black"),
          text        = element_text(size=18, family="Arial"),
          panel.grid  = element_blank(),
          plot.margin = margin(t=35, r=10, b=10, l=10)
        )
    
      return(p)
}
  


# Build panels (now with proper clipping)
p1 <- create_violinplot(dat_long, "Equivalent_Spherical_Diameter_mm", "ESD (mm)", c(0, 4),  "(a)", letters_df = letters_long)
p2 <- create_violinplot(dat_long, "Perimeter_mm",                    "Perimeter (mm)", c(0, 15), "(b)", letters_df = letters_long)
p3 <- create_violinplot(dat_long, "Area_mm2",                        "Area (mm²)", c(0, 4),     "(c)", letters_df = letters_long)
p4 <- create_violinplot(dat_long, "Circularity",                     "Circularity", c(0, 1),    "(d)", letters_df = letters_long)
p5 <- create_violinplot(dat_long, "Aspect_Ratio",                    "Aspect Ratio", c(0, 6),   "(e)", letters_df = letters_long)
p6 <- create_violinplot(dat_long, "Porosity_percent",                "2D-Porosity (%)", c(0, 20),  "(f)", letters_df = letters_long)
p7 <- create_violinplot(dat_long, "Wobble",                          "Wobbliness", c(0, 15),        "(g)", show_x_labels = TRUE, letters_df = letters_long)

# FD scatter with updated function
p8 <- create_scatterplot(fd_by_species, "Fractal Dimension", c(0, 2), "(h)", show_x_labels = TRUE, letters_df = letters_long)


final_fig <- (p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8) +
  plot_layout(ncol = 2)

print(final_fig)

ggsave("Fig4_Aggs_Violins.jpg", final_fig, width = 14, height = 12, dpi = 300)


# Load required packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(scales)
library(patchwork)
library(viridis)

setwd("C:/Users/Cashies/OneDrive - University of Tasmania/AP_THESIS/Data")

# Read CSV file
all_data <- read.csv("agg_data.csv", header = TRUE)

all_data <- all_data %>%
  mutate(Porosity_correct = Total_Hole_Area_mm2 / Area_mm2)

# Selecting variables of interest and excluding dots
dat <- all_data %>%
  select(Species, Equivalent_Spherical_Diameter_mm, Perimeter_mm, Area_mm2, Circularity, Aspect_Ratio, Porosity_correct) %>%
  filter(Equivalent_Spherical_Diameter_mm >= 0.5,
         Area_mm2 > 0, Perimeter_mm > 0) %>% 
  mutate(
    Species = as.factor(Species),
    Porosity_percent = Porosity_correct * 100,
    Wobble = Perimeter_mm / Equivalent_Spherical_Diameter_mm,
    Fractal_Dimension = 2 * log10(Perimeter_mm) / log10(Area_mm2)
  ) %>%
  filter(Species %in% c("Chaetoceros sp.", "Ditylum sp.", "Melosira sp.", "Skeletonema sp.", "Thalassionema sp."))

# Regression-derived FD by species
fd_by_species <- dat %>%
  group_by(Species) %>%
  do({
    model <- lm(log10(Perimeter_mm) ~ log10(Area_mm2), data = .)
    slope <- coef(model)["log10(Area_mm2)"]
    data.frame(Fractal_Dimension = 2 * slope)
  })

# Reshape the data to long format
dat_long <- melt(dat, id.vars = "Species")

#####ADD SIGNIFICANCE LETTERS 

# Define species levels in the order they appear in your data
species_levels <- c("Chaetoceros sp.", "Ditylum sp.", "Melosira sp.", "Skeletonema sp.", "Thalassionema sp.")

metrics_to_test <- c(
  "Equivalent_Spherical_Diameter_mm",
  "Perimeter_mm",
  "Area_mm2",
  "Circularity",
  "Aspect_Ratio",
  "Porosity_percent",
  "Wobble",
  "Fractal_Dimension"
)

# Create dataframe with all combinations
letters_grad <- expand.grid(
  Species = factor(species_levels, levels = species_levels),
  variable = metrics_to_test,
  stringsAsFactors = FALSE
)

# MANUAL ENTER OF ESD LETTERS 

letters_grad$Letter[ letters_grad$variable == "Equivalent_Spherical_Diameter_mm" & letters_grad$Species == "Chaetoceros sp." ] <- "a" 
letters_grad$Letter[ letters_grad$variable == "Equivalent_Spherical_Diameter_mm" & letters_grad$Species == "Ditylum sp." ] <- "b" 
letters_grad$Letter[ letters_grad$variable == "Equivalent_Spherical_Diameter_mm" & letters_grad$Species == "Melosira sp." ] <- "c" 
letters_grad$Letter[ letters_grad$variable == "Equivalent_Spherical_Diameter_mm" & letters_grad$Species == "Skeletonema sp." ] <- "a" 
letters_grad$Letter[ letters_grad$variable == "Equivalent_Spherical_Diameter_mm" & letters_grad$Species == "Thalassionema sp." ] <- "d" 

# MANUAL ENTER OF PERIMETER LETTERS 

letters_grad$Letter[ letters_grad$variable == "Perimeter_mm" & letters_grad$Species == "Chaetoceros sp." ] <- "a" 
letters_grad$Letter[ letters_grad$variable == "Perimeter_mm" & letters_grad$Species == "Ditylum sp." ] <- "b" 
letters_grad$Letter[ letters_grad$variable == "Perimeter_mm" & letters_grad$Species == "Melosira sp." ] <- "c" 
letters_grad$Letter[ letters_grad$variable == "Perimeter_mm" & letters_grad$Species == "Skeletonema sp." ] <- "d" 
letters_grad$Letter[ letters_grad$variable == "Perimeter_mm" & letters_grad$Species == "Thalassionema sp." ] <- "e" 

# MANUAL ENTER OF AREA LETTERS 

letters_grad$Letter[ letters_grad$variable == "Area_mm2" & letters_grad$Species == "Chaetoceros sp." ] <- "a" 
letters_grad$Letter[ letters_grad$variable == "Area_mm2" & letters_grad$Species == "Ditylum sp." ] <- "b" 
letters_grad$Letter[ letters_grad$variable == "Area_mm2" & letters_grad$Species == "Melosira sp." ] <- "c" 
letters_grad$Letter[ letters_grad$variable == "Area_mm2" & letters_grad$Species == "Skeletonema sp." ] <- "a" 
letters_grad$Letter[ letters_grad$variable == "Area_mm2" & letters_grad$Species == "Thalassionema sp." ] <- "d" 

# MANUAL ENTER OF Circularity LETTERS 

letters_grad$Letter[ letters_grad$variable == "Circularity" & letters_grad$Species == "Chaetoceros sp." ] <- "a" 
letters_grad$Letter[ letters_grad$variable == "Circularity" & letters_grad$Species == "Ditylum sp." ] <- "b" 
letters_grad$Letter[ letters_grad$variable == "Circularity" & letters_grad$Species == "Melosira sp." ] <- "c" 
letters_grad$Letter[ letters_grad$variable == "Circularity" & letters_grad$Species == "Skeletonema sp." ] <- "d" 
letters_grad$Letter[ letters_grad$variable == "Circularity" & letters_grad$Species == "Thalassionema sp." ] <- "e" 

# MANUAL ENTER OF Aspect Ratio LETTERS 

letters_grad$Letter[ letters_grad$variable == "Aspect_Ratio" & letters_grad$Species == "Chaetoceros sp." ] <- "a" 
letters_grad$Letter[ letters_grad$variable == "Aspect_Ratio" & letters_grad$Species == "Ditylum sp." ] <- "b" 
letters_grad$Letter[ letters_grad$variable == "Aspect_Ratio" & letters_grad$Species == "Melosira sp." ] <- "c" 
letters_grad$Letter[ letters_grad$variable == "Aspect_Ratio" & letters_grad$Species == "Skeletonema sp." ] <- "d" 
letters_grad$Letter[ letters_grad$variable == "Aspect_Ratio" & letters_grad$Species == "Thalassionema sp." ] <- "e" 

# MANUAL ENTER OF Porosity LETTERS 

letters_grad$Letter[ letters_grad$variable == "Porosity_percent" & letters_grad$Species == "Chaetoceros sp." ] <- "a" 
letters_grad$Letter[ letters_grad$variable == "Porosity_percent" & letters_grad$Species == "Ditylum sp." ] <- "b" 
letters_grad$Letter[ letters_grad$variable == "Porosity_percent" & letters_grad$Species == "Melosira sp." ] <- "b" 
letters_grad$Letter[ letters_grad$variable == "Porosity_percent" & letters_grad$Species == "Skeletonema sp." ] <- "c" 
letters_grad$Letter[ letters_grad$variable == "Porosity_percent" & letters_grad$Species == "Thalassionema sp." ] <- "d" 

# MANUAL ENTER OF Wobble LETTERS 

letters_grad$Letter[ letters_grad$variable == "Wobble" & letters_grad$Species == "Chaetoceros sp." ] <- "a" 
letters_grad$Letter[ letters_grad$variable == "Wobble" & letters_grad$Species == "Ditylum sp." ] <- "b" 
letters_grad$Letter[ letters_grad$variable == "Wobble" & letters_grad$Species == "Melosira sp." ] <- "c" 
letters_grad$Letter[ letters_grad$variable == "Wobble" & letters_grad$Species == "Skeletonema sp." ] <- "d" 
letters_grad$Letter[ letters_grad$variable == "Wobble" & letters_grad$Species == "Thalassionema sp." ] <- "e" 

# MANUAL ENTER OF Fractal Dimension LETTERS 

letters_grad$Letter[ letters_grad$variable == "Fractal_Dimension" & letters_grad$Species == "Chaetoceros sp." ] <- "a" 
letters_grad$Letter[ letters_grad$variable == "Fractal_Dimension" & letters_grad$Species == "Ditylum sp." ] <- "b" 
letters_grad$Letter[ letters_grad$variable == "Fractal_Dimension" & letters_grad$Species == "Melosira sp." ] <- "b" 
letters_grad$Letter[ letters_grad$variable == "Fractal_Dimension" & letters_grad$Species == "Skeletonema sp." ] <- "c" 
letters_grad$Letter[ letters_grad$variable == "Fractal_Dimension" & letters_grad$Species == "Thalassionema sp." ] <- "d" 

# Create letters_long for use in plotting functions
letters_long <- letters_grad

# ─────────────────────────────────────────────
# UPDATED Violin-plot function with proper clipping
# ─────────────────────────────────────────────
create_violinplot <- function(dat, metric, y_label, y_limits = NULL, tag = "",
                              show_x_labels = FALSE, letters_df = NULL, letter_size = 6,
                              add_points = FALSE, point_alpha = 0.35, point_size = 1.7) {
  dat_sub <- dat %>% filter(variable == metric)
  
  p <- ggplot(dat_sub, aes(x = Species, y = value, fill = Species)) +
    geom_violin(trim = FALSE, scale = "width", alpha = 0.8, color = "black") +
    # optional raw points
    { if (add_points) geom_jitter(width = 0.15, alpha = point_alpha, size = point_size) } +
    # median tick mark
    stat_summary(fun = median, geom = "point", shape = 95, size = 8, color = "black") +
    scale_fill_viridis_d(option = "D") +
    labs(y = y_label, x = NULL, tag = tag) +
    theme_bw() +
    theme(
      legend.position = "none",
      axis.text.x = if (show_x_labels) element_text(angle = 45, hjust = 1, vjust = 1, color = "black", size = 18) else element_blank(),
      axis.ticks.x = if (show_x_labels) element_line() else element_blank(),
      axis.text.y = element_text(color = "black"),
      axis.line   = element_line(color = "black"),
      text        = element_text(size = 20, color = "black"),
      panel.grid  = element_blank(),
      plot.tag    = element_text(face = "bold", size = 16),
      plot.margin = margin(t = 30, r = 10, b = 10, l = 10)
    )
  
  # Apply y-limits if specified (this clips the violins properly)
  if (!is.null(y_limits)) {
    ymin <- y_limits[1]
    ymax <- y_limits[2]
    p <- p + scale_y_continuous(limits = c(ymin, ymax), expand = c(0, 0))
  } else {
    ymin <- min(dat_sub$value, na.rm = TRUE)
    ymax <- max(dat_sub$value, na.rm = TRUE)
  }
  
  # Add letters if provided and significant for this metric
  if (!is.null(letters_df)) {
    letters_sub <- letters_df %>%
      filter(variable == metric) %>%
      mutate(Species = factor(Species, levels = levels(dat_sub$Species)))
    
    if (nrow(letters_sub) > 0) {
      # Position letters at the top of the y-limit range
      p <- p +
        geom_text(
          data = letters_sub,
          aes(x = Species, y = ymax, label = Letter),
          inherit.aes = FALSE,
          vjust = -0.6,
          size = letter_size,
          fontface = "bold",
          color = "black"
        ) +
        coord_cartesian(clip = "off")
      
      return(p)
    }
  }
  
  # If no letters, just apply coord_cartesian for consistency
  p <- p + coord_cartesian(clip = "off")
  p
}

# UPDATED Scatterplot function with matching letter positioning
create_scatterplot <- function(fd_data, y_label, y_limits = NULL, tag = "",
                               show_x_labels = FALSE, letters_df = NULL,
                               letter_size = 6) {
  
  p <- ggplot(fd_data, aes(x = Species, y = Fractal_Dimension, color = Species)) +
    geom_point(size = 5) +
    scale_color_viridis_d(option = "D") +
    labs(y = y_label, x = NULL, tag = tag) +
    theme_bw() +
    theme(
      legend.position = "none",
      axis.text.x = if (show_x_labels) element_text(angle = 45, hjust = 1, vjust = 1,
                                                    color = "black", size = 18) else element_blank(),
      axis.ticks.x = if (show_x_labels) element_line() else element_blank(),
      axis.text.y = element_text(color = "black"),
      axis.line   = element_line(color = "black"),
      text        = element_text(size = 20, color = "black"),
      panel.grid  = element_blank(),
      plot.tag    = element_text(face = "bold", size = 16),
      plot.margin = margin(t = 30, r = 10, b = 10, l = 10)
    )
  
  # Apply y-limits if specified
  if (!is.null(y_limits)) {
    ymin <- y_limits[1]
    ymax <- y_limits[2]
    p <- p + scale_y_continuous(limits = c(ymin, ymax), expand = c(0, 0))
  } else {
    ymin <- min(fd_data$Fractal_Dimension, na.rm = TRUE)
    ymax <- max(fd_data$Fractal_Dimension, na.rm = TRUE)
  }
  
  # Add letters if significant
  if (!is.null(letters_df)) {
    letters_sub <- letters_df %>%
      filter(variable == "Fractal_Dimension") %>%
      mutate(Species = factor(Species, levels = levels(fd_data$Species)))
    
    if (nrow(letters_sub) > 0) {
      # Position letters at the top of the y-limit range
      p <- p +
        geom_text(
          data = letters_sub,
          aes(x = Species, y = ymax, label = Letter),
          inherit.aes = FALSE,
          vjust = -0.6,
          size = letter_size,
          fontface = "bold",
          color = "black"
        ) +
        coord_cartesian(clip = "off")
      
      return(p)
    }
  }
  
  # If no letters, just apply coord_cartesian for consistency
  p <- p + coord_cartesian(clip = "off")
  p
}

# Build panels (now with proper clipping)
p1 <- create_violinplot(dat_long, "Equivalent_Spherical_Diameter_mm", "ESD (mm)", c(0, 4),  "(a)", letters_df = letters_long)
p2 <- create_violinplot(dat_long, "Perimeter_mm",                    "Perimeter (mm)", c(0, 15), "(b)", letters_df = letters_long)
p3 <- create_violinplot(dat_long, "Area_mm2",                        "Area (mm²)", c(0, 4),     "(c)", letters_df = letters_long)
p4 <- create_violinplot(dat_long, "Circularity",                     "Circularity", c(0, 1),    "(d)", letters_df = letters_long)
p5 <- create_violinplot(dat_long, "Aspect_Ratio",                    "Aspect Ratio", c(0, 6),   "(e)", letters_df = letters_long)
p6 <- create_violinplot(dat_long, "Porosity_percent",                "2D-Porosity (%)", c(0, 20),  "(f)", letters_df = letters_long)
p7 <- create_violinplot(dat_long, "Wobble",                          "Wobbliness", c(0, 15),        "(g)", show_x_labels = TRUE, letters_df = letters_long)

# FD scatter with updated function
p8 <- create_scatterplot(fd_by_species, "Fractal Dimension", c(0, 2), "(h)", show_x_labels = TRUE, letters_df = letters_long)

final_fig <- (p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8) +
  plot_layout(ncol = 2)


ggsave("Fig4_Aggs_Violins.jpg", final_fig, width = 14, height = 12, dpi = 300)
