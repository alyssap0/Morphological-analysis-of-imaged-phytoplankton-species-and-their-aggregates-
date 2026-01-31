library(dplyr)
library(tidyr)
library(rstatix)
library(multcompView)

# ---- Load your data ----
agg_data <- read.csv("agg_data.csv")

# Filter and prepare data
agg_filtered <- agg_data %>%
  mutate(
    Porosity_percent  = Porosity * 100,
    Wobble            = Perimeter_mm / Equivalent_Spherical_Diameter_mm,
    Fractal_Dimension = 2 * log10(Perimeter_mm) / log10(Area_mm2)
  ) %>%
  filter( 
    !Microcosm.No. %in% c("M2_D97C3.1", "M4_D96C4", "M9_D97C3.2")) %>%
    mutate(
      Species = factor(Species)   # 🔑 THIS is the critical fix
    ) %>%
      ungroup()

# ---- Define metrics to test ----
metrics_to_test <- c("Wobble",
  "Fractal_Dimension",
  "Equivalent_Spherical_Diameter_mm",
  "Perimeter_mm",
  "Area_mm2",
  "Aspect_Ratio",
  "Circularity",
  "Porosity_percent"
)

# ---- Reshape data to long format ----
dat_long <- agg_filtered %>%
  select(Species, all_of(metrics_to_test)) %>%
  pivot_longer(cols = all_of(metrics_to_test),
               names_to = "variable",
               values_to = "value")

# ---- Create ordered factor for Species ----
# Get unique species in your preferred order
# ---- Define the order of species and gradients ----
#main_species <- c("Chaetoceros sp.","Ditylum sp.","Melosira sp.","Skeletonema sp.","Thalassionema sp.")
#gradient_one <- c("Melosira sp.", "M85C15", "M49C51", "M46C54", "M41C59", "M37C63", "M32C67", "M6C94", "Chaetoceros sp.")
gradient_two <- c("Skeletonema sp.", "S99N1", "S88N12", "S82N18", "S64N36", "S49N61", "S24N76", "S1N100", "Thalassionema sp.")  # add other gradients in desired order if needed
#species_order <- c(main_species)
#species_order <- c(gradient_one)
species_order <- c(gradient_two)

# ---- Apply factor levels ----
dat_long$Species <- factor(dat_long$Species, levels = species_order)

# ---- Run Kruskal–Wallis + Dunn's test with robust letter extraction ----

letters_list    <- list()
results_summary <- list()

for (var in metrics_to_test) {
  
  cat("\n===============================================\n")
  cat("Testing:", var, "\n")
  cat("===============================================\n")
  
  # Subset data for this variable and drop NA values
  dsub <- dat_long %>%
    filter(variable == var, !is.na(value)) %>%
    droplevels()
  
  if (nrow(dsub) == 0 || length(unique(dsub$Species)) < 2) {
    cat("Not enough data for", var, "\n")
    next
  }
  
  # ---- Kruskal–Wallis test ----
  kw <- kruskal_test(dsub, value ~ Species)
  
  results_summary[[var]] <- data.frame(
    Variable    = var,
    Chi_squared = kw$statistic,
    df          = kw$df,
    p_value     = kw$p
  )
  
  cat("Kruskal–Wallis p-value:", kw$p, "\n")
  
  # ---- If NOT significant → all observed groups get same letter ----
  if (kw$p >= 0.05) {
    
    letters_list[[var]] <- data.frame(
      Species  = unique(dsub$Species),
      Letter   = "a",
      Variable = var,
      stringsAsFactors = FALSE
    )
    
    cat("Not significant → all groups assigned 'a'\n")
    next
  }
  
  # ---- Dunn's post-hoc test ----
  cat("Significant → running Dunn's test\n")
  
  dunn <- dunn_test(
    dsub,
    value ~ Species,
    p.adjust.method = "BH"
  )
  
  # ---- Preserve intended species order ----
  groups <- intersect(
    species_order,
    unique(c(dunn$group1, dunn$group2))
  )
  
  # ---- Build symmetric p-value matrix ----
  pmat <- matrix(
    1,
    nrow = length(groups),
    ncol = length(groups),
    dimnames = list(groups, groups)
  )
  
  for (i in seq_len(nrow(dunn))) {
    g1 <- dunn$group1[i]
    g2 <- dunn$group2[i]
    p  <- dunn$p.adj[i]
    
    if (g1 %in% groups && g2 %in% groups) {
      pmat[g1, g2] <- p
      pmat[g2, g1] <- p
    }
  }
  
  diag(pmat) <- 1
  pmat[lower.tri(pmat)] <- t(pmat)[lower.tri(pmat)]
  
  # ---- Compact letter display ----
  letters <- multcompLetters(pmat, threshold = 0.05)$Letters
  
  letters_list[[var]] <- data.frame(
    Species  = factor(names(letters), levels = species_order),
    Letter   = unname(letters),
    Variable = var,
    stringsAsFactors = FALSE
  )
  
  cat("Letters assigned successfully\n")
}

# ---- Combine all results ----
all_letters <- bind_rows(letters_list)
all_results <- bind_rows(results_summary)

# ---- Save to CSV ----
write.csv(all_letters, "significance_lettersgradienttwo.csv", row.names = FALSE)
hwrite.csv(all_results, "kruskal_wallis_results.csv", row.names = FALSE)

# ---- View results ----
cat("\n\n========== KRUSKAL-WALLIS SUMMARY ==========\n")
print(all_results)

cat("\n\n========== SIGNIFICANCE LETTERS ==========\n")
print(all_letters)

# ---- Create wide format table (easier to read) ----
letters_wide <- all_letters %>%
  pivot_wider(names_from = Variable, values_from = Letter)

write.csv(letters_wide, "significance_letters_wide.csv", row.names = FALSE)

cat("\n\nWide format letters:\n")
print(letters_wide)

cat("\n\n✓ Results saved to:\n")
cat("  - significance_letters.csv (long format)\n")
cat("  - significance_letters_wide.csv (wide format - one row per species)\n")
cat("  - kruskal_wallis_results.csv (test statistics)\n")