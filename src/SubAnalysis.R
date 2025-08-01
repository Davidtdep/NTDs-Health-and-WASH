################################################################################
# Neglected Tropical Diseases (NTDs) Research Impact Analysis
# Description: Analyzes the relationship between scientific publications and 
#              health indicators across different income groups
################################################################################

#===============================================================================
# 1. LOAD REQUIRED PACKAGES
#===============================================================================

library(readxl)       # For reading Excel files
library(dplyr)        # For data manipulation
library(tidyr)        # For data tidying
library(ggplot2)      # For data visualization
library(RColorBrewer) # For color palettes
library(forcats)      # For factor manipulation

#===============================================================================
# 2. DATA LOADING AND PREPARATION
#===============================================================================

# Load datasets from supplementary materials
regression <- read.csv("~/NTDs-Health-and-WASH/data/Supplementary Material 1.csv")
meta_analysis <- read.csv("~/NTDs-Health-and-WASH/data/Supplementary Material 2.csv")
meta_regression <- read.csv("~/NTDs-Health-and-WASH/data/Supplementary Material 3.csv")

# Define indicator categories for filtering and classification
health_system_indicators <- c(
  "OUT.OF.POCKED.EXPENDITURE.ON.HEALTH",
  "CURRENT.HEALTH.EXPENDITURE....OF.GDP.",
  "HEALTHCARE.ACCESS.AND.QUALITY",
  "PHYSICIANS..PER.1.000.PEOPLE.",
  "NURSES.AND.MIDWIVES..PER.1.000.PEOPLE."
)

# Updated R&D indicators - removed two drug pipeline indicators
research_dev_indicators <- c(
  "RESEARCH.AND.DEVELOPMENT.EXPENDITURE....OF.GDP.",
  "Distribution.of.R.D.funding.flows.for.neglected.diseases.by.country",
  "drugs.pipeline.for.neglected.tropical.diseases.by.country",
  "higher.education.institutions.offering.disciplines.related.to.research.for.health.in.2023.by.region",
  "CHARGES.FOR.THE.USE.OF.INTELLECTUAL.PROPERTY..PAYMENTS..BOP..CURRENT.US.."
)

# Updated order of WASH indicators
wash_indicators <- c(
  "Share.of.people.practicing.open.defecation",
  "Share.of.deaths.attributed.to.unsafe.sanitation",
  "Share.of.the.population.with.access.to.basic.handwashing.facilities",
  "Share.of.the.population.using.basic.sanitation.service",
  "Share.of.the.population.using.safely.managed.drinking.water.sources"
)

# Combine all indicators for filtering
all_indicators <- c(health_system_indicators, research_dev_indicators, wash_indicators)

# Filter regression data to include only specified indicators
filtered_regression <- regression %>%
  filter((dependent_var %in% all_indicators | independent_var %in% all_indicators))

#===============================================================================
# 3. DATA TRANSFORMATION FOR VISUALIZATION
#===============================================================================

# Create mapping for clean indicator names with updated wording
indicator_names <- c(
  # Health System and Healthcare Access
  "OUT.OF.POCKED.EXPENDITURE.ON.HEALTH" = "Out-of-Pocket Health Expenditure",
  "CURRENT.HEALTH.EXPENDITURE....OF.GDP." = "Current Health Expenditure (% of GDP)",
  "HEALTHCARE.ACCESS.AND.QUALITY" = "Healthcare Access and Quality",
  "PHYSICIANS..PER.1.000.PEOPLE." = "Physicians per 1,000 population",
  "NURSES.AND.MIDWIVES..PER.1.000.PEOPLE." = "Nurses and Midwives per 1,000 people",
  
  # Research and Development
  "RESEARCH.AND.DEVELOPMENT.EXPENDITURE....OF.GDP." = "Research and Development Expenditure (% of GDP)",
  "Distribution.of.R.D.funding.flows.for.neglected.diseases.by.country" = "R&D funding flows specifically for neglected diseases",
  "drugs.pipeline.for.neglected.tropical.diseases.by.country" = "Drug pipeline for NTDs by country",
  "higher.education.institutions.offering.disciplines.related.to.research.for.health.in.2023.by.region" = "Higher education institutions offering health research disciplines",
  "CHARGES.FOR.THE.USE.OF.INTELLECTUAL.PROPERTY..PAYMENTS..BOP..CURRENT.US.." = "Charges for the use of intellectual property, payments (BOP, current US$)",
  
  # Water, Sanitation, and Hygiene
  "Share.of.people.practicing.open.defecation" = "Share of people practicing open defecation",
  "Share.of.deaths.attributed.to.unsafe.sanitation" = "Deaths attributed to unsafe sanitation",
  "Share.of.the.population.with.access.to.basic.handwashing.facilities" = "Access to basic handwashing facilities",
  "Share.of.the.population.using.basic.sanitation.service" = "Basic sanitation services",
  "Share.of.the.population.using.safely.managed.drinking.water.sources" = "Access to safely managed drinking water sources"
)

# Process regression data for visualization
filtered_regression <- filtered_regression %>%
  # Determine which variable to plot
  mutate(
    plot_var = case_when(
      independent_var == "publication_count" ~ dependent_var,
      dependent_var == "publication_count" ~ independent_var,
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(plot_var)) %>%
  group_by(plot_var) %>%
  mutate(
    # Rank by actual value (not absolute value)
    coef_rank = rank(estimate, ties.method = "min"),
    # Convert ranks to factor levels (1 = lowest value, 4 = highest value)
    rank_category = factor(coef_rank,
                           levels = 1:4,
                           labels = c("Lowest", "Low", "High", "Highest")),
    # Add clean names for indicators
    indicator_name = indicator_names[plot_var],
    # Add direction indicator
    direction = ifelse(estimate >= 0, "Positive", "Negative")
  ) %>%
  ungroup()

# Create category mapping with clean names - updated names to match
category_mapping <- data.frame(
  indicator_name = c(
    # Health System indicators
    "Out-of-Pocket Health Expenditure",
    "Current Health Expenditure (% of GDP)",
    "Healthcare Access and Quality",
    "Physicians per 1,000 population",
    "Nurses and Midwives per 1,000 people",
    
    # Research and Development indicators
    "Research and Development Expenditure (% of GDP)",
    "R&D funding flows specifically for neglected diseases",
    "Drug pipeline for NTDs by country",
    "Higher education institutions offering health research disciplines",
    "Charges for the use of intellectual property, payments (BOP, current US$)",
    
    # WASH indicators
    "Share of people practicing open defecation",
    "Deaths attributed to unsafe sanitation",
    "Access to basic handwashing facilities",
    "Basic sanitation services",
    "Access to safely managed drinking water sources"
  ),
  category = c(
    rep("Health System and Healthcare Access", 5),
    rep("Research and Development", 5),
    rep("Water, Sanitation, and Hygiene", 5)
  )
)

# Merge category labels with data
filtered_regression <- filtered_regression %>%
  left_join(category_mapping, by = "indicator_name")

# Add significance symbols based on p-values
filtered_regression <- filtered_regression %>%
  mutate(significance = case_when(
    p_value < 0.001 ~ "***",
    p_value < 0.01 ~ "**",
    p_value < 0.05 ~ "*",
    TRUE ~ ""
  ))

# Define income group abbreviations
income_abbreviations <- c(
  "High income" = "HIC",
  "Upper middle income" = "UMIC",
  "Lower middle income" = "LMIC",
  "Low income" = "LIC"
)

# Apply income abbreviations
filtered_regression$income_abbrev <- income_abbreviations[filtered_regression$income]

# Create a label for each cell that includes significance
filtered_regression <- filtered_regression %>%
  mutate(cell_label = paste0(significance))

#===============================================================================
# 4. DATA PREPARATION FOR VISUALIZATION
#===============================================================================

# Create a mapping of dependent vs independent variables - updated with new names
# and marking Out-of-Pocket and Current Health Expenditure as dependent
variable_type_mapping <- data.frame(
  indicator_name = c(
    # Health System indicators
    "Out-of-Pocket Health Expenditure",
    "Current Health Expenditure (% of GDP)",
    "Healthcare Access and Quality",
    "Physicians per 1,000 population",
    "Nurses and Midwives per 1,000 people",
    
    # Research and Development indicators
    "Research and Development Expenditure (% of GDP)",
    "R&D funding flows specifically for neglected diseases",
    "Drug pipeline for NTDs by country",
    "Higher education institutions offering health research disciplines",
    "Charges for the use of intellectual property, payments (BOP, current US$)",
    
    # WASH indicators
    "Share of people practicing open defecation",
    "Deaths attributed to unsafe sanitation",
    "Access to basic handwashing facilities",
    "Basic sanitation services",
    "Access to safely managed drinking water sources"
  ),
  is_dependent = c(
    # Health System indicators
    TRUE,  # Out-of-Pocket Health Expenditure - Changed to TRUE
    TRUE,  # Current Health Expenditure - Changed to TRUE
    TRUE,  # Healthcare Access and Quality
    FALSE, # Physicians
    FALSE, # Nurses and Midwives
    
    # Research and Development indicators
    FALSE, # Research and Development Expenditure
    FALSE, # R&D funding flows
    TRUE,  # Drug pipeline by country
    FALSE, # Higher education institutions
    FALSE, # Charges for the use of intellectual property
    
    # WASH indicators
    TRUE, # Share of people practicing open defecation
    TRUE, # Deaths attributed to unsafe sanitation
    TRUE, # Access to basic handwashing facilities
    TRUE, # Basic sanitation services
    TRUE  # Access to safely managed drinking water sources
  )
)

# Define order of categories
category_order <- c(
  "Health System and Healthcare Access", 
  "Research and Development", 
  "Water, Sanitation, and Hygiene"
)

# Create a custom order for indicators within each category
health_indicator_order <- c(
  "Out-of-Pocket Health Expenditure",
  "Current Health Expenditure (% of GDP)",
  "Healthcare Access and Quality",
  "Physicians per 1,000 population",
  "Nurses and Midwives per 1,000 people"
)

rd_indicator_order <- c(
  "Research and Development Expenditure (% of GDP)",
  "R&D funding flows specifically for neglected diseases",
  "Drug pipeline for NTDs by country",
  "Higher education institutions offering health research disciplines",
  "Charges for the use of intellectual property, payments (BOP, current US$)"
)

wash_indicator_order <- c(
  "Share of people practicing open defecation",
  "Deaths attributed to unsafe sanitation",
  "Access to basic handwashing facilities",
  "Basic sanitation services",
  "Access to safely managed drinking water sources"
)

# Combine all indicators in the desired order
all_indicators_ordered <- c(health_indicator_order, rd_indicator_order, wash_indicator_order)

# Order indicators within each category and add dependency markers
# Using a superscript "d" as a marker
plot_data <- filtered_regression %>%
  left_join(variable_type_mapping, by = "indicator_name") %>%
  mutate(
    category = factor(category, levels = category_order),
    # Add a superscript d to dependent variables 
    indicator_label = ifelse(!is.na(is_dependent) & is_dependent, paste0(indicator_name, " ᵈ"), indicator_name)
  ) %>%
  # Use factor to set custom order
  mutate(indicator_ordered = factor(indicator_label, 
                                    levels = c(
                                      # Health indicators with dependency markers
                                      ifelse(health_indicator_order %in% unique(indicator_name[is_dependent]), 
                                             paste0(health_indicator_order, " ᵈ"), 
                                             health_indicator_order),
                                      # R&D indicators with dependency markers
                                      ifelse(rd_indicator_order %in% unique(indicator_name[is_dependent]), 
                                             paste0(rd_indicator_order, " ᵈ"), 
                                             rd_indicator_order),
                                      # WASH indicators with dependency markers
                                      ifelse(wash_indicator_order %in% unique(indicator_name[is_dependent]), 
                                             paste0(wash_indicator_order, " ᵈ"), 
                                             wash_indicator_order)
                                    )))

# Create dummy data for legends (unchanged)
dummy_data <- data.frame(
  x = c(1,1,1,1,1,1),
  y = c(1,2,3,4,5,6),
  direction = factor(c("Positive", "Negative", "Positive", "Negative", "Positive", "Negative")),
  rank_category = factor(rep(c("Highest", "High", "Low"), each=2), 
                         levels=c("Lowest", "Low", "High", "Highest"))
)

#===============================================================================
# 5. VISUALIZATION
#===============================================================================

# Create the heatmap visualization with dependency indicators
plot <- ggplot() +
  # Base plot with data
  geom_tile(data = subset(plot_data, estimate >= 0),
            aes(x = factor(income_abbrev, levels = c("HIC", "UMIC", "LMIC", "LIC")), 
                y = indicator_ordered, fill = rank_category), 
            color = "white", size = 0.5) +
  geom_point(data = subset(plot_data, estimate < 0),
             aes(x = factor(income_abbrev, levels = c("HIC", "UMIC", "LMIC", "LIC")), 
                 y = indicator_ordered, fill = rank_category), 
             color = "white", size = 6, shape = 21) +
  # Add significance symbols
  geom_text(data = plot_data,
            aes(x = factor(income_abbrev, levels = c("HIC", "UMIC", "LMIC", "LIC")), 
                y = indicator_ordered, label = significance), 
            size = 2.5) +
  
  # Dummy geoms for legend
  geom_point(data = subset(dummy_data, direction == "Positive"),
             aes(x = x, y = y, fill = rank_category, shape = direction),
             color = "black", size = 4, alpha = 0) +
  geom_point(data = subset(dummy_data, direction == "Negative"),
             aes(x = x, y = y, fill = rank_category, shape = direction),
             color = "black", size = 4, alpha = 0) +
  
  # Color and shape scales
  scale_fill_brewer(palette = "RdBu", direction = -1) +
  scale_shape_manual(values = c("Positive" = 22, "Negative" = 21)) +
  
  # Facets and labels
  facet_grid(category ~ ., scales = "free_y", space = "free_y") +
  labs(
    x = "Country Income Classification",
    y = "",
    fill = "Coefficient\nMagnitude",
    shape = "Direction") +
  
  # Configure legends
  guides(
    fill = guide_legend(override.aes = list(shape = 22, size = 5), order = 1),
    shape = guide_legend(override.aes = list(size = 5, fill = "gray70"), order = 2)
  ) +
  
  # Theme customization
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    axis.text.y = element_text(size = 8, hjust = 1),
    axis.text.x = element_text(size = 9, face = "bold", angle = 45, hjust = 1),
    strip.text.y = element_text(angle = 0, face = "bold", hjust = 0),
    strip.background = element_rect(fill = "grey95", color = NA),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    legend.box = "vertical",  # Stack legends vertically
    plot.caption = element_text(hjust = 0, face = "italic", size = 8) # Style for the caption
  )

# Print the plot
print(plot) 