# Neglected Tropical Diseases Research Impact Analysis

This repository contains scripts and data for analyzing the global landscape of neglected tropical diseases (NTDs) research output in relation to Health System, R&D and WASH indicators.

## Repository Structure

The repository is organized as follows:

- **src/** - R scripts for analysis
  - MainAnalysis.R - Core statistical analyses
  - SubAnalysis.R - Plotting
- **data/** - Supplementary materials and datasets
  - Supplementary Material 1.csv - Regression results for selected indicators
  - Supplementary Material 2.csv - Meta-analysis results
  - Supplementary Material 3.csv - Meta-regression results


## Study Overview

This project analyzes 107,251 research articles on neglected tropical diseases published between 1904-2024, exploring relationships between scientific output and Health System, R&D and WASH indicators across countries with different income levels.

## Data Sources

- Bibliometric data from systematic literature search on NTDs
- 75 country-level indicators from public databases
- Countries classified by World Bank income groups (HIC, UMIC, LMIC, LIC)

## Analysis Framework

The analysis follows a three-tiered approach:

1. **Regression Analysis** (`MainAnalysis.R`): Linear regression models examining relationships between publication counts and health indicators within each income group.

2. **Meta-Analysis** (`MainAnalysis.R`): Random-effects meta-analyses synthesizing evidence across income groups, with heterogeneity assessment.

3. **Meta-Regression** (`MainAnalysis.R`): Identifying potential moderators explaining heterogeneity in effect sizes across income groups.

4. **Visualization** (`SubAnalysis.R`): Geospatial maps and heatmaps categorizing findings by disease domain.

## Supplementary Materials

- **SM1**: Regression results for the selected disease indicators
- **SM2**: Meta-analysis results for the selected indicators
- **SM3**: Complete meta-regression results

## Requirements

- R version 4.0+ with packages:
  - Data processing: readxl, dplyr, tidyr, stringdist
  - Visualization: ggplot2, RColorBrewer, rworldmap, cowplot
  - Analysis: meta, metafor, scales

## Usage

1. Install required R packages
2. Place input data in appropriate locations (paths may need adjustment)
3. Run scripts in order: bibliometric.R → MainAnalysis.R → SubAnalysis.R

For detailed information about the methodology and results, please refer to the associated publication.

## Data Availability
This project uses openly available data from public databases. The mental health bibliometric dataset used is **available upon reasonable request**.
  
## License
This repository is licensed under the **MIT License**, allowing free use, modification, and distribution with attribution. See `LICENSE` file for more details.
