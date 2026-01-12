# Consumption-Based Emissions in Berkeley, California (ZIP-Level Dashboard)

An interactive, scroll-friendly Quarto dashboard that explores modeled **consumption-based greenhouse gas emissions** across **nine Berkeley ZIP codes**. The project visualizes how emissions vary by neighborhood and how patterns differ across major household spending categories.

## What this project does

- Estimates and visualizes consumption-based emissions at the ZIP-code level for Berkeley, California.
- Uses a scrollytelling layout (Quarto **closeread**) with interactive visuals.
- Includes interactive charts and a ZIP-level choropleth map.
- Excludes **ZIP 94720 (UC Berkeley campus)** due to limited household data availability.

## Outputs

- **Quarto HTML dashboard** (scroll narrative + sticky interactive visuals)
- Interactive Plotly charts (including a category breakdown / pie-style view)
- An interactive ZIP-level choropleth map of per-capita emissions

## Tech stack

**Languages**
- R
- CSS
- HTML (Quarto markup + inline elements)

**Core frameworks**
- Quarto (`closeread-html`)
- Shiny runtime (Quarto `runtime: shiny`)

**R packages used in this project (from the code)**
- `tidyverse`, `dplyr`, `ggplot2`
- Spatial: `sf`, `tigris`
- Census/ACS: `tidycensus`
- Visualization: `plotly`, `leaflet`, `scales`
- Data + utilities: `readr`, `readxl`, `janitor`, `here`, `jsonlite`

## Skills demonstrated (what this repo showcases)

- **Data wrangling in R**: cleaning, reshaping, joining, and validating ZIP-level datasets with `tidyverse`.
- **Census / ACS data retrieval**: pulling ZIP-code-tabulation-area metrics (e.g., income, population) using `tidycensus`.
- **Geospatial analysis & mapping**: working with shapefiles, coordinate transforms, GeoJSON export, and ZIP-level choropleths (`sf`, `tigris`, `jsonlite`).
- **Interactive data visualization**: building Plotly charts with custom annotations and interactive map layers.
- **Dashboard engineering**: integrating Quarto + Shiny runtime for responsive interactivity.
- **Front-end polish**: custom CSS for a clean, editorial scrollytelling experience (including local/self-hosted fonts).

## Repository notes (high level)

- A Plotly object-building script exists (e.g., `viz_objects.R`) that constructs interactive visuals such as `pie_chart` and `choropleth`.
- A cleaned dataset is loaded from a serialized R object (e.g., `cleaned_data.RData`) in the visualization-building workflow.

## How to run locally

### 1) Install prerequisites
- R (recent version)
- Quarto

Install required R packages (example):
```r
install.packages(c(
  "tidyverse","readxl","tidycensus","janitor","here","readr",
  "sf","tigris","plotly","scales","leaflet","shiny","sass","jsonlite"
))
```

## Notes

This dashboard is based on modeled estimates and is intended for exploratory analysis and visualization. See the Technical Appendix for data sources, assumptions, and limitations.
