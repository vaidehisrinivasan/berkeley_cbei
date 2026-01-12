# R/viz_objects.R
# Builds plotly objects: pie_chart, choropleth, income, distance_mean
# Assumes cleaned_data.RData is in your project root (same folder as the .qmd), adjust path if needed.
# -------------------------------------------------------------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(sf)
  library(scales)
  library(plotly)
  library(dplyr)
  library(jsonlite)
  library(htmlwidgets)
})

load("cleaned_data.RData")


# Annotation (used in all plots) --------------------------------------------------------------------------------------------------
ucb_exclusion_annotation <- list(
  list(
    x = 1, y = 0.03,
    xref = "paper", yref = "paper",
    xanchor = "right", yanchor = "bottom",
    text = "ZIP code 94720 (UC Berkeley campus) is excluded due to limited household data availability.",
    showarrow = FALSE,
    font = list(size = 11, color = "#666666"),
    align = "right"
  )
)

# VIZ 1: PIE CHART ====================================================================================================================

berkeley_category_shares <- berkeley_category_shares %>%
  arrange(desc(share_pct))

pie_chart <- plot_ly(
  data = berkeley_category_shares,
  labels = ~paste0("<b>", category, "</b>"),
  values = ~share_pct,
  type = "pie",
  sort = FALSE,
  direction = "clockwise",
  domain = list(y = c(0.12, 1)),
  width = 600,
  height = 600,
  autosize = FALSE,
  textinfo = "label+percent",
  textposition = "inside",
  insidetextorientation = "horizontal",
  textfont = list(
    family = "Inter, Helvetica, Arial, sans-serif",
    size = 15,
    color = "white"
  ),
  marker = list(
    colors = ecodatalab_colors,
    line = list(color = "white", width = 1)
  ),
  hoverinfo = "text",
  text = ~paste0(
    "<b>", category, "</b><br>",
    ifelse(
      diff_pp > 0,
      paste0(
        "<span style='color:#8B0000;'>↑ ",
        round(diff_pp, 1),
        " percentage points</span> vs. the average U.S. household"
      ),
      paste0(
        "<span style='color:#1F78B4;'>↓ ",
        round(abs(diff_pp), 1),
        " percentage points</span> vs. the average U.S. household"
      )
    )
  )
) %>%
  layout(
    title = list(
      x = 0.5,
      xanchor = "center"
    ),
    showlegend = FALSE,
    margin = list(l = 80, r = 80, t = 50, b = 50),
    annotations = ucb_exclusion_annotation,
    font = list(
      family = "Inter, Helvetica, Arial, sans-serif",
      color = "#222222"
    ),
    hoverlabel = list(
      bgcolor = "rgba(255,255,255,0.96)",
      bordercolor = "#CCCCCC",
      font = list(
        family = "Inter, Helvetica, Arial, sans-serif",
        size = 12,
        color = "#222222"
      ),
      align = "left"
    )
  ) %>%
  config(displayModeBar = FALSE)
  

# Helper views ================================================================================================================

if (is.null(names(ecodatalab_colors))) {
  ecodatalab_colors <- setNames(ecodatalab_colors, berkeley_category_shares$category)
}

make_pie_view <- function(highlight = NULL, fade_alpha = 0.25, pull_amt = 0.08) {
  cats <- berkeley_category_shares$category
  base_cols <- unname(ecodatalab_colors[cats])

  if (is.null(highlight)) {
    cols <- base_cols
    pull <- rep(0, length(cats))
  } else {
    highlight <- as.character(highlight)
    keep <- cats %in% highlight
}
  plot_ly(
    data = berkeley_category_shares,
    labels = ~paste0("<b>", category, "</b>"),
    values = ~share_pct,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    domain = list(y = c(0.12, 1)),
    width = 600,
    height = 600,
    autosize = FALSE,
    textinfo = "label+percent",
    textposition = "inside",
    insidetextorientation = "horizontal",
    textfont = list(
      family = "Inter, Helvetica, Arial, sans-serif",
      size = 15,
      color = "white"
    ),
    marker = list(
      colors = cols,
      line = list(color = "white", width = 1)
    ),
    pull = pull,
    hoverinfo = "text",
    text = ~paste0(
      "<b>", category, "</b><br>",
      ifelse(
        diff_pp > 0,
        paste0(
          "<span style='color:#8B0000;'>↑ ",
          round(diff_pp, 1),
          " percentage points</span> vs. the average U.S. household"
        ),
        paste0(
          "<span style='color:#1F78B4;'>↓ ",
          round(abs(diff_pp), 1),
          " percentage points</span> vs. the average U.S. household"
        )
      )
    )
  ) %>%
    layout(
      title = list(x = 0.5, xanchor = "center"),
      showlegend = FALSE,
      margin = list(l = 80, r = 80, t = 50, b = 50),
      annotations = ucb_exclusion_annotation,
      font = list(
        family = "Inter, Helvetica, Arial, sans-serif",
        color = "#222222"
      ),
      hoverlabel = list(
        bgcolor = "rgba(255,255,255,0.96)",
        bordercolor = "#CCCCCC",
        font = list(
          family = "Inter, Helvetica, Arial, sans-serif",
          size = 12,
          color = "#222222"
        ),
        align = "left"
      )
    ) %>%
    config(displayModeBar = FALSE)
}

# Base view 
pie_chart <- make_pie_view(NULL)

# Single-category highlights 
pie_transport <- make_pie_view("Transportation")
pie_housing   <- make_pie_view("Housing")
pie_food      <- make_pie_view("Food")

# Goods + Services highlight together 
cats_all <- berkeley_category_shares$category
goods_cats    <- cats_all[stringr::str_detect(cats_all, regex("goods", ignore_case = TRUE))]
services_cats <- cats_all[stringr::str_detect(cats_all, regex("service", ignore_case = TRUE))]
pie_goods_services <- make_pie_view(unique(c(goods_cats, services_cats)))

# VIZ 2: CHOROPLETH MAP ====================================================================================================================

berkeley_shapes <- berkeley_shapes %>%
  st_transform(4326) %>%
  mutate(ZCTA5CE20 = as.character(ZCTA5CE20))

tmp_geojson <- tempfile(fileext = ".geojson")
st_write(berkeley_shapes, tmp_geojson, delete_dsn = TRUE, quiet = TRUE)
berkeley_geojson <- read_json(tmp_geojson, simplifyVector = FALSE)

choropleth <- plot_geo(
  width = 750,
  height = 750
) %>%
  add_trace(
    type = "choropleth",
    geojson = berkeley_geojson,
    locations = berkeley_shapes$ZCTA5CE20,
    featureidkey = "properties.ZCTA5CE20",
    z = berkeley_shapes$emissions_pc,
    zmin = quantile(berkeley_shapes$emissions_pc, 0.05, na.rm = TRUE),
    zmax = quantile(berkeley_shapes$emissions_pc, 0.95, na.rm = TRUE),
    zauto = FALSE,
    autocolorscale = FALSE,
    colorscale = list(
      c(0, "#E6F2F3"),
      c(0.3, "#A8DADC"),
      c(0.6, "#457B9D"),
      c(1, "#1D3557")
    ),
    marker = list(line = list(color = "white", width = 1.2)),
    hoverinfo = "text",
    text = paste0(
      "<b>ZIP ", berkeley_shapes$ZCTA5CE20, "</b><br>",
      ifelse(
        berkeley_shapes$emissions_pc > mean(berkeley_shapes$emissions_pc, na.rm = TRUE),
        "Above-average",
        "Below-average"
      ),
      " emissions<br>",
      "<b>", comma(round(berkeley_shapes$emissions_pc, 0)), " kg CO₂e</b> per capita<br>",
      "Median income: <b>$", comma(round(berkeley_shapes$median_household_income, 0)), "</b>"
    ),
    colorbar = list(
      title = list(
        text = "Emissions<br>(kg CO₂e)",
        font = list(size = 10)
      ),
      tickformat = ",",
      tickfont = list(size = 9),
      len = 0.4,
      thickness = 12,
      x = 0.85,
      xanchor = "right",
      y = 0.5,
      yanchor = "middle",
      outlinewidth = 0
    )
  ) %>%
  layout(
    autosize = FALSE,
    margin = list(l = 20, r = 20, t = 0, b = 20),
    title = list(
      text = "<b>Are these emissions uniform across Berkeley?</b>",
      x = 0.5,
      xanchor = "center",
      font = list(size = 16)
    ),
    geo = list(
      fitbounds = "locations",
      visible = FALSE,
      projection = list(scale = 100)
    ),
    annotations = ucb_exclusion_annotation,
    font = list(
      family = "Inter, Helvetica, Arial, sans-serif",
      color = "#222222"
    ),
    hoverlabel = list(
      bgcolor = "rgba(255,255,255,0.98)",
      bordercolor = "#DDDDDD",
      font = list(
        family = "Inter, Helvetica, Arial, sans-serif",
        size = 12,
        color = "#222222"
      ),
      align = "left"
    ),
    paper_bgcolor = "white",
    plot_bgcolor = "white"
  ) %>%
  config(displayModeBar = FALSE, scrollZoom = FALSE)

# VIZ 3: INCOME VS EMISSIONS ===============================================================================================================
scatter_data <- berkeley_shapes %>%
  st_drop_geometry() %>%
  filter(!is.na(median_household_income), !is.na(emissions_pc))

m <- lm(emissions_pc ~ median_household_income, data = scatter_data)
scatter_data$fitted_emissions <- fitted(m)

income <- plot_ly(data = scatter_data) %>%
  add_markers(
    x = ~median_household_income,
    y = ~emissions_pc,
    hoverinfo = "text",
    text = ~paste0(
      "<b>ZIP ", ZCTA5CE20, "</b><br>",
      "Median income: <b>$", comma(round(median_household_income, 0)), "</b><br>",
      "Per-capita emissions: <b>", comma(round(emissions_pc, 1)), " tCO₂e</b>"
    ),
    marker = list(
      size = 14,
      color = ~emissions_pc,
      colorscale = list(
        c(0.00, "#2EC4B6"),
        c(0.33, "#FFD166"),
        c(0.66, "#EF476F"),
        c(1.00, "#8338EC")
      ),
      showscale = FALSE,
      opacity = 0.9,
      line = list(color = "white", width = 1.6)
    )
  ) %>%
  add_lines(
    x = ~median_household_income,
    y = ~fitted_emissions,
    line = list(
      color = "rgba(0,0,0,0.45)",
      width = 1.25,
      dash = "dot"
    ),
    hoverinfo = "skip",
    showlegend = FALSE
  ) %>%
  layout(
    title = list(
      x = 0.5,
      xanchor = "center"
    ),
    xaxis = list(
      title = "Median Household Income ($)",
      tickformat = "$,",
      zeroline = FALSE,
      showgrid = FALSE,
      ticks = "outside",
      linecolor = "#DDDDDD"
    ),
    yaxis = list(
      title = "Per-capita Emissions (tCO₂e)",
      tickformat = ",",
      zeroline = FALSE,
      showgrid = FALSE,
      ticks = "outside",
      linecolor = "#DDDDDD"
    ),
    font = list(
      family = "Inter, Helvetica, Arial, sans-serif",
      color = "#222222"
    ),
    plot_bgcolor = "white",
    paper_bgcolor = "white",
    hoverlabel = list(
      bgcolor = "rgba(255,255,255,0.96)",
      bordercolor = "#CCCCCC",
      font = list(size = 12, color = "#222222"),
      align = "left"
    ),
    margin = list(t = 70, b = 60, l = 60, r = 40)
  ) %>%
  config(displayModeBar = FALSE)

# VIZ 4: DECOMPOSITION ===============================================================================================================
city_avg <- berkeley_emissions_dash %>%
  group_by(category) %>%
  summarise(
    city_avg_emissions_pc = mean(emissions_pc, na.rm = TRUE),
    .groups = "drop"
  )

decomp_data <- berkeley_emissions_dash %>%
  left_join(city_avg, by = "category") %>%
  mutate(deviation = emissions_pc - city_avg_emissions_pc)

zip_order <- decomp_data %>%
  group_by(zip) %>%
  summarise(total_deviation = sum(deviation, na.rm = TRUE), .groups = "drop") %>%
  arrange(total_deviation) %>%
  pull(zip)

decomp_data <- decomp_data %>%
  mutate(zip = factor(zip, levels = zip_order))

category_colors <- c(
  "Housing" = "#D73027",
  "Transportation" = "#1F78B4",
  "Food" = "#33A02C",
  "Goods" = "#FF7F00",
  "Services" = "#6A3D9A"
)

distance_mean <- plot_ly(
  data = decomp_data,
  x = ~deviation,
  y = ~zip,
  type = "bar",
  orientation = "h",
  color = ~category,
  colors = category_colors,
  hoverinfo = "text",
  text = ~paste0("<b>", category, "</b><br>", comma(round(deviation, 0)), " kg CO₂e"),
  textposition = "none"
) %>%
  layout(
    barmode = "relative",
    hovermode = "closest",
    title = list(
      text = "<b>What Drives Differences in Per-Capita Carbon Emissions Across Berkeley ZIP Codes?</b>",
      x = 0.5,
      xanchor = "center"
    ),
    xaxis = list(
      title = "Deviation from Berkeley average (kg tCO₂e per capita)",
      zeroline = TRUE,
      zerolinecolor = "#BBBBBB",
      zerolinewidth = 1,
      gridcolor = "#EEEEEE"
    ),
    yaxis = list(
      title = "",
      automargin = TRUE,
      tickfont = list(size = 11)
    ),
    font = list(
      family = "Inter, Helvetica, Arial, sans-serif",
      color = "#222222"
    ),
    hoverlabel = list(
      bgcolor = "#2B2B2B",
      bordercolor = "#2B2B2B",
      font = list(size = 12, color = "#FFFFFF"),
      align = "left"
    )
  ) %>%
  config(displayModeBar = FALSE)


# Base view 
pie_chart <- make_pie_view(NULL)

# Single-category highlights 
pie_transport <- make_pie_view("Transportation")
pie_housing   <- make_pie_view("Housing")
pie_food      <- make_pie_view("Food")

# Goods + Services highlight together 
cats_all <- berkeley_category_shares$category
goods_cats    <- cats_all[stringr::str_detect(cats_all, regex("goods", ignore_case = TRUE))]
services_cats <- cats_all[stringr::str_detect(cats_all, regex("service", ignore_case = TRUE))]
pie_goods_services <- make_pie_view(unique(c(goods_cats, services_cats)))

# VIZ 2: CHOROPLETH MAP ====================================================================================================================
berkeley_shapes <- berkeley_shapes %>%
  st_transform(4326) %>%
  mutate(ZCTA5CE20 = as.character(ZCTA5CE20))

tmp_geojson <- tempfile(fileext = ".geojson")
st_write(berkeley_shapes, tmp_geojson, delete_dsn = TRUE, quiet = TRUE)
berkeley_geojson <- read_json(tmp_geojson, simplifyVector = FALSE)

choropleth <- plot_geo() %>%
  add_trace(
    type = "choropleth",
    geojson = berkeley_geojson,
    locations = berkeley_shapes$ZCTA5CE20,
    featureidkey = "properties.ZCTA5CE20",
    z = berkeley_shapes$emissions_pc,
    zmin = quantile(berkeley_shapes$emissions_pc, 0.05, na.rm = TRUE),
    zmax = quantile(berkeley_shapes$emissions_pc, 0.95, na.rm = TRUE),
    zauto = FALSE,
autocolorscale = FALSE,
    colorscale = list(
      c(0, "#E6F2F3"),
      c(0.3, "#A8DADC"),
      c(0.6, "#457B9D"),
      c(1, "#1D3557")
    ),
    marker = list(line = list(color = "white", width = 0.8)),
    hoverinfo = "text",
    text = paste0(
      "ZIP ", berkeley_shapes$ZCTA5CE20,
      " has ",
      ifelse(
        berkeley_shapes$emissions_pc > mean(berkeley_shapes$emissions_pc, na.rm = TRUE),
        "above-average",
        "below-average"
      ),
      " per-capita household carbon emissions,<br>",
      "at approximately <b>", comma(round(berkeley_shapes$emissions_pc, 0)), " kg CO₂e</b> per person.<br>",
      "The median household income in this area is <b>$", comma(round(berkeley_shapes$median_household_income, 0)), "</b>."
    ),
    colorbar = list(
      title = "Per-capita emissions<br>(kg CO₂e)",
      tickformat = ",",
      titlefont = list(size = 11),
      tickfont = list(size = 10)
    )
  ) %>%
  layout( 
  paper_bgcolor = "rgba(0,0,0,0)",  # Makes the outer canvas transparent
    plot_bgcolor = "rgba(0,0,0,0)",   # Makes the drawing area transparent
    margin = list(l = 0, r = 0, t = 0, b = 0),
    title = list(
      x = 0.5,
      xanchor = "center"
    ),
    geo = list(
      fitbounds = "locations",
      visible = FALSE
    ),
    annotations = ucb_exclusion_annotation,
    font = list(
      family = "Inter, Helvetica, Arial, sans-serif",
      color = "#222222"
    ),
    hoverlabel = list(
      bgcolor = "rgba(255,255,255,0.96)",
      bordercolor = "#CCCCCC",
      font = list(
        family = "Inter, Helvetica, Arial, sans-serif",
        size = 12,
        color = "#222222"
      ),
      align = "left"
    )
  ) %>%
  config(displayModeBar = FALSE, scrollZoom = FALSE)
  

# VIZ 3: INCOME VS EMISSIONS ===============================================================================================================
scatter_data <- berkeley_shapes %>%
  st_drop_geometry() %>%
  filter(!is.na(median_household_income), !is.na(emissions_pc))

m <- lm(emissions_pc ~ median_household_income, data = scatter_data)
scatter_data$fitted_emissions <- fitted(m)

income <- plot_ly(data = scatter_data) %>%
  add_markers(
    x = ~median_household_income,
    y = ~emissions_pc,
    hoverinfo = "text",
    text = ~paste0(
      "<b>ZIP ", ZCTA5CE20, "</b><br>",
      "Median income: <b>$", comma(round(median_household_income, 0)), "</b><br>",
      "Per-capita emissions: <b>", comma(round(emissions_pc, 1)), " tCO₂e</b>"
    ),
    marker = list(
      size = 14,
      color = ~emissions_pc,
      colorscale = list(
        c(0.00, "#2EC4B6"),
        c(0.33, "#FFD166"),
        c(0.66, "#EF476F"),
        c(1.00, "#8338EC")
      ),
      showscale = FALSE,
      opacity = 0.9,
      line = list(color = "white", width = 1.6)
    )
  ) %>%
  add_lines(
    x = ~median_household_income,
    y = ~fitted_emissions,
    line = list(
      color = "rgba(0,0,0,0.45)",
      width = 1.25,
      dash = "dot"
    ),
    hoverinfo = "skip",
    showlegend = FALSE
  ) %>%
  layout(
    title = list(
      x = 0.5,
      xanchor = "center"
    ),
    xaxis = list(
      title = "Median Household Income ($)",
      tickformat = "$,",
      zeroline = FALSE,
      showgrid = FALSE,
      ticks = "outside",
      linecolor = "#DDDDDD"
    ),
    yaxis = list(
      title = "Per-capita Emissions (tCO₂e)",
      tickformat = ",",
      zeroline = FALSE,
      showgrid = FALSE,
      ticks = "outside",
      linecolor = "#DDDDDD"
    ),
    font = list(
      family = "Inter, Helvetica, Arial, sans-serif",
      color = "#222222"
    ),
    plot_bgcolor = "white",
    paper_bgcolor = "white",
    hoverlabel = list(
      bgcolor = "rgba(255,255,255,0.96)",
      bordercolor = "#CCCCCC",
      font = list(size = 12, color = "#222222"),
      align = "left"
    ),
    margin = list(t = 70, b = 60, l = 60, r = 40)
  ) %>%
  config(displayModeBar = FALSE)


# VIZ 4: DECOMPOSITION ===============================================================================================================
city_avg <- berkeley_emissions_dash %>%
  group_by(category) %>%
  summarise(
    city_avg_emissions_pc = mean(emissions_pc, na.rm = TRUE),
    .groups = "drop"
  )

decomp_data <- berkeley_emissions_dash %>%
  left_join(city_avg, by = "category") %>%
  mutate(deviation = emissions_pc - city_avg_emissions_pc)

zip_order <- decomp_data %>%
  group_by(zip) %>%
  summarise(total_deviation = sum(deviation, na.rm = TRUE), .groups = "drop") %>%
  arrange(total_deviation) %>%
  pull(zip)

decomp_data <- decomp_data %>%
  mutate(zip = factor(zip, levels = zip_order))

category_colors <- c(
  "Housing" = "#D73027",
  "Transportation" = "#1F78B4",
  "Food" = "#33A02C",
  "Goods" = "#FF7F00",
  "Services" = "#6A3D9A"
)

distance_mean <- plot_ly(
  data = decomp_data,
  x = ~deviation,
  y = ~zip,
  type = "bar",
  orientation = "h",
  color = ~category,
  colors = category_colors,
  hoverinfo = "text",
  text = ~paste0("<b>", category, "</b><br>", comma(round(deviation, 0)), " kg CO₂e"),
  textposition = "none"
) %>%
  layout(
    barmode = "relative",
    hovermode = "closest",
    title = list(
      x = 0.5,
      xanchor = "center"
    ),
    xaxis = list(
      title = "Deviation from Berkeley average (kg tCO₂e per capita)",
      zeroline = TRUE,
      zerolinecolor = "#BBBBBB",
      zerolinewidth = 1,
      showgrid = FALSE
    ),
    yaxis = list(
      title = "",
      automargin = TRUE,
      tickfont = list(size = 11)
    ),
    legend = list(
      x = 0.98,              # Position inside plot on right
      y = 0.30,              # Top right corner
      xanchor = "right",
      yanchor = "top",
      bgcolor = "rgba(255,255,255,0.9)",  # Semi-transparent white background
      bordercolor = "#DDDDDD",
      borderwidth = 1
    ),
    font = list(
      family = "Inter, Helvetica, Arial, sans-serif",
      color = "#222222"
    ),
    hoverlabel = list(
      bgcolor = "#2B2B2B",
      bordercolor = "#2B2B2B",
      font = list(size = 12, color = "#FFFFFF"),
      align = "left"
    )
  ) %>%
  config(displayModeBar = FALSE)