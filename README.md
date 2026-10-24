<img src="man/figures/mapagen_logo.svg" align="right" height="70" alt="mapagen logo" />

# mapagen

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

**A Deterministic Spatial Toolbox for AI Agents in R**.

## Overview

Modern AI agent architectures (e.g., OpenAI Code Interpreter, Claude Computer Use, LangChain, AutoGen, and Model Context Protocol servers) operate on a fundamental principle: **the Agent is the orchestrator**, and domain-specific code engines act as **specialized execution toolboxes**.

`mapagen` provides an atomic, deterministic R toolbox specifically engineered for AI agents performing spatial data science, mapping (`sf`, `terra`, `ggplot2`), and reporting. By stripping out heavy monolithic dependencies (no Shiny, no internal LLM clients), `mapagen` focuses on what R does best:
1. Providing lightweight data inspection without consuming large token budgets.
2. Offering spatial guardrails and linters (CRS compatibility, topological validity, bounding box overlap).
3. Enforcing consistent cartographic styles and palettes.
4. Managing figure iterations with serialized numerical support for full analytical traceability.

## Key Modules & Tools

### 1. Project Scaffolding & Guidelines (`01_scaffold.R`, `02_styles.R`)
* **`init_agent_project(path)`**: Sets up a standardized directory tree (`data/raw`, `data/processed`, `output/figures`, `scripts`, `R`) and foundational templates (`style.json`, `project_concept.md`, `_quarto.yml`).
* **`get_project_rules(format)`**: Extracts project guidelines, language, preferred metric CRS, and conventions in `list`, `json`, or `markdown` for agent prompt injection.
* **`get_preferred_crs(type)`**: Queries the target metric projected CRS (e.g. `EPSG:6362`) or geographic CRS.

### 2. Context Engine & Lazy Inspection (`03_context.R`)
* **`list_project_data(dir_path)`**: Rapid, lightweight inventory of project files and sizes without loading heavy geometries or rasters into memory.
* **`scan_vector(x)`**, **`scan_raster(x)`**, **`scan_tabular(x)`**: Atomic inspection tools returning structured schemas, CRS details, bounding boxes, and column samples. Accepts either file paths or in-memory R objects (`sf`, `SpatRaster`, `data.frame`).
* **`build_context()` & `summary_context(format)`**: Builds or summarizes the complete data catalog in structured list, pure JSON, or concise Markdown.

### 3. Spatial Guardrails & Linters (`05_spatial_tools.R`)
* **`check_crs_match(x, y)`**: Verifies whether two spatial layers share identical coordinate reference systems, warning if reprojection (`st_transform`) is required.
* **`check_is_projected(x)`**: Evaluates if a layer is in a metric projected CRS, preventing agents from erroneously calculating distances or buffers on angular degrees (EPSG:4326).
* **`validate_geometries(x)`**: Runs topological validity checks (`st_is_valid`), counts invalid/empty geometries, and suggests `st_make_valid()` when self-intersections or bowtie polygons are detected.
* **`check_spatial_overlap(x, y)`**: Confirms bounding box intersection between two layers before performing spatial joins (`st_join`, `st_intersection`), avoiding empty operation results.
* **`lint_map(plot)`**: Heuristically inspects `ggplot` objects to verify presence of scale bars, north arrows, titles, and captions.

### 4. Figure Registry & Iteration Engine (`04_figures.R`)
* **`export_iteration(plot, id, caption, summary_data)`**: Exports figures to PNG (and vector PDF if final), extracts accompanying numerical metrics to JSON, and updates `figures_registry.json`.
* **`list_figures()`**: Summary table of all registered figures, versions, and artifact availability.
* **`get_figure_data(id, version)`**: Reads the serialized numerical support data directly into R for narrative construction.
* **`update_figure_metadata(id, version, ...)`**: Updates captions, sources, or publication status (`draft`, `reviewed`, `final`).
* **`delete_figure_version(id, version, delete_files)`**: Deletes an iteration entry and optionally cleans up orphaned disk artifacts.
* **`include_figure(id, version)`**: Dynamic helper for Quarto (`.qmd`) documents using `#| results: asis`.

## Installation

Install the development version of `mapagen` directly from GitHub:

```r
# install.packages("remotes")
remotes::install_github("ricardoochoasosa/mapagen")
```

## Agent Integration Workflow Example

Here is how an autonomous agent or analyst invokes `mapagen` tools inside an R execution environment:

```r
library(mapagen)
library(sf)
library(ggplot2)

# 1. Inspect existing project data lightly
data_inventory <- list_project_data("data")
print(data_inventory)

# 2. Inspect a specific vector layer
schools_meta <- scan_vector("data/raw/schools.geojson")
print(schools_meta$layers[[1]]$spatial_metadata)

# 3. Guardrail: Validate CRS and metric suitability before running spatial buffers
crs_check <- check_is_projected("data/raw/schools.geojson")
if (!crs_check$is_projected) {
  message(crs_check$message)
  # Reproject to preferred project metric CRS
  target_crs <- get_preferred_crs()
}

# 4. Guardrail: Validate geometries before spatial intersection
geom_val <- validate_geometries("data/raw/census_blocks.geojson")
if (!geom_val$valid) {
  message(geom_val$suggestion)
}

# 5. Build and lint a styled cartographic map
# (Assume schools and buffer layers loaded in memory)
p <- ggplot() +
  # ... geometries ...
  theme_from_json() +
  add_cartographic_decorations() +
  labs(title = "Pedestrian Access to Schools", caption = "Source: Census 2020")

# Check map completeness
map_lint <- lint_map(p)
print(map_lint$is_compliant)

# 6. Export and register the iteration
res <- export_iteration(
  plot = p,
  id = "school_walkability",
  caption = "Pedestrian 800m walkability buffer around schools",
  sources_id = "census_2020",
  summary_data = data.frame(accessible_pop = 24500, pct_coverage = 78.4)
)

# 7. Retrieve serialized numerical metrics later for reporting
num_data <- get_figure_data("school_walkability", version = "latest")
print(num_data$data)
```

## Quarto Integration

In Quarto (`.qmd`) documents, include figures dynamically by ID:

````markdown
```{r}
#| label: fig-walkability
#| echo: false
#| results: asis

mapagen::include_figure(id = "school_walkability")
```
````

## License and Contact

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for more details.

**Author:** Ricardo Ochoa Sosa and Socorro Román  
**Bug Reports & Feature Requests:** [GitHub Issues](https://github.com/ricardoochoasosa/mapagen/issues)
