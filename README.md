<img src="man/figures/mapagen_logo.svg" align="right" height="70" alt="mapagen logo" />

# mapagen 

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

**An Agentic Framework for Geospatial Analysis and Visualization in R**.

## Overview

In a typical geospatial data science workflow, analysts spend a disproportionate amount of time iterating between data exploration, parameterizing visualizations, and adjusting design styles. While Large Language Models (LLMs) like Gemini offer immense potential to act as "analytical copilots," the current bottleneck is the *context handoff*. Constantly copying and pasting metadata, data schemas, and plots between RStudio and an LLM web interface breaks the state of flow, introduces errors, and causes a loss of traceability.

`mapagen` solves this problem by providing an end-to-end agentic framework directly within R. It standardizes project scaffolding, automatically catalogs spatial and tabular metadata, and programmatically communicates with LLMs from your console via the `ellmer` package. By dynamically injecting project objectives, data contexts, and corporate styling rules into the LLM prompt, `mapagen` accelerates insight discovery, manages iterative figure generation, and ensures the seamless assembly of reproducible Quarto reports.

## Key Features

*   **Standardized Scaffolding (`init_agent_project`):** Instantly generates a clean directory structure, default Quarto templates, and a centralized `style.json` to enforce typographic and color consistency across all outputs.
*   **Geospatial Repomix (`build_context`):** Automatically scans your data directories to build a comprehensive data catalog (`context_metadata.json`). It extracts column schemas from tabular data, alongside bounding boxes, CRS information, and geometry types from spatial files (`sf`, `terra`) without bloating memory.
*   **Iteration Engine & Figure Registry (`export_iteration`):** A wrapper for `ggplot2` that automatically exports your plots, extracts the underlying numerical summary data to a JSON file, and updates a tracking registry (`figures_registry.json`) for full traceability and seamless Quarto integration.
*   **Agentic LLM Bridge (`ask_code`, `ask_narrative`):** Seamlessly communicates with Large Language Models (like Gemini) via the `ellmer` package. It dynamically injects your project's objectives, style rules, and data catalog to generate highly contextualized R code and analytical narratives directly from the console.

## Installation

You can install the development version of `mapagen` directly from GitHub using the `remotes` package. You will also need to install the `ellmer` package to enable the LLM agent capabilities.

```r
# Install remotes if you haven't already
# install.packages("remotes")

# Install ellmer for LLM communication
install.packages("ellmer")

# Install mapagen from GitHub
remotes::install_github("ricardoochoasosa/mapagen")
```

## Setup & Authentication

`mapagen` relies on the `ellmer` package to communicate with Large Language Models. By default, the package is configured to use Google's Gemini API. 

To allow the agent to authenticate silently in the background, you must expose your API key as an environment variable.

Open your R environment file. The easiest way is using the `usethis` package:

```r
# install.packages("usethis")
usethis::edit_r_environ()

# Add your Gemini API token on a new line:
GEMINI_API_KEY="your_actual_api_key_here"

```

Save the `.Renviron` file and restart your R session for the variable to take 
effect.

## Quick Start

This example demonstrates how to initialize a project, build a spatial data catalog, and ask the LLM agent to orchestrate a geospatial workflow using `sf` and `ggplot2`.

```r
# Load the core ecosystem
library(mapagen)
library(sf)
library(ggplot2)

# Initialize the project structure
init_agent_project(path = "cozumel_walkability")
setwd("cozumel_walkability")

# IMPORTANT: Place your raw spatial files (e.g., children.geojson, schools.geojson) 
# into the newly created 'data/raw/' folder before proceeding.

# Build the spatial and tabular context catalog
build_context()

# 1. Define the natural language prompt
my_prompt <- "Generate a spatial analysis to evaluate pedestrian accessibility to schools in Cozumel. 
1. Read the 'children.geojson' and 'schools.geojson' layers. Ensure both are in the project's preferred metric CRS for accurate spatial calculations.
2. Create an 800-meter buffer around all schools and unify the resulting geometries into a single coverage polygon.
3. Identify the census polygons (children) that intersect with this school coverage.
4. Generate a map overlaying the school coverage, school points, and census population.
5. Crucial: Strictly use mapagen's styling functions (theme_from_json, scale_fill_mapagen, add_cartographic_decorations) and export the final map using export_iteration(), registering it with the ID 'cozumel_walkability_map'."

# 2. Send the prompt to the LLM agent
# Note: This might take a few seconds while Gemini processes the context.
agent_response <- ask_code(prompt = my_prompt)

# 3. Define the target path for the generated script
script_path <- "scripts/01_cozumel_analysis.R"

# 4. Save the generated R code to the script file
writeLines(agent_response$code, script_path)

# 5. Execute the generated script to create and export the map
source(script_path)

# 6. Request an analytical narrative based on the numerical data exported by the figure
narrative <- ask_narrative(id = "cozumel_walkability_map")

# 7. Print the resulting narrative
cat(narrative)
```

## Project Structure

Running `init_agent_project("project_name")` scaffolds a standardized directory structure designed to keep your spatial data, scripts, and outputs strictly organized:

```text
project_name/
├── data/
│   ├── raw/                 # Place your raw geospatial and tabular files here
│   └── processed/           # Save clean, intermediate data here
├── output/
│   ├── figures/             # Automatically populated by export_iteration()
│   └── figures_registry.json # The tracking log for all generated figures
├── scripts/                 # Ejected scripts and R code generated by the LLM
├── R/                       # Custom local R functions
├── project_concept.md       # Your project's objectives and context for the LLM
├── style.json               # Centralized rules for colors, typography, and mapping
├── report.qmd               # A Quarto template ready to compile your findings
├── _quarto.yml              # Quarto project configuration
└── .gitignore               # Pre-configured to ignore sensitive or heavy files
```

## Quarto Integration (Reporting)

`mapagen` is designed to make reproducible reporting effortless. Since every plot generated with `export_iteration()` is automatically logged in the `figures_registry.json`, you don't need to manually keep track of image paths, versions, or captions.

Inside your Quarto (`.qmd`) document, simply use the `include_figure()` helper function within an R code chunk. It will dynamically format the image, its caption, and the data source citation based on your `style.json` settings.

Ensure you set the chunk option `#| results: asis` so Quarto renders the Markdown properly.

````markdown
```{r}
#| label: fig-walkability
#| echo: false
#| results: asis

mapagen::include_figure(id = "cozumel_walkability_map")
```
````

## License and Contact

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for more details.

**Author:** Ricardo Ochoa Sosa and Socorro Román
**Bug Reports & Feature Requests:** If you encounter any issues or have suggestions for improvements, please open an issue on the [GitHub repository](https://github.com/ricardoochoasosa/mapagen/issues).
