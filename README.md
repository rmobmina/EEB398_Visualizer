# 🌾 Dominance Dynamics in Small Fragments: A Prairie Biodiversity Explorer

This interactive Shiny app was developed as part of the **EEB398 Independent Research Project** through the **Research Excursions Program (REP)** at the University of Toronto. Our project investigates how demographic stochasticity and habitat fragment size shape the long-term dynamics of restored prairie plant communities.

We focus on four native tallgrass species across varying plot sizes at the **Koffler Scientific Research Reserve (KSR)** in King City, Ontario. Contrary to the assumption that small habitat patches are ecologically inferior, our findings explore how these fragments may foster alternate stable states and allow unexpected shifts in species dominance.

## 🔍 Features

- **Population Fluctuations (CV):** Boxplots of coefficient of variation by plot size and species.
- **Extinction Rates:** Extinction frequency by plot size for each species.
- **Relative Abundance Over Time:** Log-scale trends for species across years and plot sizes.
- **Alternate Stable States:** Highlighted time series of top and median abundance patches.
- **Interactive Filtering:** Select species, plot sizes, and year ranges to customize views.
- **Data Viewer:** View and filter raw data directly from the app.
- **Download Plots:** Export any graph for use in presentations or reports.

## 📊 Built With

- [R](https://cran.r-project.org/)  
- [Shiny](https://shiny.posit.co/)  
- [ggplot2](https://ggplot2.tidyverse.org/)  
- [plotly](https://plotly.com/r/)  
- [DT](https://rstudio.github.io/DT/)  
- [bslib](https://rstudio.github.io/bslib/)

## 🧪 Data Source

Data were collected from restored prairie plots at KSR between 2013 and 2024. Processing involved pivoting species abundance data and calculating relative abundances, coefficients of variation, and extinction rates.
