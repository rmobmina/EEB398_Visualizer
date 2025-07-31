# ==============================================================================
# EEB398 REP 2025 — GRAPH VISUALISER
# ==============================================================================

# load libraries
library(shiny)
library(tidyverse)
library(colorspace)
library(bslib)
library(plotly)
library(DT)

# define species
species_order <- c("Asclepias_tuberosa", "Ratibida_pinnata", 
                   "Coreopsis_tripteris", "Monarda_fistulosa")

# define color palette
colors <- c("0.5" = "#740580", "1" = "#F76218", "2" = "#FFBF00")
lighter_colors <- lighten(colors, amount = 0.4)

species_colors <- c(
  "Asclepias_tuberosa" = "#740580",
  "Ratibida_pinnata" = "#B10065",
  "Coreopsis_tripteris" = "#F76218",
  "Monarda_fistulosa" = "#FFBF00"
)

# load and process data
initial_data <- read.csv("Race Track Abundances Through Time to 2024.csv")

# prepare focal species dataset
focal_species <- initial_data %>%
  pivot_longer(cols = Agrostis_gigantea:Veronica_arvensis,
               names_to = "species", values_to = "abundance") %>%
  group_by(Block, Plot.size, Plot, Year) %>%
  mutate(relative_abundance = abundance / sum(abundance)) %>%
  ungroup() %>%
  filter(Mix == "TGP", Partial == "N") %>%
  mutate(
    Plot.size = as.factor(Plot.size),
    Year = as.integer(Year)
  )

# ──────────────────────────────────────────────────────────────────────────────
# USER INTERFACE (UI)
# ──────────────────────────────────────────────────────────────────────────────

ui <- fluidPage(
  fluid = TRUE,
  # apply theme
  theme = bs_theme(bootswatch = "journal"),
  # title
  div(
    style = "margin-top: 25px; margin-bottom: 15px;",
    titlePanel("🌾 Dominance & Stability in Prairie Fragments 🌾")
  ),
  # sidebar
  sidebarLayout(
    sidebarPanel(
      selectInput("species_select", "Select Species:",
                  choices = sort(unique(focal_species$species)),
                  selected = species_order,
                  multiple = TRUE,
                  selectize = TRUE),
      checkboxGroupInput("plot_size_select", "Plot Sizes:",
                         choices = levels(focal_species$Plot.size),
                         selected = levels(focal_species$Plot.size)),
      sliderInput("year_range", "Select Years:",
                  min = 2013, max = 2024,
                  value = c(2013, 2024),
                  step = 1, sep = ""),
      downloadButton("downloadPlot", "Download Current Plot")
    ),
    # main panel with tabbed sections
    mainPanel(
      tabsetPanel(
        # about section
        tabPanel("About", 
                 div(
                   h3("🌿 About This Project 🌿"),
                   style = "margin-top: 20px; margin-bottom: 20px;"
                 ),
                 p("This app showcases research conducted through the ",
                   strong("Research Excursions Program (REP)"),
                   " as part of the ",
                   strong("EEB398 Independent Research course"),
                   " at the University of Toronto. The project was supervised by ",
                   strong("Professor Benjamin Gilbert"),
                   " and is based at the ",
                   strong("Koffler Scientific Research Reserve (KSR)"),
                   ", a restored tallgrass prairie in King City, Ontario."),
                 p("It was developed by ",
                   strong("Reena Obmina, Julia Caccamo, and Nanor Pontigian"),
                   "."),
                 p("In fragmented landscapes, plant communities are scattered 
                   across habitat patches that differ in size, quality, and 
                   connectivity. These patches collectively form a ",
                   em("metacommunity,"),
                   " a network of local communities connected by dispersal and 
                   shaped by both local and regional dynamics."),
                 p("Our research investigates how four focal prairie species 
                   persist across patches of varying size. While small fragments 
                   are often seen as ecologically inferior due to their 
                   vulnerability to ",
                   em("demographic stochasticity,"),
                   " random birth, death, and colonization events, this same 
                   randomness can sometimes create surprising outcomes. In some 
                   cases, rare or suppressed species may become dominant, 
                   leading to ",
                   em("alternate stable states:"),
                   " distinct, self-reinforcing community configurations that 
                   remain stable under the same conditions."),
                 p("By analyzing long-term abundance data from KSR, we ask 
                   whether small patches, despite their instability, might 
                   actually foster opportunities for restored species to thrive. 
                   This app visualizes our findings on population fluctuations, 
                   extinction patterns, and dominance dynamics in fragmented 
                   prairie ecosystems.")
        ),
        # how to use section
        tabPanel("How to Use",
                 div(
                   h3("🌱 How to Use This Tool 🌱"),
                   style = "margin-top: 20px; margin-bottom: 20px;"
                 ),
                 tags$ul(
                   tags$li("Use the sidebar to select which species and plot 
                           sizes to display."),
                   tags$li("Switch between the tabs to view different ecological 
                           visualizations."),
                   tags$li("Hover over points in the plots for details like 
                           values and identifiers."),
                   tags$li("Use the 'Download' button in the sidebar to save 
                           the currently active plot."),
                   tags$li("For the stable states plot, only a subset of patches 
                           are shown for clarity.")
                 ),
                 div(style = "padding-top: 30px; padding-bottom: 20px;",
                     img(src = "eeb398.png", 
                         height = "350px", 
                         style = "display: block; padding-left: 20px; 
                         margin-top: -20px;")
                 ),
                 tags$figcaption(
                   HTML("<em>The 2025 KSR research team (left to right): 
                        Reena Obmina, <br> Julia Caccamo, Benjamin Gilbert, 
                        Crystal Lim, and Nanor Pontigian.</em>"),
                   style = "font-size: 14px; color: #000000; 
                   padding-left: 10px; margin-top: 5px;"
                 )
        ),
        # plot tabs (1-4)
        tabPanel("1. Population Fluctuations (CV)",
                 plotlyOutput("cv_plot", height = "600px")),
        tabPanel("2. Extinction Rates",
                 plotlyOutput("extinction_plot", height = "600px")),
        tabPanel("3. Relative Abundance Over Time",
                 plotlyOutput("abundance_plot", height = "600px")),
        tabPanel("4. Alternate Stable States",
                 plotlyOutput("stable_plot", height = "600px")),
        # raw data table
        tabPanel("Raw Data",
                 DTOutput("data_table"))
      )
    )
  )
)

# ──────────────────────────────────────────────────────────────────────────────
# SERVER
# ──────────────────────────────────────────────────────────────────────────────

server <- function(input, output) {
  
  # filter dataset based on user input
  filtered_data <- reactive({
    focal_species %>%
      filter(
        species %in% input$species_select,
        Plot.size %in% input$plot_size_select,
        Year >= input$year_range[1],
        Year <= input$year_range[2]
      )
  })
  

  # 1. population fluctuations (coefficient of variation)

  output$cv_plot <- renderPlotly({
    df <- filtered_data() %>%
      group_by(Block, Plot.size, Plot, species) %>%
      summarise(
        mean_abundance = mean(abundance),
        sd = sd(abundance),
        CV = sd / mean_abundance,
        .groups = "drop"
      ) %>%
      filter(is.finite(CV)) %>%
      mutate(Plot.size = factor(Plot.size, levels = names(colors)))
    
    p <- ggplot(df, aes(x = Plot.size, y = CV, fill = Plot.size, color = Plot.size)) +
      geom_boxplot() +
      facet_wrap(~ species, labeller = labeller(species = ~ gsub("_", " ", .))) +
      scale_fill_manual(values = lighter_colors) +
      scale_color_manual(values = colors) +
      theme_classic() +
      labs(x = "Plot Size", y = "Coefficient of Variation")
    
    ggplotly(p)
  })
  
  # 2. extinction rates at final year
  
  output$extinction_plot <- renderPlotly({
    df <- filtered_data() %>%
      mutate(present = ifelse(abundance > 0, 1, 0)) %>%
      group_by(Block, Plot.size, Plot, species) %>%
      mutate(years_present = sum(present)) %>%
      ungroup() %>%
      filter(Year == input$year_range[2]) %>%
      group_by(Plot.size, species) %>%
      summarise(
        num_patches = n(),
        num_persisting = sum(present),
        extinction_rate = 1 - num_persisting / num_patches,
        .groups = "drop"
      )
    
    extra_colors <- setNames(rep("grey70", 100),
                             setdiff(unique(df$species), names(species_colors)))
    
    p <- ggplot(df, aes(x = Plot.size, y = extinction_rate, color = species, group = species)) +
      geom_point(size = 3) +
      geom_line(linewidth = 1.2) +
      scale_color_manual(values = c(species_colors, extra_colors)) +
      labs(x = "Plot Size (m)", y = "Extinction Rate") +
      theme_classic(base_size = 14)
    
    ggplotly(p)
  })
  
  # 3. relative abundance over time

  output$abundance_plot <- renderPlotly({
    df <- filtered_data() %>%
      group_by(Year, species, Plot.size) %>%
      summarise(rel_abund = mean(relative_abundance), .groups = "drop")
    
    p <- ggplot(df, aes(x = Year, y = rel_abund + 0.01, color = Plot.size)) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "lm", linewidth = 1.2, se = FALSE) +
      scale_y_log10() +
      scale_color_manual(values = colors) +
      facet_wrap(~ species, scales = "free_y",
                 labeller = labeller(species = ~ gsub("_", " ", .))) +
      labs(x = "Year", y = "Relative Abundance (+0.01)") +
      theme_minimal(base_size = 14)
    
    ggplotly(p)
  })
  
  # 4. alternate stable states

  output$stable_plot <- renderPlotly({
    df <- filtered_data() %>%
      mutate(unique_id = paste(Block, Plot, sep = "_"))
    
    highlight_ids <- df %>%
      filter(Year == input$year_range[2]) %>%
      group_by(species, Plot.size) %>%
      arrange(desc(relative_abundance)) %>%
      mutate(
        row = row_number(),
        total = n(),
        mid1 = floor((total + 1)/2),
        mid2 = ceiling((total + 2)/2),
        category = case_when(
          row <= 2 ~ "max",
          row %in% c(mid1, mid2) ~ "median",
          TRUE ~ NA_character_
        )
      ) %>%
      filter(!is.na(category)) %>%
      distinct(species, Plot.size, unique_id, category)
    
    highlighted_data <- df %>%
      semi_join(highlight_ids, by = c("species", "Plot.size", "unique_id")) %>%
      left_join(highlight_ids, by = c("species", "Plot.size", "unique_id")) %>%
      mutate(
        base_color = species_colors[species],
        final_color = ifelse(category == "median", lighten(base_color, 0.4), base_color)
      )
    
    p <- ggplot(highlighted_data, aes(x = Year, y = relative_abundance + 0.01,
                                      group = unique_id, color = final_color)) +
      geom_line(linewidth = 1.1) +
      scale_color_identity() +
      facet_wrap(vars(species, Plot.size), ncol = 3, labeller = labeller(
        species = ~ gsub("_", " ", .),
        Plot.size = label_both
      )) +
      scale_y_log10() +
      theme_bw(base_size = 13) +
      labs(x = "Year", y = "Relative Abundance + 0.01")
    
    ggplotly(p)
  })
  
  # 5. raw data table 

  output$data_table <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 10), filter = "top")
  })
  
# ──────────────────────────────────────────────────────────────────────────────
# PLOT DOWNLOAD
# ──────────────────────────────────────────────────────────────────────────────
 
   output$downloadPlot <- downloadHandler(
    filename = function() {
      paste0("prairie_plot_", Sys.Date(), ".png")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "png", width = 10, height = 6)
    }
  )
}

# ──────────────────────────────────────────────────────────────────────────────

# run application
shinyApp(ui = ui, server = server)

# ──────────────────────────────────────────────────────────────────────────────
