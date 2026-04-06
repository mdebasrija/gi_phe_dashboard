library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(sf)
library(stringr)
library(data.table)
library(fresh)

#Google tag here 
GA_ID <- "G-2CZ0PZ15NQ"

# -------- LOAD PRE-PROCESSED DATA (FAST) --------
cat("Loading pre-processed data...\n")

df <- readRDS("df_clean.rds")
meta_new <- readRDS("meta_clean.rds")
coords_dt <- readRDS("coords_clean.rds")
bg_coords <- readRDS("bg_coords_clean.rds")
landing_metrics <- readRDS("landing_metrics.rds")
linguistic_counts <- readRDS("linguistic_counts.rds")
sample_location_counts <- readRDS("sample_location_counts.rds")
corr_preview_data <- readRDS("corr_preview_data.rds")
corr_coefficient <- readRDS("corr_coefficient.rds")

landing_ver <- tryCatch(
  readLines("www/landing/VERSION.txt", warn = FALSE)[1],
  error = function(e) ""
)
vqs <- if (nzchar(landing_ver)) paste0("?v=", landing_ver) else ""

cat("Data loaded!\n")

# Biomarkers list (needed for UI)
biomarkers <- c(
  "Fasting_Blood_Glucose", "Glycosylated_Haemoglobin", "Urea", "Creatinine",
  "Total_Bilirubin", "Aspartate_Aminotransferase", "Alanine_Aminotransferase",
  "Alkaline_Phosphatase", "Cholesterol", "Triglycerides", "HDL", "LDL", "Haemoglobin",
  "Red_Blood_Cell_Count", "Mean_Corpuscular_Haemoglobin", "White_Blood_Cell_Count",
  "Neutrophils_Percentage", "Lymphocytes_Percentage", "Eosinophils_Percentage",
  "Monocytes_Percentage", "Basophils_Percentage", "Platelet_Count", "Direct_Bilirubin",
  "Indirect_Bilirubin", "Albumin", "Protein", "Random_Blood_Glucose", "BMI", "age",
  "Systolic_Blood_Pressure", "Diastolic_Blood_Pressure", "Head_Circumference",
  "Height", "Weight", "Waist_Circumference", "Hip_Circumference", "Body_Fat_Percentage",
  "Blood_Glucose_Glucometer"
)

metricBox <- function(value, subtitle, icon_name, bg_class) {
  tags$div(
    class = paste("small-box", bg_class, "metric-box"),
    tags$div(
      class = "inner",
      tags$h3(value),
      tags$p(subtitle)
    ),
    tags$div(class = "icon", icon(icon_name))
  )
}

# helper function for header box
my_theme <- create_theme(
  adminlte_color(
    light_blue = "#3a7ca5",
    teal = "#1b4965"
  )
)

# UI
ui <- dashboardPage(
  # Dashboard Header
  dashboardHeader(
    title = "GenomeIndia Phenotypes",
    titleWidth = 300
  ),

  # Dashboard Sidebar
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "sidebar",
      menuItem("Overview", tabName = "overview", icon = icon("chart-bar")),
      menuItem("Trait Distribution", tabName = "distribution", icon = icon("chart-area")),
      menuItem("Geographic Distribution", tabName = "geography", icon = icon("map")),
      menuItem("Biomarker Correlations", tabName = "correlations", icon = icon("project-diagram")),
      menuItem("FAQ", tabName = "faq", icon = icon("question-circle"))
    ),
    hr(),

    # Show the Filters heading only where it’s relevant
    conditionalPanel(
      condition = "input.sidebar != 'overview' && input.sidebar != 'faq'",
      h4("Filters", style = "margin-left: 15px; color: #fff;")
    ),

    div(
      style = "margin: 0 15px;",

      # Biomarker: Distribution + Geography
      conditionalPanel(
        condition = "input.sidebar == 'distribution' || input.sidebar == 'geography'",
        selectInput(
          "biomarker",
          "Select Biomarker:",
          choices = setNames(biomarkers, biomarkers),
          selected = "BMI"
        )
      ),

      # Group by: Distribution only
      conditionalPanel(
        condition = "input.sidebar == 'distribution'",
        selectInput(
          "group",
          "Group by:",
          choices = c(
            "Tribal Classification" = "T_NT",
            "Ethnicity" = "ethnicity_mapping",
            "State" = "name_dob_1.state",
            "Gender" = "gender",
            "Center" = "center"
          ),
          selected = "ethnicity_mapping"
        )
      ),

      # Age: Distribution + Geography + Correlations
      conditionalPanel(
        condition = "input.sidebar == 'distribution' || input.sidebar == 'geography' || input.sidebar == 'correlations'",
        sliderInput(
          "age",
          "Age Range:",
          min = 0,
          max = 100,
          value = c(0, 100),
          step = 10
        )
      ),

      # Facet by: Distribution + Geography
      conditionalPanel(
        condition = "input.sidebar == 'distribution' || input.sidebar == 'geography'",
        checkboxGroupInput(
          "facet_vars",
          "Facet by: (Max 2)",
          choices = c(
            "Gender" = "gender",
            "Medication Status" = "history_illness.medication_currently_status",
            "Ancestry" = "ancestry",
            "Fasting Status" = "blood_draw.Blood_draw_fasting"
          ),
          selected = "gender"
        )
      ),

      # Update Map: Geography only
      conditionalPanel(
        condition = "input.sidebar == 'geography'",
        actionButton(
          "refresh_map",
          "Update Map",
          class = "btn-primary btn-block",
          style = "margin-top: 10px;"
        )
      )
    )
  ),

  # Dashboard Body
  dashboardBody(
    use_theme(my_theme),
    tags$head(
  # --- GA4 loader (gtag.js) ---
  tags$script(
    async = NA,
    src = paste0("https://www.googletagmanager.com/gtag/js?id=", GA_ID)
  ),

  # --- GA4 config + Shiny SPA instrumentation ---
tags$script(HTML(sprintf("
  window.dataLayer = window.dataLayer || [];
  function gtag(){dataLayer.push(arguments);}

  // Initialize GA
  gtag('js', new Date());

  // Shiny is SPA-like; disable auto page_view and send our own
  gtag('config', '%s', { send_page_view: false });

  // Send a GA4 page_view with a 'virtual path' (useful for Shiny tab navigation)
  function gaPageView(virtualPath) {
    if (typeof gtag !== 'function') return;
    var path = virtualPath || (window.location.pathname + window.location.search + window.location.hash);

    gtag('event', 'page_view', {
      page_title: document.title,
      page_location: window.location.href,
      page_path: path
    });
  }

  // Initial page view when Shiny connects
  $(document).on('shiny:connected', function() {
    gaPageView();
  });

  // Tab navigation: input.sidebar changes in your app
  $(document).on('shiny:inputchanged', function(e) {
    if (!e || !e.name) return;

    // 1) Virtual pageviews on sidebar tab changes
    if (e.name === 'sidebar') {
      gaPageView('/' + e.value);
    }

    // 2) Optional: click tracking for actionButtons
    if (e.name === 'refresh_map') {
      gtag('event', 'refresh_map_click', { tab: 'geography' });
    }
    if (e.name === 'goto_traits') {
      gtag('event', 'nav_click', { target: 'distribution' });
    }
    if (e.name === 'goto_geography') {
      gtag('event', 'nav_click', { target: 'geography' });
    }
    if (e.name === 'goto_correlations') {
      gtag('event', 'nav_click', { target: 'correlations' });
    }
  });

  // ---- Silent reconnect logic ----
  function hideReconnectUI() {
    var selectors = [
      '#shiny-disconnected-overlay',
      '#shiny-reconnect-message',
      '#shiny-reconnect-text',
      '#shiny-reconnect-time',
      '#shiny-reconnect-now',
      '#shiny-notification-panel',
      '.shiny-notification',
      '[id^=\"shiny-notification\"]',
      '.reconnecting-notification',
      '[class*=\"reconnect\"]',
      '[id*=\"reconnect\"]',
      '[id*=\"disconnected\"]'
    ];
    selectors.forEach(function(sel) {
      try {
        document.querySelectorAll(sel).forEach(function(el) {
          el.style.setProperty('display', 'none', 'important');
          el.style.setProperty('visibility', 'hidden', 'important');
          el.style.setProperty('opacity', '0', 'important');
          el.style.setProperty('pointer-events', 'none', 'important');
        });
      } catch(e) {}
    });
  }

  // MutationObserver: watch for reconnect UI being added to DOM and immediately hide it
  var reconnectObserver = new MutationObserver(function(mutations) {
    mutations.forEach(function(mutation) {
      if (mutation.addedNodes.length > 0) {
        hideReconnectUI();
      }
    });
  });
  reconnectObserver.observe(document.body, { childList: true, subtree: true });

  $(document).on('shiny:disconnected', function() {
    hideReconnectUI();
    var attempts = 0;
    var maxAttempts = 10;
    var interval = setInterval(function() {
      attempts++;
      hideReconnectUI();
      if (typeof Shiny !== 'undefined' && Shiny.shinyapp) {
        Shiny.shinyapp.reconnect();
      }
      if (attempts >= maxAttempts) {
        clearInterval(interval);
      }
    }, 3000);
  });

  $(document).on('shiny:reconnecting', function() {
    hideReconnectUI();
  });
", GA_ID))),
      tags$style(
        HTML(" 
.content-wrapper, 
.right-side { 
  background-color: #ffffff; 
  font-size: 18px; 
} 
.box { 
  margin-bottom: 20px;
  border: none !important;
  box-shadow: none !important;
  background: #ffffff !important;
} 
.small-box { border-radius: 25px !important; box-shadow: 0 2px 8px rgba(0,0,0,0.1); } 
.box.box-info > .box-header { background-color: #3a7ca5 !important; color: #fff; border-bottom: none !important; border-radius: 4px 4px 0 0; } 
.btn-nav { border-radius: 20px; padding: 12px 30px; font-size: 16px; transition: all 0.3s ease; border: none; box-shadow: 0 2px 8px rgba(0,0,0,0.1); } 
.btn-nav:hover { transform: translateY(-2px); box-shadow: 0 4px 12px rgba(0,0,0,0.15); } 
.landing-img{ width:100%; height:260px; object-fit:contain; display:block; } 
.landing-btn.btn-block{ width:100%; margin-top:6px !important; margin-left:0 !important; margin-right:0 !important; } 
.summary-row{
  display:flex;
  flex-wrap:nowrap;
  gap:10px;
  align-items:stretch;
  margin: 0 0 12px 0;
} 
.summary-chip{ background:#ffffff; border:2px solid #1b4965; border-radius:18px; box-shadow:0 2px 8px rgba(0,0,0,0.08); display:flex; align-items:center; justify-content:center; writing-mode:vertical-rl; transform:rotate(180deg); font-weight:700; color:#1b4965; flex:0 0 46px; height: 120px; } 
.summary-item{ flex:1 1 0; min-width:0; } 
.metric-box{ height:120px; margin-bottom:0 !important; } 
.metric-box .inner{ padding-top: 14px; }
.stats-strip {
  display: flex;
  flex-wrap: nowrap;
  gap: 10px;
  overflow-x: auto;
  padding: 4px 10px 12px 10px;
}

.stat-pill {
  flex: 1 1 0;
  min-width: 120px;
  background: #f8fbfd;
  border: 1px solid #d9e6ef;
  border-radius: 12px;
  padding: 10px 12px;
  text-align: center;
  box-shadow: 0 1px 4px rgba(0,0,0,0.06);
}

.stat-pill-label {
  font-size: 12px;
  color: #4f6b7a;
  margin-bottom: 4px;
  white-space: nowrap;
}

.stat-pill-value {
  font-size: 18px;
  font-weight: 700;
  color: #1b4965;
  white-space: nowrap;
}

/* Prevent footer from covering bottom content */
.content-wrapper,
.right-side {
  padding-bottom: 40px !important;
}
.summary-left{
  flex:1 1 0;
  display:flex;
  gap:10px;
  align-items:stretch;
  min-width:0;
}
.summary-item-fixed{
  flex:0 0 230px;
  min-width:230px;
}
.summary-brand-left{
  flex:0 0 45%;
  height:120px;
  display:flex;
  align-items:center;
  justify-content:flex-start;
  text-align:left;
  padding-left:10px;
}
.summary-brand-title{
  font-size:42px;
  font-weight:900;
  letter-spacing:2px;
  line-height:1;
  color:#1b4965;
}
.summary-brand-subtitle{
  margin-top:6px;
  font-size:14px;
  font-weight:600;
  color:#3a7ca5;
  line-height:1.1;
}
.summary-chip{
  flex:0 0 46px;
  height:120px;
}
.summary-item-fill{
  flex:1 1 0;
  min-width:0;
}

/* Prevent biomarker dropdown text overflow */
.selectize-input {
  overflow: hidden !important;
  text-overflow: ellipsis !important;
  white-space: nowrap !important;
  width: 100% !important;
  box-sizing: border-box !important;
}
.selectize-input > .item {
  overflow: hidden !important;
  text-overflow: ellipsis !important;
  white-space: nowrap !important;
  max-width: calc(100% - 30px) !important;
  display: inline-block !important;
}
.selectize-dropdown {
  width: 100% !important;
}

/* Box: no border, no shadow, white background — header strip only */
.box.box-primary,
.box.box-info {
  border: none !important;
  box-shadow: none !important;
  background: #ffffff !important;
}
.tab-pane#shiny-tab-correlations .box.box-info {
  border: none !important;
  box-shadow: none !important;
  background: #ffffff !important;
}
.box.box-primary > .box-header {
  background-color: #3a7ca5 !important;
  border-bottom: none !important;
  border-radius: 4px 4px 0 0;
}
.box > .box-body {
  background: #ffffff !important;
  padding: 0 !important;
}

/* Plot containers: subtle border around the actual plot outputs */
#plot,
#map,
#correlation_plot {
  border: 1.5px solid #ffffff !important;
  border-radius: 6px !important;
  padding: 4px !important;
  background: #fff !important;
  margin: 8px 4px !important;
}

/* Summary statistics: white, borderless, with margin */
pre#distribution_stats {
  background: #ffffff !important;
  border: none !important;
  box-shadow: none !important;
  border-radius: 0 !important;
  padding: 14px 20px !important;
  margin: 10px 6px !important;
  font-size: 14px !important;
}
pre#correlation_stats {
  background: #ffffff !important;
  border: none !important;
  box-shadow: none !important;
  border-radius: 0 !important;
  padding: 14px 20px !important;
  margin: 10px 6px !important;
  font-size: 14px !important;
}

/* Completely suppress reconnect toast and overlay */
#shiny-disconnected-overlay,
#shiny-reconnect-message,
#shiny-notification-panel,
.shiny-notification,
[id^='shiny-notification'],
.reconnecting-notification,
div[id*='reconnect'] {
  display: none !important;
  visibility: hidden !important;
  opacity: 0 !important;
  pointer-events: none !important;
}

.tab-pane#shiny-tab-geography .box-body {
  padding-top: 15px !important;
}
.tab-pane#shiny-tab-correlations .box-body {
  padding-top: 15px !important;
}

 ")
      )
    ),
tags$script(HTML("
  Shiny.addCustomMessageHandler('facet_disable', function(msg){
    var val = msg.value;
    var disable = msg.disable;

    var $inp = $('input[name=\"facet_vars\"][value=\"' + val + '\"]');
    $inp.prop('disabled', disable);

    // visually indicate disabled
    $inp.closest('label').css('opacity', disable ? 0.45 : 1.0);
  });
")),

    tabItems(
      tabItem(
        tabName = "overview",
tags$div(
  class = "summary-row",

  tags$div(
    class = "summary-brand-left",
    tags$div(
      tags$div(class = "summary-brand-title", "GENOMEINDIA"),
      tags$div(class = "summary-brand-subtitle", "Funded by DBT, Government of India")
    )
  ),

  tags$div(
    class = "summary-left",
    tags$div(class = "summary-chip", "SUMMARY"),

    tags$div(
      class = "summary-item summary-item-fill",
      metricBox(
        value = landing_metrics$total_samples_fmt,
        subtitle = "Samples",
        icon_name = "users",
        bg_class = "bg-light-blue"
      )
    ),

    tags$div(
      class = "summary-item summary-item-fill",
      metricBox(
        value = landing_metrics$ethnicities,
        subtitle = "Ethnicities",
        icon_name = "globe",
        bg_class = "bg-teal"
      )
    )
  )
),

        fluidRow(
          box(
            title = NULL,
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            tags$div(
              style = "font-size:16px; line-height:1.4;",
              tags$p("This interactive dashboard is an overview of the GenomeIndia phenotype dataset (17,777 samples across 81 ethnicities). We have built it for data exploration of the GI dataset. It can be used to assess trait distributions across linguistic and ethnicity groups, age and gender structures, states and collection centres."),
              tags$p(tags$b("Please use this as an exploration tool only."))
            )
          )
        ),

        fluidRow(
          box(
            title = "Age Distribution",
            status = "primary",
            solidHeader = TRUE,
            width = 4,
            height = 420,
            tags$img(src = paste0("landing/age_distribution.png", vqs), class = "landing-img"),
            actionButton(
              "goto_traits",
              "More Trait Distributions",
              class = "btn-nav btn-block landing-btn",
              style = "background-color:#3a7ca5; color:white;"
            )
          ),
          box(
            title = "Geographic Sample Distribution",
            status = "primary",
            solidHeader = TRUE,
            width = 4,
            height = 420,
            tags$img(src = paste0("landing/sample_map.png", vqs), class = "landing-img"),
            actionButton(
              "goto_geography",
              "More Geographical Distributions",
              class = "btn-nav btn-block landing-btn",
              style = "background-color:#1b4965; color:white;"
            )
          ),
          box(
            title = "Biomarker Correlation Preview",
            status = "primary",
            solidHeader = TRUE,
            width = 4,
            height = 420,
            tags$img(src = paste0("landing/correlation_preview.png", vqs), class = "landing-img"),
            actionButton(
              "goto_correlations",
              "More Correlations",
              class = "btn-nav btn-block landing-btn",
              style = "background-color:#3a7ca5; color:white;"
            )
          )
        )
      ),

      # Trait Distribution Tab
      tabItem(
        tabName = "distribution",
        fluidRow(
          box(
            title = "Biomarker Distribution",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            height = 760,
            plotlyOutput("plot", height = "560px"),
tags$hr(style = "margin: 8px 12px;"),
uiOutput("distribution_stats_inline")
          )
        )
      ),

      # Geographic Distribution Tab
      tabItem(
        tabName = "geography",
        fluidRow(
          box(
            title = "Geographic Distribution of Biomarkers",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            height = 700,
            div(
              style = "text-align: center; margin-bottom: 10px;",
              tags$b("Click 'Update Map' in the sidebar to generate the visualization after selecting the options."),
              tags$br(),
              "You can customize the view using the 'Facet By' options. Thank you for your patience - this may take a moment to load."
            ),
            conditionalPanel(
              condition = "!output.map",
              div(
                style = "text-align: center; margin-top: 200px;",
                tags$i(class = "fa fa-spinner fa-spin fa-3x"),
                br(),
                br(),
                "Loading map data..."
              )
            ),
            plotlyOutput("map", height = "620px")
          )
        )
      ),

      # Correlations Tab
      tabItem(
        tabName = "correlations",
        fluidRow(
          box(
            title = "Correlation Parameters",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            fluidRow(
              column(
                3,
                selectInput(
                  "biomarker_x",
                  "X-axis Biomarker:",
                  choices = setNames(biomarkers, biomarkers),
                  selected = "BMI"
                )
              ),
              column(
                3,
                selectInput(
                  "biomarker_y",
                  "Y-axis Biomarker:",
                  choices = setNames(biomarkers, biomarkers),
                  selected = "HDL"
                )
              ),
              column(
                2,
                selectInput(
                  "corr_gender",
                  "Gender:",
                  choices = c("All" = "all", "Male" = "Male", "Female" = "Female"),
                  selected = "all"
                )
              ),
              column(
                2,
                selectInput(
                  "corr_ethnicity",
                  "Group:",
                  choices = c("All" = "all"),
                  selected = "all"
                )
              ),
              column(
                2,
                selectInput(
                  "corr_medication",
                  "Medication Status:",
                  choices = c("All" = "all"),
                  selected = "all"
                )
              )
            )
          )
        ),
        fluidRow(
          box(
            title = "Correlation Plot",
            status = "primary",
            solidHeader = TRUE,
            width = 8,
            height = 700,
            plotlyOutput("correlation_plot", height = "650px")
          ),
          box(
            title = "Correlation Statistics",
            status = "info",
            solidHeader = TRUE,
            width = 4,
            height = 700,
            verbatimTextOutput("correlation_stats")
          )
        )
      ),

      # FAQ tab
      tabItem(
        tabName = "faq",
        fluidRow(
          box(
            title = "Frequently Asked Questions",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            includeMarkdown("faq.md")
          )
        )
      )
    ),

    tags$footer(
      style = "position: fixed; bottom: 0; width: 100%; background-color: #222d32; color: #fff; text-align: center; padding: 5px; z-index: 1000; font-size: 12px;",
      "Phenotype Data Visualizer | Data belongs to GenomeIndia Consortium, DBT. | Last updated: November 2025 | Best viewed on a desktop/tablet."
    )
  )
)

# Server
server <- function(input, output, session) {
session$allowReconnect(TRUE)
  
  options(
    shiny.error = function() {
      # Silent - do nothing - to prevent error message from popping up on map page
    }
  )

  # Navigation button observers
  observeEvent(input$goto_traits, {
    updateTabItems(session, "sidebar", "distribution")
  })
  observeEvent(input$goto_geography, {
    updateTabItems(session, "sidebar", "geography")
  })
  observeEvent(input$goto_correlations, {
    updateTabItems(session, "sidebar", "correlations")
  })

  observeEvent(
    input$sidebar,
    {
      if (identical(input$sidebar, "geography")) {
        updateSelectInput(session, "group", selected = "ethnicity_mapping")
      }
    },
    ignoreInit = TRUE
  )

  # Update correlation dropdown choices based on available data
observe({
  vals <- sort(unique(as.character(df$T_NT[!is.na(df$T_NT)])))
  choices <- c("All" = "all", setNames(vals, vals))
  updateSelectInput(session, "corr_ethnicity", choices = choices)

  med_vals <- sort(unique(as.character(df$history_illness.medication_currently_status[!is.na(df$history_illness.medication_currently_status)])))
  updateSelectInput(session, "corr_medication", choices = c("All" = "all", setNames(med_vals, med_vals)))
})


  observeEvent(
    input$age,
    ignoreInit = TRUE,
    {
      lower <- input$age[1]
      upper <- input$age[2]
      min_gap <- 10
      min_age <- 0
      max_age <- 100

      if ((upper - lower) < min_gap) {
        new_upper <- min(lower + min_gap, max_age)
        if (new_upper == upper) {
          new_lower <- max(upper - min_gap, min_age)
          updateSliderInput(session, "age", value = c(new_lower, upper))
        } else {
          updateSliderInput(session, "age", value = c(lower, new_upper))
        }
      }
    }
  )
observeEvent(input$group, {
  # always re-enable both first
  session$sendCustomMessage("facet_disable", list(value = "gender", disable = FALSE))
  session$sendCustomMessage("facet_disable", list(value = "ancestry", disable = FALSE))

  current_facets <- if (is.null(input$facet_vars)) character(0) else input$facet_vars

  # If grouping by gender: remove gender from facets + disable it
  if (identical(input$group, "gender")) {
    new_facets <- setdiff(current_facets, "gender")
    if (!identical(new_facets, current_facets)) {
      updateCheckboxGroupInput(session, "facet_vars", selected = new_facets)
    }
    session$sendCustomMessage("facet_disable", list(value = "gender", disable = TRUE))
  }

  # If grouping by ancestry: remove ancestry from facets + disable it
  if (identical(input$group, "ancestry")) {
    new_facets <- setdiff(current_facets, "ancestry")
    if (!identical(new_facets, current_facets)) {
      updateCheckboxGroupInput(session, "facet_vars", selected = new_facets)
    }
    session$sendCustomMessage("facet_disable", list(value = "ancestry", disable = TRUE))
  }
}, ignoreInit = TRUE)

  # Reactive data filtering with caching
  filtered_data <- reactive({
    req(input$biomarker, input$age)

    group <- if (is.null(input$group)) "ethnicity_mapping" else input$group
    facet_vars <- if (is.null(input$facet_vars)) character(0) else input$facet_vars

    data <- df[
      age >= input$age[1] &
        age <= input$age[2] &
        !is.na(get(group)) &
        !is.na(get(input$biomarker))
    ]

    if ("gender" %in% facet_vars) {
      data <- data[!is.na(gender) & gender != "other"]
    }
    if ("history_illness.medication_currently_status" %in% facet_vars) {
      data <- data[!is.na(history_illness.medication_currently_status)]
    }
    if ("blood_draw.Blood_draw_fasting" %in% facet_vars) {
      data <- data[!is.na(blood_draw.Blood_draw_fasting)]
    }
    if ("ancestry" %in% facet_vars) {
      data <- data[!is.na(ancestry)]
    }

    data
  })

  K_MIN <- 5
  MAX_PANELS <- 20

  filtered_safe <- reactive({
    dt <- filtered_data()
    group <- if (is.null(input$group)) "ethnicity_mapping" else input$group
    facet_vars <- if (is.null(input$facet_vars)) character(0) else input$facet_vars

    gvars <- c(group, facet_vars)
    gvars <- gvars[gvars != ""]
    gvars <- gvars[gvars %in% names(dt)]

    if (nrow(dt) == 0L || length(gvars) == 0L) return(dt[0])

    keep <- dt[, .N, by = gvars][N >= K_MIN][, N := NULL]
    if (nrow(keep) == 0L) return(dt[0])

    merge(dt, keep, by = gvars, all = FALSE)
  })

  # ===== LANDING PAGE OUTPUTS =====
  # Pill-style value boxes
  output$total_samples <- renderValueBox({
    valueBox(
      value = formatC(nrow(df), format = "d", big.mark = ","),
      subtitle = "Total Samples",
      icon = icon("users"),
      color = "light-blue"
    )
  })

  output$avg_age <- renderValueBox({
    avg_age <- round(mean(df$age, na.rm = TRUE), 1)
    valueBox(
      value = paste(avg_age, "years"),
      subtitle = "Average Age",
      icon = icon("calendar"),
      color = "teal"
    )
  })

  output$gender_ratio <- renderValueBox({
    gender_counts <- table(df$gender, useNA = "no")
    if (length(gender_counts) >= 2) {
      ratio <- round(gender_counts["Male"] / gender_counts["Female"], 2)
      valueBox(
        value = paste(ratio, ":1"),
        subtitle = "Male:Female Ratio",
        icon = icon("venus-mars"),
        color = "light-blue"
      )
    } else {
      valueBox(
        value = "N/A",
        subtitle = "Male:Female Ratio",
        icon = icon("venus-mars"),
        color = "light-blue"
      )
    }
  })

  output$ethnicities_count <- renderValueBox({
    ethnicities <- length(unique(df$ethnicity_mapping[!is.na(df$ethnicity_mapping)]))
    valueBox(
      value = ethnicities,
      subtitle = "Ethnic Groups",
      icon = icon("globe"),
      color = "teal"
    )
  })

  # ===== OTHER TAB OUTPUTS =====
  # Distribution statistics
 output$distribution_stats_inline <- renderUI({
  data <- filtered_safe()
  validate(
    need(
      nrow(data) >= K_MIN,
      sprintf("Not enough samples to compute stats (n < %d). Please broaden filters.", K_MIN)
    )
  )

  x <- data[[input$biomarker]]

  stat_box <- function(label, value) {
    tags$div(
      class = "stat-pill",
      tags$div(class = "stat-pill-label", label),
      tags$div(class = "stat-pill-value", value)
    )
  }

  tags$div(
    class = "stats-strip",
    stat_box("N", format(nrow(data), big.mark = ",")),
    stat_box("Mean", round(mean(x, na.rm = TRUE), 2)),
    stat_box("Median", round(median(x, na.rm = TRUE), 2)),
    stat_box("SD", round(sd(x, na.rm = TRUE), 2)),
    stat_box("Min", round(min(x, na.rm = TRUE), 2)),
    stat_box("Q1", round(quantile(x, 0.25, na.rm = TRUE), 2)),
    stat_box("Q3", round(quantile(x, 0.75, na.rm = TRUE), 2)),
    stat_box("Max", round(max(x, na.rm = TRUE), 2))
  )
})

  # Pretty names helper
  pretty_names <- c(
    "name_dob_1.state" = "State",
    "T_NT" = "Tribal Classification",
    "ethnicity_mapping" = "Ethnicity",
    "gender" = "Gender",
    "center" = "Center",
    "BMI" = "BMI",
    "Height" = "Height",
    "Weight" = "Weight",
    "Systolic_Blood_Pressure" = "Systolic Blood Pressure",
    "Diastolic_Blood_Pressure" = "Diastolic Blood Pressure",
    "Head_Circumference" = "Head Circumference",
    "Hip_Circumference" = "Hip Circumference",
    "Waist_Circumference" = "Waist Circumference",
    "Body_Fat_Percentage" = "Percentage of Body Fat",
    "Blood_Glucose_Glucometer" = "Blood Glucose (Anthropometry)",
    "Neutrophils_Percentage" = "Neutrophils (%)",
    "Monocytes_Percentage" = "Monocytes (%)",
    "Lymphocytes_Percentage" = "Lymphocytes (%)",
    "Eosinophils_Percentage" = "Eosinophils (%)",
    "Basophils_Percentage" = "Basophils (%)"
  )

  make_label <- function(var) {
    if (var %in% names(pretty_names)) {
      pretty_names[[var]]
    } else {
      stringr::str_to_title(gsub("[_.]", " ", var))
    }
  }

  output$plot <- renderPlotly({
    data <- filtered_safe()
    validate(
      need(
        nrow(data) >= K_MIN,
        sprintf("Selection too granular to display safely (n < %d). Please broaden filters.", K_MIN)
      )
    )

    p <- ggplot(data, aes_string(x = input$group, y = input$biomarker, fill = input$group)) +
      geom_boxplot(outlier.size = 0.5, alpha = 0.8, width = 0.6) +
      scale_fill_viridis_d(option = "C", end = 0.9) +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 90, hjust = 1, size = 7),
        plot.title = element_text(size = 14)
      ) +
      labs(
        x = make_label(input$group),
        y = make_label(input$biomarker),
        title = paste("Distribution of", make_label(input$biomarker), "by", make_label(input$group))
      )

    if (length(input$facet_vars) == 1) {
      p <- p + facet_wrap(as.formula(paste("~", input$facet_vars)), scales = "free_x")
    } else if (length(input$facet_vars) == 2) {
      p <- p + facet_grid(as.formula(paste(input$facet_vars[1], "~", input$facet_vars[2])), scales = "free_x")
    }

    ggplotly(p, tooltip = c("y", "x")) %>%
      layout(showlegend = FALSE, margin = list(b = 100)) %>%
      config(displayModeBar = FALSE)
  })

  # Map rendering
  map_data <- eventReactive(
  input$refresh_map,
  {
    # Use the SAME facet_vars throughout: captured at click-time
    facet_vars <- if (is.null(input$facet_vars)) character(0) else input$facet_vars
    biomarker  <- input$biomarker

    data <- as.data.table(filtered_safe())

    validate(
      need(
        nrow(data) >= K_MIN,
        sprintf("Selection too granular to map safely (n < %d). Please broaden filters.", K_MIN)
      )
    )

    # Group vars used for aggregation
    group_vars <- c("ethnicity_mapping", facet_vars)

    # Trim extreme outliers for the selected biomarker
    vals <- data[[biomarker]]
    q1  <- quantile(vals, 0.00, na.rm = TRUE)
    q99 <- quantile(vals, 1.00, na.rm = TRUE)
    data <- data[get(biomarker) >= q1 & get(biomarker) <= q99]

    # Summarize biomarker by ethnicity (+ facets), enforce k-anonymity
    bio_summary <- data[
      ,
      .(
        Avg    = mean(get(biomarker), na.rm = TRUE),
        Count  = .N,
        Median = median(get(biomarker), na.rm = TRUE),
        SD     = sd(get(biomarker), na.rm = TRUE)
      ),
      by = group_vars
    ][Count >= K_MIN]

    # Join coordinates
    merged <- coords_dt[bio_summary, on = "ethnicity_mapping"]
    merged <- merged[!is.na(lat) & !is.na(long)]

    # Attach click-time facet vars so renderPlotly can use consistent faceting
    attr(merged, "facet_vars_used") <- facet_vars
    attr(merged, "biomarker_used")  <- biomarker

    merged
  },
  ignoreNULL = TRUE
)

  output$map <- renderPlotly({
  req(input$refresh_map)

  plotly_blank <- function() {
    plotly::plotly_empty() %>%
      plotly::layout(
        paper_bgcolor = "white",
        plot_bgcolor  = "white",
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE),
        margin = list(t = 5, b = 5, l = 5, r = 5)
      ) %>%
      plotly::config(displayModeBar = TRUE, scrollZoom = FALSE, doubleClick = "reset", modeBarButtonsToRemove = c("toImage"), displaylogo = FALSE)
  }

  tryCatch({
    d0 <- map_data()
    if (is.null(d0) || nrow(d0) == 0) return(plotly_blank())

    # Avoid modifying the reactive value by reference (data.table)
    d <- data.table::as.data.table(data.table::copy(d0))

    # IMPORTANT: use the click-time selections saved in map_data()
    biomarker <- attr(d0, "biomarker_used")
    if (is.null(biomarker) || !nzchar(biomarker)) biomarker <- input$biomarker

    facet_vars <- attr(d0, "facet_vars_used")
    if (is.null(facet_vars)) facet_vars <- character(0)
    facet_vars <- facet_vars[facet_vars != ""]
    facet_vars <- facet_vars[facet_vars %in% names(d)]  # guard against mismatch

    MAP_COLORSCALE <- list(
      list(0.0, "#87CEEB"),
      list(0.5, "#FFE4B5"),
      list(1.0, "#DB7093")
    )

   apply_equal_aspect <- function(p, n_panels) {
  n_panels <- as.integer(n_panels)
  if (!is.finite(n_panels) || n_panels < 1L) return(p)

  ax <- list()
  for (i in seq_len(n_panels)) {
    xa  <- if (i == 1) "xaxis" else paste0("xaxis", i)
    ya  <- if (i == 1) "yaxis" else paste0("yaxis", i)
    # Using 'match' ensures the y-axis scale is locked to the x-axis scale
    ax[[xa]] <- list(visible = FALSE)
    ax[[ya]] <- list(visible = FALSE, scaleanchor = if(i==1) "x" else paste0("x", i), scaleratio = 1)
  }
  p$x$layout <- modifyList(p$x$layout, ax)
  p
}
    make_panel <- function(dt, show_scale = FALSE) {
      dt[, hover := paste0(
        "Ethnicity: ", ethnicity_mapping, "<br>",
        "Average ", biomarker, ": ", round(Avg, 2), "<br>",
        "Sample Size: ", Count
      )]

      plotly::plot_ly() %>%
        plotly::add_trace(
          data = bg_coords,
          x = ~long, y = ~lat,
          type = "scatter",
          mode = "lines",
          hoverinfo = "skip",
          line = list(color = "gray80", width = 0.7),
          showlegend = FALSE
        ) %>%
        plotly::add_trace(
          data = dt,
          x = ~long, y = ~lat,
          type = "scattergl",
          mode = "markers",
          text = ~hover,
          hoverinfo = "text",
          marker = list(
            symbol = "circle",
            size = ~pmax(3, sqrt(Count) * 1.2),
            color = ~Avg,
            colorscale = MAP_COLORSCALE,
            showscale = show_scale,
            colorbar = list(title = paste("Average", biomarker)),
            line = list(color = "black", width = 0.6),
            opacity = 0.85
          ),
          showlegend = FALSE
        ) %>%
        plotly::layout(
          paper_bgcolor = "white",
          plot_bgcolor  = "white",
          margin = list(t = 5, b = 5, l = 5, r = 5)
        )
    }

    # ----- 0 facets -----
    if (length(facet_vars) == 0L) {
      p <- make_panel(d, show_scale = TRUE) %>%
        plotly::layout(
          title = list(text = paste("Average", biomarker, "by Ethnicity"), x = 0.5),
          margin = list(t = 50, b = 10, l = 10, r = 10)
        ) %>%
        plotly::config(displayModeBar = TRUE, scrollZoom = FALSE, doubleClick = "reset",modeBarButtonsToRemove = c("toImage"), displaylogo = FALSE)

      return(apply_equal_aspect(p, 1L))
    }

    # ----- 1 facet -----
    if (length(facet_vars) == 1L) {
      f <- facet_vars[1]
      lvls <- sort(unique(as.character(d[[f]])))
      lvls <- lvls[!is.na(lvls) & nzchar(lvls)]
      n_panels <- length(lvls)

      if (n_panels < 1L || n_panels > MAX_PANELS) return(plotly_blank())

      plots <- lapply(seq_along(lvls), function(i) {
        make_panel(d[as.character(get(f)) == lvls[i]], show_scale = (i == 1))
      })

      ncol <- if (n_panels <= 2L) n_panels else 2L
      nrow <- as.integer(ceiling(n_panels / ncol))

      anns <- lapply(seq_along(lvls), function(i) {
        r <- ((i - 1) %/% ncol) + 1
        c <- ((i - 1) %% ncol) + 1
        y_top <- 1 - (r - 1) / nrow
        list(
          x = (c - 0.5) / ncol,
          y = y_top - 0.01,
          xref = "paper",
          yref = "paper",
          text = paste0(make_label(f), ": ", lvls[i]),
          showarrow = FALSE,
          xanchor = "center",
          yanchor = "top",
          font = list(size = 12),
          bgcolor = "rgba(230,230,230,0.85)",
          bordercolor = "rgba(0,0,0,0.25)",
          borderwidth = 1
        )
      })

      p <- plotly::subplot(plots, nrows = nrow, shareX = TRUE, shareY = TRUE, margin = 0.02) %>%
        plotly::layout(
          title = list(text = paste("Average", biomarker, "by Ethnicity"), x = 0.5),
          annotations = anns,
          margin = list(t = 70, b = 10, l = 10, r = 10)
        ) %>%
        plotly::config(displayModeBar = TRUE, scrollZoom = FALSE, doubleClick = "reset")

      return(apply_equal_aspect(p, length(plots)))
    }

    # ----- 2 facets -----
    facet_vars <- facet_vars[1:2]
    f1 <- facet_vars[1]; f2 <- facet_vars[2]

    r_lvls <- sort(unique(as.character(d[[f1]])))
    c_lvls <- sort(unique(as.character(d[[f2]])))
    r_lvls <- r_lvls[!is.na(r_lvls) & nzchar(r_lvls)]
    c_lvls <- c_lvls[!is.na(c_lvls) & nzchar(c_lvls)]

    nrow <- length(r_lvls)
    ncol <- length(c_lvls)
    n_panels <- nrow * ncol

    if (n_panels < 1L || n_panels > MAX_PANELS) return(plotly_blank())

    plots <- list()
    for (rr in r_lvls) {
      for (cc in c_lvls) {
        dt_sub <- d[as.character(get(f1)) == rr & as.character(get(f2)) == cc]
        plots[[length(plots) + 1]] <- make_panel(
          dt_sub,
          show_scale = identical(rr, r_lvls[1]) && identical(cc, c_lvls[1])
        )
      }
    }

    col_anns <- lapply(seq_along(c_lvls), function(ci) {
      list(
        x = (ci - 0.5) / ncol,
        y = 1,
        xref = "paper",
        yref = "paper",
        text = paste0(make_label(f2), ": ", c_lvls[ci]),
        showarrow = FALSE,
        xanchor = "center",
        yanchor = "bottom",
        font = list(size = 12),
        bgcolor = "rgba(230,230,230,0.85)",
        bordercolor = "rgba(0,0,0,0.25)",
        borderwidth = 1
      )
    })

    row_anns <- lapply(seq_along(r_lvls), function(ri) {
      list(
        x = 1,
        y = 1 - (ri - 0.5) / nrow,
        xref = "paper",
        yref = "paper",
        text = paste0(make_label(f1), ": ", r_lvls[ri]),
        showarrow = FALSE,
        xanchor = "left",
        yanchor = "middle",
textangle = 90,
        font = list(size = 12),
        bgcolor = "rgba(230,230,230,0.85)",
        bordercolor = "rgba(0,0,0,0.25)",
        borderwidth = 1
      )
    })

    p <- plotly::subplot(plots, nrows = nrow, shareX = TRUE, shareY = TRUE, margin = 0.02) %>%
      plotly::layout(
        title = list(text = paste("Average", biomarker, "by Ethnicity"), x = 0.5),
        annotations = c(col_anns, row_anns),
        margin = list(t = 80, b = 10, l = 10, r = 110)
      ) %>%
      plotly::config(displayModeBar = TRUE, scrollZoom = FALSE, doubleClick = "reset", modeBarButtonsToRemove = c("toImage"), displaylogo = FALSE)

    apply_equal_aspect(p, length(plots))
  }, error = function(e) {
    plotly_blank()
  })
})

  # Correlation data filtering
  correlation_data <- reactive({
    req(input$biomarker_x, input$biomarker_y, input$age)

    data <- df[
      age >= input$age[1] &
        age <= input$age[2] &
        !is.na(get(input$biomarker_x)) &
        !is.na(get(input$biomarker_y))
    ]

    if (input$corr_gender != "all") {
      data <- data[gender == input$corr_gender]
    }
   if (input$corr_ethnicity != "all") {
  data <- data[as.character(T_NT) == input$corr_ethnicity]
}

    if (input$corr_medication != "all") {
      data <- data[history_illness.medication_currently_status == input$corr_medication]
    }

    return(data)
  })

  # Correlation plot
  output$correlation_plot <- renderPlotly({
    data <- correlation_data()
    validate(
      need(
        nrow(data) >= K_MIN,
        sprintf("Not enough samples to calculate correlation (n < %d). Please broaden filters.", K_MIN)
      )
    )

    req(nrow(data) > 10)

    x_var <- input$biomarker_x
    y_var <- input$biomarker_y

    p <- ggplot(data, aes_string(x = x_var, y = y_var)) +
      geom_point(alpha = 0.6, size = 1.5, color = "#1a5d89") +
      geom_smooth(method = "lm", se = TRUE, color = "red", alpha = 0.3) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray60")
      ) +
      labs(
        title = paste("Correlation between", x_var, "and", y_var),
        subtitle = paste("N =", nrow(data)),
        x = x_var,
        y = y_var
      )

    correlation_plot <- ggplotly(p, tooltip = c("x", "y")) %>%
      layout(margin = list(t = 80, b = 50, l = 50, r = 50)) %>%
      config(displayModeBar = FALSE)

    x_hist <- plot_ly(
      data,
      x = ~get(x_var),
      type = "histogram",
      nbinsx = 30,
      marker = list(color = "lightblue", line = list(color = "black", width = 1)),
      showlegend = FALSE
    ) %>%
      layout(
        yaxis = list(title = "Count"),
        margin = list(b = 0),
        xaxis = list(title = "")
      )

    y_hist <- plot_ly(
      data,
      y = ~get(y_var),
      type = "histogram",
      nbinsy = 30,
      marker = list(color = "lightcoral", line = list(color = "black", width = 1)),
      showlegend = FALSE
    ) %>%
      layout(
        xaxis = list(title = "Count"),
        margin = list(l = 0),
        yaxis = list(title = "")
      )

    subplot(
      x_hist, plotly_empty(),
      correlation_plot, y_hist,
      nrows = 2,
      widths = c(0.75, 0.25),
      heights = c(0.25, 0.75),
      shareX = TRUE,
      shareY = TRUE,
      titleX = TRUE,
      titleY = TRUE
    ) %>%
      layout(margin = list(t = 50, b = 50, l = 50, r = 50)) %>%
      config(displayModeBar = FALSE)
  })

  # Correlation statistics
  output$correlation_stats <- renderText({
    data <- correlation_data()
    validate(
      need(
        nrow(data) >= K_MIN,
        sprintf("Not enough samples to calculate correlation (n < %d). Please broaden filters.", K_MIN)
      )
    )

    req(nrow(data) > 10)

    x_var <- input$biomarker_x
    y_var <- input$biomarker_y

    corr_coef <- cor(data[[x_var]], data[[y_var]], use = "complete.obs")
    cor_test <- cor.test(data[[x_var]], data[[y_var]])

    x_mean <- mean(data[[x_var]], na.rm = TRUE)
    y_mean <- mean(data[[y_var]], na.rm = TRUE)
    x_sd <- sd(data[[x_var]], na.rm = TRUE)
    y_sd <- sd(data[[y_var]], na.rm = TRUE)

    paste(
      ":::CORRELATION STATISTICS:::\n",
      "Sample Size: ", nrow(data), "\n",
      "Correlation Coefficient (r): ", round(corr_coef, 4), "\n",
      "R-squared: ", round(corr_coef^2, 4), "\n",
      "P-value: ", format.pval(cor_test$p.value, digits = 4), "\n",
      "95% Confidence Interval: [", round(cor_test$conf.int[1], 4), ", ", round(cor_test$conf.int[2], 4), "]\n\n",
      ":::DESCRIPTIVE STATISTICS:::\n",
      x_var, " - Mean: ", round(x_mean, 2), ", SD: ", round(x_sd, 2), "\n",
      y_var, " - Mean: ", round(y_mean, 2), ", SD: ", round(y_sd, 2), "\n\n",
      ":::INTERPRETATION:::\n",
      "Direction: ", ifelse(corr_coef > 0, "Positive", "Negative"), "\n",
      "Statistical Significance: ", ifelse(cor_test$p.value < 0.05, "Significant (p < 0.05)", "Not Significant (p \u2265 0.05)")
    )
  })
}

# Run the app
shinyApp(ui, server)
