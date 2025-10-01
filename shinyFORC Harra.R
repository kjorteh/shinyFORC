req <- c("shiny", "ggplot2", "plotly", "DT", "blavaan", "bayesplot", "shinyBS", "reshape2", "viridis", "dplyr", "rstanarm", "loo", "LaplacesDemon", "caret", "BMS", "bsplus","shinyWidgets", "shinythemes")

installed <- rownames(installed.packages())
for (p in req) {if (!p %in% installed) {install.packages(p, dependencies = TRUE) } }

library(shiny, quietly = T)
library(ggplot2, quietly = T)
library(plotly, quietly = T)
library(DT, quietly = T)
library(blavaan, quietly = T)
library(bayesplot, quietly = T)
library(shinyBS, quietly = T)
library(reshape2, quietly = T) 
library(viridis, quietly = T)
library(dplyr, quietly = T)
library(rstanarm, quietly = T)
library(loo, quietly = T)
library(LaplacesDemon, quietly = T)
library(caret, quietly = T)
library(BMS, quietly = T)
library(bsplus, quietly = T)
library(shinyWidgets, quietly = T)
library(shinythemes, quietly = T)

selectInputWithPopover <- function(id, label, choices, popover_html, ...) {
  div(
    style = "margin-bottom: 20px;",  
    tags$label(label, style = "display: inline-block;"), 
    tags$i(
      class = "fa fa-question-circle",
      style = "margin-left: 5px; cursor: pointer;",
      `data-toggle` = "popover",  
      `data-html` = "true", 
      `data-content` = popover_html  
    ),
    selectInput(id, label = NULL, choices = choices, ...)  
  )
}

ui <- navbarPage(theme = shinytheme("yeti"),
  header = tagList(
    tags$head(
      tags$script('$(function () { $(\'[data-toggle="popover"]\').popover() })')
    ),
    tags$style(
      HTML(
        "
      body {
        background-color: #F7F7F7; /* Change this to the desired background color */
        font-size: 18px; /* Increase the global font size */
      }
      .wide-output {
        width: 116%; /* Make the output container full width */
        white-space: pre-wrap; /* Ensure text wraps correctly */
        overflow-x: auto; /* Allow horizontal scrolling if needed */
      }
      #model_message {
        margin-top: 16px; /* Adjust the spacing as needed */
      }
      .narrow-sidebar {
        max-width: 340px; /* Set the maximum width for the sidebar panel */
      }
      /* Custom styles for the animated progress bar */
      .progress-bar-striped {
        background-color: #9B0000; /* Bootstrap primary color */
      }
     .progress-bar {
        background-color: #9B0000;
      }
      .progress-bar-animated {
        animation: progress-bar-stripes 1s linear infinite;
      }
      /* Increase font size for pickerInput dropdown */
      .dropdown-menu {
        font-size: 16px; /* Adjust dropdown menu font size */
      }
      .dropdown-menu .active {
      background-color: #9B0000 !important; /* Change highlight color to red */
      border-color: #9B0000; 
    }
    .dropdown-menu .active a {
      color: white !important; /* Change the text color to white when highlighted */
      border-color: #9B0000; 
    }
    .dropdown-toggle:focus, .dropdown-toggle:hover {
      box-shadow: 0 0 3px #9B0000;  /* Apply dark red shadow */
      border-color: #9B0000;  /* Optional: Change border color */
    }
    .bs-searchbox input.form-control {
      border-color: #9B0000 !important; /* Change border color to red */
       box-shadow: 0 0 3px #9B0000;
    }
    /* Apply shadow to the search bar within pickerInput dropdown */
    .bs-searchbox input:focus {
      box-shadow: 0 0 3px #9B0000;
      border-color: #9B0000;
    }
     /* Remove default orange highlight and apply dark red border and shadow */
    .btn:focus, .btn:active, .btn:focus:active {
      outline: none;                 /* Remove default outline */
      box-shadow: 0 0 5px #9B0000;   /* Dark red shadow */
      border-color: #9B0000;         /* Dark red border */
    }
    
    .btn {
      font-size: 16px; /* Adjust font size for the picker button */
    }
      
     /* Increase font size for tab panel titles */
      .nav-tabs > li > a {
        font-size: 16px; /* Adjust font size for tab panel titles */
      }
      /* Increase font size for navbar tab titles */
      .navbar-nav > li > a {
        font-size: 16px; /* Adjust font size for navbar tab titles */
      }
      /* Increase font size for input titles (e.g., pickerInput, textInput) */
      label {
        font-size: 16px; /* Adjust font size for input titles */
      }
      /* Increase font size for DataTables */
      .dataTables_wrapper {
        font-size: 16px; /* Adjust font size for the entire DataTable */
      }
      /* Increase font size for selectInput and numericInput */
      .shiny-input-container select, 
      .shiny-input-container input[type='number'] {
        font-size: 16px; /* Adjust font size for selectInput and numericInput */
      }
      /* Increase font size for the title */
      .navbar-brand {
        font-size: 24px; /* Adjust font size for the navbar title */
      }
      
       /* Custom styles for the slider */
      .irs {
        background: transparent !important; /* Make the background transparent if needed */
      }
      .irs-bar {
        background: #9B0000 !important; /* Change slider fill color to red */
        border: 1px solid #9B0000 !important; /* Change slider border color to red */
      }
      .irs-from, .irs-to {
        color: white !important; /* Color for the labels on the slider */
      }
      .irs-single {
        color: white !important; /* Change the current value label color to white */
        background: #9B0000 !important; /* Change the background of the current value label to red */
        border-radius: 5px; /* Optional: add some rounding to the corners */
        padding: 2px 5px; /* Optional: add some padding for better appearance */
      }
      .irs-slider {
        background: darkred !important; /* Slider handle color */
        border: 1px solid #fff !important; /* Optional: handle border color */
      }
      .irs-slider:before {
        background: darkred !important; /* Optional: handle shadow or border */
      }
      
      /* Custom styles for checkboxInput */
      input[type='checkbox'] {
        accent-color: #9B0000; /* Change the checkbox color to dark red */
      }
      .big-table {
        font-size: 18px; /* Adjust font size as needed */
      }
      input[type='number']:focus {
        border-color: #9B0000;  /* Border color */
        box-shadow: 0 0 1px #9B0000;  /* Outer shadow color */
        outline: none;  /* Remove the default outline */
      }
      
      /* Change the border color for selectInputWithPopover (selectize) */
      .selectize-input {
        border-color: #9B0000; /* Set the border color to red */
      }

      /* Default border for selectInputWithPopover (selectize) */
      .selectize-input {
        border: 1px solid #ccc; /* Default border color */
        border-radius: 1px;  /* Keep rounded corners */
        outline: none;
      }

      /* Change border color to red when hovering */
      .selectize-input:hover {
        border-color: #9B0000 !important;  /* Change border to red on hover */
        box-shadow: 0 0 1px #9B0000 !important;  /* Add red glow */
      }

      /* Change border color to red on focus */
      .selectize-input:focus {
        border-color: #9B0000 !important;  /* Ensure border stays red when focused */
        box-shadow: 0 0 1px #9B0000 !important;  /* Add red glow */
        outline: none;  /* Remove default outline */
      }

       /* Default background color for options */
      .selectize-dropdown .option {
        background-color: white !important; /* Default background for options */
        color: black !important; /* Default text color */
      }

      /* Change background color of the hovered or selected option */
      .selectize-dropdown .option:hover {
        background-color: #9B0000 !important; /* Red background for hovered option */
        color: white !important; /* White text for better contrast */
      }

      /* Change background color of the active option (selected in input) */
      .selectize-input.items.not-full .selectize-input {
        background-color: #9B0000 !important; /* Red background for the selected item */
        color: white !important; /* White text for the selected item */
      }
      
      
      
    p {
      font-size: 14px; /* Slightly larger font for paragraph text */
    }
    ul {
      font-size: 16px
      list-style-type: disc; /* Bullet points for list */
      margin-left: 20px; /* Indentation for the list */
    }
    
    .custom-header {
      color: black;  /* Set specific h4 headers to black */
    }
    

      ")),
    tags$script(HTML("
      $(document).on('focus', '.selectize-input', function() {
        $(this).css('border-color', '#9B0000');
      });
      $(document).on('blur', '.selectize-input', function() {
        $(this).css('border-color', '');
      });
    "))
  ),
  title = "ShinyFORC: A Shiny App for Bayesian Probabilistic Forecasting Under Model Uncertainty",
  tabPanel("About",
           h2("About This Shiny App"),
           uiOutput("about"),
           br(), br(),
           img(src = "uwlogocenter.png", height = "100px", style = "margin-right: 20px;"),  
           img(src = "Heidelberg_Logo.svg.png", height = "100px"),
           br(), br() ),
    tabPanel("Tutorial",
             tabPanel("Tutorial",
                      h2("How to Use ShinyFORC"),
                      #br(),
                      
                      h3("1. Upload and Explore Your Data"),
                      p("Start by uploading your dataset under the 'Data and Plots' tab. Supported formats include CSV and TXT files."),
                      tags$ul(
                        tags$li("Use the outcome variable selector to select time-series outcomes to visualize."),
                        tags$li("Optionally, choose an ID variable to label observations, randomly sample a select number of observations, and facet plots by a variable."),
                        tags$li("Click 'Create plots' to generate interactive visualizations.")
                      ),
                      br(),
                      
                      h3("2. Fit a Growth Curve Model"),
                      p("In the 'Growth Model' tab, define and estimate a Bayesian growth curve model."),
                      tags$ul(
                        tags$li("Choose your outcome variables (3+ required) and optional time-invariant predictors."),
                        tags$li("Select a time indicator: Linear, Quadratic, or Latent Basis."),
                        tags$li("Customize model specifications including time spacing, MCMC settings, and priors."),
                        tags$li("Click 'Run Model' to fit the model using MCMC sampling."),
                        tags$li("Navigate to the 'Diagnostics' subtab for trace, autocorrelation, and density plots."),
                        tags$li("In the 'Results' subtab, review summary output and generate probabilistic forecasts. Results and selected options can be saved locally.")
                      ),
                      br(),
                      
                      h3("3. Perform Model Ensembling"),
                      p("Use the 'Model Ensembling' tab to combine predictions from multiple models to generate forecasts. Users can select to just use one ensembling method, or both and compare results."),
                      
                      tags$h4("Bayesian Stacking"),
                      tags$ul(
                        tags$li("Choose the number of models and specify predictors for each."),
                        tags$li("Run Bayesian linear regression models and compare out-of-sample predictive performance using the leave-one-out information criterion (LOO-IC)."),
                        tags$li("Compute stacking weights and forecast future observations using the stacked model predictions."),
                        tags$li("Download stacking results for futher investigation.")
                      ),
                      
                      tags$h4("Bayesian Model Averaging (BMA)"),
                      tags$ul(
                        tags$li("Specify outcome and predictors."),
                        tags$li("Select prior options for the g-prior and model prior."),
                        tags$li("Run BMA and review model weights and marginal densities."),
                        tags$li("Download model results and diagnostic plots.")
                      ),
                      br(),
                      
                      h3("4. Method Comparison"),
                      p("Bayesian stacking and BMA results can be compared for users who run both ensembling procedures here."),
                      tags$ul(
                        tags$li("KLDs for ensembled predictions can be compared, where smaller KLD values denote better performance."),
                        tags$li("Trend forecasting plots can be generated with a user-specified number of time plots and error bars to compare trajectories of stacking and BMA."),
                        tags$li("Lastly, results can be saved for further inspection")
                      ),
                      br(),

                      h3("Need More Help?"),
                      HTML("
                        <ul>
                        <li>A more in-depth walkthrough of shinyFORC is forthcoming (Harra & Kaplan, in preparation).</li>
                        <li>For questions or support, contact <b>Kjorte Harra</b> at 
                          <a href='mailto:harra@wisc.edu'>harra@wisc.edu</a> or visit 
                          <a href='https://github.com/kjorteh' target='_blank'>GitHub</a>.</li>
                      </ul>
                      <br>
                      <p><b>Citation:</b> Harra, K., & Kaplan, D. (in preparation). 
                           <i>ShinyFORC: A Shiny App for Bayesian Probabilistic Forecasting Under Model Uncertainty</i>.</p>"),
                      br(), br(),
                      img(src = "uwlogocenter.png", height = "100px", style = "margin-right: 20px;"),  
                      img(src = "Heidelberg_Logo.svg.png", height = "100px"),
                      br(), br() )),
    tabPanel("Data and Plots",
             h2("Dataset Exploration"),
             sidebarPanel(
               fileInput("data_file", "Browse Data", multiple = FALSE, accept = c(".csv", ".txt")),
               pickerInput("x_axis", "Outcome Variables", choices = NULL, multiple = TRUE, 
                           options = pickerOptions(liveSearch = T, actionsBox = T, noneSelectedText = "Select variables")),
               pickerInput("group_variable", "Select Observation ID Variable (optional)", choices = NULL, options = pickerOptions(liveSearch = T, noneSelectedText = "Select variable")),
               checkboxInput("sample_data_checkbox", "Randomly Sample Observations?", value = FALSE),
               conditionalPanel(
                 condition = "input.sample_data_checkbox == true",
                 sliderInput("num_obs", "Number of Observations to Sample", value = 5, min = 1, max = 100, ticks = F)),
               pickerInput("facet_variable", "Facet By Variable (optional)", choices = c("None", NULL), selected = NULL), options = pickerOptions(liveSearch = T),
               actionButton("run_button", "Create plots", icon = NULL), 
               class = "narrow-sidebar"),
             mainPanel(
               DT::dataTableOutput("data_table"),  
               plotlyOutput("random_sample_plots") ) ),
    tabPanel("Growth Model",
             tabsetPanel(
               tabPanel("Model Setup",
                        h3("Bayesian Growth Curve Model"),
                        sidebarPanel(
                          uiOutput("GCM_stop"),
                          pickerInput("outcome_variable", "Outcome Variables", choices = NULL, multiple = TRUE, options = pickerOptions(liveSearch = T, noneSelectedText = "Select variables", actionsBox = T)),
                          pickerInput("predictor_variable", "Time-Invariant Predictors", choices = NULL, multiple = TRUE, options = pickerOptions(liveSearch = T, noneSelectedText = "Select variable(s) - optional")),
                          numericInput("time_distance", "Linear Distance Between Time Points", value = 1, min = 1),
                          pickerInput("time_indicator", "Time Indicator", choices = c("Linear", "Quadratic", "Latent Basis"), multiple = F),
                          conditionalPanel(
                            condition = "input.time_indicator == 'Latent Basis'",
                            numericInput("num_free_points", "Number of Free Time Points (Latent Basis)", value = 1, min = 1) ),
                          sliderInput("n_chains", "Number of MCMC chains:", value = 4, min = 2, max = 10),
                          sliderInput("burn_in", "Burn-in iterations", value = 500, min = 100, max = 4000, step = 100), 
                          sliderInput("post_burn_in", "Post-Burn-in iterations", value = 1000, min = 1000, max = 10000, step = 500), 
                          checkboxInput("custom_priors", "Customize Priors", value = FALSE),
                          conditionalPanel(condition = "input.custom_priors == true",
                            wellPanel(
                              numericInput("location_i", "Mean (intercept)", value = 0),
                              numericInput("scale_i", "Precision (intercept)", value = 10),
                              numericInput("location_s", "Mean (slope)", value = 0),
                              numericInput("scale_s", "Precision (slope)", value = 10),
                              conditionalPanel(
                                condition = "input.time_indicator == 'Latent Basis'",
                                uiOutput("latent_basis_priors_ui") ) ) ),
                          actionButton("run_model_button", "Run Model")  ),
                        mainPanel(
                          verbatimTextOutput("model_error"),
                          uiOutput("model_message") ) ),
               tabPanel("Diagnostics",
                        tabsetPanel(
                          tabPanel("Trace Plots", plotlyOutput("trace_plots")),
                          tabPanel("Autocorrelation Function Plots", plotlyOutput("acf_plots")),
                          tabPanel("Density Plots", plotlyOutput("dens_plots")) ) ),
               tabPanel("Results",
                        tabsetPanel(
                          tabPanel("Model Summary", 
                                   sidebarPanel(
                                     downloadButton("download_model", "Download Growth Curve Model"),
                                     conditionalPanel(
                                       condition = "output.show_download",
                                       downloadButton("save_inputs_GCM", "Download Inputs") )),
                                   mainPanel(
                                     verbatimTextOutput("model_summary") ) ),
                          tabPanel("Plots",
                                   sidebarLayout(
                                     sidebarPanel(
                                       numericInput("future_timepoints", "Number of Future Time Points to Forecast", value = 1, min = 1),
                                       checkboxInput("sample_checkbox", "Randomly Sample Observations?", value = FALSE),
                                       conditionalPanel(
                                         condition = "input.sample_checkbox == true",
                                         sliderInput("sample_size", "Number of Observations to Sample", value = 5, min = 1, max = 100, ticks = F)),
                                       pickerInput("obs_id_variable", "Select Observation ID Variable:", choices = c("None", NULL), selected = "None", options = pickerOptions(liveSearch = T, noneSelectedText = "Select variable")),
                                       actionButton("create_plots_button", "Create Plots")),
                                     mainPanel(
                                       plotlyOutput("gcm_plot"),
                                       plotlyOutput("gcm_plot_all") ) ) ) ) ) ) ),
    tabPanel("Model Ensembling",
             tabsetPanel(
               tabPanel("Bayesian Stacking",
                        tabsetPanel(
                          tabPanel("Model Stack Specification",
                                   h3("Bayesian Stacking Models"),
                            sidebarLayout(
                              sidebarPanel(
                                numericInput("num_models", "Number of Models to Run", value = 3, min = 2), 
                                pickerInput("model_type", "Select Model Type", choices = c("Growth Rate", "Intercept")),
                                uiOutput("model_selectors"), 
                                actionButton("run_models", "Run Models") ),
                           mainPanel(
                             uiOutput("selected_values_ui"),
                             verbatimTextOutput("model_stack_summaries") ) ) ),
                          tabPanel("LOO-IC",
                            mainPanel(
                              verbatimTextOutput("loo_stack_summaries") ) ),
                          tabPanel("Stacking & Results",
                                   sidebarLayout(
                                     sidebarPanel(
                                       pickerInput("evaluation_metric", "Select Evaluation Metric", choices = c("ELPD", "PBMA", "PBMA+")),
                                       actionButton("compute_weights", "Compute Stack"),
                                       downloadButton("download_stacking_results", "Download Stacking Results"),
                                       numericInput("future_timepoints_stack", "Number of Future Time Points to Forecast", value = 1, min = 1),
                                       actionButton("generate_plots", "Create Plots")),
                                      mainPanel(
                                        uiOutput("stacking_weights_results"),
                                        uiOutput("stacking_results"),
                                        plotlyOutput("overall_stack_plot"),
                                        uiOutput("caption_stack")))))),
               tabPanel("Bayesian Model Averaging",
                        tabsetPanel(
                          tabPanel("BMA Specification",
                                   h3("BMA Specification"),
                                  sidebarLayout(
                                    sidebarPanel(
                                      uiOutput("BMA_stop"),
                                      pickerInput("outcome_variable_BMA", "Outcome Variables", 
                                                  choices = c("Growth Rate", "Intercept"), 
                                                  selected = "Growth Rate", 
                                                  multiple = FALSE), 
                                      pickerInput("predictor_variables_BMA", "BMA Predictors", choices = NULL, multiple = T, options = pickerOptions(actionsBox = T, liveSearch = T, noneSelectedText = "Select variables")),
                                      selectInputWithPopover(
                                        "g_BMA", 
                                        label = "g =", 
                                        choices = c("uip", "RIC", "BRIC", "HQ", "EBL", "hyper = 3", "hyper = 4", "hyper = UIP", "hyper = 65/32", "hyper = BRIC"),
                                        popover_html = 'This controls the choice of g-prior for regression coefficients. <br><a href="https://link.springer.com/article/10.1186/s40536-021-00108-2#Sec14" target="_blank">More info</a>'),
                                      selectInputWithPopover("mprior_BMA", label = "mprior =", choices = c("uniform", "fixed", "random"),
                                                             popover_html = 'This controls the choice of model prior. <br><a href="https://link.springer.com/article/10.1186/s40536-021-00108-2#Sec14" target="_blank">More info</a>'),
                                      conditionalPanel(
                                        condition = "input.mprior_BMA == 'fixed'",
                                        sliderInput("mprior_size_BMA", "mprior size =", value = 2, min = 1, max = 4) ),
                                      actionButton("run_BMA", "Run BMA"),
                                      downloadButton("download_BMA", "Download BMA Results") ),
                                    mainPanel(uiOutput("removed_vars_msg"),
                                      verbatimTextOutput("BMA_summary") ) )  ),
                          tabPanel("Diagnostics",
                                   sidebarLayout(
                                     sidebarPanel(
                                       downloadButton("download_BMA_margdens", "Download Marginal Densities Plot"),
                                       downloadButton("download_BMA_modelsize", "Download Model Size Plot") ),
                                   mainPanel(
                                     plotOutput("BMA_margdens"),
                                     uiOutput("caption_margdens"),
                                     plotOutput("BMA_modelsize"),
                                     uiOutput("caption_modelsize")))),
                          tabPanel("Results",
                                   sidebarLayout(
                                     sidebarPanel(
                                       numericInput("future_timepoints_BMA", "Number of Future Time Points to Forecast", value = 1, min = 1),
                                       actionButton("generate_plots_BMA", "Create Plots")), 
                                     mainPanel(
                                       plotlyOutput("overall_BMA_plot"),
                                       uiOutput("caption_BMA")
                                       #HTML("<p style='text-align: center; font-size: 14px; color: gray;'>The above plot provides error bars to denote 50%, 75%, and 95% Bayesian prediction intervals for the BMA estimates at future time points.</p>")
                                       # kld output
                                       ) ) ))),
             tabPanel("Method Comparisons",
                      tabsetPanel(
                        tabPanel("Performance",
                                 sidebarLayout(
                                   sidebarPanel(
                                     uiOutput("comparison_condition_message"), 
                                     downloadButton("download_kld_table", "Download KLD table") ),
                                   mainPanel(style = "float:left",
                                     DT::dataTableOutput("comparison_kld_table") ) ) ),
                        tabPanel("Visualizations",
                                 sidebarLayout(
                                   sidebarPanel(
                                     uiOutput("comparison_condition_message_2"),  
                                     numericInput("comparison_future_timepoints", "Number of future time points to forecast:", value = 1, min = 1),
                                     pickerInput("error_bars", "Visualize error bars for:", choices = c("None","Stacked predictions", "BMA predictions"), multiple = F, selected = "None", options(pickerOptions(actionsBox = T))),
                                     actionButton("generate_comparison_plot", "Generate Plot"),
                                     downloadButton("download_comparison_data", "Download Data Table")),
                                  mainPanel(
                                    plotlyOutput("comparison_plot"),
                                    uiOutput("caption_comparison"),
                                    DT::dataTableOutput("comparison_data_table")  ) ))) ))),
    tabPanel("Acknowledgements", 
             h2("Acknowledgements"),
             uiOutput("acknowledgements"),
             br(), br(),
             br(),
             img(src = "uwlogocenter.png", height = "100px", style = "margin-right: 20px;"),  
             img(src = "Heidelberg_Logo.svg.png", height = "100px"))
  ) #end bracket for UI

server <- function(input, output, session) {
  
  shinyjs::runjs('$(function () { $(\'[data-toggle="tooltip"]\').tooltip() })')
  
  output$about <- renderUI({
    isolate({
    HTML(paste(
     # "<h4>Overview</h4>",
      "ShinyFORC conducts Bayesian probabilistic forecasting (BPF) with longitudinal data. 
    The app implements our BPF framework that utilizes methodologies explicitly designed 
    to obtain optimally predictive measures of rate of change as the foundation for projecting 
    future trends.ShinyFORC can be applied to a variety of longitudinal data — 
    including country-level large-scale assessments, repeated measures for individuals, and more.",
      
      "<br><br>",
      
      "<h4>Modeling Framework</h4>",
      "ShinyFORC integrates latent growth estimation, Bayesian model averaging (BMA), 
    and Bayesian stacking into one interactive and accessible Shiny software application.
    The app uses the R package <a href='https://cran.r-project.org/package=blavaan' target='_blank'><code>blavaan</code></a>, 
    which runs Stan in the background, to provide latent growth estimates for each unit of analysis. 
    These estimates serve as inputs to the <a href='https://cran.r-project.org/package=BMS' target='_blank'><code>BMS</code></a>, 
    <a href='https://cran.r-project.org/package=rstanarm' target='_blank'><code>rstanarm</code></a>, and 
    <a href='https://cran.r-project.org/package=loo' target='_blank'><code>loo</code></a> packages, allowing users 
    to conduct and compare results from BMA and Bayesian stacking under various parameter 
    and model prior specifications.",
      
      "<br><br>",
      
      "<h4>Forecasting Capabilities</h4>",
      "The stacking procedures support multiple weighting options, including 
    ELPD-LOO, pseudo-BMA, and pseudo-BMA+ weighting. ShinyFORC also produces in-sample and pseudo-out-of-sample predictions, 
    performance measures, and interactive visualizations.",
      
      "<br><br>",
      
      "Finally, the app can conduct true out-of-sample forecasts and provide forecasting visualizations.",
      
      "<br><br>",
      
      "<h4>Audience</h4>",
      "ShinyFORC brings together advanced Bayesian methods into a single user-friendly application 
    for both technical and non-technical researchers working with longitudinal data.",
      
      "<br><br>",
      
      "<h4>Contact</h4>",
      "For questions or support, contact <b>Kjorte Harra</b> at 
    <a href='mailto:harra@wisc.edu'>harra@wisc.edu</a> or visit 
    <a href='https://github.com/kjorteh' target='_blank'>GitHub</a>.",
      
      "<br><br>",
      
      "<p><b>Citation:</b> Harra, K., & Kaplan, D. (in preparation). <i>ShinyFORC: A Shiny App for Bayesian Probabilistic Forecasting Under Model Uncertainty</i>.</p>",
      
      
      sep = ""
    ))
  }) })
  
  
  output$acknowledgements <- renderUI({
    isolate({
    HTML(paste(
      "ShinyBPF was developed by Kjorte Harra and David Kaplan, 
    in conjunction with ChatGPT. This work was supported by the Institute of Education Sciences, U.S. Department of Education, 
    through Grant # R305D220012 to The University of Wisconsin-Madison. The opinions expressed are those of the authors and do not represent the views of the Institute or the U.S. Department of Education.",
      
      "<br><br>",
      
      "We thank Professor Nina Jude and Jonas Stampka for valuable feedback 
    on the overall design of our app.",
      
      "<br><br>",
      
      "<h4>Contact</h4>",
      "For questions or support, contact <b>Kjorte Harra</b> at 
    <a href='mailto:harra@wisc.edu'>harra@wisc.edu</a> or visit 
    <a href='https://github.com/kjorteh' target='_blank'>GitHub</a>.",
      
      "<br><br>", 
      
      "<p><b>Citation:</b> Harra, K., & Kaplan, D. (in preparation). 
      <i>ShinyFORC: A Shiny App for Bayesian Probabilistic Forecasting Under Model Uncertainty</i>.</p>",
      sep = ""
    ))
  }) })
  
  
  
  observeEvent(input$data_file, {
    req(input$data_file)
    df <- read.csv(input$data_file$datapath)
  
    updatePickerInput(session, "x_axis", choices = colnames(df)) #EDA
    updatePickerInput(session, "group_variable", choices = c("None", colnames(df)))
    updatePickerInput(session, "facet_variable", choices = c("None", colnames(df)))
    observe({
      updateSliderInput(session, "num_obs", max = nrow(df))
      updateSliderInput(session, "sample_size", max = nrow(df))
    })
    
    updatePickerInput(session, "outcome_variable", choices = colnames(df)) #GCM
    updatePickerInput(session, "predictor_variable", choices = colnames(df))
    updatePickerInput(session, "obs_id_variable", choices = c("None", colnames(df))) 
    
    updatePickerInput(session, "predictor_variables_BMA", choices = names(df), selected =  colnames(df)) #BMA
    })
  
  output$data_table <- renderDataTable({
    req(input$data_file)
    df <- read.csv(input$data_file$datapath)
    df[] <- lapply(df, function(x) if(is.numeric(x)) round(x, 3) else x)
    datatable(df, options = list(pageLength = 10))})

  
  observeEvent(input$run_button, {
    req(input$data_file, input$x_axis, cancelOutput = T)
    
    df <- read.csv(input$data_file$datapath)
    df$Observation <- 1:nrow(df)  
    
    if (length(input$x_axis) < 2) {
      showNotification("Please select at least 2 time points for visualization.",
                       type = "error")
      return()  }
    
    if (input$num_obs > nrow(df)) {
      showNotification(sprintf("Please select fewer than %d observations for visualization.", nrow(df)+1), type = "error")
      return() }
    
    if (input$num_obs < 1){
      showNotification("Please select at least 1 observation for visualization.", type = "error")
      return() }
    
    if (input$sample_data_checkbox) {
      sample_size <- input$num_obs
      sampled_rows <- df[sample(nrow(df), min(sample_size, nrow(df))), ] 
    } else {
      sampled_rows <- df }
    
    x_vars <- intersect(input$x_axis, colnames(sampled_rows))
    if (length(x_vars) == 0) { return(NULL)  }
    
    group_var <- input$group_variable
    if (group_var != "None" && group_var %in% colnames(sampled_rows)) {
      sampled_rows[[group_var]] <- as.factor(sampled_rows[[group_var]])  
    } else {
      group_var <- "Observation" 
      sampled_rows[[group_var]] <- as.factor(sampled_rows[[group_var]])  }
    
    id_vars <- c(group_var, input$facet_variable)
    id_vars <- id_vars[!is.null(id_vars) & id_vars != "None"]
    measure_vars <- x_vars
   
    melted_data <- reshape2::melt(sampled_rows, id.vars = id_vars, measure.vars = measure_vars)
    melted_data[[group_var]] <- as.factor(melted_data[[group_var]])

    
    random_sample_plot <- ggplot(melted_data, aes_string(x = "variable", y = "value", group = group_var)) +
      geom_line(aes_string(color = group_var)) +  
      geom_point(aes_string(color = group_var)) +
      scale_color_viridis(discrete = TRUE, option = "D") +
      labs(title = "Random Sample of Observations", x = "Outcome Variables", y = "Values", color = "Observation") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    facet_var <- input$facet_variable
    if (facet_var != "None" && facet_var %in% colnames(melted_data)) {
      random_sample_plot <- random_sample_plot + facet_wrap(as.formula(paste("~", facet_var)))
    } else if (facet_var != "None") {
      showNotification(paste("Facet variable", facet_var, "is not present in the data."), type = "warning") }
    
    output$random_sample_plots <- renderPlotly({
      ggplotly(random_sample_plot) })
  })
  
  reactive_values <- reactiveValues(y_growth = NULL, y_int = NULL)
  
  
  output$GCM_stop <- renderUI({
    if (is.null(input$data_file) || is.null(input$data_file$datapath)) {
      return(div(style = "color: #9B0000;", "Please select a data file under 'Data and Plots' first."))} })
  

  # GCM 
  output$latent_basis_priors_ui <- renderUI({
    req(input$num_free_points)
    num_free_points <- input$num_free_points
    
    lapply(1:num_free_points, function(i) {
      tagList(
        numericInput(paste0("location_latent_", i), 
                     paste("Mean (latent basis) for Free Point", i), value = 0),
        numericInput(paste0("scale_latent_", i), 
                     paste("Precision (latent basis) for Free Point", i), value = 1) ) }) })
  
  reactive_vals_outcome <- reactiveValues()
  reactive_fit_list_est <- reactiveValues(fit_est_list = list())
  empirical_df <- reactiveValues(data = NULL)
  
  
  # GCM 
  observeEvent(input$run_model_button, {
    req(input$outcome_variable, input$time_indicator, input$data_file)
    
    df <- read.csv(input$data_file$datapath)
    outcome_var <- input$outcome_variable
    reactive_vals_outcome$outcome_var <- input$outcome_variable
    time_indicator <- input$time_indicator
    predictor_var <- input$predictor_variable 
    num_free_points <- input$num_free_points
    time_distance <- input$time_distance
    future_time <- input$future_time
    
    output$show_download <- reactive({
      exists("fit")
    })
    
    outputOptions(output, "show_download", suspendWhenHidden = FALSE)
    
    output$save_inputs_GCM <- downloadHandler(
      filename = function() {
        paste("bgrowth_inputs_", Sys.Date(), ".Rdata", sep = "")
      },
      content = function(file) {
        inputs_GCM <- list(
          outcome_variable = input$outcome_variable,
          predictor_variable = input$predictor_variable,
          time_distance = input$time_distance,
          time_indicator = input$time_indicator,
          n_free_points = if (input$time_indicator == "Latent Basis") input$num_free_points else NA,
          n_chains = input$n_chains,
          burn_in = input$burn_in,
          post_burn_in = input$post_burn_in,
          custom_priors = input$custom_priors,
          location_i = if (input$custom_priors) input$location_i else NA,
          scale_i = if (input$custom_priors) input$scale_i else NA,
          location_s = if (input$custom_priors) input$location_s else NA,
          scale_s = if (input$custom_priors) input$scale_s else NA )
        
        save(inputs_GCM, file = file) } )
    

    if(input$time_distance <= 0){
      showNotification("Distance between time points must be greater than 0.", type = "error")
      return()
    }
    
    if(input$num_free_points <= 0){
      showNotification("Number of free time points must be greater than 0.", type = "error")
      return()
    }
    
    if(input$scale_s <= 0 | input$scale_i <= 0){
      showNotification("Prior precision parameters must be greater than 0.", type = "error") 
      return()
    }
 
    if (length(outcome_var) < 3) {
      showNotification("Please select at least three outcome variables.",
                       type = "error")
      return()  }
    
    if (time_indicator == "Latent Basis" && (length(outcome_var) - num_free_points < 2)) {
      showNotification("The number of free points is too high. Ensure that there are at least 2 fixed time points.",
                       type = "error")
      return()  }
    
    if (input$time_indicator == "Latent Basis" && !input$custom_priors) {
      showNotification(
        "Warning: You have selected 'Latent Basis' but have not checked 'Customize Priors'. We strongly recommend you customize the latent basis free parameters.",
        type = "warning")}
    
    if (input$custom_priors) {
      location_i <- input$location_i
      scale_i <- input$scale_i
      location_s <- input$location_s
      scale_s <- input$scale_s
      free_time_priors <- lapply(1:num_free_points, function(i) {
        list(location = input[[paste0("location_latent_", i)]],
             scale = input[[paste0("scale_latent_", i)]]) })
      
      free_time_scales <- sapply(free_time_priors, function(p) p$scale)
      
      if (scale_i <= 0 || scale_s <= 0 || any(free_time_scales <= 0)) {
        # Show notification if any scale parameter is not greater than 0
        showNotification("All scale parameters must be greater than 0.", type = "error") 
        return()}
      
    } else {
      location_i <- NULL
      scale_i <- NULL
      location_s <- NULL
      scale_s <- NULL
      free_time_priors <- NULL}
    
    
    output$model_message <- renderUI({"Running model, please wait..."})
    
    withProgress(message = 'Running model, please wait...', value = 0, { #can this better update based on model progress? 
      
      
      if (time_indicator == "Linear") {
        time_values <- seq(0, by = time_distance, length.out = length(outcome_var))
      } else if (time_indicator == "Quadratic") {
        time_values <- (seq(0, length.out = length(outcome_var)) * time_distance)^2
      } else if (time_indicator == "Latent Basis") {
        num_fixed_points <- length(outcome_var) - num_free_points
        time_values <- c(seq(0, by = time_distance, length.out = num_fixed_points), rep(NA, num_free_points)) }
      
      intercept_terms <- paste0('1*', outcome_var, collapse = ' + ')
      
      if (time_indicator == "Latent Basis") {
        fixed_terms <- paste0(seq(0, by = time_distance, length.out = num_fixed_points), '*', outcome_var[1:num_fixed_points], collapse = ' + ')
        
        if (input$custom_priors) {
          free_terms <- paste0(sapply(1:num_free_points, function(i) {
            paste0('prior("normal(', free_time_priors[[i]]$location, ',', free_time_priors[[i]]$scale, '")*', outcome_var[num_fixed_points + i])
          }), collapse = ' + ')
        } else {
          free_terms <- paste0('1*', outcome_var[(num_fixed_points + 1):length(outcome_var)], collapse = ' + ') }
        
        model <- paste0('
          i =~ ', intercept_terms, '
          s =~ ', fixed_terms, ' + ', free_terms, '
        ')
        
      } else {
        slope_terms <- paste0(time_values, '*', outcome_var, collapse = ' + ')
        model <- paste0('
          i =~ ', intercept_terms, '
          s =~ ', slope_terms, '
        ')}
      
      if (!is.null(predictor_var) && length(predictor_var) > 0) {
        predictors <- paste0(predictor_var, collapse = ' + ')
        model <- paste0(model, '
          #i ~ ', predictors, '
          s ~ ', predictors, ' 
        ') } 
      
      if (!is.null(location_i) && !is.null(scale_i) && !is.null(location_s) && !is.null(scale_s)) {
        model <- paste0(model, '
          i ~ prior("normal(', location_i, ',', scale_i, ')")*1
          s ~ prior("normal(', location_s, ',', scale_s, ')")*1
        ')}
      
      n_chains <- input$n_chains 
      burn_in <- input$burn_in
      post_burn_in <- input$post_burn_in
      
      fit <- bgrowth(model, data = df, save.lvs = T, 
                     n.chains = n_chains, burnin = burn_in, 
                     sample = post_burn_in, 
                     bcontrol = list(cores = parallel::detectCores()), 
                     control = list(progress = 10))
      
      reactive_fit_list_est$fit_est_list[[1]] <- fit@ParTable$est
      #print(reactive_fit_list_est$fit_est_list[[1]])
      
      for (i in 1:post_burn_in) {
        incProgress(1 / post_burn_in, detail = paste("Iteration", i)) }
      
      df$growth <- blavInspect(fit, "lvmeans")[,2]
      df$int <- blavInspect(fit, "lvmeans")[,1]
      reactive_values$y_growth <- blavInspect(fit, "lvmeans")[,2]
      reactive_values$y_int <- blavInspect(fit, "lvmeans")[,1]
      
      output$model_summary <- renderPrint({summary(fit)})
      
      observe({
        showNotification(
          "Model has finished running. Check the 'Results' tab for the summary.", 
          type = "message")})
      
      output$model_message <- renderUI({
        # tagList(
        #   tags$br(), tags$br(),
        #   tags$p("Model has finished running."),
        #   tags$strong("Check the 'Results' tab for the summary.")) 
        
        tags$div(
          tags$h4("Model has finished running."), 
          tags$strong("Check the 'Results' tab for the summary."),  
          HTML(paste(rep("<br>", 2), collapse = ""))  
        )
        
        }) 
      
      trace_plots <- plot(fit, plot.type = "trace", facet_args = list(ncol = 2, strip.position = "top"))
      output$trace_plots <- renderPlotly({ggplotly(trace_plots) })
      
      acf_plots <- plot(fit, plot.type = "acf")
      output$acf_plots <- renderPlotly({ggplotly(acf_plots) })
      
      dens_plots <- plot(fit, plot.type = "dens", facet_args = list(ncol = 2, strip.position = "top"))
      output$dens_plots <- renderPlotly({ggplotly(dens_plots) })
      
      output$download_model <- downloadHandler(
        filename = function() {
          paste("bgrowth_model_", Sys.Date(), ".rds", sep = "") },
        content = function(file) {
          saveRDS(fit, file) }  )
      
      observeEvent(input$create_plots_button, {  
        obs_id_var <- input$obs_id_variable
        int <- df[, "int"]
        slp <- df[,"growth"]
        
        if (input$future_timepoints < 1){
          showNotification("Please specify at least 1 future time point for visualization.", type = "error")
          return() }
        
        #latent_reactive_time <- reactiveVal(fit@ParTable$est[(length(outcome_var) + 1):(length(outcome_var) * 2)])
        
        calculate_time_values <- function(time_indicator, time_distance, outcome_var_length, future_points) {
          if (time_indicator == "Linear") {
            time_values <- seq(0, by = time_distance, length.out = outcome_var_length + future_points)
          } else if (time_indicator == "Quadratic") {
            time_values <- (seq(0, length.out = outcome_var_length + future_points) * time_distance)^2
          } else if (time_indicator == "Latent Basis") {
            latent_time <- reactive_fit_list_est$fit_est_list[[1]][(outcome_var_length + 1):(outcome_var_length * 2)]
            #latent_time <- latent_reactive_time() 
            future_time <- seq(length(latent_time), length.out = future_points) * time_distance
            time_values <- c(latent_time, future_time)
          } else {
            stop("Invalid time_indicator. Please choose 'Linear', 'Quadratic', or 'Latent Basis'.") #don't really need this for the app ig
          }
          
          return(list(time_values = time_values, future_points = future_points)) }
        
        time_values_new <- calculate_time_values(time_indicator = time_indicator, time_distance = time_distance, 
                                                 outcome_var_length = length(outcome_var), future_points = input$future_timepoints)
        
        samples <- blavPredict(fit)
        int_all_samples <- unlist(lapply(samples, function(x) x[, "i"]))  
        slp_all_samples <- unlist(lapply(samples, function(x) x[, "s"]))  
        mean_int <- mean(int_all_samples)
        mean_slp <- mean(slp_all_samples)
        
        int_quantiles <- quantile(int_all_samples, probs = c(0.025, 0.975))
        slp_quantiles <- quantile(slp_all_samples, probs = c(0.025, 0.975))
        
        
        y_avg <- mean_int + (mean_slp * time_values_new$time_values)
        y_lower <- int_quantiles[1] + slp_quantiles[1] * time_values_new$time_values
        y_upper <- int_quantiles[2] + slp_quantiles[2] * time_values_new$time_values
        
        res_df <- data.frame(time = time_values_new$time_values, 
                             y = y_avg, 
                             ymin = y_lower, 
                             ymax = y_upper, Model = "GCM")
        
        #empirical_df <- data.frame(time = time_values, y = 3, Model = "Empirical")
        #colors <- c("GCM" = "#21908CFF", "Empirical" = "darkgray")
        
        gcm_plot <- ggplot(res_df, aes(x = time, y = y)) +
          geom_vline(xintercept = time_values_new$time_values[length(time_values_new$time_values) - time_values_new$future_points], 
                     linetype = "dashed", color = "black", alpha = .7) +
          geom_line(size = 2, show.legend = F, color = viridis(7)[4]) + 
          geom_point(size = 4, color = viridis(7)[4]) +
          geom_ribbon(aes(ymin = ymin, ymax = ymax), fill = viridis(7)[4], alpha = 0.2) +
          labs(title = "Mean predicted growth trajectories",
               x = "Time Point",
               y = "Mean Predicted Value",
               caption = "Note: Points to the right of the dashed vertical line represent time points in the future, not accounted for in the Bayesian GCM") +
          scale_x_continuous(breaks = time_values_new$time_values, labels = round(time_values_new$time_values,3)) +
          theme_minimal() 
        
        output$gcm_plot <- renderPlotly({ggplotly(gcm_plot) })
        
        predicted_values <- list()
        for (i in 1:nrow(df)) {
          y_i <- int[i] + (slp[i] * time_values_new$time_values)
          if (obs_id_var == "None") {
            obs_label <- i  
          } else {
            obs_label <- df[[obs_id_var]][i] }
          predicted_values[[i]] <- data.frame(time = time_values_new$time_values, y = y_i, obs = obs_label) }
        
        res_df_all <- do.call(rbind, predicted_values)
 
        observeEvent(input$sample_checkbox, { 
          if (input$sample_checkbox) {
            sample_size <- input$sample_size
            if (sample_size > 0) {
              sampled_observations <- sample(unique(res_df_all$obs), min(sample_size, length(unique(res_df_all$obs))))
              res_df_all <- subset(res_df_all, obs %in% sampled_observations) } }
          
          gcm_plot_all <- ggplot(res_df_all, aes(x = time, y = y, group = as.factor(obs), color = as.factor(obs))) +
            geom_line(size = 1) +
            geom_point(size = 2) +
            geom_vline(xintercept = time_values_new$time_values[length(time_values_new$time_values) - time_values_new$future_points], 
                       linetype = "dashed", color = "black", alpha = .7) +
            labs(title = "Predicted growth trajectories across observations",
                 x = "Time Point",
                 y = "Predicted Value",
                 color = "Observation") +
            scale_x_continuous(breaks = time_values_new$time_values, 
                               labels = round(time_values_new$time_values,3)) +
            scale_color_viridis_d() + 
            theme_minimal() +
            theme(legend.position = "right",
                  legend.text = element_text(size = 8),
                  legend.title = element_text(size = 10),
                  axis.title = element_text(size = 10),
                  axis.text = element_text(size = 8))
          
          output$gcm_plot_all <- renderPlotly({ggplotly(gcm_plot_all)  })
        }) }) })
  })
  
  # BAYESIAN STACKING
  BMA_vars_default <- NULL
  stacking_vars <- reactiveValues(selected_vars = list())
  output$model_selectors <- renderUI({
    if (is.null(input$data_file) || is.null(input$data_file$datapath)) {
      return(div(style = "color: #9B0000;", "Please select a data file under 'Data and Plots' first."))}
    
    
    df <- read.csv(input$data_file$datapath)
    num_models <- input$num_models
    choices <- colnames(df)
    
    if (length(choices) == 0) {return(NULL) }
    
    model_selectors <- lapply(1:input$num_models, function(i) { 
      pickerInput(paste0("model_", i), paste("Select variables for Model", i), 
                            choices = colnames(df), multiple = TRUE, 
                  options = pickerOptions(liveSearch = T, noneSelectedText = "Select variables"))
      })
    
    if(num_models > ncol(df)){
      showNotification("Please choose fewer models than available variables.", 
                       type = "error")
      return()}
    
    do.call(tagList, model_selectors) })
  
  output$selected_values_ui <- renderUI({
    if (is.null(input$data_file) || is.null(input$data_file$datapath)) {
      return(NULL) }
    
    df <- read.csv(input$data_file$datapath)
    num_models <- input$num_models
    choices <- colnames(df)  
    
    if (length(choices) == 0) {return(HTML("No data available.")) }

    selected_vars <- lapply(1:num_models, function(i) {input[[paste0("model_", i)]] })
    

    selected_vars_text <- lapply(1:num_models, function(i) {
      tags$li(paste("Model", i, "selected variables:", paste(selected_vars[[i]], collapse = ", ")), tags$br(), tags$br())  # List item for each model
    })

    output_html <- tags$div(
      tags$h3("Selected variables by model"),  
      tags$ul(selected_vars_text)  )
    
    return(output_html) })
  
  reactive_loo_list <- reactiveValues(loo_list = list()) #growth
  reactive_model_list <- reactiveValues(model_list = list())
  reactive_yPredstacking_means <- reactiveValues()
  
  reactive_loo_list_int <- reactiveValues(loo_list_int = list()) #int
  reactive_model_list_int <- reactiveValues(model_list_int = list())
  reactive_yPredstacking_means_int <- reactiveValues()
  
  BMA_vars_default <- reactiveValues()
  
  observeEvent(input$run_models, {
    
    if (is.null(isolate(reactive_values$y_growth))) {
      showNotification("Please run the Bayesian GCM before proceeding to model ensembling.",
                       type = "error")
      return() }
    
    req(input$data_file, cancelOutput = TRUE)

    df <- read.csv(input$data_file$datapath)
    model_type <- input$model_type
    num_models <- input$num_models
    
    cat("Model type:", model_type, "\n")
    
    df$growth_rate <- isolate(reactive_values$y_growth)
    df$intercept <- isolate(reactive_values$y_int)
    
    var_list <- list() 
    withProgress(message = "Running models... (this may take several minutes)", value = 0, {
      for (i in 1:num_models) {
        predictors <- input[[paste0("model_", i)]]  
        
        if (length(predictors) == 0) {
          showNotification("Please select at least one predictor for each linear member model.", type = "error")
          return()
           }
        
        formula_text <- paste("growth_rate ~", paste(predictors, collapse = " + "))
        formula <- as.formula(formula_text)
        
        cat("Fitting model", i, "with formula:", formula_text, "\n")
        reactive_model_list$model_list[[i]] <- stan_glm(formula, data = df, family = gaussian())
        print(summary(reactive_model_list$model_list[[i]]))
        
        formula_text_int <- paste("intercept ~", paste(predictors, collapse = " + "))
        formula_int <- as.formula(formula_text_int)
        reactive_model_list_int$model_list_int[[i]] <- stan_glm(formula_int, data = df, family = gaussian())
        cat("Calculating loo for model", i)  
        loo_result <- loo(reactive_model_list$model_list[[i]]) #error if k_threshold and model needs to be refit
        loo_result_int <- loo(reactive_model_list_int$model_list_int[[i]])
        
        reactive_loo_list$loo_list[[i]] <- loo_result
        reactive_loo_list_int$loo_list_int[[i]] <- loo_result_int

        cat("LOO result for growth model", i, ":\n")
        print(loo_result)
        
        cat("LOO result for intercept model", i, ":\n")
        print(loo_result_int)
        
        
        incProgress(1 / num_models, detail = paste("Model", i, "of", num_models))
      } 
      
      for (i in seq_along(reactive_model_list$model_list)){
        vars <- colnames(reactive_model_list$model_list[[i]]$model)[2:ncol(reactive_model_list$model_list[[i]]$model)]
        var_list[[i]] <- vars
      }
      
      BMA_vars_default$BMA_vars_default <- unlist(var_list) #to get list of variables to be default selection for BMA
      print(BMA_vars_default$BMA_vars_default)
      
      })
    
    output$model_stack_summaries <- renderPrint({
      
      if (input$model_type == "Growth Rate"){
        for (i in seq_along(reactive_model_list$model_list)) {
          cat(sprintf("Model summary for model %d:\n", i))
          if (!is.null(reactive_model_list$model_list[[i]])) {
            print(summary(reactive_model_list$model_list[[i]]))
          } else {
            cat("No model fitted for this selection.\n")}
          cat("\n")  }} else{
            for (i in seq_along(reactive_model_list_int$model_list_int)) {
              cat(sprintf("Model summary for model %d:\n", i))
              if (!is.null(reactive_model_list_int$model_list_int[[i]])) {
                print(summary(reactive_model_list_int$model_list_int[[i]]))
              } else {
                cat("No model fitted for this selection.\n")}
              cat("\n")  } }
      })
    
    output$loo_stack_summaries <- renderPrint({
      if (input$model_type == "Growth Rate"){
        for (i in seq_along(reactive_loo_list$loo_list)) {
          cat(sprintf("LOO summary for model %d:\n", i))
          if (!is.null(reactive_loo_list$loo_list[[i]])) {
            print(reactive_loo_list$loo_list[[i]])
          } else {
            cat("No model fitted for this selection.\n")
          }
          cat("\n")  }
      } else{
        for (i in seq_along(reactive_loo_list_int$loo_list_int)) {
          cat(sprintf("LOO summary for model %d:\n", i))
          if (!is.null(reactive_loo_list_int$loo_list_int[[i]])) {
            print(reactive_loo_list_int$loo_list_int[[i]])
          } else {
            cat("No model fitted for this selection.\n")
          }
          cat("\n")  }
      } })
   
  })
  
  
  observeEvent(BMA_vars_default$BMA_vars_default, {
    if (is.null(BMA_vars_default$BMA_vars_default) || length(BMA_vars_default$BMA_vars_default) == 0) {
      updatePickerInput(
        session = session,
        inputId = "predictor_variables_BMA",
        choices = colnames(df),
        selected = colnames(df)  )
    } else {
      updatePickerInput(
        session = session,
        inputId = "predictor_variables_BMA",
        choices = colnames(df),
        selected = BMA_vars_default$BMA_vars_default   )
    }
  })
  
  #keeping model and LOO fitting in same loop bc if the loo() function needs to refit the model it's a lot more complicated to do that when the model was originally fit in another code chunk
  reactive_vals <- reactiveValues(
    kld_list = NULL,
    stacking_kld = NULL,
    kld_list_int = NULL,
    stacking_kld_int = NULL,
    
    ypredStacking = NULL,
    ypredStacking_int = NULL
  )
  
  observeEvent(input$compute_weights, { 
    if (is.null(isolate(reactive_values$y_growth))) {
      showNotification("Please run the Bayesian GCM before proceeding to model ensembling.",
                       type = "error")
      return() }
    
    if (is.null(reactive_loo_list$loo_list)) {
      showNotification("Please run the Bayesian stacking member models before proceeding to stacking.",
                       type = "error")
      return() }
    
    req(input$data_file, cancelOutput = TRUE)
    
    
    loo_list <- reactive_loo_list$loo_list 
    model_list <- reactive_model_list$model_list
    
    loo_list_int <- reactive_loo_list_int$loo_list_int 
    model_list_int <- reactive_model_list_int$model_list_int
    
    if (length(loo_list) == 0) {
      showNotification("Please run ensemble models first.", type = "error")
      return(NULL) }
    
    df <- read.csv(input$data_file$datapath)
    model_type <- input$model_type
    num_models <- input$num_models
    
    df$growth_rate <- isolate(reactive_values$y_growth)
    df$intercept <- isolate(reactive_values$y_int)
    
    # GROWTH
    if (input$evaluation_metric == "ELPD"){
      stacking_weights <- loo_model_weights(loo_list, method = "stacking")
    } else if (input$evaluation_metric == "PBMA"){
      stacking_weights <- loo_model_weights(loo_list, method = "pseudobma", BB = F)
    } else if (input$evaluation_metric == "PBMA+"){
      stacking_weights <- loo_model_weights(loo_list, method = "pseudobma", BB = T)
    } 
    
    draws <- nrow(as.matrix(model_list[[1]]))
    #print(paste("n_draws:", n_draws))
    ypredStacking <- matrix(NA, nrow = draws, ncol = nobs(model_list[[1]]))
    #print(paste("ypredStacking dimensions:", dim(ypredStacking)))
    #print(head(ypredStacking))
    
    withProgress(message = 'Running stacking procedure...', value = 0, { 
      for (d in 1:draws) {
        k <- sample(1:length(stacking_weights), size = 1, prob = stacking_weights)
        #print(d) #debugging
        ypredStacking[d, ] <- posterior_predict(model_list[[k]], draws = 1)
        
        incProgress(1 / draws, detail = paste("Draw", d, "of", draws))
      } })
    
    ypredStacking_means <- colMeans(ypredStacking)
    reactive_yPredstacking_means$ypredStacking_means <- ypredStacking_means
    reactive_vals$ypredStacking <- ypredStacking
    #print(paste("ypredStacking means:", ypredStacking_means))
    
    # INTERCEPTS
    if (input$evaluation_metric == "ELPD"){
      stacking_weights_int <- loo_model_weights(loo_list_int, method = "stacking")
    } else if (input$evaluation_metric == "PBMA"){
      stacking_weights_int <- loo_model_weights(loo_list_int, method = "pseudobma", BB = F)
    } else if (input$evaluation_metric == "PBMA+"){
      stacking_weights_int <- loo_model_weights(loo_list_int, method = "pseudobma", BB = T)
    } 
    
    draws <- nrow(as.matrix(model_list_int[[1]]))
    #print(paste("n_draws:", n_draws))
    ypredStacking_int <- matrix(NA, nrow = draws, ncol = nobs(model_list_int[[1]]))
    #print(paste("ypredStacking dimensions:", dim(ypredStacking)))
    #print(head(ypredStacking))
    
    withProgress(message = 'Running stacking procedure...', value = 0, { 
      for (d in 1:draws) {
        k <- sample(1:length(stacking_weights_int), size = 1, prob = stacking_weights_int)
        #print(d) #debugging
        ypredStacking_int[d, ] <- posterior_predict(model_list_int[[k]], draws = 1)
        
        incProgress(1 / draws, detail = paste("Draw", d, "of", draws)) } })
    
    ypredStacking_means_int <- colMeans(ypredStacking_int)
    reactive_yPredstacking_means_int$ypredStacking_means_int <- ypredStacking_means_int
    print(reactive_yPredstacking_means_int$ypredStacking_means_int)
    reactive_vals$ypredStacking_int <- ypredStacking_int
    
    output$stacking_weights_results <- renderUI({

      if (input$model_type == "Growth Rate") {
        weight_method <- paste0("Weighting method: ", input$evaluation_metric)
 
        output_html <- tags$div(
          tags$h3("Stacking Weights (Growth Rate 
                  Models)"), 
          tags$h4(class = "custom-header", weight_method),  
          tags$ul(
            lapply(1:length(model_list), function(i) {
              tags$li(
                paste("Weight for Model", i, ":", round(stacking_weights[[i]], 5)),
                tags$br() )})),
          HTML(paste(rep("<br>", 2), collapse = ""))  )
        return(output_html)
      } else {
        weight_method <- paste0("Weighting method: ", input$evaluation_metric)
        output_html <- tags$div(
          tags$h3("Stacking Weights Results (Intercept Models)"),  
          tags$h4(class = "custom-header", weight_method),  
          tags$ul(
            lapply(1:length(model_list_int), function(i) {
              tags$li(paste("Weight for Model", i, ":", round(stacking_weights_int[[i]], 5)),
                      tags$br())  })),
          HTML(paste(rep("<br>", 2), collapse = "")) )
        return(output_html) }
    })

    observe({
      kld_list <- vector("list", length(model_list))
      for (i in seq_along(model_list)) {
        kld <- KLD(model_list[[i]]$fitted.values, df$growth_rate)$sum.KLD.px.py
        kld_list[[i]] <- kld
        cat(paste("KLD for model", i, ":"))
        print(round(kld, 5))
      }
      # Update the reactive value
      reactive_vals$kld_list <- kld_list
      print(reactive_vals$kld_list)
    })
    
    observe({
      stacking_kld <- KLD(ypredStacking_means, df$growth_rate)$sum.KLD.px.py
      reactive_vals$stacking_kld <- stacking_kld
    })
    
    observe({
      kld_list_int <- vector("list", length(model_list_int))
      for (i in seq_along(model_list_int)) {
        kld_int <- KLD(model_list_int[[i]]$fitted.values, df$intercept)$sum.KLD.px.py
        kld_list_int[[i]] <- kld_int
        cat(paste("KLD for model (intercept)", i, ":"))
        print(round(kld_int, 5))
      }

            reactive_vals$kld_list_int <- kld_list_int
      print(reactive_vals$kld_list_int)
    })
    
    observe({
      stacking_kld_int <- KLD(ypredStacking_means_int, df$intercept)$sum.KLD.px.py
      reactive_vals$stacking_kld_int <- stacking_kld_int
    })
    
    
    output$stacking_results <- renderUI({

      if (input$model_type == "Growth Rate") {
        stacking_kld <- reactive_vals$stacking_kld
        kld_list <- reactive_vals$kld_list
        output_html <- tags$div(
          tags$h3("KLD Results (Growth Rate Models)"), 
          tags$h4(class = "custom-header", paste("KLD for stacked model: ", round(stacking_kld, 5))), 
          tags$ul(
            lapply(1:length(kld_list), function(i) {
              tags$li(paste("KLD for Model", i, ":", round(kld_list[[i]], 5)),
                      tags$br() )  })),
          HTML(paste(rep("<br>", 2), collapse = "")) )
        return(output_html) } 
      else {
        stacking_kld_int <- reactive_vals$stacking_kld_int
        kld_list_int <- reactive_vals$kld_list_int
        output_html <- tags$div(
          tags$h3("KLD Results (Intercept Models)"),  
          tags$h4(class = "custom-header", paste("KLD for stacked model: ", round(stacking_kld_int, 5))),  
          tags$ul(
            lapply(1:length(kld_list_int), function(i) {
              tags$li(paste("KLD for Model", i, ":", round(kld_list_int[[i]], 5)),
                      tags$br() )  })),
          HTML(paste(rep("<br>", 2), collapse = "")) )
        return(output_html) }
    })
    
    
    output$download_stacking_results <- downloadHandler(
      filename = function() {
        paste("stacking_results_", Sys.Date(), ".RData", sep = "")
      },
      content = function(file) {
        stacking_kld <- reactive_vals$stacking_kld
        stacking_kld_int <- reactive_vals$stacking_kld_int
        kld_list <- reactive_vals$kld_list
        kld_list_int <- reactive_vals$kld_list_int

        save(
          model_list, model_list_int,
          loo_list, loo_list_int,
          stacking_weights, stacking_weights_int,
          stacking_kld, stacking_kld_int,
          kld_list, kld_list_int,
          ypredStacking, ypredStacking_int,
          ypredStacking_means, ypredStacking_means_int,
          file = file)} )
  })
    
  stack_data <- reactiveValues(stack_data = NULL)
  
  observeEvent(input$generate_plots, { 
    
    if (is.null(isolate(reactive_values$y_growth))) {
      showNotification("Please run the Bayesian GCM before proceeding to model ensembling.",
                       type = "error")
      return() }
    
    if (is.null(reactive_model_list$model_list)) {
      showNotification("Please run the Bayesian stacking before proceeding to plot generation.",
                       type = "error")
      return() }
    
    if (is.null(reactive_yPredstacking_means_int$ypredStacking_means_int)){
      showNotification("Please compute the stacked predictions before generating trajectory plots.",
                       type = "error")
      return()
    }
    
    if (input$future_timepoints_stack < 1){
      showNotification("Please select at least 1 observation for visualization.", type = "error")
      return() }
    
    req(input$data_file,  cancelOutput = TRUE)
    
    
      df <- read.csv(input$data_file$datapath)
      time_indicator <- input$time_indicator
      time_distance <- input$time_distance
      outcome_var <- reactive_vals_outcome$outcome_var 
      model_list <- reactive_model_list$model_list
      model_list_int <- reactive_model_list_int$model_list_int
      #reactive_fit_list_est
      outcome_var_df <- df[, input$outcome_variable, drop = FALSE]
      #print(outcome_var_df)
      #dim(outcome_var_df)
      result_df <- data.frame(time = numeric(), y = numeric(), model = character())
      
      calculate_time_values <- function(time_indicator, time_distance, outcome_var_length, future_points) {
        if (time_indicator == "Linear") {
          time_values <- seq(0, by = time_distance, length.out = outcome_var_length + future_points)
        } else if (time_indicator == "Quadratic") {
          time_values <- (seq(0, length.out = outcome_var_length + future_points) * time_distance)^2
        } else if (time_indicator == "Latent Basis") {
          latent_time <- reactive_fit_list_est$fit_est_list[[1]][(outcome_var_length + 1):(outcome_var_length * 2)]
          #latent_time <- latent_reactive_time() 
          future_time <- seq(length(latent_time), length.out = future_points) * time_distance
          time_values <- c(latent_time, future_time)
        } else {
          stop("Invalid time_indicator. Please choose 'Linear', 'Quadratic', or 'Latent Basis'.") 
        }
        
        return(list(time_values = time_values, future_points = future_points)) }
      
      time_values_new <- calculate_time_values(time_indicator = time_indicator, time_distance = time_distance, 
                                               outcome_var_length = length(outcome_var), future_points = input$future_timepoints_stack) 
      
      stacked_pred <- array(NA, dim = c(4000, nrow(df), length(time_values_new$time_values)))
      for (i in 1:length(time_values_new$time_values)) {
        stacked_pred[, , i] <- reactive_vals$ypredStacking * time_values_new$time_values[i] + reactive_vals$ypredStacking_int }
      
      mean_predictions <- list()
      lower <- list()
      upper <- list()
      for (i in 1:length(time_values_new$time_values)) {
        mean_predictions[[i]] <- apply(stacked_pred[, , i], 2, mean)
        lower[[i]] <- apply(stacked_pred[, , i], 2, quantile, probs = 0.025)
        upper[[i]] <- apply(stacked_pred[, , i], 2, quantile, probs = 0.975) 
      } 
      
      overall_mean <- numeric(length(time_values_new$time_values))
      overall_lower2 <- numeric(length(time_values_new$time_values))
      overall_upper97 <- numeric(length(time_values_new$time_values))
      overall_lower12 <- numeric(length(time_values_new$time_values))
      overall_upper88 <- numeric(length(time_values_new$time_values))
      overall_lower25 <- numeric(length(time_values_new$time_values))
      overall_upper75 <- numeric(length(time_values_new$time_values))
      
      for (i in 1:length(time_values_new$time_values)) {
        combined_values <- as.vector(stacked_pred[, , i])
        overall_mean[i] <- mean(combined_values)
        overall_lower2[i] <- quantile(combined_values, probs = 0.025)
        overall_upper97[i] <- quantile(combined_values, probs = 0.975)
        
        overall_lower12[i] <- quantile(combined_values, probs = 0.12)
        overall_upper88[i] <- quantile(combined_values, probs = 0.88)
        
        overall_lower25[i] <- quantile(combined_values, probs = 0.25)
        overall_upper75[i] <- quantile(combined_values, probs = 0.75)}
      
      y_loo_df <- data.frame(
        time = time_values_new$time_values,
        y = overall_mean,
        ymin2 = overall_lower2,
        ymax97 = overall_upper97,
        ymin12 = overall_lower12,
        ymax88 = overall_upper88,
        ymin25 = overall_lower25,
        ymax75 = overall_upper75,
        Model = "Stacked Predictions")

      
      #slp_loo <- mean(reactive_yPredstacking_means$ypredStacking_means)
      slp_observed <- mean(isolate(reactive_values$y_growth))
      
      #int_loo <- mean(reactive_yPredstacking_means_int$ypredStacking_means_int)
      int_observed <- mean(isolate(reactive_values$y_int))

      #y_slp_loo <- int_loo + slp_loo * time_values_new$time_values
      #y_slp_observed <- int_observed + slp_observed * time_values_new$time_values

      #result_df <- rbind(result_df, data.frame(time = time_values_new$time_values , y = y_slp_observed, Model = "Baseline (GCM)"))
      #y_loo_df <-  data.frame(time = time_values_new$time_values , y = y_slp_loo, Model = "Stacked Predictions")
      
      empirical_df <- data.frame(y = colMeans(outcome_var_df), time = time_values_new$time_values[1:length(outcome_var)], Model = "Empirical")
      xint <- time_values_new$time_values[length(time_values_new$time_values) - time_values_new$future_points]
      
      slp_list <- list()
      for (i in 1:length(model_list)) {
        slp_list[[i]] <- mean(model_list[[i]]$fitted.values)
        print(slp_list[[i]])
      }
      names(slp_list) <- paste0("slp_", 1:length(model_list)) 
      
      int_list <- list()
      for (i in 1:length(model_list_int)) {
        int_list[[i]] <- mean(model_list_int[[i]]$fitted.values)
        print(int_list[[i]])
      }
      names(int_list) <- paste0("int_", 1:length(model_list)) 
      
      stack_df <- data.frame(time = numeric(), y = numeric(), model = character())
      
      for (i in 1:length(model_list)) {
        int <- int_list[[i]]
        slp <- slp_list[[i]]
        model_name <- paste0("Model ", i)
        
        y <- int + (slp * time_values_new$time_values)
        stack_df <- rbind(stack_df, data.frame(time = time_values_new$time_values, y = y, Model = model_name))  }
      
      model_colors <- c("Stacked Predictions" = "#238A8DFF","Empirical" = "gray59", 
                        setNames(viridis::viridis(length(model_list)), paste0("Model ", 1:length(model_list))))
      
      stack_data$stack_data <- y_loo_df
      
      output$overall_stack_plot <- renderPlotly({
        
        ggplotly(ggplot(empirical_df, aes(x = time, y = y, color = Model)) +
          geom_vline(xintercept = time_values_new$time_values[length(time_values_new$time_values) - time_values_new$future_points],
                     linetype = "dashed", color = "black", alpha = .7) +
          geom_line(size = 2) +
          geom_point(size = 4) +
            
          geom_line(data = stack_df, size = 2, aes(color = Model)) +
          geom_point(data = stack_df, size = 4, aes(color = Model)) +
            
          geom_line(data = y_loo_df, size = 2, aes(color = "Stacked Predictions")) +
          geom_point(data = y_loo_df, size = 4, aes(color = "Stacked Predictions")) +
            
            geom_errorbar(data = subset(y_loo_df, time > time_values_new$time_values[length(outcome_var)]), inherit.aes = F,
                          aes(x = time+.1, ymin = ymin2, ymax = ymax97),
                          width = 0.1, linewidth = .2, color = "#33638DFF", alpha = 1) +
            geom_errorbar(data = subset(y_loo_df, time > time_values_new$time_values[length(outcome_var)]),  inherit.aes = F,
                          aes(x = time, ymin = ymin12, ymax = ymax88),
                          width = 0.1, linewidth = .2, color = "#238A8DFF", alpha = 1) +
            geom_errorbar(data = subset(y_loo_df, time > time_values_new$time_values[length(outcome_var)]), inherit.aes = F,
                          aes(x = time-.1, ymin = ymin25, ymax = ymax75),
                          width = 0.1, linewidth = .2, color = "#20A387FF", alpha = 1) +
            
          # geom_line(data = result_df, size = 2, aes(color = "Baseline (GCM)")) +
          # geom_point(data = result_df, size = 4, aes(color = "Baseline (GCM)")) +
            
          labs(title = "Mean Predicted Growth Trajectories for Bayesian Stacking",
               x = "Time Point",
               y = "Predicted Value", linetype = NULL) +
          scale_x_continuous(breaks = time_values_new$time_values,
                             labels = round(time_values_new$time_values,3)) +
          scale_color_manual(values = model_colors, name = "Model") + 
          theme_minimal() +
          guides(color = guide_legend(override.aes = list(size = 2))) + 
            theme(
              axis.title.x = element_text(size = 12, margin = margin(t = 20)),     
              axis.title.y = element_text(size = 12, margin = margin(r = 20)),   
              axis.text.x = element_text(size = 10),      
              axis.text.y = element_text(size = 10),   
              legend.title = element_text(size = 12),    
              legend.text = element_text(size = 10),
              plot.title = element_text(size = 14),
              panel.grid.minor.x = element_blank())) 
      })
      
      output$caption_stack <- renderUI(tags$p("The above plot provides error bars to denote 50%, 75%, and 95% Bayesian prediction intervals for the stacked estimates at future time points.", 
                                            style = "text-align: left; font-size: 12px; color: gray; margin-top: 20px;"))
     })
    
  # BMA 
  
  output$BMA_stop <- renderUI({
    if (is.null(input$data_file) || is.null(input$data_file$datapath)) {
      return(div(style = "color: #9B0000;", "Please select a data file under 'Data and Plots' first."))}
  })
  
  BMA_values <- reactiveValues(data = NULL, model = NULL, data_int = NULL, model_int = NULL, growth_KLD = NULL, int_KLD = NULL)
  BMA_dat <- reactiveValues(BMA_dat = NULL)
    
  observeEvent(input$run_BMA, {
    
    if (is.null(isolate(reactive_values$y_growth))) {
      showNotification("Please run the Bayesian GCM before proceeding to model ensembling.",
                       type = "error")
      return() }
    
    req(input$data_file, cancelOutput = TRUE)
 
    df <- read.csv(input$data_file$datapath)
    predictor_vars <- input$predictor_variables_BMA
    g_BMA <- input$g_BMA
    mprior_BMA <- input$mprior_BMA
    mprior_size_BMA <- input$mprior_size_BMA
    
    non_numeric_vars <- predictor_vars[sapply(df[, predictor_vars], function(x) !is.numeric(x))]
    
    if (length(non_numeric_vars) > 0) {
      showNotification(paste("The following variables are non-numeric and cannot be used:", 
                             paste(non_numeric_vars, collapse = ", ")),
                       type = "error")
      return() }
  
      df$growth_rate <- isolate(reactive_values$y_growth)
      df$intercept <- isolate(reactive_values$y_int) 
  
    # GROWTH
    BMA_data <- df[, c("growth_rate", predictor_vars)]
    combos <- findLinearCombos(BMA_data)
    
    
    output$removed_vars_msg <- renderUI({
      if (!is.null(combos$remove) && length(combos$remove) > 0) {
        removed_vars <- colnames(df[, combos$remove, drop = FALSE])
        output_removed <- tags$div(
          tags$h4(class = "custom-header", "The following variables were removed due to linear dependencies/combinations:"),
          tags$ul(
            lapply(removed_vars, function(var) {
              tags$li(var, tags$br())  
            })
          ),
          HTML(paste(rep("<br>", 2), collapse = ""))
        )
      } else {
        output_removed <- tags$div(
          tags$h4(class = "custom-header", "No variables were removed due to linear dependencies/combinations.")
        )
      }
      return(output_removed)
    })
    
    if (!is.null(combos$remove) && length(combos$remove) > 0) {
      BMA_data <- BMA_data[, -combos$remove] }
 
    BMA_model <- if(mprior_BMA == "fixed"){
      bms(BMA_data, g = g_BMA, mprior = mprior_BMA, mprior.size = mprior_size_BMA)
    } else{
      bms(BMA_data, g = g_BMA, mprior = mprior_BMA) }
    BMA_results <- coef(BMA_model) 
    
    # KLD
    growth_KLD <- KLD(predict(BMA_model), BMA_data$growth_rate)$sum.KLD.px.py
    
    # INTERCEPT 
    BMA_data_int <- df[, c("intercept", predictor_vars)]
    combos <- findLinearCombos(BMA_data_int)
    if (!is.null(combos$remove) && length(combos$remove) > 0) {
      BMA_data_int <- BMA_data_int[, -combos$remove] }
    
    BMA_model_int <- if(mprior_BMA == "fixed"){
      bms(BMA_data_int, g = g_BMA, mprior = mprior_BMA, mprior.size = mprior_size_BMA)
    } else{
      bms(BMA_data_int, g = g_BMA, mprior = mprior_BMA) }
    
    BMA_results_int <- coef(BMA_model_int)
    int_KLD <- KLD(predict(BMA_model_int), BMA_data_int$intercept)$sum.KLD.px.py
    
    output$BMA_summary <- renderPrint({
      if (input$outcome_variable_BMA == "Growth Rate"){
        print(round(BMA_results,3))
        print(summary(BMA_model, digits = 3))  
        paste("KLD: ", round(growth_KLD, 3))
    } else{
      print(round(BMA_results_int,3))
      print(summary(BMA_model_int, digits = 3))   
      paste( "KLD: ",round(int_KLD, 3))
      } })
      
    output$download_BMA <- downloadHandler(
      filename = function() {
        paste("BMA_results_", Sys.Date(), ".RData", sep = "")},
      content = function(file) {
        save(
          BMA_model, BMA_model_int, 
          BMA_data, BMA_data_int,
          file = file) }) 
    
    output$BMA_margdens <- renderPlot({
      BMA_results_df <- data.frame(Variable = rownames(BMA_results), BMA_results)
      sorted_BMA_results <- BMA_results_df[order(-BMA_results_df$PIP), ]
      par(mfrow = c(3, 3)) 
      for (i in 1:min(9, nrow(sorted_BMA_results))) {
        var_name <- sorted_BMA_results$Variable[i]
        density(BMA_model, which(BMA_model$reg.names == var_name)) } }) #this is buggy if more than 9 predictors?
    
    output$caption_margdens <- renderUI(tags$p("The above plots visualizes the marginal density of the top 9 BMA predictors based on the posterior inclusion probability.", 
                                                 style = "text-align: left; font-size: 12px; color: gray; margin-top: 10px; margin-bottom: 30px"))
    
  
    output$BMA_modelsize <- renderPlot({plotModelsize(BMA_model) })
    
    output$caption_modelsize <- renderUI(tags$p("The above plot shows the posterior and prior model size distributions for the BMA analysis.", 
                                               style = "text-align: left; font-size: 12px; color: gray; margin-top: 10px;"))
    
    output$download_BMA_margdens <- downloadHandler(
      filename = function() {
        paste("BMA_margdens_", Sys.Date(), ".png", sep = "")},
      content = function(file) {
        png(file, width = 1500)
        BMA_results_df <- data.frame(Variable = rownames(BMA_results), BMA_results)
        sorted_BMA_results <- BMA_results_df[order(-BMA_results_df$PIP), ]
        par(mfrow = c(3, 3)) 
        for (i in 1:min(12, nrow(sorted_BMA_results))) {
          var_name <- sorted_BMA_results$Variable[i]
          density(BMA_model, which(BMA_model$reg.names == var_name)) }
        dev.off() }) #this no work 

    output$download_BMA_modelsize <- downloadHandler(
      filename = function() {
        paste("BMA_modelsize_", Sys.Date(), ".png", sep = "") },
      content = function(file) {
        png(file)
        plotModelsize(BMA_model)
        dev.off() })
    
    BMA_values$data <- BMA_data
    BMA_values$model <- BMA_model
    BMA_values$data_int <- BMA_data_int
    BMA_values$model_int <- BMA_model_int
    BMA_values$growth_KLD <- growth_KLD
    BMA_values$int_KLD <- int_KLD
    })
  
  observeEvent(input$generate_plots_BMA,{
    
    if (is.null(BMA_values$int_KLD)){
      showNotification("Please run BMA before generating trajectory plots.", type = "error")
    }
    
    if (input$future_timepoints_BMA < 1){
      showNotification("Please select at least 1 observation for visualization.", type = "error")
      return() }

 
    BMA_data <- BMA_values$data
    BMA_model <- BMA_values$model
    BMA_data_int <- BMA_values$data_int
    BMA_model_int <- BMA_values$model_int
    
    req(input$data_file, BMA_data, BMA_model, BMA_data_int, BMA_model_int, cancelOutput = TRUE)

    df <- read.csv(input$data_file$datapath)
    time_indicator <- input$time_indicator
    time_distance <- input$time_distance
    outcome_var <- input$outcome_variable
    predictor_vars <- input$predictor_variables_BMA
    outcome_var_df <- df[, input$outcome_variable, drop = FALSE]
    future_timepoints_BMA <- input$future_timepoints_BMA
    
    output$caption_BMA <- renderUI(tags$p("The above plot provides error bars to denote 50%, 75%, and 95% Bayesian prediction intervals for the BMA estimates at future time points.", 
                                          style = "text-align: left; font-size: 12px; color: gray; margin-top: 20px;"))
    
    calculate_time_values <- function(time_indicator, time_distance, outcome_var_length, future_points) {
      if (time_indicator == "Linear") {
        time_values <- seq(0, by = time_distance, length.out = outcome_var_length + future_points)
      } else if (time_indicator == "Quadratic") {
        time_values <- (seq(0, length.out = outcome_var_length + future_points) * time_distance)^2
      } else if (time_indicator == "Latent Basis") {
        latent_time <- reactive_fit_list_est$fit_est_list[[1]][(outcome_var_length + 1):(outcome_var_length * 2)]
        #latent_time <- latent_reactive_time() 
        future_time <- seq(length(latent_time), length.out = future_points) * time_distance
        time_values <- c(latent_time, future_time)
      } else {
        stop("Invalid time_indicator. Please choose 'Linear', 'Quadratic', or 'Latent Basis'.") #don't really need this for the app ig
      }
      
      return(list(time_values = time_values, future_points = future_points)) }
    
    time_values_new <- calculate_time_values(time_indicator = time_indicator, time_distance = time_distance, 
                                             outcome_var_length = length(outcome_var), future_points = future_timepoints_BMA ) 
    
    BMA_dens <- pred.density(BMA_model, newdata = BMA_data[,2:length(BMA_data)])
    test <- data.frame(BMA_dens$fit, BMA_dens$std.err, quantile(BMA_dens, c(.025, .975, .12, .88, .25, .75)))
    colnames(test) <- c("Mean Growth", "SE Growth", "2.5% Growth", "97.5% Growth", "12% Growth", "88% Growth", "25% Growth", "75% Growth")
    BMA_dens_int <- pred.density(BMA_model_int, newdata = BMA_data_int[,2:length(BMA_data_int)])
    test_int <- data.frame(BMA_dens_int$fit, BMA_dens_int$std.err, quantile(BMA_dens_int, c(.025, .975, .12, .88, .25, .75)))
    colnames(test_int) <- c("Mean Int", "SE Int", "2.5% Int", "97.5% Int", "12% Int", "88% Int", "25% Int", "75% Int")
    test2 <- cbind(test, test_int)
    
    BMA_df <- data.frame(y = mean(test2$`Mean Int`) + time_values_new$time_values*mean(test2$`Mean Growth`), 
                                   ymin2 = mean(test2$`2.5% Int`) + mean(test2$`2.5% Growth`)*time_values_new$time_value,
                                   ymax97 = mean(test2$`97.5% Int`) + mean(test2$`97.5% Growth`)*time_values_new$time_value,
                                   ymin12 = mean(test2$`12% Int`) + mean(test2$`12% Growth`)*time_values_new$time_value,
                                   ymax88 = mean(test2$`88% Int`) + mean(test2$`88% Growth`)*time_values_new$time_value,
                                   ymin25 = mean(test2$`25% Int`) + mean(test2$`25% Growth`)*time_values_new$time_value,
                                   ymax75 = mean(test2$`75% Int`) + mean(test2$`75% Growth`)*time_values_new$time_value,
                                   time = time_values_new$time_value, Model ="BMA")

    colors <- c("Empirical" = "gray59", "BMA" = "#238A8DFF", "50% PI (BMA)" =  "#20A387FF", "75% PI (BMA)" = "#238A8DFF", "95% PI (BMA)" = "#33638DFF")
    
    empirical_df <- data.frame(time = time_values_new$time_values[1:length(outcome_var)], y = colMeans(outcome_var_df), Model = "Empirical")
    
    empirical_df$Model <- factor(empirical_df$Model,  levels = c("Empirical", "BMA"))
    BMA_df$Model <- factor(BMA_df$Model, levels = c("Empirical", "BMA"))
    
    # sometime we get this error: Warning: Error in seq.default: 'from' must be a finite number
    BMA_plot <- ggplot(empirical_df, aes(x = time, y = y, color = Model)) +
      geom_vline(xintercept = time_values_new$time_values[length(time_values_new$time_values) - time_values_new$future_points], linetype = "dashed", alpha = 0.5) +
      geom_point(size = 3) +
      geom_line(linewidth = 1.5) +  
      geom_point(data = BMA_df, aes(x = time, y = y, color = Model), size = 4) +
      geom_line(data = BMA_df, aes(x = time, y = y, color = Model), linewidth = 2) +
      geom_errorbar(data = subset(BMA_df, time > time_values_new$time_values[length(outcome_var)]), inherit.aes = F,
                    aes(x = time+.1, ymin = ymin2, ymax = ymax97),
                    width = 0.1, linewidth = .2, color = "#33638DFF", alpha = 1) +
      geom_errorbar(data = subset(BMA_df, time > time_values_new$time_values[length(outcome_var)]),  inherit.aes = F,
                    aes(x = time, ymin = ymin12, ymax = ymax88),
                    width = 0.1, linewidth = .2, color = "#238A8DFF", alpha = 1) +
      geom_errorbar(data = subset(BMA_df, time > time_values_new$time_values[length(outcome_var)]), inherit.aes = F,
                    aes(x = time-.1, ymin = ymin25, ymax = ymax75),
                    width = 0.1, linewidth = .2, color = "#20A387FF", alpha = 1) +
      scale_color_manual(values = c(colors)) + 
      scale_x_continuous(breaks = time_values_new$time_values, labels = round(time_values_new$time_values,3)) +
      labs(title = "Mean Predicted Growth Trajectories for BMA", x = "Time Point", y = "Predicted Value", linetype = NULL, color = "Model") +  
      theme_minimal() + 
      theme(
        axis.title.x = element_text(size = 12, margin = margin(t = 20)),     
        axis.title.y = element_text(size = 12, margin = margin(r = 20)),   
        axis.text.x = element_text(size = 10),      
        axis.text.y = element_text(size = 10),   
        legend.title = element_text(size = 12),    
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 14),
        panel.grid.minor.x = element_blank()) 
    
    BMA_plot <- ggplotly(BMA_plot)
    
    for (i in 1:length(BMA_plot$x$data)){
      if (!is.null(BMA_plot$x$data[[i]]$name)){ BMA_plot$x$data[[i]]$name =  gsub("\\(","",stringr::str_split(BMA_plot$x$data[[i]]$name,",")[[1]][1]) } } #this removes the (xx,1) thing from the legend labels (ggplotly bug)

    BMA_dat$BMA_dat <- BMA_df
    
    output$overall_BMA_plot <- renderPlotly({ BMA_plot }) })
  
  output$comparison_condition_message <- renderUI({ #change to compute stack button for stacking
    if (input$run_BMA == 0 || input$run_models == 0) {HTML("<span style='color: #9B0000; margin-bottom: 20px; font-size: 20px; display: block;'>Please run both Bayesian Stacking and BMA procedures before proceeding to this section.</span>")}})
  
  kld_df <- reactiveValues(kld_dat = NULL)
  output$comparison_kld_table <- DT::renderDataTable({
    req(BMA_values$growth_KLD, BMA_values$int_KLD,reactive_vals$stacking_kld_int, reactive_vals$stacking_kld, cancelOutput = T)

    kld_data <- data.frame(
      Model = c("Growth Rate", "Intercept"),
      Stacking = c(round(reactive_vals$stacking_kld,3), round(reactive_vals$stacking_kld_int,3)), #add option to specify number of sig figs
      BMA = c(round(BMA_values$growth_KLD,3), round(BMA_values$int_KLD,3)) )
    
    
    DT::datatable(kld_data, options = list(dom = 't', autoWidth = TRUE), class = 'big-table') 
    kld_df$kld_dat <- kld_data
    })
  
  output$download_kld_table <- downloadHandler(
    filename = function() {
      paste("kld_table", Sys.Date(), ".csv", sep = "")},
    content = function(file) {
      write.csv(kld_df$kld_dat, file)} )
  
  output$comparison_condition_message_2 <- renderUI({
    if (input$run_BMA == 0 || input$run_models == 0) {
      HTML("<span style='color: #9B0000; margin-bottom: 20px; font-size: 20px; display: block;'>Please run both Bayesian Stacking and BMA procedures before proceeding to this section.</span>")}})
  
  observeEvent(input$generate_comparison_plot, {
    
    if (is.null(BMA_values$model_int) | is.null(reactive_vals$ypredStacking_int)){
      showNotification("Please run both Bayesian Stacking and BMA procedures before generating trajectory plots.", type = "error")
      return()
    }
    # add req and error messages 
    req(BMA_values$model_int, reactive_vals$ypredStacking_int, cancelOutput = T)
    
    df <- read.csv(input$data_file$datapath)
    time_indicator <- input$time_indicator
    time_distance <- input$time_distance
    outcome_var <- input$outcome_variable
    predictor_vars <- input$predictor_variables_BMA
    outcome_var_df <- df[, input$outcome_variable, drop = FALSE]
    comparison_future_timepoints <- input$comparison_future_timepoints
    
    BMA_data <- BMA_values$data
    BMA_model <- BMA_values$model
    BMA_data_int <- BMA_values$data_int
    BMA_model_int <- BMA_values$model_int
    
    calculate_time_values <- function(time_indicator, time_distance, outcome_var_length, future_points) {
      if (time_indicator == "Linear") {
        time_values <- seq(0, by = time_distance, length.out = outcome_var_length + future_points)
      } else if (time_indicator == "Quadratic") {
        time_values <- (seq(0, length.out = outcome_var_length + future_points) * time_distance)^2
      } else if (time_indicator == "Latent Basis") {
        latent_time <- reactive_fit_list_est$fit_est_list[[1]][(outcome_var_length + 1):(outcome_var_length * 2)]
        future_time <- seq(length(latent_time), length.out = future_points) * time_distance
        time_values <- c(latent_time, future_time)
      } else {
        stop("Invalid time_indicator. Please choose 'Linear', 'Quadratic', or 'Latent Basis'.") #don't really need this for the app ig
      }
      
      return(list(time_values = time_values, future_points = future_points)) }
    
    time_values_new <- calculate_time_values(time_indicator = time_indicator, time_distance = time_distance, 
                                             outcome_var_length = length(outcome_var), future_points = comparison_future_timepoints ) 
    
    BMA_dens <- pred.density(BMA_model, newdata = BMA_data[,2:length(BMA_data)])
    test <- data.frame(BMA_dens$fit, BMA_dens$std.err, quantile(BMA_dens, c(.025, .975, .12, .88, .25, .75)))
    colnames(test) <- c("Mean Growth", "SE Growth", "2.5% Growth", "97.5% Growth", "12% Growth", "88% Growth", "25% Growth", "75% Growth")
    BMA_dens_int <- pred.density(BMA_model_int, newdata = BMA_data_int[,2:length(BMA_data_int)])
    test_int <- data.frame(BMA_dens_int$fit, BMA_dens_int$std.err, quantile(BMA_dens_int, c(.025, .975, .12, .88, .25, .75)))
    colnames(test_int) <- c("Mean Int", "SE Int", "2.5% Int", "97.5% Int", "12% Int", "88% Int", "25% Int", "75% Int")
    test2 <- cbind(test, test_int)
    
    BMA_df <- data.frame(y = mean(test2$`Mean Int`) + time_values_new$time_values*mean(test2$`Mean Growth`), 
                         ymin2 = mean(test2$`2.5% Int`) + mean(test2$`2.5% Growth`)*time_values_new$time_value,
                         ymax97 = mean(test2$`97.5% Int`) + mean(test2$`97.5% Growth`)*time_values_new$time_value,
                         ymin12 = mean(test2$`12% Int`) + mean(test2$`12% Growth`)*time_values_new$time_value,
                         ymax88 = mean(test2$`88% Int`) + mean(test2$`88% Growth`)*time_values_new$time_value,
                         ymin25 = mean(test2$`25% Int`) + mean(test2$`25% Growth`)*time_values_new$time_value,
                         ymax75 = mean(test2$`75% Int`) + mean(test2$`75% Growth`)*time_values_new$time_value,
                         time = time_values_new$time_value, Model ="BMA")
    
    stacked_pred <- array(NA, dim = c(4000, nrow(df), length(time_values_new$time_values)))
    for (i in 1:length(time_values_new$time_values)) {
      stacked_pred[, , i] <- reactive_vals$ypredStacking * time_values_new$time_values[i] + reactive_vals$ypredStacking_int }
    
    mean_predictions <- list()
    lower <- list()
    upper <- list()
    for (i in 1:length(time_values_new$time_values)) {
      mean_predictions[[i]] <- apply(stacked_pred[, , i], 2, mean)
      lower[[i]] <- apply(stacked_pred[, , i], 2, quantile, probs = 0.025)
      upper[[i]] <- apply(stacked_pred[, , i], 2, quantile, probs = 0.975)} 
    
    overall_mean <- numeric(length(time_values_new$time_values))
    overall_lower2 <- numeric(length(time_values_new$time_values))
    overall_upper97 <- numeric(length(time_values_new$time_values))
    overall_lower12 <- numeric(length(time_values_new$time_values))
    overall_upper88 <- numeric(length(time_values_new$time_values))
    overall_lower25 <- numeric(length(time_values_new$time_values))
    overall_upper75 <- numeric(length(time_values_new$time_values))
    
    for (i in 1:length(time_values_new$time_values)) {
      combined_values <- as.vector(stacked_pred[, , i])
      overall_mean[i] <- mean(combined_values)
      overall_lower2[i] <- quantile(combined_values, probs = 0.025)
      overall_upper97[i] <- quantile(combined_values, probs = 0.975)
      
      overall_lower12[i] <- quantile(combined_values, probs = 0.12)
      overall_upper88[i] <- quantile(combined_values, probs = 0.88)
      
      overall_lower25[i] <- quantile(combined_values, probs = 0.25)
      overall_upper75[i] <- quantile(combined_values, probs = 0.75)}
    
    y_loo_df <- data.frame(
      time = time_values_new$time_values,
      y = overall_mean,
      ymin2 = overall_lower2,
      ymax97 = overall_upper97,
      ymin12 = overall_lower12,
      ymax88 = overall_upper88,
      ymin25 = overall_lower25,
      ymax75 = overall_upper75,
      Model = "Stacked Predictions")
    
    colors <- c("Empirical" = "gray59", "BMA" = "#481567FF", "Stacked Predictions" =  "#29AF7FFF")
    
    
    comparison_data <- rbind(y_loo_df, BMA_df)
    
    empirical_df <- data.frame(time = time_values_new$time_values[1:length(outcome_var)], y = colMeans(outcome_var_df), Model = "Empirical")
    
   plot <- ggplot(empirical_df, aes(x = time, y = y, color = Model)) +
     geom_vline(xintercept = time_values_new$time_values[length(time_values_new$time_values) - time_values_new$future_points], 
                linetype = "dashed", alpha = 0.5) +
     geom_point(size = 3) +
     geom_line(linewidth = 1.5) +  
     geom_point(data = BMA_df, aes(x = time, y = y, color = Model), size = 4) +
     geom_line(data = BMA_df, aes(x = time, y = y, color = Model), linewidth = 2) +
     geom_line(data = y_loo_df, size = 2, aes(x = time, y = y, color = Model)) +
     geom_point(data = y_loo_df, size = 4, aes(x = time, y = y, color = Model)) +
     scale_color_manual(values = c(colors)) + 
     scale_x_continuous(breaks = time_values_new$time_values, labels = round(time_values_new$time_values,3)) +
     labs(title = "Mean Predicted Growth Trajectories", x = "Time Point", y = "Predicted Value", linetype = NULL, color = "Model") +  
     theme_minimal() + 
     theme(
       axis.title.x = element_text(size = 12, margin = margin(t = 20)),     
       axis.title.y = element_text(size = 12, margin = margin(r = 20)),   
       axis.text.x = element_text(size = 10),      
       axis.text.y = element_text(size = 10),   
       legend.title = element_text(size = 12),    
       legend.text = element_text(size = 10),
       plot.title = element_text(size = 14),
       panel.grid.minor.x = element_blank()) 
   
   if (input$error_bars == "BMA predictions"){
     combined_plot <- plot + 
       geom_errorbar(data = subset(BMA_df, time > time_values_new$time_values[length(outcome_var)]), inherit.aes = F,
                    aes(x = time+.1, ymin = ymin2, ymax = ymax97),
                     width = 0.1, linewidth = .2, color = "#7c459e", alpha = 1) +
       geom_errorbar(data = subset(BMA_df, time > time_values_new$time_values[length(outcome_var)]),  inherit.aes = F,
                     aes(x = time, ymin = ymin12, ymax = ymax88),
                     width = 0.1, linewidth = .2, color = "#a375bf", alpha = 1) +
       geom_errorbar(data = subset(BMA_df, time > time_values_new$time_values[length(outcome_var)]), inherit.aes = F,
                     aes(x = time-.1, ymin = ymin25, ymax = ymax75),
                     width = 0.1, linewidth = .2, color = "#c9b4d6", alpha = 1)
     output$caption_comparison <- renderUI(tags$p("The above plot provides error bars to denote 50%, 75%, and 95% Bayesian prediction intervals for the BMA estimates at future time points.", 
                                             style = "text-align: left; font-size: 12px; color: gray; margin-top: 20px;"))
     } else if(input$error_bars == "Stacked predictions"){
      combined_plot <- plot + 
        geom_errorbar(data = subset(y_loo_df, time > time_values_new$time_values[length(outcome_var)]), inherit.aes = F,
                      aes(x = time+.1, ymin = ymin2, ymax = ymax97),
                      width = 0.1, linewidth = .2, color = "#4bd1a1", alpha = 1) +
        geom_errorbar(data = subset(y_loo_df, time > time_values_new$time_values[length(outcome_var)]),  inherit.aes = F,
                      aes(x = time, ymin = ymin12, ymax = ymax88),
                      width = 0.1, linewidth = .2, color = "#7bd1b3", alpha = 1) +
        geom_errorbar(data = subset(y_loo_df, time > time_values_new$time_values[length(outcome_var)]), inherit.aes = F,
                      aes(x = time-.1, ymin = ymin25, ymax = ymax75),
                      width = 0.1, linewidth = .2, color = "#b4cfc6", alpha = 1)
      output$caption_comparison <- renderUI(tags$p("The above plot provides error bars to denote 50%, 75%, and 95% Bayesian prediction intervals for the stacked estimates at future time points.", 
                                              style = "text-align: left; font-size: 12px; color: gray; margin-top: 20px;"))
     } else {combined_plot <- plot
     output$caption_comparison <- renderUI(tags$p("", 
                                             style = "text-align: left; font-size: 12px; color: gray; margin-top: 20px;")) }

  output$comparison_plot <- renderPlotly({ggplotly(combined_plot)})
  
  output$comparison_data_table <- DT::renderDataTable({
    colnames(comparison_data) <- c("Time", "Estimate", "2.5% Estimate", "97.5% Estimate", "12% Estimate", "88% Estimate", "25% Estimate", "75% Estimate", "Model")
    comparison_data[] <- lapply(comparison_data, function(x) if(is.numeric(x)) round(x, 3) else x)
    datatable(comparison_data, options = list(pageLength = 10))})
  
  output$download_comparison_data <- downloadHandler(
    filename = function() {
      paste("comparison_data", Sys.Date(), ".csv", sep = "")},
    content = function(file) {
      write.csv(comparison_data, file)})
  })
            
} #server end bracket 


shinyApp(ui, server)

## NEXT: 


### TO DO STILL ####
# make gcm page two columns of options since there's no output there?

# strip off individual effect of member models to see table with CIs and probabilities effects are greater or less than 0 (actually do this)