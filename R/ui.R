# R/ui.R
# ========================================
# UI Definition: navbarPage with all tabs
# ========================================
# Returns the complete UI tree.
# Called from app.R as: ui <- build_ui(modEnv)
# Depends on: modEnv (module UI functions loaded into it)

build_ui <- function(modEnv) {
  version_info <- miraprot_version_info()

  navbarPage(
    title = "MiraProt - Proteomics Data Analysis",
    theme = bslib::bs_theme(version = 4, bootswatch = "flatly"),

    header = tags$head(
      tags$style(HTML("
      .tab-content .nav-tabs > li > a {
  padding: 10px 20px !important;
  font-size: 16px;
}
        .documentation-tabset-wrapper {
          padding: 16px;
          margin-top: 12px;
          background: linear-gradient(180deg, #faf7ff 0%, #ffffff 100%);
          border: 1px solid #e2d5f8;
          border-radius: 14px;
          box-shadow: 0 10px 24px rgba(111, 66, 193, 0.08);
        }
        .documentation-tabset-wrapper .documentation-tabset-intro {
          margin-bottom: 14px;
          padding: 14px 18px;
          background: rgba(111, 66, 193, 0.08);
          border-left: 4px solid #6f42c1;
          border-radius: 5px;
          color: #5a32a3;
        }
        .documentation-tabset-wrapper .documentation-tabset-intro h4 {
          margin-bottom: 4px;
          color: #5a32a3;
        }
        .documentation-tabset-wrapper .nav-tabs {
          border-bottom: 1px solid #d8c6f3;
          gap: 6px;
        }
        .documentation-tabset-wrapper .nav-tabs > li > a {
  color: #6f42c1;
  border: 1px solid transparent;
  border-radius: 5px 5px 0 0;
  font-weight: 600;
  background-color: rgba(111, 66, 193, 0.06);

  padding: 6px 10px;
}
        .documentation-tabset-wrapper .nav-tabs > li > a:hover,
        .documentation-tabset-wrapper .nav-tabs > li > a:focus {
          color: #4b2c83;
          background-color: rgba(111, 66, 193, 0.14);
          border-color: #d8c6f3 #d8c6f3 transparent;
        }
        .documentation-tabset-wrapper .nav-tabs > li.active > a,
        .documentation-tabset-wrapper .nav-tabs > li.active > a:hover,
        .documentation-tabset-wrapper .nav-tabs > li.active > a:focus {
          color: #4b2c83;
          background-color: #ffffff;
          border: 1px solid #d8c6f3;
          border-bottom-color: #ffffff;
        }
        .documentation-tabset-wrapper .tab-content {
          padding: 20px;
          background-color: #ffffff;
          border: 1px solid #d8c6f3;
          border-top: none;
          border-radius: 0 0 12px 12px;
        }
      "))
    ),

    # ========================================
    # Main Analysis Tab
    # ========================================
    tabPanel(
      title = "Main Analysis",
      value = "main_analysis",

      useShinyjs(),

      # --- JavaScript snippets ---
      tags$script(HTML("
  // Blockiert das Mausrad f\u00fcr fokussierte number-Inputs
  document.addEventListener('wheel', function(e) {
    const el = document.activeElement;
    if (el && el.tagName === 'INPUT' && el.type === 'number') {
      e.preventDefault();
    }
  }, { passive: false });
")),

      tags$script(HTML("
  function updateWindowSize() {
    if (!window.Shiny || !Shiny.setInputValue) return;

    Shiny.setInputValue('window_inner_width', window.innerWidth);
    Shiny.setInputValue('window_inner_height', window.innerHeight);
    Shiny.setInputValue('devicePixelRatio', window.devicePixelRatio);
  }

  $(document).on('shiny:connected', function() {
    updateWindowSize();
    $(window).on('resize.windowSize', updateWindowSize);
  });
")),

      tags$script(HTML("
      Shiny.addCustomMessageHandler('update_Volcano', function(msg) {
        $('#update_Volcano').click();
      });
    ")),

      # --- CSS ---
      tags$style(HTML("
      .flex-container {
        display: flex;
        flex-direction: column;
        justify-content: space-between;
        height: 100%;
        gap: 10px;
      }
      .flex-item {
        margin-bottom: 0;
      }
      .custom-ratios-container {
        display: flex;
        flex-direction: column;
        justify-content: space-between;
        height: 100%;
        gap: 10px;
      }
      .export-panel {
        background-color: #f8f9fa;
        border: 1px solid #dee2e6;
        border-radius: 8px;
        padding: 20px;
        margin-top: 20px;
      }
      .status-indicator {
        font-weight: bold;
        padding: 8px 12px;
        border-radius: 4px;
        display: inline-block;
        margin-bottom: 10px;
      }
      .status-ready {
        background-color: #d4edda;
        color: #155724;
        border: 1px solid #c3e6cb;
      }
      .status-warning {
        background-color: #fff3cd;
        color: #856404;
        border: 1px solid #ffeaa7;
      }
      .status-error {
        background-color: #f8d7da;
        color: #721c24;
        border: 1px solid #f5c6cb;
      }
    ")),

      tags$script("
  Shiny.addCustomMessageHandler('copyToClipboard', function(message) {
    navigator.clipboard.writeText(message).then(function() {
      console.log('Copied to clipboard successfully');
    }).catch(function(err) {
      console.error('Failed to copy: ', err);
    });
  });
"),

      tags$head(
        tags$style(HTML("
    .list-group-item {
      padding: 8px 15px;
      border: 1px solid #ddd;
      margin-bottom: -1px;
      cursor: pointer;
    }
    .list-group-item:hover {
      background-color: #f5f5f5;
    }
    .list-group-item-action:focus,
    .list-group-item-action:hover {
      background-color: #f5f5f5;
      text-decoration: none;
    }
    pre {
      white-space: pre-wrap;
      word-wrap: break-word;
    }
  "))
      ),

      # --- Module tabs ---
      fluidRow(
        column(12,
               tabsetPanel(
                 id = "analysis_tabs",
                 type = "tabs",

                 tabPanel(
                   title = "Data Wizard",
                   value = "datawizard",
                   modEnv$modDataWizardUI("datawizard")
                 ),

                 tabPanel(
                   title = "Table",
                   value = "modtable",
                   modEnv$modModifiedTableUI("modtable")
                 ),

                 tabPanel(
                   title = "Abundances",
                   value = "abundance",
                   modEnv$modAbundancesUI("abundance")
                 ),

                 tabPanel(
                   title = "Sample IDs",
                   value = "sampleid",
                   modEnv$modSampleIDsUI("sampleid")
                 ),

                 tabPanel(
                   title = "Dimensionality Reduction",
                   value = "pca",
                   modEnv$modPCAUI("pca")
                 ),

                 tabPanel(
                   title = "Volcano Plot",
                   value = "volcano",
                   modEnv$modVolcanoUI("volcano")
                 ),

                 tabPanel(
                   title = "Dot Plot",
                   value = "dotplot",
                   modEnv$modDotPlotUI("dotplot")
                 ),

                 tabPanel(
                   title = "GO",
                   value = "go",
                   modEnv$modGOUI("go")
                 ),

                 tabPanel("GSEA",
                          value = "gsea",
                          tryCatch({
                            if (!exists("NS", envir = modEnv)) {
                              modEnv$NS <- shiny::NS
                            }
                            if (!exists("moduleServer", envir = modEnv)) {
                              modEnv$moduleServer <- shiny::moduleServer
                            }

                            if (exists("GSEA_module_ui", envir = modEnv)) {
                              modEnv$GSEA_module_ui("gsea")
                            } else {
                              div(
                                style = "text-align: center; padding: 50px; color: #999;",
                                h4("GSEA Module Not Available"),
                                p("The GSEA analysis module could not be loaded."),
                                p("Please check that all required files are present and Shiny is properly loaded."),
                                actionButton("reload_gsea", "Try Reload",
                                             onclick = "location.reload();",
                                             class = "btn-primary")
                              )
                            }
                          }, error = function(e) {
                            div(
                              style = "text-align: center; padding: 50px; color: #d9534f;",
                              h4("GSEA Module Error"),
                              p("Error loading GSEA module:", e$message),
                              verbatimTextOutput("gsea_error_details"),
                              br(),
                              actionButton("reload_app", "Reload Application",
                                           onclick = "location.reload();",
                                           class = "btn-warning")
                            )
                          })
                 ),

                 tabPanel(
                   title = "STRING DB",
                   value = "string",
                   modEnv$modSTRINGUI("string")
                 ),
                 tabPanel(
                   title = "Venn",
                   value = "venn",
                   modEnv$modVennUI("venn")
                 ),
                 tabPanel(
                   title = "Heatmap",
                   value = "heatmap",
                   modEnv$modHeatmapUI("heatmap")
                 ),
                 tabPanel(
                   title = "Plot Grid",
                   value = "plot_grid",
                   modEnv$modGridUI("grid")
                 )
               )
        )
      ),

      # --- Export Results Section ---
      fluidRow(
        column(12,
               div(class = "export-panel",
                   h4("Export Results", style = "color: #495057; margin-bottom: 20px;"),

                   fluidRow(
                     column(8,
                            h5("Multi-Module Excel Export"),
                            p("Download comprehensive results from all active modules as a single Excel file")
                     ),

                     column(4,
                            downloadButton(
                              "download_results",
                              "Download Excel Results",
                              class = "btn-primary btn-lg",
                              style = "width: 100%; font-weight: bold;"
                            )
                     )
                   )
               )
        )
      )
    ),

    # ========================================
    # System Info Tab
    # ========================================
    tabPanel(
      title = "System Info",
      value = "system_info",

      div(style = "padding: 20px;",
          if (MIRAPROT_IN_PORTABLE) {
            tags$div(
              class = "alert alert-info",
              style = "padding: 8px 12px; margin-bottom: 15px;",
              tags$strong("Portable Desktop Mode"),
              tags$span(style = "margin-left: 10px;",
                        paste("Port:", Sys.getenv("MIRAPROT_PORT", "?")))
            )
          },
          h4("System Information"),

          wellPanel(
            h5("Window Dimensions"),
            verbatimTextOutput("window_size"),

            h5("Module Status"),
            verbatimTextOutput("module_status_detailed"),

            h5("Debug Information"),
            verbatimTextOutput("debug_info")
          )
      )
    ),

    # ========================================
    # Session Tab
    # ========================================
    tabPanel(
      title = "Session",
      value = "session_management",

      div(style = "padding: 20px; max-width: 900px;",

        # --- Information Section ---
        h4("Session Management"),

        wellPanel(
          h5("About Session Save and Restore"),
          p(
            "Save your MiraProt session to a file and restore it later. ",
            "Choose a save level to control how much state is captured."
          ),
          tags$ul(
            tags$li(
              tags$strong("Data & Metadata: "),
              "Saves processed data, metadata definitions, filtering, and ",
              "Data Wizard pipeline state. Smallest file size."
            ),
            tags$li(
              tags$strong("Data & Analysis Results: "),
              "Additionally saves GO and GSEA enrichment results so they ",
              "do not need to be recomputed."
            ),
            tags$li(
              tags$strong("Full Session State: "),
              "Additionally saves visualization module settings (Volcano, PCA, ",
              "Heatmap, Dotplot, STRING, Venn). Plot objects are re-rendered ",
              "from the restored configuration. Largest file size."
            )
          )
        ),
        br(),

        # --- Download Section ---
        wellPanel(
          h5("Save Current Session"),
          radioButtons(
            "session_save_level",
            "Save level:",
            choices = c(
              "Data & Metadata"         = "data_only",
              "Data & Analysis Results"  = "data_and_analysis",
              "Full Session State"       = "full_session"
            ),
            selected = "full_session",
            inline = TRUE
          ),
          radioButtons(
            "session_gc_control",
            "Garbage control:",
            choices = c(
              "Active" = "active",
              "Inactive" = "inactive"
            ),
            selected = "active",
            inline = TRUE
          ),
          downloadButton(
            "session_download",
            "Download Session",
            class = "btn-primary",
            style = "font-weight: bold;"
          )
        ),
        br(),

        # --- Restore Section ---
        wellPanel(
          h5("Restore Previous Session"),
          p("Upload a previously saved MiraProt session file to restore your data state. ",
            "The session will be restored automatically after validation."),
          fileInput(
            "session_file",
            "Select session file (.rds)",
            accept = ".rds",
            placeholder = "No file selected"
          ),
          uiOutput("session_restore_status")
        ),
        br(),

        # --- App Shutdown Section ---
        wellPanel(
          h5("Application Shutdown"),
          p("Use deep cleanup to close the app with explicit connection cleanup ",
            "and synchronous garbage collection. This may take longer, but is useful ",
            "before long RStudio sessions or when diagnosing leaks."),
          actionButton(
            "session_deep_cleanup_shutdown",
            "Run Deep Cleanup and Close App",
            class = "btn-default",
            style = "font-weight: bold; background-color: #2c3e50; border-color: #2c3e50; color: #fff;"
          )
        ),
        br(),

        # --- Session Debug Log Section ---
        wellPanel(
          h5("Log"),
          p("Shows the log captured during this session. ",
            "When a saved session is loaded, the log from that session is also shown."),
          fluidRow(
            column(
              width = 3,
              selectInput(
                "debug_level_select",
                label = "Debug Level",
                choices = c(
                  "0 \u2013 Essential" = 0,
                  "1 \u2013 Debug"     = 1,
                  "2 \u2013 Verbose"   = 2
                ),
                selected = 0
              )
            ),
            column(
              width = 3,
              selectInput(
                "session_log_tag_filter",
                label = "Debug call",
                choices = c("All debug calls" = ""),
                selected = "",
                selectize = FALSE
              )
            ),
            column(
              width = 6,
              helpText(
                tags$strong("0 \u2013 Essential:"),
                " reproduction-grade events only: file loads, sheet changes, metadata assignments,
    figure creation with settings, and module result counts.", tags$br(),
                tags$strong("1 \u2013 Debug:"),
                " high-level lifecycle events (startup, session save/restore, module load).", tags$br(),
                tags$strong("2 \u2013 Verbose:"),
                " all of the above plus detailed per-module diagnostics.",
                tags$br(), tags$br()
              )
            )
          ),
          fluidRow(
            column(
              width = 6,
              actionButton("session_log_copy", "Copy log", icon = icon("copy"))
            ),
            column(
              width = 6,
              downloadButton("session_log_download", "Download log")
            )
          ),
          br(),
          verbatimTextOutput("session_log_display"),
          tags$style(HTML(
            "#session_log_display { max-height: 400px; overflow-y: auto; font-size: 12px; background: #1e1e1e; color: #d4d4d4; }"
          ))
        )
      )
    ),

    # ========================================
    # Documentation Tab
    # ========================================
    tabPanel(
      title = "Documentation",
      value = "documentation",
      div(
        class = "documentation-tabset-wrapper",
        div(
          class = "documentation-tabset-intro",
          h4("Documentation Browser")
        ),
        tabsetPanel(
          id = "doc_tabs",
          tabPanel("MiraProt",               modEnv$modMiraProtDocUI("miraprot_doc")),
          tabPanel("Datawizard",              modEnv$modDatawizardDocUI("datawizard_doc")),
          tabPanel("Table",                   modEnv$modTablesModifiedDocUI("tables_modified_doc")),
          tabPanel("Abundances",              modEnv$modAbundancesDocUI("abundances_doc")),
          tabPanel("Sample IDs",              modEnv$modSampleIDsDocUI("sampleids_doc")),
          tabPanel("Dimensionality Reduction", modEnv$modDimRedDocUI("pca_doc")),
          tabPanel("Venn",                    modEnv$modVennDocUI("venn_doc")),
          tabPanel("Volcano",                 modEnv$modVolcanoDocUI("volcano_doc")),
          tabPanel("Dot Plot",                modEnv$modDotplotDocUI("dotplot_doc")),
          tabPanel("GO",                      modEnv$modGODocUI("GO_doc")),
          tabPanel("GSEA",                    modEnv$modGSEADocUI("GSEA_doc")),
          tabPanel("STRING DB",               modEnv$modSTRINGDocUI("STRING_doc")),
          tabPanel("Heatmap",                 modEnv$modHeatmapDocUI("heatmap_doc")),
          tabPanel("Plot Grid",               modEnv$modGridDocUI("grid_doc"))
        )
      )
    ),

    # ========================================
    # About Tab
    # ========================================
    tabPanel(
      title = "About",
      value = "about",

      fluidRow(
        column(12,
               h2("About MiraProt"),
               p(
                 "MiraProt is a modular R Shiny application for end-to-end proteomics data processing and analysis. ",
                 "It guides you from raw abundance tables through data curation, statistics, enrichment (GO/GSEA), ",
                 "network analysis (STRING-DB), and heatmaps to publication-ready figures."
               ),
               p(
                 "All modules share a common data backbone, so results from differential expression, PCA, enrichment, ",
                 "overlap analysis, overlaps and networks can be combined consistently and exported together."
               ),

               h3("Version Information"),
               p("Current Version: ", version_info$version),
               p("Commit: ", version_info$commit),
               p("Last Updated: ", version_info$last_updated),

               h3("Contact"),
               tags$p(
                 HTML(paste(
                   "Chair of Physiology", "<br>",
                   "Department of Veterinary Science", "<br>",
                   "Faculty of Veterinary Medicine of the", "<br>",
                   "Ludwig-Maximilians-Universit\u00e4t", "<br>",
                   "Lena-Christ-Str. 48", "<br>",
                   "82152 Planegg/Martinsried"
                 )),
                 style = "line-height: 1.2; margin-bottom: 0.5em;"
               )
        )
      )
    )
  )
}
