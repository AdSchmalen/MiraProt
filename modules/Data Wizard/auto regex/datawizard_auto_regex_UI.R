# ============================================================================
# Sub-script: Auto Regex presentation
# Purpose: Build the private, namespaced module body and diagnostic panels.
# Owns: static controls, output placeholders, help text, and collapsible markup.
# Does not own: reactive state, observers, inference, transfers, downloads,
# Session-tab navigation, dependency installation, or application lifecycle.
# Private interface: auto_regex_ui(ns) -> Shiny tag tree;
# auto_regex_diagnostic_panel(ns, name, title, output) -> namespaced panel.
# Namespace: accepts the composition root's shiny::NS(id) function and applies
# it to every input, output, content, and icon identifier.
# Dependencies: Shiny UI primitives and DT output placeholders.
# ============================================================================

auto_regex_diagnostic_panel <- function(ns, name, title, output) {
  shiny::wellPanel(
    style = "margin-bottom: 10px;",
    shiny::tags$div(
      style = paste(
        "cursor: pointer; background-color: #f5f5f5; padding: 8px;",
        "margin: -15px -15px 10px -15px; border-bottom: 1px solid #ddd;"
      ),
      onclick = paste0(
        "Shiny.setInputValue('", ns(paste0("toggle_", name)),
        "', Math.random())"
      ),
      shiny::h5(style = "margin: 0; display: inline-block;", title),
      shiny::tags$i(
        id = ns(paste0(name, "_icon")),
        class = "fa fa-chevron-right",
        style = "float: right; margin-top: 3px;"
      )
    ),
    shiny::div(
      id = ns(paste0(name, "_content")),
      style = "display: none; overflow-x: auto;",
      shiny::uiOutput(ns(paste0(name, "_summary"))),
      output
    )
  )
}

auto_regex_ui <- function(ns) {
  diagnostic_panels <- list(
    auto_regex_diagnostic_panel(
      ns, "validation", "Metadata Validation",
      shiny::verbatimTextOutput(ns("validation_text"))
    ),
    auto_regex_diagnostic_panel(
      ns,
      "content_rules",
      "Inferred Content Rules",
      shiny::tagList(
        shiny::div(
          class = "alert alert-info",
          style = "padding: 8px;",
          shiny::strong(
            "Regex redundancy: "
          ),
          paste(
            "The global value is the default for all Content regexes.",
            "After inference, individual rules can override it below.",
            "Changing either value immediately rebuilds the displayed Auto RegEx candidate",
            "from the cached full inference. Use Transfer Rules to apply that candidate",
            "to the other tabs of the Auto-Assign Assistant."
          )
        ),
        shiny::uiOutput(
          ns(
            "content_redundancy_controls"
          )
        ),
        DT::DTOutput(
          ns(
            "content_rules_table"
          )
        )
      )
    ),
    auto_regex_diagnostic_panel(
      ns, "content_diagnostics", "Content Rule Diagnostics",
      shiny::tagList(
        shiny::div(
          class = "alert alert-info", style = "padding: 8px;",
          shiny::strong("How to read these diagnostics: "),
          "TP (true positive) and TN (true negative) are correct matches and correct rejections; ",
          "FP (false positive) is an incorrect match; FN (false negative) is a missed match. ",
          "Precision is TP / (TP + FP), recall is TP / (TP + FN), F1 balances precision and recall, ",
          "and coverage is the share of rows matched by the rule."
        ),
        DT::DTOutput(ns("content_diagnostics_table"))
      )
    ),
    auto_regex_diagnostic_panel(
      ns, "semantic_spans", "Semantic Spans",
      shiny::tagList(
        shiny::div(class = "alert alert-info", style = "padding: 8px;",
          "Confirmed condition, numerator, and denominator spans that may be generalized. ",
          "Removed condition/numerator/denominator literals are dataset-specific values, not reusable rule structure."),
        DT::DTOutput(ns("semantic_spans_table"))
      )
    ),
    auto_regex_diagnostic_panel(
      ns, "content_refinement_lineage", "Content-Rule Refinement Lineage",
      shiny::tagList(
        shiny::div(class = "alert alert-info", style = "padding: 8px;",
          "Shows whether each targeted content rule was generalized or retained unchanged. ",
          "Removed values are dataset-specific condition, numerator, or denominator literals."),
        DT::DTOutput(ns("content_refinement_lineage_table"))
      )
    ),
    auto_regex_diagnostic_panel(
      ns, "condition_rules", "Inferred Condition Rules",
      DT::DTOutput(ns("condition_rules_table"))
    ),
    auto_regex_diagnostic_panel(
      ns, "condition_diagnostics", "Condition Rule Diagnostics",
      DT::DTOutput(ns("condition_diagnostics_table"))
    ),
    auto_regex_diagnostic_panel(
      ns, "ratio_rules", "Inferred Ratio Rules",
      DT::DTOutput(ns("ratio_rules_table"))
    ),
    auto_regex_diagnostic_panel(
      ns, "ratio_diagnostics", "Ratio Rule Diagnostics",
      DT::DTOutput(ns("ratio_diagnostics_table"))
    ),
    auto_regex_diagnostic_panel(
      ns, "run_diagnostics", "Warnings, Errors, and Timings",
      shiny::verbatimTextOutput(ns("run_diagnostics_details"))
    )
  )

  shiny::div(
    id = ns("root"),
    class = "auto-regex-module",
    shiny::tags$style(shiny::HTML(paste(
      ".auto-regex-module .auto-regex-readiness-panel {",
      "background-color: #4a4a4a; border-color: #3a3a3a;",
      "}",
      ".auto-regex-module .auto-regex-readiness-heading,",
      ".auto-regex-module .auto-regex-readiness-ready { color: #fff; }"
    ))),
    shiny::wellPanel(
      style = "margin-bottom: 10px;",
      shiny::h4("Infer Assignment Rules"),
      shiny::p(
        "Infer content, condition, and ratio rules from MiraProt metadata or an Excel worksheet."
      ),
      shiny::uiOutput(
        ns("source_control")
      ),
      shiny::div(
        class = "alert alert-info",
        style = "padding: 10px;",
        shiny::strong("Active source"),
        shiny::fluidRow(
          shiny::column(9, shiny::verbatimTextOutput(ns("active_source_summary"))),
          shiny::column(3, shiny::div(
            title = paste(
              "Downloads a metadata sheet for the active dataset.",
              "Fill it in with Excel, then upload it through Excel metadata workbook mode."
            ),
            shiny::downloadButton(ns("download_metadata_template"),
              "Metadata template", style = "width: 100%;")
          ))
        )
      ),
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] == 'excel'", ns("source")),
        shiny::fluidRow(
          shiny::column(
            9,
            shiny::div(
              title = "Upload an Excel workbook containing metadata to use instead of the current Data Wizard metadata.",
              shiny::fileInput(
                ns("excel_file"), "Excel workbook:", accept = c(".xlsx", ".xls")
              )
            )
          ),
          shiny::column(
            3,
            shiny::uiOutput(ns("excel_worksheet_controls"))
          )
        ),
        shiny::uiOutput(ns("excel_mapping_controls"))
      ),
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] == 'current_metadata'", ns("source")),
        shiny::div(
          class = "alert alert-default auto-regex-readiness-panel",
          style = "padding: 10px; border: 1px solid #3a3a3a;",
          shiny::h5(
            "Current-metadata readiness",
            class = "auto-regex-readiness-heading",
            style = "margin-top: 0;"
          ),
          shiny::uiOutput(ns("current_metadata_readiness"))
        )
      ),
      shiny::fluidRow(
        shiny::column(
          4,
          shiny::uiOutput(
            ns("global_redundancy_control")
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(
          4,
          shiny::uiOutput(
            ns("transfer_rules_control")
          )
        ),
        shiny::column(
          4,
          offset = 4,
          shiny::div(
            title = paste(
              "Validate the active metadata source, run full inference when",
              "required, and automatically transfer the inferred rules."
            ),
            shiny::actionButton(
              ns("infer_rules"),
              "Infer Rules",
              class = "btn-primary",
              style = "width: 100%;"
            )
          )
        )
      ),
      shiny::div(
        class = "alert alert-default",
        style = "margin-top: 10px; padding: 10px; border: 1px solid #ddd;",
        shiny::strong("Processing status"),
        shiny::verbatimTextOutput(ns("processing_status"))
      )
    ),
    shiny::tagList(diagnostic_panels)
  )
}
