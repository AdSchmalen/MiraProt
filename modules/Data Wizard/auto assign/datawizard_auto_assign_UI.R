# ============================================================================
# Sub-Script: Data Wizard Auto-Assign UI Composition
# Purpose:
#   Define declarative UI builders for auto-assign template management and rule editors.
# Architectural Role:
#   Pure UI composition layer consumed by the auto-assign orchestrator.
# Responsibilities:
#   - Provide reusable section builders for content/sample/ratio/template UI blocks.
#   - Encapsulate labels, inputs, and layout for auto-assign interactions.
# Non-Responsibilities:
#   - Must not own business logic, state mutation, rule execution, or integration calls.
# Allowed Dependencies:
#   - Shiny UI primitives, HTML helpers, and DT output placeholders.
# Interaction Boundaries:
#   - Exports UI builder functions; no direct server-side side effects.
# Stability Guarantees:
#   - Input/output IDs and generated UI contract remain stable for server bindings.
# ============================================================================


############
# Template Management UI Section

#' Create template management UI section
#' @param ns namespace function
#' @return UI div element
create_template_management_ui <- function(ns) {
  wellPanel(
    h4("Auto-Assign Assistant"),
    p("Manage assignment rules, filter settings, edit operations, and configurations as integrated templates."),

    # Export main button
    fluidRow(
      column(4,
             div(
               title = "Export complete template including assignment rules, filter settings, edit operations, and selected UI configurations",
               downloadButton(
                 ns("export_rules_autoassign_dw"),
                 "Export Complete Template",
                 style = "width:100%",
                 class = "btn-info"
               )
             ))
    ),
    br(),

    # Master checkbox
    fluidRow(
      column(4,
             div(
               title = "Enable export of UI & settings for selected submodules",
               checkboxInput(
                 ns("save_ui_autoassign_dw"),
                 "Save UI & Settings",
                 value = FALSE
               )
             ))
    ),

    # All subordinate options wrapped in one conditionalPanel (only visible when master is TRUE)
    conditionalPanel(
      condition = sprintf("input['%s']", ns("save_ui_autoassign_dw")),

      fluidRow(
        column(4,
               div(
                 title = "Include current filtering module configuration in the exported template",
                 checkboxInput(
                   ns("include_filtering_config"),
                   "Include Filtering Config",
                   value = FALSE
                 )
               )),
        column(4,
               div(
                 title = "Include edit operations in the exported template",
                 checkboxInput(
                   ns("include_edit_operations"),
                   "Include Edit Operations",
                   value = FALSE
                 )
               )),
        column(4,
               div(
                 title = "Include current imputation module configuration in the exported template",
                 checkboxInput(
                   ns("include_imputation_config"),
                   "Include Imputation Config",
                   value = FALSE
                 )
               ))
      ),

      fluidRow(
        column(4,
               div(
                 title = "Include current batch effects module configuration in the exported template",
                 checkboxInput(
                   ns("include_batch_effects_config"),
                   "Include Batch Effects Config",
                   value = FALSE
                 )
               )),
        column(4,
               div(
                 title = "Include current pivot module configuration in the exported template",
                 checkboxInput(
                   ns("include_pivot_config"),
                   "Include Pivot Config",
                   value = FALSE
                 )
               )),
        column(4,
               div(
                 title = "Include current merge module configuration in the exported template",
                 checkboxInput(
                   ns("include_merge_config"),
                   "Include Merge Config",
                   value = FALSE
                 )
               ))
      ),

      fluidRow(
        column(4,
               div(
                 title = "Include current ratio configurations in the exported template",
                 checkboxInput(
                   ns("include_ratio_configurations"),
                   "Include Ratio Configurations",
                   value = FALSE
                 )
               )),
        column(4,
               div(
                 title = "Include current basemean module configuration in the exported template",
                 checkboxInput(
                   ns("include_basemean_ui_config"),
                   "Include Basemean Configurations",
                   value = FALSE
                 )
               ))

      )
    ),

    # Template Status
    fluidRow(
      column(12,
             verbatimTextOutput(ns("template_status"))
      )
    )
  )
}

############
# Content Rules UI Section

#' Create content assignment rules UI section
#' @param ns namespace function
#' @return UI div element
create_content_rules_ui <- function(ns) {
  wellPanel(
    h4("Define Content Assignment Rules"),
    p("Create rules to automatically assign content types based on column name patterns."),
    div(
      class = "alert alert-info",
      style = "font-size: 0.9em;",
      HTML(paste0(
        "<strong>How it works:</strong> Each rule fills the <em>Content</em> column of the metadata ",
        "table by testing column names against an include pattern and an optional exclude pattern. ",
        "Multiple terms in the include pattern are combined with <em>&amp;</em> (AND) or ",
        "<em>|</em> (OR) to refine the match.",
        "<br/><br/>",
        "<strong>Example:</strong> To classify columns as <em>Abundance Ratio p-Value</em>, set ",
        "Include to <em>Abundance&amp;Ratio&amp;p-Value</em> and Exclude to <em>Adj.</em>. ",
        "This captures standard p-value columns while leaving adjusted p-value columns available ",
        "for a separate, more specific rule. Note: whitespace is interpreted as part of the regular expression. ",
        "<em>Abundance&amp;Ratio&amp;p-Value</em> is different from <em>Abundance &amp; Ratio &amp; p-Value</em>."
      ))
    ),
    fluidRow(
      column(12,
             div(
               style = "margin-bottom: 15px; padding: 10px; border: 1px solid #ddd; background-color: #f8f9fa;",
               h5("Pattern Input Mode", style = "margin-top: 0; color: #495057;"),
               div(
                 title = "When enabled, plain text inputs will be automatically converted to regex patterns. When disabled, you can enter raw regex patterns directly.",
                 checkboxInput(ns("auto_convert_content_regex_dw"),
                               "Auto-convert text to regex patterns",
                               value = TRUE)
               ),
               div(
                 style = "font-size: 0.85em; color: #666; margin-top: 5px;",
                 conditionalPanel(
                   condition = sprintf("input['%s'] == true", ns("auto_convert_content_regex_dw")),
                   HTML("Text inputs will be automatically escaped for regex use<br/>
           <em>Example:</em> 'Abundance Ratio' → 'Abundance\\s+Ratio'")
                 ),
                 conditionalPanel(
                   condition = sprintf("input['%s'] == false", ns("auto_convert_content_regex_dw")),
                   HTML("Enter raw regex patterns directly (advanced users only)<br/>
           <em>Example:</em> 'Abundance\\s+Ratio' (stays unchanged)")
                 )
               )
             )
      )
    ),
    fluidRow(
      column(4,
             div(
               title = "Select the type of content this rule should assign to matching columns",
               selectInput(
                 ns("lookup_content_dw"),
                 "Content Type:",
                 choices = c(
                   "Abundance Ratio", "Abundance Ratio p-Value", "Abundance Ratio Adj. p-Value",
                   "Raw Abundance", "Normalized Abundance", "Batch Corrected Abundance", "Batch Corrected Normalized Abundance", "Batch Corrected Raw Abundance",
                   "Imputed Raw Abundance", "Imputed Normalized Abundance", "Imputed Batch Corrected Abundance",
                   "Imputed Batch Corrected Normalized Abundance", "Imputed Batch Corrected Raw Abundance",
                   "Protein Confidence", "Description", "# PSMs", "Found in Sample",
                   "Found in File", "Additional Information", "Identifier", "Basemean"
                 )
               ))),
      column(
        2,
        div(
          title = paste(
            "Stable identity of the currently selected content rule.",
            "Rules with the same Content type are distinguished by Rule ID."
          ),
          shiny::uiOutput(
            ns(
              "content_rule_id_display_autoassign_dw"
            )
          )
        )
      ),
      column(2,
             div(
               title = "Pattern that column names must contain to match this rule (use & for AND, | for OR)",
               textInput(ns("string_include_autoassign_dw"),
                         "Include Pattern:",
                         placeholder = "e.g., abundance OR ratio&pvalue")
             )),
      column(2,
             div(
               title = "Pattern that column names must NOT contain (optional exclusion filter)",
               textInput(ns("string_exclude_autoassign_dw"),
                         "Exclude Pattern:",
                         placeholder = "e.g., normalized")
             )),
      column(2,
             conditionalPanel(
               condition = paste(
                 sprintf("input['%s'] == 'Abundance Ratio'", ns("lookup_content_dw")),
                 sprintf("input['%s'] == 'Abundance Ratio p-Value'", ns("lookup_content_dw")),
                 sprintf("input['%s'] == 'Abundance Ratio Adj. p-Value'", ns("lookup_content_dw")),
                 sprintf("input['%s'] == 'Raw Abundance'", ns("lookup_content_dw")),
                 sprintf("input['%s'] == 'Normalized Abundance'", ns("lookup_content_dw")),
                 sprintf("input['%s'] == 'Batch Corrected Abundance'", ns("lookup_content_dw")),
                 sprintf("input['%s'] == 'Imputed Raw Abundance'", ns("lookup_content_dw")),
                 sprintf("input['%s'] == 'Imputed Normalized Abundance'", ns("lookup_content_dw")),
                 sprintf("input['%s'] == 'Imputed Batch Corrected Abundance'", ns("lookup_content_dw")),
                 sprintf("input['%s'] == 'Imputed Batch Corrected Normalized Abundance'", ns("lookup_content_dw")),
                 sprintf("input['%s'] == 'Imputed Batch Corrected Raw Abundance'", ns("lookup_content_dw")),
                 sprintf("input['%s'] == 'Batch Corrected Normalized Abundance'", ns("lookup_content_dw")),
                 sprintf("input['%s'] == 'Batch Corrected Raw Abundance'", ns("lookup_content_dw")),
                 sep = " || "
                ),
               div(
                 title = "Data transformation to apply to values in matching columns",
                 selectInput(
                   ns("transformation_col_dw"),
                   "Transformation:",
                   choices = c("None", "log2", "log10", "-log10")
                 )
               )))
    ),
    fluidRow(
      column(6,
             div(
               title = "Modify only the currently selected content Rule ID",
               actionButton(
                 ns("add_table_rule_autoassign_dw"),
                 "Modify Content Rule",
                 width = "100%",
                 class = "btn-primary"
               )
             )),
      column(6,
             div(
               title = "Remove only the currently selected content Rule ID",
               actionButton(ns("remove_table_rule_autoassign_dw"),
                            "Remove Content Rule",
                            width = "100%",
                            class = "btn-default",
                            style = "background-color: #3498db; border-color: #3498db; color: #fff;")
             ))
    ),
    br(),
    div(
      title = "View and manage active content assignment rules",
      DTOutput(ns("table_rules_content_dw"))
    ),
    br(),
    fluidRow(
      column(
        12,
        div(
          title = paste(
            "Create a new independent content rule",
            "with a new user Rule ID and VariantId."
          ),
          actionButton(
            ns("add_new_content_rule_autoassign_dw"),
            "Add New Content Rule",
            width = "100%",
            class = "btn-primary"
          )
        )
      )
    )
  )
}

############
# Condition Extraction Rules UI Section

#' Create sample/condition extraction rules UI section
#' @param ns namespace function
#' @return UI div element
create_sample_rules_ui <- function(ns) {
  wellPanel(
    h4("Define Condition Extraction Rules"),
    p("Create rules to extract condition names from column headers."),
    div(
      class = "alert alert-info",
      style = "font-size: 0.9em;",
      HTML(paste0(
        "<strong>How it works:</strong> These rules identify each condition (sample group) in the ",
        "study by parsing information directly from the column headers. For each matched column, the ",
        "<em>Sample</em> column is assigned a unique identifier for each content type, and the condition label is derived ",
        "from the column name.",
        "<br/><br/>",
        "<strong>Example:</strong> Given a column named ",
        "<em>Abundances (Normalized): F1: Sample, Case_X</em>, selecting the method ",
        "<em>End</em> with a whitespace separator extracts <em>Case_X</em> as the ",
        "condition label. Additional extraction methods such as <em>between</em>, ",
        "<em>phrase_position</em>, and <em>pattern_detect</em> are available; ",
        "refer to the documentation for details. Note: the <em>Sample</em> column in your metadata table is filled ",
        "with a unique column identifier and the assigned condition, separated by '_'. ",
        "In this example, it would be <em>F1_Case_X</em>. ",
        "None of the other parts of the column name are unique for the content type ",
        "<em>Normalized Abundance</em>."
      ))
    ),
    fluidRow(
      column(12,
             conditionalPanel(
               condition = paste(
                 sprintf("input['%s'] == 'between'", ns("cond_method_autoassign_dw")),
                 sprintf("input['%s'] == 'start'", ns("cond_method_autoassign_dw")),
                 sprintf("input['%s'] == 'end'", ns("cond_method_autoassign_dw")),
                 sep = " || "
               ),
               div(
                 style = "margin-bottom: 15px; padding: 10px; border: 1px solid #ddd; background-color: #f8f9fa;",
                 h6("Pattern Input Mode", style = "margin-top: 0; color: #495057;"),
                 div(
                   title = "When enabled, plain text inputs will be automatically converted to regex patterns. When disabled, you can enter raw regex patterns directly.",
                   checkboxInput(ns("auto_convert_sample_regex_dw"),
                                 "Auto-convert text to regex patterns",
                                 value = TRUE)
                 ),
                 div(
                   style = "font-size: 0.85em; color: #666; margin-top: 5px;",
                   conditionalPanel(
                     condition = sprintf("input['%s'] == true", ns("auto_convert_sample_regex_dw")),
                     "Pattern inputs will be automatically escaped for regex use"
                   ),
                   conditionalPanel(
                     condition = sprintf("input['%s'] == false", ns("auto_convert_sample_regex_dw")),
                     "Enter raw regex patterns directly (advanced users only)"
                   )
                 )
               )
             )
             )
    ),
    fluidRow(
      column(4,
             div(
               title = "Select the content type from which to extract sample information",
               selectInput(
                 ns("cond_content_autoassign_dw"),
                 "Source Content Type:",
                 choices = c(
                   "Abundance Ratio", "Abundance Ratio p-Value", "Abundance Ratio Adj. p-Value",
                   "Raw Abundance", "Normalized Abundance", "Batch Corrected Abundance", "Batch Corrected Normalized Abundance", "Batch Corrected Raw Abundance", "Imputed Raw Abundance",
                   "Protein Confidence", "Description", "# PSMs", "Found in Sample",
                   "Imputed Normalized Abundance", "Imputed Batch Corrected Abundance",
                   "Imputed Batch Corrected Normalized Abundance", "Imputed Batch Corrected Raw Abundance",
                   "Found in File", "Additional Information"
                 )
               ))),
      column(
        2,
        div(
          title = paste(
            "Stable identity of the currently selected condition extraction rule.",
            "The rule remains linked to its content VariantId separately."
          ),
          shiny::uiOutput(
            ns(
              "condition_rule_id_display_autoassign_dw"
            )
          )
        )
      ),
      column(2,
             div(
               title = "Method for extracting sample names from column headers",
               selectInput(
                 ns("cond_method_autoassign_dw"),
                 "Extraction Method:",
                 choices = c("between", "start", "end", "whole", "phrase_position", "pattern_detect")
               ))),
      column(2,
             conditionalPanel(
               condition = paste(
                 sprintf("input['%s'] == 'between'", ns("cond_method_autoassign_dw")),
                 sprintf("input['%s'] == 'end'", ns("cond_method_autoassign_dw")),
                 sep = " || "
               ),
               div(
                 title = "Pattern that appears before the sample name",
                 textInput(ns("cond_before_autoassign_dw"),
                           "Pattern Before:",
                           placeholder = "e.g., Sample_")
               )
             ),
             conditionalPanel(
               condition = paste(
                 sprintf("input['%s'] == 'phrase_position'", ns("cond_method_autoassign_dw")),
                 sprintf("input['%s'] == 'pattern_detect'", ns("cond_method_autoassign_dw")),
                 sep = " || "
               ),
               div(
                 title = "Position of the sample name when splitting by separators",
                 numericInput(ns("cond_pos_autoassign_dw"),
                              "Position:",
                              value = 1,
                              min = 1)
               ))),
      column(2,
             conditionalPanel(
               condition = paste(
                 sprintf("input['%s'] == 'between'", ns("cond_method_autoassign_dw")),
                 sprintf("input['%s'] == 'start'", ns("cond_method_autoassign_dw")),
                 sep = " || "
               ),
               div(
                 title = "Pattern that appears after the sample name",
                 textInput(ns("cond_after_autoassign_dw"),
                           "Pattern After:",
                           placeholder = "e.g., .raw")
               )))
    ),

    # Separator selection
    tags$head(
      tags$style(HTML("
          .fourcols .shiny-options-group {
            column-count: 4;
            column-gap: 1em;
          }
        "))
    ),
    div(
      class = "fourcols",
      div(
        title = "Choose characters that separate different parts of column names",
        checkboxGroupInput(
          ns("cond_sep_chars_autoassign_dw"),
          "Separators:",
          choices = c(
            "." = "\\.",
            ":" = ":",
            "," = ",",
            "space" = "\\s+",
            "underscore" = "_",
            "-" = "-",
            "(" = "\\(",
            ")" = "\\)"
          ),
          selected = c("\\(", "\\)")
        )
      )
    ),

    fluidRow(
      column(6,
             div(
               title = "Modify only the currently selected condition Rule ID",
               actionButton(
                 ns("add_condition_rule_autoassign_dw"),
                 "Modify Condition Rule",
                 width = "100%",
                 class = "btn-primary"
               )
             )),
      column(6,
             div(
               title = "Remove only the currently selected condition Rule ID",
               actionButton(ns("remove_condition_rule_autoassign_dw"),
                            "Remove Sample Rule",
                            width = "100%",
                            class = "btn-default",
                            style = "background-color: #3498db; border-color: #3498db; color: #fff;")
             ))
    ),
    br(),
    div(
      title = "View and manage active condition extraction rules",
      DTOutput(ns("condition_rules_table_autoassign_dw"))
    ),
    br(),
    fluidRow(
      column(
        12,
        div(
          title = paste(
            "Create another condition extraction rule",
            "for the selected content variant with a new user Rule ID."
          ),
          actionButton(
            ns("add_new_condition_rule_autoassign_dw"),
            "Add New Condition Rule",
            width = "100%",
            class = "btn-primary"
          )
        )
      )
    )
  )
}

############
# Ratio Analysis Rules UI Section

#' Create ratio analysis rules UI section
#' @param ns namespace function
#' @return UI div element
create_ratio_rules_ui <- function(ns) {
  wellPanel(
    h4("Define Ratio Analysis Rules"),
    p("Create rules to extract numerator and denominator information for ratio calculations."),
    div(
      class = "alert alert-info",
      style = "font-size: 0.9em;",
      HTML(paste0(
        "<strong>How it works:</strong> Ratio columns embed both a numerator and a denominator ",
        "condition in the column name. These rules define how the assistant locates each component.",
        "<br/><br/>",
        "<strong>Example:</strong> For a column such as ",
        "<em>Abundance Ratio p-Value: (Case) / (Control)</em>, the ",
        "<em>Regular Expressions</em> method uses patterns placed before and after each condition. ",
        "For the numerator <em>Case</em>, the preceding text is <em>p-Value: (</em> and the following text is <em>) / (</em>. ",
        "Likewise, for the denominator <em>Control</em>, the preceding text is <em>) / (</em> and the following text is <em>)$</em>. ",
        "Anchors like <em>^</em> (start of string) and <em>$</em> (end of string) ",
        "help constrain matching to the correct position within the name. ",
        "The <em>Pattern Recognition</em> and <em>Position in String</em> methods offer simpler ",
        "alternatives for common naming conventions. Refer to the documentation for a full ",
        "description of all available strategies."
      ))
    ),
    fluidRow(
      column(12,
             conditionalPanel(
               condition = sprintf("input['%s'] == 'Regular Expressions'", ns("new_method_autoassign_dw")),
               div(
                 style = "margin-bottom: 15px; padding: 10px; border: 1px solid #ddd; background-color: #f9f9f9;",
                 div(
                   title = "When enabled, plain text inputs will be automatically converted to regex patterns. When disabled, you can enter raw regex patterns directly.",
                   checkboxInput(ns("auto_convert_regex_dw"),
                                 "Auto-convert text to regex patterns",
                                 value = TRUE)
                 ),
                 div(
                   style = "font-size: 0.85em; color: #666; margin-top: 5px;",
                   conditionalPanel(
                     condition = sprintf("input['%s'] == true", ns("auto_convert_regex_dw")),
                     "Text inputs will be automatically escaped for regex use"
                   ),
                   conditionalPanel(
                     condition = sprintf("input['%s'] == false", ns("auto_convert_regex_dw")),
                     "Enter raw regex patterns directly (advanced users only)"
                   )
                 )
               )
             )
             )
    ),
    fluidRow(
      column(4,
             div(
               title = "Select the type of ratio analysis content this rule applies to",
               selectInput(
                 ns("new_content_autoassign_dw"),
                 "Ratio Content Type:",
                 choices = c("Abundance Ratio", "Abundance Ratio p-Value", "Abundance Ratio Adj. p-Value")
               ))),
      column(
        3,
        div(
          title = paste(
            "Stable identity of the currently selected ratio analysis rule.",
            "Ratio application itself remains scoped by VariantId."
          ),
          shiny::uiOutput(
            ns(
              "ratio_rule_id_display_autoassign_dw"
            )
          )
        )
      ),
      column(6,
             div(
               title = "Choose the method for extracting ratio components from column names",
               selectInput(
                 ns("new_method_autoassign_dw"),
                 "Extraction Method:",
                 choices = c("Pattern Recognition", "Regular Expressions", "Position in String")
               )))
    ),

    # Position-based extraction inputs
    conditionalPanel(
      condition = sprintf("input['%s'] == 'Position in String'", ns("new_method_autoassign_dw")),
      fluidRow(
        column(6,
               div(
                 title = "Position of numerator when column name is split by separators",
                 numericInput(ns("new_num_pos_autoassign_dw"),
                              "Numerator Position:",
                              value = 1,
                              min = 1)
               )),
        column(6,
               div(
                 title = "Position of denominator when column name is split by separators",
                 numericInput(ns("new_den_pos_autoassign_dw"),
                              "Denominator Position:",
                              value = 2,
                              min = 1)
               ))
      )
    ),

    # Separator selection for non-regex methods
    conditionalPanel(
      condition = sprintf("input['%s'] != 'Regular Expressions'", ns("new_method_autoassign_dw")),
      div(
        class = "fourcols",
        div(
          title = "Choose separators for splitting column names into components",
          checkboxGroupInput(
            ns("new_sep_chars_autoassign_dw"),
            "Separators:",
            choices = c(
              "." = "\\.",
              ":" = ":",
              "," = ",",
              "space" = "\\s+",
              "underscore" = "_",
              "-" = "-",
              "(" = "\\(",
              ")" = "\\)"
            ),
            selected = c("\\(", "\\)")
          )
        )
      )
    ),

    # Invert option for pattern recognition
    conditionalPanel(
      condition = paste(
        sprintf("input['%s'] != 'Regular Expressions'", ns("new_method_autoassign_dw")),
        sprintf("input['%s'] != 'Position in String'", ns("new_method_autoassign_dw")),
        sep = " && "
      ),
      div(
        title = "Swap the automatically detected numerator and denominator positions",
        checkboxInput(ns("new_invert_autoassign_dw"),
                      "Invert Numerator/Denominator",
                      value = FALSE)
      )
    ),

    # Regular expression inputs
    conditionalPanel(
      condition = sprintf("input['%s'] == 'Regular Expressions'", ns("new_method_autoassign_dw")),
      fluidRow(
        column(3,
               div(
                 title = "Regex pattern that appears before the numerator",
                 textInput(ns("new_num_before_autoassign_dw"),
                           "Numerator Before:",
                           placeholder = "e.g., Ratio_")
               )),
        column(3,
               div(
                 title = "Regex pattern that appears after the numerator",
                 textInput(ns("new_num_after_autoassign_dw"),
                           "Numerator After:",
                           placeholder = "e.g., _vs")
               )),
        column(3,
               div(
                 title = "Regex pattern that appears before the denominator",
                 textInput(ns("new_den_before_autoassign_dw"),
                           "Denominator Before:",
                           placeholder = "e.g., vs_")
               )),
        column(3,
               div(
                 title = "Regex pattern that appears after the denominator",
                 textInput(ns("new_den_after_autoassign_dw"),
                           "Denominator After:",
                           placeholder = "e.g., _ratio")
               ))
      )
    ),

    fluidRow(
      column(6,
             div(
               title = "Modify only the currently selected ratio Rule ID",
               actionButton(
                 ns("add_rule_autoassign_dw"),
                 "Modify Ratio Rule",
                 width = "100%",
                 class = "btn-primary"
               )
             )),
      column(6,
             div(
               title = "Remove only the currently selected ratio Rule ID",
               actionButton(ns("remove_rule_autoassign_dw"),
                            "Remove Ratio Rule",
                            width = "100%",
                            class = "btn-default",
                            style = "background-color: #3498db; border-color: #3498db; color: #fff;")
             ))
    ),
    br(),

    div(
      title = "View and manage active ratio analysis rules",
      DTOutput(ns("rules_table_ratio_autoassign_dw"))
    ),
    br(),
    fluidRow(
      column(
        12,
        div(
          title = paste(
            "Create another ratio analysis rule",
            "for the selected content variant with a new user Rule ID."
          ),
          actionButton(
            ns("add_new_ratio_rule_autoassign_dw"),
            "Add New Ratio Rule",
            width = "100%",
            class = "btn-primary"
          )
        )
      )
    )
  )
}

############
# Module-Level UI Composition

build_auto_assign_modal_ui <- function(ns) {
  modalDialog(
    title = "Auto-Assign Assistant",
    size = "l",
    easyClose = TRUE,
    footer = modalButton("Close"),
    tags$style(HTML(
      "#shiny-modal .modal-dialog.modal-lg { width: 1240px; max-width: 95vw; }"
    )),
    tabsetPanel(
      tabPanel("Auto RegEx",               modAutoRegexUI(ns("auto_regex"))),
      tabPanel("Content Rules",            create_content_rules_ui(ns)),
      tabPanel("Condition Extraction Rules", create_sample_rules_ui(ns)),
      tabPanel("Ratio Analysis Rules",     create_ratio_rules_ui(ns)),
      tabPanel("Export",                   create_template_management_ui(ns))
    )
  )
}

build_auto_assign_module_ui <- function(id) {
  ns <- NS(id)

  div(
    class = "well-panel",
    # h4("Auto-Assign Assistant"),
    actionButton(
      ns("open_auto_assign_modal"),
      "Open Auto-Assign Assistant",
      class = "btn btn-default",
      style = "width: 100%;"
    )
  )
}
