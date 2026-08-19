# ./Documentation/datawizard_doc_tech.R
# Canonical repository path: Documentation/datawizard_doc_tech.R
# Datawizard Documentation — Technical Addendum (Primary/Secondary table edits)
#
# This addendum documents row/column removal behavior in Datawizard tables.

render_data_tables_tech_content <- function() {
  div(
    h2("Data Tables — Technical Notes"),
    hr(),
    p(
      "Row and column removal is a structural change when applied to Primary data. ",
      "After such a change, Integration writes the updated table back to the shared data bus and refreshes metadata ",
      "so ", code("rv$data_def"), " and ", code("core_values$handson_metadata"), " remain aligned with current Primary columns."
    ),
    tags$ul(
      tags$li(
        strong("Primary data edits (row/column removal): "),
        "trigger metadata synchronization because metadata is defined for Primary columns."
      ),
      tags$li(
        strong("Additional/Secondary data edits: "),
        "do not trigger metadata updates, because there is no metadata model for Secondary data in Core."
      ),
      tags$li(
        strong("Data flow impact: "),
        "downstream modules reading Primary data + metadata see the updated structure immediately after sync."
      ),
      tags$li(
        strong("Metadata table edits: "),
        "in automatic mode, downstream modules see metadata changes immediately after sync; in paused mode, downstream modules continue using the last synchronized metadata until explicit synchronization."
      )
    )
  )
}
