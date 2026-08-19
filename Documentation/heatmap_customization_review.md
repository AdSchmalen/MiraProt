# Heatmap customization documentation review

## Scope

This review covers the current user-facing customization chapter and the Heatmap
module controls and behavior that the eventual replacement chapter must describe:

- `Documentation/heatmap_doc_user.R`
- `modules/Heatmap/Heatmap_ui.R`
- `modules/Heatmap/Heatmap_observers.R`
- `modules/Heatmap/Heatmap_creation.R`
- `modules/Heatmap/Heatmap_create_expression.R`
- `modules/Heatmap/Heatmap_rendering.R`

## Review outcome

The existing chapter is organized mainly by control category and includes more
implementation detail than a user needs. A future revision should instead help
readers move from a scientific or presentation goal to the relevant control and
its visible result. It must also distinguish controls shared across heatmaps from
options available only for expression, correlation, Basemean, or abundance-ratio
views.

The reviewed behavior also requires clearer guidance about timing: after the
first plot is created, eligible changes can update the active heatmap, while
large plots defer updates until the user creates the plot again. Reset restores
control defaults; the user may still need to recreate a deferred plot to see
those defaults applied.

This is a review record only. The proposed chapter structure and wording are
intentionally maintained in the pull-request description until the documentation
revision is implemented.
