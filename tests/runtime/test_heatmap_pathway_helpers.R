# Focused pure-helper coverage; no Shiny runtime is required.
heatmap_debug_log <- function(...) invisible(NULL)
source("modules/Heatmap/Heatmap_utils.R", local = TRUE)

stopifnot(nrow(heatmap_normalize_gsea_result(NULL)) == 0L)
gsea <- data.frame(Description = c("Path A", "Path B", "Path A"), ID = c("a", "b", "a2"),
                   core_enrichment = c("P1/P2; P2", "P2, P3\nP4", NA))
stopifnot(identical(heatmap_normalize_gsea_result(list(Results = gsea)), gsea))
stopifnot(identical(heatmap_pathway_choices(gsea, TRUE), c("Path A", "Path B")))
stopifnot(identical(heatmap_pathway_choices(data.frame(ID=c("x", "x", "")), TRUE), "x"))

go <- data.frame(Description = c("GO A", "GO B"), gene_id = c("P1; P2", "P2/P3"))
stopifnot(identical(heatmap_normalize_go_result(list(go)), go))
stopifnot(identical(heatmap_extract_pathway_proteins(go, "stale", "go"), list()))
groups <- heatmap_extract_pathway_proteins(go, c("GO A", "GO B"), "go")
stopifnot(identical(heatmap_combine_pathway_proteins(groups), c("P1", "P2", "P3")))
stopifnot(identical(heatmap_combine_pathway_proteins(groups, TRUE), "P2"))
for (column in c("geneID", "core_enrichment", "gene_id", "genes")) {
  alternate <- data.frame(Description="term"); alternate[[column]] <- " A/B; C, A\nD "
  stopifnot(identical(heatmap_extract_pathway_proteins(alternate, "term", "go")[[1]], c("A","B","C","D")))
}
stopifnot(identical(heatmap_merge_identifier_proteins("OLD, P1\nOLD", c("P1", "P2")), c("OLD", "P1", "P2")))
