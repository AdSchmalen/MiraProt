from pathlib import Path


UI_SOURCE = Path("modules/GSEA/GSEA_ui.R").read_text(encoding="utf-8")
OBSERVER_SOURCE = Path("modules/GSEA/GSEA_module_observer.R").read_text(encoding="utf-8")


def test_gsea_gene_set_control_has_file_and_empty_states():
    helper_start = UI_SOURCE.index("gsea_gene_set_file_control_ui <- function")
    helper_end = UI_SOURCE.index("GSEA_ui_definition <- function", helper_start)
    helper = UI_SOURCE[helper_start:helper_end]

    assert 'ns("fileSelector_GSEA")' in helper
    assert 'class = "alert alert-info"' in helper
    assert "No gene set files are available." in helper
    assert "./GSEA/" in helper
    assert "./shiny-app/GSEA/" in helper
    assert "https://www.gsea-msigdb.org/gsea/msigdb/" in helper
    assert "Refresh Gene Sets" in helper


def test_gsea_gene_set_control_reacts_to_refreshed_gmt_choices():
    assert 'uiOutput(ns("geneSetFileControl_GSEA"))' in UI_SOURCE
    assert "output$geneSetFileControl_GSEA <- renderUI({" in OBSERVER_SOURCE
    assert 'gmt_file_choices <- reactiveVal(gsea_list_gmt_files("./GSEA"))' in OBSERVER_SOURCE
    assert "gsea_gene_set_file_control_ui(session$ns, files, selected_file)" in OBSERVER_SOURCE
