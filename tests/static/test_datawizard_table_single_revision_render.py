from pathlib import Path


OBSERVER = Path("modules/Data Wizard/tables/datawizard_tables_observer.R")


def test_primary_snapshot_is_driven_by_revision_not_eager_data_publication():
    source = OBSERVER.read_text(encoding="utf-8")

    assert "revision <- primary_working_revision_debounced()" in source
    assert "published_revision <- tryCatch(" in source
    assert "isolate(rv$datawizard_data_revision_id)" in source
    assert "req(as.integer(revision) >= as.integer(published_revision))" in source
    assert "df <- isolate(get_current_primary_df())" in source
    assert "df       <- req(get_current_primary_df())" not in source


def test_dt_is_not_updated_by_render_and_proxy_for_the_same_revision():
    source = OBSERVER.read_text(encoding="utf-8")

    assert 'output[[local_output_id]] <- renderDT({' in source
    assert "DT::replaceData(primary_table_proxy" not in source
    assert "DT::replaceData(additional_table_proxy" not in source


def test_empty_startup_snapshot_does_not_raise_an_error_notification():
    source = OBSERVER.read_text(encoding="utf-8")

    empty_guard = source.split("if (snapshot$n_cols == 0L) {", 1)[1].split("}", 1)[0]
    assert "showNotification" not in empty_guard
    assert "return(NULL)" in empty_guard


def test_complete_filter_source_has_a_bounded_initial_preview():
    source = OBSERVER.read_text(encoding="utf-8")

    assert source.count("ensure_unique_preview_names(snapshot$complete_frame") == 2
    assert source.count("}, server = TRUE)") == 2
    assert source.count("DATAWIZARD_TABLE_PAGE_LENGTH") >= 2
    assert '"Preview is truncated' not in source
    assert 'paste0("Only the first "' not in source
    assert "list(primary_working_revision_debounced(), primary_show_full_table())" in source
    assert "list(secondary_revision_debounced(), additional_show_full_table())" in source

    primary_render = source.split("output[[local_output_id]] <- renderDT({", 1)[1]
    primary_render = primary_render.split("# Metadata edits do not change", 1)[0]
    additional_render = source.split("output[[local_output_id]] <- renderDT({", 2)[2]
    additional_render = additional_render.split(
        "observeEvent(input$primary_table_browser_ready", 1
    )[0]
    assert "Only the first" not in primary_render
    assert "Only the first" not in additional_render
    assert source.count("pagination_callback = JS(build_datawizard_pagination_callback") == 2
    assert source.count('"Enable full-table interaction"') == 2


def test_row_removal_prefers_dt_server_side_canonical_selection_indexes():
    source = OBSERVER.read_text(encoding="utf-8")

    assert source.count('paste0(isolate(primary_table_output_id()), "_rows_selected")') == 1
    assert source.count('paste0(isolate(additional_table_output_id()), "_rows_selected")') == 1
    assert source.count("resolve_canonical_row_identity_position") == 2
