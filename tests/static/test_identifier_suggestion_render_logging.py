from pathlib import Path

import pytest


ROOT = Path(__file__).resolve().parents[2]


def _render_body(path: str, output_name: str) -> str:
    source = (ROOT / path).read_text(encoding="utf-8")
    marker = f"output${output_name} <- renderPrint({{"
    start = source.index(marker)
    pos = start + len(marker)
    depth = 1
    while depth:
        if source[pos] == "{":
            depth += 1
        elif source[pos] == "}":
            depth -= 1
        pos += 1
    return source[start:pos]


@pytest.mark.parametrize(
    ("path", "output_name", "helper", "noisy_logger"),
    [
        (
            "modules/PCA/pca_module_server_observers.R",
            "geneSymbolList_pca",
            "get_filter_string_pca",
            "debug_log",
        ),
        (
            "modules/dot/dotplot_server_interaction.R",
            "geneSymbolList_dot",
            "get_filter_string_dot",
            "dotplot_debug_log",
        ),
        (
            "modules/dot/dotplot_server_config.R",
            "geneSymbolList_dot",
            "get_filter_string_dot",
            "dotplot_debug_log",
        ),
        (
            "modules/Heatmap/Heatmap_observers_protein_selection.R",
            "geneSymbolList_Heatmap",
            "get_filter_string_Heatmap",
            "heatmap_debug_log",
        ),
    ],
)
def test_identifier_parser_logging_is_quiet_inside_render(
    path: str, output_name: str, helper: str, noisy_logger: str
) -> None:
    body = _render_body(path, output_name)
    assert "quiet_log <- function(...) invisible(NULL)" in body
    assert f"{helper}(" in body
    helper_call = body[body.index(f"{helper}(") :]
    helper_call = helper_call[: helper_call.index(")") + 1]
    assert "quiet_log" in helper_call
    assert noisy_logger not in helper_call


def test_dot_interaction_registration_is_the_effective_duplicate() -> None:
    module = (ROOT / "modules/dotplot_module.R").read_text(encoding="utf-8")
    config = module.index('sys.source("modules/dot/dotplot_server_config.R"')
    interaction = module.index('sys.source("modules/dot/dotplot_server_interaction.R"')
    assert config < interaction


def _suggestions(query: str, identifiers: list[str]) -> str:
    # Mirrors the renderers' newline parsing and partial-match output contract.
    tokens = [line.strip().split(",")[0].split()[0] for line in query.splitlines() if line.strip()]
    matches = [value for value in identifiers if any(token.lower() in value.lower() for token in tokens)]
    return "\n".join(dict.fromkeys(matches))


@pytest.mark.parametrize(
    ("input_text", "expected"),
    [
        ("TP53", "TP53"),
        ("TP53\nEGFR", "TP53\nEGFR"),
        ("", ""),
        pytest.param("TP53\nEGFR", "TP53\nEGFR", id="restored-text"),
        pytest.param("EGFR\nAKT1", "EGFR\nAKT1", id="transferred-text"),
    ],
)
def test_suggestion_text_contains_identifiers_only(input_text: str, expected: str) -> None:
    rendered = _suggestions(input_text, ["TP53", "EGFR", "AKT1"])
    assert rendered == expected
    assert "DEBUG:" not in rendered
    assert "[PCA]" not in rendered
    assert "[Dotplot]" not in rendered
    assert "[Heatmap]" not in rendered
    assert "[STRING]" not in rendered


def test_string_render_routes_all_diagnostics_to_quiet_logger() -> None:
    body = _render_body(
        "modules/STRING/STRING_server_interactions.R", "geneSymbolList_STRING"
    )
    assert "quiet_log <- function(...) invisible(NULL)" in body
    assert "debug_log(" not in body
    assert body.count("quiet_log(") == 4


def test_heatmap_suggestions_react_while_the_output_is_hidden() -> None:
    source = (
        ROOT / "modules/Heatmap/Heatmap_observers_protein_selection.R"
    ).read_text(encoding="utf-8")
    body = _render_body(
        "modules/Heatmap/Heatmap_observers_protein_selection.R",
        "geneSymbolList_Heatmap",
    )

    assert (
        'outputOptions(output, "geneSymbolList_Heatmap", suspendWhenHidden = FALSE)'
        in source
    )
    assert "heatmap_debug_log(" not in body


def test_heatmap_filter_parser_uses_its_injected_logger() -> None:
    source = (ROOT / "modules/Heatmap/Heatmap_utils.R").read_text(encoding="utf-8")
    parser = source.split("get_filter_string_Heatmap <- function", 1)[1].split(
        "# Create protein annotation", 1
    )[0]

    assert parser.count("debug_log(") == 2
    assert "heatmap_debug_log(" not in parser
