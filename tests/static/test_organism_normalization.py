#!/usr/bin/env python3
"""Static checks for shared organism normalization in key-type resolvers."""

from pathlib import Path
import re

ROOT = Path(__file__).resolve().parents[2]
HUB = ROOT / "modules" / "GO" / "GO_module_hub.R"
GO_OBSERVER = ROOT / "modules" / "GO" / "GO_module_observer_enrichment_cache.R"
ANNOTATION_OBSERVER = ROOT / "modules" / "Data Wizard" / "Annotation" / "datawizard_annotation_observer_general.R"

hub_text = HUB.read_text()
go_text = GO_OBSERVER.read_text()
annotation_text = ANNOTATION_OBSERVER.read_text()

assert "normalize_organism_name <- function" in hub_text, "Missing shared normalize_organism_name() helper"
assert 'gsub("[._]+", " ", organism_clean)' in hub_text, "Normalizer must collapse dotted/underscored labels"
assert 'return("Homo sapiens")' in hub_text, "Normalizer should use Homo sapiens as the empty/default canonical label"
assert re.search(
    r'organism_to_orgdb <- function\(organism_name\) \{\s+organism_clean <- normalize_organism_name\(organism_name\)',
    hub_text,
), "organism_to_orgdb() must normalize through the shared helper"

assert re.search(
    r'organism_display <- normalize_organism_name\(input\$OrgDb_GO\).*?KeyType: Processing organism:',
    go_text,
    flags=re.DOTALL,
), "GO key-type observer must normalize before logging/cache resolution"
assert re.search(
    r'organism_display <- normalize_organism_name\(organism_display\).*?KeyType \[src-token=%d\]: Processing organism',
    annotation_text,
    flags=re.DOTALL,
), "Annotation key-type observer must normalize before logging/cache resolution"

print("Organism normalization static checks passed")
