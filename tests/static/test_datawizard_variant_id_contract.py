from pathlib import Path

ROOT = Path(__file__).parents[2]
UTILS = (ROOT / "modules/Data Wizard/auto regex/datawizard_auto_regex_utils.R").read_text()
ENGINE = (ROOT / "modules/Data Wizard/auto assign/datawizard_auto_assign_rule_engine.R").read_text()
TEMPLATE = (ROOT / "modules/Data Wizard/auto assign/datawizard_auto_assign_template_pipeline.R").read_text()


def test_variant_identity_and_priority_are_canonical_contract_fields():
    assert 'CONTENT_FIELDS <- c("Content", "VariantId", "Priority"' in UTILS
    assert 'CONDITION_FIELDS <- c("Content", "VariantId"' in UTILS
    assert 'RATIO_FIELDS <- c("Content", "VariantId"' in UTILS
    assert 'gsub("[^[:alnum:]]+", "-", key)' in UTILS
    assert "Include" not in UTILS[UTILS.index("stable_variant_ids <-"):UTILS.index("upgrade_rule_component <-")]


def test_execution_routes_downstream_rules_by_winning_variant():
    assert 'attr(out,"variant_id") <- winning_variant' in UTILS
    assert 'chr(variants) == table$VariantId[i]' in UTILS
    assert 'variants == rule$VariantId' in ENGINE
    assert 'table_rules[order(table_rules$Priority' in ENGINE


def test_template_import_upgrades_and_persists_identity():
    assert 'upgrade_rule_component(rules_data$table, "content")' in TEMPLATE
    assert 'c("Content", "VariantId", "Priority"' in TEMPLATE
