#!/usr/bin/env Rscript

library(testthat)

source("modules/Data Wizard/auto regex/datawizard_auto_regex_utils.R")
source("modules/Data Wizard/auto regex/datawizard_auto_regex_logic.R")

description_rows <- function(headers, labels) data.frame(
  Column=headers, Content=labels, Options="", Numerator="", Denominator="",
  Transformation="", Sample="", stringsAsFactors=FALSE, check.names=FALSE)

test_that("canonical technical Description header receives a singleton fallback", {
  rows <- description_rows(
    c("PG.ProteinDescriptions","PG.ProteinAccessions","PG.ProteinNames",
      "ProteinDescriptions","PG.ProteinDescription","PG.GeneDescriptions"),
    c("Description","Identifier","Protein Name","Other","Annotation",
      "Gene Symbol"))
  inferred <- auto_regex_infer_rules(rows)
  replay <- apply_content_table(rows,inferred$rules$table)$metadata

  expect_identical(replay$Content[[1L]],"Description")
  expect_false(any(nzchar(replay$Content[3:6])))
  expect_identical(sum(inferred$rules$table$Content=="Description"),1L)
  diagnostic <- inferred$diagnostics$technical_singleton_fallback
  expect_true(all(diagnostic$FallbackType=="technical_singleton_fallback"))
  expect_false(any(diagnostic$Grouped))
  expect_false(any(diagnostic$EvidenceBased))
  expect_false(any(diagnostic$RejectedByAuthoritativeNonDescription))

  stored <- technical_description_fallback_pattern()
  runtime <- regex_from_miraprot_storage(stored,"content")
  expect_true(validate_pcre(runtime)$valid)
  expect_identical(regex_to_miraprot_storage(runtime,"content"),stored)
  persisted <- unserialize(serialize(inferred$rules,NULL,version=2L))
  expect_identical(serialize(apply_content_table(rows,inferred$rules$table)$metadata,
    NULL,version=2L),serialize(apply_content_table(rows,persisted$table)$metadata,
    NULL,version=2L))
})

test_that("technical fallback abstains on an authoritative conflicting label", {
  rows <- description_rows(c("PG.ProteinDescriptions","OtherHeader"),
    c("Gene Symbol","Identifier"))
  inferred <- auto_regex_infer_rules(rows)
  replay <- apply_content_table(rows,inferred$rules$table)$metadata
  diagnostic <- inferred$diagnostics$technical_singleton_fallback

  expect_false(any(inferred$rules$table$Content=="Description"))
  expect_false(any(replay$Content=="Description"))
  expect_identical(sum(diagnostic$RejectedByAuthoritativeNonDescription),1L)
  expect_false(any(diagnostic$Applied))
})
