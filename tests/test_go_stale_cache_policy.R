# Focused base-R checks for the advisory GO cache-age policy. Run with Rscript.
source("modules/GO/GO_module_hub_cache.R")

stopifnot(identical(resolve_go_stale_cache_policy(6, cache_usable = TRUE), "normal"))
stopifnot(identical(resolve_go_stale_cache_policy(7, cache_usable = TRUE), "normal"))
stopifnot(identical(resolve_go_stale_cache_policy(7.1, cache_usable = TRUE), "prompt"))
stopifnot(identical(resolve_go_stale_cache_policy(8, "use_old", TRUE), "use_stale"))
stopifnot(identical(resolve_go_stale_cache_policy(8, "update", TRUE), "refresh"))
# A prior decision suppresses another prompt for that organism.
stopifnot(!identical(resolve_go_stale_cache_policy(8, "use_old", TRUE), "prompt"))
# A different organism has no decision and is evaluated independently.
stopifnot(identical(resolve_go_stale_cache_policy(8, NULL, TRUE), "prompt"))
# Missing/corrupt SQLite caches are never made valid by their age or a decision.
stopifnot(identical(resolve_go_stale_cache_policy(8, "use_old", FALSE), "normal"))
stopifnot(identical(resolve_go_stale_cache_policy(Inf, "use_old", FALSE), "normal"))
