# Optimizing `step_word_embeddings`

Goal: speed up the `bake()` path of `step_word_embeddings()`, which is driven by
`tokenlist_embedding()` in `R/tokenlist.R`.

Benchmark setup (`bench/bench_setup.R`): 50,000-token vocab, 100 embedding
dimensions, 5,000 documents of 10-30 tokens each.

## Baseline

`tokenlist_embedding()` slices the embedding tibble by row, then runs a dplyr
`group_by()` + `summarise_all()` pipeline:

```r
emb[token_index, -1] |>
  dplyr::mutate("id" = split_id) |>
  dplyr::filter(!is.na(token_index)) |>
  dplyr::group_by(id, .drop = FALSE) |>
  dplyr::summarise_all(fun, na.rm = TRUE) |>
  dplyr::select(-"id")
```

Profiling (`bench/bench_setup.R`, default `sum` aggregation):

- `bake()` elapsed: ~0.85 s
- Self-time dominated by vctrs slicing (`ffi_slice`, `ffi_vec_chop`) of the
  100-column tibble and dplyr group/summarise machinery, plus heavy GC.
- Memory: ~140 MB allocated in `vec_slice` alone.

Root causes:
1. Slicing a 100-column **tibble** by row is far slower than slicing a matrix.
2. The dplyr group/summarise pipeline has high per-call overhead and allocates a
   lot.

## Iteration 1: matrix + `rowsum` instead of the dplyr pipeline

Rewrote `tokenlist_embedding()` to:

- Convert the embedding tibble to a numeric **matrix** once and slice rows from
  it (matrix row-slicing is far cheaper than tibble slicing).
- Pass the `aggregation` name and `aggregation_default` through instead of an
  opaque closure, so the aggregation can be vectorised.
- Aggregate `sum`/`mean` with base `rowsum()` (a single C call), dividing by
  group counts for `mean`. `min`/`max` initially aggregated with a per-group
  `apply()`.

`get_aggregation_fun()` was removed (no longer needed).

Results (per-run `bake`, 3 runs):

| aggregation | before | after |
|-------------|--------|-------|
| sum         | 0.85 s | 0.066 s (~13x) |
| mean        | 0.85 s | 0.067 s (~13x) |
| min         | 0.85 s | 0.378 s (~2.2x) |
| max         | 0.85 s | 0.379 s (~2.2x) |

Output verified identical to the documented example for all four aggregations.
`min`/`max` lagged because the per-group `apply()` loops over thousands of
documents.

## Iteration 2: vectorise `min`/`max` over columns

Replaced the per-group `apply()` with a loop over the ~100 embedding columns,
each aggregated by a single `tapply()`. Far fewer iterations (columns, not
documents).

| aggregation | iter 1 | iter 2 |
|-------------|--------|--------|
| min         | 0.378 s | 0.360 s |
| max         | 0.379 s | 0.338 s |

Modest further gain. `min`/`max` remain bounded by `tapply`'s internal grouping;
the default `sum`/`mean` path (the common case) is the headline ~13x win.

## Iteration 3: sparse incidence matrix product for sum/mean

Re-profiling the `sum` path showed `rowsum.default` (~39% self-time) and the
`emb_mat[token_index, ]` row-slice (~14%) as the new hot spots, with heavy GC
(~22%) from the 80k x 100 slice allocation each bake.

Replaced slice + `rowsum` with a single **sparse-by-dense matrix product**:

```r
incidence <- Matrix::sparseMatrix(i = doc_id, j = token_index, x = 1,
                                  dims = c(n_doc, nrow(emb_mat)))
out <- as.matrix(incidence %*% emb_mat)            # per-document sums, in order
if (aggregation == "mean") out <- out / counts
out[counts == 0, ] <- aggregation_default          # empty docs get the default
```

The **embedding matrix stays dense**; only the document-token incidence matrix
is sparse, and it is sparse by construction (each document contains only a few
of the vocabulary's tokens) no matter what the embedding values are. The product
lands directly in document order, so it needs no row-slice, no grouped
reduction, and no remapping of group labels back to rows. `min`/`max` keep the
column-wise `tapply` path. `Matrix` is already a package dependency.

| aggregation | iter 2 | iter 3 |
|-------------|--------|--------|
| sum         | 0.066 s | 0.045 s |
| mean        | 0.067 s | 0.038 s |
| min         | 0.360 s | 0.342 s |
| max         | 0.338 s | 0.336 s |

After this change the remaining `bake` time is dominated by `step_tokenize`
(`stri_split_boundaries`, `stri_trans_tolower`) and the intrinsic `%*%`
multiply-add work, i.e. there is little left to win inside
`step_word_embeddings` itself for the default aggregations.

## Cumulative result

| aggregation | baseline | final | speedup |
|-------------|----------|-------|---------|
| sum         | 0.85 s | 0.045 s | ~19x |
| mean        | 0.85 s | 0.038 s | ~22x |
| min         | 0.85 s | 0.342 s | ~2.5x |
| max         | 0.85 s | 0.336 s | ~2.5x |

## Notes

- The full `devtools::test()` run segfaults inside an unrelated `ngram` C-code
  test under `load_all`; this reproduces on a clean tree (pre-existing, not
  caused by these changes). The `word_embeddings` tests pass (40 PASS).

