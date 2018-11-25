# RcppRoll 0.3.0

- Properly document the `align` argument -- the function accepts
  "center" rather than "middle". (#28)

- Fixed an issue where empty fills were not handled correctly.

- The interface has now been standardized such that each implemented window
  function has version center-aligned by default (e.g. `roll_mean()`), a
  left-aligned version (`roll_meanl()`), and right-aligned version
  (`roll_meanr()`).

- Implement rolling window functions for `mean()`, `median()`, `min()`,
  `max()`, `prod()`, `sum()`, `sd()` and `var()`.

