# trendfilter (development version)

* Initial CRAN submission.
* add option to ues `sparse_cholesky` as a solver.

# trendfilter 0.0.2

- Add an option to standardize `y` (default `TRUE`). This improves computation
  significantly
- Move to x.x.x.9xxx version numbering.
- Add News
- `lambda` must be an argument to `cv_trendfilter()`. Otherwise, passing it 
  optionally fails to forward it correctly through `...` in the CV fits.
