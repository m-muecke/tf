# tf 0.3.5
* fix broken `tfb`-arithmetic
* more `vctrs`-compliance: use `vec_arith` for group generics,
   use `vctrs`-utils in `[.tf`. 
* use `cli` for user communication & export `format_glimpse` dynamically  

# tf 0.3.4
* bug fix: normalize tf_crosscov correctly

# tf 0.3.3
* tf_rebase behaves more consistently on (irregular) data with different lengths
* avoid CRAN issues by streamlining tfb_fpc example and skipping tf_rebase tests
