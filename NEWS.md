# tf 0.3.5
* more `vctrs`-compliance: use `vec_arith` for group generics (and fix `tfb` bugs),
   use `vctrs`-utils in `[.tf`. 
* use `cli` for user communication 

# tf 0.3.4
* bug fix: normalize tf_crosscov correctly

# tf 0.3.3
* tf_rebase behaves more consistently on (irregular) data with different lengths
* avoid CRAN issues by streamlining tfb_fpc example and skipping tf_rebase tests
