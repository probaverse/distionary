# distplyr (0.2.0)

* GitHub: <https://github.com/probaverse/distplyr>
* Email: <mailto:vincenzo.coia@gmail.com>
* GitHub mirror: <https://github.com/cran/distplyr>

Run `revdepcheck::revdep_details(, "distplyr")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     > ### Name: Math.dst
     > ### Title: Mathematical Transformations for Distributions
     > ### Aliases: Math.dst
     > 
     > ### ** Examples
     > 
     > # Logarithmic transformations
     > d <- distionary::dst_unif(1, 10)
     > log(d)              # Natural log
     Error:
     ! The `.vtype` argument of `distribution()` was deprecated in distionary
       0.2.0 and is now defunct.
     ℹ Please use the `.support` argument instead.
     ℹ A variable type cannot stand in for a support: it says what kind of
       probability there is, not where it lives.
     ℹ Build one with `continuous()`, `discrete()`, or `mixed()`.
     Backtrace:
         ▆
      1. └─distplyr:::Math.dst(d)
      2.   └─distplyr:::log_distribution(x, ...)
      3.     └─distionary::distribution(...)
      4.       └─lifecycle::deprecate_stop(...)
      5.         └─lifecycle:::deprecate_stop0(msg)
      6.           └─rlang::cnd_signal(...)
     Execution halted
     ```

*   checking tests ...
     ```
       Running ‘testthat.R’
      ERROR
     Running the tests in ‘tests/testthat.R’ failed.
     Last 13 lines of output:
       i A variable type cannot stand in for a support: it says what kind of probability there is, not where it lives.
       i Build one with `continuous()`, `discrete()`, or `mixed()`.
       Backtrace:
           ▆
        1. ├─rlang::exec(verb, !!!case) at test-simplifications.R:141:7
        2. └─distplyr::mix(`<dst>`, `<dst>`, `<dst>`, weights = `<dbl>`)
        3.   └─distionary::distribution(...)
        4.     └─lifecycle::deprecate_stop(...)
        5.       └─lifecycle:::deprecate_stop0(msg)
        6.         └─rlang::cnd_signal(...)
       
       [ FAIL 32 | WARN 0 | SKIP 0 | PASS 244 ]
       Error:
       ! Test failures.
       Execution halted
     ```

*   checking running R code from vignettes ...
     ```
       ‘manipulating.Rmd’ using ‘UTF-8’... failed
      ERROR
     Errors in running code in vignettes:
     when running code in ‘manipulating.Rmd’
       ...
     > shift(d, 5)
     
       When sourcing ‘manipulating.R’:
     Error: The `.vtype` argument of `distribution()` was deprecated in distionary
     0.2.0 and is now defunct.
     ℹ Please use the `.support` argument instead.
     ℹ A variable type cannot stand in for a support: it says what kind of
       probability there is, not where it lives.
     ℹ Build one with `continuous()`, `discrete()`, or `mixed()`.
     Execution halted
     ```

