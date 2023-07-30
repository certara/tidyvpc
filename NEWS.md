# tidyvpc 1.5.0
* Plotting updates were made for ggplot2 version 3.4.0 to use `linewidth` instead of `size` for lines[#39](https://github.com/certara/tidyvpc/issues/39).
* `simulated.tidyvpcobj()` detects if the number of simulated rows is not an integer multiple of the number of observed rows and adds the new `xsim` argument to test that x values match between replicated simulations.  It will suggest that MDV filtering may not have occurred if either of these fails [#35](https://github.com/certara/tidyvpc/issues/35).
* Prevent division by zero in `predcorrect()` transformation [#31](https://github.com/certara/tidyvpc/issues/31).
* Usability enhancements for prediction corrected VPC (pcVPC), which include support for `binning.tidyvpcobj()` either before or after usage of `predcorrect.tidyvpcobj()`, and automatically performing LOESS pcVPC when `binless.tidyvpcobj()` is used. As a result, the `loess.ypc` argument is no longer required[#43](https://github.com/certara/tidyvpc/issues/43).

# tidyvpc 1.4.0
* Fix for npde calculation fix npde calc [#16](https://github.com/certara/tidyvpc/pull/16)
* Added R CMD check and test coverage workflows [#18](https://github.com/certara/tidyvpc/pull/18)
* Support `headtails`, `maximum`, and `box` binning methods [#23](https://github.com/certara/tidyvpc/pull/23)
* Usage of `predcorrect()` may now occur either before or after call to `binless(loess.ypc=TRUE)`
* Additional unit tests
* A vignette for use with `nlmixr2` was added (#27)

# tidyvpc 1.3.0
* Additional arguments in `plot()` function now include:
  - `point.size`
  - `point.stroke`
  - `point.shape`
  - `point.alpha`
  - `ribbon.alpha`
* Fixed plotting [issue](https://github.com/certara/tidyvpc/issues/11) to restore `facet_wrap()` usage for one-side stratification formula

# tidyvpc 1.2.0
* Add support for categorical VPC using binless and binning methods
* Update `plot()` function to include plotting methods for categorical VPC

# tidyvpc 1.1.0
* Add NPDE functionality
* Add unit tests
* Bug fix for plotting of censored observations
* Maintain forward compatibility with `quantreg`

# tidyvpc 1.0.0
* Initial CRAN release
* Add `binless()` vpc functionality to existing code in [`vpcstats`](https://github.com/benjaminrich/vpcstats)
