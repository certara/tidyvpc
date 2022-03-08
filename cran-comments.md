## Release summary

This is a minor release that provides additional arguments to control aesthetics in the plot() function. In addition, an [issue](https://github.com/certara/tidyvpc/issues/11) has been fixed where VPC's with one-sided statification formulas once again use `facet_wrap()` and two-sided stratification formulas use `facet_grid()`.

Additional arguments to the `plot()` function include:

* `point.size`
* `point.stroke`
* `point.shape`
* `point.alpha`
* `ribbon.alpha`

## Test environments

* Windows 10 Enterprise, R 4.1.1
* Red Hat Enterprise Linux 7, R 4.0.3

## R CMD check results

0 errors v | 0 warnings v | 0 notes v

R CMD check succeeded


