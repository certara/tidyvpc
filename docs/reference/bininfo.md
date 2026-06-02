# Obtain information about the bins from a `tidyvpcobj`

Obtain information about the bins from a `tidyvpcobj`

## Usage

``` r
bininfo(o, ...)

# S3 method for class 'tidyvpcobj'
bininfo(o, by.strata = o$bin.by.strata, ...)
```

## Arguments

- o:

  An object.

- ...:

  Additional arguments.

- by.strata:

  Should the calculations be done by strata? Defaults to what was
  specified when the binning was done.

## Value

A \`data.table\` containing the following columns:

- `nobs`: Number of observed data points in the bin

- `xmedian`: Median x-value of the observed data points in the bin

- `xmean`: Mean x-value of the observed data points in the bin

- `xmax`: Maximum x-value of the observed data points in the bin

- `xmin`: Minimum x-value of the observed data points in the bin

- `xmid`: Value halfway between \`xmin\` and \`xmax\`. x-value of the
  observed data points in the bin

- `xleft`: Value halfway between the minimum x-value of the current bin
  and the maximum x-value of the previous bin to the left (for the
  left-most bin, it is the minimum x-value).

- `xright`: Value halfway between the maximum x-value of the current bin
  and the minimum x-value of the next bin to the right (for the
  right-most bin, it is the maximum x-value).

- `xcenter`: Value halfway between \`xleft\` and \`xright\`.

In addition, if stratification was performed, the stratification columns
will be included as well.

## Methods (by class)

- `bininfo(tidyvpcobj)`: Method for `tidyvpcobj`.
