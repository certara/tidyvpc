# Perform a consistency check on observed and simulated data

This function performs a simple consistency check on an observed and
simulated dataset to make sure they are consistent with respect to
ordering as required by the other functions used in the VPC calculation.

## Usage

``` r
check_order(obs, sim, tol = 1e-05)
```

## Arguments

- obs, sim:

  A \`data.frame\` with 2 columns (see Details).

- tol:

  A tolerance for comparing time values.

## Value

The number of replicates contained in \`sim\`.

## Details

The consistency check is performed by comparing a combination of unique
subject identifier (ID) and time. Both `data.frame` objects must be
given with those in positions 1 and 2, respectively.

## See also

[`observed`](https://github.com/certara/tidyvpc/reference/observed.md),
[`simulated`](https://github.com/certara/tidyvpc/reference/simulated.md).

## Examples

``` r

# \donttest{

 require(data.table)

check_order(obs_data[, .(ID, TIME)], sim_data[, .(ID, TIME)])
#> [1] 100
# }
```
