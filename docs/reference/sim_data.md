# Example simulated data with continuous DV

A simulated dataset from a hypothetical PK model with 100 replicates.

## Usage

``` r
sim_data
```

## Format

A data.table with 60000 rows and 10 variables:

- ID:

  Subject identifier

- REP:

  Replicate num for simulation

- TIME:

  Time

- DV:

  Concentration of drug

- IPRED:

  Individual prediction variable

- PRED:

  Population prediction variable

- AMT:

  Amount of dosage initially administered at DV = 0, TIME = 0

- DOSE:

  Dosage amount

- MDV:

  Dummy indicating missing dependent variable value

- NTIME:

  Nominal Time

## Source

[`simple_data`](https://rdrr.io/pkg/vpc/man/simple_data.html)
