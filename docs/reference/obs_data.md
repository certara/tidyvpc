# Example observed data with continuous DV

An observed dataset from a hypothetical PK model, altered to include
NTIME, GROUP, GENDER.

## Usage

``` r
obs_data
```

## Format

A data.table with 600 rows and 7 variables:

- ID:

  Subject identifier

- TIME:

  Time

- DV:

  Concentration of drug

- AMT:

  Amount of dosage initially administered at DV = 0, TIME = 0

- DOSE:

  Dosage amount

- MDV:

  Dummy indicating missing dependent variable value

- NTIME:

  Nominal Time

- GENDER:

  Character variable indicating subject's gender ("M", "F")

- STUDY:

  Character variable indicating study type ("Study A", "Study B")

## Source

[`simple_data`](https://rdrr.io/pkg/vpc/man/simple_data.html)
