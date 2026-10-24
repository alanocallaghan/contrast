# Complete Two-Factor Experiment

Complete Two-Factor Experiment

## Value

- two_factor_crossed:

  A data frame

## Details

A gene expression experiment was run to assess the effect of a compound
under two different diets: high fat and low fat. The main comparisons of
interest are the difference between the treated and untreated groups
within a diet. The interaction effect was a secondary hypothesis. For
illustration, we only include the expression value of one of the genes.
The study design was a full two-way factorial with `n = 24` samples.

## Examples

``` r
two_factor_crossed
#>    expression     diet     group
#> 1       8.029 high fat treatment
#> 2       7.650 high fat   vehicle
#> 3       7.304 high fat   vehicle
#> 4       7.240  low fat   vehicle
#> 5       7.322  low fat   vehicle
#> 6       7.727  low fat treatment
#> 7       7.554  low fat treatment
#> 8       7.198  low fat   vehicle
#> 9       7.575  low fat treatment
#> 10      7.399  low fat   vehicle
#> 11      8.211 high fat treatment
#> 12      7.321  low fat   vehicle
#> 13      7.548  low fat treatment
#> 14      7.538  low fat treatment
#> 15      7.565  low fat treatment
#> 16      7.304  low fat   vehicle
#> 17      7.998 high fat treatment
#> 18      8.067 high fat treatment
#> 19      7.755 high fat   vehicle
#> 20      7.415 high fat   vehicle
#> 21      7.850 high fat treatment
#> 22      7.217 high fat   vehicle
#> 23      8.030 high fat treatment
#> 24      7.432 high fat   vehicle
```
