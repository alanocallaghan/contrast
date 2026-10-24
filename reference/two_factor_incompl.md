# Incomplete Two-Factor Experiment with Repeated Measurments

Incomplete Two-Factor Experiment with Repeated Measurments

## Value

- two_factor_incompl:

  A data frame

## Details

In a gene expression experiment, stem cells were differentiated using a
set of factors (such as media types, cell spreads etc.). These factors
were collapsed into a single cell environment configurations variable.
The cell lines were assays over three days. Two of the configurations
were only run on the first day and the other two were assays at
baseline.

To get the materials, three donors provided materials. These donors
provided (almost) equal replication across the two experimental factors
(day and configuration).

One of the goals of this experiment was to assess pre-specified
differences in the configuration at each time point. For example, the
differences between configurations `A` and `B` at day one is of
interest. Also, the differences between configurations `C` and `D` at
each time points were important.

Since there are missing cells in the design, it is not a complete
two-way factorial. One way to analyze this experiment is to further
collapse the time and configuration data into a single variable and then
specify each comparison using this factor.

## Examples

``` r
two_factor_incompl
#>    subject config day expression group
#> 1   donor1      A   1      9.364   1:A
#> 2   donor1      D   1      9.533   1:D
#> 3   donor1      C   1      9.267   1:C
#> 4   donor1      D   2      9.407   2:D
#> 5   donor1      C   2      9.463   2:C
#> 6   donor1      D   4      9.419   4:D
#> 7   donor1      C   4      9.507   4:C
#> 8   donor2      B   1      9.226   1:B
#> 9   donor2      A   1      9.153   1:A
#> 10  donor2      D   1      9.112   1:D
#> 11  donor3      B   1      9.657   1:B
#> 12  donor2      C   1      8.973   1:C
#> 13  donor2      D   2      9.096   2:D
#> 14  donor2      D   4      9.232   4:D
#> 15  donor2      C   4      9.261   4:C
#> 16  donor3      A   1      9.383   1:A
#> 17  donor3      D   1      9.365   1:D
#> 18  donor3      C   1      9.242   1:C
#> 19  donor3      D   2      9.361   2:D
#> 20  donor3      D   4      9.345   4:D
#> 21  donor3      C   2      9.444   2:C
#> 22  donor3      C   4      9.551   4:C
#> 23  donor1      B   1      9.601   1:B
```
