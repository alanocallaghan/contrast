# Print a Contrast Object

Print a Contrast Object

## Usage

``` r
# S3 method for class 'contrast'
print(x, X = FALSE, fun = function(u) u, ...)
```

## Arguments

- x:

  Result of [`contrast()`](https://rdrr.io/pkg/rms/man/contrast.html).

- X:

  A logical: set `TRUE` to print design matrix used in computing the
  contrasts (or the average contrast).

- fun:

  A function to transform the contrast, SE, and lower and upper
  confidence limits before printing. For example, specify `fun = exp` to
  anti-log them for logistic models.

- ...:

  Not used.
