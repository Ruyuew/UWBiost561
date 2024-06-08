# MyPackage

This is an example R package.

## Installation

You can install the development version from GitHub:

```r
# if (!require('devtools')) install.packages('devtools')
devtools::install_github('yourusername/mypackage')
```

## Usage

```r
library(mypackage)
set.seed(123)
data <- matrix(sample(0:1, 100, replace = TRUE), 10, 10)
data[lower.tri(data)] <- t(data)[lower.tri(data)]
diag(data) <- 1
result1 <- compute_maximal_partial_clique1(data, alpha=0.7, method="simple")
result2 <- compute_maximal_partial_clique2(data, alpha=0.7)
result3 <- compute_maximal_partial_clique3(data, alpha=0.7)
print(result1)
print(result2)
print(result3)
```

## Links

- [GitHub Repository](https://github.com/yourusername/mypackage)
- [PkgDown Website](https://yourusername.github.io/mypackage)
