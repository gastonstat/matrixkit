# `"matrixkit"`

**matrixkit** is an R package that provides a *first aid kit* for some matrix operations commonly used in multivariate data analysis methods.


## Motivation

I developed `matrixkit` as part of a major project to develop a set of tools for **multiblock** methods. The main idea behind `matrixkit` is to have a set of functions that are frequently used in several multivariate methods. For instance, calculate the (euclidean) norm of a vector, the trace of a square matrix, a *Projector* (aka *hat matrix*) matrix, the *RV coefficient* between two matrices, etc.


## Installation

Stable version on [CRAN](http://cran.r-project.org/web/packages/matrixkit/index.html):

```ruby
# stable version
install.packages('matrixkit')
```

Development version on [github](https://github.com/gastonstat/matrixkit):

```ruby
library(devtools)
install_github('matrixkit', 'gastonstat')
```

## Some examples
```ruby
library(matrixkit)

# create a vector 
v = c(1, 1, 1, 1)

# vector norm
vnorm(v)

# create a square matrix
set.seed(10)
X = matrix(runif(25), 5, 5)

# trace of X
tr(X)

# create a matrix
set.seed(5)
M = matrix(runif(15), 5, 3)

# projector matrix
projector(M)

# create two matrices with same number of rows
A = matrix(1:9, 3, 3)
B = matrix(rnorm(9), 3, 3)

# RV coefficient of A and B
RV_coefficient(A, B)
```


Author Contact
--------------
[www.gastonsanchez.com](http://www.gastonsanchez.com)

Gaston Sanchez (`gaston.stat at gmail.com`)

