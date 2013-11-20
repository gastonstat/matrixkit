# `"matrixkit"`

**matrixkit** is an [R](http://www.r-project.org/) package that provides a *first aid kit* for some matrix operations commonly used in multivariate data analysis methods.


## Motivation

I developed `matrixkit` as part of a major project to develop a set of tools for **multiblock** methods. The main idea behind `matrixkit` is to have a set of functions that are frequently used in several multivariate methods. For instance, calculate the (euclidean) norm of a vector, the trace of a square matrix, a *Projector* (aka *hat matrix*) matrix, obtain a binary matrix from a given factor etc.


## Installation

Stable version NOT on CRAN yet:

```ruby
# stable version (not yet)
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
```


Author Contact
--------------
[www.gastonsanchez.com](http://www.gastonsanchez.com)

Gaston Sanchez (`gaston.stat at gmail.com`)

