# Simulation of Pi

## Simple plot


```r
n <- 5e4 
x <- rnorm(n); y <- rnorm(n)
inside <- x^2 + y^2 <= 1
plot(x, y, col=ifelse(inside, "red", "blue"), pch=".", 
     xlim=c(-2,2), ylim=c(-2,2))
```

![](plot-simulation-of-pi_files/figure-html/simple-plot-1.png) 

## Calculate Pi


```r
n <- 1e6
x <- runif(n); y <- runif(n)
inside <- x^2 + y^2 <= 1
pi <- 4 * sum(inside) / n
idx <- sample.int(trunc(n/(log(n))), replace = FALSE)
x <- x[idx]; y <- y[idx];
inside <- inside[idx]

plot(x, y, 
     col=ifelse(inside, "blue", "red"), cex=0.5, pch=".",  
     main=sprintf("Bootstrap approximation of pi\nusing %s random samples, pi ~ %1.5f", n, pi))
```

![](plot-simulation-of-pi_files/figure-html/simulation-pi-1.png) 
