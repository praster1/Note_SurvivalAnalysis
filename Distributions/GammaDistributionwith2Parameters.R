source("colorPalette.R")


##### Gamma Distribution
### parameter
shape = c(0.25, 0.5, 0.75, 1, 2, 4, 8)
scale = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(0.1, 10, length.out = 1000)


### 수명 분포
dgamma2 = function(x, shape=shape, scale=scale)
{
    fx = dgamma(x, shape=shape, scale=scale)
    return(fx)
}


### 분위수 함수
qgamma2 = function(x, shape=shape, scale=scale)
{
    fx = qgamma(x, shape=shape, scale=scale)
    return(fx)
}


### 난수 함수
rgamma2 = function(x, shape=shape, scale=scale)
{
    fx = rgamma(x, shape=shape, scale=scale)
    return(fx)
}


### 누적분포함수
pgamma2 = function(x, shape=shape, scale=scale)
{
    fx = pgamma(x, shape=shape, scale=scale)
    return(fx)
}


### 생존함수
sgamma2 = function(x, shape=1, scale=1)
{
    fx = 1 - pgamma2(x, shape=shape, scale=scale)
    return(fx)
}


### 위험함수
hgamma2 = function (x, shape=shape, scale=scale)
{
    fx = dgamma2(x, shape=shape, scale=scale) / sgamma2(x, shape=shape, scale=scale)
    return(fx)
}