# Not Completed


source("colorPalette.R")
require(actuar)


##### Gamma Distribution with Location Parameters
### parameter
location = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
shape = c(0.25, 0.5, 0.75, 1, 2, 4, 8)
scale = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(0.1, 10, length.out = 1000)


### 수명 분포
dgamma3 = function(x, shape=shape, scale=scale, location=location)
{
    fx = dgamma(x-location, shape=shape, scale=scale)
    return(fx)
}


### 분위수 함수
qgamma3 = function(x, shape=shape, scale=scale, location=location)
{
    fx = qgamma(x-location, shape=shape, scale=scale)
    return(fx)
}


### 난수 함수
rgamma3 = function(x, shape=shape, scale=scale, location=location)
{
    fx = rgamma(x-location, shape=shape, scale=scale)
    return(fx)
}


### 누적분포함수
pgamma3 = function(x, shape=shape, scale=scale, location=location)
{
    fx = pgamma(x-location, shape=shape, scale=scale)
    return(fx)
}


### 생존함수
sgamma3 = function(x, shape=1, scale=1, location=0)
{
    fx = 1 - pgamma3(x, shape=shape, scale=scale, location=location)
    return(fx)
}


### 위험함수
hgamma3 = function (x, shape=shape, scale=scale, location=0)
{
    fx = dgamma3(x, shape=shape, scale=scale, location=location) / sgamma3(x, shape=shape, scale=scale, location=location)
    return(fx)
}
