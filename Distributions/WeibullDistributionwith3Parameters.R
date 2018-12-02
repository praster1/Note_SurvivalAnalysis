source("colorPalette.R")


##### weibull Distribution with Location Parameters
### parameter
shape = c(0.25, 0.5, 0.75, 1, 2, 4, 8)
scale = c(0.25, 0.5, 0.75, 1, 2, 4, 8)
location = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(0.1, 10, length.out = 1000)


### 수명 분포
dweibull3 = function(x, shape=shape, scale=scale, location=location)
{
    fx = dweibull(x-location, shape=shape, scale=scale)
    return(fx)
}


### 분위수 함수
qweibull3 = function(x, shape=shape, scale=scale, location=location)
{
    fx = qweibull(x-location, shape=shape, scale=scale)
    return(fx)
}


### 난수 함수
rweibull3 = function(x, shape=shape, scale=scale, location=location)
{
    fx = rweibull(x-location, shape=shape, scale=scale)
    return(fx)
}


### 누적분포함수
pweibull3 = function(x, shape=shape, scale=scale, location=location)
{
    fx = pweibull(x-location, shape=shape, scale=scale)
    return(fx)
}


### 생존함수
sweibull3 = function(x, shape=1, scale=1, location=0)
{
    fx = 1 - pweibull3(x, shape=shape, scale=scale, location=location)
    return(fx)
}


### 위험함수
hweibull3 = function (x, shape=shape, scale=scale, location=0)
{
    fx = dweibull3(x, shape=shape, scale=scale, location=location) / sweibull3(x, shape=shape, scale=scale, location=location)
    return(fx)
}