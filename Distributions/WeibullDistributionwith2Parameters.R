source("colorPalette.R")


##### weibull Distribution with 2 Parameters
### parameter
shape = c(0.25, 0.5, 0.75, 1, 2, 4, 8)
scale = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(0.1, 10, length.out = 1000)


### 수명 분포
dweibull(x, shape = 1, scale = 1)


### 분위수 함수
qweibull(x, shape = 1, scale = 1)


### 난수 함수
rweibull(x, shape = 1, scale = 1)


### 누적분포함수
pweibull(x, shape = 1, scale = 1)


### 생존함수
sweibull = function (x, shape = 1, scale = 1)
{
    fx = 1 - pweibull(x, shape = shape, scale = scale)
    return(fx)
}


### 위험함수
hweibull = function (x, shape = 1, scale = 0)
{
    fx = dweibull(x, shape = shape, scale = scale) / sweibull(x, shape = shape, scale = scale)
    return(fx)
}



### 위험함수
hweibull = function (x, scale = 1, shape = 0) 
{
    fx = dweibull(x, shape = shape, scale = scale) / sweibull(x, shape = shape, scale = scale)
    return(fx)
}