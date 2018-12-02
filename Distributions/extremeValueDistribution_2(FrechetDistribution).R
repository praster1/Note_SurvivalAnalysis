source("colorPalette.R")
require(VGAM)


##### frechet Distribution
### parameter
location = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
shape = c(0.25, 0.5, 0.75, 1, 2, 4, 8)
scale = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(0.1, 10, length.out = 1000)


### 수명 분포
dfrechet(x, location = 0, shape = 1, scale = 1)


### 분위수 함수
qfrechet(x, location = 0, shape = 1, scale = 1)


### 난수 함수
rfrechet(x, location = 0, shape = 1, scale = 1)


### 누적분포함수
pfrechet(x, location = 0, shape = 1, scale = 1)


### 생존함수
sfrechet = function (x, location = 1, shape = 1, scale = 1)
{
    fx = 1 - pfrechet(x, location = location, shape = shape, scale = scale)
    return(fx)
}


### 위험함수
hfrechet = function (x, location = 1, shape = 1, scale = 1)
{
    fx = dfrechet(x, location = location, shape = shape, scale = scale) / sfrechet(x, location = location, shape = shape, scale = scale)
    return(fx)
}