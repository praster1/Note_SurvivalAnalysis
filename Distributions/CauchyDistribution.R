source("colorPalette.R")


##### Cauchy Distribution
### parameter
location = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
scale = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(-10, 10, length.out = 1000)


### 수명 분포
dcauchy(x, location, scale)


### 분위수 함수
qcauchy(x, location, scale)


### 난수 함수
rcauchy(x, location, scale)


### 누적분포함수
pcauchy(x, location, scale)


### 생존함수
scauchy = function (x, location = 1, scale = 0) 
{
    fx = 1 - pcauchy(x, location, scale)
    return(fx)
}


### 위험함수
hcauchy = function (x, location = 1, scale = 0)
{
    fx = dcauchy(x, location, scale) / scauchy(x, location, scale)
    return(fx)
}