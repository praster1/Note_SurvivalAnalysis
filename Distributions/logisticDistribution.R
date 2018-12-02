source("colorPalette.R")


##### Logistic Distribution
### parameter
location = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
scale = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(-10, 10, length.out = 1000)


### 수명 분포
dlogis(x, location, scale)


### 분위수 함수
qlogis(x, location, scale)


### 난수 함수
rlogis(x, location, scale)


### 누적분포함수
plogis(x, location, scale)


### 생존함수
slogis = function (x, location = 1, scale = 0) 
{
    fx = 1 - plogis(x, location, scale)
    return(fx)
}


### 위험함수
hlogis = function (x, location = 1, scale = 0)
{
    fx = dlogis(x, location, scale) / slogis(x, location, scale)
    return(fx)
}