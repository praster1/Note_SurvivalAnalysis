source("colorPalette.R")


##### Normal Distribution
### parameter
mean = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
sd = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(-10, 10, length.out = 1000)


### 수명 분포
dnorm(x, mean = mean, sd = sd)


### 분위수 함수
qnorm(x, mean = mean, sd = sd)


### 난수 함수
rnorm(x, mean = mean, sd = sd)


### 누적분포함수
pnorm(x, mean = mean, sd = sd)


### 생존함수
snorm = function (x, mean = 0, sd = 1)
{
    fx = 1 - pnorm(x, mean = mean, sd = sd)
    return(fx)
}


### 위험함수
hnorm = function (x, mean = 0, sd = 1)
{
    fx = dnorm(x, mean = mean, sd = sd) / snorm(x, mean = mean, sd = sd)
    return(fx)
}