source("colorPalette.R")


##### log-normal Distribution
### parameter
meanlog = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
sdlog = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(0.1, 10, length.out = 1000)


### 수명 분포
dlnorm(x, meanlog = meanlog, sdlog = sdlog)


### 분위수 함수
qlnorm(x, meanlog = meanlog, sdlog = sdlog)


### 난수 함수
rlnorm(x, meanlog = meanlog, sdlog = sdlog)


### 누적분포함수
plnorm(x, meanlog = meanlog, sdlog = sdlog)


### 생존함수
slnorm = function (x, meanlog = 0, sdlog = 1)
{
    fx = 1 - plnorm(x, meanlog = meanlog, sdlog = sdlog)
    return(fx)
}


### 위험함수
hlnorm = function (x, meanlog = 0, sdlog = 1)
{
    fx = dlnorm(x, meanlog = meanlog, sdlog = sdlog) / slnorm(x, meanlog = meanlog, sdlog = sdlog)
    return(fx)
}