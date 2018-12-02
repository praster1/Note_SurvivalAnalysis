source("colorPalette.R")
require(EnvStats)


##### Chi Distribution
### parameter
df = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(0.1, 10, length.out = 1000)


### 수명 분포
dchi(x, df = df)


### 분위수 함수
qchi(x, df = df)


### 난수 함수
rchi(x, df = df)


### 누적분포함수
pchi(x, df = df)


### 생존함수
schi = function (x, df = 1)
{
    fx = 1 - pchi(x, df = df)
    return(fx)
}


### 위험함수
hchi = function (x, df = 1)
{
    fx = dchi(x, df = df) / schi(x, df = df)
    return(fx)
}