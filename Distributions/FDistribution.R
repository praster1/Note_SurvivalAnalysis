source("colorPalette.R")


##### F Distribution
### parameter
df1 = c(0.25, 0.5, 0.75, 1, 2, 4, 8)
df2 = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(0.1, 10, length.out = 1000)


### 수명 분포
df(x, df1 = df1, df2 = df2)


### 분위수 함수
qf(x, df1 = df1, df2 = df2)


### 난수 함수
rf(x, df1 = df1, df2 = df2)


### 누적분포함수
pf(x, df1 = df1, df2 = df2)


### 생존함수
sf = function (x, df1 = 1, df2 = 1)
{
    fx = 1 - pf(x, df1 = df1, df2 = df2)
    return(fx)
}


### 위험함수
hf = function (x, df1 = 1, df2 = 1)
{
    fx = df(x, df1 = df1, df2 = df2) / sf(x, df1 = df1, df2 = df2)
    return(fx)
}