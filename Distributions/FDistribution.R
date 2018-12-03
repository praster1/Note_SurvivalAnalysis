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