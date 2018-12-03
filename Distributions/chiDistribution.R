require(EnvStats)

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