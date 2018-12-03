### 수명 분포
dchisq(x, df = df)


### 분위수 함수
qchisq(x, df = df)


### 난수 함수
rchisq(x, df = df)


### 누적분포함수
pchisq(x, df = df)


### 생존함수
schisq = function (x, df = 1)
{
    fx = 1 - pchisq(x, df = df)
    return(fx)
}


### 위험함수
hchisq = function (x, df = 1)
{
    fx = dchisq(x, df = df) / schisq(x, df = df)
    return(fx)
}