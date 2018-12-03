require(extremefit)

### 수명 분포
rburr(x, a=alpha, k=beta)


### 분위수 함수
qburr(x, a=alpha, k=beta)


### 난수 함수
rburr(x, a=alpha, k=beta)


### 누적분포함수
pburr(x, a=alpha, k=beta)


### 생존함수
sburr = function (x, a = 1, k = 0) 
{
    fx = 1 - pburr(x, a=a, k=k)
    return(fx)
}


### 위험함수
hburr = function (x, a=a, k=k)
{
    fx = dburr(x, a=a, k=k) / sburr(x, a=a, k=k)
    return(fx)
}