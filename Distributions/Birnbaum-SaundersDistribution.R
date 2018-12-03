require(extraDistr)

### 수명 분포
dfatigue(x, alpha, beta = 1, mu = 0)


### 분위수 함수
qfatigue(x, alpha, beta, mu = 0)


### 난수 함수
rfatigue(x, alpha, beta, mu = 0)


### 누적분포함수
pfatigue(x, alpha, beta, mu = 0)


### 생존함수
sfatigue = function (x, alpha = 1, beta = 0) 
{
    fx = 1 - pfatigue(x, alpha, beta)
    return(fx)
}


### 위험함수
hfatigue = function (x, alpha = 1, beta = 0)
{
    fx = dfatigue(x, alpha, beta) / sfatigue(x, alpha, beta)
    return(fx)
}