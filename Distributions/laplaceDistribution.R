require(rmutil)

### 수명 분포
# dlaplace(x, m = 0, s = 1)


### 분위수 함수
# qlaplace(x, m = 0, s = 1)


### 난수 함수
# rlaplace(x, m = 0, s = 1)


### 누적분포함수
# plaplace(x, m = 0, s = 1)


### 생존함수
slaplace = function(x, m = 0, s = 1)
{
    fx = 1 - plaplace(x, m = m, s = s)
    return(fx)
}


### 위험함수
hlaplace = function(x, m = 0, s = 1)
{
    fx = dlaplace(x, m = m, s = s) / slaplace(x, m = m, s = s)
    return(fx)
}