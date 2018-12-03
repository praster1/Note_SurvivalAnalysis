require(rmutil)

### 수명 분포
# dhjorth(x, m = 1, s = 1, f = 1)


### 분위수 함수
# qhjorth(x, m = 1, s = 1, f = 1)


### 난수 함수
# rhjorth(x, m = 1, s = 1, f = 1)


### 누적분포함수
# phjorth(x, m = 1, s = 1, f = 1)


### 생존함수
shjorth = function (x, m = 1, s = 1, f=1)
{
    fx = 1 - phjorth(x, m = m, s = m, f = f)
    return(fx)
}


### 위험함수
hhjorth = function (x, m = 1, s = 1, f=1)
{
    fx = dhjorth(x, m = m, s = m, f = f) / shjorth(x, m = m, s = m, f = f)
    return(fx)
}