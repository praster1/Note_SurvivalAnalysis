require(extraDistr)

### input varialbe
x = seq(0.1, 10, length.out = 1000)


### 수명 분포
dhnorm(x, sigma = 1)


### 분위수 함수
qhnorm(x, sigma = 1)


### 난수 함수
rhnorm(x, sigma = 1)


### 누적분포함수
phnorm(x, sigma = 1)


### 생존함수
shnorm = function (x, sigma = 1)
{
    fx = 1 - phnorm(x, sigma = sigma)
    return(fx)
}


### 위험함수
hhnorm = function (x, sigma = 1)
{
    fx = dhnorm(x, sigma = sigma) / shnorm(x, sigma = sigma)
    return(fx)
}