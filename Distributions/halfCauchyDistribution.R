require(extraDistr)

### input varialbe
x = seq(0.1, 10, length.out = 1000)


### 수명 분포
dhcauchy(x, sigma = sigma)


### 분위수 함수
qhcauchy(x, sigma = sigma)


### 난수 함수
rhcauchy(x, sigma = sigma)


### 누적분포함수
phcauchy(x, sigma = sigma)


### 생존함수
shcauchy = function (x, sigma = 1)
{
    fx = 1 - phcauchy(x, sigma = sigma)
    return(fx)
}


### 위험함수
hhcauchy = function (x, sigma = 1)
{
    fx = dhcauchy(x, sigma = sigma) / shcauchy(x, sigma = sigma)
    return(fx)
}