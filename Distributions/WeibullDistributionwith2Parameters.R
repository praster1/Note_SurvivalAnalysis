### 수명 분포
dweibull(x, shape = 1, scale = 1)


### 분위수 함수
qweibull(x, shape = 1, scale = 1)


### 난수 함수
rweibull(x, shape = 1, scale = 1)


### 누적분포함수
pweibull(x, shape = 1, scale = 1)


### 생존함수
sweibull = function (x, shape = 1, scale = 1)
{
    fx = 1 - pweibull(x, shape = shape, scale = scale)
    return(fx)
}


### 위험함수
hweibull = function (x, shape = 1, scale = 0)
{
    fx = dweibull(x, shape = shape, scale = scale) / sweibull(x, shape = shape, scale = scale)
    return(fx)
}



### 위험함수
hweibull = function (x, scale = 1, shape = 0) 
{
    fx = dweibull(x, shape = shape, scale = scale) / sweibull(x, shape = shape, scale = scale)
    return(fx)
}