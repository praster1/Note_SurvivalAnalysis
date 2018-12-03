### 수명 분포
dgamma2 = function(x, shape=shape, scale=scale)
{
    fx = dgamma(x, shape=shape, scale=scale)
    return(fx)
}


### 분위수 함수
qgamma2 = function(x, shape=shape, scale=scale)
{
    fx = qgamma(x, shape=shape, scale=scale)
    return(fx)
}


### 난수 함수
rgamma2 = function(x, shape=shape, scale=scale)
{
    fx = rgamma(x, shape=shape, scale=scale)
    return(fx)
}


### 누적분포함수
pgamma2 = function(x, shape=shape, scale=scale)
{
    fx = pgamma(x, shape=shape, scale=scale)
    return(fx)
}


### 생존함수
sgamma2 = function(x, shape=1, scale=1)
{
    fx = 1 - pgamma2(x, shape=shape, scale=scale)
    return(fx)
}


### 위험함수
hgamma2 = function (x, shape=shape, scale=scale)
{
    fx = dgamma2(x, shape=shape, scale=scale) / sgamma2(x, shape=shape, scale=scale)
    return(fx)
}