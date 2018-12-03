### 수명 분포
dgamma3 = function(x, shape=shape, scale=scale, location=location)
{
    fx = dgamma(x-location, shape=shape, scale=scale)
    return(fx)
}


### 분위수 함수
qgamma3 = function(x, shape=shape, scale=scale, location=location)
{
    fx = qgamma(x-location, shape=shape, scale=scale)
    return(fx)
}


### 난수 함수
rgamma3 = function(x, shape=shape, scale=scale, location=location)
{
    fx = rgamma(x-location, shape=shape, scale=scale)
    return(fx)
}


### 누적분포함수
pgamma3 = function(x, shape=shape, scale=scale, location=location)
{
    fx = pgamma(x-location, shape=shape, scale=scale)
    return(fx)
}


### 생존함수
sgamma3 = function(x, shape=1, scale=1, location=0)
{
    fx = 1 - pgamma3(x, shape=shape, scale=scale, location=location)
    return(fx)
}


### 위험함수
hgamma3 = function (x, shape=shape, scale=scale, location=0)
{
    fx = dgamma3(x, shape=shape, scale=scale, location=location) / sgamma3(x, shape=shape, scale=scale, location=location)
    return(fx)
}