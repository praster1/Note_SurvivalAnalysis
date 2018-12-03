### 수명 분포
dcauchy(x, location, scale)


### 분위수 함수
qcauchy(x, location, scale)


### 난수 함수
rcauchy(x, location, scale)


### 누적분포함수
pcauchy(x, location, scale)


### 생존함수
scauchy = function (x, location = 1, scale = 0) 
{
    fx = 1 - pcauchy(x, location, scale)
    return(fx)
}


### 위험함수
hcauchy = function (x, location = 1, scale = 0)
{
    fx = dcauchy(x, location, scale) / scauchy(x, location, scale)
    return(fx)
}