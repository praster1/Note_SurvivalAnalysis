require(VGAM)

### 수명 분포
# dmakeham(x, shape = theta)


### 분위수 함수
# qmakeham(x, shape = theta)


### 난수 함수
# rmakeham(x, shape = theta)


### 누적분포함수
# pmakeham(x, shape = theta)


### 생존함수
smakeham = function (x, shape = 1)
{
    fx = 1 - pmakeham(x, shape = theta)
    return(fx)
}


### 위험함수
hmakeham = function (x, shape = 1)
{
    fx = dmakeham(x, shape = theta) / smakeham(x, shape = theta)
    return(fx)
}