require(VGAM)

### 수명 분포
dfrechet(x, location = 0, shape = 1, scale = 1)


### 분위수 함수
qfrechet(x, location = 0, shape = 1, scale = 1)


### 난수 함수
rfrechet(x, location = 0, shape = 1, scale = 1)


### 누적분포함수
pfrechet(x, location = 0, shape = 1, scale = 1)


### 생존함수
sfrechet = function (x, location = 1, shape = 1, scale = 1)
{
    fx = 1 - pfrechet(x, location = location, shape = shape, scale = scale)
    return(fx)
}


### 위험함수
hfrechet = function (x, location = 1, shape = 1, scale = 1)
{
    fx = dfrechet(x, location = location, shape = shape, scale = scale) / sfrechet(x, location = location, shape = shape, scale = scale)
    return(fx)
}