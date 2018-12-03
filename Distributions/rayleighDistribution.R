require("VGAM")

### 수명 분포
# drayleigh(x, scale = scale)


### 분위수 함수
# qrayleigh(x, scale = scale)


### 난수 함수
# rrayleigh(x, scale = scale)


### 누적분포함수
# prayleigh(x, scale = scale)


### 생존함수
srayleigh = function (x, scale = 1)
{
    fx = 1 - prayleigh(x, scale = scale)
    return(fx)
}


### 위험함수
hrayleigh = function (x, scale = 1)
{
    fx = drayleigh(x, scale = scale) / srayleigh(x, scale = scale)
    return(fx)
}