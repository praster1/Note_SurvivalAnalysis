require(rmutil)

### 수명 분포
dinvgauss(x, m = 1, s = 1)


### 분위수 함수
qinvgauss(x, m = 1, s = 1)


### 난수 함수
rinvgauss(x, m = 1, s = 1)


### 누적분포함수
pinvgauss(x, m = 1, s = 1)


### 생존함수
sinvgauss = function (x, m = 1, s = 1)
{
    fx = 1 - pinvgauss(x, m = m, s = s)
    return(fx)
}


### 위험함수
hinvgauss = function (x, m = 1, s = 1)
{
    fx = dinvgauss(x, m = m, s = s) / sinvgauss(x, m = m, s = s)
    return(fx)
}