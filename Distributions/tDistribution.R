### 수명 분포
# dt(x, df = df)


### 분위수 함수
# qt(x, df = df)


### 난수 함수
# rt(x, df = df)


### 누적분포함수
# pt(x, df = df)


### 생존함수
st = function (x, df = 1)
{
    fx = 1 - pt(x, df = df)
    return(fx)
}


### 위험함수
ht = function (x, df = 1)
{
    fx = dt(x, df = df) / st(x, df = df)
    return(fx)
}