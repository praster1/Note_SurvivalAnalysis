source("colorPalette.R")
require(pracma)


##### semielliptical Distribution
### parameter
alpha = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(-1, 1, length.out = 1000)


### 수명 분포
dsemielliptical = function(x, alpha = 0, beta = 1)
{
    fx = (2/(beta * pi)) * sqrt(1 - ((x-alpha)/beta)^2)
    return(fx)
}


### 난수 함수
rsemielliptical = function (n, min=-1, max=1, alpha = 0, beta = 1)
{
    normalization = function(x)	{	(x-min(x))/(max(x)-min(x));	}

	xseq = seq(min, max, length=1000000)
	res = sample(xseq, size=n, prob=normalization(dsemielliptical(xseq, alpha = alpha, beta = beta)), replace=TRUE)
	return(res)
}


### 누적분포함수
psemielliptical = function(x, alpha = 0, beta = 1)
{
    fx = -(ssemielliptical(x, alpha = alpha, beta = beta) - 1)
    return(fx)
}


### 생존함수
ssemielliptical = function (x, alpha = 0, beta = 1) 
{
	temp = (x - alpha)/beta
    fx = (1/2) * (1/pi) * (temp * sqrt(1-temp^2) + asin(temp))
    return(fx)
}


### 위험함수
hsemielliptical = function (x, alpha = 0, beta = 1)
{
    fx = dsemielliptical(x, alpha, beta) / ssemielliptical(x, alpha, beta)
    return(fx)
}