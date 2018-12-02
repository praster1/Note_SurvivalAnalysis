source("colorPalette.R")
require(pracma)


##### teisser Distribution
### parameter
alpha = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(-10, 10, length.out = 1000)


### 수명 분포
dteisser = function(x, alpha = 0, beta = 1)
{
	temp = (x-alpha)/beta
    fx = (1/beta) * (exp(temp) - 1) * exp(1 + temp - exp(temp))
    return(fx)
}


### 난수 함수
rteisser = function (n, min=-10, max=10, alpha = 0, beta = 1)
{
    normalization = function(x)	{	(x-min(x))/(max(x)-min(x));	}

	xseq = seq(min, max, length=1000000)
	res = sample(xseq, size=n, prob=normalization(dteisser(xseq, alpha = alpha, beta = beta)), replace=TRUE)
	return(res)
}


### 누적분포함수
pteisser = function(x, alpha = 0, beta = 1)
{
    fx = -(steisser(x, alpha = alpha, beta = beta) - 1)
    return(fx)
}


### 생존함수
steisser = function (x, alpha = 0, beta = 1) 
{
	temp = (x - alpha)/beta
    fx = (1/beta) * (exp(temp) - 1)
    return(fx)
}


### 위험함수
hteisser = function (x, alpha = 0, beta = 1)
{
    fx = dteisser(x, alpha, beta) / steisser(x, alpha, beta)
    return(fx)
}