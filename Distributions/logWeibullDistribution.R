source("colorPalette.R")


##### logweibull Distribution
### parameter
alpha = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(0.1, 10, length.out = 1000)


### 수명 분포
dlogweibull = function(x, alpha = 0, beta = 1)
{
	temp = (x-alpha)/beta
    fx = (1/beta) * exp(temp - exp(temp))
    return(fx)
}


### 난수 함수
rlogweibull = function (n, min=0.0001, max=1, alpha = 1, beta = 1) 
{
    normalization = function(x)	{	(x-min(x))/(max(x)-min(x));	}

	xseq = seq(min, max, length=1000000)
	res = sample(xseq, size=n, prob=normalization(dlogweibull(xseq, alpha = alpha, beta = beta)), replace=TRUE)
	return(res)
}


### 누적분포함수
plogweibull = function(x, alpha = 0, beta = 1)
{
    fx = -(slogweibull(x, alpha = alpha, beta = beta) - 1)
    return(fx)
}


### 생존함수
slogweibull = function (x, alpha = 0, beta = 1) 
{
	temp = (x-alpha)/beta
    fx = exp(- exp(temp))
    return(fx)
}


### 위험함수
hlogweibull = function (x, alpha = 0, beta = 1)
{
    fx = dlogweibull(x, alpha, beta) / slogweibull(x, alpha, beta)
    return(fx)
}