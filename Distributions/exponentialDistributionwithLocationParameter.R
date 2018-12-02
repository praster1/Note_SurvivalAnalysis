source("colorPalette.R")
require(EnvStats)


##### Exponential Distribution with Location Parameter
### parameter
alpha = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(0.1, 10, length.out = 1000)


### 수명 분포
dlexponential = function(x, alpha=alpha, beta=beta)
{
    fx = (1/beta) * exp(-((x-alpha)/beta))
    return(fx)
}


### 난수 함수
rlexponential = function(n, min=0.1, max=1, alpha = 1, beta = 1)
{
	normalization = function(x)	{	(x-min(x))/(max(x)-min(x));	}

	xseq = seq(min, max, length=1000000)
	res = sample(xseq, size=n, prob=normalization(dlexponential(xseq, alpha=alpha, beta=beta)), replace=TRUE)
	return(res)
}


### 누적분포함수
plexponential = function(x, alpha=alpha, beta=beta)
{
    fx = -(slexponential(x, alpha, beta) - 1)
    return(fx)
}


### 생존함수
slexponential = function (x, alpha=alpha, beta=beta)
{
    fx = exp(-((x-alpha)/beta))
    return(fx)
}


### 위험함수
hlexponential = function (x, alpha=alpha, beta=beta)
{
    fx = dlexponential(x, alpha, beta) / slexponential(x, alpha, beta)
    return(fx)
}