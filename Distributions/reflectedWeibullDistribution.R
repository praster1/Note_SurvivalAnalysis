source("colorPalette.R")


##### weibull Distribution with gamma Parameters
### parameter
alpha = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)
gamma = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(-10, 10, length.out = 1000)


### 수명 분포
dreflectedweibull = function(x, alpha=0, beta=1, gamma=1)
{
	temp = (x-alpha)/beta
    fx = (gamma/beta) * temp^(gamma-1) * exp(-temp^gamma)
    return(fx)
}


### 난수 함수
rreflectedweibull = function (n, min=-10, max=10, alpha = 0, beta = 1, gamma = 1)
{
    normalization = function(x)	{	(x-min(x))/(max(x)-min(x));	}

	xseq = seq(min, max, length=1000000)
	res = sample(xseq, size=n, prob=normalization(dreflectedweibull(xseq, alpha = alpha, beta = beta, gamma = gamma)), replace=TRUE)
	return(res)
}


### 누적분포함수
preflectedweibull = function(x, alpha=0, beta=1, gamma=1)
{
    fx = -(sreflectedweibull(x, alpha=alpha, beta=beta, gamma=gamma) - 1)
    return(fx)
}


### 생존함수
sreflectedweibull = function(x, alpha=0, beta=1, gamma=1)
{
	temp = (x-alpha)/beta
    fx = 1 - exp(-temp^gamma)
    return(fx)
}


### 위험함수
hreflectedweibull = function(x, alpha=0, beta=1, gamma=1)
{
    fx = dreflectedweibull(x, alpha=alpha, beta=beta, gamma=gamma) / sreflectedweibull(x, alpha=alpha, beta=beta, gamma=gamma)
    return(fx)
}