source("colorPalette.R")


##### parabolicInvertedUshaped Distribution
### parameter
alpha = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(-10, 10, length.out = 1000)


### 수명 분포
dparabolicInvertedUshaped = function(x, alpha = 0, beta = 1)
{
    fx = (3/(4*beta)) * (1 - ((x - alpha)/beta)^2)
    return(fx)
}


### 난수 함수
rparabolicInvertedUshaped = function (n, min=-10, max=10, alpha = 0, beta = 1) 
{
    normalization = function(x)	{	(x-min(x))/(max(x)-min(x));	}

	xseq = seq(min, max, length=1000000)
	res = sample(xseq, size=n, prob=normalization(dparabolicInvertedUshaped(xseq, alpha = alpha, beta = beta)), replace=TRUE)
	return(res)
}


### 누적분포함수
pparabolicInvertedUshaped = function(x, alpha = 0, beta = 1)
{
    fx = -(sparabolicInvertedUshaped(x, alpha = alpha, beta = beta) - 1)
    return(fx)
}


### 생존함수
sparabolicInvertedUshaped = function (x, alpha = 0, beta = 1) 
{
    fx = (1/2) - (1/4) * (3*((x-alpha)/beta) - ((x-alpha)/beta)^3)
    return(fx)
}


### 위험함수
hparabolicInvertedUshaped = function (x, alpha = 0, beta = 1)
{
    fx = dparabolicInvertedUshaped(x, alpha, beta) / sparabolicInvertedUshaped(x, alpha, beta)
    return(fx)
}