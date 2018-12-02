source("colorPalette.R")


##### rcosine Distribution
### parameter
mu = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
sigma = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(-10, 10, length.out = 1000)


### 수명 분포
drcosine = function(x, mu = 0, sigma = 1) 
{
    fx = (1/(2*sigma))*(1 + cos( pi * (x-mu)/sigma ))
    return(fx)
}


### 난수 함수
rrcosine = function (n, min=-10, max=10, mu = 0, sigma = 1)
{
    normalization = function(x)	{	(x-min(x))/(max(x)-min(x));	}

	xseq = seq(min, max, length=1000000)
	res = sample(xseq, size=n, prob=normalization(drcosine(xseq, mu = mu, sigma = sigma)), replace=TRUE)
	return(res)
}


### 누적분포함수
prcosine = function(x, mu = 0, sigma = 1)
{
    fx = -(srcosine(x, mu, sigma) - 1)
    return(fx)
}


### 생존함수
srcosine = function (x, mu = 0, sigma = 1) 
{
    fx = (1/2) * (1 - ((x-mu)/sigma) - (1/pi)*sin(pi * ((x-mu)/sigma)))
    return(fx)
}


### 위험함수
hrcosine = function (x, mu = 0, sigma = 1)
{
    fx = drcosine(x, mu, sigma) / srcosine(x, mu, sigma)
    return(fx)
}