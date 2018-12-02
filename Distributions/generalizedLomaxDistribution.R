# https://www.rdocumentation.org/packages/VGAM/versions/0.7-7/topics/glomax
source("colorPalette.R")


##### glomax Distribution
### parameter
alpha = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)
gamma = c(-1, -0.75, -0.5, -0.25, 0.25, 0.5, 0.75, 1)

### input varialbe
x = seq(0.1, 1, length.out = 1000)


### 수명 분포
dglomax = function(x, alpha = 1, beta = 1, gamma = 1)
{
    fx = (1/beta) * (1 + gamma * ((x - alpha)/beta))^(-(1 + 1/gamma))
    return(fx)
}


### 난수 함수
rglomax = function (n, min=0.0001, max=1, alpha = 1, beta = 1, gamma = 1) 
{
    normalization = function(x)	{	(x-min(x))/(max(x)-min(x));	}

	xseq = seq(min, max, length=1000000)
	res = sample(xseq, size=n, prob=normalization(dglomax(xseq, alpha = alpha, beta = beta, gamma = gamma)), replace=TRUE)
	return(res)
}


### 누적분포함수
pglomax = function(x, alpha = 1, beta = 1, gamma = 1)
{
    fx = -(sglomax(x, alpha, beta) - 1)
    return(fx)
}


### 생존함수
sglomax = function (x, alpha = 1, beta = 1, gamma = 1)
{
    fx = (1/beta) * (1 + gamma * ((x - alpha)/beta))^(-1)
    return(fx)
}


### 위험함수
hglomax = function (x, alpha = 1, beta = 1, gamma = 1)
{
    fx = dglomax(x, alpha, beta, gamma) / sglomax(x, alpha, beta, gamma)
    return(fx)
}