# https://www.rdocumentation.org/packages/VGAM/versions/0.7-7/topics/Lomax
source("colorPalette.R")


##### lomax Distribution
### parameter
alpha = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)
gamma = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(0.1, 1, length.out = 1000)


### 수명 분포
dlomax = function(x, alpha = 1, beta = 1, gamma = 1)
{
    fx = (gamma / beta) * (1 + ((x - alpha)/beta))^(-(gamma+1))
    return(fx)
}


### 난수 함수
rlomax = function (n, min=0.0001, max=1, alpha = 1, beta = 1, gamma = 1) 
{
    normalization = function(x)	{	(x-min(x))/(max(x)-min(x));	}

	xseq = seq(min, max, length=1000000)
	res = sample(xseq, size=n, prob=normalization(dlomax(xseq, alpha = alpha, beta = beta, gamma = gamma)), replace=TRUE)
	return(res)
}


### 누적분포함수
plomax = function(x, alpha = 1, beta = 1, gamma = 1)
{
    fx = -(slomax(x, alpha, beta) - 1)
    return(fx)
}


### 생존함수
slomax = function (x, alpha = 1, beta = 1, gamma = 1)
{
    fx = (1 + ((x - alpha)/beta))^(-gamma)
    return(fx)
}


### 위험함수
hlomax = function (x, alpha = 1, beta = 1, gamma = 1)
{
    fx = dlomax(x, alpha, beta, gamma) / slomax(x, alpha, beta, gamma)
    return(fx)
}