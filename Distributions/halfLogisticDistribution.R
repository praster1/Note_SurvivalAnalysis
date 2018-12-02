source("colorPalette.R")


##### Half-logistic Distribution
### parameter
alpha = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)		# s (beta)

### input varialbe
x = seq(0.1, 10, length.out = 1000)


### 수명 분포
dhalflogistic = function(x, alpha = 0, beta = 1)
{
    fx = (2 * exp((x-alpha)/beta)) / (beta * (1 + exp((x-alpha)/beta))^2)
    return(fx)
}


### 난수 함수
rhalflogistic = function (n, min=0.0001, max=10, alpha = 1, beta = 1) 
{
    normalization = function(x)	{	(x-min(x))/(max(x)-min(x));	}

	xseq = seq(min, max, length=1000000)
	res = sample(xseq, size=n, prob=normalization(dhalflogistic(xseq, alpha = alpha, beta = beta)), replace=TRUE)
	return(res)
}

### 누적분포함수
phalflogistic = function(x, alpha = 0, beta = 1)
{
    Fx = -(shalflogistic(x, alpha = alpha, beta = beta) - 1)
    return(Fx)
}


### 생존함수
shalflogistic = function(x, alpha = 0, beta = 1)
{
    fx = 2 * (1 + exp((x-alpha)/beta))^(-1)
    return(fx)
}


### 위험함수
hhalflogistic = function(x, alpha = 0, beta = 1)
{
    fx = dhalflogistic(x, alpha = alpha, beta = beta) / shalflogistic(x, alpha = alpha, beta = beta)
    return(fx)
}