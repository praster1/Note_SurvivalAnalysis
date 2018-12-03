### 수명 분포
dpareto1 = function(x, alpha = 1, beta = 1, gamma = 1)
{
    fx = (gamma/beta) * (((x-alpha)/beta)^(-(gamma+1)))
    return(fx)
}


### 난수 함수
rpareto1 = function (n, min=0.0001, max=10, alpha = 1, beta = 1, gamma = 2)
{
    normalization = function(x)	{	(x-min(x))/(max(x)-min(x));	}

	xseq = seq(min, max, length=1000000)
	res = sample(xseq, size=n, prob=normalization(dpareto1(xseq, alpha = alpha, beta = beta, gamma = gamma)), replace=TRUE)
	return(res)
}


### 누적분포함수
ppareto1 = function(x, alpha = 1, beta = 1, gamma = 1)
{
    fx = -(spareto1(x, alpha, beta, gamma) - 1)
    return(fx)
}


### 생존함수
spareto1 = function (x, alpha = 1, beta = 1, gamma = 1)
{
    fx = ((x-alpha)/beta)^(-gamma)
    return(fx)
}


### 위험함수
hpareto1 = function (x, alpha = 1, beta = 1, gamma = 1)
{
    fx = dpareto1(x, alpha, beta, gamma) / spareto1(x, alpha, beta, gamma)
    return(fx)
}