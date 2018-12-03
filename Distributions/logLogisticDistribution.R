### 수명 분포
dloglogistic = function(x, alpha = 1, beta = 1, gamma = 1)
{
    fx = (gamma/beta) * ((x - alpha)/beta)^(gamma - 1) * (1 + ((x - alpha) / beta)^gamma)^(-2)
    return(fx)
}


### 난수 함수
rloglogistic = function (n, min=0.0001, max=10, alpha = 1, beta = 1, gamma = 1) 
{
    normalization = function(x)	{	(x-min(x))/(max(x)-min(x));	}

	xseq = seq(min, max, length=1000000)
	res = sample(xseq, size=n, prob=normalization(dloglogistic(xseq, alpha = alpha, beta = beta, gamma = gamma)), replace=TRUE)
	return(res)
}


### 누적분포함수
ploglogistic = function(x, alpha = 1, beta = 1, gamma = 1)
{
    fx = -(sloglogistic(x, alpha, beta) - 1)
    return(fx)
}


### 생존함수
sloglogistic = function (x, alpha = 1, beta = 1, gamma = 1)
{
    fx = (gamma/beta) * ((x - alpha) / beta)^(gamma - 1) * (1 + ((x - alpha) / beta)^gamma)^(-1)
    return(fx)
}


### 위험함수
hloglogistic = function (x, alpha = 1, beta = 1, gamma = 1)
{
    fx = dloglogistic(x, alpha, beta, gamma) / sloglogistic(x, alpha, beta, gamma)
    return(fx)
}