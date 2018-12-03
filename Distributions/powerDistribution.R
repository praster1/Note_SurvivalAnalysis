### 수명 분포
dpower = function(x, alpha = 1, beta = 1, gamma = 1)
{
    fx = (gamma/beta) * (((x - alpha)/beta)^(gamma-1))
    return(fx)
}


### 난수 함수
rpower = function (n, min=0.0001, max=10, alpha = 1, beta = 1, gamma = 2) 
{
    normalization = function(x)	{	(x-min(x))/(max(x)-min(x));	}

	xseq = seq(min, max, length=1000000)
	res = sample(xseq, size=n, prob=normalization(dpower(xseq, alpha = alpha, beta = beta, gamma = gamma)), replace=TRUE)
	return(res)
}


### 누적분포함수
ppower = function(x, alpha = 1, beta = 1, gamma = 1)
{
    fx = -(spower(x, alpha, beta, gamma) - 1)
    return(fx)
}


### 생존함수
spower = function (x, alpha = 1, beta = 1, gamma = 1)
{
    fx = 1 - (((x-alpha)/beta)^gamma)
    return(fx)
}


### 위험함수
hpower = function (x, alpha = 1, beta = 1, gamma = 1)
{
    fx = dpower(x, alpha, beta, gamma) / spower(x, alpha, beta, gamma)
    return(fx)
}