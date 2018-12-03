### 수명 분포
dgompertzmakeham = function(x, alpha = 1, beta = 1, gamma = 1)
{
    fx = (gamma * ((alpha / beta) * (1 - exp(beta * x)) - gamma * x)) + (alpha * exp(beta * x) * exp((alpha/beta) * (1-exp(beta * x)) - gamma * x))
    return(fx)
}


### 난수 함수
rgompertzmakeham = function (n, min=0.0001, max=1, alpha = 1, beta = 1, gamma = 1) 
{
    normalization = function(x)	{	(x-min(x))/(max(x)-min(x));	}

	xseq = seq(min, max, length=1000000)
	res = sample(xseq, size=n, prob=normalization(dgompertzmakeham(xseq, alpha = alpha, beta = beta, gamma = gamma)), replace=TRUE)
	return(res)
}


### 누적분포함수
pgompertzmakeham = function(x, alpha = 1, beta = 1, gamma = 1)
{
    fx = -(sgompertzmakeham(x, alpha, beta, gamma) - 1)
    return(fx)
}


### 생존함수
sgompertzmakeham = function (x, alpha = 1, beta = 1, gamma = 1)
{
    fx = exp((alpha/beta) * (1-exp(beta * x)) - gamma * x)
    return(fx)
}


### 위험함수
hgompertzmakeham = function (x, alpha = 1, beta = 1, gamma = 1)
{
    fx = dgompertzmakeham(x, alpha, beta, gamma) / sgompertzmakeham(x, alpha, beta, gamma)
    return(fx)
}