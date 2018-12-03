### 수명 분포
darcsine = function(x, alpha = 1, beta = 0)
{
    fx = dbeta((x-alpha)/(beta-alpha), 0.5, 0.5)
    return(fx)
}


### 난수 함수
rarcsine = function(n, min=0.1, max=10, alpha = 1, beta = 0)
{
	normalization = function(x)	{	(x-min(x))/(max(x)-min(x));	}

	xseq = seq(min, max, length=1000000)
	res = sample(xseq, size=n, prob=normalization(darcsine(xseq, alpha=alpha, beta=beta)), replace=TRUE)
	return(res)
}


### 누적분포함수
parcsine = function(x, alpha = 1, beta = 0)
{
    fx = pbeta((x-alpha)/(beta-alpha), 0.5, 0.5)
    return(fx)
}


### 생존함수
sarcsine = function (x, alpha = 1, beta = 0) 
{
    fx = 1 - parcsine(x, alpha, beta)
    return(fx)
}


### 위험함수
harcsine = function (x, alpha = 1, beta = 0)
{
    fx = darcsine(x, alpha, beta) / sarcsine(x, alpha, beta)
    return(fx)
}