### 수명 분포
dinverseweibull = function(x, alpha=0, beta=1, gamma=1)
{
	temp = (x-alpha)/beta
    fx = (gamma/beta) * temp^(-gamma-1) * exp(-temp^(-gamma))
    return(fx)
}


### 난수 함수
rinverseweibull = function (n, min=0.1, max=10, alpha = 0, beta = 1, gamma = 1) 
{
    normalization = function(x)	{	(x-min(x))/(max(x)-min(x));	}

	xseq = seq(min, max, length=1000000)
	res = sample(xseq, size=n, prob=normalization(dinverseweibull(xseq, alpha = alpha, beta = beta, gamma = gamma)), replace=TRUE)
	return(res)
}



### 누적분포함수
pinverseweibull = function(x, alpha=0, beta=1, gamma=1)
{
    fx = -(sinverseweibull(x, alpha=alpha, beta=beta, gamma=gamma) - 1)
    return(fx)
}


### 생존함수
sinverseweibull = function(x, alpha=0, beta=1, gamma=1)
{
	temp = (x-alpha)/beta
    fx = 1 - exp(-temp^(-gamma))
    return(fx)
}


### 위험함수
hinverseweibull = function(x, alpha=0, beta=1, gamma=1)
{
    fx = dinverseweibull(x, alpha=alpha, beta=beta, gamma=gamma) / sinverseweibull(x, alpha=alpha, beta=beta, gamma=gamma)
    return(fx)
}