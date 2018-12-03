### 수명 분포
dexponential = function(x, lambda = 1)
{
    fx = lambda * exp(-lambda * x)
    return(fx)
}


### 난수 함수
rexponential = function(n, min=0.1, max=10, lambda = 1)
{
	normalization = function(x)	{	(x-min(x))/(max(x)-min(x));	}

	xseq = seq(min, max, length=1000000)
	res = sample(xseq, size=n, prob=normalization(dexponential(xseq, lambda = lambda)), replace=TRUE)
	return(res)
}


### 누적분포함수
pexponential = function(x, lambda = 1)
{
    fx = 1 - exp(-lambda * x)
    return(fx)
}


### 생존함수
sexponential = function (x, lambda = 1)
{
    fx = exp(-lambda * x)
    return(fx)
}


### 위험함수
hexponential = function (x, lambda = 1)
{
    fx = rep(lambda, length(x))
    return(fx)
}