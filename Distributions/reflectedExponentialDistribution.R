### 수명 분포
drexponential = function(x, alpha=1, beta=1)
{
    fx = (1/beta) * exp((x-alpha)/beta)
    return(fx)
}


### 난수 함수
rrexponential = function (n, min=0.0001, max=10, alpha = 1, beta = 1)
{
    normalization = function(x)	{	(x-min(x))/(max(x)-min(x));	}

	xseq = seq(min, max, length=1000000)
	res = sample(xseq, size=n, prob=normalization(drexponential(xseq, alpha = alpha, beta = beta)), replace=TRUE)
	return(res)
}


### 누적분포함수
prexponential = function(x, alpha=1, beta=1)
{
    fx = -(srexponential(x, alpha, beta) - 1)
    return(fx)
}


### 생존함수
srexponential = function (x, alpha=1, beta=1)
{
    fx = 1 - exp((x-alpha)/beta)
    return(fx)
}


### 위험함수
hrexponential = function (x, alpha=1, beta=1)
{
    fx = drexponential(x, alpha, beta) / srexponential(x, alpha, beta)
    return(fx)
}