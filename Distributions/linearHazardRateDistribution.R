### 수명 분포
dlinearFR = function(x, alpha = 0, beta = 1)
{
    fx = (alpha * x + beta) * exp(- (1/2) * alpha * x^2 + beta * x)
    return(fx)
}


### 난수 함수
rlinearFR = function (n, min=0.0001, max=10, alpha = 1, beta = 1) 
{
    normalization = function(x)	{	(x-min(x))/(max(x)-min(x));	}

	xseq = seq(min, max, length=1000000)
	res = sample(xseq, size=n, prob=normalization(dlinearFR(xseq, alpha = alpha, beta = beta)), replace=TRUE)
	return(res)
}


### 누적분포함수
plinearFR = function(x, alpha = 0, beta = 1)
{
    Fx = 1 - exp(- (1/2) * alpha * x^2 + beta * x)
    return(Fx)
}


### 생존함수
slinearFR = function(x, alpha = 0, beta = 1)
{
    fx = 1 - plinearFR(x, alpha = alpha, beta = beta)
    return(fx)
}


### 위험함수
hlinearFR = function(x, alpha = 0, beta = 1)
{
    fx = dlinearFR(x, alpha = alpha, beta = beta) / slinearFR(x, alpha = alpha, beta = beta)
    return(fx)
}