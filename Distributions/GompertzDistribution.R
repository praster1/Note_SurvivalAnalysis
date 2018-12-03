### 수명 분포
dgompertz = function (x, alpha = 0, beta = 1) 
{
    fx = alpha * exp(beta * x) * exp(alpha/beta * (1 - exp(beta * x)))
    return(fx)
}


### 난수 함수
rgompertz = function (n, min=0.0001, max=1, alpha = 1, beta = 1) 
{
    normalization = function(x)	{	(x-min(x))/(max(x)-min(x));	}

	xseq = seq(min, max, length=1000000)
	res = sample(xseq, size=n, prob=normalization(dgompertz(xseq, alpha = alpha, beta = beta)), replace=TRUE)
	return(res)
}


### 누적분포함수
pgompertz = function (x, alpha = 0, beta = 1) 
{
    fx = -(sgompertz(x, alpha, beta) - 1)
    return(fx)
}


### 생존함수
sgompertz = function (x, alpha = 0, beta = 1) 
{
    fx = exp(alpha/beta * (1 - exp(beta * x)))
    return(fx)
}


### 위험함수
hgompertz = function (x, alpha = 0, beta = 1) 
{
    fx = dgompertz(x, alpha, beta) / sgompertz(x, alpha, beta)
    return(fx)
}