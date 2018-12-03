require(pracma)

### 수명 분포
dhyperbolicsecant = function(x, alpha = 1, beta = 1)
{
    fx = (1 / (beta * pi)) * sech((x - alpha)/beta)
    return(fx)
}


### 난수 함수
rhyperbolicsecant = function (n, min=-10, max=10, alpha = 1, beta = 1) 
{
    normalization = function(x)	{	(x-min(x))/(max(x)-min(x));	}

	xseq = seq(min, max, length=1000000)
	res = sample(xseq, size=n, prob=normalization(dhyperbolicsecant(xseq, alpha = alpha, beta = beta)), replace=TRUE)
	return(res)
}


### 누적분포함수
phyperbolicsecant = function(x, alpha = 1, beta = 1)
{
    fx = -(shyperbolicsecant(x, alpha, beta) - 1)
    return(fx)
}


### 생존함수
shyperbolicsecant = function (x, alpha = 1, beta = 0) 
{
    fx = 1 - (2/pi) * atan(exp((x - alpha)/beta))
    return(fx)
}


### 위험함수
hhyperbolicsecant = function (x, alpha = 1, beta = 0)
{
    fx = dhyperbolicsecant(x, alpha, beta) / shyperbolicsecant(x, alpha, beta)
    return(fx)
}