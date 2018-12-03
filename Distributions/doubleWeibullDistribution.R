require(VaRES)

### 수명 분포
# ddweibull(x, c=1, mu=0, sigma=1)



### 난수 함수
rdweibull = function(n, min=-10, max=1, c=1, mu=0, sigma=1)
{
	normalization = function(x)	{	(x-min(x))/(max(x)-min(x));	}

	xseq = seq(min, max, length=1000000)
	res = sample(xseq, size=n, prob=normalization(ddweibull(xseq, c=c, mu=mu, sigma=sigma)), replace=TRUE)
	return(res)
}


### 누적분포함수
# pdweibull(x, c=1, mu=0, sigma=1)



### 생존함수
sdweibull = function(x, mu=1, sigma=1, c=0)
{
    fx = 1 - pdweibull(x, mu=mu, sigma=sigma, c=c)
    return(fx)
}


### 위험함수
hdweibull = function (x, mu=mu, sigma=sigma, c=0)
{
    fx = ddweibull(x, mu=mu, sigma=sigma, c=c) / sdweibull(x, mu=mu, sigma=sigma, c=c)
    return(fx)
}