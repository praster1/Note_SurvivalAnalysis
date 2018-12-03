### 수명 분포
dcosine = function(x, mu, sigma) 
{
    fx = (1/(2*sigma))*(cos( (x-mu)/sigma))
    return(fx)
}


### 난수 함수
rcosine = function(n, min=-10, max=10, mu = 0, sigma = 1)
{
	normalization = function(x)	{	(x-min(x))/(max(x)-min(x));	}

	xseq = seq(min, max, length=1000000)
	res = sample(xseq, size=n, prob=normalization(dcosine(xseq, mu=mu, sigma=sigma)), replace=TRUE)
	return(res)
}


### 누적분포함수
pcosine = function(x, mu = 0, sigma = 1)
{
    fx = -(scosine(x, mu, sigma) - 1)
    return(fx)
}


### 생존함수
scosine = function (x, mu = 0, sigma = 1) 
{
    fx = (1/2) * (1 - sin((x-mu)/sigma))
    return(fx)
}


### 위험함수
hcosine = function (x, mu = 0, sigma = 1)
{
    fx = dcosine(x, mu, sigma) / scosine(x, mu, sigma)
    return(fx)
}