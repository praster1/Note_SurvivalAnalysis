### 수명 분포
dmuth = function(x, alpha=alpha, beta=beta, gamma=gamma)
{
	temp = gamma * ((x - alpha)/beta)
    fx = (1/beta) * (exp(temp) - gamma) * exp(-(1/gamma) * exp(temp) + temp + (1/gamma))
    return(fx)
}


### 난수 함수
rmuth = function (n, min=0.0001, max=10, alpha = 1, beta = 1, gamma = 1) 
{
    normalization = function(x)	{	(x-min(x))/(max(x)-min(x));	}

	xseq = seq(min, max, length=1000000)
	res = sample(xseq, size=n, prob=normalization(dmuth(xseq, alpha = alpha, beta = beta, gamma = gamma)), replace=TRUE)
	return(res)
}


### 누적분포함수
pmuth = function(x, alpha=alpha, beta=beta, gamma=gamma)
{
    fx = -(smuth(x, alpha=alpha, beta=beta, gamma=gamma) - 1)
    return(fx)
}


### 생존함수
smuth = function(x, beta=1, gamma=1, alpha=0)
{
    temp = gamma * ((x - alpha)/beta)
    fx = exp(-(1/gamma) * exp(temp) + temp + (1/gamma))
    return(fx)
}


### 위험함수
hmuth = function (x, beta=beta, gamma=gamma, alpha=0)
{
    fx = dmuth(x, alpha=alpha, beta=beta, gamma=gamma) / smuth(x, alpha=alpha, beta=beta, gamma=gamma)
    return(fx)
}