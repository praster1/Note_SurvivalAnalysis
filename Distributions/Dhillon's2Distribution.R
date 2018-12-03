### 수명 분포
ddhillon2 = function(x, alpha = 1, beta = 1, gamma = 1)
{
    fx = ((gamma + 1) / (x - alpha + beta)) * (log(((x - alpha) / beta) + 1))^gamma * exp(-(log(((x - alpha) / beta) + 1))^(gamma+1))
    return(fx)
}


### 난수 함수
rdhillon2 = function(n, min=0.1, max=1, alpha = 1, beta = 1, gamma = 1)
{
	normalization = function(x)	{	(x-min(x))/(max(x)-min(x));	}

	xseq = seq(min, max, length=1000000)
	res = sample(xseq, size=n, prob=normalization(ddhillon2(xseq, alpha=alpha, beta=beta, gamma=gamma)), replace=TRUE)
	return(res)
}


### 누적분포함수
pdhillon2 = function(x, alpha = 1, beta = 1, gamma = 1)
{
    fx = -(sdhillon2(x, alpha, beta) - 1)
    return(fx)
}


### 생존함수
sdhillon2 = function (x, alpha = 1, beta = 1, gamma = 1)
{
    fx = ((gamma + 1) / (x - alpha + beta)) * (log(((x - alpha) / beta) + 1))^gamma
    return(fx)
}


### 위험함수
hdhillon2 = function (x, alpha = 1, beta = 1, gamma = 1)
{
    fx = ddhillon2(x, alpha, beta, gamma) / sdhillon2(x, alpha, beta, gamma)
    return(fx)
}