### 수명 분포
dalpha = function(x, alpha = 1, beta = 1) 
{
    # criterion_alpha(x, alpha=alpha, beta=beta)
	
    fx = (beta * exp(-(1/2) *(alpha - beta / x)^2)) / (sqrt(2 * pi) * pnorm(alpha) * x^2)
    return(fx)
}


### 난수함수
ralpha = function(n, min=0.1, max=10, alpha = 1, beta = 1)
{
    # criterion_alpha(x, alpha=alpha, beta=beta)
	normalization = function(x)	{	(x-min(x))/(max(x)-min(x));	}
	
	xseq = seq(min, max, length=1000000)
	res = sample(xseq, size=n, prob=normalization(dalpha(xseq, alpha=alpha, beta=beta)), replace=TRUE)
	return(res)
}


### 누적분포함수
palpha = function (x, alpha = 1, beta = 1) 
{
    # criterion_alpha(x, alpha=alpha, beta=beta)
    
    fx = pnorm(alpha - beta/x) / pnorm(alpha)
    return(fx)
}


### 생존함수
salpha = function (x, alpha = 1, beta = 1) 
{
    # criterion_alpha(x, alpha=alpha, beta=beta)
    
    fx = 1 - (pnorm(alpha - beta/x) / pnorm(alpha))
    return(fx)
}


### 위험함수
halpha = function (x, alpha = 1, beta = 1) 
{
    # criterion_alpha(x, alpha=alpha, beta=beta)
    
    fx = dalpha(x, alpha, beta) / salpha(x, alpha, beta)
    return(fx)
}
