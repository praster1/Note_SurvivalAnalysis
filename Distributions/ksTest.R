ksTest = function(dataVec = NULL, parameterLen = 100)
{
	if(is.null(dataVec))
	{
		dataVec = rnorm(1000)
	}

    normalization = function(x)	{	(x-min(x))/(max(x)-min(x));	}
    dataVec = normalization(dataVec) + abs(rnorm(dataVec, 0, 0.0001))
    dataLen = length(dataVec)
    
    ### 1 parameter
    # Chi-square Distribution       # rchisq(x, df = df)
    resChisq = NULL;   
    df_Chisq = seq(0.01, 10, length=parameterLen)

    # Exponential Distribution              # rexponential = function(n, min=0.1, max=10, lambda = 1)
    resExponential = NULL;
    lambda_Exponential = seq(0.01, 10, length=parameterLen)

    # half-Cahchy Distribution          # rhcauchy(x, sigma = sigma)
    resHalfCauchy = NULL; 
    sigma_HalfCauchy = seq(0.01, 10, length=parameterLen)

    # Half-normal Distribution           # rhnorm(x, sigma = 1)
    resHalfNormal = NULL;   
    sigma_HalfNormal = seq(0.01, 10, length=parameterLen)

    # makeham Distribution          # rmakeham(x, shape = theta)
    resMakeham = NULL; 
    theta_Makeham = seq(0.01, 10, length=parameterLen)

    # Rayleigh Distribution         # rrayleigh(x, scale = scale)
    resRayleigh = NULL;   
    scale_Rayleigh = seq(0.01, 10, length=parameterLen)

    # t Distribution           # rt(x, df = df)
    resT = NULL; 
    df_T = seq(0.01, 10, length=parameterLen)

    
    ### 2 parameters
    # Alpha
    resAlpha = NULL; 
    alpha_Alpha = seq(-1, 1, length=parameterLen);  
    beta_Alpha = seq(0.01, 10, length=parameterLen)
    
    # Arcsine
    resArcsine = NULL; 
    alpha_Arcsine = seq(-1, 1, length=parameterLen); 
    beta_Arcsine = seq(0.01, 10, length=parameterLen)
    
    # Beta
    resBeta = NULL;   
    alpha_Beta = seq(0.01, 10, length=parameterLen);  
    beta_Beta = seq(0.01, 10, length=parameterLen)
    
    # Birnbaum-Saunders Distribution        # rfatigue(x, alpha, beta, mu = 0)
    resBirnbaumSaunders = NULL;   
    alpha_BirnbaumSaunders = seq(0.01, 10, length=parameterLen); 
    beta_BirnbaumSaunders = seq(0.01, 10, length=parameterLen)
    
    # Burr Distribution         # rburr(x, a=alpha, k=beta)
    resBurr = NULL;
    alpha_Burr = seq(0.01, 10, length=parameterLen);  
    beta_Burr = seq(0.01, 10, length=parameterLen)
    
    # Cauchy Distribution       # rcauchy(x, location, scale)
    resCauchy = NULL; 
    location_Cauchy = seq(-1, 1, length=parameterLen); 
    scale_Cauchy = seq(0.01, 10, length=parameterLen)
    
    # Cosine Distribution           # rcosine = function(n, min=-10, max=10, mu = 0, sigma = 1)
    resCosine = NULL;  
    mu_Cosine = seq(-1, 1, length=parameterLen);
    sigma_Cosine = seq(0.01, 10, length=parameterLen)
    
    # Exponential Distribution with Location Parameter      # rlexponential = function(n, min=0.1, max=1, alpha = 1, beta = 1)
    resLexponential = NULL;
    alpha_Lexponential = seq(-1, 1, length=parameterLen);   
    beta_Lexponential = seq(0.01, 10, length=parameterLen)
    
    # 극치 분포: Gumbel 최대값 분포          # rgumbel_max = function (n, min=-10, max=10, scale = 1, location = 0) 
    resGumbel_max = NULL;  
    location_Gumbel_max = seq(-1, 1, length=parameterLen); 
    scale_Gumbel_max = seq(0.01, 10, length=parameterLen)
    
    # 극치 분포: Gumbel 최소값 분포          # rgumbel_min = function (n, min=-10, max=10, scale = 1, location = 0) 
    resGumbel_min = NULL; 
    location_Gumbel_min = seq(-1, 1, length=parameterLen);  
    scale_Gumbel_min = seq(1, 10, length=parameterLen)
    
    # F Distribution                # rf(x, df1 = df1, df2 = df2)
    resF = NULL;   
    df1_F = seq(0.01, 10, length=parameterLen); 
    df2_F = seq(0.01, 10, length=parameterLen)
    
    # Gamma Distribution        # rgamma2 = function(x, shape=shape, scale=scale)
    resGamma2 = NULL;  
    shape_Gamma2 = seq(0.01, 10, length=parameterLen);  
    scale_Gamma2 = seq(0.01, 10, length=parameterLen)
    
    # Gompertz Distribution         # rgompertz = function (n, min=0.0001, max=1, alpha = 1, beta = 1) 
    resGompertz = NULL; 
    alpha_Gompertz = seq(0.01, 10, length=parameterLen); 
    beta_Gompertz = seq(0.01, 10, length=parameterLen)
    
    # Half-logistic Distribution            # rhalflogistic = function (n, min=0.0001, max=10, alpha = 1, beta = 1) 
    resHalfLogistic = NULL; 
    alpha_HalfLogistic = seq(-1, 1, length=parameterLen);  
    beta_HalfLogistic = seq(0.01, 10, length=parameterLen)
    
    # hyperbolicsecant Distribution     # rhyperbolicsecant = function (n, min=-10, max=10, alpha = 1, beta = 1) 
    resHyperbolicsecant = NULL;
    alpha_Hyperbolicsecant = seq(-1, 1, length=parameterLen); 
    beta_Hyperbolicsecant = seq(0.01, 10, length=parameterLen)
    
    # Inverse-Normal Distribution           # rinvgauss(x, m = 1, s = 1)
    resInverseNormal = NULL;
    m_InverseNormal = seq(1, 10, length=parameterLen);   
    s_InverseNormal = seq(0.01, 10, length=parameterLen)
    
    # laplace Distribution          # rlaplace(x, m = 0, s = 1)
    resLaplace = NULL;  
    m_Laplace = seq(-1, 1, length=parameterLen);  
    s_Laplace = seq(0.01, 10, length=parameterLen)
    
    # 선형 증가 분포          # rlinearFR = function (n, min=0.0001, max=10, alpha = 1, beta = 1) 
    resLinearFR = NULL;  
    alpha_LinearFR = seq(0.01, 10, length=parameterLen);  
    beta_LinearFR = seq(0.01, 10, length=parameterLen)
    
    # Logistic Distribution         # rlogis(x, location, scale)
    resLogistic = NULL;   
    location_Logistic = seq(-1, 1, length=parameterLen); 
    scale_Logistic = seq(0.01, 10, length=parameterLen)
    
    # log-normal Distribution           # rlnorm(x, meanlog = meanlog, sdlog = sdlog)
    resLogNormal = NULL;   
    meanlog_LogNormal = seq(-1, 1, length=parameterLen); 
    sdlog_LogNormal = seq(0.01, 10, length=parameterLen)
    
    # logweibull Distribution   
    resLogweibull = NULL;   
    alpha_Logweibull = seq(0.01, 10, length=parameterLen); 
    beta_Logweibull = seq(-1, 1, length=parameterLen)
    
    # Normal Distribution           # rnorm(x, mean = mean, sd = sd)
    resNormal = NULL;   
    mean_Normal = seq(-1, 1, length=parameterLen);  
    sd_Normal = seq(0.01, 10, length=parameterLen)
    
    # parabolicInvertedUshaped Distribution     # rparabolicInvertedUshaped = function (n, min=-10, max=10, alpha = 0, beta = 1) 
    resParabolicInvertedUshaped = NULL;     
    alpha_ParabolicInvertedUshaped = seq(-1, 1, length=parameterLen);   
    beta_ParabolicInvertedUshaped = seq(0.01, 10, length=parameterLen)
    
    # parabolicUshaped Distribution         # rparabolicUshaped = function (n, min=-10, max=10, alpha = 1, beta = 1) 
    resParabolicUshaped = NULL
    alpha_ParabolicUshaped = seq(-1, 1, length=parameterLen)
    beta_ParabolicUshaped = seq(0.01, 10, length=parameterLen)

    # rcosine Distribution          # rrcosine = function (n, min=-10, max=10, mu = 0, sigma = 1)
    resRcosine = NULL;
    mu_Cosine = seq(-1, 1, length=parameterLen)
    sigma_Cosine = seq(0.01, 10, length=parameterLen)

    # Reflexted Exponential Distribution        # rrexponential = function (n, min=0.0001, max=10, alpha = 1, beta = 1)
    resRexponential = NULL;
    alpha_Rexponential = seq(-1, 1, length=parameterLen)
    beta_Rexponential = seq(0.01, 10, length=parameterLen)

    # semielliptical Distribution           # rsemielliptical = function (n, min=-1, max=1, alpha = 0, beta = 1)
    resSemielliptical = NULL
    alpha_Semielliptical = seq(-1, 1, length=parameterLen)
    beta_Semielliptical = seq(0.01, 10, length=parameterLen)

    # teisser Distribution          # rteisser = function (n, min=-10, max=10, alpha = 0, beta = 1)
    resTeisser = NULL
    alpha_Teisser = seq(-1, 1, length=parameterLen)
    beta_Teisser = seq(0.01, 10, length=parameterLen)

    # unif2 Distribution            # runif
    resUnif = NULL
    alpha_Unif = seq(-1, 1, length=parameterLen)
    beta_Unif = seq(0.01, 10, length=parameterLen)

    # weibull Distribution with 2 Parameters
    resWeibull = NULL;
    shape_Weibull = seq(0.01, 10, length=parameterLen)
    scale_Weibull = seq(0.01, 10, length=parameterLen)
    
    
    
    ### 3 parameters
    # dhillon1 Distribution         # rdhillon1 = function(n, min=0, max=1, alpha = 1, beta = 1, gamma = 1)
    resDhillon1 = NULL
    alpha_Dhillon1 = seq(-1, 1, length=parameterLen)
    beta_Dhillon1 = seq(0.01, 10, length=parameterLen)
    gamma_Dhillon1 = seq(0.01, 10, length=parameterLen)

    # dhillon2 Distribution         # rdhillon2 = function(n, min=0.1, max=1, alpha = 1, beta = 1, gamma = 1)
    resDhillon2 = NULL
    alpha_Dhillon2 = seq(-1, 1, length=parameterLen)
    beta_Dhillon2 = seq(0.01, 10, length=parameterLen)
    gamma_Dhillon2 = seq(0.01, 10, length=parameterLen)

    # weibull Distribution with c Parameters        # rdweibull = function(n, min=-10, max=1, c=1, mu=0, sigma=1)
    resDweibull = NULL
    mu_Dweibull = seq(-1, 1, length=parameterLen)
    sigma_Dweibull = seq(0.01, 10, length=parameterLen)
    c_Dweibull = seq(0.01, 10, length=parameterLen)

    # frechet Distribution          # rfrechet(x, location = 0, shape = 1, scale = 1)
    resFrechet = NULL
    location_Frechet = seq(-1, 1, length=parameterLen)
    shape_Frechet = seq(0.01, 10, length=parameterLen)
    scale_Frechet = seq(0.01, 10, length=parameterLen)

    # lfrechet Distribution         # rlfrechet = function (n, min=0.1, max=10, location = 0, shape = 1, scale = 1)
    resLfrechet = NULL
    location_Lfrechet = seq(-1, 1, length=parameterLen)
    shape_Lfrechet = seq(0.01, 10, length=parameterLen)
    scale_Lfrechet = seq(0.01, 10, length=parameterLen)

    # Gamma Distribution with Location Parameters       # rgamma3 = function(x, shape=shape, scale=scale, location=location)
    resGamma3 = NULL
    location_Gamma3 = seq(-1, 1, length=parameterLen)
    shape_Gamma3 = seq(0.01, 10, length=parameterLen)
    scale_Gamma3 = seq(0.01, 10, length=parameterLen)

    # glomax Distribution             # rglomax = function (n, min=0.0001, max=1, alpha = 1, beta = 1, gamma = 1) 
    resGlomax = NULL
    alpha_Glomax = seq(-1, 1, length=parameterLen)
    beta_Glomax = seq(0.01, 10, length=parameterLen)
    gamma_Glomax = seq(-1, 1, length=parameterLen)

    # Gompertz-Makeham Distribution         # rgompertzmakeham = function (n, min=0.0001, max=1, alpha = 1, beta = 1, gamma = 1) 
    resGompertzmakeham = NULL
    alpha_Gompertzmakeham = seq(-1, 1, length=parameterLen)
    beta_Gompertzmakeham = seq(0.01, 10, length=parameterLen)
    gamma_Gompertzmakeham = seq(0.01, 10, length=parameterLen)
    
    # Hjorth Distribution               # rhjorth(x, m = 1, s = 1, f = 1)
    resHjorth = NULL
    delta_Hjorth = seq(0.01, 10, length=parameterLen)	# m (delta)
    beta_Hjorth = seq(0.01, 10, length=parameterLen)		# s (beta)
    theta_Hjorth = seq(0.01, 10, length=parameterLen)	# f (theta)

    # weibull Distribution with gamma Parameters        # rinverseweibull = function (n, min=0.1, max=10, alpha = 0, beta = 1, gamma = 1) 
    resInverseweibull = NULL
    alpha_Inverseweibull = seq(-1, 1, length=parameterLen)
    beta_Inverseweibull = seq(0.01, 10, length=parameterLen)
    gamma_Inverseweibull = seq(0.01, 10, length=parameterLen)

    # loglogistic Distribution          # rloglogistic = function (n, min=0.0001, max=10, alpha = 1, beta = 1, gamma = 1) 
    resLoglogistic = NULL
    alpha_Loglogistic = seq(-1, 1, length=parameterLen)
    beta_Loglogistic = seq(0.01, 10, length=parameterLen)
    gamma_Loglogistic = seq(0.01, 10, length=parameterLen)

    # lomax Distribution              # rlomax = function (n, min=0.0001, max=1, alpha = 1, beta = 1, gamma = 1) 
    resLomax = NULL
    alpha_Lomax = seq(-1, 1, length=parameterLen)
    beta_Lomax = seq(0.01, 10, length=parameterLen)
    gamma_Lomax = seq(0.01, 10, length=parameterLen)

    # Muth Distribution          # rmuth = function (n, min=0.0001, max=10, alpha = 1, beta = 1, gamma = 1) 
    resMuth = NULL;
    alpha_Muth = seq(-1, 1, length=parameterLen)
    beta_Muth = seq(0.01, 10, length=parameterLen)
    gamma_Muth = c(0.25, 0.5, 0.75, 1)

    # Pareto Distribution of the first kind     # rpareto1 = function (n, min=0.0001, max=10, alpha = 1, beta = 1, gamma = 2)
    resPareto1 = NULL;
    alpha_Pareto1 = seq(-1, 1, length=parameterLen)
    beta_Pareto1 = seq(0.01, 10, length=parameterLen)
    gamma_Pareto1 = seq(0.01, 10, length=parameterLen)

    # Power Distribution     # rpower = function (n, min=0.0001, max=10, alpha = 1, beta = 1, gamma = 2) 
    resPower = NULL;
    alpha_Power = seq(-1, 1, length=parameterLen)
    beta_Power = seq(0.01, 10, length=parameterLen)
    gamma_Power = seq(0.01, 10, length=parameterLen)

    # weibull Distribution with gamma Parameters        # rreflectedweibull = function (n, min=-10, max=10, alpha = 0, beta = 1, gamma = 1)
    resReflectedweibull = NULL
    alpha_Reflectedweibull = seq(-1, 1, length=parameterLen)
    beta_Reflectedweibull = seq(0.01, 10, length=parameterLen)
    gamma_Reflectedweibull = seq(0.01, 10, length=parameterLen)

    # weibull Distribution with Location Parameters         # rweibull3 = function(x, shape=shape, scale=scale, location=location)
    resWeibull3 = NULL
    shape_Weibull3 = seq(0.01, 10, length=parameterLen)
    scale_Weibull3 = seq(0.01, 10, length=parameterLen)
    location_Weibull3 = seq(0.01, 10, length=parameterLen)

    
    
	
	
    for (i in 1:parameterLen)
    {
        # Chi-square Distribution
		print(paste("Test with Chi-square Distribution : i = ", i, " / ", length(df_Chisq), sep=""))
        randVec = rchisq(dataLen, df=df_Chisq[i])
        resChisq[[paste(i)]] = ks.test(dataVec, randVec)
        
        # Exponential Distribution 
		print(paste("Test with Exponential Distribution : i = ", i, " / ", length(lambda_Exponential), sep=""))
        randVec = rexponential(dataLen, min=min(dataVec), max=max(dataVec), lambda=lambda_Exponential[i])
        resExponential[[paste(i)]] = ks.test(dataVec, randVec)
        
        # Half-Cahchy Distribution
		print(paste("Test with Half-Cahchy Distribution : i = ", i, " / ", length(sigma_HalfCauchy), sep=""))
        randVec = rhcauchy(dataLen, sigma=sigma_HalfCauchy[i])
        resHalfCauchy[[paste(i)]] = ks.test(dataVec, randVec)
        
        # Half-normal Distribution
		print(paste("Test with Half-normal Distribution : i = ", i, " / ", length(sigma_HalfNormal), sep=""))
        randVec = rhnorm(dataLen, sigma = sigma_HalfNormal[i])
        resHalfNormal[[paste(i)]] = ks.test(dataVec, randVec)
        
        # makeham Distribution
		print(paste("Test with Makeham Distribution : i = ", i, " / ", length(theta_Makeham), sep=""))
        randVec = rmakeham(dataLen, shape = theta_Makeham[i])
        resMakeham[[paste(i)]] = ks.test(dataVec, randVec)
       
        # Rayleigh Distribution
		print(paste("Test with Rayleigh Distribution : i = ", i, " / ", length(scale_Rayleigh), sep=""))
        randVec = rrayleigh(dataLen, sigma = scale_Rayleigh[i])
        resRayleigh[[paste(i)]] = ks.test(dataVec, randVec)
        
        
        # t Distribution
		print(paste("Test with t Distribution : i = ", i, " / ", length(df_T), sep=""))
        randVec = rt(dataLen, df = df_T[i])
        resT[[paste(i)]] = ks.test(dataVec, randVec)
        
        
        for (j in 1:parameterLen)
        {
            # Alpha Distribution
			print(paste("Test with Alpha Distribution : i = ", i, " / ", length(alpha_Alpha), "     j = ", j, " / ", length(beta_Alpha), sep=""))
            randVec = ralpha(dataLen, min=min(dataVec), max=max(dataVec), alpha=alpha_Alpha[i], beta=beta_Alpha[j])
            resAlpha[[paste(i)]][[paste(j)]] = ks.test(dataVec, randVec)
            
            # Arcsine Distribution
            if (alpha_Arcsine[i] != beta_Arcsine[j])
            {
				print(paste("Test with Arcsine Distribution : i = ", i, " / ", length(alpha_Arcsine), "     j = ", j, " / ", length(beta_Arcsine), sep=""))
                randVec = rarcsine(dataLen, min=min(dataVec), max=max(dataVec), alpha=alpha_Arcsine[i], beta=beta_Arcsine[j])
                resBeta[[paste(i)]][[paste(j)]] = ks.test(dataVec, randVec)
            }
            
            # Beta Distribution
			print(paste("Test with Beta Distribution : i = ", i, " / ", length(alpha_Beta), "     j = ", j, " / ", length(beta_Beta), sep=""))
            randVec = rbeta(dataLen, shape1=alpha_Beta[i], shape2=beta_Beta[j])
            resAlpha[[paste(i)]][[paste(j)]] = ks.test(dataVec, randVec)
            
            # Birnbaum-Saunders Distribution
			print(paste("Test with Birnbaum-Saunders Distribution : i = ", i, " / ", length(alpha_BirnbaumSaunders), "     j = ", j, " / ", length(beta_BirnbaumSaunders), sep=""))
            randVec = rfatigue(dataLen, alpha=alpha_BirnbaumSaunders[i], beta=beta_BirnbaumSaunders[j], mu=0)
            resBirnbaumSaunders[[paste(i)]][[paste(j)]] = ks.test(dataVec, randVec)
            
            # Burr Distribution
			print(paste("Test with Burr Distribution : i = ", i, " / ", length(alpha_Burr), "     j = ", j, " / ", length(beta_Burr), sep=""))
            randVec = rburr(dataLen, a=alpha_Burr[i], k=beta_Burr[j])
            resBurr[[paste(i)]][[paste(j)]] = ks.test(dataVec, randVec)

            # Cauchy Distribution       # rcauchy(x, location, scale)
			print(paste("Test with Cauchy Distribution : i = ", i, " / ", length(location_Cauchy), "     j = ", j, " / ", length(scale_Cauchy), sep=""))
            randVec = rcauchy(dataLen, location=location_Cauchy[i], scale=scale_Cauchy[j])
            resCauchy[[paste(i)]][[paste(j)]] = ks.test(dataVec, randVec)

            # Cosine Distribution
			print(paste("Test with Cosine Distribution : i = ", i, " / ", length(mu_Cosine), "     j = ", j, " / ", length(sigma_Cosine), sep=""))
            randVec = rcosine(dataLen, min=min(dataVec), max=max(dataVec), mu=mu_Cosine[i], sigma=sigma_Cosine[j])
            resCosine[[paste(i)]][[paste(j)]] = ks.test(dataVec, randVec)

            # Exponential Distribution with Location Parameter
			print(paste("Test with Exponential Distribution with Location Parameter : i = ", i, " / ", length(alpha_Lexponential), "     j = ", j, " / ", length(beta_Lexponential), sep=""))
			randVec = rlexponential(dataLen, min=min(dataVec), max=max(dataVec), alpha=alpha_Lexponential[i], beta=beta_Lexponential[j])
            resLexponential[[paste(i)]][[paste(j)]] = ks.test(dataVec, randVec)
            
			# 극치 분포: Gumbel 최대값 분포
			print(paste("Test with Gumbel 최대값 분포 : i = ", i, " / ", length(location_Gumbel_max), "     j = ", j, " / ", length(scale_Gumbel_max), sep=""))
			randVec = rgumbel_max(dataLen, min=min(dataVec), max=max(dataVec), location=location_Gumbel_max[i], scale=scale_Gumbel_max[j])
            resGumbel_max[[paste(i)]][[paste(j)]] = ks.test(dataVec, randVec)
			
			# 극치 분포: Gumbel 최소값 분포
			print(paste("Test with Gumbel 최소값 분포 : i = ", i, " / ", length(location_Gumbel_min), "     j = ", j, " / ", length(scale_Gumbel_min), sep=""))
			randVec = rgumbel_min(dataLen, min=min(dataVec), max=max(dataVec), location=location_Gumbel_min[i], scale=scale_Gumbel_min[j])
            resGumbel_min[[paste(i)]][[paste(j)]] = ks.test(dataVec, randVec)
		  
			# F Distribution
			print(paste("Test with F Distribution : i = ", i, " / ", length(df1_F), "     j = ", j, " / ", length(df2_F), sep=""))
			randVec = rf(dataLen, df1=df1_F[i], df2=df2_F[j])
            resF[[paste(i)]][[paste(j)]] = ks.test(dataVec, randVec)
		  
			# Gamma Distribution
			print(paste("Test with F Distribution : i = ", i, " / ", length(shape_Gamma2), "     j = ", j, " / ", length(scale_Gamma2), sep=""))
			randVec = rgamma2(dataLen, shape=shape_Gamma2[i], scale=scale_Gamma2[j])
            resGamma2[[paste(i)]][[paste(j)]] = ks.test(dataVec, randVec)
	
			# Gompertz Distribution
			randVec = rgompertz(dataLen, min=min(dataVec), max=max(dataVec), alpha=alpha_Gompertz[i], beta=beta_Gompertz[j])
            resGompertz[[paste(i)]][[paste(j)]] = ks.test(dataVec, randVec)
            print(paste("Test with Gompertz Distribution : i = ", i, " / ", length(alpha_Gompertz), "     j = ", j, " / ", length(beta_Gompertz), sep=""))
	
            # Half-logistic Distribution
			randVec = rhalflogistic(dataLen, min=min(dataVec), max=max(dataVec), alpha=alpha_HalfLogistic[i], beta=beta_HalfLogistic[j])
            resHalfLogistic[[paste(i)]][[paste(j)]] = ks.test(dataVec, randVec)
            print(paste("Test with Half-logistic Distribution : i = ", i, " / ", length(alpha_HalfLogistic), "     j = ", j, " / ", length(beta_HalfLogistic), sep=""))
			
			# hyperbolicsecant Distribution
			print(paste("Test with hyperbolicsecant Distribution : i = ", i, " / ", length(alpha_Hyperbolicsecant), "     j = ", j, " / ", length(beta_Hyperbolicsecant), sep=""))
			randVec = rhyperbolicsecant(dataLen, min=min(dataVec), max=max(dataVec), alpha=alpha_Hyperbolicsecant[i], beta=beta_Hyperbolicsecant[j])
            resHyperbolicsecant[[paste(i)]][[paste(j)]] = ks.test(dataVec, randVec)
			
            # Inverse-Normal Distribution
			print(paste("Test with Inverse-Normal Distribution : i = ", i, " / ", length(m_InverseNormal), "     j = ", j, " / ", length(s_InverseNormal), sep=""))
			randVec = rinvgauss(dataLen, m=m_InverseNormal[i], s=s_InverseNormal[j])
            resInverseNormal[[paste(i)]][[paste(j)]] = ks.test(dataVec, randVec)
            
            # laplace Distribution
			print(paste("Test with Laplace Distribution : i = ", i, " / ", length(m_Laplace), "     j = ", j, " / ", length(s_Laplace), sep=""))
			randVec = rlaplace(dataLen, m=m_Laplace[i], s=s_Laplace[j])
            resLaplace[[paste(i)]][[paste(j)]] = ks.test(dataVec, randVec)
			
            # 선형 증가 분포
			print(paste("Test with hyperbolicsecant Distribution : i = ", i, " / ", length(alpha_LinearFR), "     j = ", j, " / ", length(beta_LinearFR), sep=""))
			randVec = rlinearFR(dataLen, min=min(dataVec), max=max(dataVec), alpha=alpha_LinearFR[i], beta=beta_LinearFR[j])
            resLinearFR[[paste(i)]][[paste(j)]] = ks.test(dataVec, randVec)
            
			# Logistic Distribution
			print(paste("Test with Logistic Distribution : i = ", i, " / ", length(location_Logistic), "     j = ", j, " / ", length(scale_Logistic), sep=""))
			randVec = rlogis(dataLen, location=location_Logistic[i], scale=scale_Logistic[j])
            resLogistic[[paste(i)]][[paste(j)]] = ks.test(dataVec, randVec)
			
			# log-normal Distribution
			print(paste("Test with Log-normal Distribution : i = ", i, " / ", length(meanlog_LogNormal), "     j = ", j, " / ", length(sdlog_LogNormal), sep=""))
			randVec = rlnorm(dataLen, meanlog=meanlog_LogNormal[i], sdlog=sdlog_LogNormal[j])
            resLogNormal[[paste(i)]][[paste(j)]] = ks.test(dataVec, randVec)
            
			# logweibull Distribution   
			# print(paste("Test with Log-Weibull Distribution : i = ", i, " / ", length(alpha_Logweibull), "     j = ", j, " / ", length(beta_Logweibull), sep=""))
			# randVec = rlogweibull(dataLen, min=min(dataVec), max=max(dataVec), alpha=alpha_Logweibull[i], beta=beta_Logweibull[j])
            # resLogweibull[[paste(i)]][[paste(j)]] = ks.test(dataVec, randVec)
            
			# Normal Distribution
			print(paste("Test with Normal Distribution : i = ", i, " / ", length(mean_Normal), "     j = ", j, " / ", length(sd_Normal), sep=""))
			randVec = rnorm(dataLen, mean=mean_Normal[i], sd=sd_Normal[j])
            resNormal[[paste(i)]][[paste(j)]] = ks.test(dataVec, randVec)
            
            # parabolicInvertedUshaped Distribution
			print(paste("Test with parabolic Inverted U-shaped Distribution : i = ", i, " / ", length(alpha_ParabolicInvertedUshaped), "     j = ", j, " / ", length(beta_ParabolicInvertedUshaped), sep=""))
			randVec = rparabolicInvertedUshaped(dataLen, min=min(dataVec), max=max(dataVec), alpha=alpha_ParabolicInvertedUshaped[i], beta=beta_ParabolicInvertedUshaped[j])
            resParabolicInvertedUshaped[[paste(i)]][[paste(j)]] = ks.test(dataVec, randVec)
			
            # parabolicUshaped Distribution
			print(paste("Test with parabolic U-shaped Distribution : i = ", i, " / ", length(alpha_ParabolicUshaped), "     j = ", j, " / ", length(beta_ParabolicUshaped), sep=""))
			randVec = rparabolicUshaped(dataLen, min=min(dataVec), max=max(dataVec), alpha=alpha_ParabolicUshaped[i], beta=beta_ParabolicUshaped[j])
            resParabolicUshaped[[paste(i)]][[paste(j)]] = ks.test(dataVec, randVec)
            
            # raised cosine Distribution
			print(paste("Test with raised cosine Distribution : i = ", i, " / ", length(mu_Cosine), "     j = ", j, " / ", length(sigma_Cosine), sep=""))
			randVec = rrcosine(dataLen, min=min(dataVec), max=max(dataVec), mu=mu_Cosine[i], sigma=sigma_Cosine[j])
            resRcosine[[paste(i)]][[paste(j)]] = ks.test(dataVec, randVec)
            
			# Reflexted Exponential Distribution
			print(paste("Test with Reflexted Exponential Distribution : i = ", i, " / ", length(alpha_Rexponential), "     j = ", j, " / ", length(beta_Rexponential), sep=""))
			randVec = rrexponential(dataLen, min=min(dataVec), max=max(dataVec), alpha=alpha_Rexponential[i], beta=beta_Rexponential[j])
            resRexponential[[paste(i)]][[paste(j)]] = ks.test(dataVec, randVec)
            
			# semielliptical Distribution
			# print(paste("Test with semi-elliptical Distribution : i = ", i, " / ", length(alpha_Semielliptical), "     j = ", j, " / ", length(beta_Semielliptical), sep=""))
			# randVec = rsemielliptical(dataLen, min=min(dataVec), max=max(dataVec), alpha=alpha_Semielliptical[i], beta=beta_Semielliptical[j])
            # resSemielliptical[[paste(i)]][[paste(j)]] = ks.test(dataVec, randVec)
            
			# teisser Distribution
			# print(paste("Test with Teisser Distribution : i = ", i, " / ", length(alpha_Teisser), "     j = ", j, " / ", length(beta_Teisser), sep=""))
			# randVec = rteisser(dataLen, min=min(dataVec), max=max(dataVec), alpha=alpha_Teisser[i], beta=beta_Teisser[j])
            # resTeisser[[paste(i)]][[paste(j)]] = ks.test(dataVec, randVec)
            
			# Uniform Distribution
			print(paste("Test with Uniform Distribution : i = ", i, " / ", length(alpha_Unif), "     j = ", j, " / ", length(beta_Unif), sep=""))
			randVec = runif(dataLen, alpha_Unif, beta_Unif)
            resUnif[[paste(i)]][[paste(j)]] = ks.test(dataVec, randVec)
            
			# Weibull Distribution with 2 Parameters
			print(paste("Test with Weibull Distribution with 2 Parameters : i = ", i, " / ", length(shape_Weibull), "     j = ", j, " / ", length(scale_Weibull), sep=""))
			randVec = rweibull(dataLen, shape=shape_Weibull[i], scale=scale_Weibull[j])
            resWeibull[[paste(i)]][[paste(j)]] = ks.test(dataVec, randVec)
    
    
            for (k in 1:parameterLen)
            {
				# dhillon1 Distribution
				# print(paste("Test with Dhillon1 Distribution : i = ", i, " / ", length(alpha_Dhillon1), "     j = ", j, " / ", length(beta_Dhillon1), "     k = ", k, " / ", length(gamma_Dhillon1), sep=""))
				# randVec = rdhillon1(dataLen, min=min(dataVec), max=max(dataVec), alpha=alpha_Dhillon1[i], beta=beta_Dhillon1[j], gamma=gamma_Dhillon1[k])
				# resDhillon1[[paste(i)]][[paste(j)]][[paste(k)]] = ks.test(dataVec, randVec)
				
				# dhillon2 Distribution
				# print(paste("Test with Dhillon2 Distribution : i = ", i, " / ", length(alpha_Dhillon2), "     j = ", j, " / ", length(beta_Dhillon2), "     k = ", k, " / ", length(gamma_Dhillon2), sep=""))
				# randVec = rdhillon2(dataLen, min=min(dataVec), max=max(dataVec), alpha=alpha_Dhillon2[i], beta=beta_Dhillon2[j], gamma=gamma_Dhillon2[k])
				# resDhillon2[[paste(i)]][[paste(j)]][[paste(k)]] = ks.test(dataVec, randVec)
				
				# Weibull Distribution with c Parameters
				# print(paste("Test with Weibull Distribution with c Parameters Distribution : i = ", i, " / ", length(c_Dweibull), "     j = ", j, " / ", length(mu_Dweibull), "     k = ", k, " / ", length(sigma_Dweibull), sep=""))
				# randVec = rdweibull(dataLen, min=min(dataVec), max=max(dataVec), c=c_Dweibull[i], mu=mu_Dweibull[j], sigma=sigma_Dweibull[k])
				# resDweibull[[paste(i)]][[paste(j)]][[paste(k)]] = ks.test(dataVec, randVec)
				
				# frechet Distribution
				# print(paste("Test with Frechet Distribution : i = ", i, " / ", length(location_Frechet), "     j = ", j, " / ", length(shape_Frechet), "     k = ", k, " / ", length(scale_Frechet), sep=""))
				# randVec = rfrechet(dataLen, location=location_Frechet[i], shape=shape_Frechet[j], scale=scale_Frechet[k])
				# resFrechet[[paste(i)]][[paste(j)]][[paste(k)]] = ks.test(dataVec, randVec)

				# lfrechet Distribution
				# print(paste("Test with Log-Frechet Distribution : i = ", i, " / ", length(location_Lfrechet), "     j = ", j, " / ", length(shape_Lfrechet), "     k = ", k, " / ", length(scale_Lfrechet), sep=""))
				# randVec = rlfrechet(dataLen, min=min(dataVec), max=max(dataVec), location=location_Lfrechet[i], shape=shape_Lfrechet[j], scale=scale_Lfrechet[k])
				# resLfrechet[[paste(i)]][[paste(j)]][[paste(k)]] = ks.test(dataVec, randVec)
				
				# Gamma Distribution with Location Parameters
				print(paste("Test with Gamma Distribution with Location Parameters : i = ", i, " / ", length(location_Gamma3), "     j = ", j, " / ", length(shape_Gamma3), "     k = ", k, " / ", length(scale_Gamma3), sep=""))
				randVec = rgamma3(dataLen, location=location_Gamma3[i], shape=shape_Gamma3[j], scale=scale_Gamma3[k])
				resGamma3[[paste(i)]][[paste(j)]][[paste(k)]] = ks.test(dataVec, randVec)
				
				# glomax Distribution
				# print(paste("Test with Generalized Lomax Distribution : i = ", i, " / ", length(alpha_Glomax), "     j = ", j, " / ", length(beta_Glomax), "     k = ", k, " / ", length(gamma_Glomax), sep=""))
				# randVec = rglomax(dataLen, min=min(dataVec), max=max(dataVec), alpha=alpha_Glomax[i], beta=beta_Glomax[j], gamma=gamma_Glomax[k])
				# resGlomax[[paste(i)]][[paste(j)]][[paste(k)]] = ks.test(dataVec, randVec)
				
				# Gompertz-Makeham Distribution
				# print(paste("Test with Gompertz-Makeham Distribution : i = ", i, " / ", length(alpha_Gompertzmakeham), "     j = ", j, " / ", length(beta_Gompertzmakeham), "     k = ", k, " / ", length(gamma_Gompertzmakeham), sep=""))
				# randVec = rgompertzmakeham(dataLen, min=min(dataVec), max=max(dataVec), alpha=alpha_Gompertzmakeham[i], beta=beta_Gompertzmakeham[j], gamma=gamma_Gompertzmakeham[k])
				# resGompertzmakeham[[paste(i)]][[paste(j)]][[paste(k)]] = ks.test(dataVec, randVec)
				
				# Hjorth Distribution
				print(paste("Test with Hjorth Distribution : i = ", i, " / ", length(delta_Hjorth), "     j = ", j, " / ", length(beta_Hjorth), "     k = ", k, " / ", length(theta_Hjorth), sep=""))
				randVec = rhjorth(dataLen, m=delta_Hjorth[i], s=beta_Hjorth[j], f=theta_Hjorth[k])
				resHjorth[[paste(i)]][[paste(j)]][[paste(k)]] = ks.test(dataVec, randVec)
			
				# Weibull Distribution with gamma Parameters
				# print(paste("Test with Weibull Distribution with gamma Parameters : i = ", i, " / ", length(alpha_Inverseweibull), "     j = ", j, " / ", length(beta_Inverseweibull), "     k = ", k, " / ", length(gamma_Inverseweibull), sep=""))
                # randVec = rinverseweibull(dataLen, min=min(dataVec), max=max(dataVec), alpha=alpha_Inverseweibull[i], beta=beta_Inverseweibull[j], gamma=gamma_Inverseweibull[k])
				# resInverseweibull[[paste(i)]][[paste(j)]][[paste(k)]] = ks.test(dataVec, randVec)
				
				# loglogistic Distribution
				print(paste("Test with log-Logistic Distribution : i = ", i, " / ", length(alpha_Loglogistic), "     j = ", j, " / ", length(beta_Loglogistic), "     k = ", k, " / ", length(gamma_Loglogistic), sep=""))
				randVec = rloglogistic(dataLen, min=min(dataVec), max=max(dataVec), alpha=alpha_Loglogistic[i], beta=beta_Loglogistic[j], gamma=gamma_Loglogistic[k])
				resLoglogistic[[paste(i)]][[paste(j)]][[paste(k)]] = ks.test(dataVec, randVec)
				
				# lomax Distribution
				print(paste("Test with Lomax Distribution : i = ", i, " / ", length(alpha_Lomax), "     j = ", j, " / ", length(beta_Lomax), "     k = ", k, " / ", length(gamma_Lomax), sep=""))
				randVec = rlomax(dataLen, min=min(dataVec), max=max(dataVec), alpha=alpha_Lomax[i], beta=beta_Lomax[j], gamma=gamma_Lomax[k])
				resLomax[[paste(i)]][[paste(j)]][[paste(k)]] = ks.test(dataVec, randVec)
				
				# Muth Distribution
				# print(paste("Test with Muth Distribution : i = ", i, " / ", length(alpha_Muth), "     j = ", j, " / ", length(beta_Muth), "     k = ", k, " / ", length(gamma_Muth), sep=""))
				# randVec = rmuth(dataLen, min=min(dataVec), max=max(dataVec), alpha=alpha_Muth[i], beta=beta_Muth[j], gamma=gamma_Muth[k])
				# resMuth[[paste(i)]][[paste(j)]][[paste(k)]] = ks.test(dataVec, randVec)

                # Pareto Distribution of the first kind
				print(paste("Test with Pareto Distribution of the first kind : i = ", i, " / ", length(alpha_Pareto1), "     j = ", j, " / ", length(beta_Pareto1), "     k = ", k, " / ", length(gamma_Pareto1), sep=""))
				randVec = rpareto1(dataLen, min=min(dataVec), max=max(dataVec), alpha=alpha_Pareto1[i], beta=beta_Pareto1[j], gamma=gamma_Pareto1[k])
				resPareto1[[paste(i)]][[paste(j)]][[paste(k)]] = ks.test(dataVec, randVec)

                # Power Distribution
				print(paste("Test with Power Distribution : i = ", i, " / ", length(alpha_Power), "     j = ", j, " / ", length(beta_Power), "     k = ", k, " / ", length(gamma_Power), sep=""))
				randVec = rpower(dataLen, min=min(dataVec), max=max(dataVec), alpha=alpha_Power[i], beta=beta_Power[j], gamma=gamma_Power[k])
				resPower[[paste(i)]][[paste(j)]][[paste(k)]] = ks.test(dataVec, randVec)

				# Weibull Distribution with gamma Parameters
				# print(paste("Test with Weibull Distribution with gamma Parameters : i = ", i, " / ", length(alpha_Reflectedweibull), "     j = ", j, " / ", length(beta_Reflectedweibull), "     k = ", k, " / ", length(gamma_Reflectedweibull), sep=""))
				# randVec = rreflectedweibull(dataLen, min=min(dataVec), max=max(dataVec), alpha=alpha_Reflectedweibull[i], beta=beta_Reflectedweibull[j], gamma=gamma_Reflectedweibull[k])
				# resReflectedweibull[[paste(i)]][[paste(j)]][[paste(k)]] = ks.test(dataVec, randVec)
				
				# Weibull Distribution with Location Parameters
				print(paste("Test with Weibull Distribution with Location Parameters : i = ", i, " / ", length(shape_Weibull3), "     j = ", j, " / ", length(scale_Weibull3), "     k = ", k, " / ", length(location_Weibull3), sep=""))
				randVec = rweibull3(dataLen, shape=shape_Weibull3[i], scale=scale_Weibull3[j], location=location_Weibull3[k])
				resWeibull3[[paste(i)]][[paste(j)]][[paste(k)]] = ks.test(dataVec, randVec)
            }
        }
    }

        
	res = list(	
		resChisq = resChisq,		# Chi-square Distribution
		resExponential = resExponential,		# Exponential Distribution
		resHalfCauchy = resHalfCauchy, 		# half-Cahchy Distribution
		resHalfNormal = resHalfNormal,   			# Half-normal Distribution
		resMakeham = resMakeham, 		# makeham Distribution
		resRayleigh = resRayleigh,   		# Rayleigh Distribution
		resT = resT, 		# t Distribution
		resAlpha = resAlpha, 			# Alpha
		resArcsine = resArcsine, 		# Arcsine
		resBeta = resBeta,   		# Beta
		resBirnbaumSaunders = resBirnbaumSaunders,   		# Birnbaum-Saunders Distribution
		resBurr = resBurr,				# Burr Distribution
		resCauchy = resCauchy, 		# Cauchy Distribution
		resCosine = resCosine,  			# Cosine Distribution
		resLexponential = resLexponential,			# Exponential Distribution with Location Parameter
		resGumbel_max = resGumbel_max,  		# 극치 분포: Gumbel 최대값 분포
		resGumbel_min = resGumbel_min, 		# 극치 분포: Gumbel 최소값 분포
		resF = resF,   			# F Distribution
		resGamma2 = resGamma2,  			# Gamma Distribution
		resGompertz = resGompertz, 			# Gompertz Distribution
		resHalfLogistic = resHalfLogistic,  		# Half-logistic Distribution
		resHyperbolicsecant = resHyperbolicsecant,			# hyperbolicsecant Distribution
		resInverseNormal = resInverseNormal,			# Inverse-Normal Distribution
		resLaplace = resLaplace,  			# laplace Distribution
		resLinearFR = resLinearFR,  			# 선형 증가 분포
		resLogistic = resLogistic,   			# Logistic Distribution
		resLogNormal = resLogNormal,   		# log-normal Distribution
		resLogweibull = resLogweibull,   		# logweibull Distribution   
		resNormal = resNormal,   			# Normal Distribution
		resParabolicInvertedUshaped = resParabolicInvertedUshaped,     			# parabolicInvertedUshaped Distribution
		resParabolicUshaped = resParabolicUshaped,			# parabolicUshaped Distribution
		resRcosine = resRcosine,			# rcosine Distribution
		resRexponential = resRexponential,			# Reflexted Exponential Distribution
		resSemielliptical = resSemielliptical,			# semielliptical Distribution
		resTeisser = resTeisser,			# teisser Distribution
		resUnif = resUnif,				# unif2 Distribution
		resWeibull = resWeibull,				# weibull Distribution with 2 Parameters
		resDhillon1 = resDhillon1,			# dhillon1 Distribution
		resDhillon2 = resDhillon2,			# dhillon2 Distribution
		resDweibull = resDweibull,			# weibull Distribution with c Parameters
		resFrechet = resFrechet,				# frechet Distribution
		resLfrechet = resLfrechet,			# lfrechet Distribution
		resGamma3 = resGamma3,			# Gamma Distribution with Location Parameters
		resGlomax = resGlomax,				# glomax Distribution
		resGompertzmakeham = resGompertzmakeham,			# Gompertz-Makeham Distribution
		resHjorth = resHjorth,				# Hjorth Distribution
		resInverseweibull = resInverseweibull,			# Weibull Distribution with gamma Parameters
		resLoglogistic = resLoglogistic,			# loglogistic Distribution
		resLomax = resLomax,			# Lomax Distribution
		resMuth = resMuth,				# Muth Distribution
		resPareto1 = resPareto1,			# Pareto Distribution of the first kind
		resPower = resPower,			# Power Distribution
		resReflectedweibull = resReflectedweibull,			# Weibull Distribution with gamma Parameters
		resWeibull3 = resWeibull3 # Weibull Distribution with Location Parameters
	)
    
    return(res)
}
