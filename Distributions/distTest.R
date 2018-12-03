rm(list = ls())

setwd("/home/lv999/Dropbox/Github/SurvivalAnalysis/Distributions")


source("colorPalette.R")

### Distributions
source("alphaDistribution.R")
source("arcsineDistribution.R")
source("betaDistribution.R")
source("Birnbaum-SaundersDistribution.R")
source("BurrDistribution.R")
source("CauchyDistribution.R")
source("chiDistribution.R")
source("chisquareDistribution.R")
source("cosineDistribution.R")
source("Dhillon's1Distribution.R")
source("Dhillon's2Distribution.R")
source("doubleWeibullDistribution.R")
source("exponentialDistribution.R")
source("exponentialDistributionwithLocationParameter.R")
source("extremeValueDistribution_1(GumbelMaxDistribution).R")
source("extremeValueDistribution_1(GumbelMinDistribution).R")
source("extremeValueDistribution_2(FrechetDistribution).R")
source("extremeValueDistribution_2(FrechetDistributionwithLocationParameter).R")
source("FDistribution.R")
source("GammaDistributionwith2Parameters.R")
source("GammaDistributionwith3Parameters.R")
source("generalizedLomaxDistribution.R")
source("GompertzDistribution.R")
source("GompertzMakehamDistribution.R")
source("halfCauchyDistribution.R")
source("halfLogisticDistribution.R")
source("halfNormalDistribution.R")
source("hjorthDistribution.R")
source("hyperbolicSecantDistribution.R")
source("inverseNormalDistribution.R")
source("inverseRayleighDistribution.R")
source("inverseWeibullDistribution.R")
source("laplaceDistribution.R")
source("linearHazardRateDistribution.R")
source("logisticDistribution.R")
source("logLogisticDistribution.R")
source("lognormalDistribution.R")
source("logWeibullDistribution.R")
source("lomaxDistribution.R")
source("makehamDistribution.R")
source("muthDistribution.R")
source("normalDistribution.R")
source("parabolicInvertedUshapedDistribution.R")
source("parabolicUshapedDistribution.R")
source("paretoDistributionoftheFirstKind.R")
source("powerDistribution.R")
source("raisedCosineDistribution.R")
source("rayleighDistribution.R")
source("reflectedExponentialDistribution.R")
source("reflectedWeibullDistribution.R")
source("semiEllipticalDistribution.R")
source("tDistribution.R")
source("teisserDistribution.R")
source("uniformDistribution.R")
source("WeibullDistributionwith2Parameters.R")
source("WeibullDistributionwith3Parameters.R")
source("_exponentiatedExponentialDistribution.R")
source("_generalizedExponentialGeometricDistribution.R")
source("_generalizedGammaDistribution.R")
source("_generalizedLinearHazardRateDistribution.R")
source("_generalizedLogisticDistribution.R")
source("_generalizedRayleighDistribution.R")
source("_logGammaDistribution.R")
source("_logLaplaceDistribution.R")
source("_lognormalDistributionwithLowerThreshold.R")
source("_lognormalDistributionwithUpperThreshold.R")
source("_Maxwell-BoltzmannDistribution.R")
source("_TriangularDistribution.R")
source("_trimmedNormalDistribution.R")
source("_VshapedDistribution.R")


<<<<<<< HEAD
source("ksTest.R")
=======



ksTest = function(dataVec = NULL)
{
	if(is.null(dataVec))
	{
		dataVec = rnorm(1000)
	}

    normalization = function(x)	{	(x-min(x))/(max(x)-min(x));	}
    dataVec = normalization(dataVec) + abs(rnorm(dataVec, 0, 0.0001))
    dataLen = length(dataVec)
    
    parameterLen = 100
    
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
        randVec = rchisq(dataLen, df=df_Chisq[i])
        resChisq[[i]] = ks.test(dataVec, randVec)
        print(paste("Test with Chi-square Distribution : i = ", i, " / ", length(df_Chisq), sep=""))
        
        # Exponential Distribution 
        randVec = rexponential(dataLen, min=min(dataVec), max=max(dataVec), lambda=lambda_Exponential[i])
        resExponential[[i]] = ks.test(dataVec, randVec)
        print(paste("Test with Exponential Distribution : i = ", i, " / ", length(lambda_Exponential), sep=""))
        
        # Half-Cahchy Distribution
        randVec = rhcauchy(dataLen, sigma=sigma_HalfCauchy[i])
        resHalfCauchy[[i]] = ks.test(dataVec, randVec)
        print(paste("Test with Half-Cahchy Distribution : i = ", i, " / ", length(sigma_HalfCauchy), sep=""))
        
        # Half-normal Distribution
        randVec = rhnorm(dataLen, sigma = sigma_HalfNormal[i])
        resHalfNormal[[i]] = ks.test(dataVec, randVec)
        print(paste("Test with Half-normal Distribution : i = ", i, " / ", length(sigma_HalfNormal), sep=""))
        
        # makeham Distribution
        randVec = rmakeham(dataLen, shape = theta_Makeham[i])
        resMakeham[[i]] = ks.test(dataVec, randVec)
        print(paste("Test with Makeham Distribution : i = ", i, " / ", length(theta_Makeham), sep=""))

        # Rayleigh Distribution
        randVec = rrayleigh(dataLen, sigma = scale_Rayleigh[i])
        resRayleigh[[i]] = ks.test(dataVec, randVec)
        print(paste("Test with Rayleigh Distribution : i = ", i, " / ", length(scale_Rayleigh), sep=""))
        
        # t Distribution
        randVec = rt(dataLen, df = df_T[i])
        resT[[i]] = ks.test(dataVec, randVec)
        print(paste("Test with t Distribution : i = ", i, " / ", length(df_T), sep=""))
        
        
        for (j in 1:parameterLen)
        {
            # Alpha Distribution
            randVec = ralpha(dataLen, min=min(dataVec), max=max(dataVec), alpha=alpha_Alpha[i], beta=beta_Alpha[j])
            resAlpha[[i]][[j]] = ks.test(dataVec, randVec)
            print(paste("Test with Alpha Distribution : i = ", i, " / ", length(alpha_Alpha), "     j = ", j, " / ", length(beta_Alpha), sep=""))
            
            # Arcsine Distribution
            if (alpha_Arcsine[i] != beta_Arcsine[j])
            {
                randVec = rarcsine(dataLen, min=min(dataVec), max=max(dataVec), alpha=alpha_Arcsine[i], beta=beta_Arcsine[j])
                resBeta[[i]][[j]] = ks.test(dataVec, randVec)
                print(paste("Test with Arcsine Distribution : i = ", i, " / ", length(alpha_Arcsine), "     j = ", j, " / ", length(beta_Arcsine), sep=""))
            }
            
            # Beta Distribution
            randVec = rbeta(dataLen, shape1=alpha_Beta[i], shape2=beta_Beta[j])
            resAlpha[[i]][[j]] = ks.test(dataVec, randVec)
            print(paste("Test with Beta Distribution : i = ", i, " / ", length(alpha_Beta), "     j = ", j, " / ", length(beta_Beta), sep=""))
            
            # Birnbaum-Saunders Distribution
            randVec = rfatigue(dataLen, alpha=alpha_BirnbaumSaunders[i], beta=beta_BirnbaumSaunders[j], mu=0)
            resBirnbaumSaunders[[i]][[j]] = ks.test(dataVec, randVec)
            print(paste("Test with Birnbaum-Saunders Distribution : i = ", i, " / ", length(alpha_BirnbaumSaunders), "     j = ", j, " / ", length(beta_BirnbaumSaunders), sep=""))
            
            # Burr Distribution
            randVec = rburr(dataLen, a=alpha_Burr[i], k=beta_Burr[j])
            resBurr[[i]][[j]] = ks.test(dataVec, randVec)
            print(paste("Test with Burr Distribution : i = ", i, " / ", length(alpha_Burr), "     j = ", j, " / ", length(beta_Burr), sep=""))

            # Cauchy Distribution       # rcauchy(x, location, scale)
            randVec = rcauchy(dataLen, location=location_Cauchy[i], scale=scale_Cauchy[j])
            resCauchy[[i]][[j]] = ks.test(dataVec, randVec)
            print(paste("Test with Cauchy Distribution : i = ", i, " / ", length(location_Cauchy), "     j = ", j, " / ", length(scale_Cauchy), sep=""))

            # Cosine Distribution
            randVec = rcosine(dataLen, min=min(dataVec), max=max(dataVec), mu=mu_Cosine[i], sigma=sigma_Cosine[j])
            resCosine[[i]][[j]] = ks.test(dataVec, randVec)
            print(paste("Test with Cosine Distribution : i = ", i, " / ", length(mu_Cosine), "     j = ", j, " / ", length(sigma_Cosine), sep=""))

            # Exponential Distribution with Location Parameter
			randVec = rlexponential(dataLen, min=min(dataVec), max=max(dataVec), alpha=alpha_Lexponential[i], beta=beta_Lexponential[j])
            resLexponential[[i]][[j]] = ks.test(dataVec, randVec)
            print(paste("Test with Exponential Distribution with Location Parameter : i = ", i, " / ", length(alpha_Lexponential), "     j = ", j, " / ", length(beta_Lexponential), sep=""))
            
			# 극치 분포: Gumbel 최대값 분포
			randVec = rgumbel_max(dataLen, min=min(dataVec), max=max(dataVec), location=location_Gumbel_max[i], scale=scale_Gumbel_max[j])
            resGumbel_max[[i]][[j]] = ks.test(dataVec, randVec)
            print(paste("Test with Gumbel 최대값 분포 : i = ", i, " / ", length(location_Gumbel_max), "     j = ", j, " / ", length(scale_Gumbel_max), sep=""))
			
			# 극치 분포: Gumbel 최소값 분포
			randVec = rgumbel_min(dataLen, min=min(dataVec), max=max(dataVec), location=location_Gumbel_min[i], scale=scale_Gumbel_min[j])
            resGumbel_min[[i]][[j]] = ks.test(dataVec, randVec)
            print(paste("Test with Gumbel 최소값 분포 : i = ", i, " / ", length(location_Gumbel_min), "     j = ", j, " / ", length(scale_Gumbel_min), sep=""))
		  
			# F Distribution
			randVec = rf(dataLen, df1=df1_F[i], df2=df2_F[j])
            resF[[i]][[j]] = ks.test(dataVec, randVec)
            print(paste("Test with F Distribution : i = ", i, " / ", length(df1_F), "     j = ", j, " / ", length(df2_F), sep=""))
		  
			# Gamma Distribution
			randVec = rgamma2(dataLen, shape=shape_Gamma2[i], scale=scale_Gamma2[j])
            resGamma2[[i]][[j]] = ks.test(dataVec, randVec)
            print(paste("Test with F Distribution : i = ", i, " / ", length(shape_Gamma2), "     j = ", j, " / ", length(scale_Gamma2), sep=""))
	
			# Gompertz Distribution
			randVec = rgompertz(dataLen, min=min(dataVec), max=max(dataVec), alpha=alpha_Gompertz[i], beta=beta_Gompertz[j])
            resGompertz[[i]][[j]] = ks.test(dataVec, randVec)
            print(paste("Test with Gompertz Distribution : i = ", i, " / ", length(alpha_Gompertz), "     j = ", j, " / ", length(beta_Gompertz), sep=""))
	
            # Half-logistic Distribution
			randVec = rhalflogistic(dataLen, min=min(dataVec), max=max(dataVec), alpha=alpha_HalfLogistic[i], beta=beta_HalfLogistic[j])
            resHalfLogistic[[i]][[j]] = ks.test(dataVec, randVec)
            print(paste("Test with Half-logistic Distribution : i = ", i, " / ", length(alpha_HalfLogistic), "     j = ", j, " / ", length(beta_HalfLogistic), sep=""))
			
			# hyperbolicsecant Distribution
			randVec = rhyperbolicsecant(dataLen, min=min(dataVec), max=max(dataVec), alpha=alpha_Hyperbolicsecant[i], beta=beta_Hyperbolicsecant[j])
            resHyperbolicsecant[[i]][[j]] = ks.test(dataVec, randVec)
            print(paste("Test with hyperbolicsecant Distribution : i = ", i, " / ", length(alpha_Hyperbolicsecant), "     j = ", j, " / ", length(beta_Hyperbolicsecant), sep=""))
			
            # Inverse-Normal Distribution
			randVec = rinvgauss(dataLen, m=m_InverseNormal[i], s=s_InverseNormal[j])
            resInverseNormal[[i]][[j]] = ks.test(dataVec, randVec)
            print(paste("Test with Inverse-Normal Distribution : i = ", i, " / ", length(m_InverseNormal), "     j = ", j, " / ", length(s_InverseNormal), sep=""))
            
            # laplace Distribution
			randVec = rlaplace(dataLen, m=m_Laplace[i], s=s_Laplace[j])
            resLaplace[[i]][[j]] = ks.test(dataVec, randVec)
            print(paste("Test with Laplace Distribution : i = ", i, " / ", length(m_Laplace), "     j = ", j, " / ", length(s_Laplace), sep=""))
			
            # 선형 증가 분포
			randVec = rlinearFR(dataLen, min=min(dataVec), max=max(dataVec), alpha=alpha_LinearFR[i], beta=beta_LinearFR[j])
            resLinearFR[[i]][[j]] = ks.test(dataVec, randVec)
            print(paste("Test with hyperbolicsecant Distribution : i = ", i, " / ", length(alpha_LinearFR), "     j = ", j, " / ", length(beta_LinearFR), sep=""))
            
			# Logistic Distribution
			randVec = rlogis(dataLen, location=location_Logistic[i], scale=scale_Logistic[j])
            resLogistic[[i]][[j]] = ks.test(dataVec, randVec)
            print(paste("Test with Logistic Distribution : i = ", i, " / ", length(location_Logistic), "     j = ", j, " / ", length(scale_Logistic), sep=""))
			
			# log-normal Distribution
			randVec = rlnorm(dataLen, meanlog=meanlog_LogNormal[i], sdlog=sdlog_LogNormal[j])
            resLogNormal[[i]][[j]] = ks.test(dataVec, randVec)
            print(paste("Test with Log-normal Distribution : i = ", i, " / ", length(meanlog_LogNormal), "     j = ", j, " / ", length(sdlog_LogNormal), sep=""))
            
			# logweibull Distribution   
			randVec = rlogweibull(dataLen, min=min(dataVec), max=max(dataVec), alpha=alpha_Logweibull[i], beta=beta_Logweibull[j])
            resLogweibull[[i]][[j]] = ks.test(dataVec, randVec)
            print(paste("Test with Log-Weibull Distribution : i = ", i, " / ", length(alpha_Logweibull), "     j = ", j, " / ", length(beta_Logweibull), sep=""))
            
			# Normal Distribution
			randVec = rnorm(dataLen, mean=mean_Normal[i], sd=sd_Normal[j])
            resNormal[[i]][[j]] = ks.test(dataVec, randVec)
            print(paste("Test with Normal Distribution : i = ", i, " / ", length(mean_Normal), "     j = ", j, " / ", length(sd_Normal), sep=""))
			
            # parabolicInvertedUshaped Distribution
			randVec = rparabolicInvertedUshaped(dataLen, min=min(dataVec), max=max(dataVec), alpha=alpha_ParabolicInvertedUshaped[i], beta=beta_ParabolicInvertedUshaped[j])
            resParabolicInvertedUshaped[[i]][[j]] = ks.test(dataVec, randVec)
            print(paste("Test with parabolic Inverted U-shaped Distribution : i = ", i, " / ", length(alpha_ParabolicInvertedUshaped), "     j = ", j, " / ", length(beta_ParabolicInvertedUshaped), sep=""))
			
            # parabolicUshaped Distribution
			randVec = rparabolicUshaped(dataLen, min=min(dataVec), max=max(dataVec), alpha=alpha_ParabolicUshaped[i], beta=beta_ParabolicUshaped[j])
            resParabolicUshaped[[i]][[j]] = ks.test(dataVec, randVec)
            print(paste("Test with parabolic U-shaped Distribution : i = ", i, " / ", length(alpha_ParabolicUshaped), "     j = ", j, " / ", length(beta_ParabolicUshaped), sep=""))
            
            # raised cosine Distribution
			randVec = rrcosine(dataLen, min=min(dataVec), max=max(dataVec), mu=mu_Cosine[i], sigma=sigma_Cosine[j])
            resRcosine[[i]][[j]] = ks.test(dataVec, randVec)
            print(paste("Test with raised cosine Distribution : i = ", i, " / ", length(mu_Cosine), "     j = ", j, " / ", length(sigma_Cosine), sep=""))
			
			# Reflexted Exponential Distribution
			randVec = rrexponential(dataLen, min=min(dataVec), max=max(dataVec), alpha=alpha_Rexponential[i], beta=beta_Rexponential[j])
            resRexponential[[i]][[j]] = ks.test(dataVec, randVec)
            print(paste("Test with Reflexted Exponential Distribution : i = ", i, " / ", length(alpha_Rexponential), "     j = ", j, " / ", length(beta_Rexponential), sep=""))
			
			# semielliptical Distribution
			# randVec = rsemielliptical(dataLen, min=min(dataVec), max=max(dataVec), alpha=alpha_Semielliptical[i], beta=beta_Semielliptical[j])
            # resSemielliptical[[i]][[j]] = ks.test(dataVec, randVec)
            # print(paste("Test with semi-elliptical Distribution : i = ", i, " / ", length(alpha_Semielliptical), "     j = ", j, " / ", length(beta_Semielliptical), sep=""))
            
			# teisser Distribution
			randVec = rteisser(dataLen, min=min(dataVec), max=max(dataVec), alpha=alpha_Teisser[i], beta=beta_Teisser[j])
            resTeisser[[i]][[j]] = ks.test(dataVec, randVec)
            print(paste("Test with Teisser Distribution : i = ", i, " / ", length(alpha_Teisser), "     j = ", j, " / ", length(beta_Teisser), sep=""))

			# Normal Distribution
			randVec = runif(dataLen, alpha_Unif, beta_Unif)
            resUnif[[i]][[j]] = ks.test(dataVec, randVec)
            print(paste("Test with Uniform Distribution : i = ", i, " / ", length(alpha_Unif), "     j = ", j, " / ", length(beta_Unif), sep=""))
            
			# weibull Distribution with 2 Parameters
			randVec = rweibull(dataLen, min=min(dataVec), max=max(dataVec), shape=shape_Weibull[i], scale=scale_Weibull[j])
            resWeibull[[i]][[j]] = ks.test(dataVec, randVec)
            print(paste("Test with Weibull Distribution with 2 Parameters : i = ", i, " / ", length(shape_Weibull), "     j = ", j, " / ", length(scale_Weibull), sep=""))
            
    
    
            for (k in 1:parameterLen)
            {
				# dhillon1 Distribution
				randVec = rdhillon1(dataLen, min=min(dataVec), max=max(dataVec), alpha=alpha_Dhillon1[i], beta=beta_Dhillon1[j], gamma=gamma_Dhillon1[k])
				resDhillon1[[i]][[j]][[k]] = ks.test(dataVec, randVec)
				print(paste("Test with Dhillon1 Distribution : i = ", i, " / ", length(alpha_Dhillon1), "     j = ", j, " / ", length(beta_Dhillon1), "     k = ", k, " / ", length(gamma_Dhillon1), sep=""))
				
				# dhillon2 Distribution
				randVec = rdhillon2(dataLen, min=min(dataVec), max=max(dataVec), alpha=alpha_Dhillon2[i], beta=beta_Dhillon2[j], gamma=gamma_Dhillon2[k])
				resDhillon2[[i]][[j]][[k]] = ks.test(dataVec, randVec)
				print(paste("Test with Dhillon2 Distribution : i = ", i, " / ", length(alpha_Dhillon2), "     j = ", j, " / ", length(beta_Dhillon2), "     k = ", k, " / ", length(gamma_Dhillon2), sep=""))
			
				# weibull Distribution with c Parameters
				randVec = rdweibull(dataLen, min=min(dataVec), max=max(dataVec), c=c_Dweibull[i], mu=mu_Dweibull[j], sigma=sigma_Dweibull[k])
				resDweibull[[i]][[j]][[k]] = ks.test(dataVec, randVec)
				print(paste("Test with Weibull Distribution with c Parameters Distribution : i = ", i, " / ", length(c_Dweibull), "     j = ", j, " / ", length(mu_Dweibull), "     k = ", k, " / ", length(sigma_Dweibull), sep=""))
                
				# frechet Distribution
				randVec = rfrechet(dataLen, location=location_Frechet[i], shape=shape_Frechet[j], scale=scale_Frechet[k])
				resFrechet[[i]][[j]][[k]] = ks.test(dataVec, randVec)
				print(paste("Test with Frechet Distribution : i = ", i, " / ", length(location_Frechet), "     j = ", j, " / ", length(shape_Frechet), "     k = ", k, " / ", length(scale_Frechet), sep=""))

				# lfrechet Distribution
				randVec = rlfrechet(dataLen, min=min(dataVec), max=max(dataVec), location=location_Lfrechet[i], shape=shape_Lfrechet[j], scale=scale_Lfrechet[k])
				resLfrechet[[i]][[j]][[k]] = ks.test(dataVec, randVec)
				print(paste("Test with Log-Frechet Distribution : i = ", i, " / ", length(location_Lfrechet), "     j = ", j, " / ", length(shape_Lfrechet), "     k = ", k, " / ", length(scale_Lfrechet), sep=""))
				
				# Gamma Distribution with Location Parameters
				randVec = rgamma3(dataLen, location=location_Gamma3[i], shape=shape_Gamma3[j], scale=scale_Gamma3[k])
				resGamma3[[i]][[j]][[k]] = ks.test(dataVec, randVec)
				print(paste("Test with Gamma Distribution with Location Parameters : i = ", i, " / ", length(location_Gamma3), "     j = ", j, " / ", length(shape_Gamma3), "     k = ", k, " / ", length(scale_Gamma3), sep=""))
				
				# glomax Distribution
				randVec = rglomax(dataLen, min=min(dataVec), max=max(dataVec), alpha=alpha_Glomax[i], beta=beta_Glomax[j], gamma=gamma_Glomax[k])
				resGlomax[[i]][[j]][[k]] = ks.test(dataVec, randVec)
				print(paste("Test with Generalized Lomax Distribution : i = ", i, " / ", length(alpha_Glomax), "     j = ", j, " / ", length(beta_Glomax), "     k = ", k, " / ", length(gamma_Glomax), sep=""))
				
				# Gompertz-Makeham Distribution
				randVec = rgompertzmakeham(dataLen, min=min(dataVec), max=max(dataVec), alpha=alpha_Gompertzmakeham[i], beta=beta_Gompertzmakeham[j], gamma=gamma_Gompertzmakeham[k])
				resGompertzmakeham[[i]][[j]][[k]] = ks.test(dataVec, randVec)
				print(paste("Test with Gompertz-Makeham Distribution : i = ", i, " / ", length(alpha_Gompertzmakeham), "     j = ", j, " / ", length(beta_Gompertzmakeham), "     k = ", k, " / ", length(gamma_Gompertzmakeham), sep=""))
				
				# Hjorth Distribution
				randVec = rhjorth(dataLen, m=delta_Hjorth[i], s=beta_Hjorth[j], f=theta_Hjorth[k])
				resHjorth[[i]][[j]][[k]] = ks.test(dataVec, randVec)
				print(paste("Test with Hjorth Distribution : i = ", i, " / ", length(delta_Hjorth), "     j = ", j, " / ", length(beta_Hjorth), "     k = ", k, " / ", length(theta_Hjorth), sep=""))

				# weibull Distribution with gamma Parameters
                randVec = rinverseweibull(dataLen, min=min(dataVec), max=max(dataVec), alpha=alpha_Inverseweibull[i], beta=beta_Inverseweibull[j], gamma=gamma_Inverseweibull[k])
				resInverseweibull[[i]][[j]][[k]] = ks.test(dataVec, randVec)
				print(paste("Test with Weibull Distribution with gamma Parameters : i = ", i, " / ", length(alpha_Inverseweibull), "     j = ", j, " / ", length(beta_Inverseweibull), "     k = ", k, " / ", length(gamma_Inverseweibull), sep=""))
				
				# loglogistic Distribution
				randVec = rloglogistic(dataLen, min=min(dataVec), max=max(dataVec), alpha=alpha_Loglogistic[i], beta=beta_Loglogistic[j], gamma=gamma_Loglogistic[k])
				resLoglogistic[[i]][[j]][[k]] = ks.test(dataVec, randVec)
				print(paste("Test with log-Logistic Distribution : i = ", i, " / ", length(alpha_Loglogistic), "     j = ", j, " / ", length(beta_Loglogistic), "     k = ", k, " / ", length(gamma_Loglogistic), sep=""))
				
				# lomax Distribution
				randVec = rlomax(dataLen, min=min(dataVec), max=max(dataVec), alpha=alpha_Lomax[i], beta=beta_Lomax[j], gamma=gamma_Lomax[k])
				resLomax[[i]][[j]][[k]] = ks.test(dataVec, randVec)
				print(paste("Test with Lomax Distribution : i = ", i, " / ", length(alpha_Lomax), "     j = ", j, " / ", length(beta_Lomax), "     k = ", k, " / ", length(gamma_Lomax), sep=""))
				
				# Muth Distribution
				randVec = rmuth(dataLen, min=min(dataVec), max=max(dataVec), alpha=alpha_Muth[i], beta=beta_Muth[j], gamma=gamma_Muth[k])
				resMuth[[i]][[j]][[k]] = ks.test(dataVec, randVec)
				print(paste("Test with Lomax Distribution : i = ", i, " / ", length(alpha_Muth), "     j = ", j, " / ", length(beta_Muth), "     k = ", k, " / ", length(gamma_Muth), sep=""))

                # Pareto Distribution of the first kind
				randVec = rpareto1(dataLen, min=min(dataVec), max=max(dataVec), alpha=alpha_Pareto1[i], beta=beta_Pareto1[j], gamma=gamma_Pareto1[k])
				resPareto1[[i]][[j]][[k]] = ks.test(dataVec, randVec)
				print(paste("Test with Pareto Distribution of the first kind : i = ", i, " / ", length(alpha_Pareto1), "     j = ", j, " / ", length(beta_Pareto1), "     k = ", k, " / ", length(gamma_Pareto1), sep=""))

                # Power Distribution
				randVec = rpower(dataLen, min=min(dataVec), max=max(dataVec), alpha=alpha_Power[i], beta=beta_Power[j], gamma=gamma_Power[k])
				resPower[[i]][[j]][[k]] = ks.test(dataVec, randVec)
				print(paste("Test with Power Distribution : i = ", i, " / ", length(alpha_Power), "     j = ", j, " / ", length(beta_Power), "     k = ", k, " / ", length(gamma_Power), sep=""))

				# Weibull Distribution with gamma Parameters
				randVec = rreflectedweibull(dataLen, min=min(dataVec), max=max(dataVec), alpha=alpha_Reflectedweibull[i], beta=beta_Reflectedweibull[j], gamma=gamma_Reflectedweibull[k])
				resReflectedweibull[[i]][[j]][[k]] = ks.test(dataVec, randVec)
				print(paste("Test with Weibull Distribution with gamma Parameters : i = ", i, " / ", length(alpha_Reflectedweibull), "     j = ", j, " / ", length(beta_Reflectedweibull), "     k = ", k, " / ", length(gamma_Reflectedweibull), sep=""))
				
				# Weibull Distribution with Location Parameters
				randVec = rweibull3(dataLen, shape=shape_Weibull3[i], scale=scale_Weibull3[j], location=location_Weibull3[k])
				resWeibull3[[i]][[j]][[k]] = ks.test(dataVec, randVec)
				print(paste("Test with Weibull Distribution with Location Parameters : i = ", i, " / ", length(shape_Weibull3), "     j = ", j, " / ", length(scale_Weibull3), "     k = ", k, " / ", length(location_Weibull3), sep=""))
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
		# resSemielliptical = resSemielliptical,			# semielliptical Distribution
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


>>>>>>> 0e77133343f5fed8fe94b6aae9b6d6b83b9fe3f4
res = ksTest(rnorm(1000))