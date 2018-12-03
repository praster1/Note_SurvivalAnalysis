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



ksTest = function(dataVec)
{
    dataVec = rnorm(1000)

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
    scale_Gumbel_min = seq(0.01, 10, length=parameterLen)
    
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
    m_InverseNormal = seq(0.01, 10, length=parameterLen);   
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
    alpha_Logweibull = seq(-1, 1, length=parameterLen); 
    beta_Logweibull = seq(0.01, 10, length=parameterLen)
    
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
    resCosine = NULL;
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
    alpha = seq(-1, 1, length=parameterLen)
    beta = seq(0.01, 10, length=parameterLen)
    gamma = seq(0.01, 10, length=parameterLen)

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
        randVec = rhnorm(dataLen, sigma = sigma_HalfNormal)
        resHalfNormal[[i]] = ks.test(dataVec, randVec)
        print(paste("Test with Half-normal Distribution : i = ", i, " / ", length(sigma_HalfNormal), sep=""))
        
        
        

        # makeham Distribution          # rmakeham(x, shape = theta)
        resMakeham = NULL; 
        theta_Makeham = seq(0.01, 10, length=parameterLen)

        # Rayleigh Distribution         # rrayleigh(x, scale = scale)
        resRayleigh = NULL;   
        scale_Rayleigh = seq(0.01, 10, length=parameterLen)

        # t Distribution           # rt(x, df = df)
        resT = NULL; 
        df_T = seq(0.01, 10, length=parameterLen)
    
    
    
        
        
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
                resArcsine[[i]][[j]] = ks.test(dataVec, randVec)
                print(paste("Test with Arcsine Distribution : i = ", i, " / ", length(resArcsine), "     j = ", j, " / ", length(beta_Arcsine), sep=""))
            }
            
            # Beta Distribution
            
            # Birnbaum-Saunders Distribution        # rfatigue(x, alpha, beta, mu = 0)
            
            
                       
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
            scale_Gumbel_min = seq(0.01, 10, length=parameterLen)
            
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
            m_InverseNormal = seq(0.01, 10, length=parameterLen);   
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
            alpha_Logweibull = seq(-1, 1, length=parameterLen); 
            beta_Logweibull = seq(0.01, 10, length=parameterLen)
            
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
            resCosine = NULL;
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
    
    
    
            for (k in 1:parameterLen)
            {
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
                alpha = seq(-1, 1, length=parameterLen)
                beta = seq(0.01, 10, length=parameterLen)
                gamma = seq(0.01, 10, length=parameterLen)

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

            }
            
        }
    }


    
    return(res)
}
