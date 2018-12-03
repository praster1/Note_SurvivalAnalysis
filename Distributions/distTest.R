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
    dataVec = iris[,1]

    normalization = function(x)	{	(x-min(x))/(max(x)-min(x));	}
    dataVec = normalization(dataVec) + 1
    dataLen = length(dataVec)
    
    parameterLen = 100
    
    ### 1 parameter
    ##### Chi-square Distribution       # rchisq(x, df = df)
    resChisq = NULL;   
    df_chisq = seq(0.01, 10, length=parameterLen)

    ##### Exponential Distribution              # rexponential = function(n, min=0.1, max=10, lambda = 1)
    resExponential = NULL;
    lambda_exponential = seq(0.01, 10, length=parameterLen)

    ##### half-Cahchy Distribution          # rhcauchy(x, sigma = sigma)
    resHalfCauchy = NULL; 
    sigma_halfCauchy = seq(0.01, 10, length=parameterLen)

    ##### Half-normal Distribution           # rhnorm(x, sigma = 1)
    resHalfNormal = NULL;   
    sigma_halfNormal = seq(0.01, 10, length=parameterLen)

    ##### makeham Distribution          # rmakeham(x, shape = theta)
    resMakeham = NULL; 
    theta_makeham = seq(0.01, 10, length=parameterLen)

    ##### Rayleigh Distribution         # rrayleigh(x, scale = scale)
    resRayleigh = NULL;   
    scale_rayleigh = seq(0.01, 10, length=parameterLen)

    ##### t Distribution           # rt(x, df = df)
    resT = NULL; 
    df_t = seq(0.01, 10, length=parameterLen)

    
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
    
    ##### Cosine Distribution           # rcosine = function(n, min=-10, max=10, mu = 0, sigma = 1)
    resCosine = NULL;  
    mu_Cosine = seq(-1, 1, length=parameterLen);
    sigma_Cosine = seq(0.01, 10, length=parameterLen)
    
    ##### Exponential Distribution with Location Parameter      # rlexponential = function(n, min=0.1, max=1, alpha = 1, beta = 1)
    resLexponential = NULL;
    alpha_Lexponential = seq(-1, 1, length=parameterLen);   
    beta_Lexponential = seq(0.01, 10, length=parameterLen)
    
    ##### 극치 분포: Gumbel 최대값 분포          # rgumbel_max = function (n, min=-10, max=10, scale = 1, location = 0) 
    resGumbel_max = NULL;  
    location_Gumbel_max = seq(-1, 1, length=parameterLen); 
    scale_Gumbel_max = seq(0.01, 10, length=parameterLen)
    
    ##### 극치 분포: Gumbel 최소값 분포          # rgumbel_min = function (n, min=-10, max=10, scale = 1, location = 0) 
    resGumbel_min = NULL; 
    location_Gumbel_min = seq(-1, 1, length=parameterLen);  
    scale_Gumbel_min = seq(0.01, 10, length=parameterLen)
    
    ##### F Distribution                # rf(x, df1 = df1, df2 = df2)
    resF = NULL;   
    df1_F = seq(0.01, 10, length=parameterLen); 
    df2_F = seq(0.01, 10, length=parameterLen)
    
    ##### Gamma Distribution        # rgamma2 = function(x, shape=shape, scale=scale)
    resGamma2 = NULL;  
    shape_Gamma2 = seq(0.01, 10, length=parameterLen);  
    scale_Gamma2 = seq(0.01, 10, length=parameterLen)
    
    ##### Gompertz Distribution         # rgompertz = function (n, min=0.0001, max=1, alpha = 1, beta = 1) 
    resGompertz = NULL; 
    alpha_Gompertz = seq(0.01, 10, length=parameterLen); 
    beta_Gompertz = seq(0.01, 10, length=parameterLen)
    
    ##### Half-logistic Distribution            # rhalflogistic = function (n, min=0.0001, max=10, alpha = 1, beta = 1) 
    resHalfLogistic = NULL; 
    alpha_HalfLogistic = seq(-1, 1, length=parameterLen);  
    beta_HalfLogistic = seq(0.01, 10, length=parameterLen)
    
    ##### hyperbolicsecant Distribution     # rhyperbolicsecant = function (n, min=-10, max=10, alpha = 1, beta = 1) 
    resHyperbolicsecant = NULL;
    alpha_Hyperbolicsecant = seq(-1, 1, length=parameterLen); 
    beta_Hyperbolicsecant = seq(0.01, 10, length=parameterLen)
    
    ##### Inverse-Normal Distribution           # rinvgauss(x, m = 1, s = 1)
    resInverseNormal = NULL;
    m_InverseNormal = seq(0.01, 10, length=parameterLen);   
    s_InverseNormal = seq(0.01, 10, length=parameterLen)
    
    ##### laplace Distribution          # rlaplace(x, m = 0, s = 1)
    resLaplace = NULL;  
    m_Laplace = seq(-1, 1, length=parameterLen);  
    s_Laplace = seq(0.01, 10, length=parameterLen)
    
    ##### 선형 증가 분포          # rlinearFR = function (n, min=0.0001, max=10, alpha = 1, beta = 1) 
    resLinearFR = NULL;  
    alpha_LinearFR = seq(0.01, 10, length=parameterLen);  
    beta_LinearFR = seq(0.01, 10, length=parameterLen)
    
    ##### Logistic Distribution         # rlogis(x, location, scale)
    resLogistic = NULL;   
    location_Logistic = seq(-1, 1, length=parameterLen); 
    scale_Logistic = seq(0.01, 10, length=parameterLen)
    
    ##### log-normal Distribution           # rlnorm(x, meanlog = meanlog, sdlog = sdlog)
    resLogNormal = NULL;   
    meanlog_LogNormal = seq(-1, 1, length=parameterLen); 
    sdlog_LogNormal = seq(0.01, 10, length=parameterLen)
    
    ##### logweibull Distribution   
    resLogweibull = NULL;   
    alpha_Logweibull = seq(-1, 1, length=parameterLen); 
    beta_Logweibull = seq(0.01, 10, length=parameterLen)
    
    ##### Normal Distribution           # rnorm(x, mean = mean, sd = sd)
    resNormal = NULL;   
    mean_Normal = seq(-1, 1, length=parameterLen);  
    sd_Normal = seq(0.01, 10, length=parameterLen)
    
    ##### parabolicInvertedUshaped Distribution     # rparabolicInvertedUshaped = function (n, min=-10, max=10, alpha = 0, beta = 1) 
    resParabolicInvertedUshaped = NULL;     
    alpha_ParabolicInvertedUshaped = seq(-1, 1, length=parameterLen);   
    beta_ParabolicInvertedUshaped = seq(0.01, 10, length=parameterLen)
    
    ##### parabolicUshaped Distribution         # rparabolicUshaped = function (n, min=-10, max=10, alpha = 1, beta = 1) 
    resParabolicUshaped = NULL
    alpha_ParabolicUshaped = seq(-1, 1, length=parameterLen)
    beta_ParabolicUshaped = seq(0.01, 10, length=parameterLen)

    ##### rcosine Distribution          # rrcosine = function (n, min=-10, max=10, mu = 0, sigma = 1)
    resCosine = NULL;
    mu_Cosine = seq(-1, 1, length=parameterLen)
    sigma_Cosine = seq(0.01, 10, length=parameterLen)

    ##### Reflexted Exponential Distribution        # rrexponential = function (n, min=0.0001, max=10, alpha = 1, beta = 1)
    resRexponential = NULL;
    alpha_Rexponential = seq(-1, 1, length=parameterLen)
    beta_Rexponential = seq(0.01, 10, length=parameterLen)

    ##### semielliptical Distribution           # rsemielliptical = function (n, min=-1, max=1, alpha = 0, beta = 1)
    resSemielliptical = NULL
    alpha_Semielliptical = seq(-1, 1, length=parameterLen)
    beta_Semielliptical = seq(0.01, 10, length=parameterLen)

    ##### teisser Distribution          # rteisser = function (n, min=-10, max=10, alpha = 0, beta = 1)
    resTeisser = NULL
    alpha_Teisser = seq(-1, 1, length=parameterLen)
    beta_Teisser = seq(0.01, 10, length=parameterLen)

    ##### unif2 Distribution            # runif
    resUnif = NULL
    alpha_Unif = seq(-1, 1, length=parameterLen)
    beta_Unif = seq(0.01, 10, length=parameterLen)

    ##### 극치 분포: Gumbel 최대값 분포      # rweibull(x, shape = 1, scale = 1)
    shape = seq(0.01, 10, length=parameterLen)
    scale = seq(0.01, 10, length=parameterLen)
    
    
    
    ### 3 parameters
    ##### dhillon1 Distribution         # rdhillon1 = function(n, min=0, max=1, alpha = 1, beta = 1, gamma = 1)
    alpha = seq(-1, 1, length=parameterLen)
    beta = seq(0.01, 10, length=parameterLen)
    gamma = seq(0.01, 10, length=parameterLen)

    ##### dhillon2 Distribution         # rdhillon2 = function(n, min=0.1, max=1, alpha = 1, beta = 1, gamma = 1)
    alpha = seq(-1, 1, length=parameterLen)
    beta = seq(0.01, 10, length=parameterLen)
    gamma = seq(0.01, 10, length=parameterLen)

    ##### weibull Distribution with c Parameters        # rdweibull = function(n, min=-10, max=1, c=1, mu=0, sigma=1)
    mu = seq(-1, 1, length=parameterLen)
    sigma = seq(0.01, 10, length=parameterLen)
    c = seq(0.01, 10, length=parameterLen)

    ##### frechet Distribution          # rfrechet(x, location = 0, shape = 1, scale = 1)
    location = seq(-1, 1, length=parameterLen)
    shape = seq(0.01, 10, length=parameterLen)
    scale = seq(0.01, 10, length=parameterLen)

    ##### lfrechet Distribution         # rlfrechet = function (n, min=0.1, max=10, location = 0, shape = 1, scale = 1)
    location = seq(-1, 1, length=parameterLen)
    shape = seq(0.01, 10, length=parameterLen)
    scale = seq(0.01, 10, length=parameterLen)

    ##### Gamma Distribution with Location Parameters       # rgamma3 = function(x, shape=shape, scale=scale, location=location)
    location = seq(-1, 1, length=parameterLen)
    shape = seq(0.01, 10, length=parameterLen)
    scale = seq(0.01, 10, length=parameterLen)

    ##### glomax Distribution             # rglomax = function (n, min=0.0001, max=1, alpha = 1, beta = 1, gamma = 1) 
    alpha = seq(-1, 1, length=parameterLen)
    beta = seq(0.01, 10, length=parameterLen)
    gamma = c(-1, -0.75, -0.5, -0.25, 0.25, 0.5, 0.75, 1)

    ##### Gompertz-Makeham Distribution         # rgompertzmakeham = function (n, min=0.0001, max=1, alpha = 1, beta = 1, gamma = 1) 
    alpha = seq(-1, 1, length=parameterLen)
    beta = seq(0.01, 10, length=parameterLen)
    gamma = seq(0.01, 10, length=parameterLen)
    
    ##### Hjorth Distribution               # rhjorth(x, m = 1, s = 1, f = 1)
    delta = seq(0.01, 10, length=parameterLen)	# m (delta)
    beta = seq(0.01, 10, length=parameterLen)		# s (beta)
    theta = seq(0.01, 10, length=parameterLen)	# f (theta)

    ##### weibull Distribution with gamma Parameters        # rinverseweibull = function (n, min=0.1, max=10, alpha = 0, beta = 1, gamma = 1) 
    alpha = seq(-1, 1, length=parameterLen)
    beta = seq(0.01, 10, length=parameterLen)
    gamma = seq(0.01, 10, length=parameterLen)

    ##### loglogistic Distribution          # rloglogistic = function (n, min=0.0001, max=10, alpha = 1, beta = 1, gamma = 1) 
    alpha = seq(-1, 1, length=parameterLen)
    beta = seq(0.01, 10, length=parameterLen)
    gamma = seq(0.01, 10, length=parameterLen)

    ##### lomax Distribution              # rlomax = function (n, min=0.0001, max=1, alpha = 1, beta = 1, gamma = 1) 
    alpha = seq(-1, 1, length=parameterLen)
    beta = seq(0.01, 10, length=parameterLen)
    gamma = seq(0.01, 10, length=parameterLen)

    ##### Gamma Distribution with alpha Parameters          # rmuth = function (n, min=0.0001, max=10, alpha = 1, beta = 1, gamma = 1) 
    alpha = seq(-1, 1, length=parameterLen)
    beta = seq(0.01, 10, length=parameterLen)
    gamma = c(0.25, 0.5, 0.75, 1)

    ##### Pareto Distribution of the first kind     # rpareto1 = function (n, min=0.0001, max=10, alpha = 1, beta = 1, gamma = 2)
    alpha = seq(-1, 1, length=parameterLen)
    beta = seq(0.01, 10, length=parameterLen)
    gamma = seq(0.01, 10, length=parameterLen)

    ##### Pareto Distribution of the first kind     # rpower = function (n, min=0.0001, max=10, alpha = 1, beta = 1, gamma = 2) 
    alpha = seq(-1, 1, length=parameterLen)
    beta = seq(0.01, 10, length=parameterLen)
    gamma = seq(0.01, 10, length=parameterLen)

    ##### weibull Distribution with gamma Parameters        # rreflectedweibull = function (n, min=-10, max=10, alpha = 0, beta = 1, gamma = 1)
    alpha = seq(-1, 1, length=parameterLen)
    beta = seq(0.01, 10, length=parameterLen)
    gamma = seq(0.01, 10, length=parameterLen)

    ##### weibull Distribution with Location Parameters         # rweibull3 = function(x, shape=shape, scale=scale, location=location)
    shape = seq(0.01, 10, length=parameterLen)
    scale = seq(0.01, 10, length=parameterLen)
    location = seq(0.01, 10, length=parameterLen)

    
    
    for (i in 1:parameterLen)
    {
        for (j in 1:parameterLen)
        {
            ##### Alpha Distribution
            randVec = ralpha(dataLen, min=min(dataVec), max=max(dataVec), alpha=alpha_Alpha[i], beta=beta_Alpha[j])
            resAlpha[[i]][[j]] = ks.test(dataVec, randVec)
            print(paste("Test with Alpha Distribution : i = ", i, " / ", length(alphaArcsine), "     j = ", j, " / ", length(betaArcsine), sep=""))
            
            ##### Arcsine Distribution
            randVec = rarcsine(dataLen, min=min(dataVec), max=max(dataVec), alpha=alpha[i], beta=beta[j])
            resArcsine[[i]][[j]] = ks.test(dataVec, randVec)
            print(paste("Test with Arcsine Distribution : i = ", i, " / ", length(alpha), "     j = ", j, " / ", length(beta), sep=""))
            
            ##### Beta Distribution
            
            ##### Birnbaum-Saunders Distribution        # rfatigue(x, alpha, beta, mu = 0)
        }
    }


    
    return(res)
}
