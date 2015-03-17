Dear Anu and Balaji,

I went back to the risk model again and reviewed all the assumptions, and I am still convinced that HLM-B (mixed effects) is more appropriate than HLM-A (two-stage linear regression).

The issue is that **LM & GLM are not appropriate for nested data due to non-independence of observations within groups**. Our data is inherently nested: repeated measurements over time (j) for a set of states (i).  Observation (i,j) == (1,1)  is not independent of observation (2,1) (different month, same state), as it is compared to observation (2,2) (different month, different state). Observations within the same state are not independent of one another. Thus the two-tier linear model that I presented as HLM-A violates assumptions of independent and identically distributed residuals (iid). In contrast, mixed effects models are designed to handle nested data.

This reasoning is detailed in my dissertation, and I invite you to re-read those sections.

To me, it feels natural that HLM-A was the first full attempt, and HLM-B arose with more time and deeper understanding of the problem. I think the results from HLM-B are quite interesting. 

Key takeaways from HLM-B: 

- Climate and supply-chain constraints are statistically significant predictors of deviations from annual-mean reliability, for all states. 
- Structural constraints do not add significant information beyond what is contained in the mean reliability score of each state. 
	- This is why the linear mixed-effects model (lme), which estimates a random intercept for each state independently, yields similar goodness of fit as the full OLS, yet requires fewer predictors and preserves assumptions of IID. 
	- The linear mixed effects model (lme) captures differences resulting from large structural variables such as capacity adequacy and capacity utilization vis-a-vis the random intercept. It is an elegant result, in my opinion.

I present Figure 1 as evidence.  

**Figure 1: Visual Inspection of Observed vs. Modeled Fit in Log-Space.**

 ![Figure 1](/Users/elliotcohen/github/Energy/r/Rplot_Comparing_6_models.png" "Comparing Observed vs. Modeled Fit for 6 Models")
 
The two models with the best "goodness of fit" are:

- Simple ordinary least squares regression with all predictors added from the beginning. AIC/BIC criteria were applied to find the best subset of predictors (top left.)
- Hierarchical linear model with random intercepts for each state and fixed effects of the predictors (for generalizability across states) using only climate and supply-chain predictors (bottom right).
- Both models have a fairly good "goodness of fit", but only the HLM preserves assumptions of iid.

Here is a side-by-side comparison of Goodness of Fit for the "best" two models, described above.

				  SST      SSR      SSE      R2   AdjustedR2 PearsonR2
	bestlm:  384.0067 312.6511 71.35563 0.8141813 0.7965774  0.8141813
	bestHLM: 384.0067 323.0498 55.52399 0.8554088 0.8347529  0.8554683
	
Detailed ANOVA tables, shown below.

### OLS starting with all predictors (bestlm)

	Coefficients:
                    Estimate Std. Error t value Pr(>|t|)    
	(Intercept)        1.139e+01  1.783e+00   6.388 6.18e-09 ***
	PctGrid           -5.944e-02  8.787e-03  -6.765 1.09e-09 ***
	Coal_Stock_Days   -1.991e-01  5.665e-02  -3.515 0.000675 ***
	gas_eff_FAF       -3.771e+00  2.505e+00  -1.505 0.135572    
	Hydro_eff_Storage  1.285e+00  7.054e-01   1.822 0.071565 .  
	IB_MAXTEMP         9.288e-02  2.355e-02   3.944 0.000153 ***
	CapAdequacy       -3.977e+00  3.776e-01 -10.534  < 2e-16 ***
	P_Act_mm           5.502e-04  1.661e-04   3.312 0.001312 ** 
	HotDry             2.416e-05  7.204e-06   3.354 0.001144 ** 
	Total_WWF          1.172e-04  1.479e-05   7.920 4.43e-12 ***
	---
	Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

	(Dispersion parameter for gaussian family taken to be 0.7511119)

	Null deviance: 384.007  on 104  degrees of freedom
	Residual deviance:  71.356  on  95  degrees of freedom
	AIC: 279.42
	
	
### HLM with climate and supply-chain predictors only (bestHLM)
	Linear mixed-effects model fit by maximum likelihood
 	Data: scaledat 
       AIC     BIC    logLik
  	277.5254 306.719 -127.7627

	Random effects:
 	Formula: ~1 | Beneficiary
    	    (Intercept)  Residual
	StdDev:    1.123007 0.7447624

	Fixed effects: ENS ~ IB_MAXTEMP + P_Anomaly_mm + P_Act_mm + HotDry + Total_WWF +      Coal_Stock_Days + gas_eff_FAF + Hydro_eff_Storage 
	                      Value Std.Error DF   t-value p-value
	(Intercept)        4.543633 0.5307103 92  8.561418  0.0000***
	IB_MAXTEMP         0.424675 0.1371040 92  3.097469  0.0026***
	P_Anomaly_mm       1.870301 0.8492686 92  2.202249  0.0301**
	P_Act_mm           0.280731 0.1059594 92  2.649420  0.0095***
	HotDry             2.247759 0.8502025 92  2.643792  0.0096***
	Total_WWF          1.362479 0.3914712 92  3.480406  0.0008***
	Coal_Stock_Days   -0.500986 0.1688276 92 -2.967441  0.0038***
	gas_eff_FAF        0.017069 0.1092523 92  0.156231  0.8762
	Hydro_eff_Storage  0.289047 0.1194029 92  2.420772  0.0175**
	 Correlation: 
    	              (Intr) IB_MAX P_Anm_ P_Act_ HotDry Tt_WWF Cl_S_D g__FAF
	IB_MAXTEMP         0.000                                                 
	P_Anomaly_mm       0.000 -0.361                                          
	P_Act_mm           0.000  0.204 -0.214                                   
	HotDry             0.000 -0.361  0.995 -0.193                            
	Total_WWF          0.000  0.220 -0.057  0.213 -0.059                     
	Coal_Stock_Days    0.000 -0.489  0.232 -0.595  0.219 -0.014              
	gas_eff_FAF        0.000  0.158  0.054  0.351  0.074 -0.039 -0.657       
	Hydro_eff_Storage  0.000  0.348  0.047 -0.272  0.055  0.124  0.390 -0.347
	
	
Given all this, I suggest we schedule a Skype call for two weeks from now to discuss and come to a final agreement regarding how to proceed.  I think we all agree that the topic and results are interesting, and would like to see this published sooner rather than later.  We have all invested substantial time and energy into this, so even more motivation to see it through.

The agreed upon plan of stripping the paper down to just one modeling approach still stands, I just think we should highlight HLM-B rather than HLM-A. 

I also invite you to try it out for yourself. Balaji, you may be particularly interested in doing so as you will be familiar with much of the code. Here is a link to a dropbox folder containing the R script and .Rdata. Everything you need to run it should be there. I have tried to document the script clearly, so it should be easy to follow along.  Let me know if anything is unclear. Note that it requires several packages at the top of the script, so you will have to `install.packages()` if you do not already have them.

Sincerely,  
Elliot	