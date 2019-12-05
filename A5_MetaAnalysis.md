Building on the shoulders of giants: meta-analysis
==================================================

Questions to be answered
------------------------

1.  What is the current evidence for distinctive vocal patterns in
    schizophrenia? Report how many papers report quantitative estimates,
    comment on what percentage of the overall studies reviewed they
    represent (see PRISMA chart) your method to analyze them, the
    estimated effect size of the difference (mean effect size and
    standard error) and forest plots representing it. N.B. Only measures
    of pitch mean and pitch sd are required for the assignment. Feel
    free to ignore the rest (although pause behavior looks interesting,
    if you check my article).

2.  Do the results match your own analysis from Assignment 3? If you add
    your results to the meta-analysis, do the estimated effect sizes
    change? Report the new estimates and the new forest plots.

3.  Assess the quality of the literature: report and comment on
    heterogeneity of the studies (tau, I2), on publication bias (funnel
    plot), and on influential studies.

Tips on the process to follow:
------------------------------

-   Download the data on all published articles analyzing voice in
    schizophrenia and the prisma chart as reference of all articles
    found and reviewed

<!-- -->

    # load package 
    library(pacman)
    p_load(tidyverse, readxl, lme4, metafor, dplyr, gridExtra)

    # load data
    xl <- readxl::read_excel("Matrix_MetaAnalysis_Diagnosis_updated290719.xlsx")

    ## New names:
    ## * frequency -> frequency...68
    ## * frequency -> frequency...73
    ## * frequency -> frequency...78
    ## * frequency -> frequency...83
    ## * frequency -> frequency...88
    ## * … and 7 more problems

-   Look through the dataset to find out which columns to use, and if
    there is any additional information written as comments (real world
    data is always messy!).
    -   Hint: PITCH\_F0M and PITCH\_F0SD group of variables are what you
        need
    -   Hint: Make sure you read the comments in the columns:
        `pitch_f0_variability`, `frequency`, `Title`,
        `ACOUST_ANA_DESCR`, `DESCRIPTION`, and `COMMENTS`

<!-- -->

    # extract data 
    xl_subset <- xl %>% select(
      StudyID, #studyID
      Article, #article
      DIAGNOSIS, #diagnosis
      TYPE_OF_TASK, # task
      SAMPLE_SIZE_SZ, # n(sz)                     -> gives us the number of sz
      SAMPLE_SIZE_HC, # n(hc)                     -> gives us the number of hc
      
      #feature 1 -mean of pitch in the groups 
      PITCH_F0_HC_M, # mean(mean(hc_pitch))       -> gives us the mean of pitch means, for hc (group mean of means)
      PITCH_F0_HC_SD, # sd(mean(hc_pitch))        -> gives us the variability in pitch means, for HC
      PITCH_F0_SZ_M, # mean(mean(sz_pitch))       -> gives us the mean of pitch means, for sz (group mean of means)
      PITCH_F0_SZ_SD, # sd(mean(sz_pitch))        -> gives us the variability in pitch means, for SZ 
      
      #feature 2 - sd of pitch in the groups
      PITCH_F0SD_HC_M, # mean(sd(hc_pitch))       -> gives us the mean of pitch variability, for HC
      PITCH_F0SD_HC_SD, # sd(sd(hc_pitch))        -> gives us how pitch variability, varies for HC
      PITCH_F0SD_SZ_M, # mean(sd(sz_pitch))       -> gives us the mean of pitch variability, for SZ
      PITCH_F0SD_SZ_SD # sd(sd(sz_pitch))         -> gives us how pitch variability, varies for SZ
      )

    colnamelist <- c("studyID", "article","diagnosis","task","n_sz","n_hc","m_m_hc","sd_m_hc","m_m_sz","sd_m_sz","m_sd_hc","sd_sd_hc","m_sd_sz","sd_sd_sz")
    colnames(xl_subset) <- colnamelist


    #task as factor
    xl_subset$task <- as.factor(xl_subset$task)


    # overview of all colnames
    colnames(xl)

    ##   [1] "ArticleID"                 "StudyID"                  
    ##   [3] "Specification"             "Title"                    
    ##   [5] "Authors"                   "Year_publication"         
    ##   [7] "Article"                   "DIAGNOSIS"                
    ##   [9] "MALE_SZ"                   "FEMALE_SZ"                
    ##  [11] "MALE_HC"                   "FEMALE_HC"                
    ##  [13] "AGE_M_SZ"                  "AGE_SD_SZ"                
    ##  [15] "AGE_M_HC"                  "AGE_SD_HC"                
    ##  [17] "EDUCATION_M_SZ"            "EDUCATION_SD_SZ"          
    ##  [19] "EDUCATION_M_HC"            "EDUCATION_SD_HC"          
    ##  [21] "SAMPLE_SIZE_SZ"            "SAMPLE_SIZE_HC"           
    ##  [23] "TASK_TOTAL_DURATION_HC_M"  "TASK_TOTAL_DURATION_HC_SD"
    ##  [25] "TASK_TOTAL_DURATION_SZ_M"  "TASK_TOTAL_DURATION_SZ_SD"
    ##  [27] "TYPE_OF_TASK"              "MEAN_DURATION"            
    ##  [29] "SP_DUR_HC_M"               "SP_DUR_HC_SD"             
    ##  [31] "SP_DUR_SZ_M"               "SP_DUR_SZ_SD"             
    ##  [33] "SPEECH_RATE"               "SP_RAT_HC_M"              
    ##  [35] "SP_RAT_HC_SD"              "SP_RAT_SZ_M"              
    ##  [37] "SP_RAT_SZ_SD"              "SPEECH_PERCENTAGE"        
    ##  [39] "SP_PER_HC_M"               "SP_PER_HC_SD"             
    ##  [41] "SP_PER_SZ_M"               "SP_PER_SZ_SD"             
    ##  [43] "PAUSE_DURATION"            "PA_DUR_HC_M"              
    ##  [45] "PA_DUR__HC_SD"             "PA_DUR_SZ_M"              
    ##  [47] "PA_DUR_SZ_SD"              "Number_of_pauses"         
    ##  [49] "PA_NUM_HC_M"               "PA_NUM_HC_SD"             
    ##  [51] "PA_NUM_SZ_M"               "PA_NUM_SZ_SD"             
    ##  [53] "Total length of pauses"    "PA_TLE_HC_M"              
    ##  [55] "PA_TLE_HC_SD"              "PA_TLE_SZ_M"              
    ##  [57] "PA_TLE_SZ_SD"              "Response latency"         
    ##  [59] "PA_RL_HC_M"                "PA_RL_HC_SD"              
    ##  [61] "PA_RL_SZ_M"                "PA_RL_SZ_SD"              
    ##  [63] "Pause SD"                  "PA_SD_HC_M"               
    ##  [65] "PA_SD_HC_SD"               "PA_SD_SZ_M"               
    ##  [67] "PA_SD_SZ_SD"               "frequency...68"           
    ##  [69] "PITCH_F0_HC_M"             "PITCH_F0_HC_SD"           
    ##  [71] "PITCH_F0_SZ_M"             "PITCH_F0_SZ_SD"           
    ##  [73] "frequency...73"            "PITCH_F1_HC_M"            
    ##  [75] "PITCH_F1_HC_SD"            "PITCH_F1_SZ_M"            
    ##  [77] "PITCH_F1_SZ_SD"            "frequency...78"           
    ##  [79] "PITCH_F2_HC_M"             "PITCH_F2_HC_SD"           
    ##  [81] "PITCH_F2_SZ_M"             "PITCH_F2_SZ_SD"           
    ##  [83] "frequency...83"            "PITCH_F3_HC_M"            
    ##  [85] "PITCH_F3_HC_SD"            "PITCH_F3_SZ_M"            
    ##  [87] "PITCH_F3_SZ_SD"            "frequency...88"           
    ##  [89] "PITCH_F4_HC_M"             "PITCH_F4_HC_SD"           
    ##  [91] "PITCH_F4_SZ_M"             "PITCH_F4_SZ_SD"           
    ##  [93] "frequency...93"            "PITCH_F5_HC_M"            
    ##  [95] "PITCH_F5_HC_SD"            "PITCH_F5_SZ_M"            
    ##  [97] "PITCH_F5_SZ_SD"            "frequency...98"           
    ##  [99] "PITCH_F6_HC_M"             "PITCH_F6_HC_SD"           
    ## [101] "PITCH_F6_SZ_M"             "PITCH_F6_SZ_SD"           
    ## [103] "pitch_f0_variability"      "PITCH_F0SD_HC_M"          
    ## [105] "PITCH_F0SD_HC_SD"          "PITCH_F0SD_SZ_M"          
    ## [107] "PITCH_F0SD_SZ_SD"          "variability...108"        
    ## [109] "PITCH_F0_ENTROPY_HC_M"     "PITCH_F0_ENTROPY__HC_SD"  
    ## [111] "PITCH_F0_ENTROPY_SZ_M"     "PITCH_F0_ENTROPY__SZ_SD"  
    ## [113] "variability...113"         "PITCH_F1SD_HC_M"          
    ## [115] "PITCH_F1SD_HC_SD"          "PITCH_F1SD_SZ_M"          
    ## [117] "PITCH_F1SD_SZ_SD"          "variability...118"        
    ## [119] "PITCH_F2SD_HC_M"           "PITCH_F2SD_HC_SD"         
    ## [121] "PITCH_F2SD_SZ_M"           "PITCH_F2SD_SZ_SD"         
    ## [123] "variability...123"         "PITCH_BDWTH_HC_M"         
    ## [125] "PITCH_BDWTH_HC_SD"         "PITCH_BDWTH_SZ_M"         
    ## [127] "PITCH_BDWTH_SZ_SD"         "variability...128"        
    ## [129] "PITCH_F0RAN_HC_M"          "PITCH_F0RAN_HC_SD"        
    ## [131] "PITCH_F0RAN_SZ_M"          "PITCH_F0RAN_SZ_SD"        
    ## [133] "intensity_mean"            "INT_MEAN_HC_M"            
    ## [135] "INT_MEAN_HC_SD"            "INT_MEAN_SZ_M"            
    ## [137] "INT_MEAN_SZ_SD"            "intensity_variability"    
    ## [139] "INT_VAR_HC_M"              "INT_VAR_HC_SD"            
    ## [141] "INT_VAR_SZ_M"              "INT_VAR_SZ_SD"            
    ## [143] "variability_entropy"       "INT_VAR_ENTR_HC_M"        
    ## [145] "INT_VAR_ENTR_HC_SD"        "INT_VAR_ENTR_SZ_M"        
    ## [147] "INT_VAR_ENTR_SZ_SD"

    # read comments of specific coloums
    comment_subset <- xl %>% select(
        StudyID, pitch_f0_variability, 
        frequency...68, frequency...73, frequency...78, frequency...83, frequency...88,
        Title)

    # only thing we do is make mean values in study 18 to NA's.
    xl_subset$m_m_hc <- ifelse(xl_subset$studyID == 18, NA, xl_subset$m_m_hc)
    xl_subset$m_m_sz <- ifelse(xl_subset$studyID == 18, NA, xl_subset$m_m_sz)

    # study 15 will be overwrited with NA
    xl_subset$m_sd_hc <- ifelse(xl_subset$studyID == 15, NA, xl_subset$m_sd_hc)
    xl_subset$m_sd_sz <- ifelse(xl_subset$studyID == 15, NA, xl_subset$m_sd_sz)
    xl_subset$sd_sd_hc <- ifelse(xl_subset$studyID == 15, NA, xl_subset$sd_sd_hc)
    xl_subset$sd_sd_sz <- ifelse(xl_subset$studyID == 15, NA, xl_subset$sd_sd_sz)

-   Following the procedure in the slides calculate effect size and
    standard error of the effect size per each study. N.B. we focus on
    pitch mean and pitch standard deviation. . first try using lmer (to
    connect to what you know of mixed effects models) . then use rma()
    (to get some juicy additional statistics)

<!-- -->

    # feature 1
    xl_subset <- escalc("SMD", #"SMD" for the standardized mean difference.
                          n1i = n_sz, n2i = n_hc,
                          m1i = m_m_sz, m2i = m_m_hc,
                          sd1i = sd_m_sz, sd2i = sd_m_hc,
                          data = xl_subset)
    colnames(xl_subset)[15] <- "ES_mean"
    colnames(xl_subset)[16] <- "var_ES_mean"

    # feature 2
    xl_subset <- escalc("SMD", #"SMD" for the standardized mean difference.
                          n1i = n_sz, n2i = n_hc,
                          m1i = m_sd_sz, m2i = m_sd_hc,
                          sd1i = sd_sd_sz, sd2i = sd_sd_hc,
                          data = xl_subset)
    colnames(xl_subset)[17] <- "ES_variability"
    colnames(xl_subset)[18] <- "var_ES_variability"



    # Mixed effects regression to assess the meta-analytic effect size
    # lmer
    f1model <- lmer(ES_mean ~ 1 + task + (1|studyID), data = xl_subset, weights = 1/var_ES_mean, REML=F,
         control = lmerControl(
           check.nobs.vs.nlev="ignore", 
           check.nobs.vs.nRE="ignore"))

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl =
    ## control$checkConv, : unable to evaluate scaled gradient

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl =
    ## control$checkConv, : Hessian is numerically singular: parameters are not
    ## uniquely determined

    f2model <- lmer(ES_variability ~ 1 + task + (1|studyID), data = xl_subset, weights = 1/var_ES_variability, REML=F,
         control = lmerControl(
           check.nobs.vs.nlev="ignore", 
           check.nobs.vs.nRE="ignore"))

    #rma
    # Feature 1
    f1_rma <- rma(ES_mean, var_ES_mean, data = xl_subset, slab=article)

    ## Warning in rma(ES_mean, var_ES_mean, data = xl_subset, slab = article):
    ## Studies with NAs omitted from model fitting.

    # Feature 2
    f2_rma <- rma(ES_variability, var_ES_variability, data = xl_subset, slab=article)

    ## Warning in rma(ES_variability, var_ES_variability, data = xl_subset, slab =
    ## article): Studies with NAs omitted from model fitting.

    f2_rma

    ## 
    ## Random-Effects Model (k = 14; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of total heterogeneity): 0.2969 (SE = 0.1446)
    ## tau (square root of estimated tau^2 value):      0.5448
    ## I^2 (total heterogeneity / total variability):   83.04%
    ## H^2 (total variability / sampling variability):  5.90
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 13) = 61.1099, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ## estimate      se     zval    pval    ci.lb    ci.ub 
    ##  -0.4708  0.1625  -2.8966  0.0038  -0.7894  -0.1522  ** 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    # What if we have a fixed effect?
    f1_rmafix <- rma(ES_mean, var_ES_mean, mods = cbind(task), data = xl_subset, slab=article)

    ## Warning in rma(ES_mean, var_ES_mean, mods = cbind(task), data =
    ## xl_subset, : Studies with NAs omitted from model fitting.

    f1_rmafix

    ## 
    ## Mixed-Effects Model (k = 5; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.1362 (SE = 0.1854)
    ## tau (square root of estimated tau^2 value):             0.3691
    ## I^2 (residual heterogeneity / unaccounted variability): 60.81%
    ## H^2 (unaccounted variability / sampling variability):   2.55
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 3) = 8.2816, p-val = 0.0405
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.5337, p-val = 0.4651
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.4229  0.4177   1.0125  0.3113  -0.3957  1.2416    
    ## task      -0.1804  0.2469  -0.7305  0.4651  -0.6644  0.3036    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    f2_rmafix <- rma(ES_variability, var_ES_variability, mods = cbind(task), data = xl_subset, slab=article)

    ## Warning in rma(ES_variability, var_ES_variability, mods = cbind(task), data
    ## = xl_subset, : Studies with NAs omitted from model fitting.

    f2_rmafix

    ## 
    ## Mixed-Effects Model (k = 14; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.2648 (SE = 0.1371)
    ## tau (square root of estimated tau^2 value):             0.5146
    ## I^2 (residual heterogeneity / unaccounted variability): 81.52%
    ## H^2 (unaccounted variability / sampling variability):   5.41
    ## R^2 (amount of heterogeneity accounted for):            10.78%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 12) = 54.3149, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 2.4704, p-val = 0.1160
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.1798  0.4408   0.4078  0.6834  -0.6842  1.0437    
    ## task      -0.3263  0.2076  -1.5717  0.1160  -0.7332  0.0806    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    # Build a forest plot of the results
    forest(f1_rma)

![](A5_MetaAnalysis_files/figure-markdown_strict/unnamed-chunk-3-1.png)

    forest(f2_rma)

![](A5_MetaAnalysis_files/figure-markdown_strict/unnamed-chunk-3-2.png)

    forest(f1_rmafix)

![](A5_MetaAnalysis_files/figure-markdown_strict/unnamed-chunk-3-3.png)

    forest(f2_rmafix)

![](A5_MetaAnalysis_files/figure-markdown_strict/unnamed-chunk-3-4.png)

-   Go back to Assignment 3, add your own study to the data table, and
    re-run meta-analysis. Do the results change?

<!-- -->

    # read data
    voice <- read_csv("/Users/rebeccakjeldsen/Dropbox/CogSci3/ExMet3/Portfolios/portfolio5/features_unscaled.csv")

    ## Warning: Missing column names filled in: 'X1' [1]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   soundname = col_character(),
    ##   Language = col_character(),
    ##   Gender = col_character()
    ## )

    ## See spec(...) for full column specifications.

    ## Warning: 26 parsing failures.
    ##  row                     col expected        actual                                                                                        file
    ## 2017 AverageSyllableDuration a double --undefined-- '/Users/rebeccakjeldsen/Dropbox/CogSci3/ExMet3/Portfolios/portfolio5/features_unscaled.csv'
    ## 2106 AverageSyllableDuration a double --undefined-- '/Users/rebeccakjeldsen/Dropbox/CogSci3/ExMet3/Portfolios/portfolio5/features_unscaled.csv'
    ## 2108 AverageSyllableDuration a double --undefined-- '/Users/rebeccakjeldsen/Dropbox/CogSci3/ExMet3/Portfolios/portfolio5/features_unscaled.csv'
    ## 2301 AverageSyllableDuration a double --undefined-- '/Users/rebeccakjeldsen/Dropbox/CogSci3/ExMet3/Portfolios/portfolio5/features_unscaled.csv'
    ## 2310 AverageSyllableDuration a double --undefined-- '/Users/rebeccakjeldsen/Dropbox/CogSci3/ExMet3/Portfolios/portfolio5/features_unscaled.csv'
    ## .... ....................... ........ ............. ...........................................................................................
    ## See problems(...) for more details.

    # devide in subsets for hc and sz
    voice_sz <- subset(voice, Diagnosis == "1") 
    voice_hc <- subset(voice, Diagnosis == "0")

    # Make a new row where the right values are extracted
    new.row <- data.frame(
      studyID=51,
      article="Fusaroli et al.(2019)",
      diagnosis="SZ", 
      task = "CONSTR", 
      n_sz = sum(n_distinct(voice_sz$Participant)),
      n_hc = sum(n_distinct(voice_hc$Participant)),
      m_m_hc = mean(voice_hc$mean_pitch),
      sd_m_hc = sd(voice_hc$mean_pitch),
      m_m_sz = mean(voice_sz$mean_pitch),
      sd_m_sz = sd(voice_sz$mean_pitch),
      m_sd_hc = mean(voice_hc$sd_pitch),
      sd_sd_hc = sd(voice_hc$sd_pitch),
      m_sd_sz = mean(voice_sz$sd_pitch),
      sd_sd_sz = sd(voice_sz$sd_pitch))

    # add escalc values for both features
    new.row <- escalc("SMD", #"SMD" for the standardized mean difference.
                          n1i = n_sz,
                          n2i = n_hc,
                          m1i = m_m_sz,
                          m2i = m_m_hc,
                          sd1i = sd_m_sz,
                          sd2i = sd_m_hc,
                          data = new.row)
    colnames(new.row)[15] <- "ES_mean"
    colnames(new.row)[16] <- "var_ES_mean"

    new.row <- escalc("SMD", #"SMD" for the standardized mean difference.
                          n1i = n_sz,
                          n2i = n_hc,
                          m1i = m_sd_sz,
                          m2i = m_sd_hc,
                          sd1i = sd_sd_sz,
                          sd2i = sd_sd_hc,
                          data = new.row)
    colnames(new.row)[17] <- "ES_variability"
    colnames(new.row)[18] <- "var_ES_variability"

    # Merge new row with the dataframe xl_subset
    xl_subset <- rbind(xl_subset, new.row)

Run all data

    # Mixed effects regression to assess the meta-analytic effect size
    # lmer
    f1model_new <- lmer(ES_mean ~ 1 + task + (1|studyID), data = xl_subset, weights = 1/var_ES_mean, REML=F,
         control = lmerControl(
           check.nobs.vs.nlev="ignore", 
           check.nobs.vs.nRE="ignore"))

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl =
    ## control$checkConv, : Hessian is numerically singular: parameters are not
    ## uniquely determined

    f2model_new <- lmer(ES_variability ~ 1 + task + (1|studyID), data = xl_subset, weights = 1/var_ES_variability, REML=F,
         control = lmerControl(
           check.nobs.vs.nlev="ignore", 
           check.nobs.vs.nRE="ignore"))

    #rma
    # Feature 1
    f1_rma_new <- rma(ES_mean, var_ES_mean, data = xl_subset, slab=article)

    ## Warning in rma(ES_mean, var_ES_mean, data = xl_subset, slab = article):
    ## Studies with NAs omitted from model fitting.

    # Feature 2
    f2_rma_new <- rma(ES_variability, var_ES_variability, data = xl_subset, slab=article)

    ## Warning in rma(ES_variability, var_ES_variability, data = xl_subset, slab =
    ## article): Studies with NAs omitted from model fitting.

    # What if we have a fixed effect?
    f1_rmafix_new <- rma(ES_mean, var_ES_mean, mods = cbind(task), data = xl_subset, slab=studyID)

    ## Warning in rma(ES_mean, var_ES_mean, mods = cbind(task), data =
    ## xl_subset, : Studies with NAs omitted from model fitting.

    f2_rmafix_new <- rma(ES_variability, var_ES_variability, mods = cbind(task), data = xl_subset, slab=studyID)

    ## Warning in rma(ES_variability, var_ES_variability, mods = cbind(task), data
    ## = xl_subset, : Studies with NAs omitted from model fitting.

    # Build a forest plot of the results
    forest(f1_rma_new)

![](A5_MetaAnalysis_files/figure-markdown_strict/unnamed-chunk-5-1.png)

    forest(f2_rma_new)

![](A5_MetaAnalysis_files/figure-markdown_strict/unnamed-chunk-5-2.png)

    forest(f1_rmafix_new)

![](A5_MetaAnalysis_files/figure-markdown_strict/unnamed-chunk-5-3.png)

    forest(f2_rmafix_new)

![](A5_MetaAnalysis_files/figure-markdown_strict/unnamed-chunk-5-4.png)
Report and comment on heterogeneity of the studies (tau, I2), on
publication bias (funnel plot), and on influential studies. - Now look
at the output of rma() - and check tau (Tau square is a measure of
overall variance) - and I2 (I square is the proportion of variance due
to heterogeneity, aka, that cannot be reduced to within-study
uncertainty)

    # to get Tau and I^2
    summary(f1_rmafix_new)

    ## 
    ## Mixed-Effects Model (k = 6; tau^2 estimator: REML)
    ## 
    ##   logLik  deviance       AIC       BIC      AICc 
    ##  -1.8501    3.7002    9.7002    7.8591   33.7002   
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.0781 (SE = 0.1008)
    ## tau (square root of estimated tau^2 value):             0.2795
    ## I^2 (residual heterogeneity / unaccounted variability): 58.31%
    ## H^2 (unaccounted variability / sampling variability):   2.40
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 4) = 8.5620, p-val = 0.0730
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.5597, p-val = 0.4544
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.3275  0.3113   1.0522  0.2927  -0.2826  0.9376    
    ## task      -0.1486  0.1986  -0.7481  0.4544  -0.5379  0.2407    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    summary(f2_rmafix_new)

    ## 
    ## Mixed-Effects Model (k = 15; tau^2 estimator: REML)
    ## 
    ##   logLik  deviance       AIC       BIC      AICc 
    ## -11.1123   22.2246   28.2246   29.9195   30.8913   
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.2312 (SE = 0.1166)
    ## tau (square root of estimated tau^2 value):             0.4809
    ## I^2 (residual heterogeneity / unaccounted variability): 81.40%
    ## H^2 (unaccounted variability / sampling variability):   5.38
    ## R^2 (amount of heterogeneity accounted for):            14.82%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 13) = 54.3637, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 3.0818, p-val = 0.0792
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.1776  0.3789   0.4687  0.6393  -0.5651  0.9203    
    ## task      -0.3239  0.1845  -1.7555  0.0792  -0.6855  0.0377  . 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    # funnel plot
    funnel(f1_rmafix_new, main = "feature 1 RMA", xlab = "Standardized Mean Difference")

![](A5_MetaAnalysis_files/figure-markdown_strict/unnamed-chunk-6-1.png)

    funnel(f2_rmafix_new, main = "feature 2 RMA", xlab = "Standardized Mean Difference")

![](A5_MetaAnalysis_files/figure-markdown_strict/unnamed-chunk-6-2.png)

    # influential studies
    inf_f1 <- influence(f1_rma_new)
    print(inf_f1)

    ## 
    ##                        rstudent  dffits cook.d  cov.r tau2.del QE.del 
    ## Pinheiro et al. (2016)   0.6705  0.2196 0.0508 1.2326   0.0641 8.5556 
    ## Martínez et al. (2015)  -1.6271 -0.6378 0.3142 0.9164   0.0307 5.8716 
    ## Graux et al. (2011)      2.0838  0.8740 0.4635 0.4312   0.0015 5.0129 
    ## Pinheiro et al. (2017)   0.6463  0.2003 0.0422 1.2222   0.0643 8.6501 
    ## Thanh (2018)            -0.7481 -0.4113 0.2063 1.4945   0.0781 8.5620 
    ## Fusaroli et al.(2019)   -0.2063 -0.1963 0.0669 1.9345   0.1052 9.4274 
    ##                           hat  weight    dfbs inf 
    ## Pinheiro et al. (2016) 0.1121 11.2126  0.2160     
    ## Martínez et al. (2015) 0.1808 18.0790 -0.6453     
    ## Graux et al. (2011)    0.1415 14.1472  1.0877   * 
    ## Pinheiro et al. (2017) 0.1030 10.3050  0.1965     
    ## Thanh (2018)           0.1816 18.1604 -0.4108     
    ## Fusaroli et al.(2019)  0.2810 28.0959 -0.2131

    plot(inf_f1)

![](A5_MetaAnalysis_files/figure-markdown_strict/unnamed-chunk-6-3.png)

    inf_f2 <- influence(f2_rma_new)
    print(inf_f2)

    ## 
    ##                         rstudent  dffits cook.d  cov.r tau2.del  QE.del 
    ## Martínez et al. (2015)    0.3847  0.1184 0.0153 1.1655   0.2997 63.4286 
    ## Bedwell et al. (2014)     1.0297  0.2455 0.0605 1.0620   0.2732 61.7932 
    ## Rapcan et al. (2010)      0.2029  0.0667 0.0048 1.1605   0.3003 63.5492 
    ## Dickey et al. (2012).1    0.1076  0.0425 0.0020 1.1664   0.3016 63.4678 
    ## Cohen et al. (2008)      -0.1629 -0.0299 0.0010 1.1639   0.3006 62.7201 
    ## Alpert et al. (2000)     -0.7984 -0.2061 0.0435 1.0957   0.2797 58.7001 
    ## Compton et al. (2018).1   0.7299  0.2185 0.0506 1.1400   0.2890 59.9835 
    ## Compton et al. (2018).2   0.5512  0.1672 0.0302 1.1596   0.2961 62.6137 
    ## Kliper et al. (2015)     -2.6546 -0.6854 0.3503 0.7270   0.1677 45.4368 
    ## Ross et al. (2001).1     -2.3314 -0.6563 0.3215 0.7677   0.1784 44.8414 
    ## Ross et al. (2001).2     -1.0636 -0.2806 0.0774 1.0485   0.2653 56.5979 
    ## Meaux et al. (2018).1     1.1463  0.3033 0.0904 1.0535   0.2658 59.6964 
    ## Meaux et al. (2018).2     1.1891  0.3132 0.0957 1.0452   0.2632 59.3606 
    ## Thanh (2018)              0.0267  0.0220 0.0005 1.1780   0.3037 63.2238 
    ## Fusaroli et al.(2019)     0.5628  0.1750 0.0334 1.1672   0.2969 61.1099 
    ##                            hat weight    dfbs inf 
    ## Martínez et al. (2015)  0.0702 7.0178  0.1186     
    ## Bedwell et al. (2014)   0.0533 5.3296  0.2454     
    ## Rapcan et al. (2010)    0.0640 6.4024  0.0666     
    ## Dickey et al. (2012).1  0.0655 6.5514  0.0424     
    ## Cohen et al. (2008)     0.0661 6.6073 -0.0299     
    ## Alpert et al. (2000)    0.0648 6.4787 -0.2060     
    ## Compton et al. (2018).1 0.0775 7.7525  0.2193     
    ## Compton et al. (2018).2 0.0749 7.4867  0.1678     
    ## Kliper et al. (2015)    0.0555 5.5485 -0.7127     
    ## Ross et al. (2001).1    0.0617 6.1731 -0.6673     
    ## Ross et al. (2001).2    0.0638 6.3779 -0.2808     
    ## Meaux et al. (2018).1   0.0667 6.6668  0.3033     
    ## Meaux et al. (2018).2   0.0667 6.6653  0.3132     
    ## Thanh (2018)            0.0700 6.9998  0.0220     
    ## Fusaroli et al.(2019)   0.0794 7.9424  0.1761

    plot(inf_f2)

![](A5_MetaAnalysis_files/figure-markdown_strict/unnamed-chunk-6-4.png)
