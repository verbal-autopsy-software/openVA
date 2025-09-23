# openVA - changes
Version 1.2.0 (2025-09-23)
==========================
+ Major update: support for new R packages vacalibration and EAVA.


Version 1.1.2 (2024-01-28)
==========================
+ Update information on downloading PHMRC data with user registration on their website.

Version 1.1.1 (2023-03-17)
==========================
+ Satisfy new CRAN requirement changes.

Version 1.1.0 (2022-09-25)
==========================
+ Major document improvement and internal code improvements.
+ Colorblind-friendly color palette is default now.

Version 1.0.15 (2022-08-02)
==========================
+ Minor bug fixes 

Version 1.0.14 (2022-03-11)
==========================
+ Minor code improvements checking the input data format.
+ Improved `getCSMF_accuracy()` function with stricter input
+ New function to compute overall chance-corrected concordance `getCCC()`
+ Updated `getTopCOD` function with parameter to return top n causes and corresponding probabilities (or values for how likely the cause is for InterVA)

Version 1.0.13 (2021-09-16)
==========================
+ Minor code improvements.
 
Version 1.0.12 (2021-02-02)
==========================
+ Now support both WHO 2012 and WHO 2016 output in `ConvertData` function.
+ Minor plot improvements.
 
Version 1.0.11 (2020-05-01)
==========================
+ Minor CRAN check update.
+ Now support `version='5'` for the latest version of InterVA-5.

Version 1.0.10 (2020-04-09)
==========================
+ Minor unit test updates.

Version 1.0.9 (2020-02-17)
==========================
+ Various minor document and helper function enhancements.

Version 1.0.8 (2019-02-18)
==========================
+ Minor clarifications in documentation.
+ Reduced the number of iterations saved for InSilicoVA when Nsim is large.


Version 1.0.8 (2019-02-15)
==========================
+ Minor improvement in data cleaning steps of PHMRC data that deals with duplicate questions with categorical responses.

Version 1.0.7 (2018-09-12)
==========================
+ Fix minor issue with reporting individual top cause when all causes have 0 probability.


Version 1.0.6 (2018-08-28)
==========================
+ Fix minor issue when saving InterVA and InSilicoVA results to file.
+ Fix crash from InterVA using training data with no symptom. 
+ Fix minor bug where default arguments not passed to function when omitted.

Version 1.0.5 (2018-07-06)
==========================
+ Optimize the imports of dependent packages.
+ Add version check functions and message when attached.

Version 1.0.4 (2018-04-20)
==========================
+ Adding support of two new methods: InterVA-5 and InSilicoVA-WHO2016.
+ Adding functions to clean PHMRC child dataset.


Version 1.0.3 (2017-01-01)
==========================
+ Change the argument ``type'' into ``phmrc.type'' in the function ``ConvertData.phmrc'' to avoid conflict with multiple arguments.
+ Fix bug for parsing arguments in calling InSilicoVA directly on PHMRC data.
+ Fix issues with nbc4va package in ova2nbc() function.
+ Add new function to calculate CSMF accuracy.
    

