# openVA - changes

Version 1.0.9
==========================
+ Various minor document and helper function enhancements.

Version 1.0.8 
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
    

