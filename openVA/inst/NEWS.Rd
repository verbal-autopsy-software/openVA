% Check from R:
\name{NEWS}
\title{News for \R Package \pkg{openVA}}
\encoding{UTF-8}

\newcommand{\CRANpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}
\newcommand{\sspace}{\ifelse{latex}{\out{~}}{ }}

\section{Changes in version 1.0.6 (2018-08-14)}{
  \itemize{ 
    \item Fix minor bug where default arguments not passed to function when omitted.
  }
}
\section{Changes in version 1.0.5 (2018-07-06)}{
  \itemize{ 
    \item Optimize the imports of dependent packages.
    \item Add version check functions and message when attached.
  }
}
\section{Changes in version 1.0.4 (2018-04-20)}{
  \itemize{ 
    \item Adding support of two new methods: InterVA-5 and InSilicoVA-WHO2016.
    \item Adding functions to clean PHMRC child dataset.
  }
}

\section{Changes in version 1.0.3 (2017-01-01)}{
  \itemize{ 
    \item Change the argument ``type'' into ``phmrc.type'' in the function ``ConvertData.phmrc'' to avoid conflict with multiple arguments.
    \item Fix bug for parsing arguments in calling InSilicoVA directly on PHMRC data.
    \item Fix issues with nbc4va package in ova2nbc() function.
    \item Add new function to calculate CSMF accuracy.
    
  }
}
