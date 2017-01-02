% Check from R:
\name{NEWS}
\title{News for \R Package \pkg{openVA}}
\encoding{UTF-8}

\newcommand{\CRANpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}
\newcommand{\sspace}{\ifelse{latex}{\out{~}}{ }}

\section{Changes in version 1.0.3 (2017-01-01)}{
  \itemize{ 
    \item Change the argument ``type'' into ``phmrc.type'' in the function ``ConvertData.phmrc'' to avoid conflict with multiple arguments.
    \item Fix bug for parsing arguments in calling InSilicoVA directly on PHMRC data.
    }
}
