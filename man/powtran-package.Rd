\name{powtran-package}
\alias{powtran-package}
%\alias{powtran}
\docType{package}
\title{
POWer TRace ANalyzer
}
\description{
This package is able to analyze power traces from repeated executions of software applications or algorithms in a efficient and accurate way.

The power traces are expected to be collected according to the following protocol:

\itemize{
  \item each work unit must be preceded a and followed by a marker
  \item the markers consist in a square pulse that has to be generated
        by a fragment of code keeping the processor as busy as possible.
        The pulse is preceded by (equally long) sleep period.

        E.g.
        \preformatted{
      __B__                  __B__
      |   |     __work__     |   |
      |   |     |      |     |   |
 __S__|   |__S__|      |__S__|   |__S__
        }
  \item the work units are repeated several times (default: 30) in order
        to obtain an accurate estimate of the real power consumptiom.
  \item the effective power consumptio is estimated by subtracting the
        sleep level power from the average power measured during the work phase.
}
}
\details{

The main functions are:

\code{\link{extract.power}}.

\code{\link{effective.power}}.


\bold{Change history}

\describe{
\item{0.1}{Initial version after several trials and debugging}
\item{0.1.1}{Minor fixes, added \code{0} as possible baseline}
}
}
\author{
Marco Torchiano \url{http://softeng.polito.it/torchiano/}
}
\keyword{ package }
\keyword{ power trace }
\keyword{ green software }
