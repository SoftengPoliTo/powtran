\name{effective.power}
\alias{effective.power}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Effective Power computation
}
\description{
Process the work information and computes the effective
power using one of the possible strategies.
}
\usage{

effective.power(work, baseline="both", FUN=median)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{work}{a data frame containing work unit data
            as produced by the \code{\link{extract.power}}}
\item{baseline}{ a character string indicating how to
                 compute the baseline power; see \emph{Details} below.
                 Default is \code{"both"}.}
\item{FUN}{ the function used to aggregate the baseline values}

}
\details{

The effective power is computed by subtracting a \emph{baseline} from
the real power of the work unit.

The baseline can be computed in several different ways as indicated
by the \code{baseline} argument:

\describe{
\item{"both"}{uses \emph{both} the sleep phases preceding and following
              the work unit.}
\item{"left"}{uses the sleep phases preceding (i.e. on the \emph{left})
              the work unit.}
\item{"right"}{uses the sleep phases following (i.e. on the \emph{right})
              the work unit.}
\item{"gboth"}{uses an aggregated value computed from \emph{both} the                    sleep phases preceding and following all
                the work units.}
\item{"gleft"}{uses an aggregated value computed fromthe sleep phases
              preceding (i.e. on the \emph{left})
              all the work units.}
\item{"gright"}{uses an aggregated value computed from the sleep phases
                following (i.e. on the \emph{right})
                all the work units.}
\item{"gmin"}{uses the global minimum value among the sleep phases
                either before or after all the work units.}
\item{"0"}{does not use any baseline (i.e. uses 0 as baseline).}

}

The aggregation of two or more power levels is computed using the
function specified as argument \code{FUN}
}
\value{

The function returns a data frame that consists in the original
data frame \code{work} with a column \code{P} containing
the computed effective power values. If such a column already
exists in the data frame it is overwritten.

}
%\references{
%}
\author{
Marco Torchiano \url{http://softeng.polito.it/torchiano/}
}
%\note{
%%
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%\code{effective.power}
\code{\link{extract.power}}
}
%\examples{
%}
\keyword{ power trace }
% __ONLY ONE__ keyword per line
