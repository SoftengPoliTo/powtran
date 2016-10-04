\name{extract.power}
\alias{extract.power}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Analyze the power trace and extract the work
}
\description{
Computes the Cohen's d and Hedges'g effect size statistics.
}
\usage{

extract.power(data,
              t.sampling,
              N=30,
              marker.length=5,
              marker.tolerance=0.25,
              cutoff = 40,
              adjust = 2,
              peakspan = 69,
              intermediate=FALSE,
              include.rawdata=FALSE,
              baseline = "gmin")

}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{data}{the power levels measured during the experiment}
\item{t.sampling}{ sampling period (in seconds)}
\item{N}{ number of work unit repetitions (default 30)}
\item{marker.length}{ marker pulse width can be expressed either
          as number of samples or in terms of duration (seconds).
          If the value is smaller than 10 it is assumed to be in seconds
          otherwise it is the number of samples}
\item{marker.tolerance}{ marker width tolerance as a percentage}
\item{cutoff}{ Cut-off frequency used to smooth down the samples
                and identify the markers, it is expressed in Hz
                (default: 9 Hz)}
\item{adjust}{ density function smoothing parameter (default=2)}
\item{peakspan}{ width of peak detection (default=69) }
\item{intermediate}{returns intermediate computations results }
\item{include.rawdata}{ should rawdata be included in the results }
\item{baseline}{ define how the baseline power is computed, see details below}

}
\details{

The function first identifies the markers in the power trace and
after that -- as a consequence -- it finds the work units between
the markers.

Starting from the work units, using the function \code{\link{effective.power}} the effective power of the work units is computed.
The argument \code{baseline} is passed to that function, see
\code{\link{effective.power}} for the possible values and meaning.
}
\value{
A list of class \code{EnergyAnalysis} containing the following components:
\describe{
\item{t}{sampling period}
\item{n}{number of samples}
\item{work}{a data frame containing the information about the work units
with the following columns:
\tabular{rll}{
  \code{start}:      \tab the initial sample\cr
  \code{end}:        \tab the final sample\cr
  \code{duration}:   \tab the duration (in sec)\cr
  \code{P.real}:     \tab the mean power\cr
  \code{P.idle.left}:\tab the mean power in the pause phase preceding the work unit\cr
  \code{P.idle.right}:\tab the mean power in the pause phase following the work unit\cr
  \code{P}:          \tab the effective power\cr
  \code{E}:          \tab the effective energy consumed\cr
  }
  }
\item{P}{a numeric array containing the power trace values, included only if \code{include.rawdata} is \code{TRUE}}
\item{P.samples}{a data frame containing the sampled elements of the
original trace and the corresponding times if \code{include.rawdata} is \code{FALSE}}
  }
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
\code{\link{effective.power}}
}
%\examples{
%}
\keyword{ power trace }
% __ONLY ONE__ keyword per line
