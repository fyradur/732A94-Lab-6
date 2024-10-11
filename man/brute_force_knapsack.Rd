\name{brute_force_knapsack}
\alias{brute_force_knapsack}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Capitalized Title (ideally limited to 65 characters)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
brute_force_knapsack(x, W)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{W}{
%%     ~~Describe \code{W} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or standard data sets, see data().

## The function is currently defined as
function (x, W) 
{
    subset_max_elements <- NA
    v_sum_max <- NA
    n <- nrow(x)
    for (i in 1:(2^n - 1)) {
        subset_elements <- which(intToBits(i) == 1)
        x_subset <- x[subset_elements, ]
        x_subset_sums <- colSums(x_subset)
        v_sum <- x_subset_sums["v"]
        w_sum <- x_subset_sums["w"]
        is_not_to_heavy <- w_sum <= W
        is_largest_observation <- if (is.na(v_sum_max)) {
            TRUE
        }
        else {
            v_sum > v_sum_max
        }
        if (is_not_to_heavy & is_largest_observation) {
            subset_max_elements <- subset_elements
            v_sum_max <- v_sum
        }
    }
    names(v_sum_max) = NULL
    return(list(value = v_sum_max, elements = subset_max_elements))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
