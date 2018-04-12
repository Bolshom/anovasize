\name{sample_sizing_anova}
\alias{sample_sizing_anova}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ANOVA Sample Sizing
}
\description{
This function calculates ANOVA sample sizes for three different situations, based on the book "Design and Analysis of Experiments".
}
\usage{
sample_sizing_anova(method, means = 0, var = 0, diff = 0, a = 0, perc = 0, alpha, power)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{method}{
    method="With means" when you have the vector of means from each group, and want to find the sample size to reject the null hypotesis from ANOVA F test.

    method = "Max difference" when you intend to reject the null hypotesis when the difference between any two treatment means exceeds a certain value.

    method = "Percentage variance" to find the ANOVA sample sizes when you want to find a certain percentage variation on standard deviation or variance.
}
  \item{means}{
The vector of means from your groups.
}
  \item{var}{
The population variance or estimated from your analyzed factor.
}
  \item{diff}{
The max difference between two treatment means, to reject the null hypotesis.
}
  \item{a}{
Number of groups on your experiment.
}
  \item{perc}{
The percentage (p.e. 20 for 20 percent) of variation on your S.D. or Variance.
}
  \item{alpha}{
The level of significance of your analysis.
}
  \item{power}{
The probability that you want to detect the phenomenon.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{n}{
    The function returns the sample size for each group to guarantee at least the power percentage.
}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
"Design and Analysis of Experiments" - Douglas C. Montgomery
}
\author{
Natan Freitas Leite
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## The function is currently defined as
function (method, means = 0, var = 0, diff = 0, a = 0, perc = 0,
    alpha, power)
{
    if (method == "With means") {
        ss_mean_vec = function(means, var, alpha, power) {
            n = 2:10000
            overall_mean = mean(means)
            tau = means - overall_mean
            a = length(means)
            fi2_n = n * sum(tau^2)/(a * var)
            power_vector = 1 - pf(qf(1 - alpha, a - 1, a * (n -
                1)), a - 1, a * (n - 1), a * fi2_n)
            n = n[min(which(power_vector > power))]
            return(n)
        }
        return(ss_mean_vec(means, var, alpha, power))
    }
    if (method == "Max difference") {
        ss_diff = function(diff, a, var, alpha, power) {
            n = 2:10000
            fi2_n = n * diff^2/(2 * a * var)
            power_vector = 1 - pf(qf(1 - alpha, a - 1, a * (n -
                1)), a - 1, a * (n - 1), a * fi2_n)
            n = n[min(which(power_vector > power))]
            return(n)
        }
        return(ss_diff(diff, a, var, alpha, power))
    }
    if (method == "Percentage variance") {
        ss_sd_diff = function(perc, a, alpha, power) {
            n = 2:10000
            fi2_n = (sqrt((1 + 0.01 * perc)^2 - 1) * sqrt(n))^2
            power_vector = 1 - pf(qf(1 - alpha, a - 1, a * (n -
                1)), a - 1, a * (n - 1), a * fi2_n)
            n = n[min(which(power_vector > power))]
            return(n)
        }
        return(ss_sd_diff(perc, a, alpha, power))
    }
  }

##On the example 3-14 on the book, there are 5 observations
##from three different batteries brands. The phenomenon studied are the
##weeks of life from those batteries.

data=data.frame("weeks"=c(100,96,92,96,92,76,80,75,84,82,108,100,96,98,100)
,"brand"=c(rep("1",5),rep("2",5),rep("3",5)))

##Sample sizing from group means:
means_vector=mean(plyr::ddply(data,"brand",summarise,mean=mean(weeks))[,2])

est_var=mean(plyr::ddply(data,"brand",summarise,var=var(weeks))[,2])

ss=sample_sizing_anova("With means",means=means_vector,var=est_var,alpha=.05,power=.9)

ss

##Sample sizing from max difference between means:
est_var=mean(plyr::ddply(data,"brand",summarise,var=var(weeks))[,2])

##The max difference is 10, so diff=10:
ss=sample_sizing_anova("Max difference",diff=10,a=3,var=est_var,alpha=.05,power=.9)


ss

##Sample sizing from percentage increase in variance or standard deviation:
##If we want to find the sample size for a 25 percentage variation, we input "perc=25":
ss=sample_sizing_anova("Percentage variance",perc=25,a=3,alpha=.05,power=.9)

ss
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
