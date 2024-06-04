#' Lot. Voltera Model with Hunting
#'
#' Function computes the rate of change of populations in a predator-prey interaction, including the effect of hunting.
#' @param t  time (days)
#' @param pop datatype list initial conditions; list with two values prey=number of prey and pred=number of predator
#' @param pars datatype list  coefficient in Lotka-Volterra pars$rprey, pars$alpha, pars$eff, pars$pmort, pars$hunt_rate, pars$min_prey
#'  \emph{rprey} is growth rate of prey population;
#'  \emph{eff} is the rate of ingestion of prey by predators
#'  \emph{alpha} is an interaction coefficient (higher values greater interaction)
#'  \emph{pmort} is the mortality rate of predator population
#'  \emph{hunt_rate} is the hunting rate on the prey population
#'  \emph{min_prey} is the minimum prey population before hunting is allowed
#' @examples
#' lotvmodK(t=1, pop=list(prey=1, pred=2), pars=list(rprey=0.5, alpha=0.3, eff=0.2, pmort=0.2, hunt_rate=0.1, min_prey=100))
#'
#' pars = list(rprey=0.5, alpha=0.3, eff=0.2, pmort=0.2, hunt_rate=0.1, min_prey=100)
#' currpop = list(prey=1, pred=1)
#' days = seq(from=1, to=20)
#' res = ode(func=lotvmodK, y=currpop, times=days, parms=pars)
#'
#' @return lotvmodK returns a list containing the following components
#' \describe{
#' \item{dprey}{rate of change of prey population}
#' \item{dpred}{rate of change of predator population}
#'}

lotvmodK = function(t, pop, pars) {
  with(as.list(c(pars, pop)), {
    dprey = rprey * (1 - prey / K) * prey - alpha * prey * pred - hunt_rate * (prey >= min_prey) * prey
    dpred = eff * alpha * prey * pred - pmort * pred
    return(list(c(dprey, dpred)))
  })
}