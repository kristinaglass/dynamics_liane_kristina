#' Lot. Voltera Model
#'
#' function computes the rate of change of populations in a predictor prey interaction
#' @param t  time (days)
#' @param pop datatype list initial conditions; list with two values prey=number of prey and pred=number of predictor
#' @param pars datatype list  coefficient in Lotka-Voltera pars$rprey, pars$alpha, pars$eff, par$pmort
#'  \emph{rprey} is growth rate of prey population;
#'  \emph{eff} is the rate of ingestion of prey by predators
#'  \emph{alpha} is a interaction coefficient with higher values greater interaction
#'  \emph{pmort}  mortality rate of predictor population
#'  \emph{hunt_prey} is the hunting rate affecting the prey population;
#'  \emph{hunt_pred} is the hunting rate affecting the predator population
#' @examples
#' lotvmodH(t=1, pop=list(prey=1, pred=2), pars=list(rprey=0.5, alpha=0.3, eff=0.2, pmort=0.2, hunt_prey=0.1, hunt_pred=0.05))
#'
#' pars = c(rprey=0.5, alpha=0.3, eff=0.2, pmort=0.2, hunt_prey=0.1, hunt_pred=0.05)
#' currpop  = c(prey = 1, pred=1)
#  days = seq(from=1,to=20)
#' res = ode(func=lotvmod, y=currpop, times=days, parms=pars)
#'
#' @return  lotvmod returns a list containing the following components
#' \describe{
#' \item{dprey}{rate of change of prey populutation}
#' \item{dpred}{rate of change of preditor populutation}
#'}

lotvmodK = function(t, pop, pars) {
with(as.list(c(pars,pop)), {
dprey = rprey*(1-prey/K)*prey -  alpha*prey*pred - hunt_prey*prey
dpred = eff*alpha*prey*pred - pmort*pred- hunt_pred*pred
return(list(c(dprey,dpred)))})
}




