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
#'  \emph{min_prey} is the minimum prey population required for hunting to occur
#' @examples
#' lotvmodH(t=1, pop=list(prey=1, pred=2), pars=list(rprey=0.5, alpha=0.3, eff=0.2, pmort=0.2, hunt_prey=0.1, hunt_pred=0.05, min_prey=0.5))
#'
#' pars = c(rprey=0.5, alpha=0.3, eff=0.2, pmort=0.2, hunt_prey=0.1, hunt_pred=0.05, min_prey=0.5)
#' currpop  = c(prey = 1, pred=1)
#  days = seq(from=1,to=20)
#' res = ode(func=lotvmod, y=currpop, times=days, parms=pars)
#'
#' @return  lotvmod returns a list containing the following components
#' \describe{
#' \item{dprey}{rate of change of prey populutation}
#' \item{dpred}{rate of change of preditor populutation}
#'}

lotvmodH = function(t, pop, pars) {
  with(as.list(c(pars, pop)), {
    # Hunting is allowed only if prey population is above the minimum threshold
    if (prey > min_prey) {
      hunting_prey = hunt_prey * prey
    } else {
      hunting_prey = 0
    }
    
    # Ensure hunting does not exceed the current population
    hunting_prey = min(hunting_prey, prey)
    
    dprey = rprey * (1 - prey / K) * prey - alpha * prey * pred - hunting_prey
    dpred = eff * alpha * prey * pred - pmort * pred - hunt_pred * pred
    return(list(c(dprey, dpred)))
  })
}




