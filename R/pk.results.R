pk.results <- function(data, conc.name, time.name, dose.name){
  # n.tail = 3 for fav works..  
  # n.tail = 6 for M1 works.. 
  tail <- 3
  if (any(data$pctest == "M1")) {
    tail <- 6
  }
  out <- nca.complete(data=data, n.tail=tail, conf.level=0.90, conc.name=conc.name, time.name=time.name, dose.name=dose.name)
  doses <- data[,eval(dose.name)]
  aucTlast <- out$est[1]
  aucTlastDose <- aucTlast / doses[1]
  aucInf <- out$est[2]
  aucInfDose <- aucInf / doses[1]
  mrt <- out$est[4]
  hl <- out$est[5]
  cl <- out$est[6]
  vd <- out$est[7]
  cmax <- max(data[eval(conc.name)])
  cmaxDose <- cmax / doses[1]
  tmax <- data[[eval(time.name)]][which(data[eval(conc.name)]==cmax)]
  data.frame(aucTlast, aucTlastDose, aucInf, aucInfDose, mrt, hl, cl, vd, cmax, cmaxDose, tmax)
}