example.algos = function() {
  n = 20
  T = 10
  topics = 1:8
  slots = rep(2,T)
  slots[1] = 1
  slots[2] = 0
  slots[7] = 1

  # Common utility
  u.com = rnorm(T,0,1)
  u.ind = matrix(rnorm(T*n,0,1), n, T)
  w = 0.8
  u = t(w * u.com + (1-w) * t(u.ind))

  prefs = t(apply(u,1,rank ))


  aspr = assignment.problem.alloc(prefs, slots=slots)
  aspr

  #prefs = t(replicate(n,sample(topics,replace = FALSE)))
  prios = runif(NROW(prefs))
  sedi = serial.dictator.alloc(prefs, prios, slots=slots)
  sedi
}

#' A simple serial dictator assignment
#' @param prefs A matrix row=students cols=topics
#' @param prios A list of priorities for each student
#' @return A vector with the assigned object for each student. NA means the student did not get an object
serial.dictator.alloc = function(prefs, prios = runif(NROW(prefs)), slots=rep(1,NCOL(prefs)), return.pref.ind = TRUE) {
  restore.point("serial.dictator.alloc")
  n = length(prios)
  T = NCOL(prefs)
  topics = (1:NCOL(prefs))[slots>0]


  ord = order(-prios)
  # get the person orders
  pers = 1:n
  pers[ord] = 1:n

  res = rep(NA_integer_,n)

  i = 1
  for (i in pers) {
    found = topics[which.min(prefs[i, topics])]
    if (length(found)==1) {
      res[i] = found
      slots[found] = slots[found]-1
      if (slots[found]<=0)
        topics = setdiff(topics, found)
    }
  }
  res
}

#' Assignment as cost minimization problem
#' @param prefs A matrix row=students cols=topics
assignment.problem.alloc = function(prefs, rank.costs=seq_len(max(prefs,na.rm=TRUE))^2, slots=rep(1,NCOL(prefs)), no.match.cost = max(rank.costs)*1000) {
  restore.point("assignment.problem.algo")

  has.na = any(is.na(prefs))
  n = NROW(prefs)
  S = sum(slots)
  T = NCOL(prefs)

  multi.slot = any(slots != 1)

  costs = matrix(rank.costs[prefs],n,T)
  if (multi.slot) {

    slot.cols = unlist(lapply(seq_along(slots), function(t) rep(t, slots[t])))
    costs = costs[,slot.cols, drop=FALSE]
  }

  if (n>S) {
    costs = cbind(costs, matrix(no.match.cost,n,n-S))
  }

  # Not all topics are neccessarily ranked
  if (has.na) {
    not.cost = ceiling(max(costs, na.rm=TRUE)*(n+1))
    costs[is.na(costs)] = not.cost
  }

  res = solve_LSAP(costs)
  res[res>S] = NA
  res = as.integer(res)

  # Map back to topics
  if (multi.slot) {
    res = slot.cols[res]
  }

  # Set students without matched topics to NA
  if (has.na) {
    na.res = is.na(prefs[cbind(1:n,res)])
    res[na.res] = NA_integer_
  }

  res
}
