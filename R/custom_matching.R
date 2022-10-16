example.custom.matching = function() {
  setwd("C:/lehre/empsem/ranking/sose21")
  dat1 = read.csv("topic_ranking_BA.csv")
  dat1 = filter(dat1, !startsWith(studemail,"test"))
  dat1$bama="BA"
  dat2 = read.csv("topic_ranking_MA.csv")
  dat2$bama = "MA"
  dat.li = list(dat1,dat2)

  dat = merge.ranking.dat(dat.li)
  dat$topic = remove.links.from.topics(dat$topic)

  unique(select(dat,studemail, data.set))
  dat = filter(dat, studemail != "daniel-1.hammer@ymail.com")

  res = assign.with.ranking.dat(ranking, assignment.problem.alloc) %>%
     left_join(unique(select(dat,studemail, bama)), by="studemail") %>%
     arrange(bama, studemail)

  write.csv(res, "topic_matching.csv",row.names = FALSE)

  #test = left_join(res, select(dat,studemail, topic, studname, rank.org=rank, shownpos), by = c("studemail", "topic"))

  prefs = ranking.csv.to.prefs(dat)

  assign = assignment.problem.alloc(prefs)
  NROW(prefs)
  NCOL(prefs)

  dat = read.csv("topic_ranking.csv")
  dat$topic = remove.links.from.topics(dat$topic)

  dat1 = dat %>% filter(nchar(topic)>=70)
  dat2 = dat %>% filter(nchar(topic)<=90)
  dat2$studemail = paste0("R2", dat2$studemail)
  dat.li = list(dat1,dat2)
  ranking = dat
}

assign.with.ranking.dat = function(ranking, assign.fun, ...) {
  ntop = max(ranking$pos)
  nstud = n_distinct(ranking$studemail)

  studs = unique(ranking$studemail)
  topics = ranking %>%
    group_by(pos,topic) %>%
    summarize() %>%
    arrange(pos) %>%
    pull(topic)

  ranking$studnum = match(ranking$studemail, studs)

  prefs = matrix(NA_integer_, nstud, ntop)
  rownames(prefs) = studs

  ind = cbind(ranking$studnum, ranking$pos)
  prefs[ind] = ranking$rank

  assign = assign.fun(prefs, ...)
  rank = prefs[cbind(1:nstud, assign)]

  res = data.frame(studemail = studs, topic = topics[assign], rank=rank) %>%
    left_join(select(ranking,studemail, topic, studname, rank.test=rank, shownpos), by = c("studemail", "topic"))

  if (!isTRUE(all(res$rank == res$rank.test))) {
    warning("There seems to have been some problem. rank != rank.test")
    return(res)
  }
  select(res, studemail, studname, topic, rank, shownpos)
}

# Allows to merge
merge.ranking.dat = function(dat.li) {
  all.dat = bind_rows(lapply(seq_along(dat.li), function(i) {
    dat = dat.li[[i]]
    dat$data.set = i
    dat
  }))

  topics = unique(all.dat$topic)
  all.dat$pos = match(all.dat$topic, topics)

  #test = unique(select(all.dat, pos, topic))

  all.dat
}

# prefs will be a matrix with one column per topic
# and one row per student
ranking.csv.to.prefs = function(ranking) {
  ntop = max(ranking$pos)
  nstud = n_distinct(ranking$studemail)

  studs = unique(ranking$studemail)
  ranking$studnum = match(ranking$studemail, studs)

  prefs = matrix(NA_integer_, nstud, ntop)
  rownames(prefs) = studs

  ind = cbind(ranking$studnum, ranking$pos)
  prefs[ind] = ranking$rank

  prefs
}

remove.links.from.topics = function(txt) {
  txt = dat$topic
  rows = has.substr(txt,"</a>")
  if (sum(rows)==0) return(txt)
  left = str.left.of(txt,"<a")
  mid = str.between(txt,'">',"</a>")
  right = str.right.of(txt,"</a>")
  txt[rows] = paste0(left[rows],mid[rows],right[rows])
  txt = gsub("\n","",txt)
  txt
}
