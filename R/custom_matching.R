example.custom.matching = function() {
  dat = read.csv("topic_ranking.csv")
  dat$topic = remove.links.from.topics(dat$topic)

  dat1 = dat %>% filter(nchar(topic)>=70)
  dat2 = dat %>% filter(nchar(topic)<=90)
  dat2$studemail = paste0("R2", dat2$studemail)
  dat.li = list(dat1,dat2)
  ranking = dat
}

# Allows to merge
merge.ranking.dat = function(dat.li) {
  all.dat = bind_rows(lapply(seq_along(dat.li), function(i) {
    dat = dat.li[[i]]
    dat$data.set = i
    dat
  }))



}

ranking.csv.to.prefs = function(ranking) {
  topics = ranking %>%
    group_by(topic,pos) %>%


  T = max(ranking$pos)

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
