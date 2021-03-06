examples.taddleApp = function() {
  restore.point.options(display.restore.point=TRUE)

  setwd("C:/libraries/taddle/")

  app = taddleApp("C:/libraries/taddle/shared")
  viewApp(app, url.args = list(key="IafFfwxoZnMeIJGnPvLz"))

  create.random.ranks("edtnlp", common.weight = 0.30,n = 10)


  app = taddleApp("D:/libraries/taddle/shared")
  create.random.ranks("zigzzd", common.weight = 0.30,n = 10)


}

show.res.ui = function(tat = app$tat, app=getApp(), show.new=FALSE,...) {

  if (is.null(tat)) {
    ui = tagList(p("Your key does not refer to an existing allocation task."))
    setUI("mainUI", ui)
    return()
  }


  ui = tagList(
    navlistPanel(id="resMainPanel",
      tabPanel("Results",value="home",uiOutput("resHomeUI")),
      tabPanel("Options", value="modify", res.modify.ui()),
      if (show.new) tabPanel("New Task", value="new", tagList(
        tags$br(),
        tags$b("Warning: If you create a new task, the results for the current task will not be accessible anymore."),
        tags$br(),
        simpleButton("resNewTaskBtn", "Create new task")
      )),
      tabPanel("Help", create.help.ui()),
      tabPanel("About", about.ui()),
      widths = c(2,10)
    ),
    tags$script(src="taddle/res.js")
  )

  if (show.new) {
    buttonHandler("resNewTaskBtn", function(...,app=getApp()) {
      app$tat = empty.tat()
      log.action("create_session", email=NULL)
      show.new.ui()
    })
  }

  eventHandler(eventId="resMainPanelClick", id=NULL, fun=res.tab.change)
  setUI("mainUI", ui)
  setUI("resHomeUI",   res.home.ui())
  log.action("res_session", method=tat$method, counts=tat$counts.table)

}

res.show.deadline = function(tat=app$tat, app=getApp()) {
  has.deadline = !is.empty.val(tat$deadline)
  diff.str = if (has.deadline) duration.string(Sys.time(),tat$deadline)
  html  = if (has.deadline) p(HTML(paste0("Deadline (Central European Time Zone): ", format(tat$deadline,"%A, %B %d at %H:%M"), " (",diff.str,")")))

  setUI("deadlineUI", html)
}

res.home.ui = function(...,tat=app$tat, app=getApp(), glob=app$glob) {
  restore.point("res.home.ui")

  strings = glob$strings

  num.inactive = sum(!tat$stu$active)
  if (is.null(tat$allocs) | sum(tat$stu$active)==0) {
    ui = tagList(
      h4(paste0(tat$title)),
      uiOutput("deadlineUI"),
      p(paste0("The task was created on ",tat$create_time)),
      p(HTML(paste0(
        "So far ", tat$num.sub-num.inactive, " ", if(num.inactive>0) paste0(" active and ", num.inactive, " deactivated") ," submissions for ", tat$num.topics, " ", strings$topics, if(tat$num.slots != tat$num.topics) paste0(" with a total of ", tat$num.slots, " slots."), if (!is.empty.val(tat$topn) & isTRUE(tat$topn>0)) paste0(" Each student ranked ", tat$topn, " ",strings$topics,".")
      )))
    )
    res.show.deadline()
    return(ui)

  }

  ct.ui = allocs.count.table.ui(tat)

  ui = tagList(
    h4(paste0(tat$title)),
    uiOutput("deadlineUI"),
    p(paste0("The task was created on ",tat$create_time)),
    p(HTML(paste0(
      "So far ", tat$num.sub-num.inactive, " ", if(num.inactive>0) paste0(" active and ", num.inactive, " deactivated") ," submissions for ", tat$num.topics, " ",strings$topics, if(tat$num.slots != tat$num.topics) paste0(" with a total of ", tat$num.slots, " slots."), if (!is.empty.val(tat$topn)  & isTRUE(tat$topn>0)) paste0(" Each student ranked ", tat$topn, " ",strings$topics,".")
    ))),
    h4(paste0("Overview of allocation mechanisms: Number of students who got their n'th ranked ", strings$topic)),
    HTML(ct.ui),
    p(paste0("Click on a row in the table above, to see the details of the allocation.", if (tat$org_method=="serialdict") " Recall that students got the information that topics are assigned with a truthful revelation mechanism (random serial dictatorship). But you can also see the details for different mechanisms.")),
    uiOutput("resUI")
    #uiOutput("optUI")
  )
  res.show.deadline()

  eventHandler(eventId="countsTableRowClick", id=NULL,fun= function(value,data,...) {
    restore.point("countsTableRowClick")
    method = tat$methods[[data$rowid]]
    res.ui = allocation.info.ui(method, tat)
    tat$method = method

    # Update selected in database
    dbUpdate(glob$db,"tat",vals = list(method=tat$method), where = list(tatid=tat$tatid))

    log.action("res_method",method=tat$method)

    setUI("resUI", res.ui)

  })

  method = tat$method
  if (!method %in% tat$methods)
    method = first(tat$methods)

  #if (tat$method %in% tat$methods) {
    res.ui = allocation.info.ui(method, tat)
    setUI("resUI", res.ui)
  #}

  #setUI("mainUI", ui)
  ui
}

get.res.tat = function(tatid, db=getApp()$glob$db) {
  restore.point("get.res.tat")
  tat = dbGet(db,"tat", list(tatid=tatid),empty.as.null = TRUE)
  if (is.null(tat)) return(NULL)

  tat = as.list(tat[1,])


  if (is.na(tat$random_seed))
    tat$random_seed = sample.int(1e9,1)

  tat$deadline_date = as.Date(tat$deadline)
  tat$deadline_time = format(tat$deadline, "%H:%M")

  tops = dbGet(db, "topic",list(tatid=tatid)) %>% arrange(pos)
  stu = dbGet(db, "student", list(tatid=tatid))
  ras = dbGet(db, "ranking", list(tatid=tatid))
  ras = left_join(ras, select(stu, studemail, studname), by="studemail")

  tat$stu = stu
  tat$ras = ras
  tat$tops = tops

  tat$num.sub = NROW(tat$stu)
  tat$num.topics = NROW(tops)
  tat$num.slots = sum(tops$slots)


  tat$allocs =compute.tat.allocations(tat)

  tat$methods = intersect(app$glob$sets$method, unique(tat$allocs$method))


  # opts = dbGet(db, "resopt",list(tatid=tat$tatid))
  # opts$pos = lapply(opts$pos,function(pos) {
  #   as.integer(strsplit(pos,",",fixed=TRUE))
  # })
  # opts$studemail = lapply(opts$studemail,function(x) {
  #   strsplit(x,",",fixed=TRUE)
  # })
  # tat$opts = opts

  tat = as.environment(tat)
  tat
}

allocs.count.table.ui = function(tat=app$tat, app=getApp()) {
  restore.point("allocs.count.table.ui")
  strings=app$glob$strings

  df = allocs.count.table(tat)

  tat$counts.table = df
  sel.row = if (tat$method %in% df$method) match(tat$method, df$method)

  count.mat = as.matrix(df[,-1,drop=FALSE])
  mat = as.matrix(df)
  mat[mat=="0"] = ""

  mat[,1] = to.label(df$method, app$glob$sets$method)


  df = as.data.frame(mat)

  # Create sparklines
  df$sl = unlist(lapply(seq_len(NROW(df)), function(row) {
    restore.point("count.sparkline")
    vals = count.mat[row,]
    names(vals) = NULL
    spk_chr(vals, type="bar",chartRangeMin=0)
  }))

  df = select(df, method, sl, everything())


  has.na = colnames(df)[NCOL(df)] == "<NA>"
  col.names = c("","", paste0("Rank ", colnames(mat[,-(1), drop=FALSE])))

  if (has.na) {
    col.names[length(col.names)] = paste0("No ", strings$Topic)
  }


  html = simpleTable(id="counts-table",class="simple-table count-table", df=df,sel.row=sel.row, col.names = col.names)
  HTML(html)

}

allocs.count.table = function(tat=app$tat, app=getApp()) {
  restore.point("allocs.count.table")
  allocs = tat$allocs
  tat$methods = methods = intersect(app$glob$sets$method, unique(allocs$method))


  max.rank = max(allocs$rank, na.rm=TRUE)
  if (!is.finite(max.rank))
    return(NULL)

  all = expand.grid(
    method=methods,
    rank=c(1:max.rank, if(any(is.na(allocs$rank))) NA_integer_)
  )

  sum = allocs %>% group_by(method, rank) %>%
    summarize(count = n()) %>%
    right_join(all, by=c("method","rank")) %>%
    spread(rank, count)
  sum[is.na(sum)] = 0L
  sum = sum[match(methods,sum$method),]
  sum
}


compute.tat.allocations = function(tat=app$tat, app=getApp()) {
  restore.point("compute.tat.allocations")
  if (NROW(tat$ras)==0) return(NULL)
  methods = unlist(app$glob$sets$method)
  allocs = bind_rows(lapply(methods, compute.tat.allocation, tat=tat))
  return(allocs)
}

compute.tat.allocation = function(method = "costmin_lin", tat, no.match.pref.add=1) {
  restore.point("compute.tat.allocation")


  all.ranked = is.empty.val(tat$topn)

  ras = tat$ras

  ras = arrange(ras, studemail, pos)
  studs = unique(ras$studemail)

  inactive = tat$stu$studemail[!tat$stu$active]


  fixed_stud = tat$stu$studemail[!is.na(tat$stu$fixed_pos)]
  fixed_stud = setdiff(fixed_stud, inactive)


  topics = tat$tops$topic
  studs = setdiff(studs, c(inactive,fixed_stud))


  # Substract slots from topics that have been
  # exogenously fixed for some students
  rows = match(fixed_stud, tat$stu$studemail)
  fixed_pos = tat$stu$fixed_pos[rows]
  minus.fixed.slots = tabulate.to(fixed_pos, NROW(tat$tops$slots))
  org.slots = tat$tops$slots
  slots = pmax(0,tat$tops$slots - minus.fixed.slots)


  ras = filter(ras, studemail %in% studs)


  # Have some active , non-fixed students
  if (NROW(ras)>0) {
    n = length(studs)
    T = max(ras$pos)
    prefs = matrix(ras$rank, nrow=n, ncol=T, byrow = TRUE)

    if (method == "serialdict") {
      prios = with.random.seed(runif(NROW(prefs)),seed = tat$random_seed)
      alloc = serial.dictator.alloc(prefs, prios=prios, slots = slots)
      restore.point("serialdict.alloc")
    } else if (method == "costmin_lin") {
      alloc = assignment.problem.alloc(prefs,rank.costs = (1:T)^(1.01), slots=slots)
    } else if (method == "costmin_quad") {
      alloc = assignment.problem.alloc(prefs,rank.costs = (1:T)^2, slots=slots)
    } else if (method == "costmin_cubic") {
      alloc = assignment.problem.alloc(prefs,rank.costs = (1:T)^3, slots=slots)
    } else if (method == "costmin_3_5") {
      costs = (1:T)^1.01
      if (T>3) costs[4] = 1000
      if (T>4) costs[5] = 1500
      if (T>5) costs[6:T] = costs[6:T]*10000

      alloc = assignment.problem.alloc(prefs,rank.costs = costs, slots)
    }
    rank = prefs[cbind(1:n,alloc)]

    res = data_frame(method = method, studemail=studs,rank=rank, pos=alloc, topic=topics[alloc], slots=org.slots[alloc], fixed=FALSE)

  } else {
    # no active, non-fixed students
    res = NULL
  }


  # Add fixed students
  if (length(fixed)>0) {
    fstu = filter(tat$stu, studemail %in% fixed_stud) %>%
      mutate(pos = fixed_pos, topic=topics[fixed_pos]) %>%
      left_join(select(tat$ras,studemail, pos, rank),by=c("studemail","pos"))

    fix.res = transmute(fstu,method=method, studemail=studemail, rank=rank, pos=pos, topic=pos, slots=org.slots[pos], fixed=TRUE)
    res = rbind(res, fix.res) %>%
      arrange(pos, studemail)
  }

  # Compute filled_slots
  res = res %>% group_by(pos) %>%
    mutate(filled_slots = ifelse(!is.na(slots),n(),NA)) %>%
    ungroup()


  if (length(inactive)>0) {
    ia.res = data_frame(method=method, studemail=inactive, rank=NA, pos=NA, topic=NA, slots=NA, fixed=NA, filled_slots=NA)
    res = rbind(res, ia.res)
  }

  res
}

allocation.info.ui = function(method = tat$method, tat=app$tat, app=getApp(), use.sparklines=TRUE) {
  restore.point("allocation.info.ui")

  strings = app$glob$strings

  .method = method
  alloc = filter(tat$allocs, method==.method) %>%
    arrange(pos) %>% left_join(tat$stu, by="studemail")

  todf = full_join(select(tat$tops,pos, topic, slots), select(alloc,-topic,-slots),by="pos") %>%
    mutate(filled_slots = ifelse(is.na(filled_slots),0, filled_slots)) %>%
    mutate(slots = ifelse(is.na(slots),NA,paste0(filled_slots, " of ", slots))) %>%
    select(pos, topic, studname, rank, studemail, slots, active,fixed)

  todf$studname = htmlEscape(todf$studname)
  todf$studemail = htmlEscape(todf$studemail)

  # Create sparklines
  ras = tat$ras %>% filter(!is.na(rank))
  max.rank = max(ras$rank, na.rm=TRUE)
  cc = rep("blue", max.rank)
  todf$sl = ""

  max.bar = max(table(paste0(ras$pos,";",ras$rank)))

  rows = which(!is.na(todf$pos))
  .pos = 0
  for (row in rows) {
    if (.pos != todf$pos[row]) {
      .pos = todf$pos[row]
      ranks = filter(ras, pos==.pos)$rank
      tabs = tabulate.to(ranks,max.rank)
    }
    ccn = cc
    ccn[todf$rank[row]] = "red"
    todf$sl[row]  = spk_chr(tabs,chartRangeMin=0, chartRangeMax=max.bar, type="bar", colorMap = ccn, tooltipFormat = '{{value}} ranked as 1+{{offset}}')
  }

  na.rows = which(is.na(todf$pos))
  todf$topic[na.rows] = paste0("-No ", strings$Topic,"-")
  todf$pos[na.rows] = ""
  todf$rank[na.rows] = ""
  todf$slots[na.rows] = ""
  na.rows = which(is.na(todf$studname))
  todf$studname[na.rows] = ""
  todf$studemail[na.rows] = ""
  todf$rank[na.rows] = ""
  #todf$slots[na.rows] = ""

  todf$topic[is.false(todf$active)] = "-Student Deactivated-"
  row.class = ifelse(is.false(todf$active),"inactive",ifelse(todf$fixed,"fixed-topic", ""))

  show.slots = any(is.true(alloc$slots>1)) | any(is.true(tat$tops$slots>1))

  if (show.slots) {
    todf = select(todf, pos, topic, studname, rank, sl, studemail, slots)
    col.names = c("",strings$Topic,"Student","Ranked as",paste0(strings$Topic," Ranks"),"Email", "Filled Slots")
  } else {
    todf = select(todf, pos, topic, studname, rank, sl, studemail)
    col.names = c("",strings$Topic,"Student","Ranked as",paste0(strings$Topic," Ranks"),"Email")

  }
  tohtml = simpleTable(id="alloc-table", df=todf,wrap = TRUE, col.names = col.names, row.class = row.class,class = "simple-table")

  mlab = to.label(method, app$glob$sets$method)
  ui = tagList(
    h4(paste0("Allocation via ", mlab)),
    downloadButton("excelDownloadBtn","Download (Excel)"),
    downloadButton("wordDownloadBtn","Download (Word)"),
    #simpleButton("sendAllocEmailBtn", icon = icon("envelope"), "Results email for students..."),
    HTML(tohtml)
  )

  setDownloadHandler("excelDownloadBtn",
    filename=function(app = getApp())
      paste0(strings$topic,"_allocation.xlsx"),
    content = function(file, ...) {
      restore.point("downloadTopics")
      app=getApp()
      withProgress(message="Excel file is generated, please wait a moment...", {
        if (show.slots) {
          alloc.df = select(todf, Pos=pos, Topic=topic, Student=studname, Email=studemail, Rank=rank, Filled_Slots=slots)

        } else {
          alloc.df = select(todf, Pos=pos, Topic=topic, Student=studname, Email=studemail, Rank=rank)
        }
        colnames(alloc.df)[2] = strings$Topic
        tabs = c(list(allocation=alloc.df), compute.ranking.df())
        library(writexl)
        write_xlsx(tabs, file)
      })
      log.action("res_excel",method=tat$method)
    }
  )

  setDownloadHandler("wordDownloadBtn",
    filename=function(app = getApp())
      paste0(paste0(strings$topic,"_allocation.docx")),
    content = function(file, ...) {
      restore.point("downloadWordTopics")
      app=getApp()
      withProgress(message="Word file is generated, please wait a moment...", {
        if (show.slots) {
          alloc.df = select(todf, Pos=pos, Topic=topic, Student=studname, Email=studemail, Rank=rank, Filled_Slots=slots)
        } else {
          alloc.df = select(todf, Pos=pos, Topic=topic, Student=studname, Email=studemail, Rank=rank)
        }
        alloc.word.report(file, alloc.df, tat)
      })
      log.action("res_word",method=tat$method)
    }
  )


  ui
}

compute.ranking.df = function(tat=app$tat,  app=getApp()) {
  restore.point("compute.ranking.df")
  strings = app$glob$strings

  ras = tat$ras
  mat = ras %>% select(studemail, rank, pos) %>%
    arrange(studemail,pos) %>%
    spread(key = pos, value = rank)
  colnames(mat)[-1] = paste0(strings$Topic," ", colnames(mat)[-1])

  mar = ras %>% filter(!is.na(rank)) %>%
    select(studemail, rank, pos) %>%
    arrange(studemail,rank) %>%
    spread(key = rank, value = pos)
  colnames(mar)[-1] = paste0("Rank ", colnames(mar)[-1])

  res = list("rank_by_topic"=mat, "topic_by_rank"=mar)
  names(res) = c(paste0("rank_by_", strings$topic),paste0(strings$topic,"_by_rank"))
  res
}

refresh.alloc.and.ui = function(tat=app$tat, app=getApp()) {
  restore.point("refresh.alloc.and.ui")
  tat$allocs =compute.tat.allocations(tat)
  show.res.ui()
}

res.questions.ui = function() {
  ui = tagList(
    p("For our research project, we would love to know, how Semmel is used. ")

  )
}

res.modify.ui = function(tat = app$tat, app=getApp()) {
  restore.point("res.modify.ui")
  strings = app$glob$strings

  topics = tat$tops$topic

  slot.input = tableSelectInputVector(paste0("slots-", seq_along(topics)),value = tat$tops$slots, choices=0:round(max(20,max(tat$tops$slots*1.5), NROW(tat$stu)+5)) , extra.class="slots-input")

  slots.tab = simpleTable(df=data_frame(Pos=seq_along(topics), Slots=slot.input, Topic=topics),class="simple-table slots-table")

  studs.active = checkBoxInputVector(paste0("active-", seq_len(NROW(tat$stu))),label = NULL,value = tat$stu$active, extra.class="stud-active")

  fix.topic.btn = simpleButtonVector(paste0("fix-topic-",seq_len(NROW(tat$stu))),label="", extra.class="fix-topic-btn", size="xs",icon = icon("pencil"))

  remove.fix.topic.btn = simpleButtonVector(paste0("remove-fix-topic-",seq_len(NROW(tat$stu))),label="", extra.class="remove-fix-topic-btn", size="xs",icon = icon("remove"))

  fixed.col = paste0("<div id='fixed-topic-div-",seq_len(NROW(tat$stu)),"'>", ifelse(is.na(tat$stu$fixed_pos), fix.topic.btn, paste0(remove.fix.topic.btn,tat$tops$topic[tat$stu$fixed_pos])), "</div>")

  if (NROW(tat$stu)>0) {
    studs.tab = simpleTable(id="stud-opt-table", df=data_frame(Active=studs.active, Student=htmlEscape(tat$stu$studname), Email=(tat$stu$studemail), "Fixed Topic"= fixed.col), class="simple-table stud-table", , row.class=ifelse(tat$stu$active,"","inactive"))
  } else {
    studs.tab = "<p>--- No student registered yet ---</p>"
  }

  ui = tagList(
    h4(paste0("Customize ",tat$title)),
    h4("Don't forget to save your changes!"),
    uiOutput("sresModifyAlert"),
    #simpleButton("saveResModBtn","Save Changes and Update Results",form.sel = ".slots-input, .active-checkbox"),

    p("You can change the deadline (Central European Time Zone) until students should enter their preferences:"),
    uiOutput("deadlineModAlert"),
    tags$table(
      tags$td(shiny::dateInput("deadline_date","Deadline Date", value=tat$deadline_date)),
      tags$td(style="padding-left: 2em;", simpleTimeInput("deadline_time", "Deadline Time", width="12em", value=tat$deadline_time))
    ),
    downloadButton("downloadRankingBtn","Download ranking (for custom matching)."),
    br(),
    #simpleButton("redrawSeedBtn","Redraw priorities to change allocation under random serial dictatorship"), br(),
    tags$b(paste0("Modify a ", strings$topic,"'s number of slots:")),
    HTML(slots.tab),
    br(),
    tags$b("Deactivate students or fix topics:"),
    HTML(studs.tab),
    p("Notes: If you fix a topic for a student, the student will still take up a slot.")
  )

#  buttonHandler("redrawSeedBtn", function(...) {
#    restore.point("redrawSeedBtn")
#    # TO DO Change Priorities
#    app$updated.options = TRUE
#  })

  changeHandler("deadline_date",fun=function(value,...) {
    args = list(...)
    restore.point("deadline_date_change")
    tat$deadline_date = value
    res.change.deadline()
  })
  idEventHandler("deadline_time",fun=function(value,...) {
    args = list(...)
    restore.point("deadline_time_change")
    tat$deadline_time = value
    res.change.deadline()
  })


  classEventHandler("fix-topic-btn", event="click", function(id, ...) {
    args = list(...)
    restore.point("fix-topic-btn-click")

    row = as.integer(str.right.of(id,"fix-topic-"))
    stu = tat$stu[row,]
    email = stu$studemail
    ra = filter(tat$ras, studemail == email) %>%
      left_join(select(tat$tops, pos, topic),by="pos") %>%
      arrange(rank)

    #ra$taken = ra$pos %in% tat$stu$fixed_pos

    btns = simpleButtonVector(paste("fix-pos-", ra$pos), label="Choose",size = "sm", extra.class = "fix-pos-btn")
    #btns[ra$taken] = "Fixed for other student"

    tab = simpleTable(id="fix-pos-table", df=data_frame(btns, ra$rank, ra$topic), col.names = c("","Rank","Topic"))

    classEventHandler("fix-pos-btn", event="click", function(id,...) {
      restore.point("fix-pos-btn-click")
      pos = as.integer(str.right.of(id,"fix-pos-"))
      tat$stu$fixed_pos[row] = pos
      dbUpdate(app$glob$db, "student", list(fixed_pos=pos),list(tatid=tat$tatid, studemail=email))
      log.action("fix_pos", studemail=email, fixed_pos=pos)

      # Update table cell
      html = paste0(remove.fix.topic.btn[row],tat$tops$topic[tat$stu$fixed_pos[row]])
      setInnerHTML(id=paste0('fixed-topic-div-',row), html)
      app$updated.options = TRUE

      removeModal()
    })

    showModal(modalDialog(easyClose=TRUE, size="l", title = paste0("Fix a topic for student ", stu$studname),
      tagList(
        p(paste0("The table shows the topics as ranked by ", stu$studname,". Press the corresponding 'Choose' button to fix that topic for the student.")),
        HTML(tab)
      )
    ))


  })

  classEventHandler("remove-fix-topic-btn", event="click", function(id, ...) {
    args = list(...)
    restore.point("fix-topic-btn-click")
    row = as.integer(str.right.of(id,"fix-topic-"))
    email = tat$stu$studemail[row]
    log.action("del_fix_pos", studemail=email, fixed_pos=tat$stu$fixed_pos[row])
    tat$stu$fixed_pos[row] = NA
    dbUpdate(app$glob$db, "student", list(fixed_pos=NA),list(tatid=tat$tatid, studemail=email))

    # Update table cell
    html = paste0(fix.topic.btn[row])
    setInnerHTML(id=paste0('fixed-topic-div-',row), html)
    app$updated.options = TRUE
  })


  classEventHandler("slots-input",event = "change", function(id, value,...) {
    #args = list(...)
    restore.point("slots-input-change")
    pos = as.integer(str.right.of(id,"slots-"))
    old_slots = tat$tops$slots[pos]
    slots = as.integer(value)
    tat$tops$slots[pos] = slots
    tat$num.slots = sum(tat$tops$slots)

    dbUpdate(app$glob$db,"topic", list(slots=slots), list(tatid=tat$tatid, pos=pos))
    app$updated.options = TRUE

    log.action("mod_slots", pos=pos, old_slots=old_slots, new_slots=slots)

  })

  checkboxChangeHandler(class="stud-active", fun= function(id, checked,...) {
    args = list(...)
    restore.point("stud-active-change")
    row = as.integer(str.right.of(id,"active-"))
    tat$stu$active[row] = checked
    dbUpdate(app$glob$db,"student", list(active=checked), list(tatid=tat$tatid, studemail=tat$stu$studemail[row]))

    if (checked) {
      log.action("activate_stud", studemail = tat$stu$studemail[row])
    } else {
      log.action("deactivate_stud", studemail = tat$stu$studemail[row])
    }

    app$updated.options = TRUE
  })

  setDownloadHandler("downloadRankingBtn",
    filename=function(app = getApp())
      paste0(strings$topic,"_ranking.csv"),
    content = function(file, ...) {
      restore.point("downloadRanking")
      app=getApp()
      withProgress(message="CSV file of ranking is generated, please wait a moment...", {
        rdat = left_join(tat$ras, tat$tops, by=c("pos","tatid"))
  tat$stu
        write.csv(rdat, file,row.names = FALSE)
      })
      log.action("download_ranking")
    }
  )


  ui
}

res.change.deadline = function(tat = app$tat, app=getApp()) {
  restore.point("res.change.deadline")

  if (is.empty.val(tat$deadline_time) | is.empty.val(tat$deadline_date)) {
    if (is.empty.val(tat$deadline)) return()

    # Remove deadline
    tat$deadline = NA
    log.action("remove_deadline",org_deadline=format(tat$deadline))
    tat$deadline = NA
    dbUpdate(app$glob$db,"tat",list(deadline=tat$deadline), list(tatid=tat$tatid))
    timedMessage("deadlineModAlert",colored.html(paste0("Deadline removed."), color="#0000cc"), millis=5000)
    res.show.deadline()
    return()
  }

  # Try to set new deadline
  deadline = NA
  try({deadline = as.POSIXct(paste0(tat$deadline_date," ", tat$deadline_time))})
  if (is.na(deadline)) {
    timedMessage("deadlineModAlert",colored.html("I could not parse your entered deadeline."), millis=5000)
    return()
  }
  if(identical(deadline, tat$deadline))
    return()

  tat$deadline = deadline
  log.action("set_deadline",deadline=format(tat$deadline))
  dbUpdate(app$glob$db,"tat",list(deadline=tat$deadline), list(tatid=tat$tatid))
  timedMessage("deadlineModAlert",colored.html(paste0("Deadline changed to ", format(tat$deadline)), color="#0000cc"), millis=5000)
  res.show.deadline()

}

res.tab.change = function(value,...,tat=app$tat,app=getApp()) {
  restore.point("res.tab.change")
  cat("\nto tab ", value)

  if (value=="home" & isTRUE(app$updated.options)) {
    app$updated.options = FALSE
    tat$allocs =compute.tat.allocations(tat)
    ui = res.home.ui(tat=tat)
    log.action("res_update",method=tat$method, counts=tat$counts.table)
    setUI("resHomeUI",ui)
  }
}


alloc.word.report = function(out.file, alloc.df, tat = app$tat, app=getApp()) {
  restore.point("alloc.word.report")
  strings = app$glob$strings

  library(officer)
  tpl.file = system.file("tpl/alloc_tpl.docx", package="semmel")
  tab_style = "Plain Table 2"
  doc = read_docx(tpl.file)
  doc = doc %>%
    body_add_par(paste0("Allocation for ", tat$title), style = "heading 1") %>%
    body_add_par(paste0("Allocation Method: ", to.label(tat$method, app$glob$sets$method)), style = "heading 2") %>%
    body_add_par("You can copy and paste the appropriate table from this report. Each table starts on a new page.")

  df = select(alloc.df, Pos, Topic, Student, Email)
  df = clear.same.prev.line(df, c("Pos","Topic"))
  colnames(df)[2] = strings$Topic
  doc = doc %>%
    body_add_par(paste0("Sorted by ", strings$Topics), style = "heading 1") %>%
    body_add_par("") %>%
    body_add_table(df, style=tab_style)
  doc = doc %>%
    body_add_break() %>%
    body_add_par(paste0("Sorted by ", strings$Topics," (No Email)"), style = "heading 1") %>%
    body_add_par("") %>%
    body_add_table(select(df,-Email), style=tab_style)

  df = select(alloc.df, Student, Email, Topic) %>% arrange(Student)
  df = filter(df, nchar(Student)>0 | nchar(Email)>0)
  colnames(df)[3] = strings$Topic
  doc = doc %>%
    body_add_break() %>%
    body_add_par("Sorted by Student", style = "heading 1") %>%
    body_add_par("") %>%
    body_add_table(df, style=tab_style)

  doc = doc %>%
    body_add_break() %>%
    body_add_par("Sorted by Student (No Email)", style = "heading 1") %>%
    body_add_par("") %>%
    body_add_table(select(df,-Email) , style=tab_style)

  print(doc, target = out.file)
  invisible(doc)
}

clear.same.prev.line = function(df, cols) {
  for (col in cols) {
    val = as.character(df[[col]])
    same = val == lag(val)
    val[same] = ""
    df[[col]] = val
  }
  df
}

