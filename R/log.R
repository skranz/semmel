log.action = function(action,  ...,  tatid=app$tat$tatid, email=app$tat$email,app=getApp()) {
  restore.point("action.log")
  log_time = format(Sys.time())
  log.file = file.path(app$glob$log.dir, paste0("taddle-",format(Sys.Date(),"%y-%m"),".log"))

  # Don't show email in clear text only the email domain
  # and a hash for formal analysis.
  # So the admin is not tempted to look at the log and
  # study behavior in a non-anonymous way.
  if (!is.null(email)) {
    email_hash = digest(email)
    email_domain = str.right.of(email, "@")
  }

  if (is.null(tatid) & is.null(email)) {
    args = list(action=action,log_time=log_time, sid=app$session_code, ...)
  } else if (is.null(email)) {
    args = list(action=action,log_time=log_time, tatid=tatid, sid=app$session_code, ...)
  } else {
    args = list(action=action,log_time=log_time,tatid=tatid,email_domain=email_domain, email_hash=email_hash, sid=app$session_code, ...)
  }

  txt = toJSON(args)
  try(write(txt,log.file,append=TRUE))
}
