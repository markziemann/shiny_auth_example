# Write the content of your email
library("mailR")
subject <- "Test subject"
msg <- "Test mail"     
sender <- "mark.ziemann@gmail.com"
recipients <- "info@mdz-analytics.com"
gpw <- readLines("~/.googlepw")

send.mail(from = sender,
  to = recipients,
  subject = subject,
  body = msg,
  smtp = list(host.name = "smtp.gmail.com", port = 587,
  user.name = "mark.ziemann@gmail.com",
  passwd = gpw, 
  ssl = TRUE),
  authenticate = TRUE,
  send = TRUE)