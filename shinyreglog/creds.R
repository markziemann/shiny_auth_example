# get some credentials
creds <- data.frame(
  username = "ShinyReglogTest",
  password = "VeryHardPassword",
  email = "shinyreglog@test"
)
saveRDS(creds,"creds.Rds")
