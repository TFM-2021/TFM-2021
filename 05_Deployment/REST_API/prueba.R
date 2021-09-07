
library(plumber)

r <- plumb("05_Deployment/REST_API/REST_API.R")

r$run(port=8000)

plumber::pl