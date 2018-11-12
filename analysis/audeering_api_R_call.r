library(base64enc)
library(httr)
library(rjson)
library(hash)
library(digest)
library(jsonlite)
options(scipen = 999)

privatekey <- "00e6a75f-026e-4c2c-81a4-28088f798bb9"
userid <- "cavan@sensum.co"


# hmac digest for auth
hash_hmac <- function(to_encode, secret) (
    hmac(secret, to_encode, algo='sha256', ascii=FALSE)
)
to_encode <- "test"
hash_hmac(to_encode,privatekey)
# should equal 'f4dc71f8c432e7dedf923b38cc4a2891d844d5fe0b47efb334304a2705bb7075'
DB <- read.table("C:/Users/dupred/Dropbox/mp3/mp3/list.txt")
list_DB <- as.vector(DB)$V1

for (audio in list_DB){
  # B64 Encoding
  MP3 <- paste0("https://s3.amazonaws.com/audio-emotion/",audio)
  x <- base64enc::base64encode(file(MP3,'rb'))
  # AUDIEERING AUTHENTICATION
  base_url <- "https://sensai.audeering.com/api/v1/"
  timestamp <- function() (round(as.numeric(Sys.time())*1000))
  authenticate <- function(userid, privatekey) {
    raw_auth_data <- rjson::toJSON(
      list(
        "auth-data"=list(
          userid=userid, 
          timestamp=timestamp()
          )
        )
      )
    return (httr::POST(paste(base_url, 'session', sep=''), 
                       body=raw_auth_data, 
                       httr::add_headers(.headers=c(signature=hash_hmac(raw_auth_data, privatekey))), 
                       encode='json')
            )
    }
  r <- authenticate(userid, privatekey)
  auth_token <- content(r)$result$token
  # AUDIEERING UPLOAD
  upload <- function(b64data, auth_token) {
    raw_upload_data <- rjson::toJSON(
      list(
        "config"=list(
          "format"="file", 
          "retrieval"="sync",
          "retrieval-timeout"=10000, #10 seconds
          "modules"=list("emotions"=setNames(rjson::fromJSON('{}'), character(0)))),
        "data"=list(
          "media-data"= b64data)
        )
      )
  return (httr::POST(paste(base_url, 'analyse', sep=''), 
                     body=raw_upload_data, 
                     httr::add_headers(.headers=c(token=auth_token)), 
                     encode='json',
                     verbose())
          )
    }
  r <- upload(x, auth_token)
  con <- httr::content(r)
  out <- jsonlite::base64_dec(con$results$emotions[[1]]$data)  
  jsonresult <- rawToChar(out)
  audeering_emotion <- as.data.frame(jsonlite::fromJSON(jsonresult,flatten = TRUE))
  write.csv(audeering_emotion,paste0("C:/Users/dupred/Dropbox/mp3/mp3/",audio,".csv"),row.names = FALSE)
}

library(plyr)

setwd("C:/Users/dupred/Dropbox/MP3/MP3")
data.file.names <- dir(pattern=".csv")
audeering_gemep <- ldply(data.file.names, function(file_name) {
  dat <- read.csv(file_name)
  segment_audio <- 1:nrow(dat)
  name_audio <- file_name
  ppt <- substr(name_audio, 1, 2)
  emotion <- substr(name_audio, 3, 5)
  return(data.frame(name_audio,ppt,emotion,segment_audio,dat))
})

#library(dplyr)
#audeering_gemep %>% 
#  group_by(group) %>% 
#  summarize(mean=mean(dt), sum=sum(dt))
library(tidyr)
audeering_gemep_emo <- audeering_gemep %>%
  gather(key = emotion_rec,value = scores,14:37)

audeering_gemep_emo$emotion_rec <- gsub("^.*?_","",audeering_gemep_emo$emotion_rec)