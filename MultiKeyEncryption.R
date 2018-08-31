rm(list = ls())


# The encrypt function takes plain text, seed number, and number of keys 
# you want to use for the encryption.  To decrypt, you need the encripted
# seed number as well as the number of keys
Encrypt <- function(PT,s,nkey) {
  set.seed(s)
  PT2CT <- function(PT, key) {
    num <- utf8ToInt(PT)
    num <- num - 97
    num <- (num + key) %% 26
    num <- num + 97
    CT <- intToUtf8(num)
    return(CT)
  }
  key <- round(runif(nkey,1,1000))
  if (nkey == 1) {return(PT2CT(PT,key))}
  result <- numeric(0)
  result[1] <- PT2CT(PT,key[1])
  for (i in 2:length(key)) {
    result[i] <- PT2CT(result[i-1],key[i])
  }
  return(result[length(key)])
}


Decrypt <- function(CT,s,nkey) {
  set.seed(s)
  key <- rev(round(runif(nkey,1,1000)))
  CT2PT <- function(CT, key) {
    num <- utf8ToInt(CT)
    num <- num - 97
    num <- (num - key) %% 26
    num <- num + 97
    PT <- intToUtf8(num)
    return(PT)
  }  
  if (nkey == 1) {return(CT2PT(CT,key))}
  result <- numeric(0)
  result[1] <- CT2PT(CT,key[1])
  for (i in 2:length(key)) {
    result[i] <- CT2PT(result[i-1],key[i])
  }
  return(result[length(key)])
}

