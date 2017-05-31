######## server.R file for shiny app

library(shiny)
library(plyr)
library(dplyr)
library(readxl)
library(feather)
library(lubridate)
library(data.table)
library(Hmisc)
library(xlsx)
#################################################################################
#################################################################################
Assembler2 <- function(real, ph)
{
  rows <- which(unlist(!is.na(real[,13])))
  real <- real[rows,]
  ### comparison step
  last_date <- as.Date(ph$date[max(which(ph$real == 1))])
  mld <- max(which(ph$real == 1))
  errors <- unlist(real[(mld+1):nrow(real),13]) - ph$pun[(mld+1):nrow(real)]
  r <- (mld+1):nrow(real)
  #write.xlsx(data.frame(ph[r,1:(ncol(ph)-2)],Errors = errors), "C:/Users/utente/Documents/forward_pun_model_error/errors.xlsx", row.names = FALSE, append = TRUE)
  ### assembling step
  re <- rep(0, nrow(ph))
  
  for(i in 1:length(rows))
  {
    ph[i, "pun"] <- unlist(real[rows[i],13])
    re[i] <- 1
  }
  ph <- data.frame(ph, real = re)
  return(ph)
}
#################################################################################
Redimensioner_pkop <- function(ph, mh, mw, from, to, what)
{
  #### @BRIEF: if what == PK => mw is referring to PK
  d_f <- data_frame()
  from <- as.Date(from)
  to <- as.Date(to)
  nOP <- nrow(ph[which(as.Date(ph$date) >= from & as.Date(ph$date) <= to & ph$`PK.OP` == "OP"),])
  nPK <- nrow(ph[which(as.Date(ph$date) >= from & as.Date(ph$date) <= to & ph$`PK.OP` == "PK"),])
  rOP <- which(as.Date(ph$date) >= from & as.Date(ph$date) <= to & ph$`PK.OP` == "OP")
  rPK <- which(as.Date(ph$date) >= from & as.Date(ph$date) <= to & ph$`PK.OP` == "PK")
  M <- nOP + nPK
  
  periodpk <- ph[rPK,]
  periodop <- ph[rOP,]
  
  nPKr <- length(which(periodpk$real == 1))
  nOPr <- length(which(periodop$real == 1))
  
  if(what == "PK")  
  {
    opm <- (1/nOP)*((mh*M) - (mw*nPK))
    
    
    pbpk <- ifelse(length(periodpk$pun[periodpk$real == 1]) > 0, (1/nPK)*sum(periodpk$pun[periodpk$real == 1]), 0)
    pbop <- ifelse(length(periodop$pun[periodop$real == 1]) > 0, (1/nOP)*sum(periodop$pun[periodop$real == 1]), 0)
    pihatpk <- (mw - pbpk)/((1/nPK)*sum(periodpk$pun[periodpk$real == 0]))
    pihatop <- (opm - pbop)/((1/nOP)*sum(periodop$pun[periodop$real == 0]))
    for(i in 1:length(rPK))
    {
      if(ph[rPK[i], "real"] == 0) ph[rPK[i], "pun"] <- pihatpk * unlist(ph[rPK[i], "pun"])
    }
    for(i in 1:length(rOP))
    {
      if(ph[rOP[i], "real"] == 0) ph[rOP[i], "pun"] <- pihatop * unlist(ph[rOP[i], "pun"])
    }
  }
  else
  {
    pkm <- (1/nPK)*((mh*M) - (mw*nOP))
    
    pbpk <- ifelse(length(periodpk$pun[periodpk$real == 1]) > 0, (1/nPK)*sum(periodpk$pun[periodpk$real == 1]), 0)
    pbop <- ifelse(length(periodop$pun[periodop$real == 1]) > 0, (1/nOP)*sum(periodop$pun[periodop$real == 1]), 0)
    pihatpk <- (pkm - pbpk)/((1/nPK)*sum(periodpk$pun[periodpk$real == 0]))
    pihatop <- (mw - pbop)/((1/nOP)*sum(periodop$pun[periodop$real == 0]))
    for(i in 1:length(rPK))
    {
      ph[rPK[i], "pun"] <- pihatpk * unlist(ph[rPK[i], "pun"])
    }
    for(i in 1:length(rOP))
    {
      ph[rOP[i], "pun"] <- pihatop * unlist(ph[rOP[i], "pun"])
    }
  }
  
  return(ph)
}
#################################################################################
WeekRedimensioner <- function(ph, mh, from, to)
{
  #### @BRIEF: if what == PK => mw is referring to PK
  d_f <- data_frame()
  from <- as.Date(from)
  to <- as.Date(to)
  M <- nrow(ph[which(as.Date(ph$date) >= from & as.Date(ph$date) <= to),])
  rows <- which(as.Date(ph$date) >= from & as.Date(ph$date) <= to)
  
  
  period <- ph[rows,]
  
  pb <- ifelse(length(period$pun[period$real == 1]) > 0, (1/M)*sum(period$pun[periodpk$real == 1]), 0)
  
  pihat <- (mh - pb)/mean(period$pun[period$real == 0])
  
  for(i in 1:length(rows))
  {
    ph[rows[i], "pun"] <- pihat * unlist(ph[rows[i], "pun"])
  }
  
  
  return(ph)
}
###################################################################
AnalyzePeriod <- function(s)
{
  mesi17 <- c("Gennaio_17", "Febbraio_17", "Marzo_17", "Aprile_17", "Maggio_16", "Giugno_17", 
              "Luglio_17", "Agosto_17", "Settembre_17", "Ottobre_17", "Novembre_17", "Dicembre_17")
  mesi18 <- c("Gennaio_18", "Febbraio_18", "Marzo_18", "Aprile_18", "Maggio_18", "Giugno_18", 
              "Luglio_18", "Agosto_18", "Settembre_18", "Ottobre_18", "Novembre_18", "Dicembre_18")
  
  splitted <- strsplit(s, "-")
  YEAR <- 0
  
  if(length(splitted[[1]]) > 1)
  {
    #### weeks
    split1 <- strsplit(splitted$Periodo[1], "/")
    split2 <- strsplit(splitted$Periodo[2], "/")
    from <- paste0(as.numeric(split1[[1]][1]),'-',as.numeric(split1[[1]][2]),'-',as.numeric(split1[[1]][3]))
    to <- paste0(as.numeric(split2[[1]][1]),'-',as.numeric(split2[[1]][2]),'-',as.numeric(split2[[1]][3]))
    YEAR <- as.numeric(split2[[1]][1])
  }
  else if(length(splitted[[1]]) == 1)
  {
    if(tolower(s) %in% tolower(mesi17))
    {
      YEAR <- 2017
      mese <- ifelse(which(tolower(s) == tolower(mesi17)) < 10, paste0('0',which(tolower(s) == tolower(mesi17))), which(tolower(s) == tolower(mesi17)))
      from <- paste0(YEAR, '-', mese, '-01')
      to <- paste0(YEAR, '-', mese, '-', days_in_month(as.Date(from)))
    }
    else if(tolower(s) %in% tolower(mesi18))
    {
      YEAR <- 2018
      mese <- ifelse(which(tolower(s) == tolower(mesi18)) < 10, paste0('0',which(tolower(s) == tolower(mesi18))), which(tolower(s) == tolower(mesi18)))
      from <- paste0(YEAR, '-', mese, '-01')
      to <- paste0(YEAR, '-', mese, '-', days_in_month(as.Date(from)))
    }
    else if(s == 'Q1_17')
    {
      YEAR <- 2017
      from <- '2017-01-01'
      to <- '2017-03-31'
    }
    else if(s == 'Q2_17')
    {
      YEAR <- 2017
      from <- '2017-04-01'
      to <- '2017-06-30'
    }
    else if(s == 'Q3_17')
    {
      YEAR <- 2017
      from <- '2017-07-01'
      to <- '2017-09-30'
    }
    else if(s == 'Q4_17')
    {
      YEAR <- 2017
      from <- '2017-10-01'
      to <- '2017-12-31'
    }
    else if(s == 'Q1_18')
    {
      from <- '2018-01-01'
      to <- '2018-03-31'
    }
    else if(s == 'Q2_18')
    {
      YEAR <- 2018
      from <- '2018-04-01'
      to <- '2018-06-30'
    }
    else if(s == 'Q3_18')
    {
      YEAR <- 2018
      from <- '2018-07-01'
      to <- '2018-09-30'
    }
    else if(s == 'Q4_18')
    {
      YEAR <- 2018
      from <- '2018-10-01'
      to <- '2018-12-31'
    }
    else
    {### BSL annuale
      YEAR <- 2018
      from <- paste0(YEAR, '-01-01')
      to <- paste0(YEAR, '-12-31')
    }
  }
  return(list(from = from, to = to))
}
###################################################################
EstraiAnno <- function(ft)
{
  splitted <- strsplit(ft, "-")
  return(as.numeric(splitted[[1]][1]))
}
###################################################################
GetQ <- function(m)
{
  if(m <= 3) return("Q1")
  else if(m > 3 & m <= 6) return("Q2")
  else if(m > 6 & m <= 9) return("Q3")
  else return("Q4")
}
###################################################################
TFileReader <- function()
{
  mesi <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  ore7 <- read_excel("C:/Users/utente/Documents/shinyapp/sudd_ore_anno.xlsx", sheet = '2017')
  ore8 <- read_excel("C:/Users/utente/Documents/shinyapp/sudd_ore_anno.xlsx", sheet = '2018')
  
  files<- list.files("C:/Users/utente/Documents/shinyapp")
  fq <- grep('mercato', files, value=TRUE)
  df <- read_excel(paste0("C:/Users/utente/Documents/shinyapp/", fq), skip = 4)
  df <- df[,1:7]
  colnames(df) <- c("Period","Base.1","Peak.1","OffPeak.1","Base.2","Peak.2","OffPeak.2")
  df[is.na(df)] <- 0
  d_f <- data_frame()
  for( i in 1:nrow(df))
  {
    if(as.numeric(df$Base.1[i]) + as.numeric(df$Peak.1[i]) > 0)
    {
      per <- as.character(df$Period[i])
      if(per %in% mesi | per %in% c("Q3", "Q4") | per == "Y") per <- as.character(paste0(per, "_17"))
      d.f <- data.frame(period = as.character(per), BSL = df$Base.1[i], PK = df$Peak.1[i], stringsAsFactors=FALSE)
      l = list(d_f, d.f)
      d_f <- rbindlist(l)
    }
    else
    {
      next
    }
  }
  for( i in 1:nrow(df))
  {
    if(as.numeric(df$Base.2[i]) + as.numeric(df$Peak.2[i]) > 0)
    {
      per <- as.character(df$Period[i])
      if(per %in% mesi | per %in% c("Q1", "Q2","Q3", "Q4") | per == "Y") per <- as.character(paste0(per, "_18"))
      d.f <- data.frame(period = as.character(per), BSL = df$Base.2[i], PK = df$Peak.2[i], stringsAsFactors=FALSE)
      l = list(d_f, d.f)
      d_f <- rbindlist(l)
    }
    else
    {
      next
    }
  }
  ##### compute the missing values #####
  usageQ7 <- c(0,0,0,0)
  usageQ8 <- c(0,0,0,0)
  
  Q1 <- mesi[1:3]
  Q2 <- mesi[4:6]
  Q3 <- mesi[7:9]
  Q4 <- mesi[10:12]
  
  Q1n <- c(1:3)
  Q2n <- c(4:6)
  Q3n <- c(7:9)
  Q4n <- c(10:12)
  
  Q1nm <- c("01","02","03")
  Q2nm <- c("04","05","06")
  Q3nm <- c("07","08","09")
  Q4nm <- c("10","11","12")
  
  DF <- data_frame()
  DF <- bind_rows(DF, data.frame(inizio = '2016-01-01', fine = '2016-01-31', BSL = 0, PK = 0))
  for(i in 1:nrow(d_f))
  {
    if(!(strsplit(d_f$period[i], "_")[[1]][1] %in% mesi) & !(strsplit(d_f$period[i], "_")[[1]][1] %in% c("Q1","Q2","Q3","Q4")) & strsplit(d_f$period[i], "_")[[1]][1] != "Y")
    {
      split1 <- gsub(" ", "", strsplit(d_f$period[i], "-")[[1]][1], fixed = TRUE)
      split2 <- gsub(" ", "", strsplit(d_f$period[i], "-")[[1]][2], fixed = TRUE)
      
      ss1 <- strsplit(split1, "/")[[1]]
      ss2 <- strsplit(split2, "/")[[1]]
      
      start <- paste0(ss1[3], "-", ss1[2], "-", ss1[1])
      end <- paste0(ss2[3], "-", ss2[2], "-", ss2[1])
      
      d.f <- data.frame(inizio = start, fine = end, BSL = d_f$BSL[i], PK = d_f$PK[i])
      
      if(!(start %in% DF$inizio))
      {
        l <- list(DF, d.f)
        DF <- rbindlist(l)
      }
    }
    
    else if(strsplit(d_f$period[i], "_")[[1]][1] %in% mesi)
    {
        mm <- strsplit(d_f$period[i], "_")[[1]][1]
        y <- strsplit(d_f$period[i], "_")[[1]][2]
        m <- lubridate::month(Sys.Date())
        Q <- get(GetQ(which(mesi == mm)))
        Qn <- get(paste0(GetQ(which(mesi == mm)), "n"))
        Qnm <- get(paste0(GetQ(which(mesi == mm)), "nm"))
        
        if(y == "17")
        {
          usageQ7[Qn[3]/3] <- 1
        }
        else
        {
          usageQ8[Qn[3]/3] <- 1
        }
        
        if(m %in% Qn)
        {
          if(m == Qn[1])
          {
            
            if(paste0(Q[2],"_",y) %in% d_f$period & paste0(Q[3],"_",y) %in% d_f$period)
            {
              ind2 <- which(d_f$period == paste0(Q[2],"_",y))
              ind3 <- which(d_f$period == paste0(Q[3],"_",y))
              start2 <- paste0("20",y,"-",Qnm[2], "-01")
              end2 <- paste0("20",y,"-",Qnm[2], "-", lubridate::days_in_month(as.Date(start, origin = "1899-12-30")))
              start3 <- paste0("20",y,"-",Qnm[3], "-01")
              end3 <- paste0("20",y,"-",Qnm[3], "-", lubridate::days_in_month(as.Date(start, origin = "1899-12-30")))
              d.f2 <- data.frame(inizio = start2, fine = end2, BSL = d_f$BSL[ind2], PK = d_f$PK[ind2], stringsAsFactors = FALSE) 
              d.f3 <- data.frame(inizio = start3, fine = end3, BSL = d_f$BSL[ind3], PK = d_f$PK[ind3], stringsAsFactors = FALSE) 
              #d.f <- rbind(d.f, d.f2)
              if(!(start2 %in% DF$inizio))
              {
                l <- list(DF, d.f2)
                DF <- rbindlist(l)
              }
              if(!(start3 %in% DF$inizio))
              {
                l <- list(DF, d.f3)
                DF <- rbindlist(l)
              }
              
            }
            else if(paste0(Q[2],"_",y) %in% d_f$period & !(paste0(Q[3],"_",y) %in% d_f$period))
            {
              ind2 <- which(d_f$period == paste0(Q[2],"_",y))
              start2 <- paste0("20",y,"-",Qnm[2], "-01")
              end2 <- paste0("20",y,"-",Qnm[2], "-", lubridate::days_in_month(as.Date(start, origin = "1899-12-30")))
              d.f2 <- data.frame(inizio = start2, fine = end2, BSL = d_f$BSL[ind2], PK = d_f$PK[ind2], stringsAsFactors = FALSE) 
              bQ <- d_f$BSL[which(d_f$period == paste0(GetQ(which(mesi == mm)), "_",y))]
              pQ <- d_f$PK[which(d_f$period == paste0(GetQ(which(mesi == mm)), "_",y))]
              missing_b <- (bQ*sum(ore7$BSL[Qn[1]:Qn[3]]) -  d_f$BSL[i]*ore7$BSL[Qn[1]] - d_f$BSL[ind2]*ore7$BSL[Qn[2]])/ore7$BSL[Qn[3]]
              missing_p <- (pQ*sum(ore7$PK[Qn[1]:Qn[3]]) -  d_f$PK[i]*ore7$PK[Qn[1]] - d_f$PK[ind2]*ore7$PK[Qn[2]])/ore7$PK[Qn[3]]
              start3 <- paste0("20",y,"-",Qnm[3], "-01")
              end3 <- paste0("20",y,"-",Qnm[3], "-", lubridate::days_in_month(as.Date(start, origin = "1899-12-30")))
              d.f3 <- data.frame(inizio = start3, fine = end3, BSL = missing_b, PK = missing_p, stringsAsFactors = FALSE) 
              
              #d.f <- rbind(d.f, d.f3)
              if(!(start2 %in% DF$inizio))
              {
                l <- list(DF, d.f2)
                DF <- rbindlist(l)
              }
              if(!(start3 %in% DF$inizio))
              {
                l <- list(DF, d.f3)
                DF <- rbindlist(l)
              }
              
            }
            else
            {
              bQ <- d_f$BSL[which(d_f$period == paste0(GetQ(which(mesi == mm)), "_",y))]
              pQ <- d_f$PK[which(d_f$period == paste0(GetQ(which(mesi == mm)), "_",y))]
              missing_b <- (bQ*sum(ore7$BSL[Qn[1]:Qn[3]]) -  d_f$BSL[i]*ore7$BSL[Qn[1]])/sum(ore7$BSL[Qn[2]:Qn[3]])
              missing_p <- (pQ*sum(ore7$PK[Qn[1]:Qn[3]]) -  d_f$PK[i]*ore7$PK[Qn[1]])/sum(ore7$PK[Qn[2]:Qn[3]])
              start3 <- paste0("20",y,"-",Qnm[2], "-01")
              s <- paste0("20",y,"-",Qnm[3], "-01")
              end3 <- paste0("20",y,"-",Qnm[3], "-", lubridate::days_in_month(as.Date(s, origin = "1899-12-30")))
              d.f3 <- data.frame(inizio = start3, fine = end3, BSL = missing_b, PK = missing_p, stringsAsFactors = FALSE)
              #d.f <- rbind(d.f, d.f3)
              if(!(start3 %in% DF$inizio))
              {
                l <- list(DF, d.f3)
                DF <- rbindlist(l)
              }
              
            }
            
          }
          else if(m == Qn[2])
          {
            if(paste0(Q[3],"_",y) %in% d_f$period)
            {
              ind3 <- which(d_f$period == paste0(Q[3],"_",y))
              start3 <- paste0("20",y,"-",Qnm[3], "-01")
              end3 <- paste0("20",y,"-",Qnm[3], "-", lubridate::days_in_month(as.Date(start, origin = "1899-12-30")))
              d.f3 <- data.frame(inizio = start3, fine = end3, BSL = d_f$BSL[ind3], PK = d_f$PK[ind3], stringsAsFactors = FALSE) 
              if(!(start3 %in% DF$inizio))
              {
                l <- list(DF, d.f3)
                DF <- rbindlist(l)
              }
              
            }
            else
            {
              bQ <- d_f$BSL[which(d_f$period == paste0(GetQ(m), "_",y))]
              pQ <- d_f$PK[which(d_f$period == paste0(GetQ(m), "_",y))]
              missing_b <- (bQ*sum(ore7$BSL[Qn[1]:Qn[3]]) -  d_f$BSL[i]*ore7$BSL[Qn[2]])/ore7$BSL[Qn[3]]
              missing_p <- (bQ*sum(ore7$PK[Qn[1]:Qn[3]]) -  d_f$PK[i]*ore7$PK[Qn[2]])/ore7$PK[Qn[3]]
              start3 <- paste0("20",y,"-",Qnm[3], "-01")
              end3 <- paste0("20",y,"-",Qnm[3], "-", lubridate::days_in_month(as.Date(start3, origin = "1899-12-30")))
              d.f3 <- data.frame(inizio = start3, fine = end3, BSL = missing_b, PK = missing_p, stringsAsFactors = FALSE)
              if(!(start3 %in% DF$inizio))
              {
                l <- list(DF, d.f3)
                DF <- rbindlist(l)
              }
              
            }
          }
          else
            {
              start3 <- paste0("20",y,"-",Qnm[3], "-01")
              end3 <- paste0("20",y,"-",Qnm[3], "-", lubridate::days_in_month(as.Date(start3, origin = "1899-12-30")))
              d.f3 <- data.frame(inizio = start3, fine = end3, BSL = d_f$BSL[i], PK = d_f$PK[i], stringsAsFactors = FALSE)
              if(!(start3 %in% DF$inizio))
              {
                l <- list(DF, d.f3)
                DF <- rbindlist(l)
              }
              
            }#### end if loop m \in Qn
        }
      else if(m < Qn[1])
      {
        start <- paste0("20",y,"-",Qnm[1], "-01")
        end <- paste0("20",y,"-",Qnm[1], "-", lubridate::days_in_month(as.Date(start, origin = "1899-12-30")))
        d.f <- data.frame(inizio = start, fine = end, BSL = d_f$BSL[i], PK = d_f$PK[i], stringsAsFactors = FALSE)
        if(!(start %in% DF$inizio))
        {
          l <- list(DF, d.f)
          DF <- rbindlist(l)
        }
        
        if(paste0(Q[2],"_",y) %in% d_f$period & paste0(Q[3],"_",y) %in% d_f$period)
        {
          ind2 <- which(d_f$period == paste0(Q[2],"_",y))
          ind3 <- which(d_f$period == paste0(Q[3],"_",y))
          start2 <- paste0("20",y,"-",Qnm[2], "-01")
          end2 <- paste0("20",y,"-",Qnm[2], "-", lubridate::days_in_month(as.Date(start, origin = "1899-12-30")))
          start3 <- paste0("20",y,"-",Qnm[3], "-01")
          end3 <- paste0("20",y,"-",Qnm[3], "-", lubridate::days_in_month(as.Date(start, origin = "1899-12-30")))
          d.f2 <- data.frame(inizio = start2, fine = end2, BSL = d_f$BSL[ind2], PK = d_f$PK[ind2], stringsAsFactors = FALSE) 
          d.f3 <- data.frame(inizio = start3, fine = end3, BSL = d_f$BSL[ind3], PK = d_f$PK[ind3], stringsAsFactors = FALSE) 
          if(!(start2 %in% DF$inizio))
          {
            l <- list(DF, d.f2)
            DF <- rbindlist(l)
          }
          if(!(start3 %in% DF$inizio))
          {
            l <- list(DF, d.f3)
            DF <- rbindlist(l)
          }
          #d.f <- rbind(d.f, d.f2)
          
        }
        else if(paste0(Q[2],"_",y) %in% d_f$period & !(paste0(Q[3],"_",y) %in% d_f$period))
        {
          ind2 <- which(d_f$period == paste0(Q[2],"_",y))
          start2 <- paste0("20",y,"-",Qnm[2], "-01")
          end2 <- paste0("20",y,"-",Qnm[2], "-", lubridate::days_in_month(as.Date(start, origin = "1899-12-30")))
          d.f2 <- data.frame(inizio = start2, fine = end2, BSL = d_f$BSL[ind2], PK = d_f$PK[ind2], stringsAsFactors = FALSE) 
          bQ <- d_f$BSL[which(d_f$period == paste0(GetQ(Qn[1]), "_",y))]
          pQ <- d_f$PK[which(d_f$period == paste0(GetQ(Qn[1]), "_",y))]
          missing_b <- (bQ*sum(ore7$BSL[Qn[1]:Qn[3]]) -  d_f$BSL[i]*ore7$BSL[Qn[1]] - d_f$BSL[ind2]*ore7$BSL[Qn[2]])/ore7$BSL[Qn[3]]
          missing_p <- (pQ*sum(ore7$PK[Qn[1]:Qn[3]]) -  d_f$PK[i]*ore7$PK[Qn[1]] - d_f$PK[ind2]*ore7$PK[Qn[2]])/ore7$PK[Qn[3]]
          start3 <- paste0("20",y,"-",Qnm[3], "-01")
          end3 <- paste0("20",y,"-",Qnm[3], "-", lubridate::days_in_month(as.Date(start, origin = "1899-12-30")))
          d.f3 <- data.frame(inizio = start3, fine = end3, BSL = missing_b, PK = missing_p, stringsAsFactors = FALSE) 
          #d.f <- rbind(d.f, d.f2)
          if(!(start2 %in% DF$inizio))
          {
            l <- list(DF, d.f2)
            DF <- rbindlist(l)
          }
          if(!(start3 %in% DF$inizio))
          {
            l <- list(DF, d.f3)
            DF <- rbindlist(l)
          }
        }
        else
        {
          bQ <- d_f$BSL[which(d_f$period == paste0(GetQ(Qn[1]), "_",y))]
          pQ <- d_f$PK[which(d_f$period == paste0(GetQ(Qn[1]), "_",y))]
          missing_b <- (bQ*sum(ore7$BSL[Qn[1]:Qn[3]]) -  d_f$BSL[i]*ore7$BSL[Qn[1]])/sum(ore7$BSL[Qn[2]:Qn[3]])
          missing_p <- (pQ*sum(ore7$PK[Qn[1]:Qn[3]]) -  d_f$PK[i]*ore7$PK[Qn[1]])/sum(ore7$PK[Qn[2]:Qn[3]])
          start3 <- paste0("20",y,"-",Qnm[2], "-01")
          s <- paste0("20",y,"-",Qnm[3], "-01")
          end3 <- paste0("20",y,"-",Qnm[3], "-", lubridate::days_in_month(as.Date(s, origin = "1899-12-30")))
          d.f3 <- data.frame(inizio = start3, fine = end3, BSL = missing_b, PK = missing_p, stringsAsFactors = FALSE)
          #d.f <- rbind(d.f, d.f3)
          if(!(start3 %in% DF$inizio))
          {
            l <- list(DF, d.f3)
            DF <- rbindlist(l)
          }
        }
      }
    }
    else if(strsplit(d_f$period[i], "_")[[1]][1] %in% c("Q1","Q2","Q3","Q4"))
    {
      q <- strsplit(d_f$period[i], "_")[[1]][1]
      y <- strsplit(d_f$period[i], "_")[[1]][2]
      
      if(y == "17" & usageQ7[which(c("Q1","Q2","Q3","Q4") == q)] > 0)
      {
        next
      }
      else if(y == "18" & usageQ8[which(c("Q1","Q2","Q3","Q4") == q)] > 0)
      {
        next
      }
      else
      {
        Q <- get(q)
        Qnm <- get(paste0(q,"nm"))
        start <- paste0("20",y,"-",Qnm[1], "-01")
        s <- paste0("20",y,"-",Qnm[3], "-01")
        end <- paste0("20",y,"-",Qnm[3],'-', lubridate::days_in_month(as.Date(s, origin = "1899-12-30")))
        d.f <- data.frame(inizio = start, fine = end, BSL = d_f$BSL[i], PK = d_f$PK[i], stringsAsFactors = FALSE)
        if(!(start %in% DF$inizio))
        {
          l <- list(DF, d.f)
          DF <- rbindlist(l)
        }
        
      }
      
    }
    else if(strsplit(d_f$period[i], "_")[[1]][1] == 'Y')
    {
      if(sum(usageQ8) > 0)
      {
        p <- max(which(usageQ8 == 1))
        Qp <- paste0("Q",1:p)
        Q <- get(paste0("Q", min(which(usageQ8 == 0))))
        Qn <- get(paste0("Q", min(which(usageQ8 == 0)),"n"))
        Qnm <- get(paste0("Q", min(which(usageQ8 == 0)),"nm"))
        start <- paste0("2018-",Qnm[1], "-01")
        end <- '2018-12-31'
        missing_b <- (d_f$BSL[i]*sum(ore7$BSL) - sum(d_f$BSL[which(d_f$period %in% Qp)])*ore7$BSL[1:(Qn[1]-1)])/ore7$BSL[Qn[1]:12]
        missing_p <- (d_f$PK[i]*sum(ore7$PK) - sum(d_f$PK[which(d_f$period %in% Qp)])*ore7$PK[1:(Qn[1]-1)])/ore7$PK[Qn[1]:12]
        d.f <- data.frame(inizio = start, fine = end, BSL = missing_b, PK = missing_p, stringsAsFactors = FALSE)
        if(!(start %in% DF$inizio))
        {
          l <- list(DF, d.f)
          DF <- rbindlist(l)
        }
      }
      else
      {
        d.f <- data.frame(inizio = '2018-01-01', '2018-12-31' = end, BSL = d_f$BSL[i], PK = d_f$PK[i], stringsAsFactors = FALSE)
        l <- list(DF, d.f)
        DF <- rbindlist(l)
      }
    }
  }
  return(DF)
}
###################################################################

#list_orep <- data.table(read_excel('C:/Users/utente/Documents/shinyapp/longterm_pun.xlsx'))
list_orep <- data.table(read_excel('C:/Users/utente/Documents/shinyapp/longterm_pun.xlsx'))
real <- read_excel("C:/Users/utente/Documents/shinyapp/DB_Borse_Elettriche_PER MI_17_conMacro - Copy.xlsm", sheet = 2)
df2 <- list_orep

df2 <- Assembler2(real, df2)
df2 <- df2[,-10]
colnames(df2)[10] <- "real"

df8 <- data.table(read_excel('C:/Users/utente/Documents/shinyapp/pun_forward_2018.xlsx'))



mercato <- data.table(read_excel('C:/Users/utente/Documents/shinyapp/prova.xlsx'))

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  
  if(input$select == 1)
  {
    observeEvent(input$Action,{
      start <- proc.time()
      output$oldpun7 <- renderText({print(paste("BASELOAD 2017 attuale =",round(m_old,2)))})
      output$oldpun8 <- renderText({print(paste("BASELOAD 2018 attuale =",round(m_old8,2)))})
      withProgress(message = "Sto elaborando...", {
        m_old <- mean(df2$pun)
        m_old8 <- mean(df8$pun)
        for(i in 1:nrow(mercato))
        {
          #print(i)
          ft <- AnalyzePeriod(unlist(mercato[i,"Periodo"]))
          from <- ft$from
          to <- ft$to
          #print(EstraiAnno(from))
          
          if(EstraiAnno(from) == 2017 & EstraiAnno(to) == 2017)
          {
            df2 <- Redimensioner_pkop(df2, unlist(mercato[i,"BSL"]), unlist(mercato[i,"PK"]), from, to, "PK")
          }
          else if(EstraiAnno(from) == 2018 & EstraiAnno(to) == 2018)
          {
            df8 <- Redimensioner_pkop(df8, unlist(mercato[i,"BSL"]), unlist(mercato[i,"PK"]), from, to, "PK")
          }
          else
          {
            break
          }
        }
        
        output$time <- renderText({paste("Tempo: ",proc.time()[1] - start[1])})  
        
        output$newpun7 <- renderText({print(paste("BASELOAD 2017 aggiornato =",round(mean(df2$pun),2)))})
        output$plot17 <- renderPlot({
          plot(df2$pun, 
               col = 'blue',
               type = "l",
               ylab = "pun 2017",
               main = 'PUN forward 2017')
        })
        
        output$newpun8 <- renderText({print(paste("BASELOAD 2018 aggiornato =",round(mean(df8$pun),2)))})
        output$plot18 <- renderPlot({
          plot(df8$pun, 
               col = "red",
               type = "l",
               ylab = "pun 2018",
               main = 'PUN forward 2018')
        })
        
      })  
      
      hide(id = "old_stats", anim = TRUE, animType = "fade")  
      path7 <- "C:/Users/utente/Documents/prova/longterm_pun.xlsx"   
      path8 <- "C:/Users/utente/Documents/prova/pun_forward_2018.xlsx"  
      
      write.xlsx(df2, path7, row.names = FALSE)
      write.xlsx(df8, path8, row.names = FALSE)
      
      output$mess7 <- renderText({print(paste("PUN forward 2017 salvato in", path7))})
      output$mess8 <- renderText({print(paste("PUN forward 2018 salvato in", path8))})
      
      output$quotingText <- renderText({print("Fatto quoting COMPLETO")})
    })
    
  }
  else
  {
    observeEvent(input$Action,{
      start <- proc.time()
      output$oldpun7 <- renderText({print(paste("BASELOAD 2017 attuale =",round(m_old,2)))})
      output$oldpun8 <- renderText({print(paste("BASELOAD 2018 attuale =",round(m_old8,2)))})
      withProgress(message = "Sto elaborando...", {
        m_old <- mean(df2$pun)
        m_old8 <- mean(df8$pun)
        for(i in 1:nrow(mercato))
        {
          #print(i)
          ft <- ####AnalyzePeriod(unlist(mercato[i,"Periodo"]))
          from <- ft$from
          to <- ft$to
          #print(EstraiAnno(from))
          
          if(EstraiAnno(from) == 2017 & EstraiAnno(to) == 2017)
          {
            df2 <- Redimensioner_pkop(df2, unlist(mercato[i,"BSL"]), unlist(mercato[i,"PK"]), from, to, "PK")
          }
          else if(EstraiAnno(from) == 2018 & EstraiAnno(to) == 2018)
          {
            df8 <- Redimensioner_pkop(df8, unlist(mercato[i,"BSL"]), unlist(mercato[i,"PK"]), from, to, "PK")
          }
          else
          {
            break
          }
        }
        
        output$time <- renderText({paste("Tempo: ",proc.time()[1] - start[1])})  
        
        output$newpun7 <- renderText({print(paste("BASELOAD 2017 aggiornato =",round(mean(df2$pun),2)))})
        output$plot17 <- renderPlot({
          plot(df2$pun, 
               col = 'blue',
               type = "l",
               ylab = "pun 2017",
               main = 'PUN forward 2017')
        })
        
        output$newpun8 <- renderText({print(paste("BASELOAD 2018 aggiornato =",round(mean(df8$pun),2)))})
        output$plot18 <- renderPlot({
          plot(df8$pun, 
               col = "red",
               type = "l",
               ylab = "pun 2018",
               main = 'PUN forward 2018')
        })
        
      })  
      
      hide(id = "old_stats", anim = TRUE, animType = "fade")  
      path7 <- "C:/Users/utente/Documents/prova/longterm_pun.xlsx"   
      path8 <- "C:/Users/utente/Documents/prova/pun_forward_2018.xlsx"  
      
      write.xlsx(df2, path7, row.names = FALSE)
      write.xlsx(df8, path8, row.names = FALSE)
      
      output$mess7 <- renderText({print(paste("PUN forward 2017 salvato in", path7))})
      output$mess8 <- renderText({print(paste("PUN forward 2018 salvato in", path8))})
      
      output$quotingText <- renderText({print("Fatto quoting di TECLA")})
    })
    
  }
  
})