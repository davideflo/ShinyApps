######## server.R file for shiny app

library(shiny)
library(plyr)
library(dplyr)
library(readxl)
library(feather)
library(lubridate)
library(data.table)
library(h2o)
library(xlsx)
#################################################################################
#################################################################################
Assembler2 <- function(real, ph)
{
  rows <- which(unlist(!is.na(real[,13])))
  re <- rep(0, nrow(real))
  
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
  
  if(what == "PK")  
  {
    opm <- (1/nOP)*((mh*M) - (mw*nPK))
    
    pbpk <- ifelse(length(periodpk$pun[periodpk$real == 1]) > 0, (1/M)*sum(periodpk$pun[periodpk$real == 1]), 0)
    pbop <- ifelse(length(periodop$pun[periodop$real == 1]) > 0, (1/M)*sum(periodop$pun[periodop$real == 1]), 0)
    pihatpk <- (mw - pbpk)/mean(periodpk$pun[periodpk$real == 0])
    pihatop <- (opm - pbop)/mean(periodop$pun[periodop$real == 0])
    for(i in 1:length(rPK))
    {
      ph[rPK[i], "pun"] <- pihatpk * unlist(ph[rPK[i], "pun"])
    }
    for(i in 1:length(rOP))
    {
      ph[rOP[i], "pun"] <- pihatop * unlist(ph[rOP[i], "pun"])
    }
  }
  else
  {
    pkm <- (1/nPK)*((mh*M) - (mw*nOP))
    
    pbpk <- ifelse(length(periodpk$pun[periodpk$real == 1]) > 0, (1/M)*sum(periodpk$pun[periodpk$real == 1]), 0)
    pbop <- ifelse(length(periodop$pun[periodop$real == 1]) > 0, (1/M)*sum(periodop$pun[periodop$real == 1]), 0)
    pihatpk <- (pkm - pbpk)/mean(periodpk$pun[periodpk$real == 0])
    pihatop <- (mw - pbop)/mean(periodop$pun[periodop$real == 0])
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
AnalyzePeriod <- function(s, YEAR)
{
  mesi <- c("Gennaio", "Febbraio", "Marzo", "Aprile", "Maggio", "Giugno", "Luglio", "Agosto", "Settembre", "Ottobre", "Novembre", "Dicembre")
  splitted <- strsplit(s, "-")
  
  if(length(splitted[[1]]) > 1)
  {
    #### siamo nelle settimane
    split1 <- strsplit(splitted$Periodo[1], "/")
    split2 <- strsplit(splitted$Periodo[2], "/")
    from <- paste0(as.numeric(split1[[1]][1]),'-',as.numeric(split1[[1]][2]),'-',as.numeric(split1[[1]][3]))
    to <- paste0(as.numeric(split2[[1]][1]),'-',as.numeric(split2[[1]][2]),'-',as.numeric(split2[[1]][3]))
  }
  else if(length(splitted[[1]]) == 1)
  {
    if(tolower(s) %in% tolower(mesi))
    {
      mese <- ifelse(which(tolower(s) == tolower(mesi)) < 10, paste0('0',which(tolower(s) == tolower(mesi))), which(tolower(s) == tolower(mesi)))
      from <- paste0(YEAR, '-', mese, '-01')
      to <- paste0(YEAR, '-', mese, '-', days_in_month(as.Date(from)))
    }
    else if(s == 'Q1_17')
    {
      from <- '2017-01-01'
      to <- '2017-03-31'
    }
    else if(s == 'Q2_17')
    {
      from <- '2017-04-01'
      to <- '2017-06-30'
    }
    else if(s == 'Q3_17')
    {
      from <- '2017-07-01'
      to <- '2017-09-30'
    }
    else if(s == 'Q4_17')
    {
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
      from <- '2018-04-01'
      to <- '2018-06-30'
    }
    else if(s == 'Q3_18')
    {
      from <- '2018-07-01'
      to <- '2018-09-30'
    }
    else if(s == 'Q4_18')
    {
      from <- '2018-10-01'
      to <- '2018-12-31'
    }
    else
    {### BSL annuale
      from <- paste0(YEAR, '-01-01')
      to <- paste0(YEAR, '-12-31')
    }
  }
  return(list(from = from, to = to))
}
###################################################################
EstraiAnno <- function(ft)
{
  splitted <- strsplit(s, "-")
  return(as.numeric(splitted[[1]][1]))
}
###################################################################
###################################################################

list_orep <- data.table(read_excel('longterm_pun.xlsx'))
real <- read_excel("DB_Borse_Elettriche_PER MI_17_conMacro - Copy.xlsm", sheet = 2)
df2 <- list_orep

df2 <- Assembler2(real, df2)
df2 <- df2[,-10]
colnames(df2)[10] <- "real"

df8 <- data.table(read_excel('pun_forward_2018.xlsx'))



mercato <- data.table(read_excel('prova.xlsx'))

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  
  if(input$Anno == '2017')
  {
    for(i in 1:nrow(mercato))
    {
      ft <- AnalyzePeriod(unlist(mercato[i,1]), input$Anno)
      from <- ft$from
      to <- ft$to
      if(EstraiAnno(from) == 2017 & EstraiAnno(to) == 2017)
      {
        df2 <- Redimensioner_pkop(df2, unlist(mercato[i,"BSL"]), unlist(mercato[i,"PK"]), from, to, "PK")
      }
      else
      {
        next
      }
    }
    output$plot17 <- renderPlot({
      plot(df2$pun, 
           data = df2,
           type = "l",
           main = 'PUN forward 2017')
    })
  }
  
  else
  {
    for(i in 1:nrow(mercato))
    {
      ft <- AnalyzePeriod(unlist(mercato[i,1]), input$Anno)
      from <- ft$from
      to <- ft$to
      if(EstraiAnno(from) == 2018 & EstraiAnno(to) == 2018)
      {
        df8 <- Redimensioner_pkop(df8, unlist(mercato[i,"BSL"]), unlist(mercato[i,"PK"]), from, to, "PK")
      }
    }
    output$plot18 <- renderPlot({
      plot(df8$pun, 
           data = df8,
           type = "l",
           main = 'PUN forward 2017')
    })
  }
  
  
})