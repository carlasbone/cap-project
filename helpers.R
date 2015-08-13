#trim white space 
trimstring <- function (x) gsub("^\\s+|\\s+$", "", x)

#parse input string and get it ready for search
cleanword <- function(theword) {
    theword <- trimstring(theword)
    for (i in 1: length(theword)) {
        theword[i] <- tolower(theword[i]) 
        theword[i] <- gsub("[^a-z']","",theword[i]) 
        result<- contractions$long[contractions$short==theword[i]]
        if (length(result) != 0) {
            temp <- gsub("'","",theword[i]) 
            if (grepl(";",result) != 0) {
                contr <<- TRUE
                result <- paste("(",temp,"|",theword[i],"|",gsub("; ","|",result),")",sep="") 
            } else {
                result <- paste("(",temp,"|",theword[i],")",sep="")
           }
        } else {
            result <- theword[i]
        }
        theword[i] <- result  
    }
    theword 
} 
# sort thru string to remove from the prediction table any words that are in the search 
#string due to contractions
decontr <- function(x) {
    x <- gsub("[^a-z]", " ", x)
    x <- trimstring(x)
    x <- unlist(str_split(x," "))
    x
} 

#if contraction, then there is an extra word, so take just the last word again to search on
fixstring <- function(x) { 
    x <- gsub("\\|", ",", x)
    x <- gsub("[^a-z, ]", "", x)
    x <- trimstring(x)
    x <- unlist(str_split(x,","))
    for (i in 1:length(x)) {
        x[i] <- gsub("[a-z ]+ ([a-z]+$)", "\\1", x[i])
    }
    x <- paste("^(",paste0(x,collapse="|"),")",sep="")
    x
}   


#main function
#check through all data tables in sequence for matches to adjusted search strings.
#returns a predict table from which the 5 top results are taken.
getnextwords <- function(data) {
    wpredict <- data.table()
    
    strend <- strlen <- (length(data))
    contr <- FALSE
    data <- cleanword(data)
    data <- data[str_length(data) >= 1]  #get rid of words of empty spaces
    if ((contr) | (strlen>2)) { 
        tempstr<- str_trim(paste(paste0("(",wordStem(data[strlen-2]),"|",data[strlen-2],")"),data[strlen-1],data[strlen],sep=" ")) 
        usergramstr <- paste("^",tempstr,sep="")
        strresultsq <- quadgramdt[grep(usergramstr,quadgramdt$quadgram),]
        if (nrow(strresultsq)>0) {
            strresultsq <- head(strresultsq[order(-strresultsq$relfreq),],30)
            strresultsq[,':='(agram=word(strresultsq$quadgram, -1))]
            strresultsq <- data.table(aggregate(relfreq ~ agram, data = strresultsq,sum))
            strresultsq$relfreq <- strresultsq$relfreq  + 2
            wpredict <- strresultsq 
            setkey(wpredict,agram)
        } 
    } 
    
    # then trigrams
    if ((contr) | (strlen>1)) { 
         tempstr<- str_trim(paste(paste0("(",wordStem(data[strlen-1]),"|",data[strlen-1],")"),data[strlen],sep=" ")) 
         usergramstr <- paste("^",tempstr,sep="")
        strresultst <- dftrigramwt[grep(usergramstr,dftrigramwt$trigram),]
        if (nrow(strresultst)>0){
            strresultst <- head(strresultst[order(-strresultst$relfreq),],30)
            strresultst[,':='(agram=word(strresultst$trigram, -1))]
            strresultst <- data.table(aggregate(relfreq ~ agram, data = strresultst,sum))
            strresultst$relfreq <- strresultst$relfreq + 1
            
            wpredtri <- strresultst
            
            setkey(wpredtri,agram)
            if(nrow(wpredict)>0)  {   #then some trigrams must have been found so merge tables 
                mm <- merge(wpredict, wpredtri, by.x="wpredict$agram", by.y="wpredtri$agram",all=T)
                mm[is.na(mm)]<-0 
                mm[, ':=' (totfreq=relfreq.x + relfreq.y)]
                wpredict <- data.table(relfreq=mm$totfreq,agram=mm$agram)
                wpredict <- data.table(aggregate(relfreq ~ agram, data = wpredict,sum))
                
            } else {
                wpredict <- strresultst 
            }
            setkey(wpredict,agram)
        }
    }
    
    # then bigrams 
    usergramstr <- paste("^",paste0("(",wordStem(data[strlen]),"|",data[strlen],")"),sep="")

    if (contr) {
        # want to get just the last words in each case 
        usergramstr <- fixstring(usergramstr)
    }
      #search bigram table
    strresultsb <- dfbigramwt[grep(usergramstr,dfbigramwt$bigram),]
    if (nrow(strresultsb)>0){
        strresultsb <- head(strresultsb[order(-strresultsb$relfreq),],30)
        strresultsb[,':='(agram=word(strresultsb$bigram, -1))]
        strresultsb <- data.table(aggregate(relfreq ~ agram, data = strresultsb,sum))
        strresultsb$relfreq <- strresultsb$relfreq + .5
        wpredbi <- strresultsb
        
        setkey(wpredbi,agram)
        if(nrow(wpredict)>0)  {   #then some bigrams must have been found so merge tables 
            mm <- merge(wpredict, wpredbi, by.x="wpredict$agram", by.y="wpredbi$agram",all=T)
            mm[is.na(mm)]<-0 
            mm[, ':=' (totfreq=relfreq.x + relfreq.y)]
            wpredict <- data.table(relfreq=mm$totfreq,agram=mm$agram)
            wpredict <- data.table(aggregate(relfreq ~ agram, data = wpredict,sum))
            
            
        } else {
            wpredict <- strresultsb 
        }
        setkey(wpredict,agram)
    }
    
    # else search trigrams for w-1 word.  Only runs if no other predictions found as last resort
    if(nrow(wpredict)< 1) {
        usergramstr <- paste("^",paste0("(",wordStem(data[strlen-1]),"|",data[strlen-1],")"),sep="")
        if (contr) {
            # want to get just the last words in each case 
            usergramstr <- fixstring(usergramstr)
        }
        
        strresultst <- dftrigramwt[grep(usergramstr,dftrigramwt$trigram),]
        if (nrow(strresultst)>0){
            strresultst <- head(strresultst[order(-strresultst$relfreq),],30)
            strresultst[,':='(agram=word(strresultst$trigram, -1))]
            strresultst <- data.table(aggregate(relfreq ~ agram, data = strresultst,sum))
           # strresultst$relfreq <- strresultst$relfreq +.5
            wpredtri <- strresultst
            
            setkey(wpredtri,agram)
            if(nrow(wpredict)>0)  {   #then some trigrams must have been found so merge tables 
                mm <- merge(wpredict, wpredtri, by.x="wpredict$agram", by.y="wpredtri$agram",all=T)
                mm[is.na(mm)]<-0 
                mm[, ':=' (totfreq=relfreq.x + relfreq.y)]
                wpredict <- data.table(relfreq=mm$totfreq,agram=mm$agram)
                wpredict <- data.table(aggregate(relfreq ~ agram, data = wpredict,sum))
                
                
            } else {
                wpredict <- strresultst 
            }
            setkey(wpredict,agram)
        }
    }
    
    
    
    if(nrow(wpredict)>0) {
        setkey(wpredict,agram)
        
        #remove any words which are also in the usergramstr to fix problem with contractions
        wpredict <-subset(wpredict, !(wpredict$agram %in% decontr(usergramstr))) 
        wpredict <-subset(wpredict, !(wpredict$agram %in% unlist(badwords))) 
        setorder(wpredict,-relfreq)
        wordpredict <- head(wpredict$agram,5)
    } else {
        wordpredict <- "Well this is unusual, Please check spelling, or enter the next word and I'll try again." 
    }    
    wordpredict 
}
