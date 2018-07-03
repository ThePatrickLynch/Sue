# Transcript analysis

# functions from qdap package featuring

# might want to look at RQDA package at somepoint
#  
# look at dplyr, 

rm(list=ls())
path <- "d:/Dropbox/Dropbox/Sue/raw"

#install.packages("tm", dependencies = T)
#install.packages("qdap", dependencies = T)

library(tm)
library(qdap)




############################################
# some setup
############################################
# abbreviations
# my additions based on Sue's coding
my_abbs <- read.csv("d:/Dropbox/Dropbox/Sue/Source/myabbreviations.csv")
Key <- rbind(abbreviations, my_abbs)

# words to keep together
keeps <- c("Virtual Learning Environment", "teaching and learning")


############################################
# Read files
############################################
#dat1 <- read.transcript(file.path(path,"MTPD-DM.docx"), col.names = qcv(person, dialogue))  #qcv - quick character vector from qdap


dir_map(path)


#check_spelling_interactive(dat1$dialogue)





############################################
# tidy up text
############################################

dat1$dialogue <- replace_abbreviation(dat1$dialogue, Key)
dat1$dialogue <- scrubber(dat1$dialogue, num2word = T, fix.comma = T, fix.space = T)
dat1$dialogue <- replace_number(dat1$dialogue) # 1 = one
dat1$dialogue <- replace_contraction(dat1$dialogue) # it's = itis ...
dat1$dialogue <- space_fill(dat1$dialogue, keeps)  # words to keep together spaces become ~~
dat1$dialogue <- incomplete_replace(dat1$dialogue) 
dat1$dialogue <- bracketX(dat1$dialogue, "square")

#dat1$dialogue <- strip(dat1$dialogue, lower=F)

# split by person
interviewer <- split(dat1, dat1$person)[['ME']]
interviewee <- split(dat1, dat1$person!='ME')[['TRUE']]

interviewee$dialogue <- rm_stopwords(interviewee$dialogue, Top200Words)
interviewee_words <- bag_o_words(interviewee$dialogue, lower=F)



# explore
#ngrams()
#wfm()
#all_words()





# look at adjacent words

# not ready yet

doc.corpus <- tm_map(doc.corpus, content_transformer(removePunctuation)) # remove punctuation
doc.corpus <- tm_map(doc.corpus, content_transformer(removeNumbers))
tm_words <- tm_map(mat_interviewee, content_transformer(tolower))

amatrix <- adjacency_matrix(client_words$WORD)

#sort
client_words <- client_words[order(-client_words$FREQ),]   


#remove common words
dat2 <- rm_stopwords(client_words$WORD, tm::stopwords("english"))  #remove common words

                   
                      
                      
                      
bing_lex <- get_sentiments("bing")
nrc_lex <- get_sentiments("nrc")
afinn_lex <- get_sentiments("afinn")



td_new <- data.frame(colSums(preppedsentiment))
td_new <- cbind("sentiment" = rownames(td_new), td_new)
numobs <- paste("(n = ",length(prepped),")", sep="")

op <- par(no.readonly=T)
par(mar=c(5.1,6,4.1,2.1))
c_range <- c('firebrick', 'plum1', 'yellow4', 'yellow', 'orchid', 'orange', ' hotpink', 'blue','green','red')

barplot(td_new[,2], 
  horiz=T, 
  main="How well did you feel prepared for using ePAD?", 
  xlab="Sentiment identified using nrc lexicon", 
  sub = numobs,
  col = c_range, 
  #legend.text = td_new[,1],
  names.arg=td_new[,1], las=1
)

#===============================================


par(op)
