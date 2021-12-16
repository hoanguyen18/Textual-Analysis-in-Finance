
# --- Task----
# How much partisanship is there among financial regulators? Has it increased over time?
# Your approach is to design a measure of political partisanship using speech of U.S. congress man/women and apply it to speech by the Board of Governors of the U.S. FED.
# Your analysis has two parts:
  # (1) Construction of a textual partisanship score
    # • Only use tools that you understand well enough so that you are comfortable with receiving
    # questions about them in the oral exam.
    # • Make sure that you convince the reader that your textual partisanship score actually measures
    # what you claim it measures.
  # (2) Analysis of the partisanship by financial regulators


# Packages & Working Directory ----
require(tidyverse)
require(pdftools)
require(RCurl)
require(rvest) #scaping
require(htmltools)
require(tidytext)
require(quanteda) #dfm
require(udpipe) #not using it  POS tagging 
require(tm) 
require(reshape2) 
require(readr) #read_delim
#reset memory
#rm(list = ls())
load("Rdata")
#set to english for encoding dates from wikipedia
Sys.setlocale(locale = "en_US.UTF-8") #for the date format


# 0 Data/Functions used throughout the code ----

# 0.1 Congress Procedural Phrase dictionary from https://data.stanford.edu/congress_text
# We use existing procedural phrase dictionary from Stanford (same source where corpus data comes from), contains over 50k entries.

# Import Vocabulary (has to be downloaded manually from the above link)
procedural_dict = read_delim(paste0("final data/vocabulary/", "procedural",".txt"),
                        col_names = T,
                        delim = "|",
                        show_col_types = F)$phrase
class(procedural_dict)
# Remove digits
procedural_dict <- gsub("\\d","",procedural_dict)

# Remove blank space at the beginning
procedural_dict <- gsub("^\\s","",procedural_dict)

# 0.2 function:  group the remaining into bigrams
bigram_tokenizer <- function(x) {
  unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
}  

#0.3 manual selected congress stopwords/procedual (see report for wherefrom)
procedural = c("absent", "committee", "gentlelady", "hereabout", "hereinafter",
               "hereto", "herewith", "nay", "pro", "sir", "thereabout", "therebeforn",
               "therein", "theretofore", "therewithal", "whereat", "whereinto",
               "whereupon", "yea", "adjourn", "con", "gentleman",
               "hereafter", "hereinbefore", "heretofore", "month", "none", "republican",
               "speak", "thereafter", "thereby", "thereinafter", "thereunder", "today",
               "whereby", "whereof", "wherever", "yes", "ask", "democrat", "gentlemen",
               "hereat", "hereinto", "hereunder", "mr", "now", "say", "speaker", "thereagainst",
               "therefor", "thereof", "thereunto", "whereabouts", "wherefore", "whereon",
               "wherewith", "yield","can", "etc", "gentlewoman", "hereby", "hereof", "hereunto",
               "mrs","part","senator","tell","thereat","therefore","thereon","thereupon","whereafter",
               "wherefrom","whereto","wherewithal","chairman","gentleladies","gentlewomen","herein",
               "hereon","hereupon","nai","per","shall","thank","therebefore","therefrom","thereto",
               "therewith","whereas","wherein","whereunder", "will")

# 0.3 timeframe vector
years <- c(1976:2015)

############## PART 1 ##############
#A. CONGRESSION SPEECHES

x<- c(107:114) 
# Loop through all congresses (from 107 to 114th), loop is needed to reduce size of corpus and dtm for performance 
for(i in x) {

#Step 1: read files
congression1 <- read_delim(paste0("final data/congressional speech/", 107, "_SpeakerMap", ".txt"),
                           col_names = T,
                           delim = "|")

congression2 <- read_delim(paste0("final data/congressional speech/", "speeches_", 107, ".txt"),
                           col_names = T,
                           delim = "|")
#change to numeric
congression1$speech_id<-as.numeric(congression1$speech_id)
congression2$speech_id<-as.numeric(congression2$speech_id)

#merge speakermap to speeches
cong<-merge(congression1,congression2)


#Step 2: Collapse Data 
congress <- ?as.data.frame(cong)
#Already collapse based on speakerId, not speech ID to reduce amount of single documents for creating DTM later
congress %>% group_by(speakerid,party) %>% summarise(speech=paste(speech, collapse=",")) -> congress

#Step 3: Pre-process and prepare Corpus
colnames(congress)[1] <- "doc_id"
colnames(congress)[3] <- "text"
congress <- congress[,c(1, 3 ,2)] #reorder the column to convert to corpus

congress <- congress %>% filter(party %in% c("R","D")) #reduce to D and R only (no Independents)


#Codes in case errors when running tolower and remove stopwords
Encoding(congress$text) <- "UTF-8"
congress$text <- iconv(congress$text, "UTF-8", "UTF-8",sub='') #change text to default UTF-8 
# Step4: Create and clean corpus
con1 <- corpus(congress)

#create tokens and clean corpus
toks <- tokens(con1, #part of quanteda package
               remove_punct = T, #remove punctuation
               remove_symbols = T, #remove symbols
               remove_numbers = T, #remove numbers
               min_nchar = 3, #set min and max word length
               max_nchar = 30)
toks <- tokens_remove(toks, stopwords("english")) #remove stopwords from quanteda stopword dict EN
toks <- tokens_remove(toks, procedural) #remove manually selected stopwords/procedural phrases (see beginning)
toks <- tokens_wordstem(toks) #stem words
bigram <- tokens_remove(toks, procedural_dict, verbose = T) #remove procedural words from Stanford dictionary
bigram <- tokens_ngrams(bigram, n = 2) #create bigrams

#Step 5: Create DFM / DTM
#DFM from quanteda package was chosen over DTM from TM package due to performance and ability to trim DFM based on word freq count.
dtm <- dfm(bigram, #document frequency matrix
              tolower = T, # better safe than sorry, do it again.
              verbose = T)
dtm <- dfm_trim(dtm, min_termfreq = 30, verbose = TRUE) # trim DFM to min term freq. of 30
dtm <- dfm_trim(dtm, min_docfreq = 2, verbose = TRUE) # trim DFM to min doc freq. of 2, e.g. at least 2 different Speaker said it.

assign(paste("dtm_", i, sep = ""), dtm) # safe dfm for each congress

#clean workspace environment to free up space
rm(list=c("con1","cong","dtm","congress","congression1","congression2"))
}

#merge dtms back into one (rbind for DFMs)
dtm_all <- ?rbind(dtm_107,dtm_108,dtm_109,dtm_110,dtm_111,dtm_112,dtm_113,dtm_114)

#clean workspace environment to free up space
rm(list=c("dtm_107","dtm_108","dtm_109","dtm_110","dtm_111","dtm_112","dtm_113","dtm_114"))

#Step 6: join back speakermaps to dfms
# loop over all speakermaps
for (i in x) {
speaker_map <- read_delim(paste0("final data/congressional speech/", i, "_SpeakerMap", ".txt"),
                          col_names = T,
                          delim = "|")
assign(paste("speaker_map", i, sep = ""), speaker_map) 
}
# combine speaker maps into one for mapping party to speakerId
speaker_map <- rbind(speaker_map107,speaker_map108,speaker_map109,speaker_map110,speaker_map111,speaker_map112,speaker_map113,speaker_map114)
# freeup workspace
rm(list=c("speaker_map107","speaker_map108","speaker_map109","speaker_map110","speaker_map111","speaker_map112","speaker_map113","speaker_map114"))
speaker_map %>% 
  select(speakerid,party) %>% #only keep SpeakerId and party
  unique() -> speaker_map #only keep uniques

#Step 7: Convert DTM and join Party affiliation
con_df <- as.data.frame(as.matrix(dtm_all)) #convert to df
doc_id <- rownames(con_df) #assign the row names into a vector 
con_df<- cbind(con_df, doc_id) #merge the row name vector to the df to make it a new variable in the df
con_df <- con_df %>% melt() %>% filter(value != 0) #create a new transposed df for doing "join" later
con_df$doc_id <- as.numeric(con_df$doc_id) #change the format for doing "join" later
speaker_map$doc_id <- speaker_map$speakerid #change the name of the column to make it consistent for doing "join" later
congress_final_df <- left_join(speaker_map, con_df, by = "doc_id" ) #merge party to dataframe by speaker id

#Step 8: Create dataframe with word freq per Party

tempD <- filter(congress_final_df, party == "D") #filter results for Dem.
tempR <- filter(congress_final_df, party == "R") #filter results for Rep.

#Group by variable (phrase, bigram) and sum values together for Dem
congress_filter_speaker_D <- tempD %>% group_by (variable) %>%
  summarise(D =n())

#Group by variable (phrase, bigram) and sum values together for Rep
congress_filter_speaker_R <- tempR %>% group_by (variable) %>%
  summarise(R =n())

#merge those back together, using the bigram (variable) as Identifier
merged <- full_join(congress_filter_speaker_D,congress_filter_speaker_R, by = "variable") # co nhung variable co ca D va R , nhung co nhung phrase chi xuat hien trong R hoac xuat hien trong D
merged[is.na(merged)] <- 0 #replaces NA with 0 occurences
merged$freq_total <- merged$D+merged$R #create freq total column 
merged %>%
  filter(freq_total > 30) ->merged #filter again (better safe) containing only words freq total over >30

#Step 9: Create final dataframe including probability for each bigram if it was said by a democrat or by a republican (1-percentageD)
final_df <- merged %>%
  mutate(percentageD = D/(R+D), percentageOverall = (R+D)/ 
           (sum(merged[, "R"], na.rm = TRUE)+sum(merged[, "D"], na.rm = TRUE))) #create likelihood for bigram being said by a dem.

#freeup memory
rm(list = c("dtm_all","merged","new_congress_join","congress_filter_speaker_D","congress_filter_speaker_R","tempD","tempR","con_df","congress_final_df"))


############## PART 2 ###############
##### (A) Data ----
# (A.0) List of FOMC Members over time
#Step 1: Get list of FOMCs from wikipedia
url <- "https://en.wikipedia.org/wiki/Federal_Reserve_Board_of_Governors" 
members <- as.data.frame(read_html(url) %>% 
                           html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/table[4]') %>% #table containing the information
                           html_table()) #retrieve table

#format date data from wiki to correct format, only keep years. # thats why we change to ENglish at the beginning
members$Term.end <- format(as.Date(members$Term.end,format="%B %d, %Y"),"%Y")
members$Term.start <- format(as.Date(members$Term.start,format="%B %d, %Y"),"%Y")

# Filter term ended after 1975 and start before 2016(because transcripts for meetings are only available from 1975 onwards)
members %>%
  filter(Term.end >= "1975",
         Term.start < "2016") -> members

# Add party affiliation of president who appointed members as best guess for their affiliation / Hypothesis
presidents <- as.data.frame(unique(members$Initialappointment)) 
colnames(presidents) <- "Initialappointment"
# manual: add for each president based on wikipedia: https://en.wikipedia.org/wiki/List_of_presidents_of_the_United_States
presidents$party <- c("D","R","R","D","R","R","D","R","D")

#add party to members datafame
members <- left_join(members,presidents, by ="Initialappointment")

#collapse to relevant info
members %>%
  select(Name,party,Term.start,Term.end) -> lastNames
# prepare for search, as Names in FOMC meetings are also Uppercase and only Lastname
lastNames$Name <- toupper(word(lastNames$Name,-1))



#### (A.1) FOMC Meeting #######
# Scrape and pre-process transcripts from FOMC meetings as a proxy of the speech by financial regulators.

#Step 1: Scrape Data
#Step 1.1: Create relevant links for download (only after 1976 transcripts exist)
#Style of link: https://www.federalreserve.gov/monetarypolicy/files/FOMCYYYYMMDDmeeting.pdf

dates <- as.character(seq(as.Date("1975-01-01"), as.Date("2015-12-31"), by="days")) #create all possible meeting dates
dates <- gsub("-","",dates) #reduce to relevant format

for (i in 1:length(dates)) {
  url <- paste( "https://www.federalreserve.gov/monetarypolicy/files/",  #create links for each possible date
                "FOMC",
                dates[i],
                "meeting.pdf",
                sep ="")
  # Step 1.2: Download files
  if(url.exists(url) == TRUE){ #check if URL exists
    download.file(url,
                  destfile = paste0("MeetingTranscripts/Meeting_", dates[i], ".pdf"), mode = "wb",
    )
  } 
}
# Step 1.3 read files as .txt 

files <- list.files(path = "MeetingTranscripts") #find all files in directory

# loop through all files and merge each year into one pdf file(instead of 8 for each meeting to increase performance)
for (i in 1976:2015) {
  temp <- c() # empty vector
  for (f in files) {
    if(grepl(i,f)){
      temp <- append(temp,paste0("MeetingTranscripts/",f)) #search for year and append it to temp for combing
    }
  }
  pdf_combine(temp, output = paste0(i,".pdf")) #combine pdfs
}

# Step 1.4 create list of Members to collect speech
# create empty dataframe
df.all <- data.frame(matrix(ncol = 2, nrow = 40))
colnames(df.all) = c("D","R")

#Step 2: Loop through files and find information
# loop through all years (2015-1975 = 40)
for (i in 1:40) {

  #read file
  txt <- pdf_text(paste0("TranscriptsYear/",years[i],".pdf"))
  #Step 3: Pre-Process Data
  #pre-clean
  txt <- gsub("\\n", " ", txt) # sub new lines
  txt <- gsub("\\[.+\\]","",txt) # sub [] remarks (no speech)
  txt <- gsub("MS.", " ",txt) # sub MS
  txt <- gsub("MR.", " ",txt) # sub MR
  txt <- paste(txt, collapse = " ") #collapse into long string
  
  #Step 4: paste speech of each gov. for each year in dataframe
  for (gov in lastNames$Name) {

    #Look-up only governors who where sitting on the board for the respective year to increase performance
    relevant <- filter(lastNames, Name == gov ) #find only relevant info of gov. in loop (start Term and end Term)
    #filter to relevant govs only
    if (gov == "YELLEN" || gov == "BERNANKE" || #those two had two terms, therefore need to be checked everytime
        years[i] >= relevant$Term.start && years[i] <= relevant$Term.end) { #start only looking if sat on board at year
      raw.data <- paste(unlist(str_extract_all(txt, paste0("(?<=",gov,").+?(?=\\s{8}[A-Z]{4})"))),collapse = " ") #search for start and end of speech for governor, search for NAME and end with [TAB]+4CAPITAL LETTERS
  #Step 5: Clean Data
      raw.data <- gsub("[^a-zA-Z ]", " ", raw.data) #reduce to letters
      raw.data <- gsub("\\s+", " ",raw.data) # sub whitespaces +1
      #create tibble
      temp <- tibble(text = raw.data)
      temp %>%
        unnest_tokens(word, text) %>% #tokens
        filter( nchar(word) >= 3, #remove words longer than 30 and less than 3 characters.
                nchar(word) <= 30,
                !word %in% stopwords()) -> tempW #remove stopwords
      temp <- paste(tempW$word,collapse=" ") #collapse into long string

  #Step 6: Add information to Dataframe for speaches of democrats and republicans
      party <- filter(lastNames, Name == gov )
      if (party$party == "D") { # if democrat, add to D , else to R
        df.all$D[i] <- paste(df.all$D[i],temp,collapse = " ") # collapse into single string
        print(paste0("Gov:",gov," ",party$party," ",years[i]," head: ",head(tempW,10), " tail: ",tail(tempW,10))) #print results to innspect
        
      } else {
        df.all$R[i] <- paste(df.all$R[i],temp,collapse = " ") # collapse into single string
        print(paste0("Gov:",gov," ",party$party," ",years[i]," head: ",head(tempW,10), " tail: ",tail(tempW,10))) #print results to innspect
      }
    } 
  }
  #free memory
  rm("txt")
}

#Step 7: Prepare for creating corpus and DTM

df.all$doc_id <- years #set years as doc id
df.all %>%
  select(doc_id,D) -> df_d #select only democrats

colnames(df_d)[2] <- "text"
#crate decades vector (for analyzing partisanship over time)
decades <- c(1985,1995,2005,2015)

#Step 8: Create dtms for each decade
for (i in decades) {
  df.d.1 <- df_d %>% filter(doc_id %in% c((i-10):i)) #filter for each decade, eg. 1975-1985, ...
  #create corpus
  con1 <- corpus(df.d.1)
  #create tokens //same cleaning steps as for congress in Part 1
  toks <- tokens(con1,
                 remove_punct = T,
                 remove_symbols = T,
                 remove_numbers = T)
  toks <- tokens_remove(toks, stopwords("english"))
  toks <- tokens_remove(toks, procedural)
  toks <- tokens_wordstem(toks)
  bigram <- tokens_remove(toks, procedural_dict, verbose = T)
  bigram <- tokens_ngrams(bigram, n = 2)
  
  #Create DFM // same as in part one
  dtm <- dfm(bigram,
             tolower = T,
             verbose = T)
  dtm <- dfm_trim(dtm, min_termfreq = 5, verbose = TRUE) #trim to words appearing at least 5 times
  
  #Convert to Dataframe for analysis
  fed_df_democrats <- as.data.frame(as.matrix(dtm)) 
  fed_df_democrats <- fed_df_democrats %>% melt() %>% filter(value != 0) #create a new transposed df
  fed_df_democrats <- fed_df_democrats %>% group_by (variable) %>% #group by bigram and sum freq.
    summarise(freq =sum(value))
  
  #Filter to only bigrams that are also present in congress Dataframe
  fed_df_democrats %>%
    filter(variable %in% final_df$variable) -> fed_df_democrats
  
  #Join congress and democrats dataframe by bigram
  fed_df_democrats <- inner_join(fed_df_democrats,final_df, by = "variable")
  fed_df_democrats$D100k <- round((fed_df_democrats$freq/sum(fed_df_democrats$freq))*10000) #add column freq of bigram per 10k words spoken based on likelihood
  fed_df_democrats$partisanship <- 1/sum(fed_df_democrats$freq) * (fed_df_democrats$freq*fed_df_democrats$percentageD) # add column for partisanship of each bigrams multiplied by freq
  
  #save each df for each decade
  assign(paste0("fed_df_d_",i),fed_df_democrats)
  
}
#free up memory
rm(list = c("bigram","df_d","df.d.1","dtm","members","presidents","relevant","speaker_map","temp","toks","party","tempW","fed_df_democrats"))

### Same for Republicans, steps are exactly the same so no comment on code
df.all %>%
  select(doc_id,R) -> df_r

colnames(df_r)[2] <- "text"

decades <- c(1985,1995,2005,2015)
for (i in decades) {
  df.r.1 <- df_r %>% filter(doc_id %in% c((i-10):i))
  
  con1 <- corpus(df.r.1)
  #create tokens
  toks <- tokens(con1,
                 remove_punct = T,
                 remove_symbols = T,
                 remove_numbers = T)
  toks <- tokens_remove(toks, stopwords("english"))
  toks <- tokens_remove(toks, procedural)
  toks <- tokens_wordstem(toks)
  bigram <- tokens_remove(toks, procedural_dict, verbose = T)

  bigram <- tokens_ngrams(bigram, n = 2)
  
  
  dtm <- dfm(bigram,
             tolower = T,
             verbose = T)
  dtm <- dfm_trim(dtm, min_termfreq = 5, verbose = TRUE)
  
  fed_df_rep <- as.data.frame(as.matrix(dtm)) 
  fed_df_rep <- fed_df_rep %>% melt() %>% filter(value != 0) #create a new transposed df
  fed_df_rep <- fed_df_rep %>% group_by (variable) %>%
    summarise(freq =sum(value))
  
  fed_df_rep %>%
    filter(variable %in% final_df$variable) -> fed_df_rep
  
  fed_df_rep <- inner_join(fed_df_rep,final_df, by = "variable")
  fed_df_rep$R100k <- round((fed_df_rep$freq/sum(fed_df_rep$freq))*10000)
  fed_df_rep$partisanship <- 1/sum(fed_df_rep$freq) * (fed_df_rep$freq*fed_df_rep$percentageD)
  
  
  
  assign(paste0("fed_df_r_",i),fed_df_rep)
  
}
rm(list = c("fed_df_rep","dtm","df.r.1","df_r","toks"))
load("Rdata")

##### PART 3: ANALYSIS ########

total_partisanship <- as.data.frame(decades)  #create empty df.
for (i in decades) { #loop through decades
  
  dem <- get(paste0("fed_df_d_",i)) #get dataframe per decade per party
  rep <- get(paste0("fed_df_r_",i)) #get dataframe per decade per party
  
  d_total <- sum(dem$freq) #calcualte total number of bigrams
  r_total <- sum(rep$freq)
  
  # create total dataframe combining rep and dem for each decade and to find most partisan sentences
  total <- full_join(dem,rep, by = "variable") %>%
    select(variable,freq.x,freq.y,percentageD.y,percentageD.x) -> total #we need both as it might be zero if does not occur in other df
  total[is.na(total)] <- 0 
  
  
  total$D100k <- round((total$freq.x/sum(total$freq.x))*10000) #column freq of phrase per 10k phrases based on likelihood
  total$sumD <- sum(total$freq.x) #total sum 
  total$R100k <- round((total$freq.y/sum(total$freq.y))*10000)
  total$sumR <- sum(total$freq.y)
  
  #rename columns
  colnames(total) <- c("bigram","D","R","percentageD","percentageDR","D_per10k","SumD","R_per10k","SumR")
  
  #Calculate partisanship based on congress for whole df.
  total$partisanship <- 1/2 * 1/total$SumD * sum(total$D*total$percentageD) +
    1/2 * 1/total$SumR * sum(total$R*(1-total$percentageDR))
  
  #create empty column in df
  total$partisanshipRemoved <- 0 
  
  #Goal: Find phrases that have the highest impact on score
  for (b in 1:length(total$bigram)) { #loop through each phrase
    
    tempBigram <- filter(total, !grepl(total$bigram[b],total$bigram)) #create temp df without that phrase
    
    #create partisanship score for "new" df without respective phrase, subtract it from partisanship score with that phrase to get impact
    total$partisanshipRemoved[b] <- (total$partisanship[b] - (1/2 * 1/sum(tempBigram$D) * sum(tempBigram$D*tempBigram$percentageD) +
                                                                1/2 * 1/sum(tempBigram$R) * sum(tempBigram$R*(1-tempBigram$percentageDR))))*100
    # take absolute value as only magnitude not sign of effect is important and save for respective phrase
    total$partisanshipRemoved[b] <- abs(total$partisanshipRemoved[b])
  }

#save for each decade
assign(paste0("total",i),total)

#FINAL STEP: Calculate partisanship per decade 
partisanshipD <- 1/2 * 1/d_total * sum(dem$freq*dem$percentageD) #partisanship D
partisanshipR <- 1/2 * 1/r_total * sum(rep$freq*(1-rep$percentageD)) #partisanship R

#Partisanship per decade:
total_partisanship$score[decades==i] <- partisanshipD + partisanshipR

}

#######PART 4: Visualisation##########
#Graph

#plot the graph 

plot <- ggplot(total_partisanship, aes(x= decades, y = score))+
  geom_point()+
  ggtitle("Figure: Partisan Score in FED from 1985-2015") +
  geom_line() + 
  theme_bw()+
  geom_text(aes(label=c("0,4978", "0,4992", "0,4994", "0,5016")),hjust=0.5, vjust=-1)+
  scale_y_continuous(name="Average Score", limits=c(0.495, 0.505)) +
  xlab("Decades")

plot + geom_hline(aes(yintercept=0.500), color = "green", lty = "dashed")

#Get most partisan phrases per decade by party for FOMC, party is determined by which side relativly said the phrase more often
#1985
total1985 %>%
  arrange(desc(partisanshipRemoved)) -> total1985_p #arrange desc for highest impact phrases
total1985_p$DorR <- ifelse(total1985_p$D_per10k > total1985_p$R_per10k, "D", "R") #assign phrase to party
total1985_p %>%
  filter(DorR == "D") -> total1985_p_d #filter for Democrats
total1985_p_d <- head(total1985_p_d,20) #save top 20 ... and so on for all others.
total1985_p %>%
  filter(DorR == "R") -> total1985_p_R
total1985_p_R <- head(total1985_p_R,20)
#1995
total1995 %>%
  arrange(desc(partisanshipRemoved)) -> total1995_p
total1995_p$DorR <- ifelse(total1995_p$D_per10k > total1995_p$R_per10k, "D", "R")
total1995_p %>%
  filter(DorR == "D") -> total1995_p_d
total1995_p_d <- head(total1995_p_d,20)
total1995_p %>%
  filter(DorR == "R") -> total1995_p_R
total1995_p_R <- head(total1995_p_R,20)
#2005
total2005 %>%
  arrange(desc(partisanshipRemoved)) -> total2005_p
total2005_p$DorR <- ifelse(total2005_p$D_per10k > total2005_p$R_per10k, "D", "R")
total2005_p %>%
  filter(DorR == "D") -> total2005_p_d
total2005_p_R <- head(total2005_p_R,20)
total2005_p %>%
  filter(DorR == "R") -> total2005_p_R
total2005_p_R <- head(total2005_p_R,20)
#2015
total2015 %>%
  arrange(desc(partisanshipRemoved)) -> total2015_p
total2015_p$DorR <- ifelse(total2015_p$D_per10k > total2015_p$R_per10k, "D", "R")
total2015_p %>%
  filter(DorR == "D") -> total2015_p_d
total2015_p_d <- head(total2015_p_d,20)
total2015_p %>%
  filter(DorR == "R") -> total2015_p_R
total2015_p_R <- head(total2015_p_R,20)
#wordcloud 
require(wordcloud)

#for Democrats
speechD <- list(total1985_p_d,total1995_p_d,total2005_p_d,total2015_p_d)
#for Republicans
speechR <- list(total1985_p_R,total1995_p_R,total2005_p_R,total2015_p_R)

for (i in 1:4) {
  layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
  par(mar=rep(0, 4))
  plot.new()
  text(x=0.5, y=0.5, i)
  speechD[[i]]$bigram  <- gsub("_", " ", speechD[[i]]$bigram)
  wordcloud(word = speechD[[i]]$bigram[1:20],
            freq = speechD[[i]]$D[1:20]
            )
}

for (i in 1:4) {
  layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
  par(mar=rep(0, 4))
  plot.new()
  text(x=0.5, y=0.5, i)
  speechR[[i]]$bigram  <- gsub("_", " ", speechR[[i]]$bigram)
  wordcloud(word = speechR[[i]]$bigram[1:20],
            freq = speechR[[i]]$R[1:20]
  )
}
