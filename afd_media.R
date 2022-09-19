

library(LexisNexisTools)
library(quanteda)

docs_all <- lnt_read("articles/Nexis_AfDall")
#choose "German" by typing corresponding value in console

#generate dataset and clean as dataset####
afd_media <- cbind(docs_all@meta, docs_all@articles)

##remove articles with less than 100(?) words and no date, deletes ~800 articles
afd_media$Length <- as.numeric(gsub(pattern=" words", replacement = "", afd_media$Length))
afd_media <- afd_media[!afd_media$Length<100,]
afd_media <- afd_media[!is.na(afd_media$Date),]
##remove one article with strange date
afd_media <- afd_media[!afd_media$Date == "1938-11-09",]

##remove duplicates by titel/text, 106 duplicates removed
article_duplicates <- afd_media[duplicated(afd_media$Article), ]
afd_media <- afd_media[!duplicated(afd_media$Article),]

##order cases by date
afd_media <- afd_media[order(afd_media$Date),]

##delete empty variables
afd_media <- subset(afd_media, select = -c(ID, Source_File, Author, Edition, Graphic))
afd_media <- subset(afd_media, select = -c(ID))

#turn into quanteda format and clean as corpus####
afd_corp <- corpus(afd_media, text_field = "Article")

#Keywords in context
afd_kwic <- kwic(afd_corp, pattern = c("AfD"), window = 5)
head(afd_kwic, 10)

#tokenization
afd_tokens <- tokens(afd_corp)
afd_tokens [[1]][1:50]

##remove punctation
afd_tokens <- tokens(afd_corp,
                     remove_punct = TRUE, remove_symbols = TRUE, 
                     remove_separators = TRUE, remove_hyphens = TRUE, remove_url = TRUE
                     )
afd_tokens [[1]][1:50]
afd_tokens <- afd_tokens %>% 
              tokens_tolower() %>% 
              tokens_remove(stopwords('german'),  padding = TRUE)
afd_tokens [[1]][1:50]

##detecting collocations
colls <- textstat_collocations(afd_tokens,min_count = 100) #minimum frequency
colls [1:10]

###corpus using collocations saved as afd_tokens_c
afd_tokens_c <- tokens_compound(afd_tokens, colls) %>% 
                tokens_remove('') # remove empty strings
afd_tokens_c[[1]][1:50]

#Creating data-frame-matrix (dfm)
afd_dfm <- dfm(afd_tokens_c, remove_numbers = TRUE)
dim(afd_dfm)
textstat_frequency(afd_dfm) %>% head(10) #'afd' most frequent word

##removing very low and very high frequent words
afd_dfm <-  afd_dfm %>% 
            dfm_keep(min_nchar = 2) %>% # remove terms < 2 characters
            dfm_trim(min_docfreq = 0.001,  #keep if word appears in at least 0.1% of documents
            max_docfreq = 0.50,# keep if word appears in <50% of documents
            docfreq_type = 'prop') # proportions instead of counts
dim(afd_dfm) #file dimensions reduced by roughly 80%
textstat_frequency(afd_dfm) %>% head(10) #spd most frequent word

textplot_wordcloud(afd_dfm, max_words = 100, color = 'black')

#dictionaries applied####

##reading in dicts
dict_LIWC_neg <- scan("dicts/LIWC_neg_germ.txt", character())
dict_LIWC_pos <- scan("dicts/LIWC_pos_germ.txt", character())
dict_LSD_neg <- scan("dicts/LSD_neg_germ.txt", character())
dict_LSD_pos <- scan("dicts/LSD_pos_germ.txt", character())
dict_rauh_neg <- scan("dicts/Rauh_neg_germ.txt", character())
dict_rauh_pos <- scan("dicts/Rauh_pos_germ.txt", character())
dict_hasel_neg <- scan("dicts/Haselmayer_Jenny_neg_germ.txt", character())
dict_sentiWS_neg <- scan("dicts/SentiWS_neg_germ.txt", character())
dict_sentiWS_pos <- scan("dicts/SentiWS_pos_germ.txt", character())

##compiling dicts as list
dicts <- dictionary(list(Haselmayer = dict_hasel_neg,
                        LIWC_neg = dict_LIWC_neg,
                        LIWC_pos = dict_LIWC_pos,
                        LSD_neg = dict_LSD_neg,
                        LSD_pos = dict_LSD_pos,
                        Rauh_neg = dict_rauh_neg,
                        Rauh_pos = dict_rauh_pos,
                        SentiWS_neg = dict_sentiWS_neg,
                        SentiWS_pos = dict_sentiWS_pos))

##apply dicts
dicts_match <- dfm_lookup(afd_dfm, dicts) 

###overall matches per dict
textstat_frequency(dicts_match)

###matches per document
dicts_match [1:10]

#computing sentiment as log(pos+0.5/neg+0.5)####

library(dplyr)
dicts_match_df <- convert(dicts_match, to = "data.frame")
dicts_match_df <- dicts_match_df %>% mutate(LIWC_sen = log((LIWC_pos+0.5)/(LIWC_neg+0.5)))
dicts_match_df <- dicts_match_df %>% mutate(LSD_sen = log((LSD_pos+0.5)/(LSD_neg+0.5)))
dicts_match_df <- dicts_match_df %>% mutate(Rauh_sen = log((Rauh_pos+0.5)/(Rauh_neg+0.5)))
dicts_match_df <- dicts_match_df %>% mutate(SentiWS_sen = log((SentiWS_pos+0.5)/(SentiWS_neg+0.5)))
head(dicts_match_df)

##adding dict matches and sentiment to existing dataframe
afd_media <- cbind(afd_media, dicts_match_df)

##set new, individual case-ID based on date-order and reorder Variables
afd_media$ID <- seq_along(afd_media[,1])
row.names(afd_media) <- afd_media$ID
afd_media <- afd_media[, c("ID", "Date", "Length", "Newspaper", "Section","Headline", "Article",
                           "Haselmayer", "LIWC_neg", "LIWC_pos", "LSD_neg", "LSD_pos", 
                           "Rauh_neg", "Rauh_pos", "SentiWS_neg", "SentiWS_pos", 
                           "LIWC_sen", "LSD_sen",  "Rauh_sen", "SentiWS_sen")]

#visualization of dict-matches####

#plotting sentiment over time for LIWC
library(ggplot2)
afd_media %>% ggplot(aes(x = Date, y = LIWC_sen)) +
  geom_line() + geom_smooth(span = 0.1, method = 'loess') + theme_bw()

#plotting sentiments of all four dicts vis-a-vis
afd_sen <- subset(afd_media, select = c(Date, LIWC_sen, LSD_sen, Rauh_sen, SentiWS_sen))
library(reshape)
afd_sen_long <- melt(afd_sen, id.vars = "Date")

ggplot(afd_sen_long, aes(x = Date, y = value, colour = variable)) + 
  geom_smooth(span = 0.1, method = 'loess')

#compute date-based averages####

#compute daily mean
Haselmayer_df <- aggregate(Haselmayer ~ Date, afd_media, mean)
LIWC_df_neg   <- aggregate(LIWC_neg ~ Date, afd_media, mean)
LIWC_df_pos   <- aggregate(LIWC_pos ~ Date, afd_media, mean)
LIWC_df       <- aggregate(LIWC_sen ~ Date, afd_media, mean)
LSD_df_neg    <- aggregate(LSD_neg ~ Date, afd_media, mean)
LSD_df_pos    <- aggregate(LSD_pos ~ Date, afd_media, mean)
LSD_df        <- aggregate(LSD_sen ~ Date, afd_media, mean)
Rauh_df_neg   <- aggregate(Rauh_neg ~ Date, afd_media, mean)
Rauh_df_pos   <- aggregate(Rauh_pos ~ Date, afd_media, mean)
Rauh_df       <- aggregate(Rauh_sen ~ Date, afd_media, mean)
SentiWS_df_neg<- aggregate(SentiWS_neg ~ Date, afd_media, mean)
SentiWS_df_pos<- aggregate(SentiWS_pos ~ Date, afd_media, mean)
SentiWS_df    <- aggregate(SentiWS_sen ~ Date, afd_media, mean)

afd_sen_daily <- cbind(Haselmayer_df, 
                       LIWC_df_neg, LIWC_df_pos, LIWC_df, 
                       LSD_df_neg, LSD_df_pos, LSD_df, 
                       Rauh_df_neg, Rauh_df_pos, Rauh_df, 
                       SentiWS_df_neg, SentiWS_df_pos, SentiWS_df)
#delete duplicate variables
afd_sen_daily <- subset(afd_sen_daily, select = -c(Date))
afd_sen_daily <- subset(afd_sen_daily, select = -c(Date.1, Date.2, Date.3, Date.4, Date.5, Date.6,
                                                   Date.7, Date.8, Date.9, Date.10, Date.11))
afd_sen_daily <- select(afd_sen_daily, Date, everything())

#Compute monthly mean
year_month <- format(as.Date(afd_sen_daily$Date, "%Y/%m/%d"), "%Y/%m")
afd_sen_daily <- cbind(afd_sen_daily, year_month)

Haselmayer_monthly     <- aggregate(Haselmayer ~ year_month, afd_sen_daily, mean)
LIWC_monthly_sen       <- aggregate(LIWC_sen ~ year_month, afd_sen_daily, mean)
LIWC_monthly_neg       <- aggregate(LIWC_neg ~ year_month, afd_sen_daily, mean)
LIWC_monthly_pos       <- aggregate(LIWC_pos ~ year_month, afd_sen_daily, mean)
LSD_monthly_sen        <- aggregate(LSD_sen ~ year_month, afd_sen_daily, mean)
LSD_monthly_neg        <- aggregate(LSD_neg ~ year_month, afd_sen_daily, mean)
LSD_monthly_pos        <- aggregate(LSD_pos ~ year_month, afd_sen_daily, mean)
Rauh_monthly_sen       <- aggregate(Rauh_sen ~ year_month, afd_sen_daily, mean)
Rauh_monthly_neg       <- aggregate(Rauh_neg ~ year_month, afd_sen_daily, mean)
Rauh_monthly_pos       <- aggregate(Rauh_pos ~ year_month, afd_sen_daily, mean)
SentiWS_monthly_sen    <- aggregate(SentiWS_sen ~ year_month, afd_sen_daily, mean)
SentiWS_monthly_neg    <- aggregate(SentiWS_neg ~ year_month, afd_sen_daily, mean)
SentiWS_monthly_pos    <- aggregate(SentiWS_pos ~ year_month, afd_sen_daily, mean)

afd_sen_monthly <- cbind(Haselmayer_monthly, 
                         LIWC_monthly_sen, LIWC_monthly_neg, LIWC_monthly_pos, 
                         LSD_monthly_sen, LSD_monthly_neg, LSD_monthly_pos,
                         Rauh_monthly_sen, Rauh_monthly_neg, Rauh_monthly_pos,
                         SentiWS_monthly_sen, SentiWS_monthly_neg, SentiWS_monthly_pos)
afd_sen_monthly <- subset(afd_sen_monthly, select = -c(year_month))
afd_sen_monthly <- subset(afd_sen_monthly, select = -c(year_month.1, year_month.2, year_month.3, year_month.4, year_month.5,
                                                       year_month.6, year_month.7, year_month.8, year_month.9, year_month.10, year_month.11))
afd_sen_monthly <- select(afd_sen_monthly, year_month, everything())

#count articles per month (Consider revising! created using proxy constant instead of n count)
afd_media$year_month <- format(as.Date(afd_media$Date, "%Y/%m/%d"), "%Y/%m")
afd_media$one <- 1
afd_mediacount_month_df <- aggregate(one ~ year_month, afd_media, sum)
colnames(afd_mediacount_month_df)[2] <- "afd_mediacount_month"
afd_media <- merge(x = afd_media, y = afd_mediacount_month_df, 
                            by = "year_month", all.x = T)
afd_sen_monthly <- merge(x = afd_sen_monthly, y = afd_mediacount_month_df, 
                   by = "year_month", all.x = T)

#count articles per day 
afd_mediacount_day_df <- aggregate(one ~ Date, afd_media, sum)
colnames(afd_mediacount_day_df)[2] <- "afd_mediacount_day"
afd_media <- merge(x = afd_media, y = afd_mediacount_day_df, 
                   by = "Date", all.x = T)
afd_sen_daily <- merge(x = afd_sen_daily, y = afd_mediacount_day_df, 
                         by = "Date", all.x = T)
ggplot() + 
  geom_line(aes(x=afd_mediacount_day_df$Date,y=afd_mediacount_day_df$afd_mediacount_day),color='blue') + 
  ylab('Values')+xlab('date')

#creating lagged media variables
afd_sen_monthly$Haselmayer_lag1m <- lag(afd_sen_monthly$Haselmayer, n = 1, default = NA)
afd_sen_monthly$LIWC_sen_lag1m <- lag(afd_sen_monthly$LIWC_sen, n = 1, default = NA)
afd_sen_monthly$LSD_sen_lag1m <- lag(afd_sen_monthly$LSD_sen, n = 1, default = NA)
afd_sen_monthly$Rauh_sen_lag1m <- lag(afd_sen_monthly$Rauh_sen, n = 1, default = NA)
afd_sen_monthly$SentiWS_sen_lag1m <- lag(afd_sen_monthly$SentiWS_sen, n = 1, default = NA)

#saving####
save(afd_media, file="afd_media.Rda")
library(haven)
write_sav(afd_media, "afd_media.sav")
write.csv2(afd_media, "afd_media.csv")

##save new dataset (monthly)
save(afd_sen_monthly, file="afd_sen_monthly.Rda")
library(haven)
write_sav(afd_sen_monthly, "afd_sen_monthly.sav")
write.csv2(afd_sen_monthly, "afd_sen_monthly.csv")

##save new dataset (daily)
save(afd_sen_daily, file="afd_sen_daily.Rda")
library(haven)
write_sav(afd_sen_daily, "afd_sen_daily.sav")
write.csv2(afd_sen_daily, "afd_sen_daily.csv")


#creating sample for manual dict check####

#media_sample100 <- afd_media[sample(nrow(afd_media), 100), ]
#write.csv2(file = "media_sample100.csv", media_sample100)
