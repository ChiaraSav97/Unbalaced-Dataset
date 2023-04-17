library(tidyverse)
library(tokenizers)
library(stringr)
library(tm)
library(dplyr)
library(psych)
library(ggplot2)
#I choose
#"The Trending YouTube Video Statistics" dataset, which is a daily record with daily 7
#statistics for trending Youtube videos. 
#Data includes video title, channel title, publish time, tags, views, likes
#and dislikes, description, and comment count.
#I choose to analize the trendind Youtube videos for Great Britain.

#First of all I read my dataset:

you.data <- read.csv("C://Users//kikis//Desktop//LASTYYYEAR//ml TESSERA//Nuova cartella//AI AND ML//SavoldiChiara5014502_project//GBvideos.csv",
                    header = T, sep=",")
#I can look to the first rows of the dataset:
view(head(you.data))
str(you.data)
# I can see some statistical information
describe(you.data)
#My dataset is composed by 38916 and 16 columns.
dim(you.data)

#Look at the variables: let's see if there are any missing values, which they will be represented with NaN or None.
#view(you.data)

#Let's check for every:
is.na(you.data$video_id)
#is.na(you.data$trending_date)
#is.na(you.data$title)
#is.na(you.data$channel_title)
#is.na(you.data$category_id)
#is.na(you.data$publish_time)
#is.na(you.data$tags)
#is.na(you.data$views)
#is.na(you.data$likes)
#is.na(you.data$dislikes)
#is.na(you.data$comment_count)
#is.na(you.data$thumbnail_linke)
#is.na(you.data$comments_disables)
#is.na(you.data$rating_disables)
#is.na(you.data$video_error_or_removed)
#is.na(you.data$description)
#So lucky. It seems I have no missing values in my dataset...
#Looking at the data I can see that the missing values are defined as [none]

view(you.data$tags)
df <- you.data
df$tags[df$tags == "[none]"] <- NA
view(df$tags)
#It's necessary to transform the undefined as 'NA' and then ,

head(which(is.na(df$tags)))# The which() function returns the positions with missing values in your vector.
# In our case there are NA's at positions 5 & 66...
# how many total missing values do I have?

total_missing <- sum(is.na(df))
total_missing
total_cells <- nrow(df)
# percent of data that is missing
percent_missing = (total_missing/total_cells) * 100
percent_missing

#A logical vector specifying which observations/rows have
#no missing values across the entire sequence.

x <- df[, -1] # x is a matrix
y <- df[,  1] # y is the complementary

stopifnot(complete.cases(y) != is.na(y))
ok <- complete.cases(x, y)
sum(!ok) # how many are not "ok" (and so they're missing values) ?
x <- x[ok,]
y <- y[ok]
# }


head(is.na(you.data$video_id))
#I can see that I have again 2010, like what I find in the total_missing variable!
#remove all the rows that contain a missing value

original_col <- dim(you.data)
print("Rows and columns in original dataset:")
original_col

you.data <- you.data[na.omit(you.data$tags == "[none]") == FALSE, ]
#how much data did we lose?
print("Rows and columns with na's dropped:")
dropped_col <-dim(you.data)
total_lose <- original_col - dropped_col
total_lose

#OUTLIERS
#In R, this can easily be done with the summary() function. After that, the best waay to 
#observe the data is to plot:
#VIEWS COLUMN
hist(you.data$views,
     xlab = "Views",
     ylab = "Observations",
     main = "Histogram of number of views",
)


ggplot(you.data) +
  aes(x = views) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  theme_minimal()
boxplot.stats(you.data$views)$out

#Thanks to the which() function it is possible to extract the row number
#corresponding to these outliers:
  
out <- boxplot.stats(you.data$views)$out
out_ind <- which(you.data$views %in% c(out))
out_ind

#With this information we can go back to the specific rows in the dataset to verify them, or print all variables for these outliers:
you.data[out_ind, ]

#It is also possible to print the values of the outliers directly on the boxplot with the mtext() function:
  
boxplot(you.data$views,
        ylab = "Views",
        main = "Boxplot of views"
)
mtext(paste("Outliers: ", paste(head(out), collapse = ", ")))
meanV <- mean(you.data$views)
meanV
max(you.data$views)
min(you.data$views)
out1 <- boxplot.stats(you.data$dislikes)$out
boxplot(you.data$dislikes,
        ylab = "Dislikes",
        main = "Boxplot of dis"
)
mtext(paste("Outliers: ", paste(head(out1), collapse = ", ")))

out2 <- boxplot.stats(you.data$likes)$out
boxplot(you.data$likes,
        ylab = "Likes",
        main = "Boxplot of dis"
)
mtext(paste("Outliers: ", paste(head(out2), collapse = ", ")))
out3 <- boxplot(you.data$views, plot=FALSE)$out
you.data<- you.data[-which(you.data$views %in% out3),]

# We found some outliers
# we must be careful that they do not influence our reasoning.
#I believe the elimination of the outliers depends on the type of analysis 
#I am interested to do. Since we're looking at the most viral videos, 
#it makes sense to eliminate those that have fewer than average views.


you.data$likes -scale(you.data$likes, center = TRUE, scale = TRUE)
#print(head(you.data$likes));

#Let's look at the type of each data:
#CHeck the data type of our date column
#Print the first date column:
date <- you.data$trending_date
#head(date)
#It seems that the date is in the format Y/D/M

you.data$trending_date <-paste(substr(you.data$trending_date,1,2),
                   substr(you.data$trending_date,7,8),
                   substr(you.data$trending_date,4,5),
                   sep="-")

#We can check if all the numbers
#We can my guess is correct. Days of the week should not exceed 31.

Days <- substr(you.data$trending_date,7,8)>31
Days
cols_v <- c("green", "red")
#Days of the week not exceed 31. The values are all false
pie(table(Days), main = "Days", col = cols_v) 
Months <- substr(you.data$trending_date,4,5)>12
#Months
#pie(table(Months), main = "Months")
Years <- substr(you.data$trending_date,1,2)>21
#Years
#pie(table(Years), main = "Years")

#I will create three new columns for my dataset
you.data <- mutate(you.data, 'Trending day' = substr(you.data$trending_date,7,8))
you.data <- mutate(you.data, 'Trending month' = substr(you.data$trending_date,4,5))
you.data <- mutate(you.data, 'Trending year' = substr(you.data$trending_date,1,2))
#head(you.data)

Trending_Days <- you.data$`Trending day`
you.data %>% 
  ggplot(aes(x = Trending_Days)) +
  geom_bar(aes(fill = Trending_Days))

Trending_Months <- you.data$`Trending month`
you.data %>% 
  ggplot(aes(x = Trending_Months)) +
  geom_bar(aes(fill = Trending_Months))
Trending_Years <- you.data$`Trending year`
you.data %>% 
  ggplot(aes(x = Trending_Years)) +
  geom_bar(aes(fill = Trending_Years))

#I take the observations where the year is the 17 and I associate to every day the 
#correct days of weeks, in order to see in which day of the week the videos are most viral 

# Specifying the date
Sys.setlocale("LC_TIME", "English")
date <- as.POSIXlt(you.data$trending_date)
date
you.data$trending_date
# Calling weekdays() function
wdays <- weekdays(date)
wdays

you.data <- mutate(you.data, 'Trending wdays'  = wdays )
#view(you.data)

you.data$trending_date <- as.POSIXct(strptime(you.data$trending_date, "%y-%m-%d"))

public_holiday <- you.data$`Trending wdays`[you.data$`Trending wdays` == 'Saturday' | 
                                              you.data$`Trending wdays` == 'Sunday']

public_holiday
dayoff <- you.data$`Trending wdays`[you.data$`Trending wdays` == 'Monday' | 
                                      you.data$`Trending wdays` == 'Tuesday'| 
                                      you.data$`Trending wdays` == 'Wednesday'| 
                                      you.data$`Trending wdays` == 'Thursday'| 
                                      you.data$`Trending wdays` == 'Friday']


dayoff

you.data$publish_time <-paste(substr(you.data$publish_time, 3,4),
                               substr(you.data$publish_time,6,7),
                               substr(you.data$publish_time,9,10),
                              substr(you.data$publish_time,12,13),
                              substr(you.data$publish_time,15,16),
                              substr(you.data$publish_time,18,19),
                               sep="-")

you.data$publish_time <- as.POSIXct(strptime(you.data$publish_time, "%y-%m-%d-%H-%M-%S"))
pub_time <- you.data$publish_time
pub_time <-paste(substr(you.data$publish_time, 12, 13),
                              substr(you.data$publish_time,15,16),
                              substr(you.data$publish_time,18,19 ),
                              sep=":")
pub_time
you.data <- mutate(you.data, 'Video time publishing'  = pub_time )
you.data$publish_time <-paste(substr(you.data$publish_time, 3,4),
                              substr(you.data$publish_time,6,7),
                              substr(you.data$publish_time,9,10),
                              sep="-")
you.data$publish_time <- as.POSIXct(strptime(you.data$publish_time, "%y-%m-%d"))

#CHARACTER COLUMN MANAGEMENT
you.data$title
you.data$title <- gsub(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)","",you.data$title)

##removing stopwords
stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
you.data$title = stringr::str_replace_all(you.data$title, stopwords_regex, '')
you.data$title <- str_squish(you.data$title)

##Removing punctuation and switching to lowercase
you.data$title <- gsub("[^[:alnum:]]", " ", you.data$title)
you.data$title<- gsub("[[:punct:]]", " ", you.data$title)
you.data$title <- tolower(you.data$title)
you.data$title
##Removing pronouns, letters with accents...
you.data$title <- gsub('Ã¯', '', you.data$title)
you.data$title <- gsub('Ã¢', '', you.data$title)
you.data$title <- gsub('Ã°', '', you.data$title)
you.data$title <- gsub('Ã¿', '', you.data$title)
you.data$title <- gsub('â‚¬', '', you.data$title)
you.data$title<- gsub('Â¦', '', you.data$title)
you.data$title<- gsub('???', '', you.data$title)
you.data$title<- gsub('©', '', you.data$title)
you.data$title<- gsub('o¨', '', you.data$title)
you.data$title<- gsub('ã', '', you.data$title)
you.data$title<- gsub('â', '', you.data$title)
you.data$title<- gsub('ë', '', you.data$title)
you.data$title<- gsub('ì>ë"ì¼???ì', '', you.data$title)
you.data$title<- gsub('£', '', you.data$title)
you.data$title<- gsub('«', '', you.data$title)
you.data$title<- gsub('¢', '', you.data$title)
you.data$title<- gsub('.¬', '', you.data$title)
you.data$title<- gsub('³', '', you.data$title)
you.data$title<- gsub('ğ', '', you.data$title)
you.data$title<- gsub('T', '', you.data$title)
you.data$title<- gsub('ìS¤ì.', '', you.data$title)
you.data$title<- gsub('ë®¤ì§', '', you.data$title)
you.data$title<- gsub('o', '', you.data$title)
you.data$title<- gsub('ì???', '', you.data$title)


##Remove single chararacter's words
you.data$title <- gsub(" *\\b[[:alpha:]]{1}\\b *", " ", you.data$title)
##Removing extra spaces
you.data$title <- str_squish(you.data$title)

#COLUMN CHANNEL
you.data$channel_title <- gsub(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)","",you.data$channel_title)

##removing stopwords
no_inf <- you.data[you.data$video_id == "YFHHGETsxkE",]
stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
you.data$channel_title = stringr::str_replace_all(you.data$channel_title, stopwords_regex, '')
you.data$channel_title <- str_squish(you.data$channel_title)

##Removing punctuation and switching to lowercase
you.data$channel_title <- gsub("[^[:alnum:]]", " ", you.data$channel_title)
you.data$channel_title <- gsub("[[:punct:]]", " ", you.data$channel_title)

#you.data$channel_title
you.data$channel_title <- tolower(you.data$channel_title)

##Removing pronouns, letters with accents..
you.data$channel_title <- gsub('Ã¯', '', you.data$channel_title)
you.data$channel_title <- gsub('Ã¢', '', you.data$channel_title)
you.data$channel_title <- gsub('Ã°', '', you.data$channel_title)
you.data$channel_title <- gsub('Ã¿', '', you.data$channel_title)
you.data$channel_title <- gsub('â‚¬', '', you.data$channel_title)
you.data$channel_title<- gsub('Â¦', '', you.data$channel_title)
you.data$channel_title<- gsub('???', '', you.data$channel_title)
you.data$channel_title<- gsub('©', '', you.data$channel_title)
you.data$channel_title<- gsub('o¨', '', you.data$channel_title)
you.data$channel_title<- gsub('ã', '', you.data$channel_title)
##Remove single chararacter's words
you.data$channel_title <- gsub(" *\\b[[:alpha:]]{1}\\b *", " ", you.data$channel_title)

#Description
pronouns <- c('i', 'you', 'he', 'she', 'it', 'we', 'they', 'm', 're', 's', 've', 't', 'don', 'can', 'is','and', 'or')
you.data$description <- removeWords(you.data$description,pronouns)

you.data$description <- gsub(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)","",you.data$description)

##removing stopwords
stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
you.data$description = stringr::str_replace_all(you.data$description, stopwords_regex, '')
you.data$description <- str_squish(you.data$description)

##Removing punctuation and switching to lowercase
you.data$description <- gsub("[^[:alnum:]]", " ", you.data$description)
you.data$description <- gsub("[[:punct:]]", " ", you.data$description)
you.data$description <- tolower(you.data$description)

##Removing pronouns, letters with accents..
you.data$description <- gsub('Ã¯', '', you.data$description)
you.data$description <- gsub('Ã¢', '', you.data$description)
you.data$description <- gsub('Ã°', '', you.data$description)
you.data$description <- gsub('Ã¿', '', you.data$description)
you.data$description <- gsub('â‚¬', '', you.data$description)
you.data$description <- gsub('Â¦', '', you.data$description)
you.data$description <- gsub('???', '', you.data$description)
you.data$description<- gsub('©', '', you.data$description)
you.data$description<- gsub('o¨', '', you.data$description)
you.data$description<- gsub('ã', '', you.data$description)
you.datadescription<- gsub('â', '', you.data$description)
you.data$description<- gsub('ë', '', you.data$description)
you.data$description<- gsub('ì>ë"ì¼???ì', '', you.data$description)
you.data$description<- gsub('£', '', you.data$description)
you.data$description<- gsub('«', '', you.data$description)
you.data$description<- gsub('¢', '', you.data$description)
you.data$description<- gsub('.¬', '', you.data$description)
you.data$description<- gsub('³', '', you.data$description)
you.data$description<- gsub('ğ', '', you.data$description)
you.data$description<- gsub('T', '', you.data$description)
you.data$description<- gsub('ìS¤ì.', '', you.data$description)
you.data$description<- gsub('ë®¤ì§', '', you.data$description)
you.data$description<- gsub('o', '', you.data$description)
you.data$description<- gsub('ì???', '', you.data$description)
you.data$description<- gsub('Ã', '', you.data$description)
you.data$description<- gsub('â', '', you.data$description)
you.data$description<- gsub('¶', '', you.data$description)

##Remove single chararacter's words
you.data$description <- gsub(" *\\b[[:alpha:]]{1}\\b *", " ", you.data$description)

##Removing extra spaces
you.data$description <- str_squish(you.data$description)

##Deleting rows with no text
sum(is.na(you.data$description))
you.data <- you.data[!(is.na(you.data$description) | you.data$description ==""), ]

#TAGS
you.data$tags <- tolower(you.data$tags)

##Removing pronouns..
you.data$tags <- gsub('Ã¯', '', you.data$tags)
you.data$tags <- gsub('Ã¢', '', you.data$tags)
you.data$tags <- gsub('Ã°', '', you.data$tags)
you.data$tags <- gsub('Ã¿', '', you.data$tags)
you.data$tags <- gsub('â‚¬', '', you.data$tags)
you.data$tags<- gsub('Â¦', '', you.data$tags)
you.data$tags <- gsub('???', '', you.data$tags)
you.data$tags<- gsub('©', '', you.data$tags)
you.data$tags<- gsub('o¨', '', you.data$tags)
you.data$tags<- gsub('ã', '', you.data$tags)
you.data$tags<- gsub('â', '', you.data$tags)
you.data$tags<- gsub('ë', '', you.data$tags)
you.data$tags<- gsub('ì>ë"ì¼???ì', '', you.data$tags)
you.data$tags<- gsub('£', '', you.data$tags)
you.data$tags<- gsub('«', '', you.data$tags)
you.data$tags<- gsub('¢', '', you.data$tags)
you.data$tags<- gsub('.¬', '', you.data$tags)
you.data$tags<- gsub('³', '', you.data$tags)
you.data$tags<- gsub('ğ', '', you.data$tags)
you.data$tags<- gsub('T', '', you.data$tags)
you.data$tags<- gsub('ìS¤ì.', '', you.data$tags)
you.data$tags<- gsub('ë®¤ì§', '', you.data$tags)
you.data$tags<- gsub('o', '', you.data$tags)
you.data$tags<- gsub('ì???', '', you.data$tags)

you.data$tags<- gsub('|¹.íz^ís¸|°íf"o."<¨', '', you.data$tags)
you.data$tags<- gsub('æ±å®|,´,¸f|ç???æ®|,f<f¡|ç´°ç°å®^|æoªæ¥|fÿf,¤|f,±ffz®å­|ss<¿,,|ss<¿,,®é>¨¨é>ª|,µfzf¼,¦,f¼,º|æ,,<,<å°å¥|æ~ ç»', '', you.data$tags)

you.data$tags<- gsub('ææ~ ç"»å???ç"ÿfª,¹f^|f,±ff', '', you.data$tags)
you.data$tags<- gsub('|f,±fff^ff,¹,¿f¼', '', you.data$tags)
you.data$tags<- gsub('"|ìs¤íf???ì???½|í<°ì', '', you.data$tags)
you.data$tags<- gsub('^"o²¨²|ì."ì´¦°|ì¡°ì´|ìê¸°|ì""|ì~^', '', you.data$tags)
you.data$tags<- gsub('ì>"ì???ì´|len|¡oì-"|"|í<°ì', '', you.data$tags)
you.data$tags<- gsub('í""¡oì ís¸tl', '', you.data$tags)
you.data$tags<- gsub('å', '', you.data$tags)
you.data$tags<- gsub('æ', '', you.data$tags) 

you.data$tags<- gsub('®¤¹', '', you.data$tags)
you.data$tags<- gsub('¼', '', you.data$tags)
you.data$tags<- gsub('¶', '', you.data$tags)
you.data$tags<- gsub('êµ', '', you.data$tags)
you.data$tags<- gsub('???', '', you.data$tags)
you.data$tags<- gsub('"|®¤ì§¹"""ì~¤|¸"ì¦^|ì.ì???o|² ì´¹"ì???oìs¸|ìo ì§???ì. |ì"oì§???ì^~|ì´¯¸ì|kei|jin|¥~ì^~ì .|ì .ì~^ì,', '', you.data$tags)
you.data$tags<- gsub('êê<¨ ì´^ì½"ì½"|ì´^ì½"ì½"|êê<¨ ì´^ì½"ì½" "|êê<¨ ì"¸ì .', '', you.data$tags)
you.data$tags<- gsub('®¤¹"|ìs¤ífì???½|í<°ì', '', you.data$tags)
you.data$tags<- gsub('í.o¥~|hallyu|f­,¨f|fÿf¥f,¸ff,¯|fÿf¥f,¸ff,¯f"f???,ª|,±fffff-|éÿ">½®­o|,,¤f???f|éÿ"µ|éÿ">½|ì."ì', '', you.data$tags)
you.data$tags[you.data$tags == "/"] <- "|"
#Instead of no_inf variable, we can drop the column without relevant information:
you.data <- you.data[!(is.na(you.data$channel_title) | you.data$channel_title ==""), ]
you.data <- you.data[!(is.na(you.data$title) | you.data$title ==""), ]
you.data <- you.data[!(is.na(you.data$tags) | you.data$tags ==""), ]

#I can look to thee binary table in order to see if the values are all 'true' or 'false'
cols<-c("yellow","white")
pie(table(you.data$comments_disabled), main = 'Comments disabled',col= cols)
cols<-c("pink","green")
pie(table(you.data$video_error_or_removed), main = 'Video Removed', col= cols)
cols<-c("lightblue","orange")
pie(table(you.data$ratings_disabled), main = 'Rating Disabled', col = cols)

#The column error removed: I exclude the values true because if a video has been removed it might not interest me
you.data <- you.data[you.data$comments_disabled == "False", ]
you.data <- you.data[you.data$ratings_disabled == "False", ]
you.data <- you.data[you.data$video_error_or_removed == "False", ]

#I can rename better the column of my dataset
colnames(you.data)[colnames(you.data) == 'video_id'] <- 'Link of the video' 
colnames(you.data)[colnames(you.data) == 'trending_date'] <- 'Trending Date'
colnames(you.data)[colnames(you.data) == 'title'] <- 'Title of the video'
colnames(you.data)[colnames(you.data) == 'channel_title'] <- 'Name of the channel'
colnames(you.data)[colnames(you.data) == 'category_id'] <- 'Category of the video'
colnames(you.data)[colnames(you.data) == 'publish_time'] <- 'Date of publishing'
colnames(you.data)[colnames(you.data) == 'tags'] <- 'Tags'
colnames(you.data)[colnames(you.data) == 'views'] <- 'Views'
colnames(you.data)[colnames(you.data) == 'likes'] <- 'Likes'
colnames(you.data)[colnames(you.data) == 'dislikes'] <- 'Dislikes'
colnames(you.data)[colnames(you.data) == 'comment_count'] <- 'Number of comments'
colnames(you.data)[colnames(you.data) == 'thumbnail_link'] <- 'Thumbnails'
colnames(you.data)[colnames(you.data) == 'comments_disabled'] <- 'Comments disabled'
colnames(you.data)[colnames(you.data) == 'ratings_disabled'] <- 'Ratings disabled'
colnames(you.data)[colnames(you.data) == 'video_error_or_removed'] <- 'Error video or removed'
colnames(you.data)[colnames(you.data) == 'description'] <- 'Video description'

##Writing the cleaned DS
write.table(as.matrix(you.data),"C://Users//kikis//Desktop//LASTYYYEAR//ml TESSERA//Nuova cartella//AI AND ML//SavoldiChiara5014502_project//cleanedDF.csv", row.names = FALSE, sep = ";")
