


#' Calculate the score of sentences
#'
#' This function loads text and calculates score of each sentence on basis
#' of presence of words of positive and negative sentiment, presence of negation,
#' and checking for sarcasm. 0 indicates neutral sentiment. Positive value indicates
#' positive sentiment. Negative value indicates negative sentiment. 99 indicates
#' sarcasm.
#' @param text A vector of sentences or a sentence (English).
#' @return A vector containing polarity of each sentence.
#' @examples
#'calculate_score("This is good")
#'calculate_score(c("This is good","This is bad"))

#'@export



calculate_score <- function(text) {
  text <- as.character(text)
  text <-
    unlist(lapply(text, function(x) {
      stringr::str_split(x, "\n")
    }))
  
  check_verb <- function(r2, words, df)
  {
    s <- 100
    if (grepl("R R V", r2) | grepl("V R V", r2))
    {
      onlyVerb <- df[which(df$r1 == 'V' | df$r1 == 'M'), ]
      verbs <- onlyVerb$words
      positive.matches <- match(verbs, positive_words)
      negative.matches <- match(verbs, negative_words)
      # get the position of the matched term or NA
      # we just want a TRUE/FALSE
      positive_matches <- !is.na(positive.matches)
      negative_matches <- !is.na(negative.matches)
      score <- (sum(positive_matches) - sum(negative_matches))
      df <- df[which(!df$r1 == 'V' & !df$r1 == 'M'), ]
      words <- df$words
      r2 <- paste(df$r1, collapse = " ")
      
      s <- check_adjectives_noun(r2, words, df)
      if (score < 0)
        s <- (-s)
      
      
      
    }
    return(s)
    
  }
  
  check_adjectives_noun <- function(r2, words, df)
  {
    score <- 0
    if (grepl("R", r2))
    {
      for (i in 1:nrow(df))
      {
        if (substring(df$r1[i], 1, 1) == "R")
        {
          negation.matches <- match(words[i], c("not", "none", "no", "never"))
          score <- (-1)
          break
        }
        
      }
      
      
      
      
      
    }
    
    
    if (grepl("R J", r2))
    {
      for (i in 1:nrow(df))
      {
        if (substring(df$r1[i], 1, 1) == "R" &&
            substring(df$r1[i + 1], 1, 1) == "J")
        {
          negation.matches <- match(words[i],  c("not", "none", "no", "never"))
          positive.matches <- match(df$words[i + 1], positive_words)
          negative.matches <- match(df$words[i + 1], negative_words)
          # get the position of the matched term or NA
          # we just want a TRUE/FALSE
          positive_matches <- !is.na(positive.matches)
          negative_matches <- !is.na(negative.matches)
          negation_matches <- !is.na(negation.matches)
          
          # final score
          score <- (sum(positive_matches) - sum(negative_matches))
          if (negation_matches > 0)
            score <- (-score)
          
          break
          
        }
        
      }
      
    }
    if (grepl("R R J", r2))
    {
      #print("Entered")
      for (i in 1:nrow(df))
      {
        if (substring(df$r1[i], 1, 1) == "R" &&
            substring(df$r1[i + 1], 1, 1) == "R" &&
            substring(df$r1[i + 2], 1, 1) == "J")
        {
          # print("Entered")
          negation.matches <-
            match(words[i],  c("not", "none", "no", "never"))
          positive.matches <- match(df$words[i + 2], positive_words)
          negative.matches <- match(df$words[i + 2], negative_words)
          # get the position of the matched term or NA
          # we just want a TRUE/FALSE
          positive_matches <- !is.na(positive.matches)
          negative_matches <- !is.na(negative.matches)
          negation_matches <- !is.na(negation.matches)
          
          # final score
          score <- (sum(positive_matches) - sum(negative_matches))
          
          if (negation_matches > 0)
            score <- (-score)
          
          break
          
        }
        
      }
      
    }
    if (grepl("R R R", r2))
    {
      #print("Entered")
      for (i in 1:nrow(df))
      {
        if (substring(df$r1[i], 1, 1) == "R" &&
            substring(df$r1[i + 1], 1, 1) == "R" &&
            substring(df$r1[i + 2], 1, 1) == "R")
        {
          #print("Entered")
          negation.matches <-
            match(words[i],  c("not", "none", "no", "never"))
          positive.matches <- match(df$words[i + 2], positive_words)
          negative.matches <- match(df$words[i + 2], negative_words)
          # get the position of the matched term or NA
          # we just want a TRUE/FALSE
          positive_matches <- !is.na(positive.matches)
          negative_matches <- !is.na(negative.matches)
          negation_matches <- !is.na(negation.matches)
          
          # final score
          score <- (sum(positive_matches) - sum(negative_matches))
          if (negation_matches > 0)
            score <- (-score)
          
          break
          
        }
        
      }
      
    }
    if (grepl("R R", r2))
    {
      for (i in 1:nrow(df))
      {
        if (substring(df$r1[i], 1, 1) == "R" &&
            substring(df$r1[i + 1], 1, 1) == "R")
        {
          negation.matches <- match(words[i],  c("not", "none", "no", "never"))
          positive.matches <- match(df$words[i + 1], positive_words)
          negative.matches <- match(df$words[i + 1], negative_words)
          # get the position of the matched term or NA
          # we just want a TRUE/FALSE
          positive_matches <- !is.na(positive.matches)
          negative_matches <- !is.na(negative.matches)
          negation_matches <- !is.na(negation.matches)
          
          # final score
          score <- (sum(positive_matches) - sum(negative_matches))
          if (negation_matches > 0)
            score <- (-score)
          
          break
          
        }
        
      }
      
    }
    
    if (grepl("V N", r2))
    {
      for (i in 1:nrow(df))
      {
        if (substring(df$r1[i], 1, 1) == "V" &&
            substring(df$r1[i + 1], 1, 1) == "N")
        {
          negation.matches <- match(words[i + 1], c("none", "nobody", "nothing"))
          positiveV.matches <- match(df$words[i], positive_words)
          negativeV.matches <- match(df$words[i], negative_words)
          
          positiveN.matches <- match(df$words[i + 1], positive_words)
          negativeN.matches <- match(df$words[i + 1], negative_words)
          # get the position of the matched term or NA
          # we just want a TRUE/FALSE
          positiveV_matches <- !is.na(positiveV.matches)
          negativeV_matches <- !is.na(negativeV.matches)
          negation_matches <- !is.na(negation.matches)
          positiveN_matches <- !is.na(positiveN.matches)
          negativeN_matches <- !is.na(negativeN.matches)
          
          # final score
          vscore <- (sum(positiveV_matches) - sum(negativeV_matches))
          nscore <- (sum(positiveN_matches) - sum(negativeN_matches))
          if (nscore == 0)
            score <- vscore
          else
          {
            if (vscore < 0)
              score <- (-nscore)
            else
              score <- nscore
          }
          if (negation_matches > 0)
            score <- (-score)
          
          break
          
        }
        
      }
    }
    return(score)
  }
  
  #function to tag parts of speech of each sentence
  POStag <- function(x, words)
  {
    type <- ""
    sent_token_annotator <- openNLP::Maxent_Sent_Token_Annotator()
    word_token_annotator <- openNLP::Maxent_Word_Token_Annotator()
    pos_tag_annotator <-  openNLP::Maxent_POS_Tag_Annotator()
    y1 <-
      NLP::annotate(x, list(sent_token_annotator, word_token_annotator))
    y2 <- NLP::annotate(x, pos_tag_annotator, y1)
    
    y2w <- subset(y2, type == "word")
    tags <- sapply(y2w$features, '[[', "POS")
    r1 <- sprintf("%s",  tags)
    for (i in 1:length(r1))
    {
      r1[i] <- substring(r1[i], 1, 1)
    }
    
    
    df <- data.frame(r1, words)
    df <- df[!df$r1 == 'D', ]
    words <- df$words
    r2 <- paste(df$r1, collapse = " ")
    
    score_adj <- check_adjectives_noun(r2, words, df)
    
    score_verb <- check_verb(r2, words, df)
    
    if (score_verb == 100)
      return (score_adj)
    else
      return(score_verb)
    
  }
  #function to calculate number of words in each category within a sentence
  getpolarity <- function(sentences,
                          negative_words,
                          positive_words) {
    negation <- c("no", "not", "none", "nobody", "nothing", "never")
    polaritys <-
      plyr::laply(sentences, function(sentence,
                                      negative_words,
                                      positive_words) {
        if (is.na(sentence))
          return(-1)
        if (regexpr("[?]", sentence) > 0)
          return(99)
        if (grepl(":-(", sentence, fixed = TRUE))
          sentence <- paste(sentence, "bad", sep = " ")
        else if (grepl(":-)", sentence, fixed = TRUE) ||
                 grepl(":)", sentence, fixed = TRUE))
          sentence <- paste(sentence, "good", sep = " ")
        else if (grepl(":-D", sentence, fixed = TRUE))
          sentence <- paste(sentence, "very good", sep = " ")
        else if (grepl(":-|", sentence, fixed = TRUE))
          sentence <- paste(sentence, "indifferent", sep = " ")
        else if (grepl(":-X", sentence, fixed = TRUE))
          sentence <- paste(sentence, "very bad", sep = " ")
        else
          sentence <- paste(sentence, "", sep = "")
        
        sentence <- iconv(sentence, "WINDOWS-1252", "UTF-8")
        
        
        #remove unnecessary characters and split up by word
        trim <- function (x)
          gsub("^\\s+|\\s+$", "", x)
        
        #n't is equivalent to not
        sentence <- gsub("n't", " not", sentence)
        
        sentence <- trim(sentence)
        sentence <- gsub('[[:punct:]]', '', sentence)
        sentence <- gsub('[[:cntrl:]]', '', sentence)
        sentence <-
          gsub("[[:punct:]]",
               "",
               iconv(sentence, to = "ASCII//TRANSLIT"))
        sentence <- tolower(sentence)
        wordList <- stringr::str_split(sentence, '\\s+')
        words <- unlist(wordList)
        
        #build vector with matches between sentence and each category
        positive.matches <- match(words, positive_words)
        negative.matches <- match(words, negative_words)
        # get the position of the matched term or NA
        # we just want a TRUE/FALSE
        positive_matches <- !is.na(positive.matches)
        negative_matches <- !is.na(negative.matches)
        # final score
        score <- sum(positive_matches) - sum(negative_matches)
        
        very.matches <- match(words, c("very", "most", "more"))
        very_matches <- !is.na(very.matches)
        if (score >= 0)
          score <- score + sum(very_matches)
        else
          score <- score - sum(very_matches)
        
        negation.matches <- match(words, negation)
        negation_matches <- !is.na(negation.matches)
        
        
        if (sum(negation_matches) > 0)
          score <- POStag(sentence, words)
        
        
        return(score)
        
        
      }, negative_words, positive_words)
    
    return(polaritys)
  }
  
  negative_words <- iconv(negative_words, "WINDOWS-1252", "UTF-8")
  positive_words <- iconv(positive_words, "WINDOWS-1252", "UTF-8")
  
  #build tables of positive and negative sentences with polaritys
  negative_words <- tolower(negative_words)
  positive_words <- tolower(positive_words)
  
  res <- getpolarity(text, negative_words, positive_words)
  return (res)
}



#' Predicts the sentiment of sentences
#'
#' This function loads text and calculates sentiment of each sentence. It classifies
#' sentences into 6 categories: Positive, Negative, Very Positive, Very Negative
#' Sarcasm and Neutral.
#'
#' @param text A vector of sentences or a sentence (English).
#' @return A vector containing sentiment of each sentence.

#' @examples
#'calculate_sentiment("This is good")
#'calculate_sentiment(c("This is good","This is bad"))
#'@export
calculate_sentiment <- function(text)
{
  res <- calculate_score(text)
  
  sentiment <- c()
  
  
  for (i in 1:length(res))
  {
    if (res[i] == 0)
    {
      sentiment[i] <- 'Neutral'
      
      
    }
    else if (res[i] == -1) {
      sentiment[i] <- 'Negative'
      
    }
    else if (res[i] == 1) {
      sentiment[i] <- 'Positive'
      
    }
    else if (res[i] > 1) {
      sentiment[i] <- 'Very Positive'
      
      
      
    }
    else if (res[i] == 99)
    {
      sentiment[i] <- 'Sarcasm'
    }
    else{
      sentiment[i] <- 'Very Negative'
      
    }
    
    
    
    
  }
  results <- data.frame(text, sentiment)
  return (results)
}
#' Calculate the number of sentences in each category of sentiment.
#'
#' This function loads text and calculates number of sentences which are positive,
#' negative, very positive, very negative, neutral and sarcasm.
#'
#' @param text A vector of sentences or a sentence (English).
#' @return A 2-D matrix with two rows and 6 columns where first row contains the name of sentiment
#' category and the second row contains the number in each category in string format.
#' @examples
#'calculate_total_presence_sentiment(c("This is good","This is bad"))
#'@export
calculate_total_presence_sentiment <- function(text) {
  res <- calculate_score(text)
  
  
  score_array <- array(0, dim = c(2, 6))
  score_array[1, 1] <- 'Sarcasm'
  score_array[1, 2] <- 'Neutral'
  score_array[1, 3] <- 'Negative'
  score_array[1, 4] <- 'Positive'
  score_array[1, 5] <- 'Very Negative'
  score_array[1, 6] <- 'Very Positive'
  
  for (i in 1:length(res))
  {
    if (res[i] == 99)
    {
      score_array[2, 1] <- as.numeric(score_array[2, 1]) + 1
    }
    else if (res[i] == 0)
    {
      score_array[2, 2] <- as.numeric(score_array[2, 2]) + 1
      
    }
    else if (res[i] == -1) {
      score_array[2, 3] <- as.numeric(score_array[2, 3]) + 1
    }
    else if (res[i] == 1) {
      score_array[2, 4] <- as.numeric(score_array[2, 4]) + 1
    }
    else if (res[i] > 1) {
      score_array[2, 6] <- as.numeric(score_array[2, 6]) + 1
    }
    
    else{
      score_array[2, 5] <- as.numeric(score_array[2, 5]) + 1
      
    }
    
    
    
    
  }
  
  return (score_array)
}
