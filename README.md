# RSentiment
A package in R to analyse sentiment of English sentences

----

This package contains three functions that allow R users to analyse sentiment (Positive, Negative, Very Positive, Very Negative, Neutral, Sarcasm) of a text. This package is currently supported in English only.

Current CRAN release is 2.1.3. To install the most updated version of this package from CRAN, run the following code.
 
`install.packages("RSentiment")`

### calculate_score()

This function loads text and calculates score of each sentence of the text fed on basis of following conditions.
* Parts of Speech tagging of each word of the sentence
* Identifying various conditions of occurrences and presence of Verb, Adverb, Adjective, Noun etc in each sentence 
* Accordingly, marking the presence of words of positive and negative sentiment and order of their occurence
* Considering negation
* Checking for sarcasm on basis of presence of punctuation(currently)
* Checking presence of emoticons in the text

This method can provide result in the following range.

* 0 indicates neutral sentiment. 
* Positive value indicates positive sentiment. 
* Negative value indicates negative sentiment. 
* 99 indicates sarcasm.

### calculate_sentiment()

This function loads text and calculates sentiment of each sentence. This function uses the score predicted by calculate_score method to 
classify the sentences.
It classifies sentences into 6 categories: 
* Positive
* Negative
* Very Positive
* Very Negative 
* Sarcasm 
* Neutral



### calculate_total_presence_sentiment()

This function loads text and calculates number of sentences under each category of 
* Positive
* Negative
* Very Positive
* Very Negative
* Neutral
* Sarcasm.


