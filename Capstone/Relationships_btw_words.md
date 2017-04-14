---
title: "Milestone report"
subtitle: "Exploratory data analysis: Relationships between words"
author: agrou
date: "13 March 2017"
---

# Scope 
Understand the distribution and relationship between the words, tokens and phares in the text and build a linguistic predictive model.

# Tasks
1. Exploratory Analysis
2. Understand frequencies of words and words pairs 

# Questions
1. Some words are more frequent than others - what are the distributions of word frequencies?
2. What are the frequencies of 2-grams and 3-grams in the dataset?
3. How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%?
4. How do you evaluate how many of the words come from foreign languages?
5. Can you think of a way to increase the coverage - identifying words that may not be in the corpora or using a smaller number of words in the dictionary to cover the same number of phrases?

# References

For this report I used Julia Silge's Documentation for Tidy Text Mining: http://juliasilge.com/blog/Life-Changing-Magic/
http://tidytextmining.com/
Feinerer, Hornik and Meyer. Text Mining Infrastructure in R. Journal of Statistical Software. 2008 (https://www.jstatsoft.org/article/view/v025i05)

# Data

Load the data
```{r, eval = FALSE}
# define the directory to store the zipfile
destfile <- "/Users/andreia/Documents/Data_Science_Projects/Coursera/courses/Capstone/Data/Coursera-Swiftkey.zip"
# save the URL with the zipfile
fileUrl <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
# Download the zipfile
download.file(url = fileUrl, destfile = destfile, method = "curl")
```

Load libraries
```{r, message = FALSE}
library(purrr) # for map function
library(tidyverse)
library(tidytext)
library(stringr)
library(ggthemes)
library(gridExtra)
library(hunspell)
```

Read the data
```{r}
# Load the three data sets together

enUS_folder <- "Data/final/en_US/"
 
# create a function to read the three documents in the same file
read_folder <- function(infolder){
        tibble(file = dir(infolder, full.names = TRUE)) %>% 
               mutate(text = map(file, read_lines)) %>% 
               transmute(id = basename(file), text) %>% 
               unnest(text)
}

files <- read_folder(enUS_folder)
head(files)
```
The 3 files combined have 4597879 lines.

Resample data for quicker analysis
```{r}
# resample data
files3 <- files %>%
        sample_frac(0.2) #resample data
```


# Text mining and Natural language processing

## Data Cleaning and Tokenization
```{r, tidy = TRUE}
# Data cleaning

cleanAll <- files3 %>%
        # rename the dataset/variable names 
        mutate(id = str_replace_all(id, c("en_US.twitter.txt" = "Twitter", 
                                             "en_US.news.txt" = "News",
                                             "en_US.blogs.txt" = "Blogs")), 
               #replace all digits with nothing
               text = str_replace_all(text, "\\d+", ""), 
               #replace bullet & html characters, single or double dashes and repeated vowels with a space
               text = str_replace_all(text, "[\r?\n|\røØ]|[^a-zA-Z0-9_']|\\_|\\b[aeiou]{2,}\\b", " "), 
               #text = str_replace_all(text, "[^a-zA-Z0-9_']", " "), #replace non-english characters with space
               #text = str_replace_all(text, "\\_", " "), #replace all extra punctuation with nothing
               # replace words that are just repetitions of vowels with nothing
               #text = str_replace_all(text, "\\b[aeiou]{2,}\\b", " "),
               text = tolower(text)
               ) 
head(cleanAll)
```

## Exploratory Analysis

### 1. Some words are more frequent than others - what are the distributions of word frequencies?

I start by transforming the text into single tokens (words).
```{r}
WordToken <- cleanAll %>%
        # Tokenization into words
        unnest_tokens(word, text) %>% # separate each line of text into word tokens 
        filter(!str_detect(word, "\\b[aeiou]{2,}\\b")) %>% #remove words with only repeated vowels generated after tokenization
        anti_join(stop_words) #remove stopwords

head(WordToken)
```
`anti_join` function is used to remove *stopwords*. 

What are the most common words in the whole corpus of text?
```{r}
AllCounts <- WordToken %>%
        count(word, sort = TRUE) %>%
        mutate(word = factor(word, levels = rev(unique(word)))) %>%
        top_n(12) %>%
        ggplot(aes(word, n, fill = word, order = n)) +
        geom_col(show.legend = FALSE) + 
        ggtitle("Most frequent words") +
        xlab("Word counts") + 
        coord_flip() +
        theme_fivethirtyeight()

AllCounts
```

The plot above represents the most frequent words in the whole text corpus. 

But what are the group counts? The most frequent words by ID? 

```{r}
IDCounts <- WordToken %>%
        group_by(id, word) %>% 
        count(word, sort = TRUE) 

head(IDCounts)
```

```{r}
WordFreq <- IDCounts %>%
        #convert words into factors for unique values
        mutate(word = factor(word, levels = rev(unique(word)))) %>%
        top_n(12) %>%
        ggplot(aes(word, n, fill = id, order = n)) +
        geom_col() + 
        ggtitle("Most frequent words by ID") +
        xlab(NULL) + 
        coord_flip() +
        theme_fivethirtyeight()

WordFreq
```

```{r}
# Total number of words by ID
TotalWords <- IDCounts %>%
        group_by(id) %>% 
        summarise(total = sum(n)) 
TotalWords
```
Here we see the three datasets have different lenghts, that's one of the reasons why we should calculate the term frequencies (tf) along with the inverse document frequency (idf). 

idf decreases the weight for commonly used words and increases the weight for words that are not used very much. tf and idf combined give the frequency of a term adjusted for how rarely it is used.

```{r}
tf <- IDCounts %>%
        bind_tf_idf(word, id, n) %>%
        arrange(desc(tf_idf)) 

head(tf)
```

# Visualize top words within groups
```{r}
Blogs_graph <- tf %>%
        filter(id == "Blogs") %>%
        # factoring is applied here to order the words by the top word frequencies 
        mutate(word = factor(word, levels = rev(unique(word)))) %>%
        top_n(12) %>%
        ggplot(aes(word, tf_idf, fill = word)) +
        geom_col(show.legend = FALSE) +
        labs(x = NULL, y = "top Blogs words frequency") +
        coord_flip()
```

```{r}
News_graph <- tf %>%
        filter(id == "News") %>%
        mutate(word = factor(word, levels = rev(unique(word)))) %>%
        top_n(12) %>%
        ggplot(aes(word, tf_idf, fill = word)) +
        geom_col(show.legend = FALSE) +
        labs(x = NULL, y = "top News words frequency") +
        coord_flip()
```

```{r}
Twitter_graph <- tf %>%
        filter(id == "Twitter") %>%
        mutate(word = factor(word, levels = rev(unique(word)))) %>%
        top_n(12) %>%
        ggplot(aes(word, tf_idf, fill = word)) +
        geom_col(show.legend = FALSE) +
        labs(x = NULL, y = "top Twitter words frequency") +
        coord_flip()
```

```{r}
grid.arrange(Twitter_graph, News_graph, Blogs_graph, ncol = 3)
```
The three plots are aggredated together to make comparison easier. These can be considered the most "important" words or words that are characteristic for one dataset (document) within a collection of datasets (documents): We can see similar words used in Twitter and Blogs. 
`tf-idf` allows us to see how different words are important in documents within a collection or corpus of documents.

===============================================================================
## 2. What are the frequencies of 2-grams and 3-grams in the dataset?
*n-grams are consecutive sequences of words*

We've covered words as individual units and considered their frequencies to visualize which were the most common words in the three data sets. Next step is to build figures and tables to understand variation in the frequencies of words and word pairs in the data.

```{r}
BiGrams <- cleanAll %>%
        # Tokenization into bigrams 
        unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% # separate each line of text into 2-grams 
        count(bigram, sort = TRUE)

head(BiGrams)
```

```{r}
TriGrams <- cleanAll %>%
        # Tokenization into bigrams 
        unnest_tokens(trigram, text, token = "ngrams", n = 3) %>% # separate each line of text into 2-grams 
        count(trigram, sort = TRUE)

head(TriGrams)
```

```{r}
# below I separate the data into two columns in order to be able to use filters of stop words and strange words

BiGrams_sep <- BiGrams %>%
        separate(bigram, c("word1", "word2"), sep = " ") %>%
        filter(!word1 %in% stop_words$word) %>%
        filter(!word2 %in% stop_words$word) %>% 
        count(word1, word2, sort = TRUE)

BiGrams_sep
```

```{r}
# below I separate the data into two columns in order to be able to use filters of stop words and strange words

TriGrams_sep <- TriGrams %>%
        separate(bigram, c("word1", "word2", "word3"), sep = " ") %>%
        filter(!word1 %in% stop_words$word) %>%
        filter(!word2 %in% stop_words$word) %>%
        filter(!word3 %in% stop_words$word) %>%
        count(word1, word2, word3, sort = TRUE)

TriGrams_sep
```
===============================================================================

## 3. How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%?

```{r}
corpus_words <- left_join(IDCounts, TotalWords)
corpus_words
```

`n` is the number of times that the word is used in each data set (Twitter, Blogs, News). To look at the distribution of `n/total` for each data set we use the number of times a word appears divided by the total number of terms (words) in that set, which corresponds to the term frequency.


```{r}
ggplot(corpus_words, aes(n/total, fill = id)) +  
        geom_histogram(show.legend = FALSE) + 
        xlim(NA, 0.000009) + 
        facet_wrap(~id, ncol = 3, scales = "free_y")
```

The plots exhibit similar distributions for the three data sets. There are many words that occur rarely and fewer that occur frequently. Zip's law states that the frequency that a word appears is inversely proportional to its rank.

```{r}
All_words <- WordToken %>%
        #filter(!str_detect(word, "\\b[aeiou]{2,}\\b")) %>%
        count(word, sort = TRUE) %>%
        mutate(word = factor(word, levels = rev(unique(word))), 
               prop = n/sum(n), #create variable with the proportion of word frequency in the text corpus
               row = row_number(), #create variable with to easily see how many words correspond to the previous calculation
               cumprop = cumsum(prop)) %>% #calculate the cumulative sum of the proportions 
        arrange(desc(cumprop)) #order proportions by descent order 

All_words
```

How many words correspond to 50% of all word instancies/observations?
```{r}
Below50 <- All_words %>%
        filter(cumprop <= .501) %>% #filter 50% of the observations

Below50 
```

How many words correspond to 90% of all observations?
```{r}
Below90 <- All_words %>%
        filter(cumprop <= .901) #filter 90% of the observations 
Below90
```

We can observe that 1638 unique words cover 50% of all the word instances, while 90% covers 20520 word instances. In a frequency sorted disctionary 50% should be enough to cover all word instances. This is also shown in the histograms above, where the plots present a long tail, with half of the observations skewed. 

===============================================================================
## 4. How do you evaluate how many of the words come from foreign languages?


I would use an english dictionary or a list of english words and match with each sentence/word in the text corpus. For this the hunspell package could be useful to detect words that would not match with the list. These words would be considered foreign words or typed not accordingly. I would keep those in the text still for future predictions with bi-grams and tri-grams, meaning that when more than one non-english word occurs there can be a chance that the next words predicted are non-english words as well. 

===============================================================================

## 5. Can you think of a way to increase the coverage - identifying words that may not be in the corpora or using a smaller number of words in the dictionary to cover the same number of phrases?**

*  Use `tm` package for stemming, to reduce complexity of words and match with language extensions that come with packages such as `Rstem`. 
* Generate term-document matrices 
* Import/use a online dictionary and match words with 50% of the corpus word instancies
* Use a Markov chain as a measure to save memory and randomly predict n-grams.

===============================================================================

*Interesting facts:* 

* Calculation of word frequencies without the text cleaning, reveals higher frequency of strange characters and words that are a repetition of vowels. For this matter, `cleanAll` represents a version of the data without these original elements.  

* Although stopwords are removed at this part of the project as they don't add value to the calculation of word frequencies (tf_idf). These words should be included in the modelling part as they are connectors in the language, thus needed in an app like swiftkey.

* The distribution of word frequencies is skewed and the result for most common words is in accordance to Zip's law. For this reason term frequencies are considered along with inverse document frequencies. For more information check document 'Word_frequencies_exploratory_analysis'