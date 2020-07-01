# Library

library(tidyverse)
library(lubridate)
library(quanteda)
library(stringi)
library(stm)
library(quanteda)
library(optiRum) ##conversao logit -> prob

options(stringsAsFactors = FALSE)

### Funcoes uteis


###  DAta

df <- read_rds("data/df_br_covid.rds") %>% 
  mutate(day = round_date(created_at, "day")) %>% 
  mutate(day = (day - min(day))/(24*60*60))


### ajustes com quanteda: 


### ajustes com quanteda

text_corpus <- corpus(df$text, docvars = df)

tweet.dfm <- dfm(text_corpus, 
                 remove_numbers = TRUE, 
                 remove_punct = TRUE, 
                 remove_symbols = TRUE, 
                 remove = stopwords("portuguese"))

tweet.dfm <- dfm_remove(tweet.dfm, stopwords("english"))

# Removendo palavras muito usadas ou pouco usadas

tweet.dfm2 <- dfm_trim(tweet.dfm, 
                       min_docfreq = 0.01, 
                       max_docfreq = 0.90, 
                       docfreq_type = "prop") # min 7.5% / max 95%


# transformando no formato stm

tweet_stm <- convert(tweet.dfm2, to = "stm", 
                     docvars = docvars(text_corpus))

#Procurando numero ótimo de K

{ 
   search <- searchK(tweet_stm$documents, tweet_stm$vocab,
                     K = c(3,4,5,6,7), 
                     data = tweet_stm, cores = 4, prop = 0.40)
  
  
   search$results %>%
     as.tibble() %>%
     ggplot(aes(x = semcoh, y = exclus, col = as.factor(K),
                label = K)) +
     geom_point() +
     geom_label() +
     theme_classic()

  ## k = 5? RElação entre semcoh (coerencia) e exclusividade

}

### stm 

stm_object <- stm(documents = tweet_stm$documents,
                  vocab = tweet_stm$vocab,
                  data = tweet_stm$meta,
                  prevalence = ~retweet_count + (favorite_count) + s(day),
                  K = 5,
                  seed = 12345)

# Validaçao

plot(stm_object, type = "labels", text.cex = 0.6)


# vendo os topicos 

i = 4 #escolha o topico

thoughts <- findThoughts(stm_object, 
                         texts = tweet_stm$meta$text,
                          n = 5, topics = i)$docs[[1]]

formattable::formattable(as.data.frame(thoughts))

### Relação entre os topicos e as variaveis: 

prep <- estimateEffect(1:5 ~ retweet_count + (favorite_count), stm_object,
                       meta = tweet_stm$meta, 
                       uncertainty = "Global")

## Topico ~ Retweet 

aux <- plot(prep, "retweet_count", method= "continuous", 
            stm_object, width = 10)

x <- rep(aux$x, 5)

topic <- aux$means 

names(topic) <- c("Topic 1", "Topic 2", "Topic 3", "Topic 4", "Topic 5")

topic %>% 
  plyr::ldply(., as.data.frame) %>% 
  mutate(rt = x) %>%
  mutate(prob = logit.prob(`X[[i]]`)) %>% 
  ggplot(aes(x = x, y = prob, 
             group = .id, col = .id)) +
  geom_line() +
  theme_classic() +
  ylab("Probilidade") +
  xlab("Retweet")


## Topico ~ Likes 

aux <- plot(prep, "favorite_count", method= "continuous", 
            stm_object, width = 10)

x <- rep(aux$x, 5)

topic <- aux$means 

names(topic) <- c("Topic 1", "Topic 2", "Topic 3", "Topic 4", "Topic 5")

topic %>% 
  plyr::ldply(., as.data.frame) %>% 
  mutate(liked = x) %>%
  mutate(prob = logit.prob(`X[[i]]`))%>% 
  ggplot(aes(x = liked, y = prob, 
             group = .id, col = .id)) +
  geom_line() +
  theme_classic() +
  ylab("Probilidade") +
  xlab("Likes")
