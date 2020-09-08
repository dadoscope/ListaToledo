library(rtweet)
library(tidyverse)
library(tidytext)
library(wordcloud)
library(purrr)
library(stringr)
library(widyr)
library(igraph)
library(ggraph)

rtweet::sto


list_members<- rtweet::lists_members("1234500090838036480")

user_names<- 
  list_members%>%
  select(screen_name)


id_members<- 
  list_members%>%
  select(user_id)
  

saveRDS(id_members, "user_id.rds")
saveRDS(user_names, "user_names.rds")

location_members<- list_members$location


analise_mensagem <- tibble(text=location_members) %>%
  unnest_tokens(palavra,text,to_lower = TRUE) %>%
  count(palavra, sort = TRUE) %>%
  ungroup()

stop_words_grupo<- c(stopwords::stopwords("pt"),stopwords::stopwords("en"))

set.seed(1972)
analise_mensagem %>%
  anti_join(data_frame(palavra = stop_words_grupo)) %>%
  group_by(palavra)%>%
  summarise(
    n = sum(n)
  ) %>%
  with(wordcloud(palavra,n,max.words = 97, colors=brewer.pal(6,"Dark2"),random.order=FALSE))

analise_mensagem<- tibble(text=list_members$description) %>%
  unnest_tokens(palavra,text,to_lower = TRUE) %>%
  count(palavra, sort = TRUE) %>%
  ungroup()


stop_words_grupo<- c(stopwords::stopwords("pt"),stopwords::stopwords("en"),c("t.co","https"))

set.seed(1972)
analise_mensagem %>%
  anti_join(data_frame(palavra = stop_words_grupo)) %>%
  group_by(palavra)%>%
  summarise(
    n = sum(n)
  ) %>%
  with(wordcloud(palavra,n,max.words = 97, colors=brewer.pal(6,"Dark2"),random.order=FALSE))

analise_mensagem %>%
  anti_join(data_frame(palavra = stop_words_grupo)) %>%
  top_n(20, n) %>%
  mutate(palavra = reorder(palavra, n)) %>%
  ggplot() +
  geom_col(aes(x=palavra, y=n), fill = "#b5403e") +
  theme_light() +
  coord_flip()

list_members %>%
  #mutate(seguidores = "Seguidores") %>%
  ggplot() +
  geom_histogram(aes(x=followers_count), fill = "#b5403e", color = "black")+
  #geom_jitter(aes(x=seguidores, y= followers_count), color = "#b5403e") +
  theme_light()+
  scale_x_log10(labels=function(x) format(x, big.mark = ".", scientific = FALSE))+
  labs(
    #title = paste("Cumulative deaths by COVID:", max(df_cross_covid_others$obitosAcumulados)),
    y = "Contagem de membros da lista",
    x="Número de seguidores"
    
    
  )+
  theme(
    panel.grid = element_blank(),
    #axis.text.x   = element_blank()
  )

list_members %>%
  select(name, followers_count) %>%
  top_n(20, followers_count) %>%
  mutate(name= reorder(name, followers_count)) %>%
  ggplot() +
  geom_col(aes(x=name, y= followers_count ),fill = "#b5403e")+
  theme_light()+
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))+
  labs(
    #title = paste("Cumulative deaths by COVID:", max(df_cross_covid_others$obitosAcumulados)),
    y = "Número de seguidores",
    x=""
  )+
  theme(
    panel.grid = element_blank(),
    #axis.text.x   = element_blank()
  ) +
  coord_flip()

  
list_members %>%
  #mutate(seguidores = "Seguidores") %>%
  ggplot() +
  geom_histogram(aes(x=statuses_count), fill = "#b5403e", color = "black")+
  #geom_jitter(aes(x=seguidores, y= followers_count), color = "#b5403e") +
  theme_light()+
  scale_x_log10(labels=function(x) format(x, big.mark = ".", scientific = FALSE))+
  labs(
    #title = paste("Cumulative deaths by COVID:", max(df_cross_covid_others$obitosAcumulados)),
    y = "Contagem de membros da lista",
    x="Número de postagens"
    
    
  )+
  theme(
    panel.grid = element_blank(),
    #axis.text.x   = element_blank()
  )

list_members %>%
  select(name, statuses_count) %>%
  top_n(20, statuses_count) %>%
  mutate(name= reorder(name, statuses_count)) %>%
  ggplot() +
  geom_col(aes(x=name, y= statuses_count ),fill = "#b5403e")+
  theme_light()+
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))+
  labs(
    #title = paste("Cumulative deaths by COVID:", max(df_cross_covid_others$obitosAcumulados)),
    y = "Número de postagens",
    x=""
  )+
  theme(
    panel.grid = element_blank(),
    #axis.text.x   = element_blank()
  ) +
  coord_flip()


member_posts<-
  map_dfr(list_members$user_id, function(a_id){
    print(a_id)
    rtweet::get_timeline(a_id,n=3200)
  })
  
  



df_br_covid<-
  member_posts %>%
  mutate(text = str_to_lower(text)) %>%
  filter((str_detect(text,"brazil") | str_detect(text,"brasil") |  str_detect(text,"brésil")),
         str_detect(text,"covid")) 

######Textos em inglês


texto<-
member_posts %>%
  mutate(text = str_to_lower(text)) %>%
  filter((str_detect(text,"brazil")),
         str_detect(text,"covid")) %>%
  select(text)
  
  
analise_mensagem <- tibble(text=texto$text) %>%
  unnest_tokens(palavra,text,to_lower = TRUE) %>%
  count(palavra, sort = TRUE) %>%
  ungroup()

stop_words_grupo<- 
  (rtweet::stopwordslangs%>%
  filter(lang %in% c("pt","en")) %>%
  select(word))$word 

stop_words_grupo<- c(stop_words_grupo, c("brazi","brasil","covid","19","Cc","covid19","brazil’s", "brazil's", "	brazilians",  "brazilian", "https","t.co")) #

set.seed(1972)
analise_mensagem %>%
  anti_join(data_frame(palavra = stop_words_grupo)) %>%
  group_by(palavra)%>%
  summarise(
    n = sum(n)
  ) %>%
  with(wordcloud(palavra,n,max.words = 97, colors=brewer.pal(6,"Dark2"),random.order=FALSE))


stop_words_net<- c(stop_words_grupo, c("19", "https","t.co")) #"brazi","brasil","covid","19","Cc","covid19","brazil’s", "brazil's", "	brazilians",  "brazilian",

analise_twitter_secoes <- tibble(texto= texto$text)%>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, texto) %>%
  filter(!word %in% stop_words_net)

word_cors <- analise_twitter_secoes %>%
  group_by(word) %>%
  filter(n() >= 5) %>%
  pairwise_cor(word, section, sort = TRUE)

set.seed(2016)

word_cors %>%
  filter(correlation > .35) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "mds") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 3) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

#####Textos em português


texto<-
  member_posts %>%
  mutate(text = str_to_lower(text)) %>%
  filter((str_detect(text,"brasil")),
         str_detect(text,"covid")) %>%
  select(text)


analise_mensagem <- tibble(text=texto$text) %>%
  unnest_tokens(palavra,text,to_lower = TRUE) %>%
  count(palavra, sort = TRUE) %>%
  ungroup()

stop_words_grupo<- 
  (rtweet::stopwordslangs%>%
     filter(lang %in% c("pt","en")) %>%
     select(word))$word 

stop_words_grupo<- c(stop_words_grupo, c("brazi","brasil","covid","19","Cc","covid19","brazil’s", "brazil's", "	brazilians",  "brazilian", "https","t.co")) #

set.seed(1972)
analise_mensagem %>%
  anti_join(data_frame(palavra = stop_words_grupo)) %>%
  group_by(palavra)%>%
  summarise(
    n = sum(n)
  ) %>%
  with(wordcloud(palavra,n,max.words = 97, colors=brewer.pal(6,"Dark2"),random.order=FALSE))


stop_words_net<- c(stop_words_grupo, c("19", "https","t.co")) #"brazi","brasil","covid","19","Cc","covid19","brazil’s", "brazil's", "	brazilians",  "brazilian",

analise_twitter_secoes <- tibble(texto= texto$text)%>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, texto) %>%
  filter(!word %in% stop_words_net)

word_cors <- analise_twitter_secoes %>%
  group_by(word) %>%
  filter(n() >= 3) %>%
  pairwise_cor(word, section, sort = TRUE)

set.seed(2016)

word_cors %>%
  filter(correlation > .70) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "mds") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 3) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()



glimpse(member_posts)

saveRDS(df_br_covid, "df_br_covid.rds")
