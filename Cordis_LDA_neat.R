#AIHEMALLINNUS AINEISTOLLE "CORDIS - EU RESEARCH PROJECTS UNDER HORIZON 2020"

library(readxl)
library(tidyverse)
library(tidytext)
library(dplyr)
library(topicmodels)
library(textmineR)
library(gridExtra)

#luetaan data, valitaan yhteenveto- ja id-sarakkeet
cordis <- read_excel("~/Downloads/cordis-h2020reports.xlsx")[,c(1,5)]

#muutetaan tidy-formaattiin
text_df <- tibble(line = 1:16144, text=cordis$summary)


#tokenisoidaan (eli erotellaan muotoon 1 sana / rivi), tieto alkuperäisestä rivistä säilyy
text_df <- text_df %>%
  unnest_tokens(word, text)

#siistitään pois liian yleiset sanat
data("stop_words")
text_df <- text_df %>%
  anti_join(stop_words) %>%
  filter(!(word %in% c("research", "european", "based","project","development",
                       "develop","developing", "1","2","3","innovation","process",
                       "time","objective","objectives","potential","system",
                       "systems","main","study","developed","solution","solutions",
                       "innovative","innovations","understanding","understand",
                       "approach","approaches","including","aims","aim","related","major",
                       "existing","real","studies","increasing","specific",
                       "key","provide","providing","impact","challenges","highly",
                       "enable","leading","single","focus","wide","result",
                       "lack","addressed","focused","aspects","include","assess",
                       "include","specifically","significantly","questions",
                       "building","multiple","feasibility","deliver","limited",
                       "wide","existing","importance","important","issues","issue",
                       "leading","support","including","includes","included","address",
                       "addressing","addresses")))

#tutkitaan huvin vuoksi yleisimpiä sanoja, poistetaan myös harvinaisimmat sanat
yleisimmat<-text_df %>%
  count(word,sort=TRUE)

poistettavat <- as.data.frame(yleisimmat$word[22369:80535])
colnames(poistettavat)<-'word'

text_df <- text_df %>%
  anti_join(poistettavat)


#luodaan document-term-matrix
wordcounts <- text_df %>%
  count(line, word)

dtm <- wordcounts %>%
  cast_dtm(line, word, n)

#sovitetaan Latent Dirichlet Allocation -malli
cordis_lda14 <- LDA(dtm, k = 14, control=list(seed=1337))

#visualisoidaan 10-aiheinen malli
cordis_topics <- tidy(cordis_lda, matrix = "beta")
cordis_top_terms <- cordis_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
cordis_top_terms

cordis_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic,scales = "free") +
  coord_flip() +
  scale_x_reordered()+xlab("")+ylab("")


#haetaan mallin aihe-per-sana-todennäköisyydet ("beta")
cordis_topics <- tidy(cordis_lda, matrix = "beta")
View(cordis_topics)

#visualisoidaan aiheiden yleisimmät sanat
library(ggplot2)
cordis_top_terms <- cordis_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
cordis_top_terms

cordis_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic,scales = "free") +
  coord_flip() +
  scale_x_reordered()+xlab("")+ylab("")

#tutkitaan myös gamma-arvoja, jotka kuvaavat, mistä aiheista kunkin 
#dokumentin sanat on luultavimmin generoitu
cordis_gamma <- tidy(cordis_lda, matrix = "gamma")

test<-cordis_gamma %>%
  arrange(document)
test<-test[c(1:10),c(2,3)]
doc1<-ggplot(test, aes(x="", y=gamma, fill=factor(topic))) +
  geom_bar(stat="identity", width=1,color="white") +
  coord_polar("y", start=0)+
  theme_void()

test2<-cordis_gamma %>%
  arrange(document)
test2<-test2[c(72561:72570),c(2,3)]
doc2<-ggplot(test2, aes(x="", y=gamma, fill=factor(topic))) +
  geom_bar(stat="identity", width=1,color="white") +
  coord_polar("y", start=0)+theme_void()

test3<-cordis_gamma %>%
  arrange(document)
test3<-test3[c(139221:139230),c(2,3)]
doc3<-ggplot(test3, aes(x="", y=gamma, fill=factor(topic))) +
  geom_bar(stat="identity", width=1,color="white") +
  coord_polar("y", start=0)+theme_void()

grid.arrange(doc1,doc2,doc3,nrow=2)

betamatrix <- matrix(cordis_topics$beta,nrow=10,byrow = FALSE)
cordis_cluster10 <- hclust(as.dist(distHellinger(betamatrix)))
op<-par(mar=c(4,4,1,2)+0.2)
plot(cordis_cluster10,main = "aihedendrogrammi",ylab = "",xlab="")
title(main="aihedendogrammi")
rm(op)

#lasketaan gamma-arvoista yleisimmät aiheet
gamsums <- rep(0,10)

for (i in 1:10) {
  c <- cordis_gamma[cordis_gamma$topic == i,]
  gamsums[i]<-sum(c$gamma)
}
ardoc<-cordis_gamma %>%
  arrange(document)

paaAiheCounts<-rep(0,10)
for(i in 1:16144) {
  temp<-ardoc[ardoc$document == i,]
  pAiheIndeksi<-which.max(temp$gamma)
  paaAiheCounts[pAiheIndeksi]= paaAiheCounts[pAiheIndeksi]+1
  pAiheIndeksi
}

sum(paaAiheCounts)
yleisyys <- c(rep("p??aiheena",10),rep("aiheena",10))
specie<-c("1","2","3","4","5","6","7","8","9","10")
value<-c(paaAiheCounts,gamsums)

aiheFrame <- data.frame(specie,yleisyys,value)

ggplot(aiheFrame1, aes(fill=yleisyys, y=value, x=specie)) + 
  geom_bar(position="dodge", stat="identity")+
  scale_x_discrete(limits=c("3","10","6","9","1","5","2","8","4","7"),
                   labels=c("social,\neurope,\npolicy","market,\nbusiness,\nproduct","data,\nservices,\ninfrastucture",
                            "cost,\ntechnology,\nmanufacturing","patients,\nclinical,\ncancer","energy,\nwater,\nfood",
                            "cell,\nbrain,\nprotein","data,\ninformation,\nsoftware","materials,\nquantum,\nlight","climate,\nchange,\nspecies"))+
  xlab("aihe")+ylab("dokumenttien m??r?, \nkokonaisgamma")+
  theme_classic()+coord_flip()
