library(plyr)
library(dplyr)
#devtools::install_github("bmschmidt/wordVectors")
library(tidytext)
library(wordVectors)
library(googledrive)
library(wordVectors)

getfile<-download.file('https://conceptnet.s3.amazonaws.com/downloads/2017/numberbatch/numberbatch-en-17.06.txt.gz',"numberbatch-en-17.06.txt.gz")
nbatch<-wordVectors::read.vectors("numberbatch-en-17.06.txt.gz",binary=F)

#READ IN FILES
files<-c(list.files(path="Documents/GitHub/Yuba/concept.codes/Micro",full.names = T),list.files(path="Documents/GitHub/Yuba/concept.codes/Macro",full.names=T))
files<-files[-stringr::str_which(files,"_")]

#MAKE CONCEPTS
reses<-c("Security","Crisis management","Preparedness","Risk management")
resilience<-paste("https://en.wikipedia.org/wiki/",reses,sep="")
mlist<-list(closest_to(nbatch,~"security"+"resilient"+"budget"+"harden"+"prevent",25),
            closest_to(nbatch,~"crisis"+"management"+"respond"+"contain"+"recover"+"resilient"+"budget",25),
            closest_to(nbatch,~"prepare"+"plan"+"forethought"+"resilient"+"budget",25),
            closest_to(nbatch,~"risk_management"+"identification"+"assess"+"approaches"+"plan"+"resilient"+"budget",25))
mlist<-lapply(mlist,function(X) {
  colnames(X)[2]<-"cos"
  X})

#REJECT OTHER CONCEPTS
nmods<-lapply(1:length(mlist),function(X) nbatch[[mlist[[X]]$word]] %>% reject(nbatch[[sapply(mlist[-X],function(K) K$word)]]))
names(nmods)<-reses

#Make valid figure

#REJECT OTHER CONCEPTS
#nmods<-list(mlist[[1]] %>% reject(mlist[[2]]),mlist[[2]] %>% reject(mlist[[1]]))

closest<-lapply(nmods,function(X) closest_to(nbatch,X,n=25))
wordlist<-  sapply(closest,function(X) X$word)
names(wordlist)<-names(nmods)
wordlist<-reshape2::melt(wordlist)
wordframe<-lapply(wordlist$value,function(X){
  data.frame("word"=X,"Security"=wordVectors::cosineSimilarity(nmods[[1]],nbatch[[X]]),
             "Crisis management"=wordVectors::cosineSimilarity(nmods[[2]],nbatch[[X]]),
             "Preparedness"=wordVectors::cosineSimilarity(nmods[[3]],nbatch[[X]]),
             "Risk management"=wordVectors::cosineSimilarity(nmods[[4]],nbatch[[X]]))}) %>% bind_rows()

wordlist<-wordlist %>% rename(word=value)
wordtable<-left_join(wordlist,wordframe)
head(wordtable)
wordtable2<-reshape2::melt(wordtable,id=c("Var1","Var2","word"))

fig1<-ggplot(wordtable2)+geom_point(aes(x=value,y=word,colour=variable))+facet_wrap(~Var2,scale="free_y")+theme_minimal()+ggthemes::scale_color_tableau(name="concept")+xlab("cosine similarity")+ylab("closest 25 words in each concept by cosine similarity")+theme(legend.position="bottom")
fig1
ggsave("Documents/GitHub/Yuba/figures/fig1.tiff",fig1,device="tiff",height=8,width=8,dpi=300)
ggsave("Documents/GitHub/Yuba/figures/fig1.png",fig1,device="png",height=8,width=8,dpi=300)

library(rvest)
reses<-c("Security","Crisis management","Preparedness","Risk management")
#live v. saved
#live
resilience<-paste("http://en.wikipedia.org/wiki/",reses,sep="") %>% URLencode()
hs<-lapply(resilience,function(X) html_text(read_html(X)))
names(hs)<-reses
#saved
#hs<-lapply(list.files("Documents/GitHub/Yuba/wikis",full.names = T),function(X) html_text(read_html(X)))
#names(hs)<-list.files("Documents/GitHub/Yuba/wikis")

pagerank<-lapply(hs,function(X){
  K<-data.frame('text'=X) %>% tidytext::unnest_tokens(word,text)
  K<-K[hunspell::hunspell_check(K$word),]
  temp<-lapply(nmods, function(Q) cosineSimilarity(nbatch[[K]],Q))
  names(temp)<-names(nmods)
  temp
})

pagerank<-pagerank %>% bind_rows()
pagerank<-scale(pagerank)
pagerank<-as.data.frame(pagerank)
pagerank$page<-names(hs)
#pagerank$page<-list.files("Documents/GitHub/Yuba/wikis")
pagerank<-reshape2::melt(pagerank)
pagerank$page<-as.character(pagerank$page)
pagerank$variable<-as.character(pagerank$variable)
pagerank$page<-gsub("\\.mht","",pagerank$page)
head(pagerank)

fa1<-ggplot(pagerank)+geom_tile(aes(x=variable,y=page,fill=value))+geom_label(aes(x=variable,y=page,label=round(value,2)))+ggthemes::scale_fill_continuous_tableau(guide=F)+theme_bw()+xlab("concept from vector")
ggsave("Documents/GitHub/Yuba/figures/a1.png",fa1,height=2,width=8, dpi=300, device="png")
#PICK HERE BY BUDGET OR PARAGRAPH

####FOLDER OUT NAME
filefolderpath<-"Documents/Github/Yuba/concept.codes/outputs_june2021/"
dir.create(filefolderpath)
######BY ENTIRE BUDGET

templist2<-lapply(as.character(files[-207]), function(BIGID) {
  txtf1<-readtext::readtext(BIGID)
  txtf1<-iconv(txtf1, "UTF-8", "UTF-8",sub='')
  customstops<-tibble(word=c("dropthis"),lexicon="CUSTOM")
  txtf1<-data.frame("uniqid"=BIGID,"txt"=as.character(txtf1),stringsAsFactors=FALSE)
  word_counts <- unnest_tokens(txtf1,word,txt,token="words")
  word_counts<-word_counts  %>%  anti_join(bind_rows(stop_words,customstops))
  tf<-data.frame("doc"=BIGID,"cos"=sapply(nmods,function(X) cosineSimilarity(X,nbatch[[word_counts$word]])))
  saveRDS(tf,file.path(filefolderpath, gsub("/",".",BIGID) %>% gsub(".txt",".rds",.)))
  tf
})

macros<-files[stringr::str_which(files,".txt")][stringr::str_which(files[stringr::str_which(files,".txt")],"Macro")]

# with no paragraphs(budgetonly)
templist3<-bind_rows(templist2)
templist3$concept<-rep(reses,nrow(templist3)/length(reses))
saveRDS(templist3,"Documents/GitHub/Yuba/concept.codes/concept_result_jpart.rds")

