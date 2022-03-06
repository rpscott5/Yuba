library(plyr)
library(dplyr)
devtools::install_github("bmschmidt/wordVectors")
library(tidytext)
library(wordVectors)
library(googledrive)
library(wordVectors)

getfile<-download.file('https://conceptnet.s3.amazonaws.com/downloads/2017/numberbatch/numberbatch-en-17.06.txt.gz',"numberbatch-en-17.06.txt.gz")
nbatch<-wordVectors::read.vectors('../../Volumes/Extreme SSD/csde/general/numberbatch-en-17.06.txt',binary=F)

#READ IN FILES
files<-c(list.files(path="../../Volumes/Extreme SSD/csde/yuba/input/Micro",full.names = T),list.files(path="../../Volumes/Extreme SSD/csde/yuba/input/Macro",full.names=T))
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
ggsave("Documents/GitHub/Yuba/figures/fig1.tiff",fig1,device="tiff",height=8,width=8,dpi=300)
ggsave("Documents/GitHub/Yuba/figures/fig1.png",fig1,device="png",height=8,width=8,dpi=300)

library(rvest)
reses<-c("Security","Crisis management","Preparedness","Risk management")
resilience<-paste("http://en.wikipedia.org/wiki/",reses,sep="") %>% URLencode()
resilience
library(httr)
hs<-lapply(list.files("Documents/GitHub/Yuba/wikis",full.names = T),function(X) html_text(read_html(X)))
names(hs)<-list.files("Documents/GitHub/Yuba/wikis")

pagerank<-lapply(hs,function(X){
  K<-data.frame('text'=X) %>% tidytext::unnest_tokens(word,text)
  K<-K[hunspell::hunspell_check(K$word),]
  temp<-lapply(nmods, function(Q) cosineSimilarity(nbatch[[K]],Q))
  names(temp)<-names(nmods)
  temp
})
pagerank[1] %>% reshape2::melt()
pagerank<-pagerank %>% bind_rows()
pagerank<-scale(pagerank)
pagerank<-as.data.frame(pagerank)
pagerank$page<-list.files("Documents/GitHub/Yuba/wikis")
pagerank<-reshape2::melt(pagerank)
pagerank$page<-as.character(pagerank$page)
pagerank$variable<-as.character(pagerank$variable)
pagerank$page<-gsub("\\.mht","",pagerank$page)
head(pagerank)

fa1<-ggplot(pagerank)+geom_tile(aes(x=variable,y=page,fill=value))+geom_label(aes(x=variable,y=page,label=round(value,2)))+ggthemes::scale_fill_continuous_tableau(guide=F)+theme_bw()+xlab("concept from vector")
ggsave("Documents/GitHub/Yuba/figures/a1.png",fa1,height=2,width=8, dpi=300, device="png")
#PICK HERE BY BUDGET OR PARAGRAPH

####FOLDER OUT NAME
filefolderpath<-"Documents/Github/yuba/outputs_june2021/"
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
reses

# with no paragraphs(budgetonly)
templist3<-bind_rows(templist2)
templist3$concept<-rep(reses,nrow(templist3)/length(reses))
head(templist3)

filter(templist3, stringr::str_detect(doc,"Macro")) 



saveRDS(fins,"templist3_update.paragraphs.march.rds")

saveRDS(templist3,"templist3_update.march.rds")

templist3<-readRDS("../../Volumes/Extreme SSD/csde/general/templist3_update.march.rds")

vectors1<-lapply(reses,function(X) dplyr::filter(templist3,concept==X)$cos)
vectors1<-data.frame(do.call(cbind, vectors1))
colnames(vectors1)<-reses
library(ggplot2)

temp<-data.frame("static"=c(vectors1$Security+vectors1$`Crisis management`),"dynamic"=c(vectors1$Preparedness+vectors1$`Risk management`),"name"=files[-205]) %>% mutate(type=ifelse(stringr::str_detect(name,"Micro"),"micro","macro")) %>% filter(type=="macro") 

library(ggplot2)
ggplot(temp)+geom_point(aes(x=dynamic,y=static))+geom_smooth(aes(x=dynamic,y=static),method="lm")+theme_minimal()+theme(text=element_text(size=20))+xlab("static resilience\n(security+crisis management)")+ylab("dynamic resilience\n (preparedness+risk management)")+geom_text(aes(x=.6,y=.4,label=paste0("R= ",cor(temp$static,temp$dynamic) %>% round(2))),size=14)

fins<-lapply(list.files("outs2",full.names=T),function(X) {X<-readRDS(X)
X$sapply.wlists..function.K..sapply.unique.word_counts.para...function.X...<-c(1:nrow(X)/nrow(X))
X
})

