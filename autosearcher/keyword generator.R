i1<-c('cereal',
      'whole grains',
      'refined grains',
      'milk OR yoghurt OR cheese OR cream OR dairy',
      'fruit',
      'vegetables',
      'processed vegetables',
      'potato OR rice OR grain OR noodles OR pastry',
      'egg',
      'meat',
      'processed meat',
      'soup OR bouillon OR broth OR consomme OR gravy',
      '(sugar sweetened beverages)',
      'coffee OR tea',
      'alcohol OR (alcoholic AND drink)',
      'legumes OR beans',
      'fish OR (sea AND products)',
      'nuts',
      'beans OR legumes',
      'mushroom',
      '(added sugar) OR (refined sugar)'
      )
i2<-c(#'Myocardial Infarction','stroke','hypertension',
      'hyperuricemia OR (uric acid) OR urate')
#i2_append<-c("insulin resistance","hyperglycemia","obesity OR overweight","gout","pcos","dyslipidemia","metabolic syndrome")
i3<-c('"cross sectional" OR "cross-sectional"','"case-control"','cohort','trial','meta-analysis','meta AND "systematic review"')

#function:combine conditions to keywords
kwdgen<-function(riskFactor,Disease,StudyDesign,output){
  keyword<-''
  generatedURLs<-''
  for (i in riskFactor) {
    for (j in Disease) {
      for (k in StudyDesign) {
        keyword<-c(keyword,paste('(',i,') AND (',j,') AND (',k,')',sep=''))
        generatedURLs<-c(generatedURLs,paste('http://www.ncbi.nlm.nih.gov/pubmed/?term=','(',i,') AND (',j,') AND (',k,')',sep=''))
      }
    }
  }
  generatedURLs<-generatedURLs[-1]
  if (output=="link") {return(generatedURLs)}
  if (output=="keyword") {return(keyword)}
}

links<-kwdgen(i1,i2,i3,"link")
links

#function: keywords->quantity of results
extractnum<-function(urlin){
  tgtpage<-readLines(urlin)
  linenum<-grep('Items: ',tgtpage)
  clean1num<-gsub(pattern = '.*value=\"*',replacement = '',tgtpage[linenum],ignore.case = F)
  clean2num<-gsub(pattern = '\" /><input name=\"En.*',replacement = '',clean1num,ignore.case = F)
  return(clean2num)
}


#function: execute batch search
batchsearch<-function(linksarray) {
  resnum<-linksarray
  keywordused<-linksarray
  countcycle<-0
  for (uin in linksarray) {
    countcycle<-countcycle+1
    print(paste('now working on link: ',countcycle,'/ ',length(linksarray),sep = ''))
    keywordused[countcycle]<-gsub('.*term=','',uin)
    res<-extractnum(uin)
    if (length(res)!=0) {
      resnum[countcycle]<-res  
    }
  }
  resnum<-as.numeric(resnum)
  fullresult<-data.frame(keywordused,resnum)
  sorted<-fullresult[order(fullresult[,2],decreasing = T),]
  return(fullresult)
}

#carry out the batch search
result<-batchsearch(links)


links_append<-kwdgen(i1,i2_append,i3,"link")
result_append300<-batchsearch(links_append[1:300])
result_append600<-batchsearch(links_append[301:600])
result_append900<-batchsearch(links_append[601:924])
finalresultappend<-rbind(result_append300,result_append600,result_append900)


exporttable<-result
write.table(exporttable,file = 'D:/Desktop/output.csv',row.names = F,col.names = F,na='',sep = ',')

extractnum('https://www.ncbi.nlm.nih.gov/pubmed/?term=(cereal)+AND+(Myocardial+Infarction)+AND+(Europe)+AND+(cohort+OR+trial)')


nonzero<-subset(fullresult,fullresult$resnum!=0)
zero<-subset(fullresult,fullresult$resnum==0)
keywordforuse<-nonzero$keywordused
zero$keywordused<-gsub('cohort OR','cohort OR (cross AND sectional) OR cross-sectional OR',zero$keywordused)

linksappendtype<-paste('http://www.ncbi.nlm.nih.gov/pubmed/?term=',zero$keywordused,sep = '')

resnum<-links
keywordused<-links
countcycle<-0

for (uin in linksappendtype) {
  countcycle<-countcycle+1
  print(paste('now working on link:',countcycle,'/ ',length(links),sep = ''))
  keywordused[countcycle]<-gsub('.*term=','',uin)
  res<-extractnum(uin)
  if (length(res)!=0) {
    resnum[countcycle]<-res  
  }
}
resnum<-as.numeric(resnum)
fullresult<-data.frame(keywordused,resnum)
sorted<-fullresult[order(fullresult[,2],decreasing = T),]


