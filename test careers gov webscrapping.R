rm(list=ls())
library(magrittr)
library(rvest)
library(tidyr)


url<- print("test data.html")

move <- read_html(url)

job_titles<-move %>%
  html_nodes("a.job-link")%>%
  html_text() %>%                      
  gsub("^\\s+|\\s+$", "", .)

links<-move %>%
  html_nodes("a.job-link")%>%
  html_attr('href')%>%                     
  gsub("^\\s+|\\s+$", "", .)

job_content<-move %>%
  html_nodes("tr")%>%
  html_text() %>%                      
  gsub("^\\s+|\\s+$|\\n", "", .)%>%
  gsub("                    ","@@",.)

job_content<-job_content[-1]
job_content<-job_content[-21]
job_content_df<-data.frame(job_content)
job_content_df2<-separate(job_content_df,1,c("Position","Agency","Job Category","Closes"),sep="@@")

job_details <- function(job_page){
  
  sample_job<-read_html(job_page)
  
  sample_job_desc<-sample_job %>%
    html_nodes("div#job-details")%>%
    html_text() %>%                      
    gsub("^\\s+|\\s+$|\\n", "", .)
  if (identical(sample_job_desc,character(0))){
    sample_job_desc<-"NA"
  }
  return(sample_job_desc)
}

job_descriptions<-{}

for (i in 1:length(links)){
  job_descriptions[i]<-job_details(paste0(links[i]))
}

results <- data.frame(job_titles,job_content_df2,job_descriptions,links)

write.csv(results, "careers_gov_data.csv")



