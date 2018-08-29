#Code to extract and put order in text and table contents to be used in the VISA html´s
#May 2018
#Adriana Villamor



library(flextable)
library(officer)
library(dplyr)
library(tidyr)
library(ReporteRs)
library(plotly)
library(grid)
get_filelist <- function(year = 2017) {
  
  # Note: You must log in to SharePoint and have this drive mapped
  sharePoint <- "//community.ices.dk/DavWWWRoot/"
  
  if(!dir.exists(sprintf("%sAdvice/Advice%s/", sharePoint, year))) {
    stop("Note: You must be on the ICES network and have sharepoint mapped to a local drive.")
  }
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  ## Download and prepare the stock information data ##
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  
  rawsd <- jsonlite::fromJSON("http://sd.ices.dk/services/odata3/StockListDWs3")$value %>% 
    filter(ActiveYear == year,
           YearOfNextAssessment == year + 1) %>% ## This should be fixed when ActiveYear is updated in SID 
    mutate(CaptionName = gsub("\\s*\\([^\\)]+\\)", "", as.character(StockKeyDescription)),
           AdviceReleaseDate = as.Date(AdviceReleaseDate, format = "%d/%m/%y"),
           PubDate = format(AdviceReleaseDate, "%e %B %Y"),
           ExpertURL = paste0("http://www.ices.dk/community/groups/Pages/", ExpertGroup, ".aspx"),
           DataCategory = gsub("\\..*$", "", DataCategory))
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  ## Find most recent released advice on SharePoint ##
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  
  advice_file_finder <- function(year){
    
    advice <- list.files(sprintf("%sAdvice/Advice%s/", sharePoint, year))
    
    if(year == 2015){
      folderNames = c("BalticSea", "BarentsSea", "BayOfBiscay", 
                      "CelticSea", "FaroePlateau", "Iceland",
                      "NASalmon","NorthSea", "Widely")
    }
    if(year == 2016){
      folderNames = c("BalticSea", "BarentsSea", "Biscay", 
                      "CelticSea", "Faroes", "Iceland",
                      "NorthSea", "Salmon", "Widely")
    }
    if(year == 2017){
      folderNames = c("BalticSea", "BarentsSea", "BayOfBiscay", 
                      "CelticSea", "Faroes", "Iceland",
                      "NorthSea", "Salmon", "Widely")
    }
    
    ### Hopefully, future folderNames are consistent, if not, map by hand, as above.
    adviceList <- lapply(advice[advice %in% folderNames],
                         function(x) list.files(sprintf("%sAdvice/Advice%s/%s/Released_Advice", sharePoint, year, x)))
    
    names(adviceList) <- folderNames
    fileList <- do.call("rbind", lapply(adviceList,
                                        data.frame, 
                                        stringsAsFactors = FALSE))
    colnames(fileList) <- "StockCode"
    fileList$filepath <- sprintf("%sAdvice/Advice%s/%s/Released_Advice/%s", 
                                 sharePoint,
                                 year,
                                 gsub("\\..*", "", row.names(fileList)),
                                 fileList$StockCode)
    fileList$StockCode <- tolower(gsub("\\.docx*", "", fileList$StockCode))
    
    return(fileList)
  }
  
  fileList <- bind_rows(
    rawsd %>%
      filter(YearOfLastAssessment == 2015) %>% 
      left_join(advice_file_finder(2015), by = c("PreviousStockKeyLabel" = "StockCode")),
    rawsd %>%  
      filter(YearOfLastAssessment == 2016) %>% 
      left_join(advice_file_finder(2016), by = c("PreviousStockKeyLabel" = "StockCode")),
    rawsd %>%
      filter(YearOfLastAssessment == 2017) %>% 
      left_join(advice_file_finder(2017), by = c("StockKeyLabel" = "StockCode"))
    ## Repeat for additional years
  ) %>% 
    mutate(URL = ifelse(is.na(filepath),
                        NA,
                        paste0(gsub("//community.ices.dk/DavWWWRoot/", 
                                    "https://community.ices.dk/", 
                                    filepath), 
                               "?Web=1")))
  
  return(fileList)
} # Close get_filelist

fileList <- get_filelist(2017)

nipag<- fileList %>% filter(ExpertGroup == "NIPAG") 
  
stock_name <- "pra.27.1-2"
  
  ## Start function here by grabbing info for a stock
  stock_sd <- fileList %>% 
    filter(StockKeyLabel == stock_name) 
  
  ## Identify the file 
  fileName <- stock_sd %>% 
    pull(filepath)
  
  ## Grab the last advice
  doc <- officer::read_docx(fileName)
  

  ## Pull out a data.frame of text (content[]) and tables (both tabs[] and content[])
  content <- officer::docx_summary(doc)
  
  text <- content %>% filter(content_type == "paragraph")

  text <- text()%>% 
    mutate (style_name = case_when(text %in% c("ICES stock advice",
  "Stock development over time",
  "Stock and exploitation status",
  "Catch options",
  "Basis of the advice",
  "Quality of the assessment",
  "Issues relevant for the advice",
  "Reference points",
  "Basis of the assessment",
  "Information from stakeholders",
  "History of the advice, catch, and management",
  "History of the catch and landings",
  "Sources and references")~"Hheading 3"))
  
  table_header_name <- content %>% 
    filter(content_type %in% "table cell",
           row_id == 1) %>%
    select(-dplyr::one_of("content_type", "style_name", "level", 
                          "num_id", "row_id", "is_header", "col_span", "row_span")) %>% 
    spread(cell_id, text) %>% 
    mutate(header_name = case_when(grepl("^[V-v]ariable", `1`) &
                                     grepl("^[V-v]alue", `2`) ~ "catchoptionsbasis",
                                   grepl("^[I-i]ndex", `1`) ~ "catchoptionsbasis",
                                   grepl("^[B-b]asis", `1`) ~ "catchoptions",
                                   grepl("^[A-a]dvice\\sbasis", `1`) ~ "advicebasis",
                                   grepl("^[D-d]escription", `1`) &
                                     grepl("^[V-v]alue", `2`) ~ "ranges",
                                   grepl("^[F-f]ramework", `1`) ~ "referencepoints",
                                   grepl("^ICES\\sstock\\sdata\\scategory", `1`) ~ "assessmentbasis",
                                   grepl("^[Y-y]ear", `1`) &
                                     grepl("^ICES\\sadvice", `2`) ~ "advice",
                                   grepl("^[C-c]atch", `1`) &
                                     grepl("^[W-w]anted\\s[C-c]atch", `2`) ~ "catchdistribution",
                                   grepl("^[C-c]atch", `1`) &
                                     grepl("^[L-l]andings", `2`) ~ "catchdistribution",
                                   grepl("^[Y-year]", `1`) ~ "assessmentsummary",
                                   TRUE ~ "other"),
           caption_index = doc_index - 1) %>% 
    select(doc_index,
           caption_index,
           header_name)
  
  table_header_name$text <- content$text[content$doc_index %in% table_header_name$caption_index]
  tab_heads <- table_header_name %>% 
    select(doc_index,
           table_name = header_name, 
           text)
  
  tab_names <- tab_heads %>% 
    select(table_name) %>% 
    filter(!table_name %in% c("stocksummary")) %>% 
    distinct(.keep_all = TRUE)
  
  ## Holds all the table information (headers, values, ugly tables... everything)
  table_cells <- content %>%
    filter(content_type %in% "table cell") %>%
    group_by(doc_index) %>%
    mutate(is_header = case_when(row_id == 1 ~ TRUE,
                                 row_id != 1 ~ FALSE),
           table_name = case_when(is_header == TRUE & grepl("^variable$", tolower(text)) ~ "catchoptionsbasis",
                                  is_header == TRUE & grepl("^index\\sa", tolower(text)) ~ "catchoptionsbasis",
                                  is_header == TRUE & grepl("^basis$", tolower(text)) ~ "catchoptions",
                                  is_header == TRUE & grepl("^advice basis$", tolower(text)) ~ "advicebasis",
                                  is_header == TRUE & grepl("^description$", tolower(text)) ~ "ranges",
                                  is_header == TRUE & grepl("^framework$", tolower(text)) ~ "referencepoints",
                                  is_header == TRUE & grepl("^ices stock data category$", tolower(text)) ~ "assessmentbasis",
                                  is_header == TRUE & grepl("^ices advice$", tolower(text)) ~ "advice",
                                  is_header == TRUE & grepl("^catch \\(\\d{4}\\)$", tolower(text)) ~ "catchdistribution",
                                  ### Add additional for Nephrops cat 3+ and other special cases ###
                                  TRUE ~ NA_character_),
           table_name = case_when(is_header == TRUE &  ave(is.na(table_name), doc_index, FUN = all) ~ "REMOVE",
                                  TRUE ~ table_name),
           table_name = ave(table_name, doc_index, FUN = function(x) unique(x[!is.na(x)]))) %>%
    left_join(tab_names, by = c("table_name")) %>% 
    ungroup() %>%
    filter(table_name != "REMOVE") %>%
    select(doc_index, table_name, is_header, row_id, cell_id, text)

  catchoptionsbasis <- table_cells%>% filter(table_name =="catchoptionsbasis")
  catchoptions <- table_cells%>% filter(table_name =="catchoptions")
  advicebasis <- table_cells%>% filter(table_name =="advicebasis")
  referencepoints <- table_cells%>% filter(table_name =="referencepoints")
  assessmentbasis <- table_cells%>% filter(table_name =="assessmentbasis")
  # table_index <- unique(tc$doc_index)[tab_index]
  
 #coming back to normal table shape
  #catchoptionsbasis
   table_body <- catchoptionsbasis %>% 
    filter(!is_header) %>%
    ungroup %>% 
    select(-doc_index) %>%
    spread(cell_id, text) %>% 
    select(-table_name,
           -is_header,
           -row_id)
  table_header <- catchoptionsbasis %>% 
    filter(is_header) %>% 
    ungroup %>% 
    select(-doc_index) %>% 
    spread(cell_id, text) %>% 
    select(-table_name,
           -is_header,
           -row_id)
  
 colnames(table_body) <- table_header[1,]
 catchoptionsbasis <- table_body
 
 #catchoptions
 table_body <- catchoptions %>% 
   filter(!is_header) %>%
   ungroup %>% 
   select(-doc_index) %>%
   spread(cell_id, text) %>% 
   select(-table_name,
          -is_header,
          -row_id)
 table_header <- catchoptions %>% 
   filter(is_header) %>% 
   ungroup %>% 
   select(-doc_index) %>% 
   spread(cell_id, text) %>% 
   select(-table_name,
          -is_header,
          -row_id)
 
 colnames(table_body) <- table_header[1,]
 catchoptions <- table_body  
 #will modify the table by hand for herring, too different!
 #catchoptions <-catchoptions[,-1]
 
 catchoptions<-catchoptions[complete.cases(catchoptions),]
 library(data.table)
 catchoptions <- data.table(catchoptions)
 
 #only for hake
 #this is different also depending on the stock, I should find a rule to translate and homogenize all
 #it is possible
 colnames(catchoptions) <- c("Basis", "Fage26", "Fage01", "Catch", "SSB2018","SSB2019")
 catchoptions$Fage26 <- as.numeric(catchoptions$Fage26)
 catchoptions$Fage01 <- as.numeric(catchoptions$Fage01)
 catchoptions$Catch <- as.numeric(catchoptions$Catch)
 catchoptions$SSB2018 <- as.numeric(catchoptions$SSB2018)
 catchoptions$SSB2019 <- as.numeric(catchoptions$SSB2019)
 library(scales)
 
 
 colnames(catchoptions) <- c("Basis", "Catch", "blu", "bla","F","bli", "ble", "SSB","tras", "tru")
 catchoptions$F <- as.numeric(catchoptions$F)
 catchoptions$Catch <- as.numeric(catchoptions$Catch)
 catchoptions$SSB <- as.numeric(catchoptions$SSB)
 library(scales)
 labels <- catchoptions$Basis
 labels <- labels[-6]
 library(ggplot2)
 
 #make catch a bar and ssb still a line
 p1 <- ggplot(catchoptions, aes(F))+
   geom_rect(xmin = -Inf, ymin = -Inf, xmax = 0.62, ymax = Inf,
             fill = "lightgreen", alpha=0.50)+theme_bw()+
   # geom_rect(xmin = 0.280, ymin = -Inf, xmax = 0.282, ymax = Inf,
   #           fill = "gold", alpha=0.50)+theme_bw()+
   geom_rect(xmin = 0.62, ymin = -Inf, xmax = 0.87, ymax = Inf,
             fill = "coral")+theme_bw()+
   geom_rect(xmin = 0.87, ymin = -Inf, xmax = Inf, ymax = Inf,
             fill = "brown1")+theme_bw()+
   geom_hline(yintercept=45000, linetype="dashed", color = "red")+
   geom_hline(yintercept=32000, linetype="dashed", color = "black")+
   geom_vline(xintercept = 0.28, 
              color = "yellow", size=3)+
   geom_vline(xintercept = 0.62, linetype="dotted", 
              color = "blue", size=0.5)+
   geom_vline(xintercept = 0.87, linetype="dotted", 
              color = "blue", size=0.5)+
   # geom_col(aes(y = Catch), width = 0.15)+
   geom_point(aes(y = Catch),size = 2, colour= "Red") +
   geom_line(aes(y = Catch),size = 0.5, colour= "Red")+
   geom_point(aes(y=SSB), size = 2, colour= "Blue")+
   geom_line(aes(y = SSB),size = 0.5, colour= "Blue")+
   scale_y_continuous("tonnes",sec.axis = sec_axis(~., name = "SSB"))+
   scale_x_continuous(breaks = unique(catchoptions$F), labels = labels)+
   xlab("Catch scenarios for 2018")+
   ylab("tonnes")
 p2 <- p1 + theme(axis.text.x = element_text(face="bold", color="Black",
                            size=10, angle=45))+
   theme(legend.text = element_text(colour="blue", size = 16, face = "bold"))
 # p3<- p2+ guide_colorbar(title = "Spawning Stock Biomass", label= TRUE,barheight = )
# p3<- p2 + geom_hline(yintercept=45000, linetype="dashed", color = "red")+
#   geom_hline(yintercept=32000, linetype="dashed", color = "black")+
#   geom_vline(xintercept = 0.28, 
#                 color = "yellow", size=3)+
#   geom_vline(xintercept = 0.62, linetype="dotted", 
#                   color = "blue", size=0.5)+
#   geom_vline(xintercept = 0.87, linetype="dotted", 
#                 color = "blue", size=0.5)
 
 

ggplotly(p2)

#add legend, move labels to the right place, highlight basis of advice
pp<-ggplotly(p2)%>%
  layout(hovermode="FALSE", #showlegend = FALSE,
       shapes = list(
         list(type = "rect", fillcolor = "green", opacity = 0.2, 
              line = list(color = "green", opacity=0.2), x0 = min(sbl$Year), x1 = max(sbl$Year)+1, 
              xref = "Year", y0 = Bpa, y1 = max(ssb$value, na.rm=TRUE)*1.05, yref = "value"),

 
pp <- style(pp, line = list(color = 'gold'), hoverinfo = "y", traces = 1)
 
pp

#reference points
 table_body <- referencepoints %>% 
   filter(!is_header) %>%
   ungroup %>% 
   select(-doc_index) %>%
   spread(cell_id, text) %>% 
   select(-table_name,
          -is_header,
          -row_id)
 table_header <- referencepoints %>% 
   filter(is_header) %>% 
   ungroup %>% 
   select(-doc_index) %>% 
   spread(cell_id, text) %>% 
   select(-table_name,
          -is_header,
          -row_id)
 colnames(table_body) <- table_header[1,]
 referencepoints <- table_body  
 
 referencepoints$Value<-gsub("t"," ",referencepoints$Value)
 # referencepoints$Value <-as.numeric(referencepoints$Value)
 
 
 #for herring:
 p1 <- ggplot(catchoptions, aes(Fage26))+geom_point(aes(y = Catch),size = 2, colour= "Red") + 
   geom_line(aes(y = Catch),size = 0.5, colour= "Red")+
   geom_point(aes(y=SSB2018), size = 2, colour= "Blue")+
   geom_line(aes(y = SSB2018),size = 0.5, colour= "Blue")+
   scale_y_continuous("Catch",sec.axis = sec_axis(~., name = "SSB"))+
   scale_x_continuous(breaks = unique(catchoptions$Fage26), labels = labels)
 p2 <- p1 + theme(axis.text.x = element_text(face="bold", color="Black",
                                             size=10, angle=45))
                            , 
                            margin(b=10)))
 # +
 #                    theme(axis.text = element_text(margin(b=10)))
p2

 
   
########
#for herring with several fleets and ages, 
 
 ggplotly(p2)
 # scale_x_continuous(breaks = F, labels = )) 
   # scale_x_discrete(limits = labels)
 
 
   
 p2 <- p1 + scale_x_continuous(labels= labels)
 p2  
 
 # p1<-ggplot(catchoptions, aes(x=F,y=SSB,group=1))+geom_point()+geom_line()
 # p2<-ggplot(catchoptions, aes(x=F,y=Catch,group=1))+geom_point()+geom_line()
 


 
 #advicebasis
 table_body <- advicebasis %>% 
   filter(!is_header) %>%
   ungroup %>% 
   select(-doc_index) %>%
   spread(cell_id, text) %>% 
   select(-table_name,
          -is_header,
          -row_id)
 table_header <- advicebasis %>% 
   filter(is_header) %>% 
   ungroup %>% 
   select(-doc_index) %>% 
   spread(cell_id, text) %>% 
   select(-table_name,
          -is_header,
          -row_id)
 
 colnames(table_body) <- table_header[1,]
 advicebasis <- table_body  
 
 #referencepoints
 table_body <- referencepoints %>% 
   filter(!is_header) %>%
   ungroup %>% 
   select(-doc_index) %>%
   spread(cell_id, text) %>% 
   select(-table_name,
          -is_header,
          -row_id)
 table_header <- referencepoints %>% 
   filter(is_header) %>% 
   ungroup %>% 
   select(-doc_index) %>% 
   spread(cell_id, text) %>% 
   select(-table_name,
          -is_header,
          -row_id)
 
 colnames(table_body) <- table_header[1,]
 referencepoints <- table_body
 
 #assessmentbasis

 table_body <- assessmentbasis %>% 
   filter(!is_header) %>%
   ungroup %>% 
   select(-doc_index) %>%
   spread(cell_id, text) %>% 
   select(-table_name,
          -is_header,
          -row_id)
 table_header <- assessmentbasis %>% 
   filter(is_header) %>% 
   ungroup %>% 
   select(-doc_index) %>% 
   spread(cell_id, text) %>% 
   select(-table_name,
          -is_header,
          -row_id)
 
 colnames(table_body) <- table_header[1,]
 assessmentbasis <- table_body
   
write.csv(catchoptionsbasis, file = "catchoptionsbasis.csv")
catchoptions[is.na(catchoptions)] <- ""
write.csv(catchoptions, file = "catchoptions.csv")
write.csv(advicebasis, file = "advicebasis.csv")
write.csv(referencepoints, file = "referencepoints.csv")
write.csv(assessmentbasis, file = "assessmentbasis.csv")

    