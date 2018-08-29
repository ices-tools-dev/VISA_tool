#Data preparation for each html file
#Map with the areas of the stock:


library(fisheryO)
ecoregion = "Bay of Biscay and the Iberian Coast Ecoregion"
area_definition(ecoregion)
file_name= "map"
output_path <- "~/"

xmin <- min(sf::st_bbox(eco_areas)[1], sf::st_bbox(ices_areas)[1])
xmax <- max(sf::st_bbox(eco_areas)[3], sf::st_bbox(ices_areas)[3])
ymin <- min(sf::st_bbox(eco_areas)[2], sf::st_bbox(ices_areas)[2])
ymax <- max(sf::st_bbox(eco_areas)[4], sf::st_bbox(ices_areas)[4])

xlims <- c(xmin, xmax)
ylims <- c(ymin, ymax)

stock_areas <- ices_areas %>% filter(Area_27 %in% c("8.c", "9.a"))
centroids <- centroids %>% filter(Area_27 %in% c("8.c", "9.a"))
p1 <- ggplot() +
  geom_sf(data = eco_areas, color = "grey60", fill = "transparent") +
  # geom_sf(data = visahke, color = "grey80", fill = "gold") +
  geom_sf(data = europe_shape, fill = "grey80", color = "grey90") +
  # geom_sf(data = ices_areas, color = "grey60", fill = "transparent") +
  geom_sf(data = stock_areas, color = "grey60", fill = "gold") +
  # geom_sf(data = ices_areas, color = "grey60", fill = "transparent") +
  geom_text(data = centroids, aes(x = X, y = Y, label = Area_27), size = 2.5) +
  #geom_text(data = visahke, aes(x = X, y = Y, label = Area_27), size = 2.5) +
  theme_bw(base_size = 8) +
  theme(plot.caption = element_text(size = 6),
        plot.subtitle = element_text(size = 7)) +
  coord_sf(crs = crs, xlim = xlims, ylim = ylims) 

map <-p1

ggsave(filename = paste0(output_path, file_name, ".png"),
       plot = p1,
       width = 178,
       height = 152,
       units = "mm",
       dpi = 300)

#for SAG plots

library(icesSAG)
refpts <- getSAG("hke.27.3a46-8abd", year= 2017,data = "refpts", combine = T, purpose = "Advice")
summ <- getSAG("hke.27.3a46-8abd", year= 2017,data = "summary", combine = T, purpose = "Advice")
data <- left_join(summ,refpts)

write.csv(data, file = "hkedata.csv")

refpts <- getSAG("her.27.25-2932", year= 2018,data = "refpts", combine = T, purpose = "Advice")
summ <- getSAG("her.27.25-2932", year= 2018,data = "summary", combine = T, purpose = "Advice")
data <- left_join(summ,refpts)

write.csv(data, file = "herdata.csv")

refpts <- getSAG("bli.27.5b67", year= 2018,data = "refpts", combine = T, purpose = "Advice")
summ <- getSAG("bli.27.5b67", year= 2018,data = "summary", combine = T, purpose = "Advice")
data <- left_join(summ,refpts)

write.csv(data, file = "blidata.csv")

#table data

library(flextable)
library(officer)
library(dplyr)
library(tidyr)
library(ReporteRs)
library(plotly)
library(grid)

# Note: You must log in to SharePoint and have this drive mapped
sharePoint <- "//community.ices.dk/DavWWWRoot/"
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
}



stock_name <- "hke.27.3a46-8abd"
stock_name <- "her.27.25-2932"
fileName <- "bli-5b67"

## Grab the last advice
#only for bli, because is 2018 advice
doc <- officer::read_docx("bli-5b67.docx")

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
                                # is_header == TRUE & grepl("^rationale$", tolower(text)) ~ "catchoptions",
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


#for bli, because it has 3 catch scenarios tables!

catchoptions <- table_cells%>% filter(doc_index %in% c(47,54))
catchoptions <- catchoptions[,-(1)]
catchoptions <-unique(catchoptions)





catchoptionsbasis <- table_cells%>% filter(table_name =="catchoptionsbasis")
catchoptions <- table_cells%>% filter(table_name =="catchoptions")
advicebasis <- table_cells%>% filter(table_name =="advicebasis")
referencepoints <- table_cells%>% filter(table_name =="referencepoints")
assessmentbasis <- table_cells%>% filter(table_name =="assessmentbasis")

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

# catchoptions<-catchoptions[complete.cases(catchoptions),]
library(data.table)
catchoptions <- data.table(catchoptions)

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

write.csv(catchoptionsbasis, file = "hkecatchoptionsbasis.csv")
# catchoptions[is.na(catchoptions)] <- ""
write.csv(catchoptions, file = "hkecatchoptions.csv")
write.csv(advicebasis, file = "hkeadvicebasis.csv")
write.csv(referencepoints, file = "hkereferencepoints.csv")
write.csv(assessmentbasis, file = "hkeassessmentbasis.csv")


write.csv(catchoptionsbasis, file = "hercatchoptionsbasis.csv")
# catchoptions[is.na(catchoptions)] <- ""
write.csv(catchoptions, file = "hercatchoptions.csv")
write.csv(advicebasis, file = "heradvicebasis.csv")
write.csv(referencepoints, file = "herreferencepoints.csv")
write.csv(assessmentbasis, file = "herassessmentbasis.csv")

write.csv(catchoptionsbasis, file = "blicatchoptionsbasis.csv")
# catchoptions[is.na(catchoptions)] <- ""
write.csv(catchoptions, file = "blicatchoptions.csv")
write.csv(advicebasis, file = "bliadvicebasis.csv")
write.csv(referencepoints, file = "blireferencepoints.csv")
write.csv(assessmentbasis, file = "bliassessmentbasis.csv")


#Preparation for catch scenarios plot
colnames(catchoptions) <- c("Basis", "Catch", "blu", "bla","F","bli", "ble", "SSB","tras", "tru")
catchoptions$F <- as.numeric(catchoptions$F)
catchoptions$Catch <- as.numeric(catchoptions$Catch)
catchoptions$SSB <- as.numeric(catchoptions$SSB)
write.csv(catchoptions, file = "hkescenariosplot.csv")






