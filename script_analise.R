################################################################################
# PROJETO AIRBNB BUENOS AIRES               
################################################################################

#instalando pacotes necessários


pacotes <- c("plotly","tidyverse","ggrepel","fastDummies","knitr","kableExtra",
             "splines","reshape2","PerformanceAnalytics","correlation","see",
             "ggraph","psych","nortest","rgl","car","ggside","tidyquant","olsrr",
             "jtools","ggstance","magick","cowplot","emojifont","beepr","Rcpp",
             "equatiomatic", "lpyr","sparklyr","arrow","nlme","reticulate",
             "mlflow", "stats","glue", "renv")

options(rgl.debug = TRUE)

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


renv::init()
usethis::edit_r_environ(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)

#Conhecendo a base de dados

listing_df <- read_csv('listings.csv')

colnames(listing_df)

#verificando a quantidade de NA em cada coluna
sapply(listing_df, function(x) sum(is.na(x)))

#Verificar a quantidade de zeros em cada coluna
ldply(listing_df, function(c) sum(c == 0))


#Entendendo os dados

ggplotly(
  ggplot(listing_df, aes(x=neighbourhood_cleansed, fill=neighbourhood_cleansed))+
    geom_bar()
)


#Os bairros com maiores quantidades de listagens disponíveis são: Palermo, recoleta, San Nicolas,Belgrano, Retiro. 
#Então se você pensa em visitar buenos Aires, esses bairros podem ser uma boa opção para começar a buscar abrigo. 


#Considerando os tipos de quartos disponíveis, Qual a média de preço em cada um deles?
#Para calcular a média, precisamos organizar a col price, removendo $ e a vírgula.

listing_df$price <- str_replace_all(listing_df$price,'[$]','')
listing_df$price <- str_replace_all(listing_df$price,',','')
listing_df$price <- as.numeric(listing_df$price)

listing_df %>%
  group_by(room_type) %>%
  summarise(avg_price = mean(price)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 22)


#Quando se vai a buenos aires é mais caro ficar em hotel. Entretando é um pouco estranho que um Private room seja mais
#barato que um Shared room, você não acha? Vamos investigar isso? 

#Vamos ver como os preços se distribuem em função de cada tipo de quarto, quem sabe alguns outliers estejam alterando
#o valor médio dos tipos de quarto. 

ggplotly(
  ggplot(listing_df, aes(x = room_type, y = price)) +
    geom_point(color = "#39568CFF", size = 2.5) +
    labs(x = "Type_room", y = "Price") +
    theme_classic()
)

#BINGO! Como a média esta sendo manipulada pelos outliers, vou analisar a mediana, que por ser menos sensível a outliers
#podem me dar um valor mais adequado a cerca dos typos de quartos.

listing_df %>%
  group_by(room_type) %>%
  summarise(median_price = median(price)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 22)

#Agora podemos concluir que os quartos compartilhados são os mais baratos. Tudo faz mais sentido agora, não acha? :)

calendar_df <- read.csv("calendar.csv")

calendar_df$date <- as.Date(calendar_df$date)
calendar_df['month'] <- (format(calendar_df$date, '%Y-%m'))
calendar_df$month <- as.factor(calendar_df$month)

glimpse(calendar_df$month)
calendar_df$price <- str_replace_all(calendar_df$price,'[$]','')
calendar_df$price <- str_replace_all(calendar_df$price,',','')
calendar_df$price <- as.numeric(calendar_df$price)

unique(calendar_df$month)

#grafico line 
price_month <- calendar_df %>%
  group_by(month) %>%
  summarise(median_price = median(price)) %>%
  ggplot(aes(x = month, y = median_price)) +
  geom_line(color='green') +
  geom_point() +
  theme(axis.text.x = element_text(angle=90)) +
  labs(x= 'Month', y= 'Price',
       title = 'Price per month') +
  theme_classic()
price_month

#Limpando o dataset

#Excluindo colunas vazias, com url, localização, comentários, nomes do host pois não serão usadas nesta análise.

listing_df <- subset(listing_df, select = -c(listing_url, scrape_id, picture_url,host_id, host_url, 
                                             host_thumbnail_url, host_picture_url, neighbourhood_group_cleansed,
                                             review_scores_value, calendar_updated, license, bathrooms,neighbourhood,
                                             neighborhood_overview, host_neighbourhood, host_location, host_response_rate, 
                                             host_about,description, name, host_name, first_review, last_review))


#Como parâmetro de localização, usarei neighbourhood_cleansed, que tem o bairro baseado na latitude e longitude do imóvel.

unique(listing_df$neighbourhood_cleansed)
table(listing_df$neighbourhood_cleansed)

#A coluna bathrooms está vazia e foi excluída, pois na verdade, a quantidade de bathrooms está
#em bathrooms_text, que precisa de alguns ajustes. Vamos fazer isso agora.

unique(listing_df$bathrooms_text)
listing_df$bathrooms_text <- str_replace_all(listing_df$bathrooms_text,'bath','')
listing_df$bathrooms_text <- str_replace_all(listing_df$bathrooms_text,'s','')
listing_df$bathrooms_text <- str_replace_all(listing_df$bathrooms_text,'S','')
listing_df$bathrooms_text <- str_replace_all(listing_df$bathrooms_text,'private','')
listing_df$bathrooms_text <- str_replace_all(listing_df$bathrooms_text,'Private','')
listing_df$bathrooms_text <- str_replace_all(listing_df$bathrooms_text,'hared','')
listing_df$bathrooms_text <- str_replace_all(listing_df$bathrooms_text,'half-','')
listing_df$bathrooms_text <- str_replace_all(listing_df$bathrooms_text,'Half-','')
listing_df$bathrooms_text <- as.numeric(listing_df$bathrooms_text)

#Bedrooms tem 3058 valores NA. Talvez o anfitrião tenha considerado quarto-sala. Vou inserir
#o valor 1 no lugar, assim como beds, pois pode ser sofá-cama. 

listing_df$beds[is.na(listing_df$beds)] <- 1
listing_df$bedrooms[is.na(listing_df$bedrooms)] <- 1
listing_df$bathrooms_text[is.na(listing_df$bathrooms_text)] <- 1
unique(listing_df$bathrooms_text) 

#review_per_month (float) = 4122 NA
#number_of_reviews(int)= 4122 ZEROS
#Como alguns imóveis não tem number_of_reviews, consequentemente esses imóveis acabam não tendo review_per_month. 
#Por isso vou substituir os NA por 0.

listing_df$number_of_reviews[is.na(listing_df$number_of_reviews)] <- 0
listing_df$reviews_per_month[is.na(listing_df$reviews_per_month)] <- 0

#Análisar as variáveis categóricas

#source possui apenas 2 níveis. Vamos transformar em factor.

listing_df$source <- as.factor(listing_df$source)
glimpse(listing_df$source)

#Transformando variáveis em factor 


listing_df$property_type <- as.factor(listing_df$property_type)
listing_df$host_response_time <- as.factor(listing_df$host_response_time)
listing_df$host_response_time <- droplevels(listing_df$host_response_time, exclude = "N/A")
listing_df$neighbourhood_cleansed <- as.factor(listing_df$neighbourhood_cleansed)
listing_df$room_type <- as.factor(listing_df$room_type)

unique(listing_df$host_response_time) #usar sempre esse comando p/ conferir se tudo ocorreu bem.

#Tratando host_verification
listing_df$host_verifications[listing_df$host_verifications == '[]'] <- 1
listing_df$host_verifications <- as.factor(listing_df$host_verifications)
listing_df$host_verifications <- droplevels(listing_df$host_verifications, exclude = 1)

#Tratando amenities
listing_df$amenities <- lengths(gregexpr(",", listing_df$amenities)) + 1L
glimpse(listing_df$amenities)


#Tratando host_acceptance_rate
listing_df$host_acceptance_rate <- str_remove_all(listing_df$host_acceptance_rate, '[%]')
listing_df$host_acceptance_rate <- as.numeric(listing_df$host_acceptance_rate)
unique(listing_df$host_acceptance_rate)

#Tratando data

listing_df$host_since <- as.Date(listing_df$host_since)
listing_df$host_since <- (format(listing_df$host_since, '%Y-%m'))
listing_df$host_since <- as.factor(listing_df$host_since)
glimpse(listing_df$host_since)

#Depois de uma breve análise, observei que as variáveis abaixo não são relevantes,portanto, vamos excluí-las.

listing_df <- subset(listing_df, select = -c(last_scraped, calendar_last_scraped))

#Tratando outliers



#Modelagem (Série temporal)

#price vs month


#Modelagem Regressão Múltipla

#  1.  Rodar modelo dropando os NA 
#  2.  Rodar modelo dropando apenas as colunas de score


#MODELO 1: Dropando NA das colunas de scores.
           
listing_df_1 <- subset(listing_df, select = -c(review_scores_accuracy, review_scores_communication, 
                                                          review_scores_cleanliness, review_scores_location,
                                                          review_scores_rating, review_scores_checkin))

           
#Dropar NAS
           
listing_df_1 <- na.omit(listing_df_1)
sapply(listing_df_1, function(x) sum(is.na(x)))
           
#Estatísticas univariadas
summary(listing_df_1)
           
#Verificando correlações

chart.Correlation((listing_df_1[,c(21,4,6,16,17,18,19)]), histogram = TRUE)

#Dummizando variáveis
listing_df_1_dummies <- dummy_columns(.data = listing_df_1,
                                      select_columns = c("room_type", "host_response_time", "host_verifications",
                                                                    "neighbourhood_cleansed", "property_type"),
                                      remove_selected_columns = T,
                                      remove_most_frequent_dummy = T)