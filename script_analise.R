################################################################################
# PROJETO AIRBNB BUENOS AIRES               
################################################################################

#instalando pacotes necessários

install.packages('renv')
renv::init()
pacotes <- c("plotly","tidyverse","ggrepel","fastDummies","knitr","kableExtra",
             "splines","reshape2","PerformanceAnalytics","correlation","see",
             "ggraph","psych","nortest","rgl","car","ggside","tidyquant","olsrr",
             "jtools","ggstance","magick","cowplot","emojifont","beepr","Rcpp",
             "equatiomatic", "lpyr","sparklyr","arrow","nlme","reticulate",
             "mlflow", "stats","glue")

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

# desconectar alguma conexão ativa com o spark
spark_disconnect_all()


sc <- spark_connect(master = "local", 
                    spark_home="./spark/build/spark-3.4.0-bin-hadoop3")
#Conhecendo a base de dados

listing_df <- read_csv('listings.csv')

colnames(listing_df)

#verificando a quantidade de NA em cada coluna
sapply(listing_df, function(x) sum(is.na(x)))

#Verificar a quantidade de zeros em cada coluna
ldply(listing_df, function(c) sum(c == 0))

#Limpando o dataset

#Excluindo colunas vazias e com url, pois não são de interesse nesta análise.
#EXCLUINDO AS VARIÁVEIS: neighbourhood, neighbourhood_overview, 
#host_neighbourhood, host_location, pois são informações de localização do anfitrião,
#além de muitos NS, estão bagunçadas. Como parâmetro de localização, usarei
#neighbourhood_cleansed, que tem o bairro baseado na latitude e longitude do imóvel.
#Excluir: host_about, que são comentários a respeito do host (+9k NA),
#além disso, não dá pra sugerir que ter opinião registrada sobre o anfitrião é algo bom 
#e não ter é algo ruim
#Dropando colunas que tenham descrições, nomes ou quaisquer outros valores únicos e que não possam
#ser classificados como factor

listing_df <- subset(listing_df, select = -c(listing_url, scrape_id, picture_url,host_id, host_url, 
                                             host_thumbnail_url, host_picture_url, neighbourhood_group_cleansed,
                                             review_scores_value, calendar_updated, license, bathrooms,neighbourhood,
                                             neighborhood_overview, host_neighbourhood, host_location, host_response_rate, 
                                             host_about,description, name, host_name, first_review, last_review))

unique(listing_df$neighbourhood_cleansed)

#Transformar 'price' em numeric

listing_df$price <- str_replace_all(listing_df$price,'[$]','')
listing_df$price <- str_replace_all(listing_df$price,',','')
listing_df$price <- as.numeric(listing_df$price)
listing_df$price

#A coluna bathrooms está vazia e foi excluída, pois na verdade, a quantidade de bathrooms está
#na coluna bathrooms_text, que precisa ser transformada em númeric. Vamos fazer isso agora!

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
listing_df$bathrooms_text 

#review_per_month (float) = 4122 NA
#number_of_reviews(int)= 4122 ZEROS
#Como number_of_reviews, consequentemente esses imóveis acabam não tendo review_per_month. 
#Por isso vou substituir os NA por 0.

listing_df$reviews_per_month[is.na(listing_df$reviews_per_month)] <- 0

#Análisar as variáveis categóricas

#source possui apenas 2 níveis. Vou passar de Character para factor. 

listing_df$source <- as.factor(listing_df$source)
glimpse(listing_df$source)

#transformando em factor 

unique(listing_df$host_response_time)
listing_df$property_type <- as.factor(listing_df$property_type)
listing_df$host_response_time <- as.factor(listing_df$host_response_time)
listing_df$host_response_time <- droplevels(listing_df$host_response_time, exclude = "N/A")
listing_df$neighbourhood_cleansed <- as.factor(listing_df$neighbourhood_cleansed)
listing_df$room_type <- as.factor(listing_df$room_type)

#host_verification
listing_df$host_verifications[listing_df$host_verifications == '[]'] <- 1
listing_df$host_verifications <- as.factor(listing_df$host_verifications)
listing_df$host_verifications <- droplevels(listing_df$host_verifications, exclude = 1)

#amenities
unique(listing_df$amenities)
glimpse(listing_df$amenities)
listing_df$amenities <- lengths(gregexpr(",", listing_df$amenities)) + 1L

#host_acceptance_rate
listing_df$host_acceptance_rate <- str_remove_all(listing_df$host_acceptance_rate, '[%]')
listing_df$host_acceptance_rate <- as.numeric(listing_df$host_acceptance_rate)

#Organizando variáveis de data

listing_df$host_since <- as.Date(listing_df$host_since)
listing_df$host_since <- (format(listing_df$host_since, '%Y-%m'))
class(listing_df$host_since)



#essas variáveis não paraecem ser muito interessantres para nosso modelo
#portanto, vamos excluí-las.

listing_df <- subset(listing_df, select = -c(last_scraped, calendar_last_scraped))


table(listing_df$neighbourhood_cleansed)

#################################################################################
ggplotly(
  ggplot(listing_df, aes(x=neighbourhood_cleansed, fill=neighbourhood_cleansed))+
    geom_bar()
)

#Os % bairros com maiores quantdades de listagens disponíveis
#são: Palermo, recoleta, San Nicolas,Belgrano, Retiro. Então se você pensa em visitar buenos
#Aires, esses bairros podem ser uma boa opção para começar a buscar abrigo. 


summary(listing_df)

#Qual a média de preço considerando cada tipo de quarto ofertado?

#selecting only room_type and price columns

listing_df %>%
  group_by(room_type) %>%
  summarise(avg_price = mean(price)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 22)


#Quando se vai a buenos aires é mais caro ficar em hotel. Caso queira economizar,
#vá de private room :)

#Vamos ver como os preços se distribuem em função de cada tipo de quarto. 

ggplotly(
  ggplot(listing_df, aes(x = room_type, y = price)) +
    geom_point(color = "#39568CFF", size = 2.5) +
    labs(x = "Type_room", y = "Price") +
    theme_classic()
)
#Como os outliers podem alterar muito o valor médio, vamos analisar a mediana
#e verificar qual tipo de quarto tem valor mais acessível. 

listing_df %>%
  group_by(room_type) %>%
  summarise(median_price = median(price)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 22)

#Agora podemos concluir que os quartos compartilhados são os mais baratos. 

calendar_df <- read.csv("calendar.csv")

calendar_df$date <- as.Date(calendar_df$date)
calendar_df['month'] <- (format(calendar_df$date, '%Y-%m'))

calendar_df$price <- str_replace_all(calendar_df$price,'[$]','')
calendar_df$price <- str_replace_all(calendar_df$price,',','')
calendar_df$price <- as.numeric(calendar_df$price)


#grafico line 
price_month <- calendar_df %>%
  group_by(month) %>%
  mutate(median_price = median(price)) %>%
  ggplot(aes(x = month, y = median_price)) +
  geom_line() +
  geom_point() +
  labs(x= 'Month', y= 'Price',
       title = 'Price per month') +
  theme_classic()
price_month


price_m <- calendar_df %>%
  select(month, price)
group by (month) %>%
  sumarise(avg_price = median(price)
           
           
           
           
           ##############################################################################
           #g <- ggplot(listing_df, aes(neighbourhood_cleansed))
           #g + geom_bar()
           #ggplot(listing_df) + geom_bar(aes(y = neighbourhood_cleansed))
           #############################################################################
           #ggplot(listing_df, aes(y = neighbourhood_cleansed)) +
           #  geom_bar(aes(fill = neighbourhood_cleansed), position = position_stack(reverse = TRUE)) +
           #  theme(legend.position = "top")
           ##########################################################################################
           
           
           
           #PRÓXIMOS PASSOS:
           
           #  1.  Rodar modelo dropando os NA 
           #  2.  Rodar modelo dropando apenas as colunas de score
           
           
           ################################################################################
           #                OBSERVANDO OS DADOS CARREGADOS DA BASE corrupcao              #
           ################################################################################
           #MODELO 1: Dropando NA das colunas de scores.
           
           listing_df_1 <- subset(listing_df, select = -c(review_scores_accuracy, review_scores_communication, 
                                                          review_scores_cleanliness, review_scores_location,
                                                          review_scores_rating, review_scores_checkin))
           
           glimpse(listing_df_1)
           
           #Dropar NAS
           
           listing_df_1 <- na.omit(listing_df_1)
           sapply(listing_df_1, function(x) sum(is.na(x)))
           
           #Estatísticas univariadas
           summary(listing_df_1)
           
           ############################################################################
           #                          ESTUDO DAS CORRELAÇÕES
           ############################################################################
           chart.Correlation((listing_df_1[,c(21,4,6,16,17,18,19)]), histogram = TRUE)
           glimpse(listing_df_1$host_listings_count)
           
           #                          PROCEDIMENTO N-1 DUMMIES                            #
           ################################################################################
           #Dummizando a variável regiao. O código abaixo, automaticamente, fará: a) o
           #estabelecimento de dummies que representarão cada uma das regiões da base de 
           #dados; b)removerá a variável dummizada original; c) estabelecerá como categoria 
           #de referência a dummy mais frequente.
           listing_df_1_dummies <- dummy_columns(.data = listing_df_1,
                                                 select_columns = c("room_type", "host_response_time", "host_verifications",
                                                                    "neighbourhood_cleansed", "property_type"),
                                                 remove_selected_columns = T,
                                                 remove_most_frequent_dummy = T)
           
           
           #Modelagem com todas as variáveis
           modelo_listing_dummies_1 <- lm(price ~ ., listing_df_1_dummies)
           
           #Parâmetros do modelo_corrupcao_dummies
           summary(modelo_listing_dummies_1)
           
           #TESTE DE ADERÊNCIA À NORMALIDADE
           
           #historgrama
           hist(listing_df_1$price)
           glimpse(listing_df)
           #gráfico box plot com a variável price
           ggplot(listing_df_1, aes(, price)) + 
             geom_boxplot(fill = 'white', colour = 'slateblue3') + 
             geom_jitter(width = 0.2, shape=16, size=2)+
             labs(title = "Diagrama Box", x= "bathrooms", y="price") + 
             theme_bw()
           