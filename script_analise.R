################################################################################
# PROJETO AIRBNB BUENOS AIRES               
################################################################################

#Business Understanding

#Neste projeto busco responder as seguintes questões: 

  #Qual o melhor período para alugar um AirBnb nem Buenos Aires?
  #Quais recursos são mais importantes para listar preços?

renv::init()

#instalando pacotes necessários

pacotes <- c("plotly","tidyverse","ggrepel","fastDummies","knitr","kableExtra",
             "splines","reshape2","PerformanceAnalytics","correlation","see",
             "ggraph","psych","nortest","rgl","car","ggside","tidyquant","olsrr",
             "jtools","ggstance","magick","cowplot","emojifont","beepr","Rcpp",
             "equatiomatic", "lpyr","sparklyr","arrow","nlme","reticulate",
             "mlflow", "stats","glue", "renv", "gridExtra","forecast","TTR",
             "smooth", "tsibble", "fable","tsibbledata", "fpp3","lubridate",
             "urca", "dygraphs", "quantmod","BETS","tseries","FinTS","feasts",
             "gridExtra", "scales", "caret","xtable", "tsutils","GetBCBData", 
             "quantmod","dgof","seasonal","devtools","transformr","gganimate")


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

#loading data
listing_df <- read_csv('listings.csv') #contém conjunto de dados airbnb completo de Buenos Aires
calendar_df <- read.csv("calendar.csv") #contém o preço de cada listagem durante o período de um ano

#Data Understanding
head(listing_df)
print(paste("o dataset tem",nrow(listing_df), "linhas e", ncol(listing_df), "colunas."))

#Pra começar vamos observar os 15 bairros mais listados através da coluna neighbourhood_cleansed.
listing_df %>% 
  group_by(neighbourhood_cleansed) %>% 
  summarise(qtd_bairros = n()) %>% 
  slice_max(qtd_bairros, n=15) %>%
  mutate(neighbourhood_cleansed = reorder(neighbourhood_cleansed, -qtd_bairros)) %>% 
  ggplot(aes(x = neighbourhood_cleansed, y = qtd_bairros, fill=neighbourhood_cleansed)) +
  theme(axis.text.x = element_text(angle = 90))+
  geom_col()
  
#-------------------------------------------------------------#
#Caso você vá até Buenos Aires, estes são os bairros em que você tem maiores chances de encontrar uma acomodação.
#-------------------------------------------------------------#


#Considerando os tipos de quartos disponíveis, Qual a média de preço em cada um deles?
#Para calcular a média, precisamos fazer uns ajustes na coluna "price", removendo $ e a vírgula.

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

#-------------------------------------------------------------#
#Quando se vai a Buenos Aires é mais caro ficar em hotel.Entretando é um pouco estranho que um Private room seja mais
#barato que um Shared room, você não acha? Vamos investigar isso? 
#-------------------------------------------------------------#


#Vamos ver como os preços se distribuem em função de cada tipo de quarto, quem sabe alguns outliers estejam alterando
#o valor médio dos tipos de quarto. 

ggplotly(
  ggplot(listing_df, aes(x = room_type, y = price)) +
    geom_point(color = "#39568CFF", size = 2.5) +
    labs(x = "Type_room", y = "Price") +
    theme_classic()
)

#BINGO! Como a média esta sendo manipulada pelos outliers, vou analisar a mediana, que por ser menos sensível a outliers,
#pode me dar um valor mais adequado a cerca dos tipos de quartos. Outilers serão tratados posteriormente. 

listing_df %>%
  group_by(room_type) %>%
  summarise(median_price = median(price)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 22)

#Agora podemos concluir que os quartos compartilhados são os mais baratos. Tudo faz mais sentido agora, não acha? :)


#O preço dos imóveis mudam significativamente ao longo do ano? 
calendar_df$date <- as.Date(calendar_df$date)
calendar_df['month'] <- (format(calendar_df$date, '%Y-%m'))
calendar_df$month <- as.factor(calendar_df$month)
calendar_df$price <- str_replace_all(calendar_df$price,'[$]','')
calendar_df$price <- str_replace_all(calendar_df$price,',','')
calendar_df$price <- as.numeric(calendar_df$price)

#preço em função da mediana de preço 
price_month <- calendar_df %>%
  group_by(month) %>%
  summarise(median_price = median(price)) %>%
  ggplot(aes(x = month, y = median_price)) +
  geom_line(color='green') +
  geom_point() +
  theme(axis.text.x = element_text(angle=90)) +
  labs(x= 'Month', y= 'Median Price',
       title = 'Price per month') +
  theme_classic()
price_month


#Verificando correlações

names(listing_df)
chart.Correlation((listing_df_1[,c(22,18,19,20)]), histogram = TRUE)

#Pelo visto o preço não é tão influenciado assim pela quantidade de banheiros, quartos e camas, já que a correção entre o preço
#e essas variáveis não é tão elevada. 


#Data Preparation

#verificando a quantidade de NA em cada coluna
sapply(listing_df, function(x) sum(is.na(x)))

#Verificar a quantidade de zeros em cada coluna
ldply(listing_df, function(c) sum(c == 0))

#Excluindo colunas vazias, com url, localização, comentários, nomes do host pois não serão usadas nesta análise.
listing_df <- subset(listing_df, select = -c(id, listing_url, scrape_id, picture_url,host_id, host_url, 
                                             host_thumbnail_url, host_picture_url, neighbourhood_group_cleansed,
                                             review_scores_value, calendar_updated, license, bathrooms,neighbourhood,
                                             neighborhood_overview, host_neighbourhood, host_location, host_response_rate, 
                                             host_about,description, name, host_name, first_review, last_review))


#Como parâmetro de localização, usarei neighbourhood_cleansed, que tem o bairro baseado na latitude e longitude do imóvel.

unique(listing_df$neighbourhood_cleansed)
table(listing_df$neighbourhood_cleansed)

#Preparando colunas 
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

#Bedrooms tem 3058 valores NA. Talvez esse imóvel seja um studio, quarta-sala ou algo to tipo. Vou inserir
#o valor 1 no lugar, assim como beds, pois pode ser sofá-cama. 

listing_df$beds[is.na(listing_df$beds)] <- 1
listing_df$bedrooms[is.na(listing_df$bedrooms)] <- 1
listing_df$bathrooms_text[is.na(listing_df$bathrooms_text)] <- 1

#review_per_month (float) = 4122 NA
#number_of_reviews(int)= 4122 ZEROS
#Como alguns imóveis não tem number_of_reviews, consequentemente esses imóveis acabam não tendo review_per_month. 
#Por isso vou substituir os NA por 0.

listing_df$number_of_reviews[is.na(listing_df$number_of_reviews)] <- 0
listing_df$reviews_per_month[is.na(listing_df$reviews_per_month)] <- 0

#Transformando variáveis em factor 
listing_df$source <- as.factor(listing_df$source)
listing_df$property_type <- as.factor(listing_df$property_type)
listing_df$host_response_time[is.na(listing_df$host_response_time)] <- "did not inform"
listing_df$host_response_time <- as.factor(listing_df$host_response_time)
listing_df$neighbourhood_cleansed <- as.factor(listing_df$neighbourhood_cleansed)
listing_df$room_type <- as.factor(listing_df$room_type)

#host_verification
listing_df$host_verifications[listing_df$host_verifications == '[]'] <- 1
listing_df$host_verifications <- as.factor(listing_df$host_verifications)
listing_df$host_verifications <- droplevels(listing_df$host_verifications, exclude = 1)

#amenities
listing_df$amenities <- lengths(gregexpr(",", listing_df$amenities)) + 1L

#host_acceptance_rate
listing_df$host_acceptance_rate <- str_remove_all(listing_df$host_acceptance_rate, '[%]')
listing_df$host_acceptance_rate <- as.numeric(listing_df$host_acceptance_rate)

#data
listing_df$host_since <- as.Date(listing_df$host_since)
listing_df$host_since <- (format(listing_df$host_since, '%Y-%m'))
listing_df$host_since <- as.factor(listing_df$host_since)


#Depois de uma breve análise, observei que as variáveis abaixo não são relevantes,portanto, vamos excluí-las.
listing_df <- subset(listing_df, select = -c(last_scraped, calendar_last_scraped))

#Tratando outliers
#Criei uma função para tratar o outliers
quartil <- function(column){
  
  q1 <- quantile(column, 0.25, na.rm = TRUE) #1º quartil
  q3 <- quantile(column, 0.75, na.rm = TRUE) #3º quartil
  iq <- q3 - q1 #interquartil
  lim_sup <- q3 + 1.5*iq #limite superior
  return(lim_sup)
  
}
max_beds<- quartil(listing_df$beds)
max_bedrooms <- quartil(listing_df$bedrooms)
max_bathrooms <- quartil(listing_df$bathrooms_text)
max_price <- quartil(listing_df$price)

print(paste("beds:",max_beds, "bedrooms:", max_bedrooms, "bathrooms:", max_bathrooms, "price:", max_price))

#Agora vou descartar qualquer linha onde preço esteja acima do limite superior
#beds > 3.5, bedrooms > 1 e bathroms > 2.25

#Excluindo outliers das colunas
 
for (i in seq_along(listing_df$beds)){
  if (listing_df$beds[i] > 3.5){
    listing_df$beds[i] <- mean(listing_df$beds)
  } 
}
  
for (i in seq_along(listing_df$bedrooms)){
  if (listing_df$bedrooms[i] > 1){
    listing_df$bedrooms[i] <- 1
  } 
}

for (i in seq_along(listing_df$bathrooms_text)){
  if (listing_df$bathrooms_text[i] > 2.25){
    listing_df$bathrooms_text[i] <- mean(listing_df$bathrooms_text)
  } 
}

for (i in seq_along(listing_df$price)){
  if (listing_df$price[i] > 24068){
    listing_df$price[i] <- mean(listing_df$price)
  } 
}

boxplot(listing_df$bedrooms)
boxplot(listing_df$beds)
boxplot(listing_df$bathrooms_text)
boxplot(listing_df$price)
boxplot(listing_df$host_acceptance_rate)

#Handling NaNs
listing_df$host_acceptance_rate[is.na(listing_df$host_acceptance_rate)] <- 77.077
listing_df <- listing_df[!is.na(listing_df$host_verifications),]
sapply(listing_df, function(x) sum(is.na(x)))

#todas as variaveis com score possuem nuitos NAns então vou dropaar 

listing_df <- subset(listing_df, select = -c(review_scores_accuracy, review_scores_communication, 
                                                       review_scores_cleanliness, review_scores_location,
                                                       review_scores_rating, review_scores_checkin))

#Sevocê retornar na linha 69 e refazer a tabela com a média de valor de cada tipo de quarto, agora com outliers tratados, verá que faz muito mais sentido.


#Dummizando variáveis
listing_df_1_dummies <- dummy_columns(.data = listing_df,
                                      select_columns = c("room_type", "host_response_time", "host_verifications",
                                                         "neighbourhood_cleansed", "property_type"),
                                      remove_selected_columns = T,
                                      remove_most_frequent_dummy = T)

summary(listing_df_1_dummies)


#Modeling

#ESTIMAÇÃO DA REGRESSÃO LINEAR MÚLTIPLA 

#modelagem com todas as variáveis
modelo_listing <- lm(price ~ ., listing_df_1_dummies)
summary(modelo_listing_dummies)


#Procedimento Step-wise no modelo
step_listing_df <- step(modelo_listing, k = 3.841459)
summary(step_listing_df)

#Kernel density estimation (KDE) 
listing_df_1_dummies %>%
  ggplot() +
  geom_density(aes(x = step_listing_df$residuals), fill = "#55C667FF") +
  labs(x = "Resíduos do Modelo Stepwise",
       y = "Densidade") +
  theme_bw()

#Teste de aderência dos resíduos à normalidade


sf_teste <- function (x) 
{
  DNAME <- deparse(substitute(x))
  x <- sort(x[complete.cases(x)])
  n <- length(x)
  if ((n < 5 || n > 25000)) 
    stop("sample size must be between 5 and 5000")
  y <- qnorm(ppoints(n, a = 3/8))
  W <- cor(x, y)^2
  u <- log(n)
  v <- log(u)
  mu <- -1.2725 + 1.0521 * (v - u)
  sig <- 1.0308 - 0.26758 * (v + 2/u)
  z <- (log(1 - W) - mu)/sig
  pval <- pnorm(z, lower.tail = FALSE)
  RVAL <- list(statistic = c(W = W), p.value = pval, method = "Shapiro-Francia normality test", 
               data.name = DNAME)
  class(RVAL) <- "htest"
  return(RVAL)
}

sf_teste(step_listing_df$residuals)

#histograma
listing_df %>%
  mutate(residuos = step_listing_df$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "white", 
                 fill = "#440154FF", 
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(step_listing_df$residuals),
                            sd = sd(step_listing_df$residuals)),
                size = 2, color = "grey30") +
  scale_color_manual(values = "grey50") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme_bw()

#O teste de Shapiro-Francia comprovou a não derência à normalidade dos resíduos. 
#Diante disso, vou fazer uma transformação Box-Cox na variável dependente e rodar novo modelo.

#TRABSFORMAÇÃO BOX-COX
lambda_BC <- powerTransform(listing_df$price)
lambda_BC

#Inserindo o lambda de Box-Cox na base de dados para a estimação de um novo modelo
listing_df_1_dummies$bcprice <- (((listing_df$price ^ lambda_BC$lambda) - 1) / 
                         lambda_BC$lambda)

#Estimando um novo modelo múltiplo com variável dependente transformada por Box-Cox
modelo_bc <- lm(formula = bcprice ~ ., na.rm = T,
                data = listing_df_1_dummies)

summary(modelo_bc)

#Step-wise no modelo com box-cox
step_modelo_bc <- step(modelo_bc, k=3.841459)
summary(step_modelo_bc)


<<<<<<< HEAD
#Plotando os novos resíduos do modelo step_modelo_bc com curva normal teórica
=======
#Diagnóstico de Heterocedasticidade para o Modelo Stepwise com Box-Cox
ols_test_breusch_pagan(step_modelo_bc)

#obs: Além dos resíduos não serem aderentes à normalidade, também observamos
#que o teste de heterocedasticidade aponta que há variáveis omissas que seriam
#relevantes para explicar Y.

#Plotando os novos resíduos do modelo step_bc_planosaude com curva normal teórica
>>>>>>> 333f01b3545c5dba8fbaba503908c72f26210887
listing_df_1_dummies %>%
  mutate(residuos = step_modelo_bc$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "white", 
                 fill = "#440154FF", 
                 bins = 15,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(step_modelo_bc$residuals),
                            sd = sd(step_modelo_bc$residuals)),
                size = 2, color = "grey30") +
  scale_color_manual(values = "grey50") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme_bw()

#Teste de shapiro francia
sf_teste(step_modelo_bc$residuals)

<<<<<<< HEAD
#Diagnóstico de Heterocedasticidade para o Modelo Stepwise com Box-Cox
ols_test_breusch_pagan(step_modelo_bc)

#obs: Além dos resíduos não serem aderentes à normalidade, também observamos
#que o teste de heterocedasticidade aponta que há variáveis omissas que seriam
#relevantes para explicar Y.

#Vou adicionar ao dataset valores de Yhat com stepwise e stepwise + Box-Cox para fins de 
#comparação
=======
  
#Adicionando ao dataset valores de Yhat com stepwise e stepwise + Box-Cox
>>>>>>> 333f01b3545c5dba8fbaba503908c72f26210887
listing_df$yhat_step_listing <- step_listing_df$fitted.values
listing_df$yhat_step_modelo_bc <- (((step_modelo_bc$fitted.values*(lambda_BC$lambda))+
                                    1))^(1/(lambda_BC$lambda))


#Visualizando os dois fitted values no dataset
#modelos step_listing_df e step_modelo_bc
listing_df %>%
  select(price, yhat_step_listing, yhat_step_modelo_bc) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)


#Ajustes dos modelos: valores previstos (fitted values) X valores reais
listing_df %>%
  ggplot() +
  geom_smooth(aes(x = price, y = yhat_step_listing , color = "Stepwise"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5), size = 1.5) +
  geom_point(aes(x = price, y = yhat_step_listing),
             color = "#440154FF", alpha = 0.6, size = 2) +
  geom_smooth(aes(x = price, y = yhat_step_modelo_bc, color = "Stepwise Box-Cox"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5), size = 1.5) +
  geom_point(aes(x = price, y = yhat_step_modelo_bc),
             color = "#287D8EFF", alpha = 0.6, size = 2) +
  geom_smooth(aes(x = price, y = price), method = "lm", formula = y ~ x,
              color = "grey30", size = 1.05,
              linetype = "longdash") +
  scale_color_manual("Modelos:", 
                     values = c("#287D8EFF", "#440154FF")) +
  labs(x = "price", y = "Fitted Values") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")


#Fazendo predições
predict(object = step_modelo_bc, 
        data.frame(disclosure = 50, 
                   liquidez = 14, 
                   ativos = 4000),
        interval = "confidence", level = 0.95)