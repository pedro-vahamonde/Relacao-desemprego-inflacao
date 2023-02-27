

###############################################################
### Trabalho Final da matéria Análises de Séries Temporais ####
###############################################################


#Definindo o Diretório 
setwd('C:\\Users\\Pedro\\Documents\\UFABC\\Pós Gradução/Mestrado\\Aulas\\Análise de Séries Temporais\\Trabalho final')
getwd()


#Pacotes utilizadas

install.packages("readxl")
install.packages('dplyr')
install.packages('lubridate')
install.packages('sidrar')
install.packages('ggplot2')
install.packages('lmtest')
install.packages('forecast')
install.packages('urca')
install.packages('seasonal')
install.packages('tsDyn')
install.packages('vars')


library("readxl")
library('dplyr')
library('lubridate')
library('sidrar')
library('ggplot2')
library('lmtest')
library('forecast')
library('urca')
library('seasonal')
library('tsDyn')
library('vars')



## Trabalhando a planilha da taxa de desemprego retirada do IPEADATA ##

x1<-read.csv("ipeadata[09-01-2023-06-26].csv",header = TRUE,sep = ',')
View(x1)

x2 <- x1 %>%
      rename(Tempo=Data,Taxa.de.desemprego=Taxa.de.desocupação.........Instituto.Brasileiro.de.Geografia.e.Estatística..Pesquisa.Nacional.por.Amostra.de.Domicílios.Contínua..IBGE.PNAD.Contínua....PNADC12_TDESOC12)   %>%
      mutate(Tempo = seq.Date(from = as_date('2012-03-01'), 
                              to = as_date("2022-10-01"),
                              by = "1 month"))

 #Transformando em Série Temporal
desemprego <- ts(x2$'Taxa.de.desemprego',start = c(2012,3), end = c(2022,10), frequency = 12)
plot.ts(desemprego)
tsdisplay(desemprego)



## Puxando a tabela da inflação a partir do site do IBGE-Sidra ##

y <- get_sidra(1737, variable = 63, period = c("201203-202210"), geo = "Brazil",
          geo.filter = NULL, classific = NULL, category = NULL, header = TRUE)
View(y)


y2 <- y %>%
         select(Mês,Valor) %>%
         rename('Inflação' = 'Valor',Tempo=Mês) %>%
         mutate (Tempo = seq.Date(from = as_date('2012-03-01'), 
                                 to = as_date('2022-10-01'),
                                 by = '1 month'))
View(y2)

#Transformandoem Série Temporal
ipca <- ts(y2$`Inflação`, start = c(2012,3), end = c(2022,10), frequency = 12)



#Verificando os gráficos das Séries Temporais
par(mfrow=c(2,1))
plot.ts(desemprego, color=(2), main=('Taxa de desemprego'))
plot.ts(ipca,color=(3), main=('Taxa de inflação'))   



##########################################################
# Verificando estacionariedade pelo teste de Dick-Fuller #
########################################################## 

 #Desemprego

    #Passo 1: Testando o ADF usando modelo com constante e com tendência:
     df.desemprego <- ur.df(desemprego, type = 'trend', lags=22)
     plot(df.desemprego)
     summary(df.desemprego)
     
    ## Value of test-statistic is: -0.3049 1.5384 2.174  ##
     
    ## Critical values for test statistics: ##
    ##       1pct  5pct 10pct ##
    ## tau3 -3.99 -3.43 -3.13 ##
    ## phi2  6.22  4.75  4.07 ##
    ## phi3  8.43  6.49  5.47 ##
     
     ## Mantém a hipótese nula (há raiz unitária), pois o valor da estatística calculada é maior que o valor tau3 ##
     ## Além disso a estatítica conjunta é menor que o phi3
    
    
     #Passo 2: Testar ADF usando modelo só com constante
     df2.desemprego <- ur.df(desemprego, type = 'drift', lags = 22)
     plot(df2.desemprego)
     summary(df2.desemprego)
     
     ## Value of test-statistic is: -2.0082 2.1511 ##
     
    ## Critical values for test statistics: ##
     ##       1pct  5pct 10pct ## 
     ## tau2 -3.46 -2.88 -2.57 ##
     ## phi1  6.52  4.63  3.81 ##
     
      ## Mantém a hipótese nula (há raiz unitária), pois o valor da estatística calculada é maior que o tau2
     ## Além disso a estatítica conjunta (2.1511) é menor que o phi2 (3.81) 
     
     
    # Passo 3: Testar o ADF sem constante e sem tendência linear
    df3.desemprego <- ur.df(desemprego, type = 'none', lags =22)
    plot(df3.desemprego)
    summary(df3.desemprego)
 
    ## Value of test-statistic is: 0.076 
    
    ## Critical values for test statistics: 
    ##       1pct  5pct 10pct
    ## tau1 -2.58 -1.95 -1.62
    
    #Mantém a hipóstese nula que há raiz unitária, pois a estatística caulada (0.076) é maior que o tau1
    
    # Portanto, a variável Desemprego2 é não estacionária
    
    
    
 #IPCA
    tsdisplay(ipca)

    #Passo 1: Testando o ADF usando modelo com constante e com tendência:
    df.ipca <- ur.df(ipca, type = 'trend', lags = 0)
    plot(df.ipca)
    summary(df.ipca)
    
    ## Value of test-statistic is: -5.8048 11.2377 16.8512  ##
    
    ## Critical values for test statistics: ##
    ##       1pct  5pct 10pct ## 
    ## tau3 -3.99 -3.43 -3.13 ##
    ## phi2  6.22  4.75  4.07 ## 
    ## phi3  8.43  6.49  5.47 ##
    
    #Descarta-se a hipótese nula de haver raiz unitária,pois a estatística calculada é menor que o tau3
    #Além disso, a estatística conjunta é maior que o phi3
    
    #Portanto a variável IPCA2 é estacionária

    
######################################  
### Estacionarizando desemprego ######
######################################

  #Estacionarizando a série desemprego   
    dif.desemprego<- diff(desemprego)
    tsdisplay(dif.desemprego)
  
  #Retirando a sazonalidade
    s.dif.desemprego<-seas(dif.desemprego)
    s.dif.desemprego <- ts(s.dif.desemprego$data[,1],start=c(2012,4), end = c(2022,10), frequency = 12)
    tsdisplay(s.dif.desemprego) 
    
  #Refazendo o teste de estacionariedade ADF
   
     #Passo 1:
    df.s.dif.desemprego <- ur.df(s.dif.desemprego, type = 'trend', lags=11)
    plot(df.s.dif.desemprego)
    summary(df.s.dif.desemprego)
    
    #Passo 2:
    df2.s.dif.desemprego <- ur.df(s.dif.desemprego, type = 'drift', lags = 11)
    plot(df2.s.dif.desemprego)
    summary(df2.s.dif.desemprego)
    
    #Passo3:
    df3.s.dif.desemprego <- ur.df(s.dif.desemprego, type = 'none', lags = 11)
    plot(df3.s.dif.desemprego)
    summary(df3.s.dif.desemprego)
    
      ## Value of test-statistic is: -3.0269 
    
       ##Critical values for test statistics: 
       ##      1pct  5pct 10pct
       ## tau1 -2.58 -1.95 -1.62
    #Como a estatística calculada é menor que a tau1, então a série não possui raíz unitária e é estacionária
    
    

    
## Formando um data frame com as séries s.dif.desemprego e ipca
    taxa.de.inflação <- ipca[2:128]
data <- data.frame(cbind(s.dif.desemprego,taxa.de.inflação))
plot.ts(data)


## Pré selcionando o modelo

acf(data,36)

VARselect(data, lag.max = 12, type='const')

Var.Est <- VAR(data,p=1,season=NULL, exogen = NULL) 

acf(residuals(Var.Est),36)
summary(Var.Est)
roots(Var.Est)


## SVAR Modelo Tipo A:

N.Restrictions <- function(K){
  R <- K*(K-1)/2
  return(R)}

N.Restrictions(2)

# Geração da Matriz A de Efeitos Feed-Back:

Amat <- diag(2)
Amat[2,1] <-NA 
Amat

AEst <- SVAR(Var.Est, Amat=Amat, Bmat=NULL, hessian=TRUE, estmethod ="direct")
AEst

irf.1 <- irf(AEst, n.ahead=10,  impulse = "s.dif.desemprego", response = ("taxa.de.inflação"))
plot(irf.1)
summary(irf.1)

irf.2<- irf(AEst, n.ahead=20, impulse = "taxa.de.inflação", response = "s.dif.desemprego")     
plot(irf.2)
summary(irf.2)

