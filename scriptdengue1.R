#### bibliotecas ####
library(prais)
library(dplyr)
library(ggplot2)
library(forecast)
library(lubridate)
library(stringr)
library(tidyr)
library(ggpubr)
require(graphics)
library(tseries)
library(zoo)
library(rio)
library(orcutt)


#####prais-winsten####

dados = import(file.choose("C:/R Studio TAE/artigo1_outubro.csv"), encoding = "Latin-1")

attach(dados)

tempo_ts=ts(dados$Ano, start = 2001, freq = 1)

# Incidencia brasil

max(dados$TidengueBRA)
max(dados$TidengueCO)
max(dados$TidengueSUL)
max(dados$TidengueNE)
max(dados$TidengueSUDESTE)
max(dados$TidengueN)

#analise da serie


dados = dados %>%
  +   mutate(log_TidengueBRA = log10(TidengueBRA),log_TidengueCO = log10(TidengueCO),log_TidengueSUL = log10(TidengueSUL),
             +     log_TidengueNE = log10(TidengueNE),log_TidengueSUDESTE= log10(TidengueSUDESTE),log_TidengueN = log10(TidengueN))


# Incidência BRASIL -------------------------------------------------------



pw=prais_winsten(log_TidengueBRA ~ Tempo + Degrau + Rampa,
                 data = dados,
                 index = NULL)
summary(pw)

#estimativas degrau

Degrau = pw$coefficients["Degrau"] # Estimativa (b1) do Degrau
DP_degrau = summary(pw)$coefficients["Degrau", "Std. Error"] # Erro padrão do Degrau

b1_min = Degrau - (1.96 * DP_degrau) # Cálculo do IC inferior
b1_max = Degrau + (1.96 * DP_degrau) # Cálculo do IC superior

paste("Mudança de nível (degrau)", #apresenta o valor da mudança de nível
round((-1+10^Degrau)*100,2),
sep = ": "
)

paste("Mudança de nível (degrau) - P valor", #apresenta o valor da mudança de nível
      round(summary(pw)$coefficients["Degrau", "Pr(>|t|)"],3),
      sep = ": "
)

paste("Mudança de nível (degrau) - IC95%:", #apresenta o valor da mudança de nível
      round((-1+10^b1_min)*100,2),
      "; ",
      round((-1+10^b1_max)*100,2)
)

#estimativas rampa

Rampa = pw$coefficients["Rampa"] # Estimativa (b1) do Rampa
DP_Rampa = summary(pw)$coefficients["Rampa", "Std. Error"] # Erro padrão do Rampa

b1_min = Rampa - (1.96 * DP_Rampa) # Cálculo do IC inferior
b1_max = Rampa + (1.96 * DP_Rampa) # Cálculo do IC superior

paste("Mudança de tendencia (Rampa)", #apresenta o valor da mudança de nível
      round((-1+10^Rampa)*100,2),
      sep = ": "
)

paste("Mudança de tendência (Rampa) - P valor", #apresenta o valor da mudança de nível
      round(summary(pw)$coefficients["Rampa", "Pr(>|t|)"],3),
      sep = ": "
)

paste("Mudança de tendência (Rampa) - IC95%:", #apresenta o valor da mudança de nível
      round((-1+10^b1_min)*100,2),
      "; ",
      round((-1+10^b1_max)*100,2)
)

#estimativa - tempo
Tempo = pw$coefficients["Tempo"] # Estimativa (b1) do Tempo
DP_Tempo = summary(pw)$coefficients["Tempo", "Std. Error"] # Erro padrão do Tempo

b1_min = Tempo - (1.96 * DP_Tempo) # Cálculo do IC inferior
b1_max = Tempo + (1.96 * DP_Tempo) # Cálculo do IC superior

paste("Tendência (Tempo)", #apresenta o valor da mudança de nível
      round((-1+10^Tempo)*100,2),
      sep = ": "
)

paste("Tendência (Tempo) - P valor", #apresenta o valor da mudança de nível
      round(summary(pw)$coefficients["Tempo", "Pr(>|t|)"],3),
      sep = ": "
)

paste("Tendência (Tempo) - IC95%:", #apresenta o valor da mudança de nível
      round((-1+10^b1_min)*100,2),
      "; ",
      round((-1+10^b1_max)*100,2)
)

#encontrando valores previstos
dados$predict_log10=predict.orcutt(pw)
dados$predict=10^dados$predict_log10


# Criando o gráfico

ggplot(dados, aes(x = Ano)) +
  annotate("rect", xmin=2010, xmax=2022, ymin=0, ymax=Inf,  fill="darkgray", alpha=0.3) +
  geom_line(aes(y = TidengueBRA, color = "Taxa de incidência"), size = 1) +
  scale_y_continuous(limits = c(0, 2100)) +
  labs(y = "Taxa de incidência do Brasil",
       x="Ano de notificação") +
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "NULL",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_continuous(breaks = seq(min(dados$Ano), max(dados$Ano), by = 1)) +
  scale_color_manual(values = c("Taxa de incidência" = "black"))+
  guides(color = guide_legend(title = NULL))+
  geom_hline(yintercept = 0, color = "black") +
    geom_vline(xintercept = 2000.5, color = "black") +
  annotate("text", x = 2016, y = 2100, label = "PÓS-PUBLICAÇÃO", size = 2.5, alpha=0.7)



# Incidência CENTRO OESTE -------------------------------------------------------



pw=prais_winsten(log_TidengueCO ~ Tempo + Degrau + Rampa,
                 data = dados,
                 index = NULL)
summary(pw)

#estimativas degrau

Degrau = pw$coefficients["Degrau"] # Estimativa (b1) do Degrau
DP_degrau = summary(pw)$coefficients["Degrau", "Std. Error"] # Erro padrão do Degrau

b1_min = Degrau - (1.96 * DP_degrau) # Cálculo do IC inferior
b1_max = Degrau + (1.96 * DP_degrau) # Cálculo do IC superior

paste("Mudança de nível (degrau)", #apresenta o valor da mudança de nível
      round((-1+10^Degrau)*100,2),
      sep = ": "
)

paste("Mudança de nível (degrau) - P valor", #apresenta o valor da mudança de nível
      round(summary(pw)$coefficients["Degrau", "Pr(>|t|)"],3),
      sep = ": "
)

paste("Mudança de nível (degrau) - IC95%:", #apresenta o valor da mudança de nível
      round((-1+10^b1_min)*100,2),
      "; ",
      round((-1+10^b1_max)*100,2)
)

#estimativas rampa

Rampa = pw$coefficients["Rampa"] # Estimativa (b1) do Rampa
DP_Rampa = summary(pw)$coefficients["Rampa", "Std. Error"] # Erro padrão do Rampa

b1_min = Rampa - (1.96 * DP_Rampa) # Cálculo do IC inferior
b1_max = Rampa + (1.96 * DP_Rampa) # Cálculo do IC superior

paste("Mudança de tendencia (Rampa)", #apresenta o valor da mudança de nível
      round((-1+10^Rampa)*100,2),
      sep = ": "
)

paste("Mudança de tendência (Rampa) - P valor", #apresenta o valor da mudança de nível
      round(summary(pw)$coefficients["Rampa", "Pr(>|t|)"],3),
      sep = ": "
)

paste("Mudança de tendência (Rampa) - IC95%:", #apresenta o valor da mudança de nível
      round((-1+10^b1_min)*100,2),
      "; ",
      round((-1+10^b1_max)*100,2)
)

#estimativa - tempo
Tempo = pw$coefficients["Tempo"] # Estimativa (b1) do Tempo
DP_Tempo = summary(pw)$coefficients["Tempo", "Std. Error"] # Erro padrão do Tempo

b1_min = Tempo - (1.96 * DP_Tempo) # Cálculo do IC inferior
b1_max = Tempo + (1.96 * DP_Tempo) # Cálculo do IC superior

paste("Tendência (Tempo)", #apresenta o valor da mudança de nível
      round((-1+10^Tempo)*100,2),
      sep = ": "
)

paste("Tendência (Tempo) - P valor", #apresenta o valor da mudança de nível
      round(summary(pw)$coefficients["Tempo", "Pr(>|t|)"],3),
      sep = ": "
)

paste("Tendência (Tempo) - IC95%:", #apresenta o valor da mudança de nível
      round((-1+10^b1_min)*100,2),
      "; ",
      round((-1+10^b1_max)*100,2)
)


# Criando o gráfico

ggplot(dados, aes(x = Ano)) +
  annotate("rect", xmin=2010, xmax=2022, ymin=0, ymax=Inf,  fill="darkgray", alpha=0.3) +
  geom_line(aes(y = TidengueCO, color = "Taxa de incidência"), size = 1) +
  scale_y_continuous(limits = c(0, 2100)) +
  labs(y = "Taxa de incidência do Centro-Oeste",
       x="Ano de notificação") +
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "NULL",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_continuous(breaks = seq(min(dados$Ano), max(dados$Ano), by = 1)) +
  scale_color_manual(values = c("Taxa de incidência" = "black"))+
  guides(color = guide_legend(title = NULL))+
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 2000.5, color = "black") +
  annotate("text", x = 2016, y = 2100, label = "PÓS-PUBLICAÇÃO", size = 2.5, alpha=0.7)




# Incidência SUDESTE -------------------------------------------------------



pw=prais_winsten(log_TidengueSUDESTE ~ Tempo + Degrau + Rampa,
                 data = dados,
                 index = NULL)
summary(pw)

#estimativas degrau

Degrau = pw$coefficients["Degrau"] # Estimativa (b1) do Degrau
DP_degrau = summary(pw)$coefficients["Degrau", "Std. Error"] # Erro padrão do Degrau

b1_min = Degrau - (1.96 * DP_degrau) # Cálculo do IC inferior
b1_max = Degrau + (1.96 * DP_degrau) # Cálculo do IC superior

paste("Mudança de nível (degrau)", #apresenta o valor da mudança de nível
      round((-1+10^Degrau)*100,2),
      sep = ": "
)

paste("Mudança de nível (degrau) - P valor", #apresenta o valor da mudança de nível
      round(summary(pw)$coefficients["Degrau", "Pr(>|t|)"],3),
      sep = ": "
)

paste("Mudança de nível (degrau) - IC95%:", #apresenta o valor da mudança de nível
      round((-1+10^b1_min)*100,2),
      "; ",
      round((-1+10^b1_max)*100,2)
)

#estimativas rampa

Rampa = pw$coefficients["Rampa"] # Estimativa (b1) do Rampa
DP_Rampa = summary(pw)$coefficients["Rampa", "Std. Error"] # Erro padrão do Rampa

b1_min = Rampa - (1.96 * DP_Rampa) # Cálculo do IC inferior
b1_max = Rampa + (1.96 * DP_Rampa) # Cálculo do IC superior

paste("Mudança de tendencia (Rampa)", #apresenta o valor da mudança de nível
      round((-1+10^Rampa)*100,2),
      sep = ": "
)

paste("Mudança de tendência (Rampa) - P valor", #apresenta o valor da mudança de nível
      round(summary(pw)$coefficients["Rampa", "Pr(>|t|)"],3),
      sep = ": "
)

paste("Mudança de tendência (Rampa) - IC95%:", #apresenta o valor da mudança de nível
      round((-1+10^b1_min)*100,2),
      "; ",
      round((-1+10^b1_max)*100,2)
)

#estimativa - tempo
Tempo = pw$coefficients["Tempo"] # Estimativa (b1) do Tempo
DP_Tempo = summary(pw)$coefficients["Tempo", "Std. Error"] # Erro padrão do Tempo

b1_min = Tempo - (1.96 * DP_Tempo) # Cálculo do IC inferior
b1_max = Tempo + (1.96 * DP_Tempo) # Cálculo do IC superior

paste("Tendência (Tempo)", #apresenta o valor da mudança de nível
      round((-1+10^Tempo)*100,2),
      sep = ": "
)

paste("Tendência (Tempo) - P valor", #apresenta o valor da mudança de nível
      round(summary(pw)$coefficients["Tempo", "Pr(>|t|)"],3),
      sep = ": "
)

paste("Tendência (Tempo) - IC95%:", #apresenta o valor da mudança de nível
      round((-1+10^b1_min)*100,2),
      "; ",
      round((-1+10^b1_max)*100,2)
)


# Criando o gráfico

ggplot(dados, aes(x = Ano)) +
  annotate("rect", xmin=2010, xmax=2022, ymin=0, ymax=Inf,  fill="darkgray", alpha=0.3) +
  geom_line(aes(y = TidengueSUDESTE, color = "Taxa de incidência"), size = 1) +
  scale_y_continuous(limits = c(0, 2100)) +
  labs(y = "Taxa de incidência do Sudeste",
       x="Ano de notificação") +
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "NULL",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_continuous(breaks = seq(min(dados$Ano), max(dados$Ano), by = 1)) +
  scale_color_manual(values = c("Taxa de incidência" = "black"))+
  guides(color = guide_legend(title = NULL))+
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 2000.5, color = "black") +
  annotate("text", x = 2016, y = 2100, label = "PÓS-PUBLICAÇÃO", size = 2.5, alpha=0.7)


###SUL
# Incidência SUL -------------------------------------------------------



pw=prais_winsten(log_TidengueSUL ~ Tempo + Degrau + Rampa,
                 data = dados,
                 index = NULL)
summary(pw)

#estimativas degrau

Degrau = pw$coefficients["Degrau"] # Estimativa (b1) do Degrau
DP_degrau = summary(pw)$coefficients["Degrau", "Std. Error"] # Erro padrão do Degrau

b1_min = Degrau - (1.96 * DP_degrau) # Cálculo do IC inferior
b1_max = Degrau + (1.96 * DP_degrau) # Cálculo do IC superior

paste("Mudança de nível (degrau)", #apresenta o valor da mudança de nível
      round((-1+10^Degrau)*100,2),
      sep = ": "
)

paste("Mudança de nível (degrau) - P valor", #apresenta o valor da mudança de nível
      round(summary(pw)$coefficients["Degrau", "Pr(>|t|)"],3),
      sep = ": "
)

paste("Mudança de nível (degrau) - IC95%:", #apresenta o valor da mudança de nível
      round((-1+10^b1_min)*100,2),
      "; ",
      round((-1+10^b1_max)*100,2)
)

#estimativas rampa

Rampa = pw$coefficients["Rampa"] # Estimativa (b1) do Rampa
DP_Rampa = summary(pw)$coefficients["Rampa", "Std. Error"] # Erro padrão do Rampa

b1_min = Rampa - (1.96 * DP_Rampa) # Cálculo do IC inferior
b1_max = Rampa + (1.96 * DP_Rampa) # Cálculo do IC superior

paste("Mudança de tendencia (Rampa)", #apresenta o valor da mudança de nível
      round((-1+10^Rampa)*100,2),
      sep = ": "
)

paste("Mudança de tendência (Rampa) - P valor", #apresenta o valor da mudança de nível
      round(summary(pw)$coefficients["Rampa", "Pr(>|t|)"],3),
      sep = ": "
)

paste("Mudança de tendência (Rampa) - IC95%:", #apresenta o valor da mudança de nível
      round((-1+10^b1_min)*100,2),
      "; ",
      round((-1+10^b1_max)*100,2)
)

#estimativa - tempo
Tempo = pw$coefficients["Tempo"] # Estimativa (b1) do Tempo
DP_Tempo = summary(pw)$coefficients["Tempo", "Std. Error"] # Erro padrão do Tempo

b1_min = Tempo - (1.96 * DP_Tempo) # Cálculo do IC inferior
b1_max = Tempo + (1.96 * DP_Tempo) # Cálculo do IC superior

paste("Tendência (Tempo)", #apresenta o valor da mudança de nível
      round((-1+10^Tempo)*100,2),
      sep = ": "
)

paste("Tendência (Tempo) - P valor", #apresenta o valor da mudança de nível
      round(summary(pw)$coefficients["Tempo", "Pr(>|t|)"],3),
      sep = ": "
)

paste("Tendência (Tempo) - IC95%:", #apresenta o valor da mudança de nível
      round((-1+10^b1_min)*100,2),
      "; ",
      round((-1+10^b1_max)*100,2)
)


# Criando o gráfico

ggplot(dados, aes(x = Ano)) +
  annotate("rect", xmin=2010, xmax=2022, ymin=0, ymax=Inf,  fill="darkgray", alpha=0.3) +
  geom_line(aes(y = TidengueSUL, color = "Taxa de incidência"), size = 1) +
  scale_y_continuous(limits = c(0, 2100)) +
  labs(y = "Taxa de incidência do Sul",
       x="Ano de notificação") +
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "NULL",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_continuous(breaks = seq(min(dados$Ano), max(dados$Ano), by = 1)) +
  scale_color_manual(values = c("Taxa de incidência" = "black"))+
  guides(color = guide_legend(title = NULL))+
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 2000.5, color = "black") +
  annotate("text", x = 2016, y = 2100, label = "PÓS-PUBLICAÇÃO", size = 2.5, alpha=0.7)


### NORDESTE
# Incidência Nordeste -------------------------------------------------------



pw=prais_winsten(log_TidengueNE ~ Tempo + Degrau + Rampa,
                 data = dados,
                 index = NULL)
summary(pw)

#estimativas degrau

Degrau = pw$coefficients["Degrau"] # Estimativa (b1) do Degrau
DP_degrau = summary(pw)$coefficients["Degrau", "Std. Error"] # Erro padrão do Degrau

b1_min = Degrau - (1.96 * DP_degrau) # Cálculo do IC inferior
b1_max = Degrau + (1.96 * DP_degrau) # Cálculo do IC superior

paste("Mudança de nível (degrau)", #apresenta o valor da mudança de nível
      round((-1+10^Degrau)*100,2),
      sep = ": "
)

paste("Mudança de nível (degrau) - P valor", #apresenta o valor da mudança de nível
      round(summary(pw)$coefficients["Degrau", "Pr(>|t|)"],3),
      sep = ": "
)

paste("Mudança de nível (degrau) - IC95%:", #apresenta o valor da mudança de nível
      round((-1+10^b1_min)*100,2),
      "; ",
      round((-1+10^b1_max)*100,2)
)

#estimativas rampa

Rampa = pw$coefficients["Rampa"] # Estimativa (b1) do Rampa
DP_Rampa = summary(pw)$coefficients["Rampa", "Std. Error"] # Erro padrão do Rampa

b1_min = Rampa - (1.96 * DP_Rampa) # Cálculo do IC inferior
b1_max = Rampa + (1.96 * DP_Rampa) # Cálculo do IC superior

paste("Mudança de tendencia (Rampa)", #apresenta o valor da mudança de nível
      round((-1+10^Rampa)*100,2),
      sep = ": "
)

paste("Mudança de tendência (Rampa) - P valor", #apresenta o valor da mudança de nível
      round(summary(pw)$coefficients["Rampa", "Pr(>|t|)"],3),
      sep = ": "
)

paste("Mudança de tendência (Rampa) - IC95%:", #apresenta o valor da mudança de nível
      round((-1+10^b1_min)*100,2),
      "; ",
      round((-1+10^b1_max)*100,2)
)

#estimativa - tempo
Tempo = pw$coefficients["Tempo"] # Estimativa (b1) do Tempo
DP_Tempo = summary(pw)$coefficients["Tempo", "Std. Error"] # Erro padrão do Tempo

b1_min = Tempo - (1.96 * DP_Tempo) # Cálculo do IC inferior
b1_max = Tempo + (1.96 * DP_Tempo) # Cálculo do IC superior

paste("Tendência (Tempo)", #apresenta o valor da mudança de nível
      round((-1+10^Tempo)*100,2),
      sep = ": "
)

paste("Tendência (Tempo) - P valor", #apresenta o valor da mudança de nível
      round(summary(pw)$coefficients["Tempo", "Pr(>|t|)"],3),
      sep = ": "
)

paste("Tendência (Tempo) - IC95%:", #apresenta o valor da mudança de nível
      round((-1+10^b1_min)*100,2),
      "; ",
      round((-1+10^b1_max)*100,2)
)


# Criando o gráfico

ggplot(dados, aes(x = Ano)) +
  annotate("rect", xmin=2010, xmax=2022, ymin=0, ymax=Inf,  fill="darkgray", alpha=0.3) +
  geom_line(aes(y = TidengueNE, color = "Taxa de incidência"), size = 1) +
  scale_y_continuous(limits = c(0, 2100)) +
  labs(y = "Taxa de incidência do Nordeste",
       x="Ano de notificação") +
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "NULL",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_continuous(breaks = seq(min(dados$Ano), max(dados$Ano), by = 1)) +
  scale_color_manual(values = c("Taxa de incidência" = "black"))+
  guides(color = guide_legend(title = NULL))+
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 2000.5, color = "black") +
  annotate("text", x = 2016, y = 2100, label = "PÓS-PUBLICAÇÃO", size = 2.5, alpha=0.7)


### NORTE
# Incidência NORTE -------------------------------------------------------



pw=prais_winsten(log_TidengueN ~ Tempo + Degrau + Rampa,
                 data = dados,
                 index = NULL)
summary(pw)

#estimativas degrau

Degrau = pw$coefficients["Degrau"] # Estimativa (b1) do Degrau
DP_degrau = summary(pw)$coefficients["Degrau", "Std. Error"] # Erro padrão do Degrau

b1_min = Degrau - (1.96 * DP_degrau) # Cálculo do IC inferior
b1_max = Degrau + (1.96 * DP_degrau) # Cálculo do IC superior

paste("Mudança de nível (degrau)", #apresenta o valor da mudança de nível
      round((-1+10^Degrau)*100,2),
      sep = ": "
)

paste("Mudança de nível (degrau) - P valor", #apresenta o valor da mudança de nível
      round(summary(pw)$coefficients["Degrau", "Pr(>|t|)"],3),
      sep = ": "
)

paste("Mudança de nível (degrau) - IC95%:", #apresenta o valor da mudança de nível
      round((-1+10^b1_min)*100,2),
      "; ",
      round((-1+10^b1_max)*100,2)
)

#estimativas rampa

Rampa = pw$coefficients["Rampa"] # Estimativa (b1) do Rampa
DP_Rampa = summary(pw)$coefficients["Rampa", "Std. Error"] # Erro padrão do Rampa

b1_min = Rampa - (1.96 * DP_Rampa) # Cálculo do IC inferior
b1_max = Rampa + (1.96 * DP_Rampa) # Cálculo do IC superior

paste("Mudança de tendencia (Rampa)", #apresenta o valor da mudança de nível
      round((-1+10^Rampa)*100,2),
      sep = ": "
)

paste("Mudança de tendência (Rampa) - P valor", #apresenta o valor da mudança de nível
      round(summary(pw)$coefficients["Rampa", "Pr(>|t|)"],3),
      sep = ": "
)

paste("Mudança de tendência (Rampa) - IC95%:", #apresenta o valor da mudança de nível
      round((-1+10^b1_min)*100,2),
      "; ",
      round((-1+10^b1_max)*100,2)
)

#estimativa - tempo
Tempo = pw$coefficients["Tempo"] # Estimativa (b1) do Tempo
DP_Tempo = summary(pw)$coefficients["Tempo", "Std. Error"] # Erro padrão do Tempo

b1_min = Tempo - (1.96 * DP_Tempo) # Cálculo do IC inferior
b1_max = Tempo + (1.96 * DP_Tempo) # Cálculo do IC superior

paste("Tendência (Tempo)", #apresenta o valor da mudança de nível
      round((-1+10^Tempo)*100,2),
      sep = ": "
)

paste("Tendência (Tempo) - P valor", #apresenta o valor da mudança de nível
      round(summary(pw)$coefficients["Tempo", "Pr(>|t|)"],3),
      sep = ": "
)

paste("Tendência (Tempo) - IC95%:", #apresenta o valor da mudança de nível
      round((-1+10^b1_min)*100,2),
      "; ",
      round((-1+10^b1_max)*100,2)
)


# Criando o gráfico

ggplot(dados, aes(x = Ano)) +
  annotate("rect", xmin=2010, xmax=2022, ymin=0, ymax=Inf,  fill="darkgray", alpha=0.3) +
  geom_line(aes(y = TidengueN, color = "Taxa de incidência"), size = 1) +
  scale_y_continuous(limits = c(0, 2100)) +
  labs(y = "Taxa de incidência do Norte",
       x="Ano de notificação") +
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "NULL",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_continuous(breaks = seq(min(dados$Ano), max(dados$Ano), by = 1)) +
  scale_color_manual(values = c("Taxa de incidência" = "black"))+
  guides(color = guide_legend(title = NULL))+
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 2000.5, color = "black") +
  annotate("text", x = 2016, y = 2100, label = "PÓS-PUBLICAÇÃO", size = 2.5, alpha=0.7)

######## LETALIDADE

# Letalidade no Brasil

max(dados$TLetdengueBRA)
max(dados$TLetdengueSUL)
max(dados$TLetdengueSUDESTE)
max(dados$TLetdengueNE)
max(dados$TLetdengueCO)
max(dados$TLetdengueN)

#analise da serie


dados = dados %>%
  mutate(log_TLetdengueBRA = log10(TLetdengueBRA),log_TLetdengueCO = log10(TLetdengueCO),log_TLetdengueSUL = log10(TLetdengueSUL), log_TLetdengueNE = log10(TLetdengueNE),log_TLetdengueSUDESTE= log10(TLetdengueSUDESTE),log_TLetdengueN = log10(TLetdengueN))


# Letalidade BRASIL -------------------------------------------------------



pw=prais_winsten(log_TLetdengueBRA ~ Tempo + Degrau + Rampa,
                 data = dados,
                 index = NULL)
summary(pw)

#estimativas degrau

Degrau = pw$coefficients["Degrau"] # Estimativa (b1) do Degrau
DP_degrau = summary(pw)$coefficients["Degrau", "Std. Error"] # Erro padrão do Degrau

b1_min = Degrau - (1.96 * DP_degrau) # Cálculo do IC inferior
b1_max = Degrau + (1.96 * DP_degrau) # Cálculo do IC superior

paste("Mudança de nível (degrau)", #apresenta o valor da mudança de nível
      round((-1+10^Degrau)*100,2),
      sep = ": "
)

paste("Mudança de nível (degrau) - P valor", #apresenta o valor da mudança de nível
      round(summary(pw)$coefficients["Degrau", "Pr(>|t|)"],3),
      sep = ": "
)

paste("Mudança de nível (degrau) - IC95%:", #apresenta o valor da mudança de nível
      round((-1+10^b1_min)*100,2),
      "; ",
      round((-1+10^b1_max)*100,2)
)

#estimativas rampa

Rampa = pw$coefficients["Rampa"] # Estimativa (b1) do Rampa
DP_Rampa = summary(pw)$coefficients["Rampa", "Std. Error"] # Erro padrão do Rampa

b1_min = Rampa - (1.96 * DP_Rampa) # Cálculo do IC inferior
b1_max = Rampa + (1.96 * DP_Rampa) # Cálculo do IC superior

paste("Mudança de tendencia (Rampa)", #apresenta o valor da mudança de nível
      round((-1+10^Rampa)*100,2),
      sep = ": "
)

paste("Mudança de tendência (Rampa) - P valor", #apresenta o valor da mudança de nível
      round(summary(pw)$coefficients["Rampa", "Pr(>|t|)"],3),
      sep = ": "
)

paste("Mudança de tendência (Rampa) - IC95%:", #apresenta o valor da mudança de nível
      round((-1+10^b1_min)*100,2),
      "; ",
      round((-1+10^b1_max)*100,2)
)

#estimativa - tempo
Tempo = pw$coefficients["Tempo"] # Estimativa (b1) do Tempo
DP_Tempo = summary(pw)$coefficients["Tempo", "Std. Error"] # Erro padrão do Tempo

b1_min = Tempo - (1.96 * DP_Tempo) # Cálculo do IC inferior
b1_max = Tempo + (1.96 * DP_Tempo) # Cálculo do IC superior

paste("Tendência (Tempo)", #apresenta o valor da mudança de nível
      round((-1+10^Tempo)*100,2),
      sep = ": "
)

paste("Tendência (Tempo) - P valor", #apresenta o valor da mudança de nível
      round(summary(pw)$coefficients["Tempo", "Pr(>|t|)"],3),
      sep = ": "
)

paste("Tendência (Tempo) - IC95%:", #apresenta o valor da mudança de nível
      round((-1+10^b1_min)*100,2),
      "; ",
      round((-1+10^b1_max)*100,2)
)

#encontrando valores previstos
dados$predict_log10=predict.orcutt(pw)
dados$predict=10^dados$predict_log10


# Criando o gráfico

ggplot(dados, aes(x = Ano)) +
  annotate("rect", xmin=2010, xmax=2022, ymin=0, ymax=Inf,  fill="darkgray", alpha=0.3) +
  geom_line(aes(y = TLetdengueBRA, color = "Taxa de letalidade"), size = 1) +
  scale_y_continuous(limits = c(0, 0.55)) +
  labs(y = "Taxa de letalidade no Brasil",
       x="Ano") +
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "NULL",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_continuous(breaks = seq(min(dados$Ano), max(dados$Ano), by = 1)) +
  scale_color_manual(values = c("Taxa de letalidade" = "black"))+
  guides(color = guide_legend(title = NULL))+
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 2000.5, color = "black") +
  annotate("text", x = 2016, y = 0.55, label = "PÓS-PUBLICAÇÃO", size = 2.5, alpha=0.7)


## Letalidade SUL

pw=prais_winsten(log_TLetdengueSUL ~ Tempo + Degrau + Rampa,
                 data = dados,
                 index = NULL)
summary(pw)

#estimativas degrau

Degrau = pw$coefficients["Degrau"] # Estimativa (b1) do Degrau
DP_degrau = summary(pw)$coefficients["Degrau", "Std. Error"] # Erro padrão do Degrau

b1_min = Degrau - (1.96 * DP_degrau) # Cálculo do IC inferior
b1_max = Degrau + (1.96 * DP_degrau) # Cálculo do IC superior

paste("Mudança de nível (degrau)", #apresenta o valor da mudança de nível
      round((-1+10^Degrau)*100,2),
      sep = ": "
)

paste("Mudança de nível (degrau) - P valor", #apresenta o valor da mudança de nível
      round(summary(pw)$coefficients["Degrau", "Pr(>|t|)"],3),
      sep = ": "
)

paste("Mudança de nível (degrau) - IC95%:", #apresenta o valor da mudança de nível
      round((-1+10^b1_min)*100,2),
      "; ",
      round((-1+10^b1_max)*100,2)
)

#estimativas rampa

Rampa = pw$coefficients["Rampa"] # Estimativa (b1) do Rampa
DP_Rampa = summary(pw)$coefficients["Rampa", "Std. Error"] # Erro padrão do Rampa

b1_min = Rampa - (1.96 * DP_Rampa) # Cálculo do IC inferior
b1_max = Rampa + (1.96 * DP_Rampa) # Cálculo do IC superior

paste("Mudança de tendencia (Rampa)", #apresenta o valor da mudança de nível
      round((-1+10^Rampa)*100,2),
      sep = ": "
)

paste("Mudança de tendência (Rampa) - P valor", #apresenta o valor da mudança de nível
      round(summary(pw)$coefficients["Rampa", "Pr(>|t|)"],3),
      sep = ": "
)

paste("Mudança de tendência (Rampa) - IC95%:", #apresenta o valor da mudança de nível
      round((-1+10^b1_min)*100,2),
      "; ",
      round((-1+10^b1_max)*100,2)
)

#estimativa - tempo
Tempo = pw$coefficients["Tempo"] # Estimativa (b1) do Tempo
DP_Tempo = summary(pw)$coefficients["Tempo", "Std. Error"] # Erro padrão do Tempo

b1_min = Tempo - (1.96 * DP_Tempo) # Cálculo do IC inferior
b1_max = Tempo + (1.96 * DP_Tempo) # Cálculo do IC superior

paste("Tendência (Tempo)", #apresenta o valor da mudança de nível
      round((-1+10^Tempo)*100,2),
      sep = ": "
)

paste("Tendência (Tempo) - P valor", #apresenta o valor da mudança de nível
      round(summary(pw)$coefficients["Tempo", "Pr(>|t|)"],3),
      sep = ": "
)

paste("Tendência (Tempo) - IC95%:", #apresenta o valor da mudança de nível
      round((-1+10^b1_min)*100,2),
      "; ",
      round((-1+10^b1_max)*100,2)
)

#encontrando valores previstos
dados$predict_log10=predict.orcutt(pw)
dados$predict=10^dados$predict_log10


# Criando o gráfico

ggplot(dados, aes(x = Ano)) +
  annotate("rect", xmin=2010, xmax=2022, ymin=0, ymax=Inf,  fill="darkgray", alpha=0.3) +
  geom_line(aes(y = TLetdengueSUL, color = "Taxa de letalidade"), size = 1) +
  scale_y_continuous(limits = c(0, 0.55)) +
  labs(y = "Taxa de letalidade no Sul",
       x="Ano") +
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "NULL",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_continuous(breaks = seq(min(dados$Ano), max(dados$Ano), by = 1)) +
  scale_color_manual(values = c("Taxa de letalidade" = "black"))+
  guides(color = guide_legend(title = NULL))+
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 2000.5, color = "black") +
  annotate("text", x = 2016, y = 0.55, label = "PÓS-PUBLICAÇÃO", size = 2.5, alpha=0.7)



## SUDESTE
pw=prais_winsten(log_TLetdengueSUDESTE ~ Tempo + Degrau + Rampa,
                 data = dados,
                 index = NULL)
summary(pw)

#estimativas degrau

Degrau = pw$coefficients["Degrau"] # Estimativa (b1) do Degrau
DP_degrau = summary(pw)$coefficients["Degrau", "Std. Error"] # Erro padrão do Degrau

b1_min = Degrau - (1.96 * DP_degrau) # Cálculo do IC inferior
b1_max = Degrau + (1.96 * DP_degrau) # Cálculo do IC superior

paste("Mudança de nível (degrau)", #apresenta o valor da mudança de nível
      round((-1+10^Degrau)*100,2),
      sep = ": "
)

paste("Mudança de nível (degrau) - P valor", #apresenta o valor da mudança de nível
      round(summary(pw)$coefficients["Degrau", "Pr(>|t|)"],3),
      sep = ": "
)

paste("Mudança de nível (degrau) - IC95%:", #apresenta o valor da mudança de nível
      round((-1+10^b1_min)*100,2),
      "; ",
      round((-1+10^b1_max)*100,2)
)

#estimativas rampa

Rampa = pw$coefficients["Rampa"] # Estimativa (b1) do Rampa
DP_Rampa = summary(pw)$coefficients["Rampa", "Std. Error"] # Erro padrão do Rampa

b1_min = Rampa - (1.96 * DP_Rampa) # Cálculo do IC inferior
b1_max = Rampa + (1.96 * DP_Rampa) # Cálculo do IC superior

paste("Mudança de tendencia (Rampa)", #apresenta o valor da mudança de nível
      round((-1+10^Rampa)*100,2),
      sep = ": "
)

paste("Mudança de tendência (Rampa) - P valor", #apresenta o valor da mudança de nível
      round(summary(pw)$coefficients["Rampa", "Pr(>|t|)"],3),
      sep = ": "
)

paste("Mudança de tendência (Rampa) - IC95%:", #apresenta o valor da mudança de nível
      round((-1+10^b1_min)*100,2),
      "; ",
      round((-1+10^b1_max)*100,2)
)

#estimativa - tempo
Tempo = pw$coefficients["Tempo"] # Estimativa (b1) do Tempo
DP_Tempo = summary(pw)$coefficients["Tempo", "Std. Error"] # Erro padrão do Tempo

b1_min = Tempo - (1.96 * DP_Tempo) # Cálculo do IC inferior
b1_max = Tempo + (1.96 * DP_Tempo) # Cálculo do IC superior

paste("Tendência (Tempo)", #apresenta o valor da mudança de nível
      round((-1+10^Tempo)*100,2),
      sep = ": "
)

paste("Tendência (Tempo) - P valor", #apresenta o valor da mudança de nível
      round(summary(pw)$coefficients["Tempo", "Pr(>|t|)"],3),
      sep = ": "
)

paste("Tendência (Tempo) - IC95%:", #apresenta o valor da mudança de nível
      round((-1+10^b1_min)*100,2),
      "; ",
      round((-1+10^b1_max)*100,2)
)

#encontrando valores previstos
dados$predict_log10=predict.orcutt(pw)
dados$predict=10^dados$predict_log10


# Criando o gráfico

ggplot(dados, aes(x = Ano)) +
  annotate("rect", xmin=2010, xmax=2022, ymin=0, ymax=Inf,  fill="darkgray", alpha=0.3) +
  geom_line(aes(y = TLetdengueSUDESTE, color = "Taxa de letalidade"), size = 1) +
  scale_y_continuous(limits = c(0, 0.55)) +
  labs(y = "Taxa de letalidade no Sudeste",
       x="Ano") +
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "NULL",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_continuous(breaks = seq(min(dados$Ano), max(dados$Ano), by = 1)) +
  scale_color_manual(values = c("Taxa de letalidade" = "black"))+
  guides(color = guide_legend(title = NULL))+
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 2000.5, color = "black") +
  annotate("text", x = 2016, y = 0.55, label = "PÓS-PUBLICAÇÃO", size = 2.5, alpha=0.7)

## Letalidade NORDESTE
pw=prais_winsten(log_TLetdengueNE ~ Tempo + Degrau + Rampa,
                 data = dados,
                 index = NULL)
summary(pw)

#estimativas degrau

Degrau = pw$coefficients["Degrau"] # Estimativa (b1) do Degrau
DP_degrau = summary(pw)$coefficients["Degrau", "Std. Error"] # Erro padrão do Degrau

b1_min = Degrau - (1.96 * DP_degrau) # Cálculo do IC inferior
b1_max = Degrau + (1.96 * DP_degrau) # Cálculo do IC superior

paste("Mudança de nível (degrau)", #apresenta o valor da mudança de nível
      round((-1+10^Degrau)*100,2),
      sep = ": "
)

paste("Mudança de nível (degrau) - P valor", #apresenta o valor da mudança de nível
      round(summary(pw)$coefficients["Degrau", "Pr(>|t|)"],3),
      sep = ": "
)

paste("Mudança de nível (degrau) - IC95%:", #apresenta o valor da mudança de nível
      round((-1+10^b1_min)*100,2),
      "; ",
      round((-1+10^b1_max)*100,2)
)

#estimativas rampa

Rampa = pw$coefficients["Rampa"] # Estimativa (b1) do Rampa
DP_Rampa = summary(pw)$coefficients["Rampa", "Std. Error"] # Erro padrão do Rampa

b1_min = Rampa - (1.96 * DP_Rampa) # Cálculo do IC inferior
b1_max = Rampa + (1.96 * DP_Rampa) # Cálculo do IC superior

paste("Mudança de tendencia (Rampa)", #apresenta o valor da mudança de nível
      round((-1+10^Rampa)*100,2),
      sep = ": "
)

paste("Mudança de tendência (Rampa) - P valor", #apresenta o valor da mudança de nível
      round(summary(pw)$coefficients["Rampa", "Pr(>|t|)"],3),
      sep = ": "
)

paste("Mudança de tendência (Rampa) - IC95%:", #apresenta o valor da mudança de nível
      round((-1+10^b1_min)*100,2),
      "; ",
      round((-1+10^b1_max)*100,2)
)

#estimativa - tempo
Tempo = pw$coefficients["Tempo"] # Estimativa (b1) do Tempo
DP_Tempo = summary(pw)$coefficients["Tempo", "Std. Error"] # Erro padrão do Tempo

b1_min = Tempo - (1.96 * DP_Tempo) # Cálculo do IC inferior
b1_max = Tempo + (1.96 * DP_Tempo) # Cálculo do IC superior

paste("Tendência (Tempo)", #apresenta o valor da mudança de nível
      round((-1+10^Tempo)*100,2),
      sep = ": "
)

paste("Tendência (Tempo) - P valor", #apresenta o valor da mudança de nível
      round(summary(pw)$coefficients["Tempo", "Pr(>|t|)"],3),
      sep = ": "
)

paste("Tendência (Tempo) - IC95%:", #apresenta o valor da mudança de nível
      round((-1+10^b1_min)*100,2),
      "; ",
      round((-1+10^b1_max)*100,2)
)

#encontrando valores previstos
dados$predict_log10=predict.orcutt(pw)
dados$predict=10^dados$predict_log10


# Criando o gráfico

ggplot(dados, aes(x = Ano)) +
  annotate("rect", xmin=2010, xmax=2022, ymin=0, ymax=Inf,  fill="darkgray", alpha=0.3) +
  geom_line(aes(y = TLetdengueNE, color = "Taxa de letalidade"), size = 1) +
  scale_y_continuous(limits = c(0, 0.55)) +
  labs(y = "Taxa de letalidade no Nordeste",
       x="Ano") +
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "NULL",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_continuous(breaks = seq(min(dados$Ano), max(dados$Ano), by = 1)) +
  scale_color_manual(values = c("Taxa de letalidade" = "black"))+
  guides(color = guide_legend(title = NULL))+
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 2000.5, color = "black") +
  annotate("text", x = 2016, y = 0.55, label = "PÓS-PUBLICAÇÃO", size = 2.5, alpha=0.7)

##letalidade CENTRO-OESTE
pw=prais_winsten(log_TLetdengueCO ~ Tempo + Degrau + Rampa,
                 data = dados,
                 index = NULL)
summary(pw)

#estimativas degrau

Degrau = pw$coefficients["Degrau"] # Estimativa (b1) do Degrau
DP_degrau = summary(pw)$coefficients["Degrau", "Std. Error"] # Erro padrão do Degrau

b1_min = Degrau - (1.96 * DP_degrau) # Cálculo do IC inferior
b1_max = Degrau + (1.96 * DP_degrau) # Cálculo do IC superior

paste("Mudança de nível (degrau)", #apresenta o valor da mudança de nível
      round((-1+10^Degrau)*100,2),
      sep = ": "
)

paste("Mudança de nível (degrau) - P valor", #apresenta o valor da mudança de nível
      round(summary(pw)$coefficients["Degrau", "Pr(>|t|)"],3),
      sep = ": "
)

paste("Mudança de nível (degrau) - IC95%:", #apresenta o valor da mudança de nível
      round((-1+10^b1_min)*100,2),
      "; ",
      round((-1+10^b1_max)*100,2)
)

#estimativas rampa

Rampa = pw$coefficients["Rampa"] # Estimativa (b1) do Rampa
DP_Rampa = summary(pw)$coefficients["Rampa", "Std. Error"] # Erro padrão do Rampa

b1_min = Rampa - (1.96 * DP_Rampa) # Cálculo do IC inferior
b1_max = Rampa + (1.96 * DP_Rampa) # Cálculo do IC superior

paste("Mudança de tendencia (Rampa)", #apresenta o valor da mudança de nível
      round((-1+10^Rampa)*100,2),
      sep = ": "
)

paste("Mudança de tendência (Rampa) - P valor", #apresenta o valor da mudança de nível
      round(summary(pw)$coefficients["Rampa", "Pr(>|t|)"],3),
      sep = ": "
)

paste("Mudança de tendência (Rampa) - IC95%:", #apresenta o valor da mudança de nível
      round((-1+10^b1_min)*100,2),
      "; ",
      round((-1+10^b1_max)*100,2)
)

#estimativa - tempo
Tempo = pw$coefficients["Tempo"] # Estimativa (b1) do Tempo
DP_Tempo = summary(pw)$coefficients["Tempo", "Std. Error"] # Erro padrão do Tempo

b1_min = Tempo - (1.96 * DP_Tempo) # Cálculo do IC inferior
b1_max = Tempo + (1.96 * DP_Tempo) # Cálculo do IC superior

paste("Tendência (Tempo)", #apresenta o valor da mudança de nível
      round((-1+10^Tempo)*100,2),
      sep = ": "
)

paste("Tendência (Tempo) - P valor", #apresenta o valor da mudança de nível
      round(summary(pw)$coefficients["Tempo", "Pr(>|t|)"],3),
      sep = ": "
)

paste("Tendência (Tempo) - IC95%:", #apresenta o valor da mudança de nível
      round((-1+10^b1_min)*100,2),
      "; ",
      round((-1+10^b1_max)*100,2)
)

#encontrando valores previstos
dados$predict_log10=predict.orcutt(pw)
dados$predict=10^dados$predict_log10


# Criando o gráfico

ggplot(dados, aes(x = Ano)) +
  annotate("rect", xmin=2010, xmax=2022, ymin=0, ymax=Inf,  fill="darkgray", alpha=0.3) +
  geom_line(aes(y = TLetdengueCO, color = "Taxa de letalidade"), size = 1) +
  scale_y_continuous(limits = c(0, 0.55)) +
  labs(y = "Taxa de letalidade no Centro-Oeste",
       x="Ano") +
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "NULL",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_continuous(breaks = seq(min(dados$Ano), max(dados$Ano), by = 1)) +
  scale_color_manual(values = c("Taxa de letalidade" = "black"))+
  guides(color = guide_legend(title = NULL))+
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 2000.5, color = "black") +
  annotate("text", x = 2016, y = 0.55, label = "PÓS-PUBLICAÇÃO", size = 2.5, alpha=0.7)

## letalidade NORTE
pw=prais_winsten(log_TLetdengueN ~ Tempo + Degrau + Rampa,
                 data = dados,
                 index = NULL)
summary(pw)

#estimativas degrau

Degrau = pw$coefficients["Degrau"] # Estimativa (b1) do Degrau
DP_degrau = summary(pw)$coefficients["Degrau", "Std. Error"] # Erro padrão do Degrau

b1_min = Degrau - (1.96 * DP_degrau) # Cálculo do IC inferior
b1_max = Degrau + (1.96 * DP_degrau) # Cálculo do IC superior

paste("Mudança de nível (degrau)", #apresenta o valor da mudança de nível
      round((-1+10^Degrau)*100,2),
      sep = ": "
)

paste("Mudança de nível (degrau) - P valor", #apresenta o valor da mudança de nível
      round(summary(pw)$coefficients["Degrau", "Pr(>|t|)"],3),
      sep = ": "
)

paste("Mudança de nível (degrau) - IC95%:", #apresenta o valor da mudança de nível
      round((-1+10^b1_min)*100,2),
      "; ",
      round((-1+10^b1_max)*100,2)
)

#estimativas rampa

Rampa = pw$coefficients["Rampa"] # Estimativa (b1) do Rampa
DP_Rampa = summary(pw)$coefficients["Rampa", "Std. Error"] # Erro padrão do Rampa

b1_min = Rampa - (1.96 * DP_Rampa) # Cálculo do IC inferior
b1_max = Rampa + (1.96 * DP_Rampa) # Cálculo do IC superior

paste("Mudança de tendencia (Rampa)", #apresenta o valor da mudança de nível
      round((-1+10^Rampa)*100,2),
      sep = ": "
)

paste("Mudança de tendência (Rampa) - P valor", #apresenta o valor da mudança de nível
      round(summary(pw)$coefficients["Rampa", "Pr(>|t|)"],3),
      sep = ": "
)

paste("Mudança de tendência (Rampa) - IC95%:", #apresenta o valor da mudança de nível
      round((-1+10^b1_min)*100,2),
      "; ",
      round((-1+10^b1_max)*100,2)
)

#estimativa - tempo
Tempo = pw$coefficients["Tempo"] # Estimativa (b1) do Tempo
DP_Tempo = summary(pw)$coefficients["Tempo", "Std. Error"] # Erro padrão do Tempo

b1_min = Tempo - (1.96 * DP_Tempo) # Cálculo do IC inferior
b1_max = Tempo + (1.96 * DP_Tempo) # Cálculo do IC superior

paste("Tendência (Tempo)", #apresenta o valor da mudança de nível
      round((-1+10^Tempo)*100,2),
      sep = ": "
)

paste("Tendência (Tempo) - P valor", #apresenta o valor da mudança de nível
      round(summary(pw)$coefficients["Tempo", "Pr(>|t|)"],3),
      sep = ": "
)

paste("Tendência (Tempo) - IC95%:", #apresenta o valor da mudança de nível
      round((-1+10^b1_min)*100,2),
      "; ",
      round((-1+10^b1_max)*100,2)
)

#encontrando valores previstos
dados$predict_log10=predict.orcutt(pw)
dados$predict=10^dados$predict_log10


# Criando o gráfico

ggplot(dados, aes(x = Ano)) +
  annotate("rect", xmin=2010, xmax=2022, ymin=0, ymax=Inf,  fill="darkgray", alpha=0.3) +
  geom_line(aes(y = TLetdengueN, color = "Taxa de letalidade"), size = 1) +
  scale_y_continuous(limits = c(0, 0.55)) +
  labs(y = "Taxa de letalidade no Norte",
       x="Ano") +
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "NULL",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_continuous(breaks = seq(min(dados$Ano), max(dados$Ano), by = 1)) +
  scale_color_manual(values = c("Taxa de letalidade" = "black"))+
  guides(color = guide_legend(title = NULL))+
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 2000.5, color = "black") +
  annotate("text", x = 2016, y = 0.55, label = "PÓS-PUBLICAÇÃO", size = 2.5, alpha=0.7)


