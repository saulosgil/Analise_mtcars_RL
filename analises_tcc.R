# lendo pacotes

library(ggplot2)
library(patchwork)
library(corrplot)

#  Lendo e visualizando ---------------------------------------------------

mtcars <- mtcars # lendo a base

View(mtcars) # visualizando a base


#  verificando a presença de dados faltantes ------------------------------

mice::md.pattern(mtcars)

# Tabela descritiva -------------------------------------------------------

mtcars |>
  dplyr::select(where(is.numeric)) |>
  tidyr::pivot_longer(cols = everything()) |>
  dplyr::group_by(name) |>
  dplyr::summarise_at("value",
                      list(
                        Paramêtro = ~min(.),
                        Q1 = ~quantile(., probs = 0.25),
                        Mediana = ~median(.),
                        Média = ~mean(.),
                        "Desvio Padrão" = ~sd(.),
                        Q3 = ~quantile(., probs = 0.75),
                        maximo = ~max(.))) |>
  dplyr::mutate_if(is.numeric,
                   format,
                   digits = 2,
                   nsmall = 2
  ) |>
  kableExtra::kable(caption = "Tabela 1. Estatistíca descritiva de cada variável.") |>


# Grafícos de dispersão para verificar "nuvem" ----------------------------

# mpg vs cyl

mpg_cyl <-
  mtcars |>
  ggplot(mapping = aes(x = cyl,
                       y = mpg)) +
  geom_point(show.legend = FALSE,
             color = "blue") +
  geom_smooth(method = "lm",
              show.legend = FALSE,
              color = "red") +
  labs(title = "mpg vs. cyl") +
  theme_classic()

mpg_cyl

# mpg vs disp

mpg_disp <-
  mtcars |>
  ggplot(mapping = aes(x = disp,
                       y = mpg)) +
  geom_point(show.legend = FALSE,
             color = "blue") +
  geom_smooth(method = "lm",
              show.legend = FALSE,
              color = "red") +
  labs(title = "mpg vs. disp") +
  theme_classic()

mpg_disp

# mpg vs hp

mpg_hp <-
  mtcars |>
  ggplot(mapping = aes(x = hp,
                       y = mpg)) +
  geom_point(show.legend = FALSE,
             color = "blue") +
  geom_smooth(method = "lm",
              show.legend = FALSE,
              color = "red") +
  labs(title = "mpg vs. hp") +
  theme_classic()

mpg_hp

# mpg vs draft

mpg_drat <-
  mtcars |>
  ggplot(mapping = aes(x = drat,
                       y = mpg)) +
  geom_point(show.legend = FALSE,
             color = "blue") +
  geom_smooth(method = "lm",
              show.legend = FALSE,
              color = "red") +
  labs(title = "mpg vs. drat") +
  theme_classic()

mpg_drat

# mpg vs wt

mpg_wt <-
  mtcars |>
  ggplot(mapping = aes(x = wt,
                       y = mpg)) +
  geom_point(show.legend = FALSE,
             color = "blue") +
  geom_smooth(method = "lm",
              show.legend = FALSE,
              color = "red") +
  labs(title = "mpg vs. wt") +
  theme_classic()

mpg_wt

# mpg vs qsec

mpg_qsec <-
  mtcars |>
  ggplot(mapping = aes(x = qsec,
                       y = mpg)) +
  geom_point(show.legend = FALSE,
             color = "blue") +
  geom_smooth(method = "lm",
              show.legend = FALSE,
              color = "red") +
  labs(title = "mpg vs. qsec") +
  theme_classic()


mpg_qsec

# mpg vs vs

mpg_vs <-
  mtcars |>
  ggplot(mapping = aes(x = vs,
                       y = mpg)) +
  geom_point(show.legend = FALSE,
             color = "blue") +
  geom_smooth(method = "lm",
              show.legend = FALSE,
              color = "red") +
  labs(title = "mpg vs. vs") +
  theme_classic()

mpg_vs

# mpg vs am

mpg_am <-
  mtcars |>
  ggplot(mapping = aes(x = am,
                       y = mpg)) +
  geom_point(show.legend = FALSE,
             color = "blue") +
  geom_smooth(method = "lm",
              show.legend = FALSE,
              color = "red") +
  labs(title = "mpg vs. am") +
  theme_classic()

mpg_am

# mpg vs gear

mpg_gear <-
  mtcars |>
  ggplot(mapping = aes(x = gear,
                       y = mpg)) +
  geom_point(show.legend = FALSE,
             color = "blue") +
  geom_smooth(method = "lm",
              show.legend = FALSE,
              color = "red") +
  labs(title = "mpg vs. gear") +
  theme_classic()

mpg_gear

# mpg vs carb

mpg_carb <-
  mtcars |>
  ggplot(mapping = aes(x = carb,
                       y = mpg)) +
  geom_point(show.legend = FALSE,
             color = "blue") +
  geom_smooth(method = "lm",
              show.legend = FALSE,
              color = "red") +
  labs(title = "mpg vs. carb") +
  theme_classic()

mpg_carb

# Layout com todos os gráficos de dispersão -------------------------------

(mpg_cyl + mpg_disp + mpg_hp) /
  (mpg_drat + mpg_wt + mpg_qsec) /
  (mpg_vs + mpg_am + mpg_gear) /
  (patchwork::plot_spacer() + mpg_carb + patchwork::plot_spacer())


# matriz de correlação (r, p-value) ---------------------------------------

corr <- round(cor(mtcars),
              digits = 2)

#função para fazer matriz com os p-value

# mat: matriz da base

# ... : argumentos para passar para a função "cor.test function"

cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

# matriz com os p-value da correlação

p.mat <- cor.mtest(mtcars)

corrplot::corrplot(corr,
                   method = "number",
                   type = "upper")

# Paleta de cores que será utilizada

col <- colorRampPalette(c("#BB4444",
                          "#EE9988",
                          "#FFFFFF",
                          "#77AADD",
                          "#4477AA"))

corrplot(corr, method="color", col=col(200),
         type="upper", order="hclust",
         addCoef.col = "black", # coeficiente da correlação
         tl.col="black", tl.srt=45, # cor e inclinação do texto
         # p-value
         p.mat = p.mat, sig.level = 0.01, insig = "blank",
         # retirando o coeficiente de correlação da mesma variável(diagonal=1)
         diag=FALSE
)

# Modelagem - Redução do modelo -------------------------------------------

# modelo 1 - Completo

M1 <-
  lm(mpg ~ wt + cyl + disp + hp + drat + qsec + vs + am + gear + carb, mtcars)

parameters::parameters(M1)

# modelo 2

M2 <-
  lm(mpg ~ wt + hp + drat + qsec + vs + am + gear + carb, mtcars)

parameters::parameters(M2)

# modelo 3

M3 <-
  lm(mpg ~ wt + hp + drat + qsec + am + gear + carb, mtcars)

parameters::parameters(M3)

# modelo 4

M4 <-
  lm(mpg ~ wt + hp + drat + qsec + am + carb, mtcars)

parameters::parameters(M4)

# modelo 5

M5 <-
  lm(mpg ~ wt+ drat + qsec + am + carb, mtcars)

parameters::parameters(M5)

# modelo 6

M6 <-
  lm(mpg ~ wt + qsec + am + carb, mtcars)

parameters::parameters(M6)

# modelo 7

M7 <-
  lm(mpg ~ wt + qsec + am, mtcars)

# Tabela com modelo proposto

performance::compare_performance(M7,
                                 metrics = c("RMSE", "R2", "R2_adj")) |>
  knitr::kable(caption = "Tabela 2. Resultados da regressão linear multivariada.") |>
  kableExtra::kable_paper()

# Checando multicolinearidade

colinearidade <- performance::check_collinearity(M7)|>
  knitr::kable(caption = "Tabela 2. Resultados da regressão linear multivariada.") |>
  kableExtra::kable_paper()

colinearidade

# Diagnóstico do modelo proposto

par(mfrow = c(3, 2))

plot(M7, which = 1:5)

# Teste de heterocedasticidade (Breusch-Pagan Test)

lmtest::bgtest(M7)

# Normalidade dos resíduos (Shapiro-Wilk test)

norm_test <- broom::augment(M7)

shapiro.test(norm_test$.resid)


# Explorando interações ---------------------------------------------------

# Grafícos de dispersão para verificar "nuvem" ----------------------------

# mpg vs wt

mpg_wt_int <-
  mtcars |>
  ggplot(mapping = aes(x = wt,
                       y = mpg,
                       color = factor(am))) +
  geom_point(show.legend = FALSE) +
  geom_smooth(method = "lm",
              show.legend = FALSE) +
  labs(title = "mpg vs. wt",
       color = "Transmissão manual = 1") +
  scale_color_brewer(palette = "Set1") +
  theme_classic()

mpg_wt_int

# mpg vs qsec

mpg_wt_qsec <-
  mtcars |>
  ggplot(mapping = aes(x = qsec,
                       y = mpg,
                       color = factor(am))) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "mpg vs. qsec",
       color = "Transmissão manual = 1") +
  scale_color_brewer(palette = "Set1") +
  theme_classic() +
  theme(legend.position = "bottom")

mpg_wt_qsec

# patchwork

mpg_wt_int/mpg_wt_qsec

# modelo com interação

summary(lm(mpg ~ qsec + wt * am, mtcars))

# multicolineatidade do modelo com interação

performance::check_collinearity(novo_mod)|>
  knitr::kable(digits = 2, align = "c", caption = "Tabela 7. Fator de Inflação da Variância (VIF) das variáveis inseridas no novo modelo.") |>
  kableExtra::kable_paper()
