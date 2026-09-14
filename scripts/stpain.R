### Short-term pain relief

library(meta)
library(metafor)
library(readr)
library(readxl)


table_stpain <- read_delim("C:/Users/lucas/Desktop/metaMVDvsPBC-Trigeminal/Outcomes/Short-term pain relief.csv",
                            delim = ",", escape_double = FALSE, trim_ws = TRUE)


out_stpain<-metabin(event.e,n.e,event.c,n.c,
                     studlab = study,
                     data = table_stpain,
                     method="MH",
                     method.tau="REML",
                     sm="OR"
)
out_stpain


out_stpain_metareg_sample <- metareg(out_stpain, sample)
out_stpain_metareg_continent <- metareg(out_stpain, continent)
out_stpain_metareg_year <- metareg(out_stpain, year)

bubble(out_stpain_metareg_sample,
       studlab = F,
       xlab = "Sample size",
       ylab = "Short-term Pain Relief (OR)",
       col = "black",
       bg = "white",
       pch = 21,
       cex = 1.5,
       lwd = 2)


bubble(out_stpain_metareg_year,
       studlab = F,
       xlab = "Publication year",
       ylab = "Short-term Pain Relief (OR)",
       col = "black",
       bg = "white",
       pch = 21,
       cex = 1.5,
       lwd = 2)


forest(out_stpain,
       xlim = c(0.25, 4),
       sortvar = study,
       pooled.events = T,
       pooled.totals = T,
       label.e = "MVD", label.c = "PBC",
       fixed = T,
       random = T,
       test.overall = TRUE,
       digits = 2,
       digits.pval = 3,
       digits.sd = 2,
       print.Q = TRUE,
       print.pval.Q = TRUE,
       print.tau.ci = TRUE,
       layout = "RevMan5",
       colgap = "3mm",
       colgap.forest.left = "5mm",
       col.diamond.random = "darkgrey",
       col.diamond.lines.random = "black",
       col.diamond.common = "darkgrey",
       col.diamond.lines.common = "darkgrey",
       col.square = "black",
       col.square.lines = "black",
       col.line = "black",
       col.inside = "black",
       col.study = "black",
       col.pval = "black",
       col.hetstat = "black",
       col.label.right = "black",
       col.label.left = "black",
       fs.heading = 12,
       fs.study = 12,
       fs.hetstat = 12,
       leftcols = c("studlab", "event.e", "n.e", "event.c", "n.c",
                    "effect", "ci", "w.random"),
       leftlabs = c("Study or Subgroup", "Events", "Total", "Events", "Total",
                    "OR", "95% CI", "Weight"),
       label.right = "Favors MVD",
       label.left = "Favors PBC",
       subgroup = T,
       print.subgroup.labels = F,
       subgroup.name = "comp_type",
       print.subgroup.name = T,
       sort.subgroup = F,
       test.effect.subgroup.random = T,
       col.pval = "black",
       col.hetstat = "black",
       col.lab = "black",
       col.text = "black",
       col.summary = "black",
       col.subgroup = "black",
       prediction = T,
       col.predict = "white",
       col.predict.lines = "black"
)


# Assessment of Publication Bias

funnel(out_stpain,
       studlab = F,
       bg = "black",
       col = "darkgray",
       pch = 16,
       xlab = "Effect Size",
       ylab = "Standard Error",
       main = "",
       refline = 0,
       shade = TRUE,
       col.shade = "gray90",
       grid = TRUE,
       lwd = 1.0,
       cex = 0.8,
       cex.studlab = 0.8,
       cex.axis = 0.8,
       cex.main = 1.0
)


# Leave-one-out sensitivity analysis


l1o_out_stpain<-metainf(out_stpain,
                      pooled="random")

forest(l1o_out_stpain,
       col.square="black",
       col.inside = "black",
       col.line = "black",
       col.square.lines = "black",
       col.diamond="darkgray",
       col.diamond.lines.random = "darkgrey",
       xlim=c(0.5, 2),
       xlab="Favors PBC       Favors MVD",
       rightcols = c("effect", "ci", "I2"),
       subgroup = mis_class
)


#2nd = Test for funnel plot asymmetry
#Utilizar a função "metabias()"
#Rules for using statistical tests to evaluate asymmetry
#1)Tests for funnel plot asymmetry should be used only when there are at least 10 studies included in the meta-analysis.
#2)Tests should not be used if studies are of similar size (similar standard errors of intervention effect estimates).
#3)When there is evidence of funnel plot asymmetry from a test, non-reporting biases should be considered as one of several possible explanations.
## Non-reporting biases
## Poor methodological quality leading to spuriously inflated effects in smaller studies
## True heterogeneity
## Artefactual
## Chance
#4)Tests for continuous outcomes with intervention effects measured as MD
## Egger test = "Egger"
#5)Tests for dichotomous outcomes with intervention effects measured as OR or RR
## Harbord test = "Harbord"

metabias(out_stpain,
         method.bias="Egger",
         k.min = 8,
         studlab=F ) #número mínimo de estudos

# Baujat plot to explore heterogeneity

baujat(out_stpain,
       bg="grey",
       stulab=F,
       Xlim=c(0,50))

# L'Abbé plot to explore heterogeneity

labbe (out_stpain,
       bg="gray",
       studlab=T,
       Xlim=c(0,0.25),
       ylim=c(0,0.25))


# Meta-regression

#1st = rodar a função "metareg

mreg_stpain(metareg(out_stpain
                     ~year) #nome da coluna na tabela de dados no qual estão dispostos os dados da covariável em questão

#2nd = Bubble plot to display the result of a meta-regression

bubble(metareg, studlab=TRUE, bg="red",
       cex.studlab = 0.6, #tamanho da bolinha
       pos.studlab = 3, #posição (1 a 4)
       offset=-0.5,#posição texto studlab
       ylab="Treatment Effect (Risk Ratio or Odds Ratio or Hazard Ratio)",
       xlab="XXX") # Descrever a unidade de medida da covariável utilizada para a associação
## editar eixo x e y use:
#xlim=c(20,90),ylim = c(0.5,1.5), #limites dos eixos x e y


# Confere o tipo
str(table_stpain$year)

# Meta-regressão
meta_reg_year <- metareg(out_stpain, ~ year)
summary(meta_reg_year)

# Visualização
bubble(meta_reg_year,
       xlab = "Publication Year",
       ylab = "Log Odds Ratio (MVD vs PBC)",
       xlim = c(1990, 2026),          # define limites horizontais
       ylim = c(0.1, 5),               # ajusta faixa vertical
       col = adjustcolor("gray60"),  # pontos semi-transparentes
       pch = 21,                      # formato do ponto (bolinha)
       bg = adjustcolor("gray60", alpha.f = 1),  # cor de fundo do ponto
       cex = 1.5,                     # tamanho do ponto
       col.line = "black",          # linha de regressão vermelha escura (mesmo tom da sua forest)
       lwd = 2.5,                       # espessura da linha
       cex.lab = 1.2,                 # tamanho dos títulos dos eixos
       cex.axis = 1.1,                # tamanho dos números nos eixos
       cex.main = 1.3,                # tamanho do título (se usar)
       bty = "l",                     # borda em formato “L”
       las = 1                        # rótulos horizontais no eixo Y
)


##mregression sample


table_stpain$sample <- as.numeric(table_stpain$n.e + table_stpain$n.c)

# 2) Meta-regression (sample size) — robust way (passes vector)
meta_reg_sample <- metareg(out_stpain,
                           covariate = table_stpain$sample,
                           method.tau = "REML")   # or "DL" if you prefer
summary(meta_reg_sample)

# 3) Bubble plot (log OR on y-axis)
bubble(meta_reg_sample,
       xlab = "Total Sample Size",
       ylab = "Log Odds Ratio (MVD vs PBC)",
       col.line = "#a3020f", lwd = 2,
       col = adjustcolor("gray40", 0.6), pch = 21, bg = adjustcolor("gray60", 0.6),
       cex = 1.4, main = "Meta-regression by Sample Size", las = 1, bty = "l")
