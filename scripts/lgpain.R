### Long-term pain relief

library(meta)
library(metafor)
library(readr)
library(readxl)


table_ltpain <- read_delim("C:/Users/lucas/Desktop/metaMVDvsPBC-Trigeminal/Outcomes/Long-term pain relief.csv",
                           delim = ",", escape_double = FALSE, trim_ws = TRUE)


out_ltpain<-metabin(event.e,n.e,event.c,n.c,
                    studlab = study,
                    data = table_ltpain,
                    method="MH",
                    method.tau="DL",
                    sm="OR")
out_ltpain


forest(out_ltpain,
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


funnel(out_ltpain,
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


l1o_out_ltpain<-metainf(out_ltpain,
                        pooled="random")

forest(l1o_out_ltpain,
       col.square="black",
       col.inside = "black",
       col.line = "black",
       col.square.lines = "black",
       col.diamond="darkgray",
       col.diamond.lines.random = "darkgrey",
       xlim=c(0.25, 4),
       xlab="Favors PBC        Favors MVD",
       rightcols = c("effect", "ci", "I2"),
       subgroup = mis_class
)
