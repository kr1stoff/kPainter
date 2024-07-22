# current directory generate log
# draw venn diagram, max venn size 5

library(VennDiagram)

args <- commandArgs(T)
f_data <- args[1]
f_png <- args[2]
# f_data <- "./gene_presence_absence.Rtab"
# f_png <- "venn.png"

df <- read.table(f_data, sep = "\t", header = 1)
df <- subset(df, select = -c(1:6))

# venn input list
list_venn <- list()
for (colu in colnames(df)) {
  list_venn[[colu]] <- rownames(dplyr::filter(df, !!rlang::sym(colu) != 0))
}

# venn plot
mypal <- c("#E64B35FF", "#4DBBD5FF", "#00A087FF", "#3C5488FF", "#F39B7FFF", "#8491B4FF", "#91D1C2FF", "#DC0000FF", "#7E6148FF", "#B09C85FF")

fig_venn <- venn.diagram(
  x = list_venn,
  filename = f_png,
  fill = mypal[1:length(list_venn)],
  cex = 1,
  cat.cex = 1,
  alpha = 0.5,
  col = "transparent",
  fontfamily = "serif",
  cat.fontfamily = "serif",
  margin = 0.2
)
