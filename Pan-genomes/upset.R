library(stringr)
library(UpSetR)

args <- commandArgs(T)
f_data <- args[1]
f_png <- args[2]

# f_data <- "./gene_presence_absence.Rtab"
# f_png <- "./upset.png"

df <- read.table(f_data, sep = "\t", header = 1)
png(filename = f_png, width = 7, height = 5, units = "in", res = 300)
upset(df, nsets = dim(df)[2])
dev.off()
