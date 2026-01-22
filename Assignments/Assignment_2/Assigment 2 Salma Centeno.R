csv_files <- list.files("Data", pattern = "\\.csv$", full.names = TRUE)
length(csv_files)
df <- read.csv("Data/wingspan_vs_mass.csv")
head(df, 5)
b_files <- list.files("Data", pattern = "^b", full.names = TRUE, recursive = TRUE)
b_files
for (file in b_files) {
  cat("----", file, "----\n")
  cat(readLines(file, n = 1), "\n\n")
}
for (file in csv_files) {
  cat("----", file, "----\n")
  cat(readLines(file, n = 1), "\n\n")
}