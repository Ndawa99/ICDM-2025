# 1. Chargement automatique et renommage
load_table <- function(folder, suffix) {
  files <- list.files(folder, pattern = "\\.RData$", full.names = TRUE)
  for (f in files) {
    load(f)  # charge "latex_table"
    base_name <- tools::file_path_sans_ext(basename(f))
    var_name <- paste0( base_name, "_", suffix)
    assign(var_name, latex_table, envir = .GlobalEnv)
    rm(latex_table)
  }
}

# Charger les 3 groupes
load_table("calibration", "cal")
load_table("recalibration", "PS")
load_table("recalibration_beta", "BC")

# 2. Fonction pour transformer les tableaux latex
clean_latex_table <- function(tbl) {
  tbl_clean <- apply(tbl, 2, function(x) {
    cleaned <- gsub("\\s*\\(.*?\\)", "", x)
    return(cleaned)
  })
  return(as.data.frame(tbl_clean, stringsAsFactors = FALSE))
}
latex_table_uniform_ <- clean_latex_table(latex_table_uniform_cal)
