library(dplyr)
install.packages("writexl")
library(writexl)
install.packages("readr")
library(readr)

data <- read_csv("data/data_original/all_variables.csv")

# Creando una nueva variable llamada 'pais' basada en los valores de 'demo_resid'
data <- data %>%
  mutate(pais = case_when(
    demo_resid == "1" ~ "argentina",
    demo_resid == "2" ~ "brasil",
    demo_resid == "3" ~ "chile",
    demo_resid == "4" ~ "colombia",
    demo_resid == "5" ~ "mexico",
    demo_resid == "6" ~ "peru",
    demo_resid == "7" ~ "usa",
    demo_resid == "8" ~ "other",
    TRUE ~ NA_character_  # Asignar NA si no coincide con ningún criterio
  ))

# Inicializar el data frame para el reporte de exclusión
reporte_exclusion <- data.frame(
  Paso = integer(),
  Descripcion = character(),
  Filas_Iniciales = integer(),
  Filas_Excluidas = integer(),
  Filas_Restantes = integer(),
  stringsAsFactors = FALSE
)

# Función para registrar cada paso de depuración
registrar_filtro <- function(data, filtro, descripcion, paso) {
  filas_iniciales <- nrow(data)
  data_filtrado <- data %>% filter(!!rlang::parse_expr(filtro))
  filas_restantes <- nrow(data_filtrado)
  filas_excluidas <- filas_iniciales - filas_restantes
  
  # Actualizar el reporte
  reporte_exclusion <<- bind_rows(
    reporte_exclusion,
    data.frame(
      Paso = paso,
      Descripcion = descripcion,
      Filas_Iniciales = filas_iniciales,
      Filas_Excluidas = filas_excluidas,
      Filas_Restantes = filas_restantes
    )
  )
  
  return(data_filtrado)
}

# Base inicial
data_filtrada <- data


# Paso 1: Filtrar sujetos con valor '0' en 't_adiq_ans'
data_filtrada <- registrar_filtro(
  data_filtrada,
  "t_adiq_ans == 0",
  "Filtrar sujetos con '0' en 't_adiq_ans'",
  1
)

# Paso 2: Filtrar sujetos con valor '1' en 'mri_research_image___1' y 'mri_research_image___3'
data_filtrada <- registrar_filtro(
  data_filtrada,
  "mri_research_image___1 == 1 & mri_research_image___3 == 1",
  "Filtrar sujetos con '1' en 'mri_research_image___1' y 'mri_research_image___3'",
  2
)

# Paso 3: Filtrar diagnósticos válidos ('CN', 'AD', 'FTD') en 'clinical_diagnosis'
data_filtrada <- registrar_filtro(
  data_filtrada,
  "clinical_diagnosis %in% c('CN', 'AD', 'FTD')",
  "Filtrar sujetos con diagnósticos válidos ('CN', 'AD', 'FTD')",
  3
)

# Paso 4: Filtrar sujetos que cumplen al menos uno de los criterios adicionales
data_filtrada <- registrar_filtro(
  data_filtrada,
  "clinical_diagnosis_adsp == 'ADa' | clinical_diagnosis_ftdsp == 'FTDbehav' | diagnosiscognormal == 1",
  "Filtrar sujetos que cumplen con criterios adicionales",
  4
)

# Paso 5: Filtrar sujetos con valor '0' en 'cdr_ans'
data_filtrada <- registrar_filtro(
  data_filtrada,
  "cdr_ans == 0",
  "Filtrar sujetos con '0' en 'cdr_ans'",
  5
)

# Agregar más pasos según sea necesario para las demás variables
variables <- c("mmse_ans", "gad_ans", "gds_ans", "npi_ans", "uls_ans", "cog_ans", "stmb_ans")
for (i in seq_along(variables)) {
  var <- variables[i]
  descripcion <- paste("Filtrar sujetos con '0' en", var)
  data_filtrada <- registrar_filtro(
    data_filtrada,
    paste(var, "== 0"),
    descripcion,
    5 + i
  )
}

# Crear data_limpia_2 (último paso)
data_limpia_2 <- data_filtrada %>% 
  filter(!pais %in% c("other", NA),
         cog_ed != 0)

# Registrar el paso de data_limpia_2
reporte_exclusion <- bind_rows(
  reporte_exclusion,
  data.frame(
    Paso = nrow(reporte_exclusion) + 1,
    Descripcion = "Filtrar pais 'other' y NA, y cog_ed != 0",
    Filas_Iniciales = nrow(data_filtrada),
    Filas_Excluidas = nrow(data_filtrada) - nrow(data_limpia_2),
    Filas_Restantes = nrow(data_limpia_2)
  )
)



# Mostrar los datos filtrados
print(head(data_limpia_2))

# Guardar el reporte de exclusión en Excel
# Definir rutas
dir_out  <- "outputs/01"
dir_mod  <- "data/data_mod/01"

# Crear directorios si no existen
if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)
if (!dir.exists(dir_mod)) dir.create(dir_mod, recursive = TRUE)

# Guardar reporte de exclusión en Excel
write_xlsx(
  reporte_exclusion,
  path = file.path(dir_out, "Reporte_Exclusion_Paso_a_Paso.xlsx")
)

# Guardar la base limpia_2 en CSV
write_csv(
  data_limpia_2,
  file = file.path(dir_mod, "data_limpia_2.csv"))

# Guardar la base limpia_2 en xlsx
write_xlsx(
  data_limpia_2,
  path = file.path(dir_mod, "data_limpia_2.xlsx"))

