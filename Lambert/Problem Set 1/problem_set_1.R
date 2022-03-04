library(tidyverse)

violence_data <- readxl::read_xls("problemset1.xls")

slr1 <- lm(Violence ~ Unemployment, data = violence_data)
