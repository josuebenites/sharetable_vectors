table_share <- function(data, vects, comp) {
  library(tidyverse)
  datos <- data
  
  # Lista para almacenar los data frames estratificados
  tmp_list <- list()
  
  if(length(vects) > 0) {
    t = 1
    final = list()
    sub_list = list()
    s = 1
    
    
    for (v in 1:length(vects)) {
      
      # Lista para almacenar los datasets estratificados para cada variable en vects
      separate_datasets <- list()
      alternativas <- unique(na.omit(data[[vects[v]]])) %>% sort() 
      completos <- data.frame(alternativas)
      colnames(completos) <- vects[v]  # Corregir aquí
      
      borrador <- datos %>%
        drop_na(!!rlang::sym(vects[v])) %>%
        group_by(!!rlang::sym(vects[v])) %>%
        tally() %>%
        mutate(
          `%` = 100*(n / sum(n))
        ) %>%
        `colnames<-`(c("var","n","%")) %>%
        mutate(comp = "All")
      
      tmp_list[[t]] = borrador
      t = 1 + t
      
      if (length(comp) > 0) {
        for (i in 1:(length(comp))) {
          
          var_comb <- c(vects[v], comp[1:i])
          k = (i)
          var_comb1 <- comp[k]
          
          # Data frame estratificado por combinaciones de variables
          main <- datos %>%
            group_by_at(vars(!!!rlang::syms(var_comb))) %>%
            drop_na(!!rlang::sym(vects[v])) %>%
            tally() %>%
            group_by_at(vars(!!!rlang::syms(var_comb1))) %>%
            mutate(
              `%` = 100*(n / sum(n))
            ) %>%
            ungroup()
          
          # Crear un nombre para cada data frame en función de las variables usadas
          #names(tmp_list) <- sapply(tmp_list, function(df) paste(colnames(df), collapse = " "))
          
          # Datasets separados para var_comb1
          for (j in pull(unique(main[var_comb1]))) {
            
            borrador <- filter(main, !!rlang::sym(var_comb1) == j)
            
            
            # Realizar la unión después de completar los niveles
            sub_list[[s]] <- left_join(completos, borrador, by = c(var_comb[1]))%>%
              mutate_at(vars(c(n,`%`)),
                        ~ replace_na(.x, 0)
              ) %>%
              `colnames<-`(c("var", "comp", "n","%"))
            
            #borrador = filter(main, !!rlang::sym(var_comb1) == j)
            #tmp_list[[m]] <- borrador %>%
            s = 1 + s
          }
        }
      }
    }
  }
  
  return(list(tmp_list,sub_list))
}
