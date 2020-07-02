#' ninebox Function
#'
#' Models the distribution of the ninebox with the specified boundaries
#'
#' @param df_list Output of the df_ninbox function.
#' @param upper_desempeno Upper boundary for desempeño axis as dbl.
#' @param lower_desempeno Lower boundary for desempeño axis as dbl.
#' @param upper_potencial Upper boundary for potencial axis as dbl.
#' @param lower_potencial Lower boundary for potencial axis as dbl.
#' @param col_desempeno Name of the column containing desempeño as string.
#' @param col_potencial Name of the column containing potencial as string.
#' @param transparency Manipulates the alpha parameter from ggplots geom_point. 
#' @examples
#' ninebox(test, 90, 85, 100, 85, "desempeño ranco", "potencial ranco")
#' ninebox(test2, 3, 1.5, 2, 1.5)
#' ninebox(test3, 4, 2, 4, 2, "evaluación de desempeño", "evaluación de potencial", 0.1)
#' @export


ninebox <- function (df_list, upper_desempeno, lower_desempeno, upper_potencial, lower_potencial, col_desempeno = "desempeño" , col_potencial = "potencial", transparency = 0.2){
  
  df <- df_list[[1]] %>% 
    select(potencial = rlang::UQ(rlang::sym(col_potencial)), desempeño := rlang::UQ(rlang::sym(col_desempeno)))
  
  min_max <- df_list[[2]] 
  
  min_desempeno <- min_max %>% filter(name == col_desempeno) %>% .[[1,2]]
  max_desempeno <- min_max %>% filter(name == col_desempeno) %>% .[[1,3]]
  
  min_potencial <- min_max %>% filter(name == col_potencial) %>% .[[1,2]]
  max_potencial <- min_max %>% filter(name == col_potencial) %>% .[[1,3]]
  
  df_cat_2 <- df %>% 
    mutate(desempeno_cat_actual = case_when(desempeño >= upper_desempeno ~ "Nivel alto",
                                            desempeño >= lower_desempeno~ "Nivel medio",
                                            TRUE ~ "Nivel bajo"),
           potencial_cat_actual = case_when(potencial >= upper_potencial ~ "Nivel alto",
                                            potencial >= lower_potencial ~ "Nivel medio",
                                            TRUE ~ "Nivel bajo")) %>% 
    na.omit() %>% 
    mutate(cuadrante_actual = case_when(potencial_cat_actual == "Nivel alto" & desempeno_cat_actual == "Nivel alto" ~ "Estrella",
                                        potencial_cat_actual == "Nivel alto" & desempeno_cat_actual == "Nivel medio" ~ "Estrella potencial",
                                        potencial_cat_actual == "Nivel alto" & desempeno_cat_actual == "Nivel alto" ~ "Talento latente",
                                        potencial_cat_actual == "Nivel medio" & desempeno_cat_actual == "Nivel alto" ~ "Futura Estrella",
                                        potencial_cat_actual == "Nivel medio" & desempeno_cat_actual == "Nivel medio" ~ "Contribución Consistente",
                                        potencial_cat_actual == "Nivel medio" & desempeno_cat_actual == "Nivel bajo" ~ "Alto Soporte",
                                        potencial_cat_actual == "Nivel bajo" & desempeno_cat_actual == "Nivel alto" ~ "Profesional experto",
                                        potencial_cat_actual == "Nivel bajo" & desempeno_cat_actual == "Nivel medio" ~ "Contribución Marginal",
                                        potencial_cat_actual == "Nivel bajo" & desempeno_cat_actual == "Nivel bajo" ~ "No cumple",
    ))
  
  
  distribucion <- df_cat_2 %>% 
    count(cuadrante_actual) %>% 
    mutate(porc = round(n*100/sum(n)),
           porc = str_c(as.character(porc), "%")) 
  
  
  grafico <- df_cat_2 %>% 
    ggplot(aes(y = potencial, x = desempeño)) +
    geom_point(alpha = transparency, color = "blue") +
    geom_vline(xintercept =  upper_desempeno) +
    geom_vline(xintercept =  lower_desempeno) +
    geom_hline(yintercept =  upper_potencial) +
    geom_hline(yintercept =  lower_potencial) 
  
  grafico + annotate("text",x = lower_desempeno-(lower_desempeno- min_desempeno)/4, y= lower_potencial - (lower_potencial-min_potencial)/4, label = ifelse("No cumple" %in% distribucion$cuadrante_actual, distribucion %>% 
                                                                                                                                                             filter(cuadrante_actual == "No cumple")%>% 
                                                                                                                                                             .$porc, "0%")) +
    annotate("text",x = lower_desempeno + (upper_desempeno - lower_desempeno)/2, y= lower_potencial - (lower_potencial-min_potencial)/4, label = ifelse("Contribución Marginal" %in% distribucion$cuadrante_actual, distribucion %>% 
                                                                                                                                                          filter(cuadrante_actual == "Contribución Marginal") %>% 
                                                                                                                                                          .$porc, "0%")) +
    annotate("text",x = upper_desempeno + (max_desempeno-upper_desempeno)*0.2, y= lower_potencial - (lower_potencial-min_potencial)/4, label = ifelse("Profesional experto" %in% distribucion$cuadrante_actual, distribucion %>% 
                                                                                                                                                        filter(cuadrante_actual == "Profesional experto") %>% 
                                                                                                                                                        .$porc, "0%")) +
    annotate("text",x = lower_desempeno-(lower_desempeno- min_desempeno)/4, y= lower_potencial + (upper_potencial - lower_potencial)/2, label = ifelse("Alto Soporte" %in% distribucion$cuadrante_actual, distribucion %>% 
                                                                                                                                                         filter(cuadrante_actual == "Alto Soporte") %>% 
                                                                                                                                                         .$porc, "0%")) +
    annotate("text",x = lower_desempeno + (upper_desempeno - lower_desempeno)/2, y= lower_potencial + (upper_potencial - lower_potencial)/2, label = ifelse("Contribución Consistente" %in% distribucion$cuadrante_actual, distribucion %>% 
                                                                                                                                                              filter(cuadrante_actual == "Contribución Consistente") %>% 
                                                                                                                                                              .$porc, "0%")) +
    annotate("text",x = upper_desempeno + (max_desempeno-upper_desempeno)*0.2, y= lower_potencial + (upper_potencial - lower_potencial)/2, label = ifelse("Futura Estrella" %in% distribucion$cuadrante_actual, distribucion %>% 
                                                                                                                                                            filter(cuadrante_actual == "Futura Estrella") %>% 
                                                                                                                                                            .$porc, "0%")) +
    
    annotate("text",x = lower_desempeno-(lower_desempeno- min_desempeno)/4, y= upper_potencial + (max_potencial-upper_potencial)*0.3, label = ifelse("Talento Latente" %in% distribucion$cuadrante_actual, distribucion %>% 
                                                                                                                                                       filter(cuadrante_actual == "Talento Latente") %>% 
                                                                                                                                                       .$porc, "0%")) +
    annotate("text",x = lower_desempeno + (upper_desempeno - lower_desempeno)/2, y= upper_potencial + (max_potencial-upper_potencial)*0.3, label = ifelse("Estrella potencial" %in% distribucion$cuadrante_actual, distribucion %>% 
                                                                                                                                                            filter(cuadrante_actual == "Estrella potencial") %>% 
                                                                                                                                                            .$porc, "0%")) +
    annotate("text",x = upper_desempeno + (max_desempeno-upper_desempeno)*0.2, y= upper_potencial + (max_potencial-upper_potencial)*0.3, label = ifelse("Estrella" %in% distribucion$cuadrante_actual, distribucion %>% 
                                                                                                                                                          filter(cuadrante_actual == "Estrella") %>% 
                                                                                                                                                          .$porc, "0%")) +
    labs(y = 'Potencial', x= 'Desempeño') +
    theme_minimal()
  
}