#' @export
lm_resid_group <- function(df,model,.groups=NA,output_mode='table'){
  
  
  .groups_syms <- rlang::syms(.groups) 
  
  
  if(any(is.na(.groups))){
    stop('please inform a group',call. = FALSE)
  }else if(length(.groups)==1){
    group_var2 <- paste(.groups,"_",sep="")
    df[[group_var2]] <- df[[.groups]]
  }else{
    group_var2 <- c()
    for(i in 1:length(.groups)){
      
      group_var2[i] <- paste(.groups[i],"_",sep="")
      df[[group_var2[i]]] <- df[[.groups[i]]]
      
    }
  }
  # return(group_var2)
  #return(group_var2)
  lm_resid_g_ex = df %>% 
    # dplyr::mutate(!!grou := {{ group_var }}) %>% # vamos duplicar a coluna de grupo, 
    dplyr::group_by(!!!.groups_syms) %>% # pois precisamos delas dentro e fora dos grupos
    tidyr::nest() %>%  # vamos nestar o dado, para aplicar em cada parte isolada. a coluna e chamada de data
    dplyr::mutate(reg = purrr::map(data,
                                   ~lm_resid(df = ., # simplesmente aplicamos o modelo em data,
                                             model=model, # que no caso sao os dados nestados
                                             group_print=group_var2, # nome da coluna de grupo, convertida em char
                                             output_mode=output_mode))) %>% # saida selecionada
    dplyr::mutate(data = purrr::map(data,# aqui apagamos as colunas duplicadas
                                    ~dplyr::select(., -dplyr::any_of(group_var2) ))) %>% 
    tidyr::unnest(reg) %>% # desnestar a saida final
    dplyr::ungroup() # desagrupar os dados
  return(lm_resid_g_ex)
}


