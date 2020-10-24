#' @export
lm_resid <- function(df,model,group_print=NA,output_mode='table'){
  # copiar nome da variavel Y, com base no modelo
  Y <- all.vars( stats::formula(model)[[2]]) 
  
  # aqui criamos a interface. nothing fancy
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Drag to select points"),
    
    miniUI::miniContentPanel(
      # The brush="brush" argument means we can listen for
      # brush events on the plot using input$brush.
      shiny::plotOutput("plot", height = "100%", brush = "brush")
    ),
    # adiciona mini botoes
    miniUI::miniButtonBlock(
      shiny::actionButton('rerun','retirar pontos'),
      shiny::actionButton("reset", "restaurar")
    ),
    
  ) # ui end
  
  
  server <- function(input, output, session){
    
    if(!any(is.na(group_print))){
      title_group <- paste0(group_print,unique(df[group_print]),collapse = " ")
      print(title_group)
    }
    # criar valor reativo
    vals <- shiny::reactiveValues()
    
    vals$keeprows = rep(TRUE, nrow(df) )
    
    # Toggle points that are clicked
    shiny::observeEvent( input$rerun, {
      
      selected <- shiny::brushedPoints(resid_orig(), input$brush)[[Y]]
      
      vals$keeprows <- xor(vals$keeprows,  df[[Y]] %in% selected )
      
    })
    
    # observar o botao 'restaurar'. se ele rodar,
    # aplicar true pra tudo na coluna keeprows, ou seja,
    # manter todas as colunas
    shiny::observeEvent(input$reset, {
      vals$keeprows <- rep(TRUE, nrow(df) )
    })
    
    # dfok e o dataframe que contem apenas as linhas keeprows,
    # ou seja, apenas as que o usuario nao marcou e retirou.
    dfok <- shiny::reactive({
      
      dfok <- df
      
      if(input$rerun){
        dfok <- dfok[vals$keeprows, ]
      }
      
      dfok
      
    })
    
    # este e o ajuste com o dataframe original, sem alteracoes. utilizamos o res_table=TRUE
    # para ter como saida a tabela de residuos. sera uzado para fazer o plot com pontos vazios.
    resid_orig <- shiny::reactive({
      
      tab <- lm_table(df, model, output = "merge_est") 
      
      resid_plot(tab, Y,'est',res_table = TRUE)
      
    })
    # aqui fazemos o grafico de fato. Sao dois graficos de dispersao,
    # sobrepostos. um com o dado original, outro com o dado filtrado
    output$plot <- shiny::renderPlot({
      
      # aqui, ajustamos o modelo novamente, porem utilizando dfok, nosso dado filtrado.
      dfok <- lm_table(dfok(), model, output = "merge_est") 
      # aqui puxamos os residuos dos dados originais
      residorig <- resid_orig()
      # aqui fazemos o grafico com o dfok
      resid_plot(dfok, Y,'est') +  # em seguida adicionamos uma nova camada, com os residuos originais
        ggplot2::geom_point(data=residorig,ggplot2::aes_string( Y,'ERROR'),
                            fill=NA,col="black",alpha=0.75,shape=21) + {
                              if(!any(is.na(group_print)))
                                ggplot2::ggtitle(title_group)
                            } # mudamos o formato para circulo vazio
      # assim serao plotados duas camadas de dispersao, uma com o ajuste customizado, outra com o ajuste real
      
    })
    # out_final e a saida final da funcao. ela precisa ser reativa, pois depende
    # da selecao que o usuario fara dos pontos. # apenas ajustamos o modelo informado com dfok
    # o tipo de saida e definida pelo usuario. apenas betas, df com os dados (merge_est), etc
    out_final <- shiny::reactive({
      
      lm_table(dfok(), model, 
                            #output = "merge_est"
                            output=output_mode
      ) 
      
    })
    # para encerrar e entregar o resultado, o usuario deve clicar no botao done.
    # observamos ele. Caso seja clicado, paramos o app e entregamos out_final
    shiny::observeEvent(input$done, {
      print('reg_ok')
      shiny::stopApp( out_final() )
    })
    
  } # fim do servidor
  
  # como a ideia seria fazer um mini shiny dentro da funcao. acima criamos um servidor e UI.
  # agora no final da funcao, rodamos runGadget. isso faz com que a funcao entregue o shiny.
  # a funcao faz o resto
  shiny::runGadget(ui,server)
  
}