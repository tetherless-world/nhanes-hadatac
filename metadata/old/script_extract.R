anos <- c(1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021)

# Função para obter tabelas DEMO de cada ano
tabelas_demo <- lapply(anos, function(ano) {
  tryCatch(
    nhanesTables("LAB", ano, details = TRUE),  # Tenta buscar as tabelas
    error = function(e) NULL                   # Ignora erros
  )
})

tabelas_lab_completas <- do.call(rbind, tabelas_demo)

# Exibir o resultado
tabelas_lab_completas

View(tabelas_lab_completas)

# Recuperar os nomes das tabelas (coluna 1)
nomes_tabelas <- tabelas_lab_completas$Data.File.Name
print(nomes_tabelas)

safe_extract <- function(lst, key) {
  if (!is.null(lst[[key]])) {
    return(lst[[key]])
  } else {
    return(NA)
  }
}

# Criar um data frame para armazenar os resultados
resultados <- data.frame(
  Tabela = character(),
  Variavel = character(),
  Variable_Name = character(),
  SAS_Label = character(),
  English_Text = character(),
  Target = character(),
  English_Instructions = character(),  # Adicionando a nova coluna
  Tabela_Valores = character(),  # Adicionando a tabela formatada
  stringsAsFactors = FALSE
)


# Loop para acessar as tabelas e variáveis
for (nome_tabela in nomes_tabelas) {
  tryCatch({
    # Obter as variáveis da tabela
    variaveis <- nhanesTableVars("LAB", nome_tabela)
    nomes_variaveis <- variaveis$Variable.Name # Extrair os nomes das variáveis
    
    # Loop para acessar os detalhes de cada variável
    for (nome_variavel in nomes_variaveis) {
      tryCatch({
        # Obter o Codebook da variável
        detalhes <- nhanesCodebook(nome_tabela, nome_variavel)
        
        # Extraindo os valores das variáveis de forma segura
        variable_name <- safe_extract(detalhes[[nome_variavel]], "Variable Name:")
        sas_label <- safe_extract(detalhes[[nome_variavel]], "SAS Label:")
        english_text <- safe_extract(detalhes[[nome_variavel]], "English Text:")
        target <- safe_extract(detalhes[[nome_variavel]], "Target:")
        english_instructions <- safe_extract(detalhes[[nome_variavel]], "English Instructions:")
        
        
        # Converter o tibble em string de listas do Python
        if (!is.null(detalhes[[nome_variavel]][[nome_variavel]])) {
          tibble_valores <- detalhes[[nome_variavel]][[nome_variavel]] # Acessa a tabela
          # Formatar como lista Python
          colunas_como_listas <- lapply(tibble_valores, function(coluna) {
            paste0("[", paste0('"', as.character(coluna), '"', collapse = ", "), "]")
          })
          # Combinar as colunas e seus nomes como string
          tabela_string <- paste0(
            names(colunas_como_listas), "=", colunas_como_listas, collapse = "; "
          )
        } else {
          tabela_string <- NA
        }
        
        # Adicionar ao data frame
        resultados <- rbind(
          resultados,
          data.frame(
            Tabela = nome_tabela,
            Variavel = nome_variavel,
            Variable_Name = variable_name,
            SAS_Label = sas_label,
            English_Text = english_text,
            Target = target,
            English_Instructions = english_instructions,  # Adiciona a nova variável
            Tabela_Valores = tabela_string,  # Adiciona a string formatada
            stringsAsFactors = FALSE
          )
        )
      }, error = function(e) {
        cat("Erro ao acessar a variável", nome_variavel, "na tabela", nome_tabela, ":", conditionMessage(e), "\n")
      })
    }
  }, error = function(e) {
    cat("Erro ao acessar a tabela", nome_tabela, ":", conditionMessage(e), "\n")
  })
}

# Visualizar o resultado
View(resultados)

# Salvar o data frame em um arquivo CSV
write.csv(resultados, "nhanes_codebook_lab_resultados.csv", row.names = FALSE)




###############################################################################
anos <- c(1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021)

data_type <- "Q"

# Função para obter tabelas DEMO de cada ano
tabelas_demo <- lapply(anos, function(ano) {
  tryCatch(
    nhanesTables(data_type, ano, details = TRUE),  # Tenta buscar as tabelas
    error = function(e) NULL                   # Ignora erros
  )
})

tabelas_QUE_completas <- do.call(rbind, tabelas_demo)

# Exibir o resultado
tabelas_QUE_completas

View(tabelas_QUE_completas)
write.csv(tabelas_QUE_completas, paste0(data_type, "_nhanes_codebook_COMPLETAS.csv"), row.names = FALSE)


# Recuperar os nomes das tabelas (coluna 1)
nomes_tabelas <- tabelas_QUE_completas$Data.File.Name
print(nomes_tabelas)

# Função para verificar a existência de um elemento na lista e retornar NA se não existir
safe_extract <- function(lst, key) {
  if (!is.null(lst[[key]])) {
    return(lst[[key]])
  } else {
    return(NA)
  }
}

# Criar um data frame para armazenar os resultados
resultados <- data.frame(
  Tabela = character(),
  Variavel = character(),
  Variable_Name = character(),
  SAS_Label = character(),
  English_Text = character(),
  Target = character(),
  English_Instructions = character(),  # Adicionando a nova coluna
  Tabela_Valores = character(),  # Adicionando a tabela formatada
  stringsAsFactors = FALSE
)

# Loop para acessar as tabelas e variáveis
for (nome_tabela in nomes_tabelas) {
  tryCatch({
    # Obter as variáveis da tabela
    variaveis <- nhanesTableVars(data_type, nome_tabela)
    nomes_variaveis <- variaveis$Variable.Name # Extrair os nomes das variáveis
    print(nome_tabela)
    print(nomes_variaveis)
    
    # Loop para acessar os detalhes de cada variável
    for (nome_variavel in nomes_variaveis) {
      tryCatch({
        # Obter o Codebook da variável
        detalhes <- nhanesCodebook(nome_tabela, nome_variavel)
        
        # Extraindo os valores das variáveis de forma segura
        variable_name <- safe_extract(detalhes[[nome_variavel]], "Variable Name:")
        sas_label <- safe_extract(detalhes[[nome_variavel]], "SAS Label:")
        english_text <- safe_extract(detalhes[[nome_variavel]], "English Text:")
        target <- safe_extract(detalhes[[nome_variavel]], "Target:")
        english_instructions <- safe_extract(detalhes[[nome_variavel]], "English Instructions:")
        
        # Converter o tibble em string de listas do Python
        if (!is.null(detalhes[[nome_variavel]][[nome_variavel]])) {
          tibble_valores <- detalhes[[nome_variavel]][[nome_variavel]] # Acessa a tabela
          # Formatar como lista Python
          colunas_como_listas <- lapply(tibble_valores, function(coluna) {
            paste0("[", paste0('"', as.character(coluna), '"', collapse = ", "), "]")
          })
          # Combinar as colunas e seus nomes como string
          tabela_string <- paste0(
            names(colunas_como_listas), "=", colunas_como_listas, collapse = "; "
          )
        } else {
          tabela_string <- NA
        }
        
        
        # Adicionar ao data frame
        resultados <- rbind(
          resultados,
          data.frame(
            Tabela = nome_tabela,
            Variavel = nome_variavel,
            Variable_Name = variable_name,
            SAS_Label = sas_label,
            English_Text = english_text,
            Target = target,
            English_Instructions = english_instructions,  # Adiciona a nova variável
            Tabela_Valores = tabela_string,  # Adiciona a string formatada
            stringsAsFactors = FALSE
          )
        )
      }, error = function(e) {
        cat("Erro ao acessar a variável", nome_variavel, "na tabela", nome_tabela, ":", conditionMessage(e), "\n")
      })
    }
    write.csv(resultados, paste0(data_type, "_nhanes_codebook_", nome_tabela, "_resultados.csv"), row.names = FALSE)
    
  }, error = function(e) {
    cat("Erro ao acessar a tabela", nome_tabela, ":", conditionMessage(e), "\n")
  })
}

# Visualizar o resultado
View(resultados)

# Salvar o data frame em um arquivo CSV
write.csv(resultados, "nhanes_codebook_lab_resultados.csv", row.names = FALSE)