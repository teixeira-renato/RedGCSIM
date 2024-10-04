# Algoritmo de redistribuição de causas garbage para os dados do SIM
##Versão: 01.2024a

O repositório foi criado para compartilhar as informações metodológicas e técnicas do processo de redistribuição de causas garbage para o Brasil que vem sendo desenvolvido pelo Grupo de Pesquisas em Epidemiologia e Avaliação em Saúde (GPEAS) da Faculdade de Medicina da Universidade Federal de Minas Gerais (UFMG) coordenado pela Profa. Deborah Malta.

O trabalho continua em desenvolvimento e ao surgir novas versões o repositório será atualizado pelo grupo (redegbdbrasil2015@gmail.com).

Um pacote está em desenvolvimento e em breve novas atualizações poderão facilitar o uso do algoritmo para usuários finais.

Sua ajuda é bem vinda para sugestões e identificações a serem implementadas.



## Dicas de uso

Para usar o pacote recomenda-se seguir os passos e funções descritas a seguir:

1 - Carregar a base em um data frame (df)

2 - Usar a função padroniza_idade(x = df) para padronizar a idade do SIM no padrão utilizado pelo pacote

3 - Usar a função padroniza_local(df2) para padronizar os municípios com suas respectivas informações de meso e macro região

4 - Usar a função tabela_final_1(df3) para gerar um df com as variáveis utilizadas pelo pacote e com os padrões necessários

5 - Usar a função separa_reg_ing(df4) para separar os dados com informações ignoradas

6 - Usar a função prepara_base_generalizada(df5[["completos"]]) para gerar a base completa para gerar os pesos para o processo de redistribuição

7 - Usar a função prop_causas(df6) para gerar as proporções dos dados completod

8 - Usar a função redistribuicao_dados_faltantes(base_prop = df7, dados_ign = df5[["ignorados"]]) para redistribuir aqueles dados com ignorados ou dados faltantes para sexo, idade e município de residência

9 - Usar a função separa_reg_GC(df8) para gerar os pacotes de redistribuição considerados na versão usada

10 - Aqui inicia-se o processo de redistribuição. Primeiro as causas externas. Para isso, usar a função redistribuicao_causas_externas(dados_completos = df9[["completos"]],dados_redis = df9[["redistribuir"]])

11 - Depois as causas maternase infeciosa por da função redistribuicao_causas_mat_inf(dados_completos = out.df10,dados_redis = outdf9file9[["redistribuir"]])

12 - Redistribuição das causas baseadas na investigação de óbitos: redistribuicao_causas_ivestigacao(dados_completos = df11,dados_redis = df9[["redistribuir"]], pesos =paste0(path,"/ICD_MAPPING_V6_2023 pos OPAS_nov2023.xlsx"))
13 - 
