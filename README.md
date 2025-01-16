# Algoritmo de Redistribuição de Causas Garbage para os Dados do SIM

**Versão: 01.2024a**

Este repositório foi criado para compartilhar informações metodológicas e técnicas relacionadas ao processo de redistribuição de causas garbage no Brasil. Este trabalho é desenvolvido pelo **Grupo de Pesquisas em Epidemiologia e Avaliação em Saúde (GPEAS)** da Faculdade de Medicina da Universidade Federal de Minas Gerais (UFMG), sob a coordenação da Profa. **Deborah Malta**.

Este algoritmo está em constante desenvolvimento, e novas versões serão disponibilizadas no repositório. Sugestões e contribuições são bem-vindas e podem ser enviadas ao pesquisador responsável Renato Teixeira  **renato115@yahoo.com**.

### **Sobre o Pacote**
Atualmente, estamos desenvolvendo um pacote em R que tem como objetivo facilitar a aplicação do algoritmo de redistribuição. Atualizações frequentes serão realizadas para aprimorar sua funcionalidade e atender às necessidades dos usuários.

Sua contribuição para melhorias, identificação de erros ou novas funcionalidades será muito valiosa.

---

## **Guia de Uso do Pacote**

Abaixo estão os passos recomendados para usar o pacote e aplicar o algoritmo:

### **Passos**

1. **Carregar a base de dados:**
   Inicie carregando os dados do SIM em um data frame (por exemplo, `df`).

2. **Padronizar as idades:**
   Use a função `padroniza_idade(x = df)` para ajustar as idades no formato esperado pelo pacote.

3. **Padronizar informações de localidade:**
   Utilize `padroniza_local(df2)` para adicionar informações de municípios, mesorregiões e macrorregiões.

4. **Criar a tabela inicial:**
   Execute `tabela_final_1(df3)` para gerar um data frame contendo as variáveis formatadas de acordo com os padrões do pacote.

5. **Separar registros ignorados:**
   A função `separa_reg_ing(df4)` separa os dados em duas categorias: registros completos e registros com informações ignoradas.

6. **Preparar a base generalizada:**
   Com `prepara_base_generalizada(df5[["completos"]])`, crie a base necessária para gerar os pesos para redistribuição.

7. **Gerar proporções:**
   Aplique a função `prop_causas(df6)` para calcular proporções entre as diferentes causas.

8. **Redistribuir dados faltantes:**
   Use `redistribuicao_dados_faltantes(base_prop = df7, dados_ign = df5[["ignorados"]])` para redistribuir registros com informações faltantes de sexo, idade e município.

9. **Separar causas garbage:**
   A função `separa_reg_GC(df8)` gera os pacotes para redistribuição de causas garbage baseados na versão atual do algoritmo.

10. **Redistribuir causas externas:**
    Use `redistribuicao_causas_externas(dados_completos = df9[["completos"]], dados_redis = df9[["redistribuir"]])` para iniciar a redistribuição de causas externas.

11. **Redistribuir causas maternas e infecciosas:**
    Aplique `redistribuicao_causas_mat_inf(dados_completos = out.df10, dados_redis = outdf9file9[["redistribuir"]])` para redistribuir causas relacionadas a óbitos maternos e infecciosos.

12. **Redistribuir causas investigadas:**
    Use `redistribuicao_causas_ivestigacao(dados_completos = df11, dados_redis = df9[["redistribuir"]], pesos = paste0(path, "/ICD_MAPPING_V6_2023 pos OPAS_nov2023.xlsx"))` para redistribuir óbitos baseados em investigações específicas.

---

## **Contribuições**
Se você deseja contribuir com o projeto, tenha dúvidas ou mais informações, entre em contato com:

- Envie sugestões ou relatórios de bugs para **renato115@yahoo.com**.
- Participe do desenvolvimento através de pull requests neste repositório.

Agradecemos seu interesse e colaboração!

---
