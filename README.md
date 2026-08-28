# RedGCSIM: Algoritmo de Redistribuição de Causas Garbage nos Dados do SIM

**Versão: 01.2024a**

Este pacote implementa um algoritmo para redistribuição de causas garbage nos dados do Sistema de Informações sobre Mortalidade (SIM) no Brasil. Desenvolvido pelo **Grupo de Pesquisas em Epidemiologia e Avaliação em Saúde (GPEAS)** da Faculdade de Medicina da UFMG, sob coordenação da Profa. **Deborah Malta**.

> ⚠️ Este pacote está em **desenvolvimento ativo**. Contribuições, sugestões e correções são bem-vindas!  
> ✉️ Contato: **Renato Teixeira** – [renato115@yahoo.com](mailto:renato115@yahoo.com)

---

## 💾 Destaques metodológicos

Os dados ignorados são redistribuídos segundo local (município), ano, sexo, idade e causa básica.

A causa básica de óbito do SIM, que utiliza a CID-10, foi categorizada por uma tabulação inspirada no estudo GBD 2017. Entretanto, alterações foram feitas e amesma conta no data frame ICD. 

Foram utilizados dois métodos para criação dos pesos de redistirbuição:
-Proporcional;
-Investigação de óbitos (Y34, X59 e pneumonias).



## 📦 Instalação

Você pode instalar a versão de desenvolvimento diretamente do GitHub:

```r
# Instale os pacotes necessários para instalar do GitHub
install.packages("devtools")

# Instale o RedGCSIM
devtools::install_github("teixeira-renato/RedGCSIM")
```

---

## 🚀 Começando

Carregue o pacote após a instalação:

```r
library(RedGCSIM)
```

Prepare sua base de dados do SIM (`.dbc` ou `.csv`) e siga os passos abaixo para aplicar o algoritmo.

---

## 🔁 Fluxo de Uso – Passo a Passo

Abaixo está o fluxo recomendado para utilizar o pacote:

### 1. **Importar os dados**
```r
dados <- rio::import("SIM_arquivo.csv")
```

### 2. **Padronizar idade**
```r
dados_idade <- padroniza_idade(dados)
```

### 3. **Padronizar localidade**
```r
dados_local <- padroniza_local(dados_idade)
```

### 4. **Gerar tabela inicial**
```r
tabela_inicial <- tabela_final_1(dados_local)
```

### 5. **Separar registros ignorados**
```r
dados_sep <- separa_reg_ing(tabela_inicial)
```

### 6. **Preparar base generalizada**
```r
base_generalizada <- prepara_base_generalizada(dados_sep[["completos"]])
```

### 7. **Calcular proporções**
```r
proporcoes <- prop_causas(base_generalizada)
```

### 8. **Redistribuir dados faltantes (sexo, idade, município)**
```r
dados_faltantes_redistribuidos <- redistribuicao_dados_faltantes(
  base_prop = proporcoes,
  dados_ign = dados_sep[["ignorados"]]
)
```

### 9. **Separar causas garbage**
```r
dados_gc <- separa_reg_GC(dados_faltantes_redistribuidos)
```

### 10. **Redistribuir causas externas**
```r
completo_externas <- redistribuicao_causas_externas(
  dados_completos = dados_gc[["completos"]],
  dados_redis = dados_gc[["redistribuir"]]
)
```

### 11. **Redistribuir causas maternas e infecciosas**
```r
dados_mat_inf <- redistribuicao_causas_mat_inf(
  dados_completos = completo_externas,
  dados_redis = dados_gc[["redistribuir"]]
)
```

### 12. **Redistribuir causas sob investigação**
```r
final <- redistribuicao_causas_ivestigacao(
  dados_completos = dados_mat_inf,
  dados_redis = dados_gc[["redistribuir"]]
)
```

---

## 🧰 Funções Principais

| Função                                 | Descrição                                                                 |
|----------------------------------------|---------------------------------------------------------------------------|
| `padroniza_idade()`                    | Reclassifica faixas etárias para análise                                 |
| `padroniza_local()`                    | Gera colunas padronizadas de município e ano                             |
| `tabela_final_1()`                     | Organiza variáveis para redistribuição                                   |
| `separa_reg_ing()`                    | Separa registros com sexo/idade/município ignorados                      |
| `prepara_base_generalizada()`         | Cria base de referência com totais por grupo populacional                |
| `prop_causas()`                        | Calcula proporções por causa                                             |
| `redistribuicao_dados_faltantes()`    | Redistribui registros com dados ignorados                                |
| `separa_reg_GC()`                      | Identifica causas garbage                                                |
| `redistribuicao_causas_externas()`    | Redistribui causas externas específicas                                  |
| `redistribuicao_causas_mat_inf()`     | Redistribui causas maternas e infecciosas                                |
| `redistribuicao_causas_ivestigacao()` | Redistribui causas investigadas por equipes locais                       |

---

## 🤝 Contribuindo

Sinta-se à vontade para:

- Relatar bugs ou problemas
- Sugerir novas funcionalidades

**Contato:** [renato115@yahoo.com](mailto:renato115@yahoo.com)  
**Repositório:** [github.com/teixeira-renato/RedGCSIM](https://github.com/teixeira-renato/RedGCSIM)

---
