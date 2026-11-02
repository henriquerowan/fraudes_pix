# Hipóteses investigativas para seleção de gráficos

Os resultados são descritivos. A base de fraudes é mensal e nacional; não permite atribuir fraude a região, município, modalidade ou perfil individual.

| Hipótese/pergunta | Variáveis | Método | Gráfico indicado |
|---|---|---|---|
| H1. Contestações e transações têm ritmos mensais semelhantes? | `QtdePixcontestados`, `quantidade_total`, `AnoMes` | variação mensal; Pearson e Spearman descritivos | linhas de índice base 100 + variação |
| H2. O valor contestado evolui no mesmo ritmo do valor transacionado? | `ValorPixcontestadosaceitos`, `valor_total` | índice base 100, variação e correlação | linhas mensais indexadas |
| H3. Quais meses têm variação atípica? | variações mensais | regra IQR (1,5×IIQ) | linha de variação com destaques |
| H4. Devolução e aceitação de contestações mudam no período? | campos de devolução/aceitação | tendência temporal descritiva | linhas ou áreas mensais |
| H5. Há concentração de valor PIX entre municípios? | `Municipio`, `valor_pagador` | participação acumulada, top 10/top 100 e HHI | curva de concentração ou top 10 |
| H6. Regiões têm trajetórias distintas de valor? | `Regiao`, `AnoMes`, `valor_pagador` | participação e evolução mensal | linhas por região |
| H7. Modalidade muda sua composição temporal? | `FORMAINICIACAO`, `NATUREZA`, `FINALIDADE`, `AnoMes` | participação mensal por categoria | área 100% empilhada |
| H8. Perfis PF/PJ diferem em valor e quantidade? | `PAG_PFPJ`, `REC_PFPJ`, `VALOR`, `QUANTIDADE` | agregação por combinação | barras agrupadas ou heatmap |

H1 e H2 não são taxas de fraude: numeradores de contestação/valores aceitos e denominadores de transações podem ter conceitos e coberturas distintos. Correlação temporal não estabelece causalidade.
