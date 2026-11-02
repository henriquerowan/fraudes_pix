# Relatório do pipeline analítico PIX

**Execução:** 2026-11-02 16:18:36

## Metodologia

As três bases foram lidas usando os tipos, a localidade e os códigos de ausência definidos em `R/config.R`. Os CSVs originais não foram alterados. Foram produzidos diagnósticos de esquema, ausências, parsing, dimensões e cobertura temporal. `AnoMes` foi validado como `YYYYMM`; meses inválidos foram registrados e não entram nas agregações temporais.

As séries nacionais foram agregadas por mês antes da junção, evitando junções muitos-para-muitos. A integração é exclusivamente por `AnoMes` e compara apenas meses comuns. A base municipal foi agregada por município identificado e a base nacional por dimensões categóricas disponíveis.

## Resumo

- Meses nacionais em comum: **52**.
- Municípios agregados: **5571**.
- Dimensão selecionada para o gráfico 6: **FORMAINICIACAO**.
- Meses-indicador sinalizados pelo IQR: **8**.

## Hipóteses e resultados

```
# A tibble: 3 × 8
  tipo_teste           
  <chr>                
1 Correlação de Pearson
2 Tendência log-linear 
3 Tendência log-linear 
  hipotese                                                                      
  <chr>                                                                         
1 H1: variações mensais nas contestações estão associadas às variações na quant…
2 H2: a quantidade mensal de PIX contestados apresenta tendência temporal posit…
3 H3: a quantidade mensal total de transações PIX apresenta tendência temporal …
      n correlacao_pearson  p_valor
  <int>              <dbl>    <dbl>
1    51              0.378 6.26e- 3
2    52             NA     4.67e-34
3    52             NA     1.17e-31
  status                                         
  <chr>                                          
1 Testada; associação não implica causalidade    
2 Tendência descritiva; não demonstra causalidade
3 Tendência descritiva; não demonstra causalidade
  crescimento_mensal_estimado_pct     r2
                            <dbl>  <dbl>
1                           NA    NA    
2                            6.87  0.949
3                            3.44  0.937
```

Os testes são exploratórios. Um p-valor pequeno não comprova causalidade, nem valida contestações como fraude. Tendências log-lineares são ajustadas somente a valores positivos e não constituem previsão.

## Gráficos produzidos

1. Evolução comparada das contestações e transações (índice base 100).
2. Variações percentuais mensais e observações atípicas pelo IQR.
3. Até dez municípios com maior valor PIX pago.
4. Concentração acumulada do valor pago por município.
5. Composição por forma de iniciação, comparando participação em valor e quantidade quando a dimensão está disponível.
6. Evolução temporal por categoria; dimensão escolhida: FORMAINICIACAO.

## Limitações

- PIX contestado ou contestação aceita não equivale a fraude comprovada.
- As bases não possuem chave transacional compartilhada; associação nacional é apenas mensal e não identifica causa.
- Correlação entre séries temporais pode refletir tendência, sazonalidade ou autocorrelação; não é evidência causal.
- Ausências são documentadas, não imputadas. Somas ignoram `NA`, e resultados devem ser interpretados junto aos diagnósticos.
- Municípios sem código IBGE não são atribuídos; isso pode alterar a participação e concentração calculadas.
- Se a coluna de forma de iniciação ou categorias adequadas estiver ausente, o gráfico correspondente registra indisponibilidade em vez de inventar dados.

## Arquivos

- Diagnósticos: `outputs/pipeline/diagnostico/`.
- Resultados: `outputs/pipeline/resultados/`.
- Gráficos PNG (300 dpi) e PDFs opcionais: `outputs/graficos/`.
