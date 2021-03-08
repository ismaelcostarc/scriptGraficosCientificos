# scriptGraficosCientificos
Script R para geração de gráficos para trabalhos científicos. No início do código é possível definir os parâmetros para cada gráfico, além do caminho do arquivo .csv com os dados e o caminho do arquivo .pdf de saída.
O arquivo .csv deve ser organizado do modo mostrado no arquivo exemplo. É necessário que os números no arquivo .csv utilizem ponto, e não vírgula, para separar casas decimais.

## Execução:
O script pode ser executado no terminal com `Rscript script.r` ou na IDE RStudio.

### BUG:
Nas curvas em que o limite inferior do intervalo de confiança ultrapassa o valor y mínimo da curva, ocorre um bug onde o intervalo de confiança desaparece. Para corrigir o problema é necessário calcular o valor mínimo e modificar o código nos trechos do desenho da linha do intervalo de confiança para as curvas em que o problema ocorre.
