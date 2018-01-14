# hs-trade
Breakable Toy para estudo de Haskell. Esta aplicação irá buscar os dados de trade da Api pública do Bitcointrade e exibir informações úteis utilizando estes dados como base.

## Build
Para construir a aplicação, utilize o stack

    stack build

Para instalar a aplicação em sua estação, utilize:

    stack install

## Execução
Para executar a aplicação, execute o binário gerado pelo stack passando qual informação deseja-se dentre as suportadas: 

- ticker
- ofertas

Exemplo:

    hs-trade ticker
    hs-trade ofertas

## TODO
- [] Formatar output de Ticker

## Estórias
- [x] Como um usuário, desejo obter o Ticker da plataforma, para que possa saber estatísticas úteis das últimas 24h de operações
- [x] Como um usuário, desejo obter o livro de ofertas da plataforma, para que eu possa saber estatísticas úteis das últimas operações como: valor total em compras / vendas, média de valor de compra / venda e diferença entre valor total de compras e vendas
- [ ] Como um usuário, desejo obter os dados de Trades da plataforma em um gráfico Candlestick, para que possa ter uma visão do histórico de operações executadas e planejar minhas próximas operações e estratégias