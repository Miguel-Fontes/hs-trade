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
- [x] Formatar output de Ticker
- [ ] Alterar Double para tipo de dados que suporte valores financeiros maiores

## Estórias
- [x] Como um usuário, desejo obter o Ticker da plataforma, para que possa saber estatísticas úteis das últimas 24h de operações
- [x] Como um usuário, desejo obter o livro de ofertas da plataforma, para que eu possa saber estatísticas úteis das últimas operações como: valor total em compras / vendas, média de valor de compra / venda e diferença entre valor total de compras e vendas
- [ ] Como um usuário, desejo obter os dados de Trades da plataforma em um gráfico Candlestick, para que possa ter uma visão do histórico de operações executadas e planejar minhas próximas operações e estratégias
- [ ] Como um usuário, desejo visualizar as ordens de compra em venda por uma valor de distribuição arbitrário, para que eu possa ter uma visão de alto nível das ofertas em livro e identificar tendências do mercado
- [ ] Como um usuário, desejo ter acesso à um guia sobre a utilização da aplicação, para que eu possa conhecer todas as opções disponíveis e aprender sobre como utilizar a aplicação.