# hs-trade
Breakable Toy para estudo de Haskell. Esta aplicação irá buscar os dados de trade da Api pública do Bitcointrade e exibir informações úteis utilizando estes dados como base.

- Development: [![Build Status](https://travis-ci.org/Miguel-Fontes/hs-trade.svg?branch=development)](https://travis-ci.org/Miguel-Fontes/hs-trade)
- Master: [![Build Status](https://travis-ci.org/Miguel-Fontes/hs-trade.svg?branch=master)](https://travis-ci.org/Miguel-Fontes/hs-trade)


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

## Tarefas, Histórias e Roadmap
### v1.1.0
- [x] Ticker - Formatar output de Ticker (#E1)
- [x] Ofertas + Ticker - Corrigir exibição de valores em notação científica (#E1, #E2)
- [x] CI - Criar estrutura de testes e configurar Travis (#E7)
- [x] CI - Adicionar testes, atingindo alta cobertura (#E7)
- [x] CI - Automatizar geração de GitHub Release via Travis (#E7)
- [ ] Ofertas - Exibir mais informações sobre os ranges de trades nos dados agrupados (limites do range; 30.000 ~ 40.000: 132) (#E4)
- [x] Ofertas - Exibir para venda e compra, o trade no topo, fundo e meio do Book de Ofertas (#E10)

### v1.2.0
- [ ] Criar pacote DEB para aplicação (#E6)
- [ ] Automatizar criação de pacote DEB (#E6)
- [ ] Implementar library de CLI App para Projeto (#E5)
- [ ] Criar menu de ajuda, acessível via HELP (#E5)
- [ ] Exibir versão da aplicação em Menu --version (#E5)

### Pool
- [ ] Obter dados de Trades da Plataforma (#E3)
- [ ] Gerar gráfico Candlestick com Trades (#E3)
- [ ] Mover lógica de exibição para console de binário Main para Módulo especializado (#E12)
- [ ] Configurar Travis para construir aplicação em OS Windows (#14)
- [ ] Configurar Travis para construir aplicação em OS OSX (#14)
- [ ] Remover ações IO () de módulos Ticker e Orders (#13)
- [ ] Remover de módulos Order e Ticker formatação para console (#E12)
- [ ] Criar Datatype para Ordergroup e remover a necessidade da função prettify(#E12)

## Estórias
- [x] #E1 - Como um usuário, desejo obter o Ticker da plataforma, para que possa saber estatísticas úteis das últimas 24h de operações
- [x] #E2 - Como um usuário, desejo obter o livro de ofertas da plataforma, para que eu possa saber estatísticas úteis das últimas operações como: valor total em compras / vendas, média de valor de compra / venda e diferença entre valor total de compras e vendas
- [ ] #E3 - Como um usuário, desejo obter os dados de Trades da plataforma em um gráfico Candlestick, para que possa ter uma visão do histórico de operações executadas e planejar minhas próximas operações e estratégias
- [x] #E4 - Como um usuário, desejo visualizar as ordens de compra e venda por uma valor de distribuição arbitrária, para que eu possa ter uma visão de alto nível das ofertas em livro e identificar tendências do mercado
- [ ] #E5 - Como um usuário, desejo ter acesso à um guia sobre a utilização da aplicação, para que eu possa conhecer todas as opções disponíveis e aprender sobre como utilizar a aplicação.
- [ ] #E6 - Como um usuário, desejo instalar a aplicação através de um Instalador / Package Manager, para que eu possa instalar e atualizar a aplicação com um único comando.
- [x] #E7 - Como um desenvolvedor, desejo que a aplicação esteja configurada em um pipeline de integração contínua, para que eu possa ter certeza de que o codebase está em bom estado, identificar problemas cedo e corrigí-los rapidamente.
- [ ] #E8 - Como um usuário, desejo ter visualizar dados passados pelo aplicativo, para que eu possa visualizar a evolução do mercado e identificar tendências
- [ ] #E9 - Como um usuário, desejo exportar os dados de Ticker para um arquivo persistente e em formato compatível com worksheet, para que eu possa analisar e criar estatísticas próprias
- [ ] #E10 - Como um usuário, desejo visualizar as ordens de compra e venda do topo, meio, e fundo do book de ofertas, para ter uma visão da varição de preços do mercado
- [ ] #E11 - Como um usuário, desejo exportar os dados de Ofertas para um arquivo persistente e em formato compatível com worksheet, para que eu possa analisar e criar estatísticas próprias
- [ ] #E12 - Como um desenvolvedor, desejo que os módulos de processamento de dados de Ofertas e Trade desconheçam o mecanismo de entrega de seus dados ao cliente, garantindo que estes possam ser reutilizados com outras UIs posteriormente
- [ ] #13 - Como um desenvolvedor, desejo que as ações de IO () sejam removidas dos módulos Ofertas e Trade, garantindo que estes não conheçam a origem dos dados e permanecem apenas com funções puras
- [ ] #14 - Como um usuário, desejo utilizar a aplicação nas plataformas Windows, Linux e OSX, para que eu possa acessar a aplicação em qualquer estação que possua