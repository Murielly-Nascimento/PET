:- module( instituicao,
            [ carrega_tab/1,
              instituicao/4,
              insere/4, 
              remove/1, 
              atualiza/4 ]).

:- use_module(library(persistency)).

:- use_module(chave, []). % A lista vazia evita conflito de predicados

:- persistent
   instituicao( id:positive_integer, % chave primária
          nome: string,
          sigla:string,
          cidade:string).

:- initialization( at_halt(db_sync(gc(always))) ).

carrega_tab(ArqTabela):- db_attach(ArqTabela, []).

insere(Id, Nome, Sigla, Cidade):-
    chave:pk(instituicao, Id), % obtenha a chave primária
    with_mutex( instituicao,
                assert_instituicao(Id, Nome, Sigla, Cidade)).

remove(Id):- with_mutex(instituicao,
                        retractall_instituicao(Id, _Nome, _Sigla, _Cidade)).

atualiza(Id, Nome, Sigla, Cidade):-
    with_mutex(curso,
                (retract_instituicao(Id, _NomeAntigo, _SiglaAntiga, _CidadeAntiga),
                 assert_instituicao(Id, Nome, Sigla, Cidade)) ).
