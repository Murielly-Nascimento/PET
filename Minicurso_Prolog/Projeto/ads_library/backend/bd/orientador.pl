:- module( orientador,
            [ carrega_tab/1,
              orientador/3,
              insere/3, 
              remove/1, 
              atualiza/3 ]).

:- use_module(library(persistency)).

:- use_module(chave, []). % A lista vazia evita conflito de predicados

:- persistent
   orientador(  id:positive_integer, % chave primária
                nome: string,
                email:string).

:- initialization( at_halt(db_sync(gc(always))) ).

carrega_tab(ArqTabela):- db_attach(ArqTabela, []).

insere(Id, Nome, Email):-
    chave:pk(orientador, Id), % obtenha a chave primária
    with_mutex( orientador,
                assert_orientador(Id, Nome, Email)).

remove(Id):- with_mutex(orientador,
                        retractall_orientador(Id, _Nome, _Email)).

atualiza(Id, Nome, Email):-
    with_mutex(orientador,
                (retract_orientador(Id, _NomeAntigo, _EmailAntigo),
                 assert_orientador(Id, Nome, Email)) ).
