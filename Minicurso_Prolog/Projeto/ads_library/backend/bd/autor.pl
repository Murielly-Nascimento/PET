:- module( autor,
            [ carrega_tab/1,
              autor/3,
              insere/3, 
              remove/1, 
              atualiza/3 ]).

:- use_module(library(persistency)).

:- use_module(chave, []). % A lista vazia evita conflito de predicados

:- persistent
   autor( ra:positive_integer, % chave primária
          nome: string,
          email:string).

:- initialization( at_halt(db_sync(gc(always))) ).

carrega_tab(ArqTabela):- db_attach(ArqTabela, []).

insere(Ra, Nome, Email):-
    chave:pk(autor, Ra), % obtenha a chave primária
    with_mutex( autor,
                assert_autor(Ra, Nome, Email)).

remove(Ra):- with_mutex(autor,
                        retractall_autor(Ra, _Nome, _Email)).

atualiza(Ra, Nome, Email):-
    with_mutex(autor,
                (retract_autor(Ra, _NomeAntigo, _EmailAntigo),
                 assert_autor(Ra, Nome, Email)) ).
