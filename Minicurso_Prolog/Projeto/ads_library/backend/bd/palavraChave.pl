:- module( palavraChave,
            [ carrega_tab/1,
              palavraChave/2,
              insere/2, 
              remove/1, 
              atualiza/2 ]).

:- use_module(library(persistency)).

:- use_module(chave, []). % A lista vazia evita conflito de predicados

:- persistent
palavraChave(
	id:positive_integer,  
	palavra:string ).

:- initialization( at_halt(db_sync(gc(always))) ).

carrega_tab(ArqTabela):- db_attach(ArqTabela, []).

insere(Id, Palavra):-
    chave:pk(palavraChave, Id), % obtenha a chave primária
    with_mutex( palavraChave,
                assert_palavraChave(Id, Palavra)).

remove(Id):- with_mutex(palavraChave,
                        retractall_palavraChave(Id, _Palavra)).

atualiza(Id, Palavra):-
    with_mutex(palavraChave,
                (retract_palavraChave(Id, _PalavraAntiga),
                 assert_palavraChave(Id, Palavra)) ).
