:- module( curso,
            [ carrega_tab/1,
              curso/4,
              insere/4, 
              remove/1, 
              atualiza/4 ]).

:- use_module(library(persistency)).

:- use_module(chave, []). % A lista vazia evita conflito de predicados
:- use_module(instituicao, []).

:- persistent
   curso( id:positive_integer, % chave primária
          nome: string,
          sigla:string,
          instituicao:positive_integer).

:- initialization( at_halt(db_sync(gc(always))) ).

carrega_tab(ArqTabela):- db_attach(ArqTabela, []).

insere(Id, Nome, Sigla, Universidade):-
    chave:pk(curso, Id), % obtenha a chave primária
    instituicao:instituicao(IdInstituição, Universidade,_,_),
    with_mutex( curso,
                assert_curso(Id, Nome, Sigla, IdInstituição)).

remove(Id):- with_mutex(curso,
                        retractall_curso(Id, _Nome, _Sigla, _IdInstituicao)).

atualiza(Id, Nome, Sigla, Universidade):-
    instituicao:instituicao(IdInstituição, Universidade,_,_),
    with_mutex(curso,
                (retract_curso(Id, _NomeAntigo, _SiglaAntiga, _IdInstituicaoAntiga),
                 assert_curso(Id, Nome, Sigla, IdInstituição)) ).

