:- module( monografia,
            [ carrega_tab/1,
              monografia/12,
              insere/12, 
              remove/1, 
              atualiza/12 ]).

:- use_module(library(persistency)).

:- use_module(chave, []). % A lista vazia evita conflito de predicados
:- use_module(instituicao, []).
:- use_module(autor, []).
:- use_module(orientador, []).
:- use_module(palavraChave, []).
:- use_module(curso, []).

:- persistent
   monografia( id:positive_integer, % chave primária
          titulo: string,
          ano:string,
          data:string,
          nome:string,
          resumo:string,
          download:string,
          instituicao:positive_integer,
          curso:positive_integer,
          autor:positive_integer,
          orientador:positive_integer,
          palavra:positive_integer).

:- initialization( at_halt(db_sync(gc(always))) ).

carrega_tab(ArqTabela):- db_attach(ArqTabela, []).

insere(Id, Titulo, Ano, Data, Nome, Resumo, Download, Instituicao, Curso, Autor, Orientador, Palavra):-
    chave:pk(monografia, Id), % obtenha a chave primária
    instituicao:instituicao(IdInstituição, Instituicao,_,_),
    curso:curso(IdCurso, Curso,_,_),
    autor:autor(IdAutor, Autor,_),
    orientador:orientador(IdOrientador, Orientador,_),
    palavraChave:palavraChave(IdPalavra, Palavra),
    with_mutex( monografia,
                assert_monografia(Id, Titulo, Ano, Data, Nome, Resumo, Download, IdInstituição, IdCurso, IdAutor, IdOrientador, IdPalavra)).

remove(Id):- with_mutex(monografia,
                        retractall_monografia(Id, _Titulo, _Ano, _Data, _Nome, _Resumo, _Download, _IdInstituição, _IdCurso, _IdAutor, _IdOrientador, _IdPalavra)).

atualiza(Id, Titulo, Ano, Data, Nome, Resumo, Download, Instituicao, Curso, Autor, Orientador, Palavra):-
    instituicao:instituicao(IdInstituição, Instituicao,_,_),
    curso:curso(IdCurso, Curso,_,_),
    autor:autor(IdAutor, Autor,_),
    orientador:orientador(IdOrientador, Orientador,_),
    palavraChave:palavraChave(IdPalavra, Palavra),
    with_mutex(monografia,
                (retract_monografia(Id, _Titulo, _Ano, _Data, _Nome, _Resumo, _Download, _IdInstituição, _IdCurso, _IdAutor, _IdOrientador, _IdPalavra),
                 assert_monografia(Id, Titulo, Ano, Data, Nome, Resumo, Download, IdInstituição, IdCurso, IdAutor, IdOrientador, IdPalavra)) ).
                 
                 
                 

