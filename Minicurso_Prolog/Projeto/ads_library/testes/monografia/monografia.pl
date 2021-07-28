:- module(
       monografia,
       [ carrega_tab/1,
         monografia/13,
         insere/13,
         remove/1,
         atualiza/13 ]).

:- use_module(library(persistency)).

:- use_module(chave, []).
:- use_module(instituicao, []).
:- use_module(curso, []).
:- use_module(funcionario, []).
:- use_module(autor, []).
:- use_module(orientador, []).
:- use_module(palavraChave, []).

:- persistent
   monografia( id:postive_integer,
             	título:string,
             	anoApresentação:positive_integer,
             	dataCadastro:string,
             	nomeArquivo:string,
             	resumo:string,
             	numDownload:positive_integer,
             	instituicao:positive_integer,
             	curso:positive_integer,
             	orientador:positive_integer,
             	autor:positive_integer,
             	funcionario:positive_integer,
             	palavraChave:positive_integer).


:- initialization( at_halt(db_sync(gc(always))) ).

carrega_tab(ArqTabela):-
    db_attach(ArqTabela, []).


insere(Id, Título, AnoApresentacao, DataCadastro, NomeArquivo, Resumo, NumDownload, Universidade, Graduação, Professor, Escritor, Login, Palavra):-
    atom_number(NumDownload, Download),
    atom_number(AnoApresentacao, Ano),
    chave:pk(monografia, Id), % obtenha a chave primária
    instituicao:instituicao(IdInstituição,Universidade,_,_),
    curso:curso(IdCurso,Graduação,_,_),
    orientador:orientador(IdOrientador,Professor,_),
    autor:autor(IdAutor,Escritor,_),
    funcionario:funcionario(IdFuncionario,Login,_,_),
    palavraChave:palavraChave(IdPalavraChave,Palavra),
    with_mutex(monografia,
               assert_monografia(Id, Título, Ano, DataCadastro, NomeArquivo, Resumo, Download, IdInstituição, IdCurso, IdOrientador, IdAutor, IdFuncionario, IdPalavraChave)).

remove(Id):-
    with_mutex(monografia,
               retractall_monografia(Id, _Título, _AnoApresentacao, _DataCadastro, _NomeArquivo, _Resumo, _NumDownload, _Instituição, _Curso, _Orientador, _Autor, _Funcionario, _PalavraChave)).


atualiza(Id, Título, AnoApresentacao, DataCadastro, NomeArquivo, Resumo, NumDownload, Universidade, Graduação, Professor, Escritor, Login, Palavra):-
    atom_number(NumDownload, Download),
    atom_number(AnoApresentacao, Ano),
    instituicao:instituicao(IdInstituição,Universidade,_,_),
    curso:curso(IdCurso,Graduação,_,_),
    orientador:orientador(IdOrientador,Professor,_),
    autor:autor(IdAutor,Escritor,_),
    funcionario:funcionario(IdFuncionario,Login,_,_),
    palavraChave:palavraChave(IdPalavraChave,Palavra),
    with_mutex(monografia,
               ( retract_monografia(Id, _TítuloAntigo, _AnoAntigo, _DataCadastroAntigo, _NomeArquivoAntigo, _ResumoAntigo, _DownloadAntigo, _InstituiçãoAntigo, _CursoAntigo, _OrientadorAntigo, _AutorAntigo, _FuncionarioAntigo, _PalavraChaveAntigo),
                 assert_monografia(Id, Título, Ano, DataCadastro, NomeArquivo, Resumo, Download, IdInstituição, IdCurso, IdOrientador, IdAutor, IdFuncionario, IdPalavraChave)) ).
                 
                 
                 
                 
                 
                 
                 
