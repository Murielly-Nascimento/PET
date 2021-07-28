/* Instituicao
*curso(IDInstituicao,Nome,Sigla,Cidade).
*/

:-module(
	instituicao,
	[instituicao/4]).

:- use_module(library(persistency)).

:-persistent
	instituicao(	
		idinstituicao: nonneg,
		nome:atom,
		sigla:atom,
		cidade:atom
	).

:-initialization(db_attach('tbl_instituicao.pl',[])).

insere(IDInstituicao,Nome,Sigla,Cidade):-
	with_mutex(instituicao,
		assert_instituicao(IDInstituicao,Nome,Sigla,Cidade)).

remove(IDInstituicao):-
	with_mutex(instituicao,
		retract_instituicao(IDInstituicao,_Nome,_Sigla,_Cidade)).

atualiza(IDInstituicao,Nome,Sigla,Cidade):-
	with_mutex(instituicao,
		(retractall_instituicao(IDInstituicao,Nome,Sigla,Cidade),
	    assert_instituicao(IDInstituicao,Nome,Sigla,Cidade))).
sincroniza:-
	db_sync(gc(always)).

listarInstituicoes:-
	instituicao(IDInstituicao,Nome,Sigla,Cidade),
	write(IDInstituicao),tab(2),write(Nome),tab(2),write(Sigla),tab(2),write(Cidade),
	nl,fail.