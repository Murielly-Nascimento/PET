/* Curso
*curso(IDCurso,Nome,Sigla).
*/

:-module(
	curso,
	[curso/3]).

:- use_module(library(persistency)).

:-persistent
	curso(
		idcurso: nonneg,
  		nome:atom,
  		sigla:atom
	).

:-initialization(db_attach('tbl_curso.pl',[])).

insere(IDCurso,Nome,Sigla):-
	with_mutex(curso,
		assert_curso(IDCurso,Nome,Sigla)).

remove(IDCurso):-
	with_mutex(curso,
		retract_curso(IDCurso,_Nome,_Sigla)).

atualiza(IDCurso,Nome,Sigla):-
	with_mutex(curso,
		(retractall_curso(IDCurso,Nome,Sigla),
		assert_curso(IDCurso,Nome,Sigla))).

sincroniza:-
	db_sync(gc(always)).

listarCursos:-
	curso(IDCurso,Nome,Sigla),
	write(IDCurso),tab(2),write(Nome),tab(2),write(Sigla),
	nl,fail.
