/* http_parameters   */
:- use_module(library(http/http_parameters)).
/* http_reply        */
:- use_module(library(http/http_header)).
/* reply_json_dict   */
:- use_module(library(http/http_json)).


:- use_module(bd(curso), []).

/*
    GET api/v1/cursos/
    Retorna uma lista com todos os cursos.
*/
cursos(get, '', _Pedido):- !,
    envia_tabela_cursos.

/*
    GET api/v1/cursos/Id
    Retorna o ` curso ` com Id 1 ou erro 404 caso o ` curso ` não
    seja encontrado.
*/
cursos(get, AtomId, _Pedido):-
    atom_number(AtomId, Id),    % o identificador aparece na rota como um átomo,
    !,                          % converta-o para um número inteiro.
    envia_cursos(Id).

/*
    POST api/v1/cursos
    Adiciona um novo curso.
    Os dados são passados no corpo do pedido usando o formato JSON.
    Um erro 400 (BAD REQUEST) deve ser retornado caso a URL não tenha sido
    informada.
*/
cursos(post, _, Pedido):-
    http_read_json_dict(Pedido, Dados), % lê o JSON enviado com o Pedido
    !,
    insere_cursos(Dados).

/*
    PUT api/v1/cursos/Id
    Atualiza o curso com o Id dado.
    Os dados são passados no corpo do pedido usando o formato JSON.
*/
cursos(put, AtomId, Pedido):- 
    atom_number(AtomId, Id),
    http_read_json_dict(Pedido, Dados), % lê o JSON enviado com o Pedido
    !,
    atualiza_cursos(Dados, Id).

/*
    DELETE api/v1/cursos/Id
    Apaga o curso com o Id informado
*/
cursos(delete, AtomId, _Pedido):-
    atom_number(AtomId, Id),
    !,
    curso:remove(Id),
    throw(http_reply(no_content)).  % Responde usando o código 204 No Content

/* 
    Se algo ocorrer de errado, a resposta de método não
    permitido será retornada.
*/
cursos(Método, Id, _Pedido) :-
    % responde com o código 405 Method Not Allowed
    throw(http_reply(method_not_allowed(Método, Id))).


insere_cursos( _{ nome:Nome, sigla:Sigla, instituicao:Instituicao}):-
    % Validar URL antes de inserir
    curso:insere(Id, Nome, Sigla, Instituicao)
    -> envia_cursos(Id);
    throw(http_reply(bad_request('URL ausente'))).

atualiza_cursos( _{nome:Nome, sigla:Sigla, instituicao:Instituicao}, Id):-
    curso:atualiza(Id, Nome, Sigla, Instituicao)
    -> envia_cursos(Id)
    ; throw(http_reply(not_found(Id))).

envia_cursos(Id):-
    (  curso:curso(Id, Nome, Sigla, Instituicao)
    -> reply_json_dict( _{id:Id, nome:Nome, sigla:Sigla, instituicao:Instituicao} )
    ;  throw(http_reply(not_found(Id)))
    ).


envia_tabela_cursos :-
    findall( _{id:Id, nome:Nome, sigla:Sigla, instituicao:Instituicao},
             curso:curso(Id, Nome, Sigla, Instituicao),
             Tuplas ),
    reply_json_dict(Tuplas).    % envia o JSON para o solicitante
