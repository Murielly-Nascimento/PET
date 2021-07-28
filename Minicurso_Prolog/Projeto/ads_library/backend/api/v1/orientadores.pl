/* http_parameters   */
:- use_module(library(http/http_parameters)).
/* http_reply        */
:- use_module(library(http/http_header)).
/* reply_json_dict   */
:- use_module(library(http/http_json)).


:- use_module(bd(orientador), []).

/*
    GET api/v1/orientadores/
    Retorna uma lista com todos os orientadores.
*/
orientadores(get, '', _Pedido):- !,
    envia_tabela_orientadores.

/*
    GET api/v1/orientadores/Id
    Retorna o ` orientador ` com Id 1 ou erro 404 caso o ` orientador ` não
    seja encontrado.
*/
orientadores(get, AtomId, _Pedido):-
    atom_number(AtomId, Id),    % o identificador aparece na rota como um átomo,
    !,                          % converta-o para um número inteiro.
    envia_orientadores(Id).

/*
    POST api/v1/orientadores
    Adiciona um novo orientador.
    Os dados são passados no corpo do pedido usando o formato JSON.
    Um erro 400 (BAD REQUEST) deve ser retornado caso a URL não tenha sido
    informada.
*/
orientadores(post, _, Pedido):-
    http_read_json_dict(Pedido, Dados), % lê o JSON enviado com o Pedido
    !,
    insere_orientadores(Dados).

/*
    PUT api/v1/orientadores/Id
    Atualiza o orientador com o Id dado.
    Os dados são passados no corpo do pedido usando o formato JSON.
*/
orientadores(put, AtomId, Pedido):- 
    atom_number(AtomId, Id),
    http_read_json_dict(Pedido, Dados), % lê o JSON enviado com o Pedido
    !,
    atualiza_orientadores(Dados, Id).

/*
    DELETE api/v1/orientadores/Id
    Apaga o orientador com o Id informado
*/
orientadores(delete, AtomId, _Pedido):-
    atom_number(AtomId, Id),
    !,
    orientador:remove(Id),
    throw(http_reply(no_content)).  % Responde usando o código 204 No Content

/* 
    Se algo ocorrer de errado, a resposta de método não
    permitido será retornada.
*/
orientadores(Método, Id, _Pedido) :-
    % responde com o código 405 Method Not Allowed
    throw(http_reply(method_not_allowed(Método, Id))).


insere_orientadores( _{ nome:Nome, email:Email}):-
    % Validar URL antes de inserir
    orientador:insere(Id, Nome, Email)
    -> envia_orientadores(Id);
    throw(http_reply(bad_request('URL ausente'))).

atualiza_orientadores( _{nome:Nome, email:Email}, Id):-
    orientador:atualiza(Id, Nome, Email)
    -> envia_orientadores(Id)
    ; throw(http_reply(not_found(Id))).

envia_orientadores(Id):-
    (  orientador:orientador(Id, Nome, Email)
    -> reply_json_dict( _{id:Id, nome:Nome, email:Email} )
    ;  throw(http_reply(not_found(Id)))
    ).


envia_tabela_orientadores :-
    findall( _{id:Id, nome:Nome, email:Email},
             orientador:orientador(Id, Nome, Email),
             Tuplas ),
    reply_json_dict(Tuplas).    % envia o JSON para o solicitante
