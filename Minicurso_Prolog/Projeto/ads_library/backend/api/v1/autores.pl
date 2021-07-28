/* http_parameters   */
:- use_module(library(http/http_parameters)).
/* http_reply        */
:- use_module(library(http/http_header)).
/* reply_json_dict   */
:- use_module(library(http/http_json)).


:- use_module(bd(autor), []).

/*
    GET api/v1/autores/
    Retorna uma lista com todos os autores.
*/
autores(get, '', _Pedido):- !,
    envia_tabela_autores.

/*
    GET api/v1/autores/Ra
    Retorna o ` autor ` com Ra 1 ou erro 404 caso o ` autor ` não
    seja encontrado.
*/
autores(get, AtomId, _Pedido):-
    atom_number(AtomId, Ra),    % o identificador aparece na rota como um átomo,
    !,                          % converta-o para um número inteiro.
    envia_autores(Ra).

/*
    POST api/v1/autores
    Adiciona um novo autor.
    Os dados são passados no corpo do pedido usando o formato JSON.
    Um erro 400 (BAD REQUEST) deve ser retornado caso a URL não tenha sido
    informada.
*/
autores(post, _, Pedido):-
    http_read_json_dict(Pedido, Dados), % lê o JSON enviado com o Pedido
    !,
    insere_autores(Dados).

/*
    PUT api/v1/autores/Ra
    Atualiza o autor com o Ra dado.
    Os dados são passados no corpo do pedido usando o formato JSON.
*/
autores(put, AtomId, Pedido):- 
    atom_number(AtomId, Ra),
    http_read_json_dict(Pedido, Dados), % lê o JSON enviado com o Pedido
    !,
    atualiza_autores(Dados, Ra).

/*
    DELETE api/v1/autores/Ra
    Apaga o autor com o Ra informado
*/
autores(delete, AtomId, _Pedido):-
    atom_number(AtomId, Ra),
    !,
    autor:remove(Ra),
    throw(http_reply(no_content)).  % Responde usando o código 204 No Content

/* 
    Se algo ocorrer de errado, a resposta de método não
    permitido será retornada.
*/
autores(Método, Ra, _Pedido) :-
    % responde com o código 405 Method Not Allowed
    throw(http_reply(method_not_allowed(Método, Ra))).


insere_autores( _{ nome:Nome, email:Email}):-
    % Validar URL antes de inserir
    autor:insere(Ra, Nome, Email)
    -> envia_autores(Ra);
    throw(http_reply(bad_request('URL ausente'))).

atualiza_autores( _{nome:Nome, email:Email}, Ra):-
    autor:atualiza(Ra, Nome, Email)
    -> envia_autores(Ra)
    ; throw(http_reply(not_found(Ra))).

envia_autores(Ra):-
    (  autor:autor(Ra, Nome, Email)
    -> reply_json_dict( _{ra:Ra, nome:Nome, email:Email} )
    ;  throw(http_reply(not_found(Ra)))
    ).


envia_tabela_autores :-
    findall( _{ra:Ra, nome:Nome, email:Email},
             autor:autor(Ra, Nome, Email),
             Tuplas ),
    reply_json_dict(Tuplas).    % envia o JSON para o solicitante
