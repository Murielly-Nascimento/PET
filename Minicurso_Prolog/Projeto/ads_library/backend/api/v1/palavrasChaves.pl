/* http_parameters   */
:- use_module(library(http/http_parameters)).
/* http_reply        */
:- use_module(library(http/http_header)).
/* reply_json_dict   */
:- use_module(library(http/http_json)).


:- use_module(bd(palavraChave), []).

/*
    GET api/v1/palavrasChaves/
    Retorna uma lista com todos os palavrasChaves.
*/
palavrasChaves(get, '', _Pedido):- !,
    envia_tabela_palavrasChaves.

/*
    GET api/v1/palavrasChaves/Id
    Retorna o ` palavraChave ` com Id 1 ou erro 404 caso o ` palavraChave ` não
    seja encontrado.
*/
palavrasChaves(get, AtomId, _Pedido):-
    atom_number(AtomId, Id),    % o identificador aparece na rota como um átomo,
    !,                          % converta-o para um número inteiro.
    envia_palavrasChaves(Id).

/*
    POST api/v1/palavrasChaves
    Adiciona um novo palavraChave.
    Os dados são passados no corpo do pedido usando o formato JSON.
    Um erro 400 (BAD REQUEST) deve ser retornado caso a URL não tenha sido
    informada.
*/
palavrasChaves(post, _, Pedido):-
    http_read_json_dict(Pedido, Dados), % lê o JSON enviado com o Pedido
    !,
    insere_palavrasChaves(Dados).

/*
    PUT api/v1/palavrasChaves/Id
    Atualiza o palavraChave com o Id dado.
    Os dados são passados no corpo do pedido usando o formato JSON.
*/
palavrasChaves(put, AtomId, Pedido):- 
    atom_number(AtomId, Id),
    http_read_json_dict(Pedido, Dados), % lê o JSON enviado com o Pedido
    !,
    atualiza_palavrasChaves(Dados, Id).

/*
    DELETE api/v1/palavrasChaves/Id
    Apaga o palavraChave com o Id informado
*/
palavrasChaves(delete, AtomId, _Pedido):-
    atom_number(AtomId, Id),
    !,
    palavraChave:remove(Id),
    throw(http_reply(no_content)).  % Responde usando o código 204 No Content

/* 
    Se algo ocorrer de errado, a resposta de método não
    permitido será retornada.
*/
palavrasChaves(Método, Id, _Pedido) :-
    % responde com o código 405 Method Not Allowed
    throw(http_reply(method_not_allowed(Método, Id))).


insere_palavrasChaves( _{ palavra:Palavra}):-
    % Validar URL antes de inserir
    palavraChave:insere(Id, Palavra)
    -> envia_palavrasChaves(Id);
    throw(http_reply(bad_request('URL ausente'))).

atualiza_palavrasChaves( _{palavra:Palavra}, Id):-
    palavraChave:atualiza(Id, Palavra)
    -> envia_palavrasChaves(Id)
    ; throw(http_reply(not_found(Id))).

envia_palavrasChaves(Id):-
    (  palavraChave:palavraChave(Id, Palavra)
    -> reply_json_dict( _{id:Id, palavra:Palavra} )
    ;  throw(http_reply(not_found(Id)))
    ).


envia_tabela_palavrasChaves :-
    findall( _{id:Id, palavra:Palavra},
             palavraChave:palavraChave(Id, Palavra),
             Tuplas ),
    reply_json_dict(Tuplas).    % envia o JSON para o solicitante
