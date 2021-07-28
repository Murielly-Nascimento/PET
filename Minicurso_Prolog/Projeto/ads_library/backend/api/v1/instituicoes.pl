/* http_parameters   */
:- use_module(library(http/http_parameters)).
/* http_reply        */
:- use_module(library(http/http_header)).
/* reply_json_dict   */
:- use_module(library(http/http_json)).


:- use_module(bd(instituicao), []).

/*
    GET api/v1/instituicoes/
    Retorna uma lista com todos os instituicoes.
*/
instituicoes(get, '', _Pedido):- !,
    envia_tabela_instituicoes.

/*
    GET api/v1/instituicoes/Id
    Retorna o ` instituicao ` com Id 1 ou erro 404 caso o ` instituicao ` não
    seja encontrado.
*/
instituicoes(get, AtomId, _Pedido):-
    atom_number(AtomId, Id),    % o identificador aparece na rota como um átomo,
    !,                          % converta-o para um número inteiro.
    envia_instituicoes(Id).

/*
    POST api/v1/instituicoes
    Adiciona um novo instituicao.
    Os dados são passados no corpo do pedido usando o formato JSON.
    Um erro 400 (BAD REQUEST) deve ser retornado caso a URL não tenha sido
    informada.
*/
instituicoes(post, _, Pedido):-
    http_read_json_dict(Pedido, Dados), % lê o JSON enviado com o Pedido
    !,
    insere_instituicoes(Dados).

/*
    PUT api/v1/instituicoes/Id
    Atualiza o instituicao com o Id dado.
    Os dados são passados no corpo do pedido usando o formato JSON.
*/
instituicoes(put, AtomId, Pedido):- 
    atom_number(AtomId, Id),
    http_read_json_dict(Pedido, Dados), % lê o JSON enviado com o Pedido
    !,
    atualiza_instituicoes(Dados, Id).

/*
    DELETE api/v1/instituicoes/Id
    Apaga o instituicao com o Id informado
*/
instituicoes(delete, AtomId, _Pedido):-
    atom_number(AtomId, Id),
    !,
    instituicao:remove(Id),
    throw(http_reply(no_content)).  % Responde usando o código 204 No Content

/* 
    Se algo ocorrer de errado, a resposta de método não
    permitido será retornada.
*/
instituicoes(Método, Id, _Pedido) :-
    % responde com o código 405 Method Not Allowed
    throw(http_reply(method_not_allowed(Método, Id))).


insere_instituicoes( _{ nome:Nome, sigla:Sigla, cidade:Cidade}):-
    % Validar URL antes de inserir
    instituicao:insere(Id, Nome, Sigla, Cidade)
    -> envia_instituicoes(Id);
    throw(http_reply(bad_request('URL ausente'))).

atualiza_instituicoes( _{nome:Nome, sigla:Sigla, cidade:Cidade}, Id):-
    instituicao:atualiza(Id, Nome, Sigla, Cidade)
    -> envia_instituicoes(Id)
    ; throw(http_reply(not_found(Id))).

envia_instituicoes(Id):-
    (  instituicao:instituicao(Id, Nome, Sigla, Cidade)
    -> reply_json_dict( _{id:Id, nome:Nome, sigla:Sigla, cidade:Cidade} )
    ;  throw(http_reply(not_found(Id)))
    ).


envia_tabela_instituicoes :-
    findall( _{id:Id, nome:Nome, sigla:Sigla, cidade:Cidade},
             instituicao:instituicao(Id, Nome, Sigla, Cidade),
             Tuplas ),
    reply_json_dict(Tuplas).    % envia o JSON para o solicitante
