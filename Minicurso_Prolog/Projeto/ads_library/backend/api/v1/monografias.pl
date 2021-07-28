/* http_parameters   */
:- use_module(library(http/http_parameters)).
/* http_reply        */
:- use_module(library(http/http_header)).
/* reply_json_dict   */
:- use_module(library(http/http_json)).


:- use_module(bd(monografia), []).

/*
    GET api/v1/monografias/
    Retorna uma lista com todos os monografias.
*/
monografias(get, '', _Pedido):- !,
    envia_tabela_monografias.

/*
    GET api/v1/monografias/Id
    Retorna o ` monografia ` com Id 1 ou erro 404 caso o ` monografia ` não
    seja encontrado.
*/
monografias(get, AtomId, _Pedido):-
    atom_number(AtomId, Id),    % o identificador aparece na rota como um átomo,
    !,                          % converta-o para um número inteiro.
    envia_monografias(Id).

/*
    POST api/v1/monografias
    Adiciona um novo monografia.
    Os dados são passados no corpo do pedido usando o formato JSON.
    Um erro 400 (BAD REQUEST) deve ser retornado caso a URL não tenha sido
    informada.
*/
monografias(post, _, Pedido):-
    http_read_json_dict(Pedido, Dados), % lê o JSON enviado com o Pedido
    !,
    insere_monografias(Dados).

/*
    PUT api/v1/monografias/Id
    Atualiza o monografia com o Id dado.
    Os dados são passados no corpo do pedido usando o formato JSON.
*/
monografias(put, AtomId, Pedido):- 
    atom_number(AtomId, Id),
    http_read_json_dict(Pedido, Dados), % lê o JSON enviado com o Pedido
    !,
    atualiza_monografias(Dados, Id).

/*
    DELETE api/v1/monografias/Id
    Apaga o monografia com o Id informado
*/
monografias(delete, AtomId, _Pedido):-
    atom_number(AtomId, Id),
    !,
    monografia:remove(Id),
    throw(http_reply(no_content)).  % Responde usando o código 204 No Content

/* 
    Se algo ocorrer de errado, a resposta de método não
    permitido será retornada.
*/
monografias(Método, Id, _Pedido) :-
    % responde com o código 405 Method Not Allowed
    throw(http_reply(method_not_allowed(Método, Id))).


insere_monografias( _{ titulo:Titulo, ano:Ano, data:Data, nome:Nome, resumo:Resumo, download:Download, instituicao:Instituicao, curso:Curso, autor:Autor, orientador:Orientador, palavra:Palavra}):-
    % Validar URL antes de inserir
    monografia:insere(Id, Titulo, Ano, Data, Nome, Resumo, Download, Instituicao, Curso, Autor, Orientador, Palavra)
    -> envia_monografias(Id);
    throw(http_reply(bad_request('URL ausente'))).

atualiza_monografias( _{titulo:Titulo, ano:Ano, data:Data, nome:Nome, resumo:Resumo, download:Download, instituicao:Instituicao, curso:Curso, autor:Autor, orientador:Orientador, palavra:Palavra}, Id):-
    monografia:atualiza(Id, Titulo, Ano, Data, Nome, Resumo, Download, Instituicao, Curso, Autor, Orientador, Palavra)
    -> envia_monografias(Id)
    ; throw(http_reply(not_found(Id))).

envia_monografias(Id):-
    (  monografia:monografia(Id, Titulo, Ano, Data, Nome, Resumo, Download, Instituicao, Curso, Autor, Orientador, Palavra)
    -> reply_json_dict( _{id:Id, titulo:Titulo, ano:Ano, data:Data, nome:Nome, resumo:Resumo, download:Download, instituicao:Instituicao, curso:Curso, autor:Autor, orientador:Orientador, palavra:Palavra} )
    ;  throw(http_reply(not_found(Id)))
    ).


envia_tabela_monografias :-
    findall( _{id:Id, titulo:Titulo, ano:Ano, data:Data, nome:Nome, resumo:Resumo, download:Download, instituicao:Instituicao, curso:Curso, autor:Autor, orientador:Orientador, palavra:Palavra},
             monografia:monografia(Id, Titulo, Ano, Data, Nome, Resumo, Download, Instituicao, Curso, Autor, Orientador, Palavra),
             Tuplas ),
    reply_json_dict(Tuplas).    % envia o JSON para o solicitante
