% http_handler, http_reply_file
:- use_module(library(http/http_dispatch)).

% http:location
:- use_module(library(http/http_path)).

:- ensure_loaded(caminhos).

% Converte um apelido em rota.
% Ex:
% ? - apelido_rota(api1(usuarios), Rota).
% Rota = '/api/v1/usuarios/'

apelido_rota(Apelido, RotaCompleta):-
   http_absolute_location(Apelido, Rota, []),
   atom_concat(Rota, '/', RotaCompleta).

/***********************************
 *                                 *
 *      Rotas do Servidor Web      *
 *                                 *
 ***********************************/

:- multifile http:location/3.
:- dynamic   http:location/3.

%% http:location(Apelido, Rota, Opções)
%      Apelido é como será chamada uma Rota do servidor.
%      Os apelidos css, icons e js já estão definidos na
%      biblioteca http/http_server_files, os demais precisam
%      ser definidos.

http:location(img, root(img), []).
http:location(api, root(api), []).
http:location(api1, api(v1), []).
http:location(webfonts, root(webfonts), []).

/**************************
 *                        *
 *      Tratadores        *
 *                        *
 **************************/

% Recursos estáticos
:- http_handler( css(.),
                 serve_files_in_directory(dir_css), [prefix]).
:- http_handler( img(.),
                 serve_files_in_directory(dir_img), [prefix]).
:- http_handler( js(.),
                 serve_files_in_directory(dir_js),  [prefix]).
:- http_handler( webfonts(.),
                 serve_files_in_directory(dir_webfonts), [prefix]).
:- http_handler( '/favicon.ico',
                 http_reply_file(dir_img('favicon.ico', [])),
                 []).

% Rotas do Frontend

%% A página inicial
:- http_handler( root(.), entrada,   []).

%% A página de termos de uso
:- http_handler( root(termos), termos,   []).

% Tabelas

:-use_module(frontend(pagina_instituicao),[]).

%% INSTITUIÇÃO

%% A página de listagem das instituições
:- http_handler( root(instituicao), pagina_instituicao:lista, []).

%% A página de cadastro de novas instituições
:- http_handler( root(instituicao/cadastro), pagina_instituicao:cadastra, []).

%% A página para edição de uma instituição existente
:- http_handler( root(instituicao/editar/Id), pagina_instituicao:editar(Id), []).


:-use_module(frontend(pagina_autor),[]).

%% AUTOR

:- http_handler( root(autor), pagina_autor:lista, []).

:- http_handler( root(autor/cadastro), pagina_autor:cadastra, []).

:- http_handler( root(autor/editar/Id), pagina_autor:editar(Id), []).



:-use_module(frontend(pagina_orientador),[]).

%% ORIENTADOR

:- http_handler( root(orientador), pagina_orientador:lista, []).

:- http_handler( root(orientador/cadastro), pagina_orientador:cadastra, []).

:- http_handler( root(orientador/editar/Id), pagina_orientador:editar(Id), []).



:-use_module(frontend(pagina_curso),[]).

%% CURSO

:- http_handler( root(curso), pagina_curso:lista, []).

:- http_handler( root(curso/cadastro), pagina_curso:cadastra, []).

:- http_handler( root(curso/editar/Id), pagina_curso:editar(Id), []).



:-use_module(frontend(pagina_monografia),[]).

%% MONOGRAFIA

:- http_handler( root(monografia), pagina_monografia:lista, []).

:- http_handler( root(monografia/cadastro), pagina_monografia:cadastra, []).

:- http_handler( root(monografia/editar/Id), pagina_monografia:editar(Id), []).



:-use_module(frontend(pagina_palavraChave),[]).

%% PALAVRA CHAVE

:- http_handler( root(palavraChave), pagina_palavraChave:lista, []).

:- http_handler( root(palavraChave/cadastro), pagina_palavraChave:cadastra, []).

:- http_handler( root(palavraChave/editar/Id), pagina_palavraChave:editar(Id), []).



% Rotas da API
:- http_handler( api1(cursos/Id), cursos(Método, Id),
                 [ method(Método),
                   methods([ get, post, put, delete ]) ]).

:- http_handler( api1(instituicoes/Id), instituicoes(Método, Id),
                [ method(Método),
                  methods([ get, post, put, delete ]) ]).

:- http_handler( api1(autores/Id), autores(Método, Id),
                [ method(Método),
                  methods([ get, post, put, delete ]) ]).


:- http_handler( api1(monografias/Id), monografias(Método, Id),
                [ method(Método),
                  methods([ get, post, put, delete ]) ]).

:- http_handler( api1(orientadores/Id), orientadores(Método, Id),
                [ method(Método),
                  methods([ get, post, put, delete ]) ]).

:- http_handler( api1(palavrasChaves/Id), palavrasChaves(Método, Id),
                [ method(Método),
                  methods([ get, post, put, delete ]) ]).
                  

