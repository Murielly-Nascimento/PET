% http_handler
:- use_module(library(http/http_dispatch)).

% http:location está aqui
:- use_module(library(http/http_path)).

% serve_files_in_directory
:- use_module(library(http/http_server_files)).

% Para as ações de logging
:- use_module(library(http/http_log)).

% Para usar JSON
:- use_module(library(http/http_json)).

/* Aumenta a lista de tipos aceitos pelo servidor */
:- multifile http_json/1.

http_json:json_type('application/x-javascript').
http_json:json_type('text/javascript').
http_json:json_type('text/x-javascript').
http_json:json_type('text/x-json').


:- load_files([ caminhos,   % arquivo contendo os caminhos dos diretórios
                config(banco_de_dados),
                config(carrega_website)
              ],
              [ silent(true),
                if(not_loaded) ]).
