% http_server
:- use_module(library(http/thread_httpd)).

% http_dispatch
:- use_module(library(http/http_dispatch)).

%  http_set_session_options
:- use_module(library(http/http_session)).

servidor(Porta) :-
    % As sessões serão criadas quando forem explicitamente solicitadas
    http_set_session_options([ create(noauto) ]),
    http_server(http_dispatch, [port(Porta)]).
