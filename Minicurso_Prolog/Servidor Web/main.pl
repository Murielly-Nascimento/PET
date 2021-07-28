:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).

servidor(Porta) :-
    http_server(http_dispatch, [port(Porta)]).

:- http_handler(/, oi, []).

oi(_Pedido) :-
    format('Content-type: text/plain~n~n'),
    format('Oi Mundo!~n').
