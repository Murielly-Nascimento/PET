:-module(pagina_autor,
	[lista/1,
	cadastra/1,
	editar/2]).

/* html//1, reply_html_page  */
:- use_module(library(http/html_write)).

/* html_requires  */
:- use_module(library(http/html_head)).

:- use_module(bd(autor), []).
  
lista(_):-
    reply_html_page(
        boot5rest,
        [ title('Autores')],
        [ div(class(container),
              [ \html_requires(css('entrada.css')),
              	 \html_requires(css('custom.css')),
                \html_requires(js('ads_library.js')),
                \navegação('menu-topo'),
                \tabela_de_autores,
                \retorna_home
              ]) ]).

tabela_de_autores -->
    html(div(class('container-fluid py-3'),
             [ \cabeça_da_tabela('Autores', '/autor/cadastro'),
               \tabela
             ]
            )).


cabeça_da_tabela(Título,Link) -->
    html( div(class('d-flex'),
              [ div(class('me-auto p-2'), h2(b(Título))),
                div(class('p-2'),
                    a([ href(Link), class('btn btn-primary')],
                      'Novo'))
              ]) ).


tabela -->
    html(div(class('table-responsive-md'),
             table(class('table table-striped w-100'),
                   [ \cabeçalho,
                     tbody(\corpo_tabela)
                   ]))).

cabeçalho -->
    html(thead(tr([ th([scope(col)], '#'),
                    th([scope(col)], 'Nome'),
                    th([scope(col)], 'Email'),
                    th([scope(col)], 'Ações')
                  ]))).



corpo_tabela -->
    {
        findall( tr([th(scope(row), Ra), td(Nome), td(Email), td(Ações)]),
                 linha(Ra, Nome, Email, Ações),
                 Linhas )
    },
    html(Linhas).


linha(Ra, Nome, Email, Ações):-
    autor:autor(Ra, Nome, Email),
    ações(Ra,Ações).


ações(Ra, Campo):-
    Campo = [ a([ class('text-success'), title('Alterar'),
                  href('/autor/editar/~w' - Ra),
                  'data-toggle'(tooltip)],
                [ \lápis ]),
              a([ class('text-danger ms-1'), title('Excluir'),
                  href('/api/v1/autores/~w' - Ra),
                  onClick("apagar( event, '/autor' )"),
                  'data-toggle'(tooltip)],
                [ \lixeira ])
            ].
  
/* Página de cadastro de autores */

cadastra(_Pedido):-
  reply_html_page(
      boot5rest,
      [ title('Cadastro de Autores')],
      [ div(class(container),
            [ \html_requires(js('ads_library.js')),
              h1('Cadastrar autores'),
              \form_autores
            ]) ]).

form_autores -->
  html(form([ id('autor-form'),
              onsubmit("redirecionaResposta( event, '/autor' )"),
              action('/api/v1/autores/') ],
            [ \método_de_envio('POST'),
              \campo(nome, 'Nome', text),
              \campo(email, 'Email', text),
              \enviar_ou_cancelar('/autor')
            ])).


enviar_ou_cancelar(RotaDeRetorno) -->
  html(div([ class('btn-group'), role(group), 'aria-label'('Enviar ou cancelar')],
           [ button([ type(submit),
                      class('btn btn-outline-primary')], 'Enviar'),
             a([ href(RotaDeRetorno),
                 class('ms-3 btn btn-outline-danger')], 'Cancelar')
          ])).



campo(Nome, Rótulo, Tipo) -->
  html(div(class('mb-3'),
           [ label([ for(Nome), class('form-label') ], Rótulo),
             input([ type(Tipo), class('form-control'),
                     id(Nome), name(Nome)])
           ] )).
           
/* Página para edição (alteração) de um autor  */

editar(AtomId, _Pedido):-
    atom_number(AtomId, Ra),
    ( autor:autor(Ra, Nome, Email)
    ->
    reply_html_page(
        boot5rest,
        [ title('Editar Autores')],
        [ div(class(container),
              [ \html_requires(js('ads_library.js')),
                h1('Editar Autores'),
                \form_autor(Ra, Nome, Email)
              ]) ])
    ; throw(http_reply(not_found(Ra)))
    ).


form_autor(Ra, Nome, Email) -->
    html(form([ id('autor-form'),
                onsubmit("redirecionaResposta( event, '/autor' )"),
                action('/api/v1/autores/~w' - Ra) ],
              [ \método_de_envio('PUT'),
                \campo_não_editável(ra, 'Ra', text, Ra),
                \campo(nome, 'Nome', text, Nome),
                \campo(email,    'Email',    text,  Email),
                \enviar_ou_cancelar('/autor')
              ])).


campo(Nome, Rótulo, Tipo, Valor) -->
    html(div(class('mb-3'),
             [ label([ for(Nome), class('form-label')], Rótulo),
               input([ type(Tipo), class('form-control'),
                       id(Nome), name(Nome), value(Valor)])
             ] )).

campo_não_editável(Nome, Rótulo, Tipo, Valor) -->
    html(div(class('mb-3 w-25'),
             [ label([ for(Nome), class('form-label')], Rótulo),
               input([ type(Tipo), class('form-control'),
                       id(Nome),
                       % name(Nome),%  não é para enviar o id
                       value(Valor),
                       readonly ])
             ] )).

método_de_envio(Método) -->
  html(input([type(hidden), name('_método'), value(Método)])).

  
  
