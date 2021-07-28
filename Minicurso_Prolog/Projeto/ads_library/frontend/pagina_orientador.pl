:-module(pagina_orientador,
	[lista/1,
	cadastra/1,
	editar/2]).

/* html//1, reply_html_page  */
:- use_module(library(http/html_write)).

/* html_requires  */
:- use_module(library(http/html_head)).

:- use_module(bd(orientador), []).
  
lista(_):-
    reply_html_page(
        boot5rest,
        [ title('Orientadores')],
        [ div(class(container),
              [ \html_requires(css('entrada.css')),
                \html_requires(css('custom.css')),
                \html_requires(js('ads_library.js')),
                \navegação('menu-topo'),
                \tabela_de_orientadores,
                \retorna_home
              ]) ]).

tabela_de_orientadores -->
    html(div(class('container-fluid py-3'),
             [ \cabeça_da_tabela('Orientadores', '/orientador/cadastro'),
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
        findall( tr([th(scope(row), Id), td(Nome), td(Email), td(Ações)]),
                 linha(Id, Nome, Email, Ações),
                 Linhas )
    },
    html(Linhas).


linha(Id, Nome, Email, Ações):-
    orientador:orientador(Id, Nome, Email),
    ações(Id,Ações).


ações(Id, Campo):-
    Campo = [ a([ class('text-success'), title('Alterar'),
                  href('/orientador/editar/~w' - Id),
                  'data-toggle'(tooltip)],
                [ \lápis ]),
              a([ class('text-danger ms-1'), title('Excluir'),
                  href('/api/v1/orientadores/~w' - Id),
                  onClick("apagar( event, '/orientador' )"),
                  'data-toggle'(tooltip)],
                [ \lixeira ])
            ].
  
/* Página de cadastro de orientadores */

cadastra(_Pedido):-
  reply_html_page(
      boot5rest,
      [ title('Cadastro de Orientadores')],
      [ div(class(container),
            [ \html_requires(js('ads_library.js')),
              h1('Cadastrar orientadores'),
              \form_orientadores
            ]) ]).

form_orientadores -->
  html(form([ id('orientador-form'),
              onsubmit("redirecionaResposta( event, '/orientador' )"),
              action('/api/v1/orientadores/') ],
            [ \método_de_envio('POST'),
              \campo(nome, 'Nome', text),
              \campo(email, 'Email', text),
              \enviar_ou_cancelar('/orientador')
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
           
/* Página para edição (alteração) de um orientador  */

editar(AtomId, _Pedido):-
    atom_number(AtomId, Id),
    ( orientador:orientador(Id, Nome, Email)
    ->
    reply_html_page(
        boot5rest,
        [ title('Editar Orientadores')],
        [ div(class(container),
              [ \html_requires(js('ads_library.js')),
                h1('Editar Orientadores'),
                \form_autor(Id, Nome, Email)
              ]) ])
    ; throw(http_reply(not_found(Id)))
    ).


form_autor(Id, Nome, Email) -->
    html(form([ id('orientador-form'),
                onsubmit("redirecionaResposta( event, '/orientador' )"),
                action('/api/v1/orientadores/~w' - Id) ],
              [ \método_de_envio('PUT'),
                \campo_não_editável(id, 'Id', text, Id),
                \campo(nome, 'Nome', text, Nome),
                \campo(email,    'Email',    text,  Email),
                \enviar_ou_cancelar('/orientador')
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

  
  
