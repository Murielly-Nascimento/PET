:-module(pagina_palavraChave,
	[lista/1,
	cadastra/1,
	 editar/2]).

/* html//1, reply_html_page  */
:- use_module(library(http/html_write)).

/* html_requires  */
:- use_module(library(http/html_head)).

:- use_module(bd(palavraChave), []).

lista(_):-
    reply_html_page(
        boot5rest,
        [ title('Palavras Chave')],
        [ div(class(container),
              [ \html_requires(css('entrada.css')),
                \html_requires(css('custom.css')),
                \html_requires(js('ads_library.js')),
                \navegação('menu-topo'),
                \tabela_de_palavras,
                \retorna_home
              ]) ]).

tabela_de_palavras -->
    html(div(class('container-fluid py-3'),
             [ \cabeça_da_tabela('Palavras Chaves', '/palavraChave/cadastro'),
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
                    th([scope(col)], 'Palavra'),
                    th([scope(col)], 'Ações')
                  ]))).



corpo_tabela -->
    {
        findall( tr([th(scope(row), Id), td(Palavra), td(Ações)]),
                 linha(Id, Palavra, Ações),
                 Linhas )
    },
    html(Linhas).


linha(Id, Palavra, Ações):-
    palavraChave:palavraChave(Id, Palavra),
    ações(Id,Ações).


ações(Id, Campo):-
    Campo = [ a([ class('text-success'), title('Alterar'),
                  href('/palavraChave/editar/~w' - Id),
                  'data-toggle'(tooltip)],
                [ \lápis ]),
              a([ class('text-danger ms-1'), title('Excluir'),
                  href('/api/v1/palavrasChaves/~w' - Id),
                  onClick("apagar( event, '/palavraChave' )"),
                  'data-toggle'(tooltip)],
                [ \lixeira ])
            ].
  
/* Página de cadastro de palavras chaves */

cadastra(_Pedido):-
  reply_html_page(
      boot5rest,
      [ title('Cadastro de Palavras Chaves')],
      [ div(class(container),
            [ \html_requires(js('ads_library.js')),
              h1('Cadastrar Palavras Chaves'),
              \form_palavrasChaves
            ]) ]).

form_palavrasChaves -->
  html(form([ id('palavra-form'),
              onsubmit("redirecionaResposta( event, '/palavraChave' )"),
              action('/api/v1/palavrasChaves/') ],
            [ \método_de_envio('POST'),
              \campo(palavra, 'Palavras Chaves', text),
              \enviar_ou_cancelar('/palavraChave')
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
           
/* Página para edição (alteração) de uma Palavra  */

editar(AtomId, _Pedido):-
    atom_number(AtomId, Id),
    ( palavraChave:palavraChave(Id, Palavra)
    ->
    reply_html_page(
        boot5rest,
        [ title('Editar Palavras Chaves')],
        [ div(class(container),
              [ \html_requires(js('ads_library.js')),
                h1('Editar Palavras'),
                \form_palavraChave(Id, Palavra)
              ]) ])
    ; throw(http_reply(not_found(Id)))
    ).


form_palavraChave(Id, Palavra) -->
    html(form([ id('palavraChave-form'),
                onsubmit("redirecionaResposta( event, '/palavraChave' )"),
                action('/api/v1/palavrasChaves/~w' - Id) ],
              [ \método_de_envio('PUT'),
                \campo_não_editável(id, 'Id', text, Id),
                \campo(palavra, 'Palavra', text, Palavra),
                \enviar_ou_cancelar('/palavraChave')
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

  
  
