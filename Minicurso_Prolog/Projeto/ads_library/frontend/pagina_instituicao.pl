:-module(pagina_instituicao,
	[lista/1,
	cadastra/1,
	editar/2]).

/* html//1, reply_html_page  */
:- use_module(library(http/html_write)).

/* html_requires  */
:- use_module(library(http/html_head)).

:- use_module(bd(instituicao), []).
  
lista(_):-
    reply_html_page(
        boot5rest,
        [ title('Instituições')],
        [ div(class(container),
              [ \html_requires(css('entrada.css')),
              	 \html_requires(css('custom.css')),
                \html_requires(js('ads_library.js')),
                \navegação('menu-topo'),
                \tabela_de_instituicoes,
                \retorna_home
              ]) ]).

tabela_de_instituicoes -->
    html(div(class('container-fluid py-3'),
             [ \cabeça_da_tabela('Instituições', '/instituicao/cadastro'),
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
                    th([scope(col)], 'Sigla'),
                    th([scope(col)], 'Cidade'),
                    th([scope(col)], 'Ações')
                  ]))).



corpo_tabela -->
    {
        findall( tr([th(scope(row), Id), td(Nome), td(Sigla), td(Cidade), td(Ações)]),
                 linha(Id, Nome, Sigla, Cidade, Ações),
                 Linhas )
    },
    html(Linhas).


linha(Id, Nome, Sigla, Cidade, Ações):-
    instituicao:instituicao(Id, Nome, Sigla, Cidade),
    ações(Id,Ações).


ações(Id, Campo):-
    Campo = [ a([ class('text-success'), title('Alterar'),
                  href('/instituicao/editar/~w' - Id),
                  'data-toggle'(tooltip)],
                [ \lápis ]),
              a([ class('text-danger ms-1'), title('Excluir'),
                  href('/api/v1/instituicoes/~w' - Id),
                  onClick("apagar( event, '/instituicao' )"),
                  'data-toggle'(tooltip)],
                [ \lixeira ])
            ].
  
/* Página de cadastro da instituição */

cadastra(_Pedido):-
  reply_html_page(
      boot5rest,
      [ title('Cadastro da Instituição')],
      [ div(class(container),
            [ \html_requires(js('ads_library.js')),
              h1('Cadastrar instituições'),
              \form_instituicao
            ]) ]).

form_instituicao -->
  html(form([ id('instituicao-form'),
              onsubmit("redirecionaResposta( event, '/instituicao' )"),
              action('/api/v1/instituicoes/') ],
            [ \método_de_envio('POST'),
              \campo(nome, 'Nome', text),
              \campo(sigla, 'Sigla', text),
              \campo(cidade, 'Cidade', text),
              \enviar_ou_cancelar('/instituicao')
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
           
/* Página para edição (alteração) de uma instituição  */

editar(AtomId, _Pedido):-
    atom_number(AtomId, Id),
    ( instituicao:instituicao(Id, Nome, Sigla, Cidade)
    ->
    reply_html_page(
        boot5rest,
        [ title('Editar Instituições')],
        [ div(class(container),
              [ \html_requires(js('ads_library.js')),
                h1('Editar Instituições'),
                \form_instituicao(Id, Nome, Sigla, Cidade)
              ]) ])
    ; throw(http_reply(not_found(Id)))
    ).


form_instituicao(Id, Nome, Sigla, Cidade) -->
    html(form([ id('instituicao-form'),
                onsubmit("redirecionaResposta( event, '/instituicao' )"),
                action('/api/v1/instituicoes/~w' - Id) ],
              [ \método_de_envio('PUT'),
                \campo_não_editável(id, 'Id', text, Id),
                \campo(nome, 'Nome', text, Nome),
                \campo(sigla,    'Sigla',    text,  Sigla),
                \campo(cidade,    'Cidade',    text,  Cidade),
                \enviar_ou_cancelar('/instituicao')
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

  
  
