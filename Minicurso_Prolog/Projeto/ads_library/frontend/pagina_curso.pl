:-module(pagina_curso,
	[lista/1,
	cadastra/1,
	editar/2]).

/* html//1, reply_html_page  */
:- use_module(library(http/html_write)).

/* html_requires  */
:- use_module(library(http/html_head)).

:- use_module(bd(curso), []).
:- use_module(bd(instituicao), []).
  
lista(_):-
    reply_html_page(
        boot5rest,
        [ title('Cursos')],
        [ div(class(container),
              [ \html_requires(css('entrada.css')),
              	 \html_requires(css('custom.css')),
                \html_requires(js('ads_library.js')),
                \navegação('menu-topo'),
                \tabela_de_cursos,
                \retorna_home
              ]) ]).

tabela_de_cursos -->
    html(div(class('container-fluid py-3'),
             [ \cabeça_da_tabela('Cursos', '/curso/cadastro'),
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
                    th([scope(col)], 'Universidade'),
                    th([scope(col)], 'Ações')
                  ]))).



corpo_tabela -->
    {
        findall( tr([th(scope(row), Id), td(Nome), td(Sigla), td(Universidade), td(Ações)]),
                 linha(Id, Nome, Sigla, Universidade, Ações),
                 Linhas )
    },
    html(Linhas).


linha(Id, Nome, Sigla, Universidade, Ações):-
    curso:curso(Id, Nome, Sigla, Instituição),
    instituicao:instituicao(Instituição,Universidade,_,_),
    ações(Id,Ações).


ações(Id, Campo):-
    Campo = [ a([ class('text-success'), title('Alterar'),
                  href('/curso/editar/~w' - Id),
                  'data-toggle'(tooltip)],
                [ \lápis ]),
              a([ class('text-danger ms-1'), title('Excluir'),
                  href('/api/v1/cursos/~w' - Id),
                  onClick("apagar( event, '/curso' )"),
                  'data-toggle'(tooltip)],
                [ \lixeira ])
            ].
  
/* Página de cadastro de cursos */

cadastra(_Pedido):-
  reply_html_page(
      boot5rest,
      [ title('Cadastro de Curso')],
      [ div(class(container),
            [ \html_requires(js('ads_library.js')),
              h1('Cadastrar cursos'),
              \form_curso
            ]) ]).

form_curso -->
  html(form([ id('curso-form'),
              onsubmit("redirecionaResposta( event, '/curso' )"),
              action('/api/v1/cursos/') ],
            [ \método_de_envio('POST'),
              \campo(nome, 'Nome', text),
              \campo(sigla, 'Sigla', text),
              \seleciona(instituicao, 'Instituicao',text),
              \enviar_ou_cancelar('/curso')
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
           
seleciona(Nome, Rótulo, Tipo) -->
	html(div(class('mb-3'),
		[ label([ for(Nome), class('form-label') ], Rótulo),
		  select([type(Tipo), class('form-control'), id(Nome), name(Nome) ],[
		  	option(selected,'Selecione uma Instituição'),
		  	option(\universidade)
		  ])	
		]
	)).

universidade -->
  {
    findall( 
      option(value(Universidade)),
      (instituicao:instituicao(_,Universidade,_,_)),
      Valores)
  },
  html(Valores).
           
/* Página para edição (alteração) de um curso  */

editar(AtomId, _Pedido):-
    atom_number(AtomId, Id),
    ( curso:curso(Id, Nome, Sigla, Instituição)
    ->
    reply_html_page(
        boot5rest,
        [ title('Editar Cursos')],
        [ div(class(container),
              [ \html_requires(js('ads_library.js')),
                h1('Editar Cursos'),
                \form_curso(Id, Nome, Sigla, Instituição)
              ]) ])
    ; throw(http_reply(not_found(Id)))
    ).


form_curso(Id, Nome, Sigla, Instituição) -->
    html(form([ id('curso-form'),
                onsubmit("redirecionaResposta( event, '/curso' )"),
                action('/api/v1/cursos/~w' - Id) ],
              [ \método_de_envio('PUT'),
                \campo_não_editável(id, 'Id', text, Id),
                \campo(nome, 'Nome', text, Nome),
                \campo(sigla,    'Sigla',    text,  Sigla),
                \seleciona(instituicao,    'Instituição',    text,  Instituição),
                \enviar_ou_cancelar('/curso')
              ])).
              
seleciona(Nome, Rótulo, Tipo, Valor) -->
	html(div(class('mb-3'),
		[ label([ for(Nome), class('form-label') ], Rótulo),
		  select([type(Tipo), class('form-control'),id(Nome), name(Nome), value(Valor)],[
		  	option(selected,'Selecione uma Instituição'),
		  	option(\universidade)
		  ])	
		]
	)).


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

  
  
