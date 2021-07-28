:-module(pagina_monografia,
	[lista/1,
	cadastra/1,
	editar/2]).

/* html//1, reply_html_page  */
:- use_module(library(http/html_write)).

/* html_requires  */
:- use_module(library(http/html_head)).

:- use_module(bd(curso), []).
:- use_module(bd(instituicao), []).
:- use_module(bd(autor), []).
:- use_module(bd(orientador), []).
:- use_module(bd(palavraChave), []).
:- use_module(bd(monografia), []).

lista(_):-
    reply_html_page(
        boot5rest,
        [ title('Monografias')],
        [ div(class(container),
              [ \html_requires(css('entrada.css')),
                \html_requires(css('custom.css')),
                \html_requires(js('ads_library.js')),
                \navegação('menu-topo'),
                \tabela_de_monografias
              ]) ]).

tabela_de_monografias -->
    html(div(class('container-fluid py-3'),
             [ \cabeça_da_tabela('Monografias', '/monografia/cadastro'),
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
                    th([scope(col)], 'Titulo'),
                    th([scope(col)], 'Resumo'),
                    th([scope(col)], 'Ações')
                  ]))).



corpo_tabela -->
    {
        findall( tr([th(scope(row), Id), td(Titulo), td(Resumo), td(Ações)]),
                 linha(Id, Titulo, Resumo, Ações),
                 Linhas )
    },
    html(Linhas).


linha(Id, Titulo, Resumo, Ações):-
    monografia:monografia(Id, Titulo,_,_,_,Resumo,_,_,_,_,_,_),
    ações(Id,Ações).


ações(Id, Campo):-
    Campo = [ a([ class('text-success'), title('Alterar'),
                  href('/monografia/editar/~w' - Id),
                  'data-toggle'(tooltip)],
                [ \lápis ]),
              a([ class('text-danger ms-1'), title('Excluir'),
                  href('/api/v1/monografias/~w' - Id),
                  onClick("apagar( event, '/monografia' )"),
                  'data-toggle'(tooltip)],
                [ \lixeira ])
            ].
  
/* Página de cadastro de monografias */

cadastra(_Pedido):-
  reply_html_page(
      boot5rest,
      [ title('Cadastro de Monografia')],
      [ div(class(container),
            [ \html_requires(js('ads_library.js')),
              h1('Cadastrar monografias'),
              \form_monografia
            ]) ]).

form_monografia -->
  html(form([ id('monografia-form'),
              onsubmit("redirecionaResposta( event, '/monografia' )"),
              action('/api/v1/monografias/') ],
            [ \método_de_envio('POST'),
              \campo(titulo, 'Titulo', text),
              \campo(ano, 'Ano', text),
              \campo(data, 'Data', text),
              \campo(nome, 'Nome', text),
              \campo(resumo, 'Resumo', text),
              \campo(download, 'Download', text),
              \seleciona(instituicao, 'Instituicao',text),
              \seleciona(curso, 'Curso',text),
              \seleciona(autor, 'Autor',text),
              \seleciona(orientador, 'Orientador',text),
              \seleciona(palavra, 'Palavra',text),
              \enviar_ou_cancelar('/monografia')
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
		  	option(selected,'Selecione uma Opção'),
		  	option(\opcoes(Nome))
		  ])	
		]
	)).

opcoes(instituicao) -->
  {
    findall( 
      option(value(Universidade)),
      (instituicao:instituicao(_,Universidade,_,_)),
      Valores)
  },
  html(Valores).
  
opcoes(curso) -->
  {
    findall( 
      option(value(Curso)),
      (curso:curso(_,Curso,_,_)),
      Valores)
  },
  html(Valores).

opcoes(autor) -->
  {
    findall( 
      option(value(Autor)),
      (autor:autor(_,Autor,_)),
      Valores)
  },
  html(Valores).
  
opcoes(orientador) -->
  {
    findall( 
      option(value(Orientador)),
      (orientador:orientador(_,Orientador,_)),
      Valores)
  },
  html(Valores).
  
opcoes(palavra) -->
  {
    findall( 
      option(value(Palavra)),
      (palavraChave:palavraChave(_,Palavra)),
      Valores)
  },
  html(Valores).
           
/* Página para edição (alteração) de um monografia  */

editar(AtomId, _Pedido):-
    atom_number(AtomId, Id),
    ( monografia:monografia(Id, Titulo, Ano, Data, Nome, Resumo, Download, Instituicao, Curso, Autor, Orientador, Palavra)
    ->
    reply_html_page(
        boot5rest,
        [ title('Editar Monografias')],
        [ div(class(container),
              [ \html_requires(js('ads_library.js')),
                h1('Editar Monografias'),
                \form_monografia(Id, Titulo, Ano, Data, Nome, Resumo, Download, Instituicao, Curso, Autor, Orientador, Palavra)
              ]) ])
    ; throw(http_reply(not_found(Id)))
    ).


form_monografia(Id, Titulo, Ano, Data, Nome, Resumo, Download, Instituicao, Curso, Autor, Orientador, Palavra) -->
    html(form([ id('monografia-form'),
                onsubmit("redirecionaResposta( event, '/monografia' )"),
                action('/api/v1/monografias/~w' - Id) ],
              [ \método_de_envio('PUT'),
                \campo_não_editável(id, 'Id', text, Id),
                \campo(titulo, 'Titulo', text, Titulo),
                \campo(ano, 'Ano', text, Ano),
                \campo(data, 'Data', text, Data),
                \campo(nome, 'Nome', text, Nome),
                \campo(resumo, 'Resumo', text, Resumo),
                \campo(download, 'Download', text, Download),
                \seleciona(instituicao, 'Instituicao',text, Instituicao),
                \seleciona(curso, 'Curso',text, Curso),
                \seleciona(autor, 'Autor',text, Autor),
                \seleciona(orientador, 'Orientador',text, Orientador),
                \seleciona(palavra, 'Palavra',text, Palavra),
                \enviar_ou_cancelar('/monografia')
              ])).
              
seleciona(Nome, Rótulo, Tipo, Valor) -->
	html(div(class('mb-3'),
		[ label([ for(Nome), class('form-label') ], Rótulo),
		  select([type(Tipo), class('form-control'),id(Nome), name(Nome), value(Valor)],[
		  	option(selected,'Selecione uma Opção'),
		  	option(\opcoes(Nome))
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

  
  
