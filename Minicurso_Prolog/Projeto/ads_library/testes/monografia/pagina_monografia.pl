:-module(pagina_monografia,
	[cadastra/1]).

/* html//1, reply_html_page  */
:- use_module(library(http/html_write)).

/* html_requires  */
:- use_module(library(http/html_head)).

:- use_module(bd(curso), []).
:- use_module(bd(instituicao), []).
:- use_module(bd(orientador), []).
:- use_module(bd(funcionario), []).
:- use_module(bd(palavraChave), []).
:- use_module(bd(autor), []).
  
/* Página de cadastro de monografias */

cadastra(_Pedido):-
  reply_html_page(
      boot5rest,
      [ title('Cadastro de Monografias')],
      [ div(class(container),
            [ \html_requires(js('ads_library.js')),
              h1('Cadastrar monografias'),
              \form_monografia
            ]) ]).

form_monografia -->
  html(form([ id('monografia-form'),
              onsubmit("redirecionaResposta( event, '/' )"),
              action('/api/v1/monografias/')],
            [ \método_de_envio('POST'),
              \campo(título, 'Título', text),
              \campo(anoApresentação, 'Ano de Apresentação', text),
              \campo(dataCadastro, 'Data de Cadastro', text),
              \campo(nomeArquivo, 'Nome do Arquivo', text),
              \campo(resumo, 'Resumo', text),
              \campo(numDownload, 'Número de Downloads', text),
              \seleciona(universidade, 'Universidade',text),
              \seleciona(graduação, 'Graduação',text),
              \seleciona(professor, 'Professor',text),
              \seleciona(escritor, 'Escritor',text),
              \seleciona(login, 'Login',text),
              li(a(href('/palavraChave/cadastro'),'Cadastrar Palavras Chaves')),
              \seleciona(palavra, 'Palavra Chave',text),
              \enviar_ou_cancelar('/')
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
		  	option(\opções(Nome))
		  ])	
		]
	)).
	
opções(universidade) -->
  {
    findall( 
      option(value(Universidade)),
      (instituicao:instituicao(_,Universidade,_,_)),
      Valores)
  },
  html(Valores).
  
opções(graduação) -->
  {
    findall( 
      option(value(Graduação)),
      (curso:curso(_,Graduação,_,_)),
      Valores)
  },
  html(Valores).
  
opções(professor) -->
  {
    findall( 
      option(value(Professor)),
      (orientador:orientador(_,Professor,_)),
      Valores)
  },
  html(Valores).

opções(escritor) -->
  {
    findall( 
      option(value(Escritor)),
      (autor:autor(_,Escritor,_)),
      Valores)
  },
  html(Valores).
  
opções(login) -->
  {
    findall( 
      option(value(Login)),
      (funcionario:funcionario(_,Login,_,_)),
      Valores)
  },
  html(Valores).

opções(palavra) -->
  {
    findall( 
      option(value(Palavra)),
      (palavraChave:palavraChave(_,Palavra)),
      Valores)
  },
  html(Valores).
  
método_de_envio(Método) -->
  html(input([type(hidden), name('_método'), value(Método)])).
  
  
  
