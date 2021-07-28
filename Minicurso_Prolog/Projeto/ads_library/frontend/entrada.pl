/* html//1, reply_html_page  */
:- use_module(library(http/html_write)).
/* html_requires  */
:- use_module(library(http/html_head)).

:- ensure_loaded(gabarito(boot5rest)).

entrada(_):-
    reply_html_page(
        boot5rest,
        [ title('ADS Library')],
        [ div(class(container),
              [ \html_requires(css('custom.css')),
                \html_requires(js('ads_library.js')),
                \navegação_inicial('navegacao-inicial'),
                \propaganda
              ]) ]).
    
propaganda -->
    html(div([ class='container-fluid' ],
             div([ id='propaganda',
                   class='py-5 text-center block block-1'],
                 [ div(class('py-5'), \livro),
                   h1(class('py-5'), 'ADS - Library'),
                   h2(class('py-5'), 'Uma biblioteca para suas Monografias!'),
                   p(class(lead),
                     [ 'O ADS Library é uma biblioteca digital de Trabalhos de Graduação da Faculdade de Tecnologia de Sorocaba.']),
                   p(class(lead),
                     [ 'Desenvolvido em um projeto de Iniciação Científica,',
                       'seu objetivo é disponibilizar as informações e conteúdo dos,',
                       ' Trabalho de Graduação de forma simples e organizada.'
                     ]),
                   p(class(lead),
                     ['Aqui você pode pesquisar as monografias de alunos da FATEC que já estão cadastradas, saber seus detalhes e fazer download de sua versão original.']),
                   p(class(lead), 'Disponibilize aqui seu trabalho! (Procure a biblioteca de sua unidade de ensino para mais informações).')]))).
	
   




