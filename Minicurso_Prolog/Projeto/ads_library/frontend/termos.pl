/* html//1, reply_html_page  */
:- use_module(library(http/html_write)).
/* html_requires  */
:- use_module(library(http/html_head)).

:- ensure_loaded(gabarito(boot5rest)).

termos(_):-
    reply_html_page(
        boot5rest,
        [ title('Termos de Uso')],
        [ div(class(container),
              [ \html_requires(css('entrada.css')),
                \html_requires(css('custom.css')),
                \html_requires(js('ads_library.js')),
                \navegação_inicial('navegação-inicial'),
                \corpo
              ]) ]).
                
corpo -->
    html(div([ class='container-fluid' ],
             div([ id='termos',
                   class='py-5 text-center block block-1'],
                 [ div(class('py-5'), \livro),
                   h1(class('py-5'), 'Termos de Uso'),
                   p(class(lead),
                     [ 'O acesso à Biblioteca Digital é público e gratuito']),
                   p(class(lead),
                     [ 'Todo o conteúdo disponível na Biblioteca Digital Ads Library destina-se para uso pessoal ou científico,',
                       'sendo proibida a sua comercialização.'
                     ]),
                   p(class(lead), 'Ao utilizar este site, você concorda com os termos de uso')]))).




