/* html//1, reply_html_page  */
:- use_module(library(http/html_write)).
/* html_requires  */
:- use_module(library(http/html_head)).

:- ensure_loaded(gabarito(bootstrap5)).
:- ensure_loaded(frontend(icones)).


navegação(Id) -->
    html(nav([ class='navbar navbar-expand-lg navbar-light bg-light fixed-top'],
             [ div(class('container-fluid'),
                   [ a([ class='navbar-brand', href= #],
                       [ \livro, ' ADS Library']),
                     button([ class('navbar-toggler'),
                              type(button),
                              'data-bs-toggle'=collapse,
                              'data-bs-target'='#~w'-[Id],
                              'aria-controls'=Id,
                              'aria-expanded'=false,
                              'aria-label'='Toggle navigation'],
                            [span([class='navbar-toggler-icon'], [])]),
                     div([ class(['collapse', 'navbar-collapse']),
                           id=Id ],
                         [ ul([class='navbar-nav ms-auto mb-2 mb-lg-0'],
                              [ \nav_item('/', 'Início'),
                                \nav_item('/instituicao', 'Instituição'),
                                \nav_item('/autor', 'Autor'),
                                \nav_item('/orientador', 'Orientador'),
                                \nav_item('/curso', 'Curso'),
                                \nav_item('/palavraChave', 'Palavras Chaves')
                              ]) ])
                   ])
             ]) ).


nav_item(Link, Rótulo) -->
    html(li([ class='nav-item'],
            [ a([class='nav-link m-1 menu-item', href=Link], Rótulo) ])).
            
retorna_home --> 
	html(div(class(row), a([ class(['btn', 'btn-primary']),href('/monografia')],'Voltar para o início') )).
	
navegação_inicial(Id) -->
    html(nav([ class='navbar navbar-expand-lg navbar-light bg-light fixed-top'],
             [ div(class('container-fluid'),
                   [ a([ class='navbar-brand', href= #],
                       [ \livro, ' ADS - Library']),
                     button([ class('navbar-toggler'),
                              type(button),
                              'data-bs-toggle'=collapse,
                              'data-bs-target'='#~w'-[Id],
                              'aria-controls'=Id,
                              'aria-expanded'=false,
                              'aria-label'='Toggle navigation'],
                            [span([class='navbar-toggler-icon'], [])]),
                     div([ class(['collapse', 'navbar-collapse']),
                           id=Id ],
                         [ ul([class='navbar-nav ms-auto mb-2 mb-lg-0'],
                              [ \nav_item('/', 'Início'),
                                \nav_item('/termos', 'Termos'),
                                \nav_item('/monografia', 'Monografias')
                              ]) ])
                   ])
             ]) ).
             
             
             
