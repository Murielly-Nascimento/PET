% html_requires
:- use_module(library(http/html_head)).

% html, html_post, html_root_attribute
:- use_module(library(http/html_write)).

:- multifile
        user:body//2.

user:body(boot5rest, Corpo) -->
       html(body([ \html_post(head,
                              [ meta([name(viewport),
                                      content([ 'width=device-width',
                                                'initial-scale=1',
                                                'shrink-to-fit=no'
                                              ])])]),
                   \html_root_attribute(lang,'pt-br'),
                   \html_requires(css('bootstrap.min.css')),
                   \html_requires(js('rest.js')),

                   Corpo,

                   script([ src('/js/bootstrap.bundle.min.js'),
                            type('text/javascript')], [])
                 ])).
