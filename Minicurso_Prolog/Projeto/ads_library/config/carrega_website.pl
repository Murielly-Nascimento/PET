% Configuração do servidor


% Carrega o servidor e as rotas

:- load_files([ servidor,
                rotas
              ],
              [ silent(true),
                if(not_loaded) ]).

% Inicializa o servidor para ouvir a porta 8000
:- initialization( servidor(8000) ).


% Carrega o frontend

:- load_files([ gabarito(bootstrap5),  % gabarito usando Bootstrap 5
                gabarito(boot5rest),   % Bootstrap 5 com API REST
                frontend(entrada),
                frontend(termos),
                frontend(menu_topo),
                frontend(icones)
              ],
              [ silent(true),
                if(not_loaded) ]).


% Carrega o backend

:- load_files([ api1(cursos), % API REST
                api1(instituicoes), 
                api1(palavrasChaves),
                api1(orientadores),
                api1(monografias),
                api1(autores)
              ],
              [ silent(true),
                if(not_loaded) ]).
