/**************************************************************
 *                                                            *
 *      Localização dos diretórios no sistema de arquivos     *
 *                                                            *
 **************************************************************/

:- multifile user:file_search_path/2.

% file_search_path(Apelido, Caminho)
%     Apelido é como será chamado um Caminho absoluto ou
%     relativo no sistema de arquivos

% Diretório principal do servidor: sempre coloque o caminho completo
% Por exemplo: se o projeto bookmarks está em '/home/pedro/bookmarks'
%          então o caminho até o diretório do projeto é '/home/pedro'

user:file_search_path(dir_base, '/home/mury/Área de Trabalho/UFU/PET/Minicurso_Prolog/Projeto').

% Diretório do projeto
user:file_search_path(projeto, dir_base(ads_library)).

% Diretório de configuração
user:file_search_path(config, projeto(config)).

%% Front-end
user:file_search_path(frontend, projeto(frontend)).

%% Recursos estáticos
user:file_search_path(dir_css, frontend(css)).
user:file_search_path(dir_js,  frontend(js)).
user:file_search_path(dir_img, frontend(img)).
user:file_search_path(dir_webfonts, frontend(webfonts)).

% Gabaritos para estilização
user:file_search_path(gabarito, frontend(gabaritos)).


%% Backend
user:file_search_path(backend, projeto(backend)).


% Banco de dados
user:file_search_path(bd, backend(bd)).
user:file_search_path(bd_tabs, bd(tabelas)).

% API REST
user:file_search_path(api,  backend(api)).
user:file_search_path(api1, api(v1)).


% Middle-end
user:file_search_path(middle_end, projeto(middle_end)).
