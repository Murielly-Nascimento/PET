/* Esse módulo assume que rest.js tenha sido corregado.
 */

/**
 *  Função auxiliar que imprime para o console do navegador
 *  a resposta recebida do servidor e depois redireciona
 *  para a rota dada.
 *
 * @param {Object} resp - corpo da resposta devolvida pelo servidor
 * @param {URL}    rota - a rota a ser seguida
 */
function redireciona(resposta, rota){

  console.log(resposta);
  window.location.href = rota; /* redireciona para a rota dada */

}



function redirecionaResposta(evento, rotaRedireção) {
  enviarDados(evento, resposta => redireciona(resposta, rotaRedireção));
}


function apagar(evento, rotaRedireção) {
  evento.preventDefault();
  const elemento = evento.currentTarget;
  const url = elemento.href;

  console.log('delete url = ', url);
  remover( url,  resposta => redireciona(resposta, rotaRedireção));
}
