config.options <- function() list(github.api.url="http://127.0.0.1:7990/rest/snippets/1.0/snippets", github.base.url="http://127.0.0.1:7990/", github.client.id=TRUE, github.client.secret=TRUE)

create.gist.context <- function(username, token, github.api.url, github.client.id, github.client.secret, github.base.url, ...) {
  if (is.character(token) && !isTRUE(nzchar(token))) token <- NULL ## github requires token to be NULL if not used
  ctx <- Rstash::create.github.context(api_url=github.api.url, client_id=github.client.id, client_secret=github.client.secret, access_token=token)
  ctx$github.base.url=github.base.url
  str(ctx)
  ctx
}

auth.url.snippetcontext <- function(redirect, ctx) {
  state <- list(nonce=rnorm(1), redirect=as.vector(redirect))
  res <- verify.token(ctx$token, ctx$client_id, ctx$client_secret, ctx$api_url)
  if(res$status == "error") {
    vals <- authenticate.sni(ctx$client_id, ctx$client_secret, ctx$github.base.url)
    paste0(ctx$github.base.url,
           "plugins/servlet/oauth/authorize?oauth_token=", vals["oauth_token"],
           "&state=",URLencode(toJSON(state), TRUE),
           "&scope=gist,user:email")
  } else {
    paste0("/edit.html")
  }
}

access.token.snippetcontext <- function(query, ctx) {
  result <- Rstash::POST(paste(rcloud.config("github.base.url"), "login/oauth/access_token", sep=''),
                 config=accept_json(),
                 body=list(
                   client_id=ctx$client_id,
                   client_secret=ctx$client_secret,
                   code=query["code"]))
  l <- list(token=Rstash::content(result)$access_token)
  l
}

context.info.snippetcontext <- function(ctx) list(username=ctx$user$login)

get.gist.snippetcontext <- Rstash::get.gist

fork.gist.snippetcontext <- Rstash::fork.gist

modify.gist.snippetcontext <- Rstash::modify.gist

create.gist.snippetcontext <- Rstash::create.gist

delete.gist.snippetcontext <- Rstash::delete.gist

modify.gist.snippetcontext <- Rstash::modify.gist

create.gist.comment.snippetcontext <- Rstash::create.gist.comment

get.gist.comments.snippetcontext <- Rstash::get.gist.comments
