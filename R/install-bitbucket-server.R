
install_bitbucket_server <- function(repo, ref = "master", subdir = NULL,
  auth_user = bitbucket_server_user(), password = bitbucket_server_password(),
  host = bitbucket_server_host(),
  dependencies = NA,
  upgrade = c("default", "ask", "always", "never"),
  force = FALSE,
  quiet = FALSE,
  build = TRUE, build_opts = c("--no-resave-data", "--no-manual", "--no-build-vignettes"),
  build_manual = FALSE, build_vignettes = FALSE,
  repos = getOption("repos"),
  type = getOption("pkgType"),
  ...) {

  remotes <- lapply(repo, bitbucket_server_remote, ref = ref,
    subdir = subdir, auth_user = auth_user, password = password, host = host)

  install_remotes(remotes, auth_user = auth_user, password = password, host = host,
    dependencies = dependencies,
    upgrade = upgrade,
    force = force,
    quiet = quiet,
    build = build,
    build_opts = build_opts,
    build_manual = build_manual,
    build_vignettes = build_vignettes,
    repos = repos,
    type = type,
    ...)
}


bitbucket_server_remote <- function(repo, ref = "master", subdir = NULL,
  auth_user = bitbucket_server_user(), password = bitbucket_server_password(),
  sha = NULL, host = bitbucket_server_host(), ...) {

  meta <- parse_git_repo(repo)

  remote("bitbucket_server",
    repo = meta$repo,
    subdir = meta$subdir %||% subdir,
    project = meta$username,
    ref = meta$ref %||% ref,
    sha = sha,
    auth_user = auth_user,
    password = password,
    host = host
  )
}

remote_download.bitbucket_server_remote <- function(x, quiet = FALSE) {
  if (!quiet) {
    message(
      "Downloading Bitbucket Server repo ",
      x$project, "/", x$repo, "@", x$ref
    )
  }

  dest <- tempfile(fileext = paste0(".tar.gz"))
  cat(dest, "\n")

  # Can download archive using REST API
  # https://docs.atlassian.com/bitbucket-server/rest/6.8.1/bitbucket-rest.html#idp190
  # /REST/API/1.0/PROJECTS/{PROJECTKEY}/REPOS/{REPOSITORYSLUG}/ARCHIVE?AT&FILENAME&FORMAT&PATH&PREFIX

  url <- bitbucket_server_uri(host = x$host, x$project, "repos", x$repo,
    paste0("archive?", "at=", x$ref, "&format=tar.gz")
  )

  download(dest, url, basic_auth = basic_auth(x))

}


remote_metadata.bitbucket_server_remote <- function(x, bundle = NULL, source = NULL, sha = NULL) {
  if (!is.null(bundle)) {
    # Might be able to get from archive
    sha <- git_extract_sha1_tar(bundle)
  } else if (is.na(sha)) {
    sha <- NULL
  }

  list(
    RemoteType = "bitbucket_server",
    RemoteHost = x$host,
    RemoteRepo = x$repo,
    RemoteUsername = x$project,
    RemoteRef = x$ref,
    RemoteSha = sha,
    RemoteSubdir = x$subdir
  )
}

remote_package_name.bitbucket_server_remote <- function(remote, ...) {

  bitbucket_server_DESCRIPTION(
    project = remote$project, repo = remote$repo,
    subdir = remote$subdir, ref = remote$ref,
    host = remote$host, auth = basic_auth(remote)
  )$Package

}

remote_sha.bitbucket_server_remote <- function(remote, ...) {
  bitbucket_server_commit(project = remote$project, repo = remote$repo,
    host = remote$host, ref = remote$ref, auth = basic_auth(remote))$id %||% NA_character_
}

bitbucket_server_commit <- function(project, repo, ref = "master",
  host = bitbucket_server_host(), auth = NULL) {

  # Can download archive using REST API
  # https://docs.atlassian.com/bitbucket-server/rest/6.8.1/bitbucket-rest.html#idp207
  # /rest/api/1.0/projects/{projectKey}/repos/{repositorySlug}/commits/{commitId}

  url <- bitbucket_server_uri(host = host, project, "repos", repo,
    "commits", ref)

  tmp <- tempfile()
  download(tmp, url, basic_auth = auth)

  json$parse_file(tmp)
}

format.bitbucket_server_remote <- function(x, ...) {
  "Bitbucket Server"
}

bitbucket_server_DESCRIPTION <- function(project, repo, subdir = NULL,
  ref = "master", host = bitbucket_server_host(), auth = NULL,...) {

  # https://docs.atlassian.com/bitbucket-server/rest/6.8.1/bitbucket-rest.html#idp330
  # /rest/api/1.0/projects/{projectKey}/repos/{repositorySlug}/raw/{path:.*}

  url <- bitbucket_server_uri(host = host, project, "repos", repo, "raw",
    paste0("DESCRIPTION", "?at=", ref))
  tmp <- tempfile()
  download(tmp, url, basic_auth = auth)

  read_dcf(tmp)
}

bitbucket_server_uri <- function(host = bitbucket_server_host(), ...,
  port = NULL, api = "1.0") {
  # https://docs.atlassian.com/bitbucket-server/rest/6.8.1/bitbucket-rest.html
  # v6.8.1:
  #   URI:
  #      http://host:port/context/rest/api-name/api-version/path/to/resource
  #      http://example.com/rest/api/1.0/projects/~johnsmith/repos   (personal)
  #   e.g.
  #      https://stash.atlassian.com/rest/api/1.0/projects/JIRA/repos/jira/commits

  build_url(
    paste0(host, if(!is.null(port)) paste0(":", port)),
    "rest",
    "api",
    api,
    "projects",
    ...
  )
}

bitbucket_server_password <- function(quiet = TRUE) {
  pass <- Sys.getenv("BITBUCKET_SERVER_PASSWORD")
  if (identical(pass, "")) return(NULL)
  if (!quiet) {
    message(
      "Using Bitbucket Server password from envvar BITBUCKET_SERVER_PASSWORD"
    )
  }
  pass
}

bitbucket_server_user <- function(quiet = TRUE) {
  user <- Sys.getenv("BITBUCKET_SERVER_USERNAME")
  if (identical(user, "")) return(NULL)
  if (!quiet)
    message("Using Bitbucket Server username from envvar BITBUCKET_SERVER_USERNAME")
  user
}


bitbucket_server_host <- function() {
  Sys.getenv("BITBUCKET_SERVER_HOST")
}
