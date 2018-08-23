proj <- new.env(parent = emptyenv())

proj_get_ <- function() proj$cur

proj_active <- function() !is.null(proj_get_())

proj_set_ <- function(path) {
   old <- proj$cur
   proj$cur <- path
   invisible(old)
}

proj_set <- function(path = ".", force = FALSE) {
   if (is.null(path) || force) {
      path <- proj_path_prep(path)
      proj_string <- if (is.null(path)) code("NULL") else value(path)
      done("Setting active project to {proj_string}")
      return(proj_set_(path))
   }
}

proj_path_prep <- function(path) {
   if (is.null(path)) return(path)
   fs::path_real(path)
}

proj_get <- function() {
   # Called for first time so try working directory
   if (!proj_active()) {
      proj_set(".")
   }
}

proj_get()


is_package <- function(base_path = proj_get()) {
   res <- tryCatch(
      rprojroot::find_package_root_file(path = base_path),
      error = function(e) NULL
   )
   !is.null(res)
}

project_data <- function(base_path = proj_get()) {
   if (is_package(base_path)) {
      project_data(base_path)$Package
   } else {
      project_data(base_path)$Project
   }
}

pkg <- project_data()
repo_name <- pkg$Project %||% gsub("\n", " ", pkg$Package)
repo_desc <- pkg$Title %||% ""

todo("Check title and description")
code_block(
   paste0("Name:        ", repo_name),
   paste0("Description: ", repo_desc),
   copy = FALSE
)
if (yesno("Are title and description ok?")) {
   return(invisible())
}

done("Creating GitHub repository")

if (is.null(organisation)) {
   create <- gh::gh("POST /user/repos",
                    name = repo_name,
                    description = repo_desc,
                    private = private,
                    .api_url = host,
                    .token = auth_token
   )
} else {
   create <- gh::gh("POST /orgs/:org/repos",
                    org = organisation,
                    name = repo_name,
                    description = repo_desc,
                    private = private,
                    .api_url = host,
                    .token = auth_token
   )
}

done("Adding GitHub remote")
r <- git2r::repository(proj_get())
protocol <- match.arg(protocol)
origin_url <- switch(protocol,
                     https = create$clone_url,
                     ssh = create$ssh_url
)
git2r::remote_add(r, "origin", origin_url)

if (is_package()) {
   done("Adding GitHub links to DESCRIPTION")
   use_github_links(auth_token = auth_token, host = host)
   if (git_uncommitted()) {
      git2r::add(r, "DESCRIPTION")
      git2r::commit(r, "Add GitHub links to DESCRIPTION")
   }
}

done("Pushing to GitHub and setting remote tracking branch")
if (protocol == "ssh") {
   ## [1] push via ssh required for success setting remote tracking branch
   ## [2] to get passphrase from ssh-agent, you must use NULL credentials
   git2r::push(r, "origin", "refs/heads/master", credentials = credentials)
} else { ## protocol == "https"
   ## in https case, when GITHUB_PAT is passed as password,
   ## the username is immaterial, but git2r doesn't know that
   cred <- git2r::cred_user_pass("EMAIL", auth_token)
   git2r::push(r, "origin", "refs/heads/master", credentials = cred)
}
git2r::branch_set_upstream(git2r::head(r), "origin/master")

view_url(create$html_url)

invisible(NULL)
