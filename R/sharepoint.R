#' @title An R6 Class to interface CDC Sharepoint sites
#' @import R6
#' @import httr
#' @importFrom curl new_handle handle_setform curl_fetch_memory
#' @importFrom jsonlite fromJSON
#' @importFrom data.table fread
#' @importFrom readxl read_xlsx read_xls
#' @importFrom stringr str_interp
#' @export
spoConnection <- R6Class(
  classname = "spoConnection",
  public = list(
    #' @description
    #' Create a new Sharepoint Site connection
    #'
    #' @param tenant (character) An Azure tenant string
    #' @param client_id (character) A Client ID for the App Registration
    #' @param client_secret (character) A Client Secret for the App Registration
    #' @param teams_name (character) Either a MS Teams name or Sharepoint site name
    #'
    #' @return A new `spoConnection` object
    initialize = function(tenant, client_id, client_secret, teams_name) {
      private$acquire_token(tenant, client_id, client_secret)
      private$configure_url(teams_name)
      private$site_name <- teams_name
    },
    #' @description
    #' Read in a file from Sharepoint Site
    #' @param relative_url File path relative to the "Documents" folder
    #' @param ... other named parameters passed to a read in function
    #'
    #' @return If Excel or CSV file, a data frame. All other types will return error
    read_file = function(relative_url, ...) {
      URL <- URLencode(
        sprintf(
          "%s/GetFileByServerRelativeUrl('%s')/$value",
          private$url_root_path,
          paste0(private$docs_root_path, URLencode(relative_url))
        )
      )

      tmp_file <- tempfile(fileext = paste0(".", tools::file_ext(relative_url)))
      read_fn <- get_read_fn(relative_url)


      req <- GET(
        URL,
        add_headers(.headers = c("Authorization" = private$access_token)),
        write_disk(tmp_file, overwrite = TRUE)
      )

      out <- read_fn(tmp_file, ...)

      return(out)
    },

    #' @description
    #' List files in a Teams / Sharepoint Folder
    #'
    #' @param relative_url (character) a folder URL relative to the "Shared Documents" folder
    #'
    #' @return A character vector of files present in the folder
    list_files = function(relative_url) {
      dir_path <- paste0(private$docs_root_path, URLencode(relative_url))

      URL <- URLencode(
        sprintf(
          "%s/GetFolderByServerRelativeUrl('%s')/Files",
          private$url_root_path,
          dir_path
        )
      )

      req <- GET(
        URL,
        add_headers(
          .headers = c(
            "Authorization" = private$access_token,
            "Accept" = "application/json"
          )
        )
      )

      res <- fromJSON(rawToChar(req$content))
      out <- res[["value"]][["ServerRelativeUrl"]]

      return(sub(URLdecode(dir_path), "", out))
    },
    #' @description
    #' Pull a Sharepoint List from an MS Teams/Sharepoint Site
    #'
    #' @param list_name (character) Name of the Sharepoint list
    #'
    #' @return Possibly a data.frame of the sharepoint list
    get_list = function(list_name) {
      URL <- URLencode(
        sprintf(
          "%s/lists/getbytitle('%s')/Items",
          private$url_root_path,
          list_name
        )
      )

      req <- GET(URL, add_headers(.headers = c("Authorization" = private$access_token)))

      res <- content(req, as = "parsed")

      return(res)
    },
    #' @description
    #' Write a local file to a MS Teams/Sharepoint Site
    #'
    #' @param x (character) Local path to the file to write
    #' @param relative_url (character) Destination path where the file should be written to
    #'
    #' @return URL path linking to the newly created file
    write_file = function(x, relative_url) {
      dir_path <- paste0(private$docs_root_path, URLencode(dirname(relative_url)))

      URL <- URLencode(
        sprintf(
          "%s/GetFolderByServerRelativeUrl('%s')/Files/add(url='%s',overwrite=true)",
          private$url_root_path,
          dir_path,
          URLencode(basename(relative_url))
        )
      )


      res <- POST(
        URL,
        add_headers(
          .headers = c(
            "Authorization" = private$access_token,
            "X-RequestDigest" = private$digest()
          )
        ),
        body = upload_file(x, type = "application/octet-stream")
      )

      httr::stop_for_status(res)

      # Return URL
      return(content(res)$LinkingUrl)
    }
  ),
  private = list(
    # Expect a string
    access_token = "",
    url_root_path = "",
    docs_root_path = "",
    site_name = "",
    configure_url = function(name) {
      private$url_root_path <- sprintf("https://cdc.sharepoint.com/teams/%s/_api/web", name)
      private$docs_root_path <- sprintf("/teams/%s/Shared%%20Documents/", name)
      # Check that URL is valid
      HEAD(
        private$url_root_path,
        add_headers(.headers = c("Authorization" = private$access_token))
      )
    },

    #
    # Get Sharepoint's "Request Digest" security feature.  This method
    # sends a request to get the request digest for a given sharepoint
    # site, which needs to be used in subsequent \code{POST} requests.
    # This was inspired by the `spud` package.
    digest = function() {
      url <- sprintf("https://cdc.sharepoint.com/teams/%s/_api/contextinfo", private$site_name)
      r <- POST(
        url,
        add_headers(.headers = c(
          "Authorization" = private$access_token,
          "Accept" = "application/json"
        ))
      )
      httr::stop_for_status(r)
      dat <- content(r, as = "parsed")
      return(dat$FormDigestValue)
    },
    # Acquire an access token to use the Sharepoint REST API v1
    acquire_token = function(tenant, clientId, clientSecret) {
      h <- curl::new_handle()
      curl::handle_setform(h,
        "grant_type" = "client_credentials",
        "client_id" = str_interp("${clientId}@${tenant}"),
        "client_secret" = clientSecret,
        "resource" = str_interp("00000003-0000-0ff1-ce00-000000000000/cdc.sharepoint.com@${tenant}")
      )

      path <- stringr::str_interp("https://accounts.accesscontrol.windows.net/${tenant}/tokens/OAuth/2") # use V1 token endpoint.  V2 token endpoint does not work
      req <- curl::curl_fetch_memory(path, handle = h)
      res <- jsonlite::fromJSON(rawToChar(req$content))

      private$access_token <- paste("Bearer", res$access_token)
    }
  )
)
