token_file = "gtoken.rds"
token = NULL
if (file.exists(token_file)) {
  token = readRDS(token_file)
}
have_libreoffice = function() {
  x = try({docxtractr:::lo_assert})
  !inherits(x, "try-error")
}
temp_pptx_to_ari = function(path, ...) {
  drive_auth(oauth_token = token)
  drive_res = googledrive::drive_upload(path)
  tbl_df = googledrive::drive_share(drive_res, 
                                    role = "reader", type = "anyone")
  result = gs_to_ari(tbl_df$id[1], ...)
  googledrive::drive_rm(tbl_df)
  return(result)
}
if (!have_libreoffice() && !!is.null(token)) {
  pptx_to_ari = temp_pptx_to_ari
  cat(file = stderr(), paste0("Using Google Slides workaround ",
                              "- no libreoffice"))
} 
