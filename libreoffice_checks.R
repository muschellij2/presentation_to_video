token_file = "gtoken.rds"
token = NULL
if (file.exists(token_file)) {
  token = readRDS(token_file)
}
have_libreoffice = function() {
  x = try({docxtractr:::lo_assert()}, silent = TRUE)
  !inherits(x, "try-error")
}
temp_pptx_to_ari = function(path, ...) {
  googledrive::drive_auth(token = token)
  cat(file = stderr(), "Drive Authorized\n")
  drive_res = googledrive::drive_upload(path)
  cat(file = stderr(), "Drive Upload\n")
  tbl_df = googledrive::drive_share(drive_res, 
                                    role = "reader", type = "anyone")
  cat(file = stderr(), "Drive shared\n")
  result = gs_to_ari(tbl_df$id[1], ...)
  cat(file = stderr(), "GS to Ari\n")
  googledrive::drive_rm(tbl_df)
  return(result)
}
if (!have_libreoffice() && !is.null(token)) {
  cat(file = stderr(), 
      paste0("Using Google Slides workaround ",
             "- no libreoffice\n"))
  pptx_to_ari = temp_pptx_to_ari
}
