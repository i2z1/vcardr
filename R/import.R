#' Import data fram vcf file
#'
#' @param filepath path to VCF file
#'
#' @return dataframe
#' @export
#' @import tibble purrr stringr
#'
#' @examples
#'\dontrun{
#'contacts_df <- import_vcf("dir/mycontacts.vcf")
#'}
import_vcf <- function(filepath) {
  lns <- readLines(con = file(filepath))

  lns_tbl <- tibble::tibble(lns)
  start_no <- which(stringr::str_detect(lns_tbl$lns, "BEGIN:VCARD"))
  ends_no <- which(stringr::str_detect(lns_tbl$lns, "END:VCARD"))
  tbl_list <- purrr::map2(start_no, ends_no, ~ lns_tbl[.x:.y, ])

  res_all <- purrr::map_dfr(tbl_list, vcard_item2df)

  return(res_all)
}

#' Parses lines of VCF for one contact and makes dataframe for it
#'
#' @param v_item per contact group of VCF lines encapsulated in dataframe, each line of VCF in row
#'
#' @return dataframe
#' @import stringr tidyr dplyr purrr
#'
vcard_item2df <- function(v_item) {
  vdf <- v_item %>%
    .[[1]] %>%
    tibble(lns = .)

  v_colon <- vdf %>%
    filter(stringr::str_detect(lns, "^FN:|^N:|^UID:|^VERSION:|^BDAY:")) %>%
    tidyr::separate_wider_delim(., lns, delim = ":", names = c("key", "value")) %>%
    tidyr::pivot_wider(names_from = key) %>%
    tidyr::separate(., col = N, into = c("N_surname", "N_name", "N_middlename", "N_suffix", "N_title"), sep = ";")

  v_semicolon <- vdf %>%
    filter(stringr::str_detect(lns, "^TEL;|^ORG;|^TITLE;")) %>%
    tidyr::separate_wider_delim(., lns, delim = ";", names = c("key", "value")) %>%
    tidyr::pivot_wider(names_from = key) %>%
    {
      if ("ORG" %in% names(.)) {
        tidyr::separate(., col = ORG, sep = ":", into = c("CHARSET", "ORG")) %>%
          tidyr::separate(., col = CHARSET, sep = "=", into = c("key", "CHARSET")) %>%
          select(-key)
      } else {
        .
      }
    } %>%
    {
      if ("TEL" %in% names(.)) {
        tidyr::separate(., col = TEL, sep = ":", into = c("TYPE", "TEL")) %>%
          mutate(PREF = stringr::str_detect(TYPE, "PREF")) %>%
          mutate(TYPE = stringr::str_replace(TYPE, ",PREF", "")) %>%
          tidyr::separate(., col = TYPE, sep = "=", into = c("key", "TYPE")) %>%
          select(-key)
      } else {
        .
      }
    } %>%
    {
      if ("TITLE" %in% names(.)) {
        tidyr::separate(., col = TITLE, sep = ":", into = c("CHARSET", "TITLE")) %>%
          tidyr::separate(., col = CHARSET, sep = "=", into = c("key", "CHARSET_TITLE")) %>%
          select(-key)
      } else {
        .
      }
    }

  if (nrow(v_semicolon) > 0) {
    res_df <- cbind(v_colon, v_semicolon) %>%
      relocate(FN, starts_with("N_"), .before = everything())
  } else {
    res_df <- v_colon %>%
      relocate(FN, starts_with("N_"), .before = everything())
  }


  ## PHOTO detection
  photo_lines_no <- which(grepl("PHOTO;", vdf$lns))

  if (length(photo_lines_no) > 0) {
    key_lines_no <- which(grepl(";|:", vdf$lns))

    # previous line (-1) to next control word [+1]
    photo_lines_bottom_no <- key_lines_no[which(key_lines_no %in% photo_lines_no) + 1] - 1

    photo_df_list <- purrr::map2(photo_lines_no, photo_lines_bottom_no, ~ {
      photo_lns <- vdf[.x:.y, ]
      photo <- photo_lns$lns %>%
        paste0(., collapse = "") %>%
        stringr::str_replace_all(., " ", "")
    })

    photo_df <- tibble::tibble(lns = photo_df_list)

    photo_df <- photo_df %>%
      tidyr::separate(., col = lns, into = c("TYPE", "PHOTO"), sep = ":") %>%
      mutate(TYPE = stringr::str_replace(TYPE, "PHOTO;", "")) %>%
      tidyr::separate(., col = TYPE, into = c("photo_ENCODING", "photo_TYPE"), sep = ";") %>%
      mutate(photo_ENCODING = stringr::str_replace(photo_ENCODING, "ENCODING=", "")) %>%
      mutate(photo_TYPE = stringr::str_replace(photo_TYPE, "TYPE=", ""))
    res_df <- cbind(res_df, photo_df)
  }
  return(res_df)
}
