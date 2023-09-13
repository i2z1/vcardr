#' Produces VCF contact record for ONE person dataframe rows
#'
#' @param df dataframe with ONE person contact fields. From this dataframe will be produced one contact record
#'
#' @return str VCF record for one contact
#' @import dplyr
#'
write_contact <- function(df){
  N <- df %>%
    select(starts_with("N_")) %>%
    mutate(not_na_N_s = sum(!is.na(.))) %>%
    arrange() %>%
    tail(1) %>%
    select(starts_with("N_")) %>%
    unlist() %>%
    ifelse(is.na(.), "", .) %>%
    paste0(., collapse = ";")

  out_bday <- unify_most_char(df$BDAY)
  out_org <- unify_most_char(df$ORG)
  out_title <- unify_most_char(df$TITLE)
  out_photo <- unify_most_char(df$PHOTO)

  tel_strlist_1 <- df %>%
    select(TYPE, PREF, TEL) %>%
    filter(!is.na(TEL)) %>%
    mutate(PREF = ifelse(is.na(PREF), FALSE, PREF)) %>%
    distinct(TEL, .keep_all = T)
  if(nrow(tel_strlist_1)>0){
    tel_strlist_2 <-  tel_strlist_1 %>%
      rowwise() %>%
      mutate(res_str = paste0("TEL;TYPE=", TYPE, ifelse(PREF, ",PREF", ""), ":", TEL, collapse = "")) %>%
      select(res_str) %>%
      unlist() %>%
      paste0(., collapse = "\n")
  } else {
    tel_strlist_2 <- NA
  }

  out <- paste0(
    "BEGIN:VCARD\n",
    "VERSION:3.0\n",
    "N:", N,"\n",
    "FN:", df$FN[1], "\n",
    ifelse(is.na(tel_strlist_2),"", paste0(tel_strlist_2, "\n")),
    ifelse(is.na(out_bday),"", paste0("BDAY:",out_bday, "\n")),
    ifelse(is.na(out_org),"", paste0("ORG;CHARSET=UTF-8:",out_org, "\n")),
    ifelse(is.na(out_title),"", paste0("TITLE;CHARSET=UTF-8:",out_title, "\n")),
    ifelse(is.na(out_photo),"", paste0("PHOTO;ENCODING=b;TYPE=JPEG:",out_photo, "\n")),
    "END:VCARD\n")
  return(out)
}

#' Export contacts dataframe to VCF vcard file
#'
#' @param contacts_df cantacts dataframe. Should have FN and at last one of N contact fields
#' @param filepath_vcf path to write VCF file
#'
#' @return NA
#' @export
#' @import dplyr purrr
#'
export2vcf <- function(contacts_df, filepath_vcf){
  contact_lst <- contacts_df %>%
    dplyr::group_split(FN)

  all_str <- purrr::map_chr(contact_lst, write_contact)
  write(all_str, file = filepath_vcf)
}

#' Collapsing vector in one element, which has max str length
#'
#' @param vec vector of string parameters or empty string or NA
#'
#' @return str
#' @import stringr dplyr
#'
unify_most_char <- function(vec){
  data.frame(vec = vec) %>%
    mutate(len = stringr::str_count(vec)) %>%
    arrange(len) %>%
    na.omit(len) %>%
    tail(1) %>%
    select(1) %>%
    unlist() %>%
    unname()
}
