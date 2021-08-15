#####################################
# 第48回衆議院議員総選挙
# https://www.soumu.go.jp/senkyo/senkyo_s/data/shugiin48/index.html
# 都道府県別届出政党等別得票数（小選挙区）
#####################################
library(dplyr)
read_pref_party_votes <- function(data, party) {
  data %>% 
    purrr::set_names(c("区分", stringr::str_c(
      rep(party, each = 3),
      "_",
      c("男", "女", "計") %>% 
        rep(times = length(party))))) %>% 
    mutate(across(c(ends_with("男"),
                    ends_with("女"),
                    ends_with("計")),
                  .fns = ~ as.character(.x) %>% 
                    readr::parse_number()))
}

df_shugiin48_party_votes <-
  left_join(
    readxl::read_xls(here::here("data/shugiin48/000516724.xls"), 
                     skip = 4, 
                     n_max = 50) %>% 
      read_pref_party_votes(c("自由民主党", "立憲民主党", "希望の党", "公明党")),
    readxl::read_xls(here::here("data/shugiin48/000516724.xls"), 
                     skip = 56) %>% 
      select(1:10) %>% 
      read_pref_party_votes(c("日本共産党", "日本維新の会", "社会民主党")),
    by = "区分") %>% 
  left_join(
    readxl::read_xls(here::here("data/shugiin48/000516724.xls"), 
                     skip = 4, 
                     n_max = 50, 
                     sheet = 2) %>% 
      select(1:10) %>% 
      read_pref_party_votes(c("諸派", "無所属", "合計")),
    by = "区分")

df_shugiin48_party_votes %>% 
  readr::write_rds(here::here("data/shugiin48_prefecture_party_votes.rds"))
