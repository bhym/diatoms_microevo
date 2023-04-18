ganr_0 <- readRDS(file = "../disco_data/gnr_bout.RDS")
ganr_1 <- readRDS(file = "../disco_data/gnr_bout_higher.RDS")
ganr_2 <- readRDS(file = "../disco_data/gnr_bout_asex.RDS")
ganr_3 <- readRDS(file = "../disco_data/gnr_bout_compl.RDS")
ganr_4 <- readRDS(file = "../disco_data/gnr_bout_compl2.RDS")
ganr_5 <- readRDS(file = "../disco_data/gnr_bout_compl3.RDS")

ganr_0 <- ganr_0 %>% mutate(run = gsub("out", "", run))  %>% mutate(run = as.integer(run))
ganr_1 <- ganr_1 %>% mutate(run = gsub("out", "", run))  %>% mutate(run = as.integer(run) + max(ganr_0$run))
ganr_2 <- ganr_2 %>% mutate(run = gsub("asex", "", run)) %>% mutate(run = as.integer(run) + max(ganr_1$run))
ganr_3 <- ganr_3 %>% mutate(run = 1, rr = "1E-6", mu = "1E-3", perc = "0.35") %>% mutate(run = as.integer(run) + max(ganr_2$run))
ganr_4 <- ganr_4 %>% mutate(run = 1, rr = "1E-6", mu = "1E-4", perc = "0.35") %>% mutate(run = as.integer(run) + max(ganr_3$run))
ganr_5 <- ganr_5 %>% mutate(run = 1, rr = "1E-6", mu = "1E-2", perc = "0.35") %>% mutate(run = as.integer(run) + max(ganr_4$run))

ganr <- bind_rows(ganr_0, ganr_1) %>%
  bind_rows(ganr_2) %>%
  bind_rows(ganr_3) %>%
  bind_rows(ganr_4) %>%
  bind_rows(ganr_5) %>%
  mutate_all(as.numeric)

hz_0  <- readRDS(file = "../disco_data/gnr_hz_bout.RDS")
hz_1  <- readRDS(file = "../disco_data/gnr_hz_bout_higher.RDS")
hz_2  <- readRDS(file = "../disco_data/gnr_hz_bout_asex.RDS")
hz_3  <- readRDS(file = "../disco_data/gnr_hz_bout_compl.RDS")
hz_4  <- readRDS(file = "../disco_data/gnr_hz_bout_compl2.RDS")
hz_5  <- readRDS(file = "../disco_data/gnr_hz_bout_compl3.RDS")

hz_0 <- hz_0 %>% mutate(run = gsub("out", "", run)) %>% mutate(run = as.integer(run))
hz_1 <- hz_1 %>% mutate(run = gsub("out", "", run)) %>% mutate(run = as.integer(run) + max(ganr_0$run))
hz_2 <- hz_2 %>% mutate(run = gsub("asex", "", run)) %>% mutate(run = as.integer(run) + max(ganr_1$run))
hz_3 <- hz_3 %>%  mutate(run = 1, rr = "1E-6", mu = "1E-3", perc = "0.35")  %>% mutate(run = as.integer(run) + max(ganr_2$run))
hz_4 <- hz_4 %>%  mutate(run = 1, rr = "1E-6", mu = "1E-4", perc = "0.35")  %>% mutate(run = as.integer(run) + max(ganr_3$run))
hz_5 <- hz_5 %>%  mutate(run = 1, rr = "1E-6", mu = "1E-2", perc = "0.35")  %>% mutate(run = as.integer(run) + max(ganr_4$run))

time_    <- max(ganr$Generation)
bloom_start <- 126
bloom_durat <- 60

v <- rep(0, (time_ - 1))
v[c(seq_along(v) %% bloom_start) < bloom_durat] <- 1
inds <- diff(c(0, v))
start <- (2:time_)[inds == 1]
end <- (2:time_)[inds == -1]

dummy_gendayc <- tibble(Generation = seq_len(time_)[-1]) %>%
  mutate(Phase = case_when(Generation %in% c(2, start) ~1,
                           Generation %in% end ~ 0)) %>%
  fill(Phase) %>%
  mutate(Days_needed = ifelse(Phase == 1, 0.5, 5)) %>%
  mutate(Day = cumsum(Days_needed)) #%>% select(Generation, Day)

all_ <- bind_rows(hz_0, hz_1) %>%
  bind_rows(hz_2) %>%
  bind_rows(hz_3) %>%
  bind_rows(hz_4) %>%
  bind_rows(hz_5) %>%
  filter(grepl("hetero", name)) %>%
  select(-name) %>%
  mutate_all(as.numeric) %>%
  left_join(ganr) %>%
  left_join(dummy_gendayc) %>%
   mutate(Moment = case_when(between(N, 9.0e2, 1.1e3) ~ "nonb",
                             between(N, 1.9e3, 2.1e3) ~	"off1",
                             between(N, 1.4e4, 1.7e4) ~ "off2",
                             TRUE                     ~ "bloom"))  %>%
  group_by(run, rr, mu, perc) %>%
  mutate(just = ifelse((Moment == "nonb" & lead(Moment) != "nonb"), "pre", "uninf")) %>%
  mutate(just = ifelse((Moment == "nonb" & lag(Moment) != "nonb"), "post", just)) %>%
  ungroup() %>%
  rename(H = value) %>%
  mutate(Day = Day - min(Day)) %>%
  mutate(sex = ifelse(Generation %% 126 == 30, Day, NA),
         bst = ifelse(Generation %% 126 == 00, Day, NA),
         bee = ifelse(Generation %% 126 == 60, Day, NA),
  ) 
