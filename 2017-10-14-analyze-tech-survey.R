library(tidyverse)
library(googlesheets)
library(stringr)

my_sheets <- gs_ls()

my_sheets

df <- gs_title('MAET 2017 Tech Pre-Survey (Responses)')

df <- gs_read(df, ws = 1)

df1 <- gs_title('MAET 2017 Tech Post-Survey (Responses)')

df1 <- gs_read(df1, ws = 1)

library(quanteda)

x <- df1$`Most Important`

xx <- x %>% 
  str_split("[^[:alnum:] ]") %>% 
  map_chr(~ paste(., sep="", collapse=""))

xxx <- dfm(xx, remove = stopwords("english"))

dfp <- topfeatures(xxx, n = 20)

dfp <- dfp[c(-1, -4, -5, -9, -10, -11, -11)]

data_frame(var = names(dfp), n = dfp) %>% 
  ggplot(aes(x = reorder(var, n), y = n)) +
  geom_col() +
  coord_flip() +
  hrbrthemes::theme_ipsum_rc() +
  xlab(NULL) + 
  ylab("Number of Times Mentioned")


ggsave("want_to_learn.png", width = 5, height = 5)
names(dfp) <- c("var", "n")

dfp

x <- df$`Want to Learn More`
x <- x[1:14]

x

strsplit(gsub("[^[:alnum:] ]", "", x), " +")[[1]]

dfmm <- x %>% 
  stringr::str_split("[^[:alnum:] ]") %>% 
  map_chr(~ paste(., sep=" ", collapse=" ")) %>% 
  dfm(stem = F, remove = stopwords("english"))

textplot_wordcloud(dfmm, min.freq = 1, random.order = FALSE,
                   rot.per = .25, 
                   colors = RColorBrewer::brewer.pal(8,"Dark2"))


df_ss <- select(df, `Overall Confidence`:`Navigate a Firewall`)

df1_ss <- select(df1, `Overall Confidence`:`Navigate a Firewall`)

df_ss <- mutate(df_ss, time = "Pre")

df1_ss <- mutate(df1_ss, time = "Post")

df <- bind_rows(df_ss, df1_ss)

df %>% 
  group_by(time) %>% 
  summarize_all(funs(mean)) %>% 
  gather(key, val, -time) %>% 
  filter(time == "Pre") %>% 
  ggplot(aes(x = reorder(key, val), y= val, fill = time)) +
  geom_col(position = "dodge") +
  hrbrthemes::theme_ipsum_rc() +
  coord_flip() +
  xlab(NULL) +
  ylab("Mean Change") +
  theme(legend.position="none")

ggsave("pre.png", width = 6, height = 7.25)

df %>% 
  group_by(time) %>% 
  summarize_all(funs(mean)) %>% 
  gather(key, val, -time) %>% 
  mutate(time = as.factor(time)) %>%
  mutate(time = forcats::fct_relevel(time, "Pre", "Post")) %>% 
  ggplot(aes(x = reorder(key, val), y= val, fill = time)) +
  geom_col(position = "dodge") +
  hrbrthemes::theme_ipsum_rc() +
  coord_flip() +
  xlab(NULL) +
  ylab("Mean Change") +
  viridis::scale_fill_viridis("", discrete=T)

ggsave("pre-post.png", width = 6.67, height = 8)

to_plot <- df %>% 
  group_by(time) %>% 
  summarize_all(funs(mean)) %>% 
  gather(key, val, -time) %>% 
  arrange(key, desc(time)) %>% 
  mutate(diff = val - lag(val)) %>% 
  filter(time == "Post") %>% 
  arrange(desc(diff))

to_plot[1:10, ] %>% 
  ggplot(aes(x = reorder(key, diff), y = diff)) +
  geom_col() +
  hrbrthemes::theme_ipsum_rc() +
  coord_flip() +
  xlab(NULL) +
  ylab("Mean Change")

ggsave("fig_8-17.png", width = 6, height = 5)

x <- df %>% 
  group_by(time) %>% 
  summarize_all(funs(mean)) %>% 
  gather(key, val, -time) %>% 
  arrange(key, desc(time)) %>% filter(time == "Pre")

x

xx <- df %>% 
  group_by(time) %>% 
  summarize_all(funs(mean)) %>% 
  gather(key, val, -time) %>% 
  arrange(key, desc(time)) %>% 
  mutate(diff = val - lag(val)) %>% 
  filter(time == "Post") %>% 
  arrange(desc(val)) %>% 
  left_join(x, by = "key")

xx

xx[39:48, ] %>% 
  select(key, pre = val.y, post = val.x) %>% 
  gather(keykey, val, -key) %>% 
  mutate(keykey = as.factor(keykey)) %>%
  mutate(keykey = forcats::fct_relevel(keykey, "pre", "post")) %>% 
  ggplot(aes(x = reorder(key,val), y = val, group = keykey, fill = keykey)) +
  geom_col(position = "dodge") +
  hrbrthemes::theme_ipsum_rc() +
  coord_flip() +
  xlab(NULL) +
  ylab("Mean Change") +
  viridis::scale_fill_viridis("", discrete = T)

ggsave("fig3_8-17.png", width = 6, height = 5)

