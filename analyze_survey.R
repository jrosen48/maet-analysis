library(googlesheets)
library(tidyverse)
library(prcr)

x <- gs_ls()
x
x %>% select(sheet_title) %>% filter(stringr::str_detect(sheet_title, "2013"))

# Pre

df1 <- gs_title('MAET 2013 Tech Survey (Responses) x')
df1 <- gs_read(df1, ws = 1)

df2 <- gs_title('MAET 2014 Tech Pre-Survey (Responses) x')
df2 <- gs_read(df2, ws = 1)

df3 <- gs_title('MAET 2015 Tech Pre-Survey (Responses)')
df3 <- gs_read(df3, ws = 1)
df3

df4 <- gs_title('MAET 2016 Tech Pre-Survey (Responses)')
df4 <- gs_read(df4, ws = 1)
df4 <- select(df4, -54, -53, -52, -51)
df4

df5 <- gs_title('MAET 2017 Tech Pre-Survey (Responses)')
df5 <- gs_read(df5, ws = 1)
df5 <- select(df5, -52, -51)
df5

nrow(df1) + nrow(df2) + nrow(df3) + nrow(df4) + nrow(df5)

names_df <- data_frame(names_df1 = names(df1),
                       names_df2 = names(df2),
                       names_df3 = names(df3),
                       names_df4 = names(df4),
                       names_df5 = names(df5))

names_df # checking these are all named the same

names(df1) <- names(df5)
names(df2) <- names(df5)
names(df3) <- names(df5)
names(df4) <- names(df5)
names(df5) <- names(df5)

df_pre <- bind_rows(df1, df2, df3, df4, df5) # 74 observations from five years

df_pre <- mutate(df_pre, 
                 date = lubridate::mdy_hms(Timestamp),
                 year = lubridate::year(date), 
                 time = "pre")

# Posts

x %>% 
    select(sheet_title) %>% 
    filter(stringr::str_detect(sheet_title, "2016"))

df1 <- gs_title('MAET 2013 Tech Post-survey  (Responses)')
df1 <- gs_read(df1, ws = 1)
df1

df2 <- gs_title('MAET 2014 Tech Post-Survey  (Responses)')
df2 <- gs_read(df2, ws = 1)
df2

df3 <- gs_title('MAET 2015 Tech Post-Survey  (Responses)')
df3 <- gs_read(df3, ws = 1)
df3

df4 <- gs_title('MAET 2016 Tech Post-Survey (Responses)')
df4 <- gs_read(df4, ws = 1)
df4 <- select(df4, -54, -53, -52, -51)
df4

df5 <- gs_title('MAET 2017 Tech Post-Survey (Responses)')
df5 <- gs_read(df5, ws = 1)
df5 <- select(df5, -54, -53, -52, -51)
df5

nrow(df1) + nrow(df2) + nrow(df3) + nrow(df4) + nrow(df5) # 69 post obs 

names_df <- data_frame(names_df1 = names(df1),
                       names_df2 = names(df2),
                       names_df3 = names(df3),
                       names_df4 = names(df4))

df1

names(df1) <- names(df5)
names(df2) <- names(df5)
names(df3) <- names(df5)
names(df4) <- names(df5)

df_post <- bind_rows(df1, df2, df3, df4)

df_post <- mutate(df_post, 
                 date = lubridate::mdy_hms(Timestamp),
                 year = lubridate::year(date), 
                 time = "post")

# All

df_pre <- filter(df_pre, year < 2017)
df_post

gs_new("MAET year 2", df)

df <- bind_rows(df_pre, df_post)

clipr::write_clip(df)

gs_ws_feed("MEAT year 2", df)

df %>% 
    group_by(time, year) %>% 
    summarize_if(is.integer, mean) %>% 
    gather(key, val, -year, -time) %>% 
    ggplot(aes(x = year, y = val, color = time)) +
    geom_point() +
    geom_line() +
    # geom_smooth(method = "lm", color = "red") +
    facet_wrap("key") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

dff %>% 
    select(-1) %>% 
    group_by(year) %>% 
    summarize_if(is.integer, mean) %>% 
    gather(key, val, -year) %>% 
    ggplot(aes(x = year, y = val)) +
    geom_point() +
    geom_line() +
    geom_smooth(method = "lm", color = "red") +
    facet_wrap("key") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Old

df %>% 
    select(`Overall Confidence`:`Navigate a Firewall`)

df %>% 
    select(`Overall Confidence`:`Navigate a Firewall`) %>% 
    map_df(mean) %>% 
    gather(variable, mean) %>% 
    arrange(desc(mean))

x <- df %>% 
    select(`Overall Confidence`:`Navigate a Firewall`) %>% 
    map_df(mean) %>% 
    gather(variable, mean) %>% 
    arrange(mean)

x[1:10, ] %>% ggplot(aes(x = reorder(variable, -mean), y = mean)) +
    geom_col() +
    coord_flip() +
    hrbrthemes::theme_ipsum_rc() +
    xlab(NULL) + 
    ylab("Mean Score")

ggsave("bottom.png", width = 5, height = 4)

# Plot

df %>% 
    select(`Overall Confidence`:`Navigate a Firewall`) %>% 
    map_df(mean) %>% 
    gather(variable, mean) %>% 
    arrange(desc(mean)) %>% 
    ggplot(aes(x = reorder(variable, mean), y = mean)) +
    geom_col(position = "identity", fill = "cyan4") +
    # coord_flip() +
    hrbrthemes::theme_ipsum_rc(base_size = 12) +
    ylab("Confidence (5 = Most Confident, 1 = Least Confident)") +
    xlab(NULL) +
    ggtitle("Confidence in Using Technologies") +
    labs(subtitle = c("Pre-survey Results for MAET Y2 2017 Students")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(plot.margin = unit(c(50, 50, 50, 50), "points")) + 
    coord_polar()

ggsave("pre_confidence_2017_MAET.png", width = 12, height = 10)

# want to learn

df <- gs_title("Least and Most Confident Technologies")

df <- gs_read(df, ws = 1)

df$`Most Confidence (Top 10)`

library(quanteda)

x <- df$`Specific Technology to Learn More About`
x <- x[1:14]

xx <- x %>% 
    stringr::str_split("[^[:alnum:] ]") %>% 
    map_chr(~ paste(., sep="", collapse=""))

xxx <- dfm(xx, remove = stopwords("english"))

dfp <- topfeatures(xxx, n = 20)
dfp

dfp <- dfp[c(-1, -7, -9)]
dfp <- dfp[1:10]

data_frame(var = names(dfp), n = dfp) %>% 
    ggplot(aes(x = reorder(var, n), y = n)) +
    geom_col() +
    coord_flip() +
    hrbrthemes::theme_ipsum_rc() +
    xlab(NULL) + 
    ylab("Number of Times Mentioned")


names(dfp) <- c("var", "n")

x <- df$`Want to Learn More`
x <- x[1:14]

strsplit(gsub("[^[:alnum:] ]", "", x), " +")[[1]]

dfmm <- x %>% 
    stringr::str_split("[^[:alnum:] ]") %>% 
    map_chr(~ paste(., sep=" ", collapse=" ")) %>% 
    dfm(stem = F, remove = stopwords("english"))

textplot_wordcloud(dfmm, min.freq = 1, random.order = FALSE,
                   rot.per = .25, 
                   colors = RColorBrewer::brewer.pal(8,"Dark2"))
