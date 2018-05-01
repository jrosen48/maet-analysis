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
                       names_df4 = names(df4),
                       names_df5 = names(df5))

names_df # checking these are named the same

names(df1) <- names(df5)
names(df2) <- names(df5)
names(df3) <- names(df5)
names(df4) <- names(df5)
names(df5) <- names(df5)

df_post <- bind_rows(df1, df2, df3, df4, df5)
df_pre <- rename(df_pre, "Most Important" = "Want to Learn More")
df_post

df_post <- mutate(df_post, 
                 date = lubridate::mdy_hms(Timestamp),
                 year = lubridate::year(date), 
                 time = "post")

# All

b <- gs_title('Summary Data MAET Survey')
b <- gs_read(b, ws = 1)
the_cat <- as.numeric(as.vector(unlist(b[b$Timestamp == "Type of variable", ])))
the_cat

d <- bind_rows(df_pre, df_post)
d

d %>%
  group_by(time) %>% 
  summarize_if(is.numeric, mean) %>% 
  gather(key, val, -time, -year) %>% 
  ggplot(aes(x = reorder(key, val), y = val, fill = time)) +
    geom_col(position = "dodge") +
  coord_flip() +
  xlab(NULL) +
  ylab("Value") +
  scale_fill_brewer("Survey Timepoint",  type = "qual") +
  theme_bw() +
  ggtitle("MAET Technology Survey Responses From 2013-2017 (n = 74)")
