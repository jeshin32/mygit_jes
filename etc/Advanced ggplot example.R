library(ggplot2)
library(magrittr)
library(dplyr)
library(gridExtra)

df <- expand.grid(
        type = c("target", "real"),
        cate = c('A','B','C'),
        month = str_pad(1:12, width=2, pad=0),
        year = c(2018)
      )
df

set.seed(1)
df %<>% mutate(value = runif(nrow(df),2,3),
              ym = as.factor(paste0(year, "/", month)),
              positionBar = as.numeric(interaction(ym, type)),
              positionLine = as.numeric(ym)) %>% 
        mutate(positionBar = ifelse(positionBar %% 2 ==1, positionBar+0.07, positionBar-0.07))
df

g1 <- ggplot(df, aes(x = positionBar, y = value, fill = cate)) + 
  geom_bar(stat = "identity",color="white", width=0.8, alpha=0.8) +
  scale_x_continuous(breaks=seq(1.5, length=length(unique(df$ym)), by=2),
                     labels = unique(df$ym)) +
  xlab("Year") +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

df2 <- df %>% select(ym, type, value) %>% group_by(ym, type) %>% summarize(value = sum(value))  
df2
g2 <- ggplot(df2, aes(ym, value, group=type, color=type)) + 
  geom_line() +
  geom_point(size=4, shape=21, fill="white")+
  coord_cartesian(xlim=c(0.5, length(unique(df$ym))+0.5), ylim=c(5,9)) + 
  theme(legend.position = "top") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
g2

grid.arrange(
  grobs = list(g2, g1),
  heights = c(1, 2)
)

