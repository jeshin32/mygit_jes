
library(ggplot2)
library(ggalt)
library(grid)

# 팔레트 설정  --------------------------------------------------
library(RColorBrewer)
col_pal <- "Accent"

# 사전 테마 설정 ---------------------------------------------------------------------------------------
theme_set(theme_bw())  # pre-set the bw theme.

# 기본 그리기  ---------------------------------------------------------------------------------------
ggplot(iris, aes(Sepal.Length, Sepal.Width, 
                 color = Species, group = Species)) +
  geom_point(aes(size = Petal.Length), alpha =0.5) +
  
  # 컬러 팔레트 설정  ---------------------------------------------------------------------------------------
  scale_color_brewer(name = "SPCS", palette = col_pal) + 
  scale_fill_brewer(palette = col_pal) +
  
  # 축 설정  ---------------------------------------------------------------------------------------
  coord_cartesian(xlim = c(4,8), ylim = c(2, 4.5)) +    # Zooming In
  # xlim(c(4.5, 10)) + ylim(c(2, 4.5)) +  # deletes points
  # coord_flip() +
  
  # 축 간격 및 라벨 설정  ---------------------------------------------------------------------------------------
  # scale_x_continuous(breaks = seq(4, 8, 0.8), labels = sprintf("%1.1f%%", seq(4, 8, 0.8))) +    # scale_x_reverse() +
  # scale_x_continuous(breaks = seq(4, 8, 0.4)) +
  scale_x_continuous(breaks = seq(4, 8, 0.4), labels = letters[1:11]) +
  
  # label 설정  ---------------------------------------------------------------------------------------
  labs(title="Advanced ggplot Example",
       subtitle="Example1", 
       y="SWidth", 
       x="SLength", 
       caption = "By Kevin") +
  # xlab("SLength") + ylab("SWidth") + ggtitle("Advanced ggplot Example", subtitle = "Example1") +
  
  # 텍스트 입력  ---------------------------------------------------------------------------------------
  geom_text(aes(label = Species), size = 2, alpha = 0.7) +
  annotation_custom(grid.text("my_text", x = 0.7,  y = 0.8, 
                              gp=gpar(col = "firebrick", fontsize = 11, fontface = "bold"))) +
  
  # 차트 격자 설정  ---------------------------------------------------------------------------------------
  facet_wrap( ~ Species) +
  
  # Grid lines  ---------------------------------------------------------------------------------------
  theme(panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(colour = "grey", size = 0.1),
        panel.grid.minor = element_line(colour = "grey",
                                        size = .05,
                                        linetype = "dashed"),
        # panel.border = element_blank(),
        axis.line.x = element_line(colour = "black",
                                   size = 0.5,
                                   lineend = "butt"),
        axis.line.y = element_line(colour = "black",
                                   size = 0.5)) +
  

  # 폰트 크기  ---------------------------------------------------------------------------------------
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 8)) +
  
  # 축 라벨 각도 위치
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  # 범례  ---------------------------------------------------------------------------------------
  theme(legend.position = "None") +
  theme(legend.position = "bottom") +
  
  # scale_color_discrete(name="Spec") +
  # scale_size_continuous(name="PL") +
  # theme(legend.title = element_text(size=12, color = "salmon", face="bold"),
  #         legend.justification=c(1,0), 
  #         legend.position=c(0.95, 0.05),  
  #         legend.background = element_blank(),
  #         legend.key = element_blank()) + 
  
  # encircle : 데이터의 바깥 영역 둘레 표시   ---------------------------------------------------------------------------------------
  geom_encircle(color = "green", 
                size = 2, fill="red",alpha=0.1,
                expand = 0.08) +
  
  # 보조 라인 그리기  ---------------------------------------------------------------------------------------
  geom_vline(aes(xintercept = mean(Sepal.Length)),size = 1, color = "red") + 
  geom_hline(aes(yintercept = mean(Sepal.Width)),size = 1, color = "red") +
  # geom_smooth(color = "grey40") +
  geom_smooth(method = "loess", se = T, color = "grey40")     # method="lm" 선형 
  
ggsave("myGgplot.png",width = 20, height = 10, units = "cm")
  