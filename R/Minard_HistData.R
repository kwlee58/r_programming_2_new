library(HistData)
data(Minard.troops)
data(Minard.cities)
data(Minard.temp)

library(ggplot2)
library(magrittr)
library(scales)
library(gridExtra)

plot_troops <- ggplot(Minard.troops, aes(long, lat)) +
  geom_path(aes(linewidth = survivors, colour = direction, group = group),
            lineend = "round", linejoin = "round")
plot_cities <- geom_text(aes(label = city), size = 4, data = Minard.cities)

breaks <- c(1, 2, 3) * 10^5 
plot_minard <- plot_troops + plot_cities +
  scale_size("Survivors", range = c(1, 10), 
             breaks = breaks, labels = scales::comma(breaks)) +
  scale_color_manual("Direction", 
                     values = c("#E1B08C", "#333333"), 
                     labels=c("Advance", "Retreat")) +
  coord_cartesian(xlim = c(23.5, 40)) +
  xlab(NULL) + 
  ylab("Latitude") + 
  ggtitle("Napoleon's March on Moscow") +
  theme_bw() +
  theme(legend.position = "inside", 
        legend.position.inside=c(.8, .2), 
        legend.box="horizontal")

# 1. 데이터 준비 (Minard.temp에서 date가 NA인 행 제거)
Minard.temp.clean <- Minard.temp %>% 
  dplyr::filter(!is.na(Minard.temp$date))

# 2. plot_temp 수정 (새로운 데이터셋 사용)
plot_temp <- ggplot(Minard.temp.clean, aes(long, temp)) + # clean 데이터 사용
  geom_path(color="grey", size=1.5) +
  geom_point(size=2) +
  geom_text(aes(label=date)) +
  xlab("Longitude") + ylab("Temperature") +
  coord_cartesian(xlim = c(23.5, 40)) + 
  theme_bw()

# 3. plot_minard는 그대로 (xlim=c(23.5, 40)이 적용된 상태)

# 4. 최종 실행
grid.arrange(plot_minard, plot_temp, nrow=2, heights=c(3,1))

