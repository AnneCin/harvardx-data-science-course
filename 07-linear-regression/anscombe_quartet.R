library(tidyverse)
cor1 <- cor(anscombe$x1, anscombe$y1)
cor2 <- cor(anscombe$x2, anscombe$y2)
cor3 <- cor(anscombe$x3, anscombe$y3)
cor4 <- cor(anscombe$x4, anscombe$y4)

anscombe_long <- anscombe %>%
  pivot_longer(everything(),
               names_to = c(".value", "set"),
               names_pattern = "(.)(.)"
  )

anscombe_long %>% ggplot(aes(x, y)) + 
  geom_point() + geom_smooth(method=glm, se=FALSE)+ 
  facet_wrap(~set)