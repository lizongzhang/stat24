data(mpg)
# mpg %>% 
#   ggplot(aes(displ, hwy, col = drv)) +
#   geom_point(aes(shape = cyl)) # 形状必须用离散型变量来映射，否则会报错

mpg %>% 
  ggplot(aes(displ, hwy, col = drv)) +
  geom_point(aes(shape = fl)) #chr类型就是离散型变量

mpg %>% 
  ggplot(aes(displ, hwy, col = drv, shape = fl)) + # col = drv放在ggplot画布上，
  #会对下面的所有图层生效
  geom_point() +
  geom_smooth(method = "lm", se = F) 

mpg %>% 
  ggplot(aes(displ, hwy, shape = fl)) + 
  #会对下面的所有图层生效
  geom_point(aes(color = drv)) + # col = drv放在点图图层，只对点图图层生效
  geom_smooth(method = "lm", se = F)

mpg %>% 
  ggplot(aes(displ, hwy, shape = fl)) + 
  #会对下面的所有图层生效
  geom_point() +
  geom_smooth(aes(color = drv),      # col = drv放在平滑线图层，只对平滑线图层生效
              method = "lm", se = F)

mpg %>% 
  ggplot(aes(hwy, fill = substr(trans, 1, 4))) +
  geom_histogram() +
  facet_wrap(~ substr(trans, 1, 4),
             labeller = labeller(`auto` = "Automatic", `manu` = "Manual")) +
  labs(title = "Highway Mileage Distribution by Transmission Type",
       x = "Highway Mileage", 
       y = "Frequency",
       fill = "Transmission")  


data(mtcars)

table(mtcars$vs, mtcars$am) %>% 
  chisq.test()

mtcars <- mtcars %>%
  mutate(vs = factor(vs, labels = c("V", "S")),
         am = factor(am, labels = c("Automatic", "Manual"))) 

table(mtcars$vs, mtcars$am) %>% 
  chisq.test()






