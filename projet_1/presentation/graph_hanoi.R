library(ggplot2)
nb_disc<-seq(1,10)
nb_coup<-(2^nb_disc)-1
df <- data.frame(nb_disc, nb_coup)
ggplot(data = df, aes(x = nb_disc, y = nb_coup)) +
  geom_line(method = "lm", color="salmon1")+
  ggtitle("Tours de Hanoi : 
          nombre de coups en fonction du nombre de disque")+ 
  xlab("nombre de disque") +
  ylab("nombre de coups")+
  theme_dark(base_size = 12, base_family = "")
ggsave("graph_hanoi.png")

