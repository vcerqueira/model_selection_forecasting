df <- data.frame(
  Loss = rep(c("True", "E1", "E2"), each=4),
  Model = rep(c("A1", "A2", "A3", "A4"), 3),
  value = c(1,2,3,4,
            2,1.5,5,3,
            1*3,2*3,3*3,4*3
            )
)

df$Loss <- factor(df$Loss, levels=unique(df$Loss))
ggplot(data=df, aes(x=Model, y=value, fill=Loss)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=value), vjust=1.6, color="white",
            position = position_dodge(0.9), size=4)+
  scale_fill_brewer(palette="Paired")+
  theme_minimal() + 
  labs(y="Error Estimates") + 
  theme(legend.position="top")
