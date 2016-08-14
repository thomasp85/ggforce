library(ggforce)
lines <- data.frame(
  x = c(1, 1.5, 2, 9, 6),
  y = c(0, 2, 0, 15, 5),
  xend = c(19, 17, 2, 9, 5),
  yend = c(10, 18, 7, 12, 1),
  width = c(1, 10, 6, 2, 3),
  colour = letters[1:5]
)

ggplot(lines[1:3, ]) + geom_bezier(aes(x=x,y=y), size=4) + geom_point(aes(x=x,y=y))

states <- c('eaten', "eaten but said you didn't", 'cat took it', 'for tonight',
            'will decompose slowly')
pie <- data.frame(
  state = factor(rep(states, 2), levels = states),
  type = rep(c('Pie', 'Donut'), each = 5),
  r0 = rep(c(0, 0.8), each = 5),
  focus=rep(c(0.2, 0, 0, 0, 0), 2),
  amount = c(4,3, 1, 1.5, 6, 6, 1, 2, 3, 2),
  stringsAsFactors = FALSE
)

pielinks <- pie
pielinks$destination <- pielinks$state
pielinks$destination[8] <- pielinks$state[2]
pielinks$destination[-8] <- NA 
pp <- subset(pielinks, type=="Donut")
pp <- rbind(pp, pp[1,])
pp$state[6] <- "cat took it"
pp$destination[6] <- "eaten"
# Look at the cakes
ggplot() + geom_arc_bar(data=pp[1:5, ],aes(x0=0, y0=0, r0=r0, r=1, amount=amount,
                            fill=state), sep=0.02,
                       stat='pie') +
  geom_bezier(data=pp, aes(x0=0, y0=0, r0=r0, r=r0, amount=amount, destinationnode=destination,
                   fill=state, sourcenode=state), size=8, sep=0.02,
                stat='link_pie') +
  coord_fixed() +
  theme_no_axes() +
  scale_fill_brewer('', type='qual')
