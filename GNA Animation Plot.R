library(tidyverse)
library(gganimate)
library(latticeExtra)
library(EpiModel)



#Generating networks

#Number of nodes 
nw <- network_initialize(n = 100)
#Creating rule for edges 
formation <- ~edges
#Number of edges 
target.stats <- c(50)
#Edge dissolving rule 
coef.diss <- dissolution_coefs(dissolution = ~offset(edges), duration = 50)
#Generating networks 
est <- netest(nw, formation, target.stats, coef.diss)

#Simulations 

#Infection Parameters
param <- param.net(inf.prob = 0.1, act.rate = 0.5, rec.rate = 0.01)
#Initial Conditions 
init <- init.net(i.num = 10, r.num = 0)
#Simulation controls 
control <- control.net(type = "SIR", nsims = 10, nsteps = 1000)
#Simulation 
sim <- netsim(est, param, init, control)
#Creating Dataframe
data <- as.data.frame(sim)


#Plot
p <- data%>%rename("Survived"=s.num,"Infected"=i.num,"Recovered"=r.num)%>%
  reshape2::melt(id.vars=c("time","sim"),measure.vars=c("Survived","Recovered","Infected"))%>%
  group_by(time,variable)%>%
  summarise(n = n(),
            mean = mean(value),
            median = median(value),
            sd = sd(value)) %>%
  mutate(sem = sd / sqrt(n - 1),
         CI_lower = mean + qt((1-0.95)/2, n - 1) * sem,
         CI_upper = mean - qt((1-0.95)/2, n - 1) * sem)%>%
  ggplot()+
  geom_line(aes(x=time,y=mean,colour=variable))+
  geom_ribbon(aes(x=time,ymin=CI_lower,ymax=CI_upper,fill=variable),colour="grey70",alpha=0.4)+
  labs(x="Time",y="Number",title="Epidemic Simulation",subtitle = "Time: {frame_along}")+
  theme_minimal()%+replace%
  theme(legend.position = "bottom",legend.title = element_blank())+
  transition_reveal(time)

animate(p)

anim_save(filename = "epianim.gif", animation = last_animation())
