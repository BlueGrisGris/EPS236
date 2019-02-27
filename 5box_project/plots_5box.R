library(ggplot2)
library(tidyr)
library(dplyr) 


####### plot int.results ##########
int.results.plotable <- data.frame(tt, int.results) 
colnames(int.results.plotable) <- c("time" , 
                       "box.1" , 
                       "box.2" , 
                       "box.3" , 
                       "box.4" , 
                       "box.5" ) 
                       
  
  
int.results.plotable <- gather(int.results.plotable , 
                               key  = "box",
                               value = ct ,
                              "box.1" ,
                              "box.2" ,
                              "box.3" ,
                              "box.4" ,
                              "box.5" 
                              ) #end gather
                              

int.results.plot <- ggplot(data = int.results.plotable,
       aes(x= time, color = factor(box))
) + #end ggplot
  
  geom_point(data = filter(int.results.plotable, box == "box.1"), aes(y = ct), color = "blue", size = .5 ) + 
  geom_point(data = filter(int.results.plotable, box == "box.2"), aes(y = ct), color = "red", size = .5 ) + 
  geom_point(data = filter(int.results.plotable, box == "box.3"), aes(y = ct), color = "black", size = .5 ) + 
  geom_point(data = filter(int.results.plotable, box == "box.4"), aes(y = ct), color = "green", size = .5 ) + 
  geom_point(data = filter(int.results.plotable, box == "box.5"), aes(y = ct), color = "grey", size = .5 ) + 
  
  theme_classic() + 
  
  scale_color_viridis_c() + 
  # theme_ethan()+
  # geom_text_repel(aes(label = type))+
  labs(x= "time" ,
       y= "ct") + 
  
  
  ggtitle("Integration Model") 
  
#geom_text_repel(aes(label = mean.perc.bb, size = 11) )  # turned off bc I cant control how many
# sigfigs the labels display

print(int.results.plot)



####### plot teo.results0 ##########
teo.results0.plotable <- data.frame(tt, teo.results0) 
colnames(teo.results0.plotable) <- c("time" , 
                                    "box.1" , 
                                    "box.2" , 
                                    "box.3" , 
                                    "box.4" , 
                                    "box.5" ) 



teo.results0.plotable <- gather(teo.results0.plotable , 
                               key  = "box",
                               value = ct ,
                               "box.1" ,
                               "box.2" ,
                               "box.3" ,
                               "box.4" ,
                               "box.5" 
) #end gather


teo.results0.plot <- ggplot(data = teo.results0.plotable,
                           aes(x= time, color = factor(box))
) + #end ggplot
  
  geom_point(data = filter(teo.results0.plotable, box == "box.1"), aes(y = ct), color = "blue", size = .5 ) + 
  geom_point(data = filter(teo.results0.plotable, box == "box.2"), aes(y = ct), color = "red", size = .5 ) + 
  geom_point(data = filter(teo.results0.plotable, box == "box.3"), aes(y = ct), color = "black", size = .5 ) + 
  geom_point(data = filter(teo.results0.plotable, box == "box.4"), aes(y = ct), color = "green", size = .5 ) + 
  geom_point(data = filter(teo.results0.plotable, box == "box.5"), aes(y = ct), color = "grey", size = .5 ) + 
  
  theme_classic() + 
  
  scale_color_viridis_c() + 
  # theme_ethan()+
  # geom_text_repel(aes(label = type))+
  labs(x= "time" ,
       y= "ct") + 
  

  ggtitle("Time Evolution Operator Model") 



print(teo.results0.plot)














####### plot sys.error.teo.int.results ##########
sys.error.teo.int.plotable <- data.frame(tt, sys.error.teo.int) 

colnames(sys.error.teo.int.plotable) <- c("time" , 
                                    "box.1" , 
                                    "box.2" , 
                                    "box.3" , 
                                    "box.4" , 
                                    "box.5" ) 



sys.error.teo.int.plotable <- gather(sys.error.teo.int.plotable , 
                               key  = "box",
                               value = ct ,
                               "box.1" ,
                               "box.2" ,
                               "box.3" ,
                               "box.4" ,
                               "box.5" 
) #end gather


sys.error.teo.int.plot <- ggplot(data = sys.error.teo.int.plotable,
                           aes(x= time, color = factor(box))
) + #end ggplot
  
  geom_point(data = filter(sys.error.teo.int.plotable, box == "box.1"), aes(y = ct), color = "blue", size = .5 ) + 
  geom_point(data = filter(sys.error.teo.int.plotable, box == "box.2"), aes(y = ct), color = "red", size = .5 ) + 
  geom_point(data = filter(sys.error.teo.int.plotable, box == "box.3"), aes(y = ct), color = "black", size = .5 ) + 
  geom_point(data = filter(sys.error.teo.int.plotable, box == "box.4"), aes(y = ct), color = "green", size = .5 ) + 
  geom_point(data = filter(sys.error.teo.int.plotable, box == "box.5"), aes(y = ct), color = "grey", size = .5 ) + 
  
  xlim(0, 2) +
  ylim(-.005, .005) +
  
  
  theme_classic() + 
  
  scale_color_viridis_c() + 
  # theme_ethan()+
  # geom_text_repel(aes(label = type))+
  labs(x= "time" ,
       y= "delta ct") + 
  
  
  ggtitle("Sytematic Error TEO, Integration") 

#geom_text_repel(aes(label = mean.perc.bb, size = 11) )  # turned off bc I cant control how many
# sigfigs the labels display

print(sys.error.teo.int.plot)









####### plot teo.results1 ##########
teo.results1.plotable <- data.frame(tt, teo.results1) 
colnames(teo.results1.plotable) <- c("time" , 
                                    "box.1" , 
                                    "box.2" , 
                                    "box.3" , 
                                    "box.4" , 
                                    "box.5" ) 



teo.results1.plotable <- gather(teo.results1.plotable , 
                                key  = "box",
                                value = ct ,
                                "box.1" ,
                                "box.2" ,
                                "box.3" ,
                                "box.4" ,
                                "box.5" 
) #end gather


teo.results1.plot <- ggplot(data = teo.results1.plotable,
                            aes(x= time, color = factor(box))
) + #end ggplot
  
  geom_point(data = filter(teo.results1.plotable, box == "box.1"), aes(y = ct), color = "blue", size = .5 ) + 
  geom_point(data = filter(teo.results1.plotable, box == "box.2"), aes(y = ct), color = "red", size = .5 ) + 
  geom_point(data = filter(teo.results1.plotable, box == "box.3"), aes(y = ct), color = "black", size = .5 ) + 
  geom_point(data = filter(teo.results1.plotable, box == "box.4"), aes(y = ct), color = "green", size = .5 ) + 
  geom_point(data = filter(teo.results1.plotable, box == "box.5"), aes(y = ct), color = "grey", size = .5 ) + 
  
  theme_classic() + 
  
  scale_color_viridis_c() + 
  # theme_ethan()+
  # geom_text_repel(aes(label = type))+
  labs(x= "time" ,
       y= "ct") + 
  
  
  ggtitle("Unit Impulse in Box 2 Time Evolution Operator Model") 

#geom_text_repel(aes(label = mean.perc.bb, size = 11) )  # turned off bc I cant control how many
# sigfigs the labels display

print(teo.results1.plot)














####### plot teo.results2 ##########
teo.results2.plotable <- data.frame(tt, teo.results2) 
colnames(teo.results2.plotable) <- c("time" , 
                                    "box.1" , 
                                    "box.2" , 
                                    "box.3" , 
                                    "box.4" , 
                                    "box.5" ) 



teo.results2.plotable <- gather(teo.results2.plotable , 
                                key  = "box",
                                value = ct ,
                                "box.1" ,
                                "box.2" ,
                                "box.3" ,
                                "box.4" ,
                                "box.5" 
) #end gather


teo.results2.plot <- ggplot(data = teo.results2.plotable,
                            aes(x= time, color = factor(box))
) + #end ggplot
  
  geom_point(data = filter(teo.results2.plotable, box == "box.1"), aes(y = ct), color = "blue", size = .5 ) + 
  geom_point(data = filter(teo.results2.plotable, box == "box.2"), aes(y = ct), color = "red", size = .5 ) + 
  geom_point(data = filter(teo.results2.plotable, box == "box.3"), aes(y = ct), color = "black", size = .5 ) + 
  geom_point(data = filter(teo.results2.plotable, box == "box.4"), aes(y = ct), color = "green", size = .5 ) + 
  geom_point(data = filter(teo.results2.plotable, box == "box.5"), aes(y = ct), color = "grey", size = .5 ) + 
  
  theme_classic() + 
  
  scale_color_viridis_c() + 
  # theme_ethan()+
  # geom_text_repel(aes(label = type))+
  labs(x= "time" ,
       y= "ct") + 
  
  
  ggtitle("Unit Impulse in Box 2 Time Evolution Operator Model") 

#geom_text_repel(aes(label = mean.perc.bb, size = 11) )  # turned off bc I cant control how many
# sigfigs the labels display

print(teo.results2.plot)














####### plot teo.results3 ##########
teo.results3.plotable <- data.frame(tt, teo.results3) 
colnames(teo.results3.plotable) <- c("time" , 
                                    "box.1" , 
                                    "box.2" , 
                                    "box.3" , 
                                    "box.4" , 
                                    "box.5" ) 



teo.results3.plotable <- gather(teo.results3.plotable , 
                                key  = "box",
                                value = ct ,
                                "box.1" ,
                                "box.2" ,
                                "box.3" ,
                                "box.4" ,
                                "box.5" 
) #end gather


teo.results3.plot <- ggplot(data = teo.results3.plotable,
                            aes(x= time, color = factor(box))
) + #end ggplot
  
  geom_point(data = filter(teo.results3.plotable, box == "box.1"), aes(y = ct), color = "blue", size = .5 ) + 
  geom_point(data = filter(teo.results3.plotable, box == "box.2"), aes(y = ct), color = "red", size = .5 ) + 
  geom_point(data = filter(teo.results3.plotable, box == "box.3"), aes(y = ct), color = "black", size = .5 ) + 
  geom_point(data = filter(teo.results3.plotable, box == "box.4"), aes(y = ct), color = "green", size = .5 ) + 
  geom_point(data = filter(teo.results3.plotable, box == "box.5"), aes(y = ct), color = "grey", size = .5 ) + 
  
  theme_classic() + 
  
  scale_color_viridis_c() + 
  # theme_ethan()+
  # geom_text_repel(aes(label = type))+
  labs(x= "time" ,
       y= "ct") + 
  
  
  ggtitle("Unit Impulse in Box 4 Time Evolution Operator Model") 

#geom_text_repel(aes(label = mean.perc.bb, size = 11) )  # turned off bc I cant control how many
# sigfigs the labels display

print(teo.results3.plot)














####### plot teo.results4 ##########
teo.results4.plotable <- data.frame(tt, teo.results4) 
colnames(teo.results4.plotable) <- c("time" , 
                                    "box.1" , 
                                    "box.2" , 
                                    "box.3" , 
                                    "box.4" , 
                                    "box.5" ) 



teo.results4.plotable <- gather(teo.results4.plotable , 
                                key  = "box",
                                value = ct ,
                                "box.1" ,
                                "box.2" ,
                                "box.3" ,
                                "box.4" ,
                                "box.5" 
) #end gather


teo.results4.plot <- ggplot(data = teo.results4.plotable,
                            aes(x= time, color = factor(box))
) + #end ggplot
  
  geom_point(data = filter(teo.results4.plotable, box == "box.1"), aes(y = ct), color = "blue", size = .5 ) + 
  geom_point(data = filter(teo.results4.plotable, box == "box.2"), aes(y = ct), color = "red", size = .5 ) + 
  geom_point(data = filter(teo.results4.plotable, box == "box.3"), aes(y = ct), color = "black", size = .5 ) + 
  geom_point(data = filter(teo.results4.plotable, box == "box.4"), aes(y = ct), color = "green", size = .5 ) + 
  geom_point(data = filter(teo.results4.plotable, box == "box.5"), aes(y = ct), color = "grey", size = .5 ) + 
  
  theme_classic() + 
  
  scale_color_viridis_c() + 
  # theme_ethan()+
  # geom_text_repel(aes(label = type))+
  labs(x= "time" ,
       y= "ct") + 
  
  
  ggtitle("Unit Impulse in Box 5 Time Evolution Operator Model") 

#geom_text_repel(aes(label = mean.perc.bb, size = 11) )  # turned off bc I cant control how many
# sigfigs the labels display

print(teo.results4.plot)
















####### plot super.results ##########
super.results.plotable <- data.frame(tt2, super.results) 
colnames(super.results.plotable) <- c("time" , 
                                    "box.1" , 
                                    "box.2" , 
                                    "box.3" , 
                                    "box.4" , 
                                    "box.5" ) 



super.results.plotable <- gather(super.results.plotable , 
                                key  = "box",
                                value = ct ,
                                "box.1" ,
                                "box.2" ,
                                "box.3" ,
                                "box.4" ,
                                "box.5" 
) #end gather


super.results.plot <- ggplot(data = super.results.plotable,
                            aes(x= time, color = factor(box))
) + #end ggplot
  
  geom_point(data = filter(super.results.plotable, box == "box.1"), aes(y = ct), color = "blue", size = .5 ) + 
  geom_point(data = filter(super.results.plotable, box == "box.2"), aes(y = ct), color = "red", size = .5 ) + 
  geom_point(data = filter(super.results.plotable, box == "box.3"), aes(y = ct), color = "black", size = .5 ) + 
  geom_point(data = filter(super.results.plotable, box == "box.4"), aes(y = ct), color = "green", size = .5 ) + 
  geom_point(data = filter(super.results.plotable, box == "box.5"), aes(y = ct), color = "grey", size = .5 ) + 
  
  theme_classic() + 
  
  scale_color_viridis_c() + 
  # theme_ethan()+
  # geom_text_repel(aes(label = type))+
  labs(x= "time" ,
       y= "ct") + 
  
  
  ggtitle("Superposition Time Evolution Operator Model") 

#geom_text_repel(aes(label = mean.perc.bb, size = 11) )  # turned off bc I cant control how many
# sigfigs the labels display

print(super.results.plot)














####### plot inverse.results ##########
inverse.results.plotable <- data.frame(tt3, inverse.results) 
colnames(inverse.results.plotable) <- c("time" , 
                                      "box.1" , 
                                      "box.2" , 
                                      "box.3" , 
                                      "box.4" , 
                                      "box.5" ) 



inverse.results.plotable <- gather(inverse.results.plotable , 
                                 key  = "box",
                                 value = ct ,
                                 "box.1" ,
                                 "box.2" ,
                                 "box.3" ,
                                 "box.4" ,
                                 "box.5" 
) #end gather


inverse.results.plot <- ggplot(data = inverse.results.plotable,
                             aes(x= time, color = factor(box))
) + #end ggplot
  
  geom_point(data = filter(inverse.results.plotable, box == "box.1"), aes(y = ct), color = "blue", size = .5 ) + 
  geom_point(data = filter(inverse.results.plotable, box == "box.2"), aes(y = ct), color = "red", size = .5 ) + 
  geom_point(data = filter(inverse.results.plotable, box == "box.3"), aes(y = ct), color = "black", size = .5 ) + 
  geom_point(data = filter(inverse.results.plotable, box == "box.4"), aes(y = ct), color = "green", size = .5 ) + 
  geom_point(data = filter(inverse.results.plotable, box == "box.5"), aes(y = ct), color = "grey", size = .5 ) + 
  
  geom_point(data = intercept.ct.1, aes(x = time, y = ct) ) +

  
  xlim(0, 5) +
  ylim(0, 1) +
  
  
  theme_classic() + 
  
  scale_color_viridis_c() + 
  # theme_ethan()+
  # geom_text_repel(aes(label = type))+
  labs(x= "-time" ,
       y= "ct") + 
  
  
  ggtitle("Inverse Time Evolution Operator Model") 

#geom_text_repel(aes(label = mean.perc.bb, size = 11) )  # turned off bc I cant control how many
# sigfigs the labels display

print(inverse.results.plot)















####### sys.error.rev.results ##########
sys.error.rev.plotable <- data.frame(tt3, sys.error.rev) 
colnames(sys.error.rev.plotable) <- c("time" , 
                                        "box.1" , 
                                        "box.2" , 
                                        "box.3" , 
                                        "box.4" , 
                                        "box.5" ) 



sys.error.rev.plotable <- gather(sys.error.rev.plotable , 
                                   key  = "box",
                                   value = ct ,
                                   "box.1" ,
                                   "box.2" ,
                                   "box.3" ,
                                   "box.4" ,
                                   "box.5" 
) #end gather


sys.error.rev.plot <- ggplot(data = sys.error.rev.plotable,
                               aes(x= time, color = factor(box))
) + #end ggplot
  
  geom_point(data = filter(sys.error.rev.plotable, box == "box.1"), aes(y = ct), color = "blue", size = .5 ) + 
  geom_point(data = filter(sys.error.rev.plotable, box == "box.2"), aes(y = ct), color = "red", size = .5 ) + 
  geom_point(data = filter(sys.error.rev.plotable, box == "box.3"), aes(y = ct), color = "black", size = .5 ) + 
  geom_point(data = filter(sys.error.rev.plotable, box == "box.4"), aes(y = ct), color = "green", size = .5 ) + 
  geom_point(data = filter(sys.error.rev.plotable, box == "box.5"), aes(y = ct), color = "grey", size = .5 ) + 
  
  xlim(0, 5) +
  ylim(-1, 1) +
  
  
  theme_classic() + 
  
  scale_color_viridis_c() + 
  # theme_ethan()+
  # geom_text_repel(aes(label = type))+
  labs(x= "time" ,
       y= "delta ct") + 
  
  
  ggtitle("Introduced Error") 

#geom_text_repel(aes(label = mean.perc.bb, size = 11) )  # turned off bc I cant control how many
# sigfigs the labels display

print(sys.error.rev.plot)













