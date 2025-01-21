source('~/R_func/ReadingData/Rtfunction.R')

piemonte<-as.data.frame(c(0,0,0,9,0,38,2,5,26,26,35,64,83,60,103,48,79,260,33,238,405,381,444,591,529, 291,668,441,654,509,510,558,579,535,506,
                          589,494,558,543,813,653,562,419,540,639,490,996,652,474,556,539,879,695,661,593,292,606,784,401,682,604,394,278,352,
                          411,428))


Rt_reference<-Rt_calculation(piemonte)
Rt_reference$Rt$Rt_familyGamma


ggsave(plot = Rt_reference$Rt$Rt_familyGamma,filename = paste("Plot/Rt_reference.pdf"),
       dpi = 400, width = 16, height = 8,device = "pdf")
