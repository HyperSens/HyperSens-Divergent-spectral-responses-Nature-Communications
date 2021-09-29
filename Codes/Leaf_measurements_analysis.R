################################################################################################
################################################################################################

# The following code reproduces the statistical results in Figure 2e-j of the article:
# Title: Spectral fingerprints of plant pathogen infection diverge from abiotic signals
# Authors: Zarco-Tejada, P.J., Poblete, T., Camino, C., Gonzalez-Dugo, V, Calderon, R., Hornero, A., Hernandez-Clemente, R.,
# Román-Écija, M., Velasco-Amo, M.P., Landa, B.B., Beck, P.S.A., Saponari, M., Boscia, D., Navas-Cortes, J.A.

################################################################################################
################################################################################################

rm(list=ls())
if (!require('RColorBrewer')) { install.packages('RColorBrewer'); require('RColorBrewer') }  ### colors
if (!require('ggplot2')) { install.packages('ggplot2'); require('ggplot2') }  ### plot
if (!require('gridExtra')) { install.packages('gridExtra'); require('gridExtra') }  ### plot
if (!require('dplyr')) { install.packages('dplyr'); require('dplyr') }  ### databases
if (!require('cowplot')) { install.packages('cowplot'); require('cowplot') }  ### plot
if (!require('grid')) { install.packages('grid'); require('grid') }  ### plot

### Please select the leaf files in your  machine


################################################################################################
# 1. Read data at leaf level  () -------------------------------------------------------
################################################################################################

## read temperature measured with Dualex instrument (only midday hours)
data.T<-read.table('Leaf_Midday_temperature_Biotic-Xf-infected almond and olive trees.csv', head=T,sep="," )
print(paste('variable is:', names(data.T)[9]))
print(paste('n cases: ',dim(data.T)[1]))

## Read NPQI  measured with PolyPen instrument
data.npqi<-read.table('Leaf_NPQI_Biotic-Xf-infected  almond and olive trees.csv', head=T,sep="," )
print(paste('variable is:', names(data.npqi)[9]))
print(paste('n cases: ',dim(data.npqi)[1]))

## Read Anthocyanin content measured with Dualex instrument, filtered values > 0
data.anth<-read.table('Leaf_Anthocyanins_Biotic-Xf-infected almond and olive trees.csv', head=T,sep="," )
print(paste('variable is:', names(data.anth)[9]))
print(paste('n cases: ',dim(data.anth)[1]))

## Read Fluoresecen emission (Ft) measured with Flluorpen instrument
data.ft<-read.table('Leaf_ft_Biotic-Xf-infected almond and olive trees.csv', head=T,sep="," )
print(paste('variable is:', names(data.ft)[9]))
print(paste('n cases: ',dim(data.ft)[1]))

## Read PRIn measured with PolyPen instrument
data.prin<-read.table('Leaf_PRIn_Biotic-Xf-infected almond  almond and olive trees.csv', head=T,sep="," )
print(paste('variable is:', names(data.prin)[9]))
print(paste('n cases: ',dim(data.prin)[1]))

## read Chlorophyll content and temperature measured with Dualex instrument
data.cab<-read.table('Leaf_Chlorophyll_Biotic-Xf-infected  almond and olive trees.csv', head=T,sep="," )
print(paste('variable is:', names(data.cab)[9]))
print(paste('n cases: ',dim(data.cab)[1]))

### info classes in Olive: (Column: SEV_cond)
### Class : 1-H-O refers to : Olive Xf asympt
### Class: 3-Xf-I refers to : Olive Xf sympt.

### info classes in Almonds: (Column: SEV_cond)
### Class : 4-H refers to : Almond Xf healthy
### Class: 7-TP-Xf refers to : Almond Xf infected
### Class : 8-TP-Xf refers to : Almond Xf infected (Irrigated)

################################################################################################
# 2. Statistical test  () -------------------------------------------------------
################################################################################################

# Create a list of datasets
Datasets <- list(data.T,data.npqi, data.anth,data.ft,data.prin,data.cab)
Figure<-c('2e','2f','2g','2h','2i','2j')
########################################################################
############ Statistical test for leaf samples
########################################################################

for (i in 1:length(Datasets)){
  ### select the leaf dataframe
  data_i <- Datasets[[i]]
  #print(head(data_i))

  ### select the crop in each leaf dataset
  data.olives<-subset(data_i, orderData == '4P')
  data.almonds<-subset(data_i, orderData != '4P')
  data.crops <-list(data.olives,data.almonds)
  for (j in 1:length(data.crops)){
    data.crops.sub <- data.crops[[j]]
    variable <- names(data.crops.sub)[9]
    category <- names(data.crops.sub)[7]

    ########################################################################
    ############ Statistical test for olive samples
    ########################################################################
    if (j == 1){
      print('Statitical analysis for olive samples')
      print(paste(variable, 'parameter: ....Figure ',Figure[i]))
      # Anova
      model=lm(data.crops.sub[,variable] ~ data.crops.sub[,category] )
      ANOVA=aov(model)
      # Extract the residuals
      aov_residuals <- residuals(object = ANOVA )
      # Run Shapiro-Wilk test;  p-value > 0.05 implying , we can assume the normality.
      Shapiro.T<-shapiro.test(x = aov_residuals )
      # Run kruskal test;  p-value < 0.05 are significant differences between the treatment groups.
      K.test<-kruskal.test(data.crops.sub[,variable],data.crops.sub[,category], data = data.crops.sub)
      wilcox.test<-pairwise.wilcox.test(data.crops.sub[,variable], data.crops.sub[,category], p.adjust.method = "BH")
      print(paste('Olive Xf asympt. vs Olive Xf sympt. ---- p-value :',wilcox.test$p.value[1]))

    }
    ########################################################################
    ############ Statistical test for almond samples
    ########################################################################
    else {
      print('Statitical analysis for almond samples')
      print(paste(variable, 'parameter: ....Figure ',Figure[i]))
      # Anova
      model=lm(data.crops.sub[,variable] ~ data.crops.sub[,category] )
      ANOVA=aov(model)
      # Extract the residuals
      aov_residuals <- residuals(object = ANOVA )
      # Run Shapiro-Wilk test;  p-value > 0.05 implying , we can assume the normality.
      Shapiro.T<-shapiro.test(x = aov_residuals )
      # Run kruskal test;  p-value < 0.05 are significant differences between the treatment groups.
      K.test<-kruskal.test(data.crops.sub[,variable],data.crops.sub[,category], data = data.crops.sub)
      wilcox.test<-pairwise.wilcox.test(data.crops.sub[,variable], data.crops.sub[,category], p.adjust.method = "BH")
      print(paste('Almond Xf healthy vs Almond Xf infected ---- p-value :',wilcox.test$p.value[1]))
      print(paste('Almond Xf healthy vs Almond Xf infected (Irrigated) ---- p-value :',wilcox.test$p.value[2]))
      print(paste('Almond Xf infected vs Almond Xf infected (Irrigated) ---- p-value :',wilcox.test$p.value[4]))

      }
    print('........................')
  } #end loop j
} #end loop i


################################################################################################
# 3. Plots -------------------------------------------------------
################################################################################################

color.1 = brewer.pal(4, "Greens") ##Olive
color.2 = brewer.pal(4, "Blues")   ##Almond
color.3 = brewer.pal(3, "Greys")   ##Almond
color.4 = brewer.pal(9, "Reds")  ##Almond
color.d<-c(color.2[c(4,2)],color.4[7],color.4[5],color.4[3] )

################################################################################################
# 3.1 Boxplot Temp  (Figure 2.e)  -------------------------------------------------------
################################################################################################

data.T$temp_K<-data.T$temp+273.15

axis_y<-expression(bold('t (K)'['']))
axis_x<-expression(bold('SEV'))
# reorder(reorder(SEV_cond, orderDat
p_thermal<-ggplot(data.T,aes(x = orderData, y =temp_K ,fill=factor(SEV_cond))) +
  theme_bw()+
  geom_boxplot(width=0.4,outlier.size=-1,position=position_dodge(0.6))+#,preserve = "single"))+##position_dodge(0.6)) +
  scale_fill_manual(values = color.d[c(1:9)],name="",
                    breaks=c("1-H-O","3-Xf-I","4-H","7-TP-Xf","8-TP-Xf"),
                    labels=c("Olive Xf asympt.", "Olive Xf sympt.","Almond Xf healthy", "Almond Xf infected","Almond Xf infected (M_I)")) +
  ylab (axis_y) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 6, face = "bold", colour = 'Black'),
        axis.title.y = element_text(size=8, face="bold"),
        axis.text.y = element_text(size = 6, face = "bold",colour = 'Black'),
        legend.position = "none",
        legend.title = element_text(size = 9, face = "bold"),
        legend.text = element_text(size = 7, face = "bold"),
        legend.box.background = element_rect(colour = "black"))+
  labs(fill ="")+ geom_vline(xintercept = c(1.5)) +  guides(colour = "none")+
  annotate(geom="text", size=2.0,x=0.7, y=312, label=expression('e)'),color="black")+
  annotate(geom="text", size=2.0,x=0.9, y=307, label=expression(bold('a')),color="black")+
  annotate(geom="text", size=2.0,x=1.25, y=308, label=expression(bold('b')),color="black")+
  annotate(geom="text", size=2.0,x=1.85, y=311, label=expression(bold('a')),color="black")+
  annotate(geom="text",size=2.0, x=2.10, y=312, label=expression(bold('b')),color="black") +
  annotate(geom="text",size=2.0, x=2.3, y=309, label=expression(bold('c')),color="black") +
  scale_y_continuous(limits = c(300,313))+
  scale_x_discrete(breaks = c('4P','A-M'),expand = c(0.2,0.2),
                   label = (c('Olive','Almond')))

################################################################################################
# 3.2 Boxplot NPQI  (Figure 2.f)  -------------------------------------------------------
################################################################################################

axis_y<-expression(bold('NPQI'['']))
axis_x<-expression(bold('SEV'))
# reorder(reorder(SEV_cond, orderDat
p_npqi<-ggplot(data.npqi,aes(x = orderData, y =NPQI ,fill=factor(SEV_cond))) +
  theme_bw()+
  #geom_point(aes(color = factor(SEV)))    +
  geom_boxplot(width=0.4,outlier.size=-1,position=position_dodge(0.6))+#,preserve = "single"))+##position_dodge(0.6)) +
  scale_fill_manual(values = color.d[c(1:9)],name="",
                    breaks=c("1-H-O","3-Xf-I","4-H","7-TP-Xf","8-TP-Xf"),
                    labels=c("Olive Xf asympt.", "Olive Xf sympt.","Almond Xf healthy", "Almond Xf infected","Almond Xf infected (M_I)")) +
  ylab (axis_y) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 6, face = "bold", colour = 'Black'),
        axis.title.y = element_text(size=8, face="bold"),
        axis.text.y = element_text(size = 6, face = "bold",colour = 'Black'),
        legend.position = "none",
        legend.title = element_text(size = 9, face = "bold"),
        legend.text = element_text(size = 7, face = "bold"),
        legend.box.background = element_rect(colour = "black"))+
  labs(fill ="")+ geom_vline(xintercept = c(1.5)) +  guides(colour = "none")+
  annotate(geom="text", size=2.0,x=0.7, y=0.04, label=expression('f)'),color="black")+
  annotate(geom="text", size=2.0,x=0.90, y=0.01, label=expression(bold('a')),color="black")+
  annotate(geom="text", size=2.0,x=1.25, y=0.025, label=expression(bold('a')),color="black")+
  annotate(geom="text", size=2.0,x=1.85, y=0.007, label=expression(bold('a')),color="black")+
  annotate(geom="text", size=2.0,x=2.05, y=0.012, label=expression(bold('a')),color="black") +
  annotate(geom="text", size=2.0,x=2.3, y=0.009, label=expression(bold('b')),color="black") +
  scale_y_continuous(limits = c(-0.10,0.05))+
  scale_x_discrete(breaks = c('4P','A-M'),expand = c(0.2,0.2),
                   label = (c('Olive','Almond')))

################################################################################################
# 3.3. Boxplot Anth   (Figure 2.g)  -------------------------------------------------------
################################################################################################

axis_y<-expression(bold('A'['nth']))
axis_x<-expression(bold('SEV'))
# reorder(reorder(SEV_cond, orderDat
p_anth<-ggplot(data.anth,aes(x = orderData, y =Anth ,fill=factor(SEV_cond))) +
  theme_bw()+
  #geom_point(aes(color = factor(SEV)))    +
  geom_boxplot(width=0.4,outlier.size=-1,position=position_dodge(0.6))+#,preserve = "single"))+##position_dodge(0.6)) +
  scale_fill_manual(values = color.d[c(1:9)],name="",
                    breaks=c("1-H-O","3-Xf-I","4-H","7-TP-Xf","8-TP-Xf"),
                    labels=c("Olive Xf asympt.", "Olive Xf sympt.","Almond Xf healthy", "Almond Xf infected","Almond Xf infected (M_I)")) +
  ylab (axis_y) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 6, face = "bold", colour = 'Black'),
        axis.title.y = element_text(size=8, face="bold"),
        axis.text.y = element_text(size = 6, face = "bold",colour = 'Black',  family="Arial"),
        legend.position = "none",

        legend.title = element_text(size = 9, face = "bold"),
        legend.text = element_text(size = 8, face = "bold"),
        legend.box.background = element_rect(colour = "black"))+
  labs(fill ="")+ geom_vline(xintercept = c(1.5)) +  guides(colour = "none")+
  annotate(geom="text", size=2.0,x=0.7, y=0.17, label=expression('g)'),color="black")+
  annotate(geom="text", size=2.0,x=0.85, y=0.04, label=expression(bold('a')),color="black")+
  annotate(geom="text", size=2.0,x=1.2, y=0.12, label=expression(bold('b')),color="black")+
  annotate(geom="text", size=2.0,x=1.80, y=0.09, label=expression(bold('a')),color="black")+
  annotate(geom="text", size=2.0,x=2.05, y=0.17, label=expression(bold('b')),color="black") +
  annotate(geom="text", size=2.0,x=2.25, y=0.17, label=expression(bold('b')),color="black") +
  scale_y_continuous(limits = c(0,0.18))+
  scale_x_discrete(breaks = c('4P','A-M'),expand = c(0.2,0.2),
                   label = (c('Olive','Almond')))

################################################################################################
# 3.4.  Boxplot Ft (Figure 2.h) -------------------------------------------------------
################################################################################################

axis_y<-expression(bold('F'['t']))
axis_x<-expression(bold('SEV'))
# reorder(reorder(SEV_cond, orderDat
p_ft<-ggplot(data.ft,aes(x = orderData, y =Ft ,fill=factor(SEV_cond))) +
  theme_bw()+
  #geom_point(aes(color = factor(SEV)))    +
  geom_boxplot(width=0.4,outlier.size=-1,position=position_dodge(0.6))+##position_dodge(0.6,preserve = "single")) +
  #scale_color_manual(values = color.d)+
  scale_fill_manual(values = color.d[c(1:5)],name="",
                    breaks=c("1-H-O","3-Xf-I","4-H","7-TP-Xf","8-TP-Xf"),
                    labels=c("Olive Xf asympt.", "Olive Xf sympt.","Almond Xf healthy", "Almond Xf infected","Almond Xf infected (M_I)")) +
  ylab (axis_y) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 6, face = "bold", colour = 'Black'),
        axis.title.y = element_text(size=8, face="bold"),
        axis.text.y = element_text(size = 6, face = "bold",colour = 'Black'),
        legend.position = "none",
        legend.title = element_text(size = 9, face = "bold"),
        legend.text = element_text(size = 7, face = "bold"),
        legend.box.background = element_rect(colour = "black"))+
  labs(fill ="")+ geom_vline(xintercept = c(1.5)) +  guides(colour = "none")+
  #plot.background = element_rect(fill = "white", colour = 'black', size = 1))  +
  scale_y_continuous(limits = c(0,3500)) +
  annotate(geom="text", size=2.0,x=0.7, y=3300, label=expression('h)'),color="black")+
  annotate(geom="text", size=2.0,x=0.9, y=3000, label=expression(bold('a')), color="black")+
  annotate(geom="text", size=2.0,x=1.2, y=2200, label=expression(bold('b')), color="black")+
  annotate(geom="text", size=2.0,x=1.80, y=2750, label=expression(bold('a')),color="black")+
  annotate(geom="text", size=2.0,x=2.10, y=3200, label=expression(bold('b')),color="black")+
  annotate(geom="text", size=2.0,x=2.2, y=2900, label=expression(bold('c')),color="black")+

  scale_x_discrete(breaks = c('4P','A-M'),expand = c(0.2,0.2),
                   label = (c('Olive','Almond')))

################################################################################################
# 3.5 Boxplot PRIn (Figure 2.i)  -------------------------------------------------------
################################################################################################

axis_y<-expression(bold('PRI'['n']))
axis_x<-expression(bold('SEV'))
# reorder(reorder(SEV_cond, orderDat
p_prin<-ggplot(data.prin,aes(x = orderData, y =PRIn ,fill=factor(SEV_cond))) +
  theme_bw()+
  geom_boxplot(width=0.4,outlier.size=-1,position=position_dodge(0.8))+#,preserve = "single"))+##position_dodge(0.6)) +
  scale_fill_manual(values = color.d[c(1:9)],name="",
                    breaks=c("1-H-O","3-Xf-I","4-H","7-TP-Xf","8-TP-Xf"),
                    labels=c("Olive Xf asympt.", "Olive Xf sympt.","Almond Xf healthy", "Almond Xf infected","Almond Xf infected (M_I)")) +
  ylab (axis_y) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 6, face = "bold", colour = 'Black'),
        axis.title.y = element_text(size=8, face="bold"),
        axis.text.y = element_text(size = 6, face = "bold",colour = 'Black'),
        legend.position = "none",
        legend.title = element_text(size = 9, face = "bold"),
        legend.text = element_text(size = 7, face = "bold"),
        legend.box.background = element_rect(colour = "black"))+
  labs(fill ="")+ geom_vline(xintercept = c(1.5)) +  guides(colour = "none")+
  annotate(geom="text", size=2.0,x=0.7, y=0.04, label=expression('i)'),color="black")+
  annotate(geom="text", size=2.0,x=0.85, y=0.030, label=expression(bold('a')),color="black")+
  annotate(geom="text", size=2.0,x=1.3, y=0.035, label=expression(bold('b')),color="black")+
  annotate(geom="text", size=2.0,x=1.80, y=0.005, label=expression(bold('a')),color="black")+
  annotate(geom="text", size=2.0,x=2.10, y=0.02, label=expression(bold('b')),color="black") +
  annotate(geom="text", size=2.0,x=2.35, y=0.02, label=expression(bold('b')),color="black") +
  scale_y_continuous(limits = c(-0.05,0.05))+
  scale_x_discrete(breaks = c('4P','A-M'),expand = c(0.2,0.2),
                   label = (c('Olive','Almond')))

################################################################################################
# 3.6 Boxplot Chl  (Figure 2.j)  -------------------------------------------------------
################################################################################################

axis_y<-expression(bold('C'['a+b']))
axis_x<-expression(bold('SEV'))
# reorder(reorder(SEV_cond, orderDat
p_cab<-ggplot(data.cab,aes(x = orderData, y =Chl ,fill=factor(SEV_cond))) +
  theme_bw()+
  #geom_point(aes(color = factor(SEV)))    +
  geom_boxplot(width=0.4,outlier.size=-1,position=position_dodge(0.6))+#,preserve = "single"))+##position_dodge(0.6)) +
  scale_fill_manual(values = color.d[c(1:9)],name="",
                    breaks=c("1-H-O","3-Xf-I","4-H","7-TP-Xf","8-TP-Xf"),
                    labels=c("Olive Xf asympt.", "Olive Xf sympt.","Almond Xf healthy", "Almond Xf infected","Almond Xf infected (M_I)")) +
  ylab (axis_y) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 6, face = "bold", colour = 'Black'),
        axis.title.y = element_text(size=8, face="bold"),
        axis.text.y = element_text(size = 6, face = "bold",colour = 'Black'),
        legend.position = "none",
        legend.title = element_text(size = 9, face = "bold"),
        legend.text = element_text(size = 7, face = "bold"),
        legend.box.background = element_rect(colour = "black"))+
  labs(fill ="")+ geom_vline(xintercept = c(1.5)) +  guides(colour = "none")+
  annotate(geom="text", size=2.0,x=0.7, y=65, label=expression('j)'),color="black")+
  annotate(geom="text", size=2.0,x=0.9, y=60, label=expression(bold('a')),color="black")+
  annotate(geom="text", size=2.0,x=1.25, y=65, label=expression(bold('a')),color="black")+
  annotate(geom="text", size=2.0,x=1.85, y=47, label=expression(bold('a')),color="black")+
  annotate(geom="text", size=2.0,x=2.10, y=45, label=expression(bold('b')),color="black") +
  annotate(geom="text", size=2.0,x=2.30, y=40, label=expression(bold('b')),color="black") +
  scale_y_continuous(limits = c(0,70))+
  scale_x_discrete(breaks = c('4P','A-M'),expand = c(0.2,0.2),
                   label = (c('Olive','Almond')))

################################################################################################
# 4. Figure Leaf sample (Fig 2.e-2.j)  -------------------------------------------------------
################################################################################################


# Apply user-defined function to extract legend
gglegend<-ggplot(data.T,aes(x = orderData, y =temp_K ,fill=factor(SEV_cond))) +
  theme_bw()+
  geom_boxplot(width=0.4,outlier.size=-1,position=position_dodge(0.6)) +
  #scale_color_manual(values = color.d)+
  scale_fill_manual(values = color.d[c(1:9)],name="",
                    breaks=c("1-H-O","3-Xf-I","4-H","7-TP-Xf", "8-TP-Xf"),
                    labels=c(expression(bold('Olive ')~bold(italic(Xf))~bold(' asympt.')),
                            expression(bold('Olive ')~bold(italic(Xf))~bold(' sympt.')),
                             expression(bold('Almond ')~bold(italic(Xf))~bold(' healthy')),
                             expression(bold('Almond ')~bold(italic(Xf))~bold(' infected')),
                             expression(bold('Almond ')~bold(italic(Xf))~bold(' infected (irrigated)')) ))+

  ylab (axis_y)+
  theme(legend.position = "bottom",
        legend.background = element_rect(fill="white"),
        legend.key=element_blank(),
        legend.title = element_text(size = 5, face = "bold"),
        legend.text = element_text(size = 4, face = "bold"),
        legend.box.background = element_rect(colour = "white"))+
  labs(fill ="")



legend <- get_legend(gglegend +
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom"))

prow<-plot_grid(p_thermal,p_npqi,p_anth,p_ft,p_prin,p_cab, ncol=2,align="hv")

# here you add the legend
Figure_leaf <- plot_grid( prow, legend, nrow=2,rel_heights = c(5, .4))

ggsave("Figure_2e_2j_NatureComunications.eps", plot=Figure_leaf,device='eps',
       units="in", width=4.3, height=3, dpi=300,limitsize = FALSE)

ggsave("Figure_2e_2j_NatureComunications.png", plot=Figure_leaf,
       units="in", width=4.3, height=3, dpi=300,limitsize = FALSE)
