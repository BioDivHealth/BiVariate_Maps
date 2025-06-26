rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) # Set the working directory to the directory in which the code is stored
td<-tempdir()
dir.create(td,showWarnings = FALSE)
#
#
###//\/\/\/\/\/\/\////\/\/\/\/\/\/\///\\\\\\//\/\/\/\///////////////////////////////##-#
##                        Example Plot for BiVariate maps                           ##-#  
###///\/\/\/\/\/\/\////\/\/\//\/\/\/\/\/\///\\\\\\\\\..\.\\\\.\\\\\\\\\\\\\\\><\\\\\##-#
#'
#'
# 0. Load the packages----
list.of.packages<-c("sf","terra")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages,require,character.only=TRUE)
rm(list.of.packages,new.packages)

# 0.2 Load the functions needed for the mapping ----
functions<-"./Functions" %>% list.files(recursive = FALSE,pattern = ".R$",full.names = TRUE)
lapply(functions,function(x) source(x))

# 1. Get some examples ----
# 1.a Create the exit directory ----
dir.create(paste(getwd(),"Figures",sep="/"),showWarnings = F,recursive = T)

# 1.b Run the examples ----
example_ramps<-list(a=c("red","green"),b=c("green4","yellow3"),c=c("gold","purple"),
                    d=c("yellow","#ff008dff"),e=c("skyblue","navy"),f=c("grey","black"))


png(paste("./Figures","RampsBiVar.png",sep="/"),width = 25,height = 25,units="cm",res=600)
  # Create the layout for the plots
  lt <- layout(matrix(c(rep(1,6),2:7),ncol=4,nrow=3,byrow = F),respect=T)

  # Create the color palette
  ExCol <- BiColor(n=4,display = F)
  
  par(mar=c(2.5,2.5,2.5,2.5)+3)
  biLegend(ExCol$hex_matrix,title="Bivariate color scale",cex.title=1.3,cex.vars = 1)
  
  # Plot the rest of the color gradients/ramps
  par(mar=c(2.5,2.5,2.5,2.5))
  
  for(i in 1:length(example_ramps)){
    ExCol <- BiColor(color01 = example_ramps[[i]][1],color02 = example_ramps[[i]][2], n=4, display = F)
    biLegend(ExCol$hex_matrix)
    
    mtext(side=3,adj=0,paste0(letters[i],")"),line=1,xpd=T)
  }

dev.off()

# 2. Load the some data to create bi-variate raster maps ----
# 2.a Get the spatial information ----
  dir.create("./Data/Bioclim",recursive = T,showWarnings = F) 
  dir.create("./Data/Border",recursive = T,showWarnings = F)

geodata::worldclim_country(country="gbr",var="bio",path="./Data/Bioclim")
c_pol <- geodata::gadm(country="gbr",level = 2,path="./Data/Border")

c_pol <- sf::st_as_sf(c_pol) 
g_pol <- c_pol %>% st_union()

BioClim <- rast(list.files("./Data/Bioclim",pattern=".tif$",recursive = T,full.names = T))

# 2.b Crop the environmental data to addapt to the selection ----
BioClim <- BioClim %>% crop(vect(g_pol)) %>% mask(vect(g_pol))

# 2.b Create the bivariate map ----
biMap <- biRaster(var1 = BioClim[[5]],var2 = BioClim[[6]],color01 = "orange",color02 = "purple",n=4)

# 2.c Display the information ----
# Composite map
png(paste("./Figures","CompMap.png",sep="/"),width = 25,height = 25,units="cm",res=600)
  lt<-layout(matrix(c(rep(1,3),2,rep(1,4),rep(rep(c(3,4),each=2),2)),ncol=4,nrow=4,byrow=F),respect=F)
  layout.show(lt)
    plot(biMap$raster$bi,col=levels(biMap$raster$bi)[[1]]$color,type="classes",legend=F,mar=c(1,1,1,1),axes=F)
    mtext(side=3,adj=0,"a)",cex=1.5,line=-2)
    
    par(mar=c(6,6,6,6))
    biLegend(biMap$color$hexMatrx)
    
    plot(biMap$raster$v1,col=levels(biMap$raster$v1)[[1]]$color,type="classes",legend=F,mar=c(1,1,1,1),axes=F)
    mtext(side=3,adj=0,"b)",cex=1.5,line=-2)
    
    plot(biMap$raster$v2,col=levels(biMap$raster$v2)[[1]]$color,type="classes",legend=F,mar=c(1,1,1,1),axes=F)
    mtext(side=3,adj=0,"c)",cex=1.5,line=-2)

dev.off()

# Simple map
png(paste("./Figures","BiVarMap.png",sep="/"),width = 15,height = 25,units="cm",res=600)
  lt<-layout(matrix(c(rep(1,3),2,rep(1,4)),ncol=2,nrow=4,byrow=F),respect=F)
  layout.show(lt)

  plot(biMap$raster$bi,col=levels(biMap$raster$bi)[[1]]$color,type="classes",legend=F,mar=c(1,1,1,1),axes=F)
  mtext(side=3,adj=0,"a)",cex=1.5,line=-2)
  
  par(mar=c(2,2,2,2)+3)
  biLegend(biMap$color$hexMatrx,var.name1 = "Max Temap",var.name2 = "Min Temp",title="Legend")

dev.off()

# 2.d Build a Bivariate map from polygon data
Var1 <-terra::extract(BioClim[[5]],c_pol %>% vect(),fun=mean,na.rm=T)
Var2 <-terra::extract(BioClim[[6]],c_pol %>% vect(),fun=mean,na.rm=T)

c_pol <- cbind(c_pol,cbind(Var1$wc2.1_30s_bio_5,Var2$wc2.1_30s_bio_6))

BiVect <- biPol(c_pol,var1="X1",var2="X2",n=4,color01="gold",color02="magenta3")

# 2.e Create some example plots
# Composite map
png(paste("./Figures","CompVect.png",sep="/"),width = 25,height = 25,units="cm",res=600)
lt<-layout(matrix(c(rep(1,3),2,rep(1,4),rep(rep(c(3,4),each=2),2)),ncol=4,nrow=4,byrow=F),respect=F)
layout.show(lt)
  par(mar=c(0,0,0,0))
  plot(BiVect$spatial %>% st_geometry(),col=BiVect$spatial$BivarCol,mar=c(1,1,1,1),axes=F)
  mtext(side=3,adj=0,"a)",cex=1.5,line=-2)
  
  par(mar=c(6,6,6,6))
  biLegend(BiVect$color$hexMatrx)
  
  par(mar=c(0,0,0,0))
  plot(BiVect$spatial %>% st_geometry(),col=BiVect$spatial$col1,mar=c(1,1,1,1),axes=F)
  mtext(side=3,adj=0,"b)",cex=1.5,line=-2)
  
  par(mar=c(0,0,0,0))
  plot(BiVect$spatial %>% st_geometry(),col=BiVect$spatial$col2,mar=c(1,1,1,1),axes=F)
  mtext(side=3,adj=0,"c)",cex=1.5,line=-2)
  
dev.off()

# Simple map
png(paste("./Figures","BiVarMap.png",sep="/"),width = 15,height = 25,units="cm",res=600)
  lt<-layout(matrix(c(rep(1,3),2,rep(1,4)),ncol=2,nrow=4,byrow=F),respect=F)
  layout.show(lt)
  
    plot(BiVect$spatial %>% st_geometry(),col=BiVect$spatial$BivarCol,mar=c(1,1,1,1),axes=F)
    mtext(side=3,adj=0,"a)",cex=1.5,line=-2)
    
    par(mar=c(2,2,2,2)+3)
    biLegend(biMap$color$hexMatrx,var.name1 = "Max Temap",var.name2 = "Min Temp",title="Legend")
  
  dev.off()

# End of the script