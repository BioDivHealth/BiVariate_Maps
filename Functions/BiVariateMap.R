#
#
###//\/\/\/\/\/\/\////\/\/\/\/\/\/\///\\\\\\//\/\/\/\///////////////////////////////##-#
##                  Functions for the compuatation of Bivariate maps                ##-#  
###///\/\/\/\/\/\/\////\/\/\//\/\/\/\/\/\///\\\\\\\\\..\.\\\\.\\\\\\\\\\\\\\\><\\\\\##-#
#'
#
# Function to create the color scale
BiColor<- function(color01="red",color02="blue",n,cero.color="grey89",display=FALSE){
  
  # a. Set the two color_ramps
  fun1 <- colorRampPalette(c(cero.color,color01))
  fun2 <- colorRampPalette(c(cero.color,color02))
  
  col1 <- col2rgb(fun1(n))
  col2 <- col2rgb(fun2(n))
  
  # b. Extract the rgb values and get the possible combinations of the two color vectors into an ordered matrix
  # b.1 RGB matrix (red, greeen and blue color matrices)
  rgb_matrix <- list()
  
  for(i in 1:nrow(col1)){
    rgb_matrix[[i]]<-outer(col1[i,],col2[i,], function(x,y) (x+y)/2)# %>% rast()
    names(rgb_matrix)[i]<-rownames(col1)[i]
  }
  # b.2 Transform the RGB into hexadecimal color codes
  a <- rgb(rgb_matrix[[1]] %>% as.vector(),
           rgb_matrix[[2]] %>% as.vector(),
           rgb_matrix[[3]] %>% as.vector(),maxColorValue = 255) %>% matrix(nrow=n,byrow=F)
  
  # c. Check the color scale and gradients----
  if(display==T){
    image(x=1:ncol(a), y=1:nrow(a),matrix(1:prod(dim(a)),nrow = nrow(a),ncol=ncol(a)),
          col = as.vector(a), 
          axes = FALSE, 
          xlab = "", ylab = "", 
          asp = 1,xaxs = "i", yaxs = "i"  # 'i' = internal, no padding
    )
  }
  
  # d. return the information
  return(list(rgb_matrix=rgb_matrix,hex_matrix=a))
}

# 0.2 Function to classified the raster into homogeneous classes ----
VarClass <- function(x,n=n){
  if(class(x) %in% c("SpatRaster")) {
    xy <- values(x) %>% as.numeric()
  }else{
    xy <- x
  }
  
  breaks_n <- quantile(xy, probs = seq(0, 1, length.out = n + 1), na.rm = TRUE)
  
  # a.2 Add a small epsilon to the last break to ensure the max value is included
  breaks_n[length(breaks_n)] <- breaks_n[length(breaks_n)] + .Machine$double.eps
  
  # b. Create the labels for the different break points
  class_breaks <- cut(xy,breaks=breaks_n,right=T,include.lowest=T,ordered_result=TRUE)
  class_points <- as.numeric(as.factor(class_breaks))# %>% as.factor() %>% as.numeric() 
  
  return(list(cut_class=class_points,cut_levels=class_breaks))
}

# bivariate raster_maps ----
biRaster <- function(var1,var2,n,color01="gold",color02="magenta3",cero.color="grey89"){
  
  # 0. Load the Needed packages
  lapply(c("terra","sf"),require,character.only=TRUE)
  
  # a. Create the color matrix----
  cp <- BiColor(n=n,color01=color01,color02=color02,cero.color=cero.color,display = F)
  g <- cp$hex_matrix
  
  # b. Classify the raster variables and takes the color value from the matrix----
  xCutFull <- lapply(list(var1,var2),VarClass, n=n)
  
  # b.1 Unify the data----
  xCut <- cbind(xCutFull[[1]]$cut_class,xCutFull[[2]]$cut_class)
  colnames(xCut) <- c("var1","var2")
  
  biCol <- lapply(1:nrow(xCut), function(j) y <- g[xCut[j,1],xCut[j,2]])
  biCol <-do.call("c",biCol)
  
  xCut <- cbind(xCut,as.factor(biCol))
  
  # Create the color matrices for the raster mapping
  m.col <- list(v1 = matrix(g[xCut[,1],1],ncol=ncol(var1),nrow=nrow(var1),byrow=T),
                v2 = matrix(g[1,xCut[,2]],ncol=ncol(var1),nrow=nrow(var1),byrow=T),
                bi = matrix(biCol,ncol=ncol(var1),nrow=nrow(var1),byrow=T)
  )
  
  # c. Create the raster objects----
  r.cols <- list()
  
  for(k in 1:length(m.col)){
    
    if(names(m.col)[k]=="bi"){
      yp <- NA
      
    }else{
      yp <- levels(xCutFull[[k]]$cut_levels) 
      
    }
    
    fm <- factor(m.col[[k]])
    r <- rast(matrix(as.integer(fm),ncol=ncol(m.col[[k]]),nrow=nrow(m.col[[k]])),crs=crs(var1))
    levels(r) <- data.frame(value = seq_along(levels(fm)),
                            color = levels(fm),
                            breaks = yp)
    r.cols[[k]]<-r
    names(r.cols)[k] <- names(m.col)[k]
  }
  
  # d. Return the raster and color information
  return(list(raster=rast(r.cols),
              color=list(hexMatrx=g,rgbMatrix=cp$rgb_matrix)))
}

# Function to create a bivariate polygon map
biPol <- function(x,var1,var2,n,color01="gold",color02="magenta3",cero.color="grey89"){
  
  # 0. Load the Needed packages
  lapply(c("sf"),require,character.only=TRUE)
  
  # a. Create the color matrix----
  cp <- BiColor(n=n,color01=color01,color02=color02,cero.color=cero.color,display = F)
  g <- cp$hex_matrix
  
  # b. Classify the raster variables and takes the color value from the matrix----
  xy <- x[,names(x) %in% c(var1,var2)] %>% st_drop_geometry()
  xCutFull <- lapply(xy,VarClass, n=n)
  
  # b.1 Unify the data----
  xCut <- cbind(xCutFull[[1]]$cut_class,xCutFull[[2]]$cut_class)
  colnames(xCut) <- c("var1","var2")
  
  xCut <- cbind(xCut,data.frame(col1=g[xCut[,1],1],col2=g[1,xCut[,2]]))
  
  biCol <- lapply(1:nrow(xCut), function(j) y <- g[xCut[j,1],xCut[j,2]])
  biCol <- do.call("c",biCol)
  
  xCut <- cbind(xCut,data.frame(BivarCol=biCol))
  
  # b.3 Format and Combine the data with the original spatial object
  y <- cbind(x,xCut)
  
  # d. Return the raster and color information
  return(list(spatial=y,
              color=list(hexMatrx=g,rgbMatrix=cp$rgb_matrix)))
}

# Legend for the biVariate maps ----
biLegend <- function(g,x.axis=0.45,var.name1="var1",y.axis=0.45,var.name2="var2",title=NULL,cex.title=0.8,cex.vars=0.8,details=TRUE,mar=c(2.5,2.5,2.5,2.5)){
  
  # a. Set the paramters
  par(xaxs = "i", yaxs = "i", pty = "s",mar=mar,xpd=T)
  
  # b. Display the color grid
  image(x=1:ncol(g), y=1:nrow(g),matrix(1:prod(dim(g)),nrow = nrow(g),ncol=ncol(g)),
        col = as.vector(g), 
        axes = FALSE, 
        xlab = "", ylab = "", 
        asp = 1,xaxs = "i", yaxs = "i"  # 'i' = internal, no padding
  )
  
  
  if(!is.null(title)){
    mtext(title,side=3,adj=0,font=2,line=1,xpd=T,cex=cex.title)
  }
  
  # Add the arrows to describe the variables
  if(!is.null(details)){
    usr <- par("usr")
    
    usr[c(1,3)]<-usr[c(1,3)] - x.axis
    usr[c(2,4)]<-usr[c(2,4)] - y.axis
    
    arrows(x0 = usr[1L], y0 = usr[3L],
           x1 = usr[1:2],y1 = usr[4:3],
           length = 0.1,angle = 20,xpd = TRUE,
           lwd=1
    )
    
    # Add the text
    # d. Simbols
    text(x=usr[c(2)]+0.35,y=usr[c(3)],"+",xpd=T,font=2,cex=2) 
    text(x=usr[c(1)],y=usr[c(4)]+0.35,"+",xpd=T,font=2,cex=2)
    
    # e. Variable names
    text(x=usr[c(1:2)] %>% mean(),y=usr[c(3)]-0.4,var.name1,xpd=T,font=2,cex=cex.vars)
    text(x=usr[c(1)]-0.4,y=usr[c(3:4)] %>% mean(),var.name2,xpd=T,font=2,cex=cex.vars,srt=90)
  }
}


