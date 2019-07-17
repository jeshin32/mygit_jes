
library(stringr)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++------
# > 1. Utilities  ------------------------ ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++------

makeDirStructure <- function(HOME_PATH, folder_list) {
  #
  # Description: 
  #   HOME.PATH에 필요한 하이 폴더를 자동으로 만들어 주는 함수 
  #
  
  for (i in 1:length(folder_list)) {
    
    path <- paste0(HOME_PATH, folder_list[i], "/")
    path_constant_name <- gsub("/",  "_", paste0(folder_list[i], "_PATH"))
    assign(path_constant_name, path, envir = .GlobalEnv)
    
    if(!dir.exists(path)) {
      dir.create(path, recursive = T)
    }
    
    cat(path_constant_name, "is set to", path, "\n")
  }
}

printResult <- function(title, data=NULL, length = 130, timeYN = T) {
  #
  # Description: 
  #   중간중간 결과를 양식에 맞게 출력해수 주는 함ㅅ 
  #
  
  cat("\n", str_pad(paste0(title," : "), width=length, pad = "#", side="right"), "\n\n")
  
  if (!is.null(data)) {
    print(data) 
    cat("\n")
  }
}




#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++------
# > 2. Visualizations  ------------------------ ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++------

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
