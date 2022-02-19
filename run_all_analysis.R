library(plyr)
library(dplyr)
library(readr)
library(tcltk)
library(readxl)
library(magrittr)
library(stringr)






read = function(name.query) 
  
{
  print("BEGIN READ.R")
  library(plyr)
  library(dplyr)
  library(readr)
  library(tcltk)
  library(readxl)
  library(magrittr)
  

  print(getwd())

  x <- read_excel(paste0(name.query,".xlsx"))
  print("STEP0")
  print("File:")
  print(paste0(name.query,".xlsx"))
  print(x)
  x = select(x, plate, row, col, array, ctrl, expr) 
  print("STEP1")
  print(x)
  #print("Please select file REMOVE.xlsx")
  list_f <- list.files(getwd())#
  for (fi in list_f) {
	if(grepl("Remove",fi)){
	     print(fi)
             x = read_excel(fi) %>% mutate(remove = 1) %>% left_join(x, .)
	}
  }

  #x = tk_choose.files() %>% read_excel %>% mutate(remove = 1) %>% left_join(x, .)
  print("STEP2")
  x = x %>% filter(!is.na(array), is.na(remove)) %>% unique
  print(x)

  x = tibble(Query = name.query, Array = x$array, Exp = x$expr, Ctl = x$ctrl)
  
  print("END READ.R")
  return(x)
  
}



bind = function()
  
{
  
  library(plyr)
  library(dplyr)
  library(readr)
  library(tcltk)
  library(readxl)
  library(magrittr)
  
  print("Please choose all the CSV files previously generated (in the results folder)")
  x = tk_choose.files() %>% ldply(read_csv)
  
  x = arrange(x, Query, Array, Exp, Ctl)
  
  return(x)
  
}




analyse = function()
  
{
  
  library(plyr)
  library(dplyr)
  library(readr)
  library(tcltk)
  library(readxl)
  library(magrittr)
  
  #print("Please select library file previously generated ")
  x = read_csv(paste0(getwd(),"/library.csv"))
  
  x = ddply(x, 'Query', transform, Query.smf = median(Exp))
  
  x = ddply(x, 'Array', transform, Array.smf = median(Ctl))
  
  global.median = select(x, Exp, Ctl) %>% unlist %>% median
  
  x$Query.smf = as.numeric(x$Query.smf)/as.numeric(global.median)
  
  x$Array.smf = as.numeric(x$Array.smf)/as.numeric(global.median)
  
  x$final.dmf = as.numeric(x$Exp)/as.numeric(global.median)
  
  x$log2FoldChange = log2(x$final.dmf/(x$Query.smf*x$Array.smf))
  
  x = ddply(x, c('Query', 'Array'), transform, pvalue = 2*pnorm(-abs(mean(log2FoldChange)/(sd(log2FoldChange)/length(log2FoldChange)))) )
  
  return(x)
  
}






setwd("/media/sookie/Storage/Collaborations/Marcin/LAST_SGA_folders/")
print("Please select the folder with the data")


setwd(tk_choose.dir())
#source('../../scripts/read.R')
#source('../../scripts/bind.R')
#source('../../scripts/analyse.R')


print(getwd())

list_files <- list.files(getwd())

for (file in list_files) {
	if(grepl(".xls",file)){
		if(!grepl("Remove",file)){
			print(file)
			f <- str_split(file,".xlsx")
			f <- f[[1]][1]
			print(f)
			write.csv(read(f), paste0(paste0("results/",f),'.csv'), row.names = FALSE)
		}
	}
}

print("CHECK!!!")
print(getwd())


write.csv(bind(), paste0(getwd(),'/results/library.csv'), row.names = FALSE)

############################################ FROM HERE 

print("Please select the results folder!!")
setwd(tk_choose.dir("/media/sookie/Storage/Collaborations/Marcin/LAST_SGA_folders/"))
print(getwd())

write.csv(analyse(), paste0(getwd(),'/result.csv'), row.names = FALSE)

########

#print("Please select 'result.csv' file")
x = read_csv("result.csv") %>% filter(pvalue < 0.05) %>% ## %>% -> reads it and filter for significant interactions
ddply(c('Query', 'Array'), summarise, mean = mean(log2FoldChange), stdv = sd(log2FoldChange)) %>% 
write_csv(paste0(getwd(),'/signif.csv')) ## -> calculates mean and standard deviation of each query-array interaction and saves as signif.csv


#######
#print("Please select directory 'results'")
#setwd(tk_choose.dir())
#print("Please select results.csv")
x = read_csv("result.csv") %>%
ddply(c('Query', 'Array'), summarise, mean = mean(log2FoldChange), stdv = sd(log2FoldChange), pvalue = unique(pvalue))
x$qvalue = p.adjust(x$pvalue, 'bonferroni')
### table(ifelse(x$qvalue < 0.001 & abs(x$mean) > log2(2), 1, 0))

#x = subset(x, x$qvalue < 0.001 & abs(x$mean) > log2(2))
x = x[!(x$Query == x$Array),]

write.csv(x, 'all_interactions_result.csv', row.names = FALSE)

x1 = subset(x, x$qvalue < 0.001 & abs(x$mean) > log2(0.5))
x2 = subset(x, x$qvalue < 0.001 & abs(x$mean) > log2(1))
x3 = subset(x, x$qvalue < 0.001 & abs(x$mean) > log2(2))


x4 = subset(x, x$qvalue < 0.01 & abs(x$mean) > log2(0.5))
x5 = subset(x, x$qvalue < 0.01 & abs(x$mean) > log2(1))
x6 = subset(x, x$qvalue < 0.01 & abs(x$mean) > log2(2))

x7 = subset(x, x$qvalue < 0.05 & abs(x$mean) > log2(0.5))
x8 = subset(x, x$qvalue < 0.05 & abs(x$mean) > log2(1))
x9 = subset(x, x$qvalue < 0.05 & abs(x$mean) > log2(2))



write.csv(x1, 'result_subset_qvalue0001_LFC15.csv', row.names = FALSE)
write.csv(x2, 'result_subset_qvalue0001_LFC2.csv', row.names = FALSE)
write.csv(x3, 'result_subset_qvalue0001_LFC4.csv', row.names = FALSE)

write.csv(x4, 'result_subset_qvalue001_LFC15.csv', row.names = FALSE)
write.csv(x5, 'result_subset_qvalue001_LFC2.csv', row.names = FALSE)
write.csv(x6, 'result_subset_qvalue001_LFC4.csv', row.names = FALSE)


write.csv(x7, 'result_subset_qvalue005_LFC15.csv', row.names = FALSE)
write.csv(x8, 'result_subset_qvalue005_LFC2.csv', row.names = FALSE)
write.csv(x9, 'result_subset_qvalue005_LFC4.csv', row.names = FALSE)

##################################################
## FOR SNR17A & B
x = read_csv("result.csv") %>%
ddply(c('Query', 'Array'), summarise, mean = mean(log2FoldChange), stdv = sd(log2FoldChange), pvalue = unique(pvalue))
x$qvalue = p.adjust(x$pvalue, 'bonferroni')

x10 = subset(x, x$qvalue <= 0.1 & abs(x$mean) > log2(1))
x11 = subset(x, x$qvalue <= 0.01 & abs(x$mean) > log2(1))

write.csv(x10, 'result_subset_qvalue_01_FC2.csv', row.names = FALSE)
write.csv(x11, 'result_subset_qvalue_001_FC2.csv', row.names = FALSE)


##################################################




library(visNetwork)  ### create an interactive network
library(igraph)      ### create an interactive network
library(stringr)



#wd <- "/media/sookie/Storage/Collaborations/Marcin/SGA_folders/FOR_MARCIN/all_SGA_Gly30_31queries/results/"


#setwd(wd)
wd <- getwd()
print(wd)

list_files <- list.files(getwd())

for (file in list_files) {
	if(grepl("result_subset",file)){
		print(file)
		f <- str_split(file,".csv")
		f <- f[[1]][1]
                f <- str_split(f,"result_subset_")
		f <- f[[1]][2]
                print(f)
		dat <- read.csv(file, stringsAsFactors=FALSE)
   
		### colours to represent positive/negative interactions
		type <- c('-1'='lightgray', '1'='coral')

		alpha      <- 0.05  ### p-value cutoff
		node.size  <- 5      ### maximum node size
		edge.width <- 5      ### maximum edge width


		#dat <- read.csv('result_subset.csv', stringsAsFactors=FALSE)


		dat <- dat[abs(dat[, 5]) <= alpha, c(1, 2, 3)]#[1:150, ]

		dat <- data.frame(dat, type=dat[, 3]/abs(dat[, 3]))

		dat[, 3] <- abs(dat[, 3]); r <- range(dat[, 3])
		dat[, 3] <- (dat[, 3]-r[1])/(r[2]-r[1])
		dat[, 3] <- dat[, 3]*edge.width +1

		dat[, 4] <- type[match(as.character(dat[, 4]), names(type))]


		########################################
		########################################
		###  create an igraph object and calculate
		###  pagerank statistics to represent
		###  quality and quantity of links

		g <- graph_from_data_frame(dat[, 1:2], directed=FALSE)

		size <- page.rank(g, weights=dat[, 3]); r <- range(size$vector)
		size <- (size$vector-r[1])/(r[2]-r[1]); size <- size*node.size

		########################################
		########################################
		###  create a D3 object and combine with
		###  node size and edge width

		g <- toVisNetworkData(g)

		title <- paste("<p><b>", g$nodes$id, sep='')

		g$nodes <- data.frame(g$nodes, value=size, title=title)
		g$edges <- data.frame(g$edges, width=dat[, 3])
		g$edges <- data.frame(g$edges, color=dat[, 4])

		g <- visNetwork(
		  nodes = g$nodes,
		  edges = g$edges,
		  width = 1200,
		  height= 1200
		) %>%

		  visInteraction(dragNodes = FALSE, dragView = FALSE, zoomView = FALSE) %>%

		  
		  visEdges(
		    smooth = FALSE
		  ) %>%
		  
		  visOptions(
		    highlightNearest = TRUE,
		    nodesIdSelection = list(enabled = TRUE,
				            values = g$edges$from,
				            style = 'width: 200px; height: 26px;
				            background: #f8f8f8;
				            font-family: arial;
				            color: gray;
				            border:none;
				            outline:none;')
		  ) %>%
		  
		  visPhysics(solver =  'forceAtlas2Based')

		# freeze network
		#visNetwork(g$nodes, g$edges) %>%
		 #visInteraction(dragNodes = FALSE, dragView = FALSE, zoomView = FALSE)


		# gephi json file
		#visNetwork(g, gephi = 'test.json') %>% visPhysics(stabilization = FALSE,   barnesHut = list(
		#    gravitationalConstant = -10000,
		#    springConstant = 0.002,
		#    springLength = 150
		#  ))


		visSave(g, file=paste0(paste0("../network/sga_network_",f),".html"))


		########################################
		########################################   REPULSION SOLVER
		###  create an igraph object and calculate
		###  pagerank statistics to represent
		###  quality and quantity of links

		g <- graph_from_data_frame(dat[, 1:2], directed=FALSE)

		size <- page.rank(g, weights=dat[, 3]); r <- range(size$vector)
		size <- (size$vector-r[1])/(r[2]-r[1]); size <- size*node.size

		########################################
		########################################
		###  create a D3 object and combine with
		###  node size and edge width

		g <- toVisNetworkData(g)

		title <- paste("<p><b>", g$nodes$id, sep='')

		g$nodes <- data.frame(g$nodes, value=size, title=title)
		g$edges <- data.frame(g$edges, width=dat[, 3])
		g$edges <- data.frame(g$edges, color=dat[, 4])

		g <- visNetwork(
		  nodes = g$nodes,
		  edges = g$edges,
		  width = 1200,
		  height= 1200
		) %>%

		  visInteraction(dragNodes = FALSE, dragView = FALSE, zoomView = FALSE) %>%

		  
		  visEdges(
		    smooth = FALSE
		  ) %>%
		  
		  visOptions(
		    highlightNearest = TRUE,
		    nodesIdSelection = list(enabled = TRUE,
				            values = g$edges$from,
				            style = 'width: 200px; height: 26px;
				            background: #f8f8f8;
				            font-family: arial;
				            color: gray;
				            border:none;
				            outline:none;')
		  ) %>%
		  
		  visPhysics(solver =  'repulsion')

		# freeze network
		#visNetwork(g$nodes, g$edges) %>%
		 #visInteraction(dragNodes = FALSE, dragView = FALSE, zoomView = FALSE)


		# gephi json file
		#visNetwork(g, gephi = 'test.json') %>% visPhysics(stabilization = FALSE,   barnesHut = list(
		#    gravitationalConstant = -10000,
		#    springConstant = 0.002,
		#    springLength = 150
		#  ))


		visSave(g, file=paste0(paste0("../network/repulsion_sga_network_",f),".html"))
	}
}


