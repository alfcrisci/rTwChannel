#' main_data_channel
#'
#' @description Export result from  \code{channel_analytic} objects.
#'
#' @param  x  list  Output obtained by channel_analytic
#' @param  name  character Name of output files.
#' @param  native logical If channel stream have only native tweets.
#' @param  corpus logical export csv files.
#' @param  users logical export users data file.
#' @param  csv logical Default export is Excel files if csv also desired.
#' @param  filterdegree numeric Is the degree of filtering.
#' @param  graph_files logical  Requesting data of centrality measures.
#' @return Data from  \code{channel_analytic} object.
#' @author  Istituto per la Bioeconomia Firenze Italy  Alfonso Crisci \email{alfonso.crisci@@ibe.cnr.it}
#' @keywords  annotation
#'
#'
#' @importFrom XLConnect writeWorksheetToFile
#' @importFrom igraph  induced.subgraph degree betweenness closeness eigen_centrality get.data.frame write.graph
#' @importFrom networkD3 simpleNetwork
#' @importFrom htmlwidgets saveWidget
#' @export
#'
#'

main_data_channel=function(x,name,native=F,graph=T,corpus=F,users=T,data=T,csv=FALSE,filterdegree=0,graph_files=FALSE) 
   {
   name=ifelse( nchar(name)>8,substr(name, 1, 8),name)
   if ( native==F) {name=paste0(name,"_F")}
   if ( native==T) {name=paste0(name,"_N")}

  

  
  res=list()
  res[[1]]=channel_outputs(x,param="channel_stat",suffix_file=name, na_string="",filecsv =csv)
  res[[2]]=channel_outputs(x,param="daily_stat",suffix_file=name,filecsv =csv)
  res[[3]]=channel_outputs(x,param="table_hash",suffix_file=name,filecsv =csv)
  res[[4]]=channel_outputs(x,param="table_mentions",suffix_file=name,filecsv =csv)
  res[[5]]=channel_outputs(x,param="table_authors",suffix_file=name,filecsv =csv)
  res[[6]]=channel_outputs(x,param="table_links",suffix_file=name,filecsv =csv)
  res[[7]]=channel_outputs(x,param="favorited_authors",suffix_file=name,filecsv =csv)
  res[[8]]=channel_outputs(x,param="retweeted_authors",suffix_file=name,filecsv =csv)
  res[[9]]=NULL
  res[[10]]=NULL
  
  if ( native ==FALSE) {
    res[[9]]=channel_outputs(x,param="retweeted_authors",suffix_file=name,filecsv =csv)
    res[[10]]=channel_outputs(x,param="table_authors_retweeter",suffix_file=name,filecsv =csv)
  }
  
  res[[11]]=NULL
  if ( corpus == T) {
    res[[11]]=channel_outputs(x,param="word_freq_matr",suffix_file=name,filecsv=csv)
  }
  res[[12]]=NULL
  if ( data==T) {                   
                writeWorksheetToFile(file=paste0("data_channel_",name,".xls"), x$channel_data, sheet=paste("data",name))
                
                }
 
   if ( users==T) {
                  writeWorksheetToFile(file=paste0("users_channel_",name,".xls"), x$users_data, sheet=paste("users",name))
                  res[[12]]=x$users_data
                  }
   res[[13]]=NULL
   res[[14]]=NULL


  if ( graph == T) 
     {
    
    centrality_mentions=data.frame(users=names(igraph::degree(x$graph_mentions, mode='in')),
                                   indegree=as.numeric(igraph::degree(x$graph_mentions, mode='in')),
                                   outdegree=as.numeric(igraph::degree(x$graph_mentions, mode='out')),
                                   betweenness=as.numeric(igraph::betweenness(x$graph_mentions)),
                                   closeness=as.numeric(igraph::closeness(x$graph_mentions)),
                                   eigenvector=as.numeric(igraph::eigen_centrality(x$graph_mentions)$vector))
    res[[13]]=centrality_mentions
   
    if ( graph_files == T) 
        {
          writeWorksheetToFile(paste0("mentions_graph_centrality_",name,".xls"), data=centrality_mentions, sheet="mentions_g_par")
         }

    
    graph_mentions_codified_full=x$graph_mentions
    V(graph_mentions_codified_full)$size=degree(graph_mentions_codified_full)
    V(graph_mentions_codified_full)$indegree=centrality_mentions$indegree
    V(graph_mentions_codified_full)$outdegree=centrality_mentions$outdegree
    V(graph_mentions_codified_full)$betweenness=centrality_mentions$betweenness
    V(graph_mentions_codified_full)$eigenvector=centrality_mentions$indegree
    

    deg <- degree(graph_mentions_codified_full, mode = "in")
    idx <- names(which(deg > filterdegree))
    graph_mentions_codified_full <- induced.subgraph(graph_mentions_codified_full, idx)
    gd <- get.data.frame(graph_mentions_codified_full, what = "edges")
    a=simpleNetwork(gd, fontSize = 12)
    saveWidget(a,paste0("mention_graph_",name,".html"))
    write.graph(graph_mentions_codified_full, file =paste0("mention_graph_",name,".graphml"), format = "graphml")
    
    if ( native ==FALSE) {
      
      centrality_retweet=data.frame(users=names(degree(x$graph_retweet, mode='in')),
                                    indegree=as.numeric(degree(x$graph_retweet, mode='in')),
                                    outdegree=as.numeric(degree(x$graph_retweet, mode='out')),
                                    betweenness=as.numeric(betweenness(x$graph_retweet)),
                                    closeness=as.numeric(closeness(x$graph_retweet)),
                                    eigenvector=as.numeric(eigen_centrality(x$graph_retweet)$vector))
      res[[14]]=centrality_retweet
    
      if ( graph_files == T) 
        {
         writeWorksheetToFile(paste0("retweet_graph_centrality_",name,".xls"), data=centrality_retweet, sheet="retweet_g_par")
       }
      
      graph_retweet_codified_full=x$graph_retweet
      V(graph_retweet_codified_full)$size=degree(graph_retweet_codified_full)
      V(graph_retweet_codified_full)$indegree=centrality_retweet$indegree
      V(graph_retweet_codified_full)$outdegree=centrality_retweet$outdegree
      V(graph_retweet_codified_full)$betweenness=centrality_retweet$betweenness
      V(graph_retweet_codified_full)$eigenvector=centrality_retweet$indegree
      deg <- degree(graph_retweet_codified_full, mode = "in")
      idx <- names(which(deg > filterdegree))
      graph_retweet_codified_full <- induced.subgraph(graph_retweet_codified_full, idx)
      gd <- get.data.frame(graph_retweet_codified_full, what = "edges")
      a=simpleNetwork(gd, fontSize = 12)
      saveWidget(a,paste0("retweet_graph_",name,".html"))
      write.graph(graph_retweet_codified_full, file =paste0("retweet_graph_",name,".graphml"), format = "graphml")
      
    }
  }
  
  return(res)
  
}



