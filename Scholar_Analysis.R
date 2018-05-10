

# Load libraries
library(scholar)      # To request data from google scholar.
library(tidyverse)    # What do you do without?
library(igraph)       # To create a Network Object.
library(ggraph)       # To create the network visualization.

# Define the google scholar ids for several researchers
id <- 'uR-N1H0AAAAJ&hl'       # Naomi Wray
id <- 'WLaQnegAAAAJ&hl'       # Vincent Ranwez
id <- 'K8-EhrwAAAAJ&hl'       # Nicolas O Rode
id <- 'Ol-P2LQAAAAJ&hl'       # Sylvain Glémin
id <- 'o8gYsiIAAAAJ&hl'       # Sylvain Santoni
id <- 'KR7K-XEAAAAJ'          # John Mac Grath

# Get his profile and print his name
l <- get_profile(id)
name=l$name
tmp=strsplit(name, " ") %>% unlist()
last_name = tmp[length(tmp)]
last_name

# Get his citation history, i.e. citations to his work in a given year 
citation = get_citation_history(id)
citation %>% 
  ggplot( aes(x=year, y=cites)) + 
    geom_segment( aes(x=year, y=0, xend=year, yend=cites), color="grey") +
    geom_point( size=4) + 
    theme_bw()

# Get his publications (a large data frame)
data=get_publications(id)

# What are the involved Journals?
length(unique(tolower(data$journal)))
table(tolower(data$journal)) %>% as.data.frame() %>% filter(Freq>4) %>% arrange(Freq) %>% mutate(Var1=factor(Var1, Var1)) %>%
  ggplot(aes(x=Var1, y=Freq)) +
    geom_bar(stat="identity", width=0.5) +
    coord_flip() +
    xlab("") +
    theme_classic()

# Number of paper per year?
data %>% ggplot(aes(x=year)) + geom_bar()

# Number of publication
N_publication = nrow(data)
N_publication

# A function that return all the pair of a specific publication
return_all_pair=function(x){
  tmp = x  %>% gsub(", \\.\\.\\.", "", .) %>% strsplit(", ")  %>% unlist()
  if(length(tmp)>1){
    tmp = t(combn(tmp, 2))
  }
  return(tmp)
}

# Make a dataframe with all the connection between author?
list_of_pairs = lapply( data$author, return_all_pair ) 
# Concatenate all these list in a data frame
connect = do.call(rbind, list_of_pairs) %>% unique()
colnames(connect)=c("from", "to")
nrow(connect)

# Delete the target author
connect = connect %>% 
  as.data.frame() %>% 
  filter( !grepl(last_name, from, ignore.case = TRUE) ) %>% 
  filter( !grepl(last_name, to, ignore.case = TRUE) )
nrow(connect)

# Number of different coauthor?
N_coauthor = c( as.character(connect$from), as.character(connect$to)) %>% unique() %>% length()
N_coauthor

# Top - coauthor?
c( as.character(connect$from), as.character(connect$to)) %>% table() %>% as.data.frame() %>% filter(Freq>20) %>% arrange(Freq) %>% mutate(Var1=factor(.,.)) %>%
  ggplot(aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", width=0.5) +
  coord_flip() +
  xlab("") +
  theme_classic() +
  ylab("Number of publication together")

# Make a data frame with caracteristics concerning the co authors?
coauth=data.frame(name=unique(c( as.character(connect$from), as.character(connect$to)) ))
#Average year of copublication with our target?
return_year=function(i){
  name=gsub(" ", "", i)
  myrows=grep(name, gsub(" ", "", data$author))
  val=mean(data$year[ myrows ], na.rm=T)
  return(val)
}
coauth$year = sapply(coauth$name, return_year)

# Plot the network?
mygraph <- graph_from_data_frame( connect, vertices=coauth)

# Find the number of connection per people
V(mygraph)$vertex_degree <-  degree(mygraph)
V(mygraph)$vertex_degree[V(mygraph)$vertex_degree<10]=0

# Find the community?
wc <- cluster_edge_betweenness(mygraph)
modularity(wc)
membership(wc)
plot(wc, mygraph)

# Make the graph
p=ggraph(mygraph, layout="nicely") + 
  geom_edge_density(edge_fill="skyblue") +
  geom_edge_link(edge_colour="black", edge_alpha=0.2, edge_width=0.3) +
  geom_node_point(aes(colour=year), size=0.1, alpha=1 ) +
  geom_node_label( aes(label=name, filter=vertex_degree>35 , fill=year), size=5.6) +
  scale_size_continuous( range = c(0,10) ) +
  scale_colour_distiller(palette="BuPu") +
  theme_void() +
  #ggtitle(paste(l$name, " | # of coauthor: ", N_coauthor, " | # of publication: ", N_publication, sep="")) +
  theme(
    legend.position="none",
    plot.margin=unit(c(0,0,0,0), "null"),
    panel.spacing=unit(c(0,0,0,0), "null")
  )


# Save it as png
png(file=paste("~/Dropbox/SCHOLAR_ANALYSIS/",last_name,"_Network.png", sep=""), width=1500, height=1500)
p
dev.off()


