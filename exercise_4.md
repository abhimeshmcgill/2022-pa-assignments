Exercise_4
================

``` r
node_names <- tibble(
  id   = c(1,2,3,4,5,6,7,8,9,10),
  name = c("1","2","3","4","5","6","A","B","C","D")
)
node_names
```

    ## # A tibble: 10 x 2
    ##       id name 
    ##    <dbl> <chr>
    ##  1     1 1    
    ##  2     2 2    
    ##  3     3 3    
    ##  4     4 4    
    ##  5     5 5    
    ##  6     6 6    
    ##  7     7 A    
    ##  8     8 B    
    ##  9     9 C    
    ## 10    10 D

``` r
edge_list <- tibble(
  from = c(1,2,3,3,3,3,3,4,5,5,6,6,7,7,8,8,9),
  to   = c(2,7,4,5,8,9,10,9,6,10,8,10,8,9,9,10,10)
)
edge_list
```

    ## # A tibble: 17 x 2
    ##     from    to
    ##    <dbl> <dbl>
    ##  1     1     2
    ##  2     2     7
    ##  3     3     4
    ##  4     3     5
    ##  5     3     8
    ##  6     3     9
    ##  7     3    10
    ##  8     4     9
    ##  9     5     6
    ## 10     5    10
    ## 11     6     8
    ## 12     6    10
    ## 13     7     8
    ## 14     7     9
    ## 15     8     9
    ## 16     8    10
    ## 17     9    10

``` r
fakebook_graph <- tbl_graph(nodes = node_names, edges = edge_list, directed = FALSE)
fakebook_graph
```

    ## # A tbl_graph: 10 nodes and 17 edges
    ## #
    ## # An undirected simple graph with 1 component
    ## #
    ## # Node Data: 10 x 2 (active)
    ##      id name 
    ##   <dbl> <chr>
    ## 1     1 1    
    ## 2     2 2    
    ## 3     3 3    
    ## 4     4 4    
    ## 5     5 5    
    ## 6     6 6    
    ## # ... with 4 more rows
    ## #
    ## # Edge Data: 17 x 2
    ##    from    to
    ##   <int> <int>
    ## 1     1     2
    ## 2     2     7
    ## 3     3     4
    ## # ... with 14 more rows

``` r
fakebook_graph %>% 
    ggraph(layout = 'kk') + 
    geom_edge_link() + 
    geom_node_point(size = 8, colour = 'gray') +
    geom_node_text(aes(label = name), colour = 'steelblue', vjust = 0.4) + 
    ggtitle('fakebook bus network') + 
    theme_graph()
```

    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family not
    ## found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
    ## family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
    ## family not found in Windows font database

![](exercise_4_files/figure-gfm/plot-1.png)<!-- -->

``` r
fakebook_graph <- fakebook_graph %>% 
  activate(nodes) %>% 
  mutate(d_centrality = centrality_degree()) %>%  # adding measure of degree centrality
  mutate(b_centrality = centrality_betweenness()) # adding betweenness centrality
```

    ## Warning in betweenness(graph = graph, v = V(graph), directed = directed, :
    ## 'nobigint' is deprecated since igraph 1.3 and will be removed in igraph 1.4

``` r
fakebook_graph %>% 
  ggraph(layout = 'kk') + 
  geom_edge_link() + 
  geom_node_point(aes(size = d_centrality, colour = b_centrality)) + 
  scale_color_continuous(guide = 'legend') +
  geom_node_text(aes(label = name), colour = 'red', vjust = 1.6) + 
  ggtitle('Fakebook bus network') + 
  theme_graph()
```

    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family not
    ## found in Windows font database

    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family not
    ## found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
    ## family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
    ## family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
    ## family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
    ## family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
    ## family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
    ## family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
    ## family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
    ## family not found in Windows font database

![](exercise_4_files/figure-gfm/degree%20centrality-1.png)<!-- -->

## Which seat should you pick ?

Assumptions : 1. The seats are fixed that is the seats could not be
changed on a daily basis for the entire duration of the internship 2.
The duration of the internship is assumed to be 90 days

The intent of the bus-ride is to make as many informal connections as
possible. Based on the betweenness and degree centrality values I would
choose seat B for the following reasons: 1. Seat B provides a good
balance between the degree and betweenness centrality and therefore will
enjoy the maximum ties and increased density of connections to others.
2. I will have access to seat A which will ensure circulation of
information with seat 1 and 2 via A. Therefore enlarging the circle of
reach. 3. Seat B is adjacent to the luggage storage space. This could be
a positive as while disembarkation helping someone with their luggage
could earn additional brownie points when establishing connections.

Although seats B and C do not have any substantial advantage over one
another, I believe seat B will have the following limitations: 1.Seat B
will not have direct access to seats 5,4,2 and 1. In the scenario that
these seats are frequently occupied by someone who is extremely social
who galvanizes relationships or by someone who is extremely valuable
within the organization who can determine whether the internship can
lead to a full-time position with the firm, then seat B will not be in a
favorable position.
