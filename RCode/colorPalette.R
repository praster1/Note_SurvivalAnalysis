colorPalette = function(n)
{
    if (!require("RColorBrewer")) install.packages("RColorBrewer")
    library(RColorBrewer)
    
    n = n
    qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',];
    col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)));
    color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)];
    
    return(color)
}

