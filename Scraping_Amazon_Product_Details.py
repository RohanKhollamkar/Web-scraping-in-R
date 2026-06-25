Amazon Product Details
================
Rohan Khollamkar
17 April 2019

**Libraries**

``` r
library(rvest)
```

    ## Loading required package: xml2

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(stringr)
```

**Go to product
page**

``` r
HtmlLink <- "https://www.amazon.com/GS65-Stealth-i7-8750H-Notebook-Computer/dp/B07MQ73SPZ/ref=sr_1_4?keywords=2080+laptops+2tb+ssd&qid=1555596596&s=electronics&sr=1-4"
txtPage <- read_html(HtmlLink)
print(txtPage)
```

    ## {xml_document}
    ## <html lang="en-us" class="a-no-js" data-19ax5a9jf="dingo">
    ## [1] <head>\n<script>var aPageStart = (new Date()).getTime();</script><me ...
    ## [2] <body class="a-m-us a-aui_152852-c a-aui_157141-c a-aui_158613-c a-a ...

**Page Title**

``` r
page.title <- html_node(txtPage,'title')
page.title <- html_text(page.title)
print(page.title)
```

    ## [1] "Amazon.com: CUK MSI GS65 Stealth Ultra Thin & Light Gaming Laptop (Intel i7-8750H, 32GB RAM, 2TB NVMe SSD, NVIDIA GeForce RTX 2080 8GB Max-Q, 15.6\" FHD 144Hz, Windows 10 Pro) Gamer Notebook Computer: Electronics"

**Generic Cleaning Function**

``` r
getCleanText <- function(vcsAllText){
    #print(vcsAllText)
    vcsAllText <- str_replace_all(vcsAllText,"\n","")
    #print(vcsAllText)
    vcsAllText <- trimws(vcsAllText, which = c("both"))
    #print(vcsAllText)
    return(vcsAllText)
}
```

**Product Title**

``` r
Prodtitle <- html_node(txtPage,"#productTitle")
Prodtitle <- html_text(Prodtitle)
Prodtitle <- getCleanText(Prodtitle)
Prodtitle <- str_sub(Prodtitle,1,53)  ## We want name only and not a configuration
print(Prodtitle)
```

    ## [1] "CUK MSI GS65 Stealth Ultra Thin & Light Gaming Laptop"

**product Price**

``` r
ProdPrice <- txtPage%>%
             html_node("#priceblock_ourprice")%>%
             html_text()
ProdPrice <- getCleanText(ProdPrice)
ProdPrice <- strsplit(ProdPrice,"$",fixed = T)
ProdPrice <- unlist(ProdPrice)
ProdPrice <- getCleanText(ProdPrice)
ProdPrice <- ProdPrice[2]
print(ProdPrice)
```

    ## [1] "2,849.99"

**Product size and prices**

``` r
ProdSize <- txtPage%>%
            html_node("#variation_size_name")%>%
            html_text()
ProdSize <- getCleanText(ProdSize)
ProdSize <- str_replace(gsub("\\s+"," ",ProdSize),"B","b")
ProdSize <- str_split(ProdSize ,"/")
ProdSize <- unlist(ProdSize)
ProdSize <- ProdSize[grepl(ProdSize,pattern = "GB+")]
ProdSize <- str_split(ProdSize,";")
ProdSize <- unlist(ProdSize)
ProdSize <- ProdSize[grepl(ProdSize,pattern = "GB+")]


ProdSize[1] <- gsub(x = ProdSize[1],pattern = "Size",replacement = "")
ProdSize <- trimws(ProdSize)
ProdPrices <- strsplit(ProdSize,split = "$",fixed = T)
ProdPrices <- unlist(ProdPrices)
ProdSize <- ProdPrices[c(1,3,5,7,9,11,13)]
ProdSize <- str_remove_all(ProdSize,"[[:punct:]]")
ProdSize <- trimws(ProdSize)
ProdPrices <- ProdPrices[c(2,4,6,8,10,12,14)]

sprintf("Product Sizes:")
```

    ## [1] "Product Sizes:"

``` r
print(ProdSize)
```

    ## [1] "32Gb RAM | 2TB NVMe SSD | RTX 2080 8GB 16GB RAM | 256GB NVMe SSD | RTX 2070 8GB"
    ## [2] "269999 32GB RAM | 2TB NVMe SSD | RTX 2080 8GB"                                  
    ## [3] "219999 32GB RAM | 500GB NVMe SSD | RTX 2080 8GB"                                
    ## [4] NA                                                                               
    ## [5] NA                                                                               
    ## [6] NA                                                                               
    ## [7] NA

``` r
print("Product Prices:")
```

    ## [1] "Product Prices:"

``` r
print(ProdPrices)
```

    ## [1] "2,069.99 16GB RAM | 256GB NVMe SSD | RTX 2080 8GB "
    ## [2] "2,849.99 32GB RAM | 500GB NVMe SSD | RTX 2070 8GB "
    ## [3] "2,699.99"                                          
    ## [4] NA                                                  
    ## [5] NA                                                  
    ## [6] NA                                                  
    ## [7] NA

**Product description**

``` r
ProdDesc <- txtPage%>%
            html_node("ul.a-spacing-none")%>%
            html_text()
ProdDesc <- getCleanText(ProdDesc)
ProdDesc <- gsub("\\t+", " ",ProdDesc)
ProdDesc <- strsplit(ProdDesc,"  ",fixed = T)
ProdDesc <- unlist(ProdDesc)

ProdDesc <- ProdDesc[22:26]
ProdDesc <- trimws(ProdDesc)

ProdDesc[5] <- strsplit(ProdDesc[5],"(",fixed = TRUE)
ProdDesc <- unlist(ProdDesc)
ProdDesc <- trimws(ProdDesc[1:5])
print(paste0(ProdDesc))
```

    ## [1] "Processor: Intel Core i7-8750H Six Core Processor (9MB Cache, 2.2GHz-4.1GHz) 45W"                                                       
    ## [2] "RAM: 32GB DDR4 2666MHz | Hard Drive: 2TB NVMe Solid State Drive"                                                                        
    ## [3] "Keyboard: Steel Series per-Key RGB with Anti-Ghost Key (84 Key) | Operating System: Windows 10 Pro x64"                                 
    ## [4] "Graphics: NVIDIA GeForce RTX 2080 8GB GDDR6 Max-Q Design | Display: 15.6\" Full HD 144Hz 7ms Anti-Glare Wide View Display (1920 x 1080)"
    ## [5] "32GB RAM / 2TB NVMe SSD Upgrades | 3-Year CUK Limited Warranty"

**Final Details of
Product**

``` r
final = data.frame(Variants = ProdSize, Price = ProdPrices, stringsAsFactors = F)
final
```

    ##                                                                          Variants
    ## 1 32Gb RAM | 2TB NVMe SSD | RTX 2080 8GB 16GB RAM | 256GB NVMe SSD | RTX 2070 8GB
    ## 2                                   269999 32GB RAM | 2TB NVMe SSD | RTX 2080 8GB
    ## 3                                 219999 32GB RAM | 500GB NVMe SSD | RTX 2080 8GB
    ## 4                                                                            <NA>
    ## 5                                                                            <NA>
    ## 6                                                                            <NA>
    ## 7                                                                            <NA>
    ##                                                Price
    ## 1 2,069.99 16GB RAM | 256GB NVMe SSD | RTX 2080 8GB 
    ## 2 2,849.99 32GB RAM | 500GB NVMe SSD | RTX 2070 8GB 
    ## 3                                           2,699.99
    ## 4                                               <NA>
    ## 5                                               <NA>
    ## 6                                               <NA>
    ## 7                                               <NA>
