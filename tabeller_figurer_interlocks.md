En simpel introduktion til de 1000 største virksomheder i Danmark
========================================================

I det her dokument samles tabeller og figurer til analysen af corporate interlocks i Danmark



```
## Loading required package: plyr
```

```
## Attaching package: 'reshape'
```

```
## The following object(s) are masked from 'package:plyr':
## 
## rename, round_any
```


Pareto fordelinger eller Lorentz-kurver for økonomiske variable og netværksvariable.


```
Warning: Removed 38 rows containing missing values (geom_path).
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-21.png) ![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-22.png) 


## Deskriptiv statistik

Her beskrives data udfra branche


```
                               BILH   BYGG   DETH    ENER    ENGH   ENTR    FINA    ITEL   JERN   KEMI   KONG    LEVN   MASK    MEDI   MEFO    MØBL    PAPI   REDE    SERV   TEKS    TRAN   UNDE
Number of vertices             49.0   46.0   33.0    56.0    74.0   38.0    77.0    79.0   30.0   22.0   18.0   101.0   62.0    47.0   26.0    23.0    20.0   27.0    76.0   15.0    51.0   12.0
Number of edges               228.0  220.0  126.0   242.0   310.0  132.0   344.0   376.0  118.0  116.0   48.0   446.0  280.0   234.0  154.0   200.0    88.0   80.0   420.0   60.0   244.0   82.0
Average degree                  4.7    4.8    3.8     4.3     4.2    3.5     4.5     4.8    3.9    5.3    2.7     4.4    4.5     5.0    5.9     8.7     4.4    3.0     5.5    4.0     4.8    6.8
Part density (o/oo)             9.1    9.4    7.5     8.5     8.2    6.8     8.8     9.3    7.7   10.3    5.2     8.7    8.8     9.8   11.6    17.0     8.6    5.8    10.8    7.8     9.4   13.4
Number of clusters in part     82.0   80.0   68.0    88.0   101.0   74.0   106.0   106.0   65.0   58.0   58.0   120.0   90.0    82.0   66.0    59.0    60.0   66.0   107.0   55.0    81.0   52.0
Average path length             Inf    Inf    Inf     Inf     Inf    Inf     Inf     Inf    Inf    Inf    Inf     Inf    Inf     Inf    Inf     Inf     Inf    Inf     Inf    Inf     Inf    Inf
Average path length in part     Inf    Inf    Inf     Inf     Inf    Inf     Inf     Inf    Inf    Inf    Inf     Inf    Inf     Inf    Inf     Inf     Inf    Inf     Inf    Inf     Inf    Inf
Longest path in part            Inf    Inf    Inf     Inf     Inf    Inf     Inf     Inf    Inf    Inf    Inf     Inf    Inf     Inf    Inf     Inf     Inf    Inf     Inf    Inf     Inf    Inf
Highest degree                 36.0   34.0   16.0    30.0    36.0   16.0    34.0    28.0   22.0   22.0   26.0    34.0   22.0    36.0   28.0    32.0    26.0   16.0    32.0   22.0    30.0   24.0
Highest degree in part          4.0    4.0    4.0     4.0     6.0    4.0     4.0     6.0    4.0    4.0    0.0     6.0    6.0     4.0    0.0     2.0     0.0    2.0     4.0    0.0     4.0    0.0
Largest 2. neighborhood        82.0   79.0   39.0    71.0    88.0   58.0   104.0    78.0   67.0   48.0   58.0    92.0   52.0    71.0   77.0    97.0    58.0   37.0    87.0   58.0    80.0   88.0
Largest 3. neighborhood       236.0  217.0  143.0   223.0   242.0  205.0   265.0   232.0  210.0  169.0  175.0   248.0  177.0   215.0  226.0   248.0   186.0  139.0   251.0  185.0   234.0  247.0
Average closeness               0.0    0.0    0.0     0.0     0.0    0.0     0.0     0.0    0.0    0.0    0.0     0.0    0.0     0.0    0.0     0.0     0.0    0.0     0.0    0.0     0.0    0.0
Average closeness in part       2.3    2.6    4.2     1.9     1.2    3.4     1.1     1.1    4.8    7.5    9.7     0.7    1.6     2.5    5.9     7.0     8.5    5.6     1.1   12.3     2.2   16.3
Maximum closeness               0.0    0.0    0.0     0.0     0.0    0.0     0.0     0.0    0.0    0.0    0.0     0.0    0.0     0.0    0.0     0.0     0.0    0.0     0.0    0.0     0.0    0.0
Average betweeness            895.0  823.0  635.0  1230.0  1028.0  493.0   977.0   950.0  690.0  953.0  120.0   944.0  864.0   749.0  922.0  2229.0  1103.0  489.0  1156.0  609.0   941.0 1054.0
Average betweenness in part     0.0    0.0    0.0     0.0     0.0    0.0     0.0     0.0    0.0    0.0    0.0     1.0    0.0     0.0    0.0     0.0     0.0    0.0     0.0    0.0     0.0    0.0
Maximum betweenness         16221.7 9567.0 8514.0 13652.3 19808.2 8877.8 21940.0 10731.0 6205.1 5604.4 2104.3 23012.0 7074.9 15221.8 8247.3 18496.6 10870.2 4514.7 11269.2 3967.5 11163.9 5298.7
Average eigencentrality        39.2   16.8    7.4    25.9    32.7   20.0    16.5    19.4   12.2   14.1   12.9    29.5    9.7    19.0   26.7    40.2     8.9   12.9    28.9   19.3    32.9   53.7
Maximum eigencentrality       544.1  200.0   65.9   622.3  1000.0  265.6   384.1   304.2   94.1  110.0  228.0   847.8  112.6   174.3  211.5   221.0    50.2  110.3   847.8  143.4   687.7  386.6
```


## De 10 største

```
Error: object 'mat' not found
```


## De 10 midterste

```
Error: object 'mat' not found
```


## De 10 mindste

```
Error: object 'mat' not found
```


Her ser vi Lorenz-kurven / Pareto fordelingen for omsætning blandt virksomhederne. Omsætningen blandt de 1000 største virksomheder følger perfekt Pareto fordelingen med 20% af virksomhederne, der har 80% af omsætningen.

```r
data.o <- data[order(data$Omsætning, decreasing = FALSE), ]
```

```
## Error: argument 1 is not a vector
```

```r
ggplot(data.o, aes(y = cumsum(prop), x = seq(prop)/length(prop))) + geom_area(color = "black", fill = "red", alpha = 0.3) + geom_vline(xintercept = 0.8)
```

```
## Error: object 'data.o' not found
```

```r
ggplot(data, aes(y = Omsætning, x = Rang.11)) + geom_point()
```

```
## Error: object 'Rang.11' not found
```



