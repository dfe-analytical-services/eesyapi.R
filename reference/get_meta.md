# Get a parsed version of the API response for a data set's meta data

Get a list of metadata information for a data set available from the EES
API. Provides either look-up tables from human readable labels to ids
used in the API, or the raw response from the meta endpoint.

## Usage

``` r
get_meta(
  dataset_id,
  dataset_version = NULL,
  preview_token = NULL,
  ees_environment = NULL,
  api_version = NULL,
  verbose = FALSE
)
```

## Arguments

- dataset_id:

  ID of data set to be connected to. This is required if the endpoint is
  one of "get-dataset-versions", "get-summary", "get-meta", "get-csv",
  "get-data" or "post-data"

- dataset_version:

  Version of data set to be connected to, in "major.minor.patch" format,
  with optional wildcards, e.g. "*", "2.*", "2.1.\*", "2.1.0". Can also
  be provided as a numeric value

- preview_token:

  Preview token required for access to private data sets

- ees_environment:

  EES ees_environment to connect to: "dev", "test", "preprod" or "prod"

- api_version:

  EES API version

- verbose:

  Run with additional contextual messaging. Logical, default = FALSE

## Value

List of data frames containing a data set's meta data

## Examples

``` r
get_meta(example_id())
#> $time_periods
#>    code period        label
#> 1    W2   2026  2026 Week 2
#> 2    W4   2026  2026 Week 4
#> 3    W8   2025  2025 Week 8
#> 4   W10   2025 2025 Week 10
#> 5   W12   2025 2025 Week 12
#> 6   W14   2025 2025 Week 14
#> 7   W15   2025 2025 Week 15
#> 8   W18   2025 2025 Week 18
#> 9   W20   2025 2025 Week 20
#> 10  W21   2025 2025 Week 21
#> 11  W24   2025 2025 Week 24
#> 12  W26   2025 2025 Week 26
#> 13  W28   2025 2025 Week 28
#> 14  W30   2025 2025 Week 30
#> 
#> $locations
#>     item_id                               label      code oldCode
#> 1     rQkNj                Barking and Dagenham E09000002     301
#> 2     GCsgr                              Barnet E09000003     302
#> 3     u9Mo4                            Barnsley E08000016     370
#> 4     tdEm5        Bath and North East Somerset E06000022     800
#> 5     WU2bo                             Bedford E06000055     822
#> 6     3se8u                              Bexley E09000004     303
#> 7     emDuS                          Birmingham E08000025     330
#> 8     0kT5D               Blackburn with Darwen E06000008     889
#> 9     HTzLj                           Blackpool E06000009     890
#> 10    7YFXo                              Bolton E08000001     350
#> 11    9e4vZ Bournemouth, Christchurch and Poole E06000058     839
#> 12    dt0Zw                    Bracknell Forest E06000036     867
#> 13    Poqeb                            Bradford E08000032     380
#> 14    SdgVx                               Brent E09000005     304
#> 15    3P8uH                   Brighton and Hove E06000043     846
#> 16    fyYFZ                    Bristol, City of E06000023     801
#> 17    yW5aB                             Bromley E09000006     305
#> 18    P0ebW                     Buckinghamshire E06000060     825
#> 19    p5PSo                                Bury E08000002     351
#> 20    1rPi3                          Calderdale E08000033     381
#> 21    HvezL                      Cambridgeshire E10000003     873
#> 22    RySka                              Camden E09000007     202
#> 23    ENqFR                Central Bedfordshire E06000056     823
#> 24    DcQeg                       Cheshire East E06000049     895
#> 25    eFuSW           Cheshire West and Chester E06000050     896
#> 26    o47mX                      City of London E09000001     201
#> 27    45UZU                            Cornwall E06000052     908
#> 28    b8tTc                       County Durham E06000047     840
#> 29    TuNPJ                            Coventry E08000026     331
#> 30    bqZtT                             Croydon E09000008     306
#> 31    o49mX                          Cumberland E06000063     942
#> 32    it6Xr                          Darlington E06000005     841
#> 33    z4FQE                               Derby E06000015     831
#> 34    CXVId                          Derbyshire E10000007     830
#> 35    YJuHK                               Devon E10000008     878
#> 36    XH4fK                           Doncaster E08000017     371
#> 37    2alzG                              Dorset E06000059     838
#> 38    hLOyW                              Dudley E08000027     332
#> 39    Db3Qe                              Ealing E09000009     307
#> 40    YPHKM            East Riding of Yorkshire E06000011     811
#> 41    qgvjG                         East Sussex E10000011     845
#> 42    emJuS                             Enfield E09000010     308
#> 43    alDLP                               Essex E10000012     881
#> 44    UhYRF                           Gateshead E08000037     390
#> 45    BYe7J                     Gloucestershire E10000013     916
#> 46    5TYdi                           Greenwich E09000011     203
#> 47    4kdUZ                             Hackney E09000012     204
#> 48    LxWjE                              Halton E06000006     876
#> 49    mU59K              Hammersmith and Fulham E09000013     205
#> 50    znRFQ                           Hampshire E10000014     850
#> 51    0evT5                            Haringey E09000014     309
#> 52    WcR2b                              Harrow E09000015     310
#> 53    O7CLF                          Hartlepool E06000001     805
#> 54    EADqF                            Havering E09000016     311
#> 55    TsPJP            Herefordshire, County of E06000019     884
#> 56    kLDhs                       Hertfordshire E10000015     919
#> 57    Qt6wb                          Hillingdon E09000017     312
#> 58    9Ru4v                            Hounslow E09000018     313
#> 59    Rpka2                       Isle of Wight E06000046     921
#> 60    mV9KC                     Isles of Scilly E06000053     420
#> 61    23Llz                           Islington E09000019     206
#> 62    PoTeb              Kensington and Chelsea E09000020     207
#> 63    u4Ko4                                Kent E10000016     886
#> 64    CpId1         Kingston upon Hull, City of E06000010     810
#> 65    NwNDC                Kingston upon Thames E09000021     314
#> 66    dPE0Z                            Kirklees E08000034     382
#> 67    qGJjG                            Knowsley E08000011     340
#> 68    wqtbx                             Lambeth E09000022     208
#> 69    XzUfK                          Lancashire E10000017     888
#> 70    u9Oo4                               Leeds E08000035     383
#> 71    krhsL                           Leicester E06000016     856
#> 72    ToMPJ                      Leicestershire E10000018     855
#> 73    oU4mX                            Lewisham E09000023     209
#> 74    csC31                        Lincolnshire E10000019     925
#> 75    a2NLP                           Liverpool E08000012     341
#> 76    AkGK0                               Luton E06000032     821
#> 77    IXFBz                          Manchester E08000003     352
#> 78    1Ti3Z                              Medway E06000035     887
#> 79    9kk4v                              Merton E09000024     315
#> 80    7zXob                       Middlesbrough E06000002     806
#> 81    GZgru                       Milton Keynes E06000042     826
#> 82    5Tsdi                 Newcastle upon Tyne E08000021     391
#> 83    deu0Z                              Newham E09000025     316
#> 84    5Pxdi                             Norfolk E10000020     926
#> 85    qFjG7             North East Lincolnshire E06000012     812
#> 86    arLPb                  North Lincolnshire E06000013     813
#> 87    NrDCj              North Northamptonshire E06000061     940
#> 88    juAMt                      North Somerset E06000024     802
#> 89    tj0Em                      North Tyneside E08000022     392
#> 90    dPe0Z                     North Yorkshire E06000065     815
#> 91    Qiwb4                      Northumberland E06000057     929
#> 92    X9fKb                          Nottingham E06000018     892
#> 93    tATEm                     Nottinghamshire E10000024     891
#> 94    iNE6X                              Oldham E08000004     353
#> 95    fFLYF                         Oxfordshire E10000025     931
#> 96    ghO9G                        Peterborough E06000031     874
#> 97    TrPJP                            Plymouth E06000026     879
#> 98    SGVxt                          Portsmouth E06000044     851
#> 99    lpcBn                             Reading E06000038     870
#> 100   O7fCL                           Redbridge E09000026     317
#> 101   pTSoj                Redcar and Cleveland E06000003     807
#> 102   7HJXo                Richmond upon Thames E09000027     318
#> 103   LlMWj                            Rochdale E08000005     354
#> 104   TuxPJ                           Rotherham E08000018     372
#> 105   uxo41                             Rutland E06000017     857
#> 106   6DArf                             Salford E08000006     355
#> 107   mUq9K                            Sandwell E08000028     333
#> 108   zvUFQ                              Sefton E08000014     343
#> 109   cZO31                           Sheffield E08000019     373
#> 110   5fdi9                          Shropshire E06000051     893
#> 111   UfRFO                              Slough E06000039     871
#> 112   gIyO9                            Solihull E08000029     334
#> 113   OBXCL                            Somerset E06000066     933
#> 114   eNuSW               South Gloucestershire E06000025     803
#> 115   fzaYF                      South Tyneside E08000023     393
#> 116   yVaBm                         Southampton E06000045     852
#> 117   VN5XE                     Southend-on-Sea E06000033     882
#> 118   pdeSo                           Southwark E09000028     210
#> 119   BfP7J                          St. Helens E08000013     342
#> 120   jOmAM                       Staffordshire E10000028     860
#> 121   0ehT5                           Stockport E08000007     356
#> 122   IzBzg                    Stockton-on-Tees E06000004     808
#> 123   5Zdi9                      Stoke-on-Trent E06000021     861
#> 124   e9QuS                             Suffolk E10000029     935
#> 125   jgoAM                          Sunderland E08000024     394
#> 126   ToDPJ                              Surrey E10000030     936
#> 127   IfJBz                              Sutton E09000029     319
#> 128   mE9KC                             Swindon E06000030     866
#> 129   HJPzL                            Tameside E08000008     357
#> 130   cg31S                  Telford and Wrekin E06000020     894
#> 131   PEebW                            Thurrock E06000034     883
#> 132   hgyW6                              Torbay E06000027     880
#> 133   iDq6X                       Tower Hamlets E09000030     211
#> 134   C7SId                            Trafford E08000009     358
#> 135   l8ScB                           Wakefield E08000036     384
#> 136   AOhGK                             Walsall E08000030     335
#> 137   LcKWj                      Waltham Forest E09000031     320
#> 138   6QGrf                          Wandsworth E09000032     212
#> 139   6jrfe                          Warrington E06000007     877
#> 140   hWCyW                        Warwickshire E10000031     937
#> 141   ugo41                      West Berkshire E06000037     869
#> 142   wwbxT               West Northamptonshire E06000062     941
#> 143   mRj9K                         West Sussex E10000032     938
#> 144   0uyT5                         Westminster E09000033     213
#> 145   9RR4v             Westmorland and Furness E06000064     943
#> 146   YFeHK                               Wigan E08000010     359
#> 147   0JT5D                           Wiltshire E06000054     865
#> 148   oBmXk              Windsor and Maidenhead E06000040     868
#> 149   kaNhs                              Wirral E08000015     344
#> 150   rRNj1                           Wokingham E06000041     872
#> 151   V0D5X                       Wolverhampton E08000031     336
#> 152   gBaO9                      Worcestershire E10000034     885
#> 153   BT7J3                                York E06000014     816
#> 154   dP0Zw                             England E92000001    <NA>
#> 155   1Nei3                       East Midlands E12000004    <NA>
#> 156   u4Co4                     East of England E12000006    <NA>
#> 157   lZVcB                              London E12000007    <NA>
#> 158   ACyGK                          North East E12000001    <NA>
#> 159   VTQ5X                          North West E12000002    <NA>
#> 160   UyHRF                          South East E12000008    <NA>
#> 161   oUXmX                          South West E12000009    <NA>
#> 162   deq0Z                       West Midlands E12000005    <NA>
#> 163   Pmjeb            Yorkshire and The Humber E12000003    <NA>
#>     geographic_level_code geographic_level
#> 1                      LA  Local authority
#> 2                      LA  Local authority
#> 3                      LA  Local authority
#> 4                      LA  Local authority
#> 5                      LA  Local authority
#> 6                      LA  Local authority
#> 7                      LA  Local authority
#> 8                      LA  Local authority
#> 9                      LA  Local authority
#> 10                     LA  Local authority
#> 11                     LA  Local authority
#> 12                     LA  Local authority
#> 13                     LA  Local authority
#> 14                     LA  Local authority
#> 15                     LA  Local authority
#> 16                     LA  Local authority
#> 17                     LA  Local authority
#> 18                     LA  Local authority
#> 19                     LA  Local authority
#> 20                     LA  Local authority
#> 21                     LA  Local authority
#> 22                     LA  Local authority
#> 23                     LA  Local authority
#> 24                     LA  Local authority
#> 25                     LA  Local authority
#> 26                     LA  Local authority
#> 27                     LA  Local authority
#> 28                     LA  Local authority
#> 29                     LA  Local authority
#> 30                     LA  Local authority
#> 31                     LA  Local authority
#> 32                     LA  Local authority
#> 33                     LA  Local authority
#> 34                     LA  Local authority
#> 35                     LA  Local authority
#> 36                     LA  Local authority
#> 37                     LA  Local authority
#> 38                     LA  Local authority
#> 39                     LA  Local authority
#> 40                     LA  Local authority
#> 41                     LA  Local authority
#> 42                     LA  Local authority
#> 43                     LA  Local authority
#> 44                     LA  Local authority
#> 45                     LA  Local authority
#> 46                     LA  Local authority
#> 47                     LA  Local authority
#> 48                     LA  Local authority
#> 49                     LA  Local authority
#> 50                     LA  Local authority
#> 51                     LA  Local authority
#> 52                     LA  Local authority
#> 53                     LA  Local authority
#> 54                     LA  Local authority
#> 55                     LA  Local authority
#> 56                     LA  Local authority
#> 57                     LA  Local authority
#> 58                     LA  Local authority
#> 59                     LA  Local authority
#> 60                     LA  Local authority
#> 61                     LA  Local authority
#> 62                     LA  Local authority
#> 63                     LA  Local authority
#> 64                     LA  Local authority
#> 65                     LA  Local authority
#> 66                     LA  Local authority
#> 67                     LA  Local authority
#> 68                     LA  Local authority
#> 69                     LA  Local authority
#> 70                     LA  Local authority
#> 71                     LA  Local authority
#> 72                     LA  Local authority
#> 73                     LA  Local authority
#> 74                     LA  Local authority
#> 75                     LA  Local authority
#> 76                     LA  Local authority
#> 77                     LA  Local authority
#> 78                     LA  Local authority
#> 79                     LA  Local authority
#> 80                     LA  Local authority
#> 81                     LA  Local authority
#> 82                     LA  Local authority
#> 83                     LA  Local authority
#> 84                     LA  Local authority
#> 85                     LA  Local authority
#> 86                     LA  Local authority
#> 87                     LA  Local authority
#> 88                     LA  Local authority
#> 89                     LA  Local authority
#> 90                     LA  Local authority
#> 91                     LA  Local authority
#> 92                     LA  Local authority
#> 93                     LA  Local authority
#> 94                     LA  Local authority
#> 95                     LA  Local authority
#> 96                     LA  Local authority
#> 97                     LA  Local authority
#> 98                     LA  Local authority
#> 99                     LA  Local authority
#> 100                    LA  Local authority
#> 101                    LA  Local authority
#> 102                    LA  Local authority
#> 103                    LA  Local authority
#> 104                    LA  Local authority
#> 105                    LA  Local authority
#> 106                    LA  Local authority
#> 107                    LA  Local authority
#> 108                    LA  Local authority
#> 109                    LA  Local authority
#> 110                    LA  Local authority
#> 111                    LA  Local authority
#> 112                    LA  Local authority
#> 113                    LA  Local authority
#> 114                    LA  Local authority
#> 115                    LA  Local authority
#> 116                    LA  Local authority
#> 117                    LA  Local authority
#> 118                    LA  Local authority
#> 119                    LA  Local authority
#> 120                    LA  Local authority
#> 121                    LA  Local authority
#> 122                    LA  Local authority
#> 123                    LA  Local authority
#> 124                    LA  Local authority
#> 125                    LA  Local authority
#> 126                    LA  Local authority
#> 127                    LA  Local authority
#> 128                    LA  Local authority
#> 129                    LA  Local authority
#> 130                    LA  Local authority
#> 131                    LA  Local authority
#> 132                    LA  Local authority
#> 133                    LA  Local authority
#> 134                    LA  Local authority
#> 135                    LA  Local authority
#> 136                    LA  Local authority
#> 137                    LA  Local authority
#> 138                    LA  Local authority
#> 139                    LA  Local authority
#> 140                    LA  Local authority
#> 141                    LA  Local authority
#> 142                    LA  Local authority
#> 143                    LA  Local authority
#> 144                    LA  Local authority
#> 145                    LA  Local authority
#> 146                    LA  Local authority
#> 147                    LA  Local authority
#> 148                    LA  Local authority
#> 149                    LA  Local authority
#> 150                    LA  Local authority
#> 151                    LA  Local authority
#> 152                    LA  Local authority
#> 153                    LA  Local authority
#> 154                   NAT         National
#> 155                   REG         Regional
#> 156                   REG         Regional
#> 157                   REG         Regional
#> 158                   REG         Regional
#> 159                   REG         Regional
#> 160                   REG         Regional
#> 161                   REG         Regional
#> 162                   REG         Regional
#> 163                   REG         Regional
#> 
#> $filter_columns
#>   col_id        col_name           label
#> 1  BT7J3 education_phase Education phase
#> 
#> $filter_items
#>   col_id        col_name           label item_id  item_label isAggregate
#> 1  BT7J3 education_phase Education phase   UyHRF All schools          NA
#> 2  BT7J3 education_phase Education phase   oUXmX     Primary          NA
#> 3  BT7J3 education_phase Education phase   rwhNj   Secondary          NA
#> 4  BT7J3 education_phase Education phase   GIxgr     Special          NA
#> 
#> $indicators
#>   col_id                   col_name                   label
#> 1  uxo41 persistent_absence_percent Persistent absence rate
#> 
```
