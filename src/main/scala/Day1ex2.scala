object Day1ex2 extends App {
  val sumIncreases = (input: Seq[Int]) =>
    input.zipWithIndex.drop(1).foldLeft(0) {
        case (acc, (curr, index)) => {
          if (curr > input(index - 1)) acc + 1 else acc
        }
      }

  val asThreeWindow = (input: Seq[Int]) =>
    input.zipWithIndex.take(input.size - 2).map((x,y) => x + input(y +1) + input(y+2))

//  println(sumIncreases(Seq(199,200,208,210,200,207,240,269,260,263)))
//  println(sumIncreases(asThreeWindow(Seq(199,200,208,210,200,207,240,269,260,263))))
println(sumIncreases(asThreeWindow(Seq(
  150
  ,152
  ,155
  ,156
  ,157
  ,141
  ,124
  ,138
  ,143
  ,145
  ,144
  ,146
  ,148
  ,149
  ,148
  ,149
  ,124
  ,122
  ,123
  ,124
  ,117
  ,118
  ,119
  ,132
  ,159
  ,152
  ,175
  ,183
  ,194
  ,222
  ,226
  ,237
  ,242
  ,248
  ,229
  ,225
  ,227
  ,235
  ,229
  ,230
  ,227
  ,229
  ,252
  ,253
  ,251
  ,269
  ,270
  ,266
  ,268
  ,269
  ,270
  ,256
  ,257
  ,258
  ,261
  ,258
  ,251
  ,242
  ,245
  ,270
  ,275
  ,287
  ,309
  ,310
  ,330
  ,337
  ,344
  ,347
  ,353
  ,350
  ,355
  ,356
  ,353
  ,354
  ,372
  ,371
  ,370
  ,365
  ,368
  ,370
  ,374
  ,370
  ,332
  ,329
  ,340
  ,343
  ,347
  ,348
  ,349
  ,350
  ,351
  ,366
  ,358
  ,370
  ,369
  ,370
  ,367
  ,368
  ,369
  ,371
  ,382
  ,380
  ,385
  ,379
  ,394
  ,395
  ,393
  ,406
  ,389
  ,400
  ,389
  ,396
  ,386
  ,387
  ,390
  ,399
  ,406
  ,408
  ,413
  ,415
  ,416
  ,417
  ,416
  ,417
  ,416
  ,424
  ,425
  ,426
  ,427
  ,434
  ,435
  ,434
  ,436
  ,433
  ,430
  ,431
  ,434
  ,435
  ,436
  ,437
  ,452
  ,456
  ,459
  ,462
  ,463
  ,475
  ,477
  ,479
  ,500
  ,497
  ,498
  ,501
  ,507
  ,522
  ,517
  ,509
  ,519
  ,541
  ,542
  ,540
  ,551
  ,558
  ,551
  ,552
  ,553
  ,564
  ,569
  ,575
  ,581
  ,587
  ,589
  ,591
  ,596
  ,610
  ,617
  ,621
  ,618
  ,614
  ,615
  ,620
  ,640
  ,654
  ,669
  ,670
  ,686
  ,685
  ,686
  ,690
  ,691
  ,700
  ,706
  ,722
  ,721
  ,720
  ,725
  ,726
  ,736
  ,738
  ,754
  ,761
  ,759
  ,757
  ,765
  ,773
  ,790
  ,797
  ,798
  ,797
  ,798
  ,796
  ,789
  ,794
  ,798
  ,797
  ,804
  ,806
  ,807
  ,831
  ,823
  ,833
  ,836
  ,857
  ,852
  ,856
  ,863
  ,896
  ,898
  ,893
  ,904
  ,908
  ,931
  ,933
  ,948
  ,966
  ,967
  ,957
  ,961
  ,969
  ,971
  ,985
  ,988
  ,989
  ,996
  ,1018
  ,1020
  ,1025
  ,1060
  ,1061
  ,1060
  ,1088
  ,1090
  ,1096
  ,1091
  ,1092
  ,1091
  ,1064
  ,1070
  ,1087
  ,1088
  ,1083
  ,1085
  ,1090
  ,1091
  ,1094
  ,1093
  ,1091
  ,1093
  ,1103
  ,1104
  ,1105
  ,1107
  ,1126
  ,1127
  ,1126
  ,1145
  ,1151
  ,1165
  ,1166
  ,1171
  ,1179
  ,1189
  ,1190
  ,1193
  ,1217
  ,1218
  ,1219
  ,1220
  ,1222
  ,1225
  ,1210
  ,1221
  ,1225
  ,1247
  ,1249
  ,1246
  ,1247
  ,1228
  ,1227
  ,1214
  ,1224
  ,1254
  ,1256
  ,1276
  ,1279
  ,1273
  ,1268
  ,1271
  ,1272
  ,1269
  ,1279
  ,1286
  ,1290
  ,1263
  ,1267
  ,1270
  ,1272
  ,1286
  ,1276
  ,1277
  ,1278
  ,1280
  ,1266
  ,1267
  ,1266
  ,1231
  ,1235
  ,1260
  ,1261
  ,1271
  ,1280
  ,1284
  ,1291
  ,1299
  ,1297
  ,1310
  ,1322
  ,1331
  ,1342
  ,1336
  ,1345
  ,1358
  ,1361
  ,1358
  ,1343
  ,1334
  ,1333
  ,1330
  ,1346
  ,1349
  ,1348
  ,1353
  ,1371
  ,1380
  ,1388
  ,1362
  ,1373
  ,1378
  ,1393
  ,1417
  ,1407
  ,1412
  ,1417
  ,1419
  ,1420
  ,1421
  ,1436
  ,1432
  ,1437
  ,1442
  ,1444
  ,1447
  ,1448
  ,1467
  ,1468
  ,1472
  ,1477
  ,1479
  ,1513
  ,1541
  ,1549
  ,1566
  ,1562
  ,1567
  ,1571
  ,1581
  ,1583
  ,1578
  ,1583
  ,1601
  ,1588
  ,1611
  ,1612
  ,1614
  ,1615
  ,1624
  ,1618
  ,1612
  ,1632
  ,1633
  ,1635
  ,1634
  ,1646
  ,1661
  ,1663
  ,1656
  ,1657
  ,1658
  ,1657
  ,1658
  ,1677
  ,1679
  ,1692
  ,1693
  ,1692
  ,1718
  ,1719
  ,1720
  ,1721
  ,1727
  ,1732
  ,1731
  ,1730
  ,1722
  ,1723
  ,1737
  ,1743
  ,1744
  ,1747
  ,1753
  ,1766
  ,1768
  ,1765
  ,1767
  ,1773
  ,1768
  ,1773
  ,1778
  ,1776
  ,1788
  ,1790
  ,1799
  ,1807
  ,1808
  ,1805
  ,1806
  ,1803
  ,1804
  ,1818
  ,1831
  ,1832
  ,1828
  ,1837
  ,1840
  ,1841
  ,1837
  ,1840
  ,1844
  ,1853
  ,1854
  ,1855
  ,1853
  ,1856
  ,1857
  ,1856
  ,1862
  ,1873
  ,1860
  ,1862
  ,1863
  ,1872
  ,1888
  ,1892
  ,1893
  ,1906
  ,1919
  ,1901
  ,1910
  ,1916
  ,1919
  ,1921
  ,1922
  ,1945
  ,1942
  ,1943
  ,1944
  ,1947
  ,1956
  ,1958
  ,1964
  ,1967
  ,1981
  ,1988
  ,1976
  ,1978
  ,1999
  ,2003
  ,2008
  ,2039
  ,2042
  ,2049
  ,2075
  ,2072
  ,2076
  ,2082
  ,2091
  ,2096
  ,2120
  ,2131
  ,2120
  ,2116
  ,2119
  ,2117
  ,2139
  ,2141
  ,2143
  ,2144
  ,2147
  ,2129
  ,2140
  ,2151
  ,2166
  ,2167
  ,2186
  ,2198
  ,2199
  ,2205
  ,2206
  ,2207
  ,2209
  ,2207
  ,2228
  ,2231
  ,2223
  ,2231
  ,2242
  ,2244
  ,2232
  ,2234
  ,2237
  ,2242
  ,2235
  ,2225
  ,2227
  ,2233
  ,2228
  ,2229
  ,2230
  ,2229
  ,2221
  ,2222
  ,2214
  ,2216
  ,2219
  ,2223
  ,2229
  ,2230
  ,2233
  ,2241
  ,2263
  ,2264
  ,2259
  ,2255
  ,2260
  ,2277
  ,2308
  ,2311
  ,2312
  ,2315
  ,2320
  ,2322
  ,2321
  ,2295
  ,2297
  ,2293
  ,2295
  ,2296
  ,2300
  ,2304
  ,2298
  ,2305
  ,2306
  ,2305
  ,2302
  ,2305
  ,2306
  ,2298
  ,2316
  ,2335
  ,2326
  ,2325
  ,2291
  ,2298
  ,2300
  ,2305
  ,2312
  ,2314
  ,2316
  ,2320
  ,2323
  ,2321
  ,2318
  ,2315
  ,2318
  ,2319
  ,2343
  ,2341
  ,2340
  ,2342
  ,2341
  ,2345
  ,2346
  ,2347
  ,2349
  ,2353
  ,2327
  ,2318
  ,2339
  ,2342
  ,2355
  ,2356
  ,2357
  ,2358
  ,2359
  ,2386
  ,2387
  ,2389
  ,2403
  ,2408
  ,2396
  ,2394
  ,2419
  ,2414
  ,2419
  ,2420
  ,2426
  ,2425
  ,2415
  ,2419
  ,2420
  ,2424
  ,2425
  ,2436
  ,2438
  ,2440
  ,2435
  ,2441
  ,2448
  ,2456
  ,2446
  ,2448
  ,2447
  ,2440
  ,2452
  ,2451
  ,2449
  ,2460
  ,2452
  ,2453
  ,2456
  ,2473
  ,2480
  ,2478
  ,2486
  ,2488
  ,2467
  ,2469
  ,2493
  ,2494
  ,2493
  ,2495
  ,2509
  ,2511
  ,2515
  ,2541
  ,2554
  ,2555
  ,2541
  ,2535
  ,2536
  ,2539
  ,2535
  ,2538
  ,2539
  ,2548
  ,2555
  ,2558
  ,2557
  ,2560
  ,2570
  ,2571
  ,2585
  ,2588
  ,2599
  ,2612
  ,2615
  ,2623
  ,2640
  ,2647
  ,2648
  ,2645
  ,2639
  ,2637
  ,2641
  ,2643
  ,2678
  ,2680
  ,2712
  ,2721
  ,2727
  ,2758
  ,2753
  ,2750
  ,2751
  ,2758
  ,2759
  ,2778
  ,2789
  ,2800
  ,2794
  ,2799
  ,2803
  ,2804
  ,2810
  ,2793
  ,2794
  ,2792
  ,2801
  ,2803
  ,2804
  ,2805
  ,2831
  ,2851
  ,2855
  ,2837
  ,2848
  ,2864
  ,2832
  ,2822
  ,2828
  ,2831
  ,2823
  ,2822
  ,2835
  ,2821
  ,2841
  ,2842
  ,2845
  ,2830
  ,2831
  ,2837
  ,2838
  ,2843
  ,2845
  ,2844
  ,2871
  ,2874
  ,2873
  ,2872
  ,2873
  ,2869
  ,2890
  ,2893
  ,2912
  ,2914
  ,2908
  ,2949
  ,2948
  ,2961
  ,2965
  ,2970
  ,2971
  ,2974
  ,2966
  ,2968
  ,2974
  ,2980
  ,2948
  ,2980
  ,2985
  ,2999
  ,3005
  ,3018
  ,3022
  ,3021
  ,3024
  ,3025
  ,3027
  ,3026
  ,3025
  ,3029
  ,3031
  ,3024
  ,2998
  ,3008
  ,3016
  ,3020
  ,3019
  ,3016
  ,3018
  ,3035
  ,3062
  ,3063
  ,3030
  ,3021
  ,3022
  ,3023
  ,3017
  ,3032
  ,3042
  ,3043
  ,3044
  ,3043
  ,3041
  ,3047
  ,3049
  ,3055
  ,3060
  ,3061
  ,3033
  ,3034
  ,3027
  ,3032
  ,3055
  ,3054
  ,3059
  ,3058
  ,3052
  ,3074
  ,3087
  ,3104
  ,3106
  ,3108
  ,3104
  ,3113
  ,3104
  ,3106
  ,3102
  ,3103
  ,3110
  ,3116
  ,3124
  ,3123
  ,3116
  ,3146
  ,3132
  ,3140
  ,3143
  ,3141
  ,3145
  ,3149
  ,3158
  ,3154
  ,3156
  ,3164
  ,3173
  ,3172
  ,3173
  ,3179
  ,3178
  ,3187
  ,3188
  ,3196
  ,3195
  ,3198
  ,3199
  ,3205
  ,3206
  ,3216
  ,3222
  ,3223
  ,3222
  ,3223
  ,3211
  ,3219
  ,3220
  ,3221
  ,3226
  ,3224
  ,3225
  ,3252
  ,3246
  ,3257
  ,3258
  ,3256
  ,3274
  ,3260
  ,3273
  ,3277
  ,3279
  ,3296
  ,3297
  ,3302
  ,3304
  ,3306
  ,3330
  ,3355
  ,3360
  ,3368
  ,3365
  ,3371
  ,3373
  ,3376
  ,3389
  ,3400
  ,3402
  ,3426
  ,3414
  ,3420
  ,3433
  ,3434
  ,3431
  ,3440
  ,3442
  ,3437
  ,3427
  ,3428
  ,3415
  ,3427
  ,3415
  ,3399
  ,3400
  ,3392
  ,3421
  ,3428
  ,3435
  ,3455
  ,3456
  ,3457
  ,3459
  ,3470
  ,3477
  ,3496
  ,3497
  ,3492
  ,3499
  ,3500
  ,3502
  ,3524
  ,3510
  ,3512
  ,3515
  ,3525
  ,3532
  ,3533
  ,3534
  ,3559
  ,3561
  ,3551
  ,3573
  ,3578
  ,3579
  ,3582
  ,3583
  ,3578
  ,3584
  ,3583
  ,3594
  ,3596
  ,3601
  ,3602
  ,3609
  ,3595
  ,3601
  ,3600
  ,3606
  ,3607
  ,3608
  ,3609
  ,3611
  ,3635
  ,3654
  ,3673
  ,3676
  ,3663
  ,3669
  ,3696
  ,3685
  ,3687
  ,3692
  ,3707
  ,3711
  ,3710
  ,3720
  ,3723
  ,3727
  ,3739
  ,3761
  ,3774
  ,3785
  ,3811
  ,3818
  ,3816
  ,3821
  ,3823
  ,3824
  ,3825
  ,3827
  ,3839
  ,3841
  ,3853
  ,3864
  ,3861
  ,3860
  ,3873
  ,3868
  ,3867
  ,3866
  ,3867
  ,3864
  ,3868
  ,3869
  ,3854
  ,3842
  ,3845
  ,3852
  ,3870
  ,3871
  ,3870
  ,3832
  ,3838
  ,3832
  ,3828
  ,3827
  ,3840
  ,3841
  ,3840
  ,3864
  ,3860
  ,3861
  ,3860
  ,3867
  ,3835
  ,3850
  ,3851
  ,3864
  ,3866
  ,3867
  ,3876
  ,3879
  ,3881
  ,3884
  ,3898
  ,3910
  ,3916
  ,3919
  ,3906
  ,3875
  ,3876
  ,3878
  ,3880
  ,3892
  ,3897
  ,3901
  ,3905
  ,3901
  ,3909
  ,3915
  ,3921
  ,3920
  ,3942
  ,3936
  ,3945
  ,3951
  ,3961
  ,3950
  ,3961
  ,3962
  ,3966
  ,3967
  ,3968
  ,3979
  ,3981
  ,3982
  ,3976
  ,3977
  ,3980
  ,3983
  ,3984
  ,4001
  ,4020
  ,4015
  ,4023
  ,4025
  ,4026
  ,4012
  ,4017
  ,4029
  ,4050
  ,4037
  ,4041
  ,4055
  ,4062
  ,4053
  ,4056
  ,4055
  ,4053
  ,4066
  ,4064
  ,4089
  ,4093
  ,4110
  ,4111
  ,4110
  ,4112
  ,4114
  ,4115
  ,4126
  ,4135
  ,4150
  ,4151
  ,4184
  ,4190
  ,4195
  ,4204
  ,4211
  ,4220
  ,4219
  ,4229
  ,4225
  ,4232
  ,4248
  ,4269
  ,4268
  ,4261
  ,4257
  ,4256
  ,4257
  ,4269
  ,4277
  ,4274
  ,4277
  ,4290
  ,4279
  ,4314
  ,4315
  ,4350
  ,4374
  ,4388
  ,4389
  ,4386
  ,4378
  ,4379
  ,4409
  ,4437
  ,4439
  ,4441
  ,4437
  ,4442
  ,4448
  ,4435
  ,4436
  ,4413
  ,4400
  ,4408
  ,4409
  ,4393
  ,4392
  ,4395
  ,4398
  ,4399
  ,4400
  ,4404
  ,4415
  ,4416
  ,4425
  ,4411
  ,4412
  ,4406
  ,4430
  ,4431
  ,4430
  ,4431
  ,4429
  ,4430
  ,4426
  ,4435
  ,4439
  ,4449
  ,4460
  ,4453
  ,4454
  ,4488
  ,4496
  ,4474
  ,4480
  ,4481
  ,4467
  ,4469
  ,4472
  ,4477
  ,4474
  ,4477
  ,4484
  ,4489
  ,4490
  ,4521
  ,4522
  ,4536
  ,4543
  ,4544
  ,4545
  ,4549
  ,4556
  ,4561
  ,4562
  ,4588
  ,4584
  ,4576
  ,4577
  ,4576
  ,4595
  ,4608
  ,4607
  ,4610
  ,4611
  ,4581
  ,4584
  ,4591
  ,4609
  ,4630
  ,4631
  ,4635
  ,4636
  ,4635
  ,4636
  ,4670
  ,4683
  ,4685
  ,4686
  ,4687
  ,4682
  ,4683
  ,4680
  ,4682
  ,4678
  ,4682
  ,4683
  ,4686
  ,4697
  ,4702
  ,4703
  ,4702
  ,4703
  ,4733
  ,4772
  ,4766
  ,4764
  ,4771
  ,4773
  ,4761
  ,4760
  ,4761
  ,4763
  ,4776
  ,4783
  ,4790
  ,4789
  ,4793
  ,4798
  ,4797
  ,4805
  ,4813
  ,4804
  ,4803
  ,4804
  ,4808
  ,4798
  ,4799
  ,4811
  ,4808
  ,4807
  ,4810
  ,4832
  ,4869
  ,4870
  ,4877
  ,4880
  ,4879
  ,4883
  ,4878
  ,4875
  ,4880
  ,4882
  ,4884
  ,4883
  ,4884
  ,4921
  ,4925
  ,4926
  ,4921
  ,4919
  ,4920
  ,4922
  ,4924
  ,4923
  ,4914
  ,4911
  ,4917
  ,4925
  ,4923
  ,4925
  ,4937
  ,4944
  ,4945
  ,4949
  ,4957
  ,4953
  ,4923
  ,4925
  ,4926
  ,4925
  ,4933
  ,4934
  ,4940
  ,4941
  ,4956
  ,4944
  ,4956
  ,4977
  ,4987
  ,4988
  ,4989
  ,4992
  ,4997
  ,4998
  ,5016
  ,5017
  ,5024
  ,5033
  ,5039
  ,5037
  ,5042
  ,5045
  ,5048
  ,5066
  ,5107
  ,5131
  ,5145
  ,5150
  ,5154
  ,5153
  ,5148
  ,5149
  ,5150
  ,5154
  ,5159
  ,5161
  ,5152
  ,5168
  ,5170
  ,5174
  ,5167
  ,5191
  ,5192
  ,5206
  ,5208
  ,5211
  ,5212
  ,5249
  ,5254
  ,5255
  ,5252
  ,5257
  ,5291
  ,5302
  ,5315
  ,5326
  ,5327
  ,5332
  ,5333
  ,5328
  ,5331
  ,5322
  ,5327
  ,5328
  ,5332
  ,5338
  ,5339
  ,5340
  ,5351
  ,5354
  ,5339
  ,5340
  ,5349
  ,5347
  ,5355
  ,5346
  ,5354
  ,5352
  ,5353
  ,5354
  ,5353
  ,5328
  ,5345
  ,5346
  ,5345
  ,5334
  ,5338
  ,5359
  ,5361
  ,5369
  ,5374
  ,5377
  ,5378
  ,5384
  ,5385
  ,5389
  ,5403
  ,5407
  ,5399
  ,5423
  ,5430
  ,5429
  ,5448
  ,5458
  ,5485
  ,5502
  ,5504
  ,5506
  ,5507
  ,5509
  ,5515
  ,5519
  ,5523
  ,5526
  ,5527
  ,5522
  ,5520
  ,5521
  ,5525
  ,5528
  ,5499
  ,5500
  ,5504
  ,5492
  ,5498
  ,5499
  ,5503
  ,5504
  ,5503
  ,5510
  ,5500
  ,5524
  ,5527
  ,5530
  ,5532
  ,5538
  ,5541
  ,5520
  ,5528
  ,5539
  ,5540
  ,5541
  ,5538
  ,5545
  ,5546
  ,5548
  ,5553
  ,5557
  ,5554
  ,5562
  ,5573
  ,5572
  ,5573
  ,5597
  ,5581
  ,5577
  ,5579
  ,5586
  ,5578
  ,5579
  ,5600
  ,5601
  ,5620
  ,5642
  ,5634
  ,5629
  ,5656
  ,5640
  ,5643
  ,5642
  ,5629
  ,5637
  ,5638
  ,5639
  ,5641
  ,5643
  ,5649
  ,5615
  ,5632
  ,5623
  ,5620
  ,5629
  ,5630
  ,5631
  ,5624
  ,5639
  ,5642
  ,5662
  ,5671
  ,5693
  ,5692
  ,5694
  ,5705
  ,5719
  ,5727
  ,5728
  ,5732
  ,5717
  ,5721
  ,5732
  ,5733
  ,5748
  ,5750
  ,5755
  ,5756
  ,5763
  ,5780
  ,5785
  ,5787
  ,5781
  ,5782
  ,5796
  ,5797
  ,5803
  ,5824
  ,5830
  ,5833
  ,5836
  ,5835
  ,5838
  ,5837
  ,5854
  ,5853
  ,5844
  ,5843
  ,5846
  ,5847
  ,5856
  ,5878
  ,5887
  ,5890
  ,5898
  ,5896
  ,5898
  ,5897
  ,5898
  ,5899
  ,5920
  ,5915
  ,5925
  ,5924
  ,5942
  ,5939
  ,5935
  ,5939
  ,5941
  ,5942
  ,5943
  ,5951
  ,5942
  ,5939
  ,5940
  ,5944
  ,5954
  ,5955
  ,5958
  ,5959
  ,5960
  ,5959
  ,5962
  ,5986
  ,6008
  ,6018
  ,6011
  ,6009
  ,6008
  ,6012
  ,6013
  ,6010
  ,6015
  ,6012
  ,6015
  ,6024
  ,6028
  ,6023
  ,6027
  ,6018
  ,6043
  ,6059
  ,6066
  ,6072
  ,6074
  ,6087
  ,6095
  ,6096
  ,6098
  ,6099
  ,6096
  ,6073
  ,6074
  ,6072
  ,6075
  ,6087
  ,6088
  ,6084
  ,6089
  ,6097
  ,6095
  ,6085
  ,6083
  ,6084
  ,6082
  ,6093
  ,6085
  ,6081
  ,6082
  ,6106
  ,6108
  ,6124
  ,6127
  ,6122
  ,6103
  ,6108
  ,6110
  ,6109
  ,6121
  ,6122
  ,6132
  ,6153
  ,6155
  ,6160
  ,6162
  ,6185
  ,6193
  ,6198
  ,6196
  ,6187
  ,6188
  ,6206
  ,6207
  ,6222
  ,6243
  ,6244
  ,6237
  ,6255
  ,6251
  ,6253
  ,6255
  ,6257
  ,6259
  ,6269
  ,6273
  ,6277
  ,6298
  ,6301
  ,6305
  ,6310
  ,6312
  ,6315
  ,6332
  ,6333
  ,6334
  ,6333
  ,6335
  ,6327
  ,6332
  ,6330
  ,6347
  ,6348
  ,6344
  ,6343
  ,6349
  ,6355
  ,6359
  ,6365
  ,6370
  ,6352
  ,6349
  ,6375
  ,6381
  ,6366
  ,6398
  ,6414
  ,6416
  ,6417
  ,6419
  ,6420
  ,6419
  ,6418
  ,6405
  ,6414
  ,6417
  ,6418
  ,6419
  ,6406
  ,6410
  ,6409
  ,6414
  ,6416
  ,6403
  ,6402
  ,6404
  ,6406
  ,6425
  ,6426
  ,6427
  ,6431
  ,6447
  ,6432
  ,6433
  ,6435
  ,6440
  ,6469
  ,6484
  ,6489
  ,6508
  ,6512
  ,6514
  ,6517
  ,6523
  ,6505
  ,6503
  ,6504
  ,6512
  ,6513
  ,6515
  ,6516
  ,6521
  ,6522
  ,6525
  ,6533
  ,6532
  ,6535
  ,6536
  ,6535
  ,6539
  ,6543
  ,6572
  ,6576
  ,6579
  ,6590
  ,6600
  ,6601
  ,6614
  ,6616
  ,6625
  ,6635
  ,6636
  ,6647
  ,6652
  ,6659
  ,6660
  ,6675
  ,6691
  ,6703
  ,6706
  ,6719
  ,6743
  ,6729
  ,6756
  ,6755
  ,6739
  ,6740
  ,6748
  ,6737
  ,6738
  ,6740
  ,6741
  ,6746
  ,6748
  ,6745
  ,6744
  ,6715
  ,6729
  ,6743
  ,6742
  ,6741
  ,6744
  ,6770
  ,6783
  ,6793
  ,6792
  ,6805
  ,6788
  ,6800
  ,6806
  ,6793
  ,6795
  ,6800
  ,6801
  ,6816
  ,6826
  ,6822
  ,6829
  ,6835
  ,6847
  ,6845
  ,6847
  ,6845
  ,6846
  ,6853
  ,6860
  ,6861
  ,6874
  ,6866
  ,6869
  ,6863
  ,6866
  ,6867
  ,6870
  ,6849
  ,6848
  ,6849
  ,6853
  ,6847
  ,6857
  ,6898
  ,6899
  ,6897
  ,6888
  ,6899
  ,6901
  ,6896
  ,6899
  ,6909
  ,6912
  ,6913
  ,6914
  ,6915
  ,6916
  ,6930
  ,6931
  ,6932
  ,6929
  ,6930
  ,6941
  ,6934
  ,6954
  ,6975
  ,6982
  ,6988
  ,7010
  ,7017
  ,7021
  ,7050
  ,7052
  ,7029
  ,7030
  ,7035
  ,7036
  ,7046
  ,7070
  ,7071
  ,7078
  ,7079
  ,7078
  ,7094
  ,7101
  ,7113
  ,7119
  ,7120
  ,7121
  ,7122
  ,7135
  ,7139
  ,7116
  ,7119
  ,7129
  ,7132
  ,7136
  ,7133
  ,7151
  ,7148
  ,7136
  ,7143
  ,7170
  ,7183
  ,7184
  ,7205
  ,7207
  ,7216
  ,7207
  ,7232
  ,7228
  ,7230
  ,7256
  ,7276
  ,7295
  ,7320
  ,7303
  ,7311
  ,7313
  ,7314
  ,7315
  ,7325
  ,7306
  ,7301
  ,7291
  ,7293
  ,7310
  ,7318
  ,7335
  ,7354
  ,7374
  ,7379
  ,7393
  ,7396
  ,7413
  ,7403
  ,7387
  ,7399
  ,7400
  ,7399
  ,7400
  ,7402
  ,7412
  ,7413
  ,7409
  ,7410
  ,7409
  ,7414
  ,7415
  ,7416
  ,7417
  ,7421
  ,7388
  ,7389
  ,7388
  ,7371
  ,7377
  ,7378
  ,7379
  ,7384
  ,7396
  ,7390
  ,7399
  ,7401
  ,7422
  ,7423
  ,7421
  ,7422
  ,7425
  ,7444
  ,7453
  ,7454
  ,7455
  ,7457
  ,7461
  ,7456
  ,7442
  ,7407
  ,7408
  ,7418
  ,7419
  ,7427
  ,7447
  ,7448
  ,7449
  ,7450
  ,7452
  ,7462
  ,7440
  ,7471
  ,7492
  ,7512
  ,7529
  ,7530
  ,7543
  ,7542
  ,7543
  ,7558
  ,7560
  ,7566
  ,7584
  ,7588
  ,7589
  ,7586
  ,7587
  ,7586
  ,7596
  ,7612
  ,7616
  ,7615
  ,7616
  ,7605
  ,7607
  ,7627
  ,7648
  ,7667
  ,7670
  ,7679
  ,7680
  ,7685
  ,7695
  ,7705
  ,7725
  ,7751
  ,7757
  ,7739
))))
}
