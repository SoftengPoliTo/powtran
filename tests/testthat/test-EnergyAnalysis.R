
library(powtran)

#### Utilities ######################################


read.from.zip <-function(zipfilename,skip=1,n=NA){
  if(!file.exists(zipfilename)){
    stop("File ",zipfilename,"does not exists!");
  }
  files = subset( unzip(zipfilename,list=TRUE),Length>0)
  if(dim(files)[1]==1){
    n=1
  }
  if(is.na(n)){
    stop("Must select a file index inside the zip.\n",
         "Available files:\n",
         paste(seq(dim(files)[1]),":",files$Name,collapse="\n")
    )
  }
  filename =files$Name[n]

  conn = unz(zipfilename,filename)
  samples <- scan(conn,skip=skip,quiet = TRUE)
  close(conn)
  return(samples)
}


context("extract.power")

### FIXTURES #############################
voltage = 5
period = 1/1000

### TESTS #############################

test_that("Clean trace RB pi 2 c_bubblesort_10000_3",{
  filename = "tests/raspberrypi2_c_bubblesort_10000_3.txt"
  samples = scan(filename,skip=1,quiet = TRUE)
  res = extract.power(samples*voltage,period)
  plot(res)
  #summary(res)
  expect_gte(dim(res$work)[1],30)
  expect_equal(sum(!is.na(res$work$P)),30)
})

test_that("Wavy trace RB pi 2 bubble 1k sorted",{
  zipfilename = "tests/raspberrypi2_c_bubblesort_1000_1.zip"
  samples = read.from.zip(zipfilename)

  res = extract.power(samples*voltage,period,
                        intermediate=TRUE, include.rawdata = TRUE)
  plot(res)
  #summary(res)
  expect_gte(dim(res$work)[1],30)
  expect_gt(sum(!is.na(res$work$P)),20)
})

test_that("Wavy trace RB pi 2 counting 5k sorted",{
  zipfilename = "tests/raspberrypi2_c_countingsort_5000_4.zip"
  samples = read.from.zip(zipfilename)
  res = extract.power(samples*voltage,period,
                        intermediate=TRUE, include.rawdata = TRUE)
  plot(res)
  #summary(res)
  expect_gte(dim(res$work)[1],30)
  expect_gt(sum(!is.na(res$work$P)),25)
})


test_that("Good trace RB pi 1 bubble 50k random, truncated begin",{
  zipfilename = "tests/raspberrypi1a_c_bubblesort_50000_5.zip"
  samples = read.from.zip(zipfilename)
  res = extract.power(samples*voltage,period,
                        intermediate=TRUE, include.rawdata = TRUE)
  plot(res)
  #summary(res)
  expect_gte(dim(res$work)[1],30)
  expect_gte(sum(!is.na(res$work$P)),29)
})


test_that("Good trace RB pi 2 bubble 10k random",{
  zipfilename = "tests/raspberrypi2_c_bubblesort_10000_4.zip"
  samples = read.from.zip(zipfilename)
  res = extract.power(samples*voltage,period,
                        intermediate=TRUE, include.rawdata = TRUE)
  plot(res)
  #summary(res)
  expect_gte(dim(res$work)[1],30)
  expect_gte(sum(!is.na(res$work$P)),30)
})

test_that("Good trace RB pi 1 bubble 1k sorted, only 29",{
  zipfilename = "tests/raspberrypi1a_c_bubblesort_1000_1.zip"
  samples = read.from.zip(zipfilename)
  res = extract.power(samples*voltage,period,
                        intermediate=TRUE, include.rawdata = TRUE)
  plot(res)
  #summary(res)
  expect_gte(dim(res$work)[1],29)
  expect_gte(sum(!is.na(res$work$P)),28)
})



test_that("Wavy trace RB pi 3 merge 5k random",{
  zipfilename = "tests/raspberrypi3b_c_mergesort_5000_3.zip"
  samples = read.from.zip(zipfilename)
  res = extract.power(samples*voltage,period,
                        intermediate=TRUE, include.rawdata = TRUE)
  plot(res)
  #summary(res)
  expect_gte(dim(res$work)[1],30)
  expect_gte(sum(!is.na(res$work$P)),26)
})


test_that("Good trace RB pi 2 quick 50k random",{
  zipfilename = "tests/raspberrypi2_c_quicksort_50000_4.zip"
  samples = read.from.zip(zipfilename)
  res = extract.power(samples*voltage,period,
                        intermediate=TRUE, include.rawdata = TRUE)
  plot(res)
  #summary(res)
  expect_gte(dim(res$work)[1],30)
  expect_gte(sum(!is.na(res$work$P)),30)
})

test_that("Long trace from Android @ 125kHz",{
  file = "tests/test1_android_125kHz.RData"
  load(file)
  res = extract.power(power.samples,
                      t.sampling = 1/125000,
                      marker.length = .300,
                      baseline = "gmin"
                      )
  plot(res)
  #summary(res)
  expect_gte(dim(res$work)[1],30)
  expect_gte(sum(!is.na(res$work$P)),10)
})

test_that("Norm trace from Android @ 125kHz",{
  file = "tests/test2_bubble_android.RData"
  load(file)
  #res = extract.power(power.samples,t.sampling = 1/125000,marker.length = .300)
  res = extract.power(power.samples,
                      t.sampling = 1/125000,
                      marker.length = .300,
                      baseline = "gmin")

  plot(res)
  #summary(res)
  expect_gte(dim(res$work)[1],30)
  expect_gte(sum(!is.na(res$work$P)),25)
})

test_that("Low sampling frequncy (e.g. 1Hz)",{
  filename = "tests/pt_all.txt"
  samples = read.table(filename, dec=",", header=TRUE, col.names="P", skip=1)
  res = extract.power(samples/1000, 1, marker.length=7)

  plot(res)
  #summary(res)
  expect_equal(dim(res$work)[1],30)
  expect_equal(sum(!is.na(res$work$P)),30)
})
