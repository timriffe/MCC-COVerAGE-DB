#south korea
library(readxl)
#http://ncov.mohw.go.kr/en/bdBoardList.do?brdId=16&brdGubun=162&dataGubun=&ncvContSeq=&contSeq=&board_id=&gubun=

#daegu
daegu_city <- read.csv("https://www.data.go.kr/cmm/cmm/fileDownload.do?atchFileId=FILE_000000002577252&fileDetailSn=1&dataNm=%EB%8C%80%EA%B5%AC%EA%B4%91%EC%97%AD%EC%8B%9C_%EC%9D%BC%EC%9D%BC%20%EC%BD%94%EB%A1%9C%EB%82%9819%20%ED%99%95%EC%A7%84%EC%9E%90%20%EC%88%98_20220727")[,-1]
names(daegu_city)[1] <- "date"
names(daegu_city)[2] <- "cases"
daegu_city$mcccityname1 <- "Daegu"

#daejeon
daejeon <- read.table("https://www.data.go.kr/cmm/cmm/fileDownload.do?atchFileId=FILE_000000002519399&fileDetailSn=1&dataNm=%EB%8C%80%EC%A0%84%EA%B4%91%EC%97%AD%EC%8B%9C_%EC%BD%94%EB%A1%9C%EB%82%98%20%ED%99%95%EC%A7%84%EC%9E%90%20%EC%88%98_20211231")
#to be continued


#gwangju
#5 city parts
gwangju1 <- read.csv("https://www.data.go.kr/cmm/cmm/fileDownload.do?atchFileId=FILE_000000002522974&fileDetailSn=1&dataNm=%EA%B4%91%EC%A3%BC%EA%B4%91%EC%97%AD%EC%8B%9C%20%EA%B4%91%EC%82%B0%EA%B5%AC_%EC%BD%94%EB%A1%9C%EB%82%9819%20%ED%99%95%EC%A7%84%EC%9E%90%20%EB%B0%8F%20%EC%82%AC%EB%A7%9D%EC%9E%90%20%ED%98%84%ED%99%A9_20220207")
gwangju2 <- read.csv("https://www.data.go.kr/cmm/cmm/fileDownload.do?atchFileId=FILE_000000002509659&fileDetailSn=1&dataNm=%EA%B4%91%EC%A3%BC%EA%B4%91%EC%97%AD%EC%8B%9C%20%EB%82%A8%EA%B5%AC_%EC%BD%94%EB%A1%9C%EB%82%9819%20%ED%99%95%EC%A7%84%EC%9E%90%20%EB%B0%8F%20%EC%82%AC%EB%A7%9D%EC%9E%90%20%ED%98%84%ED%99%A9_20220207")
gwangju3 <- read.csv("https://www.data.go.kr/cmm/cmm/fileDownload.do?atchFileId=FILE_000000002509856&fileDetailSn=1&dataNm=%EA%B4%91%EC%A3%BC%EA%B4%91%EC%97%AD%EC%8B%9C%20%EC%84%9C%EA%B5%AC_%EC%BD%94%EB%A1%9C%EB%82%9819%20%ED%99%95%EC%A7%84%EC%9E%90%20%EB%B0%8F%20%EC%82%AC%EB%A7%9D%EC%9E%90%20%ED%98%84%ED%99%A9_20220208")
gwangju4 <- read.csv("https://www.data.go.kr/cmm/cmm/fileDownload.do?atchFileId=FILE_000000002509332&fileDetailSn=1&dataNm=%EA%B4%91%EC%A3%BC%EA%B4%91%EC%97%AD%EC%8B%9C%20%EB%B6%81%EA%B5%AC_%EC%BD%94%EB%A1%9C%EB%82%9819%20%ED%99%95%EC%A7%84%EC%9E%90%20%EB%B0%8F%20%EC%82%AC%EB%A7%9D%EC%9E%90%20%ED%98%84%ED%99%A9_20220203")
gwangju5 <- read.csv("https://www.data.go.kr/cmm/cmm/fileDownload.do?atchFileId=FILE_000000002556453&fileDetailSn=1&dataNm=%EA%B4%91%EC%A3%BC%EA%B4%91%EC%97%AD%EC%8B%9C%20%EB%8F%99%EA%B5%AC_%EC%BD%94%EB%A1%9C%EB%82%9819%ED%99%95%EC%A7%84%EC%9E%90%20%EB%B0%8F%20%EC%82%AC%EB%A7%9D%EC%9E%90_20220125")

